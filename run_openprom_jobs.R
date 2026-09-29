# Run the standalone energy jobs workflow on an OPEN-PROM run folder.
#
# Usage:
#   Rscript run_openprom_jobs.R "C:/path/to/OPEN-PROM/run" [output_csv] [--visualize=none|total|breakdown|all] [--daccs-case=low|reference|high] [--beccs-case=low|reference|high]
#   Rscript run_openprom_jobs.R

required_packages <- c(
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  "ggplot2",
  "RColorBrewer",
  "readxl",
  "zoo"
)

initialize_required_packages <- function(packages) {
  repos <- getOption("repos")
  if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }

  user_lib <- Sys.getenv("R_LIBS_USER")
  if (nzchar(user_lib)) {
    user_lib <- normalizePath(path.expand(user_lib), winslash = "/", mustWork = FALSE)
    if (!dir.exists(user_lib)) {
      dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
    }
    if (dir.exists(user_lib)) {
      .libPaths(unique(c(user_lib, .libPaths())))
    }
  }

  missing_packages <- setdiff(packages, rownames(installed.packages()))
  if (length(missing_packages) > 0) {
    message("Installing missing R packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, dependencies = TRUE)
  }

  still_missing <- setdiff(packages, rownames(installed.packages()))
  if (length(still_missing) > 0) {
    stop(
      "Could not install/load required R packages: ",
      paste(still_missing, collapse = ", "),
      "\nCheck your internet connection or install them manually with install.packages().",
      call. = FALSE
    )
  }

  invisible(lapply(packages, function(package) {
    suppressPackageStartupMessages(library(package, character.only = TRUE))
  }))
}

initialize_required_packages(required_packages)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

read_interactive_path <- function(prompt) {
  value <- readline(prompt)
  value <- trimws(value)
  sub('^"(.*)"$', "\\1", value)
}

read_interactive_visualization_mode <- function() {
  value <- tolower(trimws(readline("Visualization mode [none/total/breakdown/all]: ")))
  if (!nzchar(value)) {
    return("none")
  }
  value
}

parse_arguments <- function(args) {
  options <- list(
    run_dir = NULL,
    output_csv = NULL,
    visualize = NULL,
    plots_dir = NULL,
    daccs_case = "reference",
    beccs_case = "reference",
    prompted = FALSE
  )
  positional <- character()

  for (arg in args) {
    if (startsWith(arg, "--visualize=")) {
      options$visualize <- sub("^--visualize=", "", arg)
    } else if (startsWith(arg, "--plots-dir=")) {
      options$plots_dir <- sub("^--plots-dir=", "", arg)
    } else if (startsWith(arg, "--daccs-case=")) {
      options$daccs_case <- sub("^--daccs-case=", "", arg)
    } else if (startsWith(arg, "--beccs-case=")) {
      options$beccs_case <- sub("^--beccs-case=", "", arg)
    } else {
      positional <- c(positional, arg)
    }
  }

  if (length(positional) >= 1) {
    options$run_dir <- positional[[1]]
  }
  if (length(positional) >= 2) {
    options$output_csv <- positional[[2]]
  }

  options
}

args <- commandArgs(trailingOnly = TRUE)
parsed <- parse_arguments(args)
if (length(args) < 1) {
  message("Usage: Rscript run_openprom_jobs.R <run_dir> [output_csv] [--visualize=none|total|breakdown|all] [--daccs-case=low|reference|high] [--beccs-case=low|reference|high]")
  parsed$run_dir <- read_interactive_path("OPEN-PROM run folder path: ")
  if (!nzchar(parsed$run_dir)) {
    stop("No run folder path provided.", call. = FALSE)
  }
  parsed$visualize <- read_interactive_visualization_mode()
  parsed$prompted <- TRUE
}

visualize_mode <- tolower(parsed$visualize %||% "none")
valid_visualize_modes <- c("none", "total", "breakdown", "all")
if (!visualize_mode %in% valid_visualize_modes) {
  stop(
    "Invalid visualization mode: ", visualize_mode,
    ". Use one of: ", paste(valid_visualize_modes, collapse = ", "),
    call. = FALSE
  )
}

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(file_arg) == 0) NULL else sub("^--file=", "", file_arg[[1]])
repo_dir <- if (is.null(script_path)) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else {
  dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
}
setwd(repo_dir)

run_dir <- normalizePath(parsed$run_dir, winslash = "/", mustWork = TRUE)
input_mif <- file.path(run_dir, "reporting.mif")
if (!file.exists(input_mif)) {
  stop("Could not find reporting.mif in: ", run_dir, call. = FALSE)
}

results_dir <- file.path(repo_dir, "energy_jobs_results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

run_name <- basename(run_dir)
default_output <- file.path(results_dir, paste0(run_name, "_energy_jobs_iamc.csv"))
output_csv <- parsed$output_csv %||% default_output
plots_dir <- parsed$plots_dir %||% file.path(results_dir, paste0(run_name, "_plots"))

source_definitions <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- sub("^require\\(tidyverse\\)", "# Loaded explicitly by run_openprom_jobs.R", lines)
  marker <- grep("^######### Run processing", lines)[1]
  if (is.na(marker)) {
    stop("Could not find run-processing marker in ", path, call. = FALSE)
  }
  eval(parse(text = paste(lines[seq_len(marker - 1)], collapse = "\n")), envir = .GlobalEnv)
}

year_columns <- function(data) {
  grep("^[0-9]{4}$", names(data), value = TRUE)
}

scale_rows <- function(data, rows, factor, unit) {
  years <- year_columns(data)
  data[rows, years] <- lapply(data[rows, years, drop = FALSE], function(x) as.numeric(x) * factor)
  data$Unit[rows] <- unit
  data
}

prepare_extraction_fallback <- function(data) {
  primary_rows <- data$Variable %in% c("Primary Energy|Coal", "Primary Energy|Gas", "Primary Energy|Oil")
  if (!any(primary_rows)) {
    return(data)
  }

  data <- scale_rows(data, primary_rows, 0.041868, "EJ/yr")

  primary <- data[primary_rows, , drop = FALSE]
  trade <- primary
  trade$Variable <- sub(
    "^Primary Energy\\|(Coal|Gas|Oil)$",
    "Trade|Primary Energy|\\1|Volume",
    trade$Variable
  )
  years <- year_columns(trade)
  trade[years] <- 0
  rbind(data, trade)
}

append_oil_refinery_proxy <- function(data) {
  liquids <- data[data$Variable == "Secondary Energy|Liquids", , drop = FALSE]
  if (nrow(liquids) == 0) {
    return(data)
  }

  liquids$Variable <- "Secondary Energy|Liquids|Oil"
  liquids <- scale_rows(liquids, seq_len(nrow(liquids)), 0.0036, "EJ/yr")
  rbind(data, liquids)
}

validate_openprom_component_identity <- function(data, aggregate_variable, component_variables) {
  variables <- c(aggregate_variable, component_variables)
  comparison <- data %>%
    filter(.data$Variable %in% variables) %>%
    select(Model, Scenario, Region, Variable, matches("^[0-9]{4}$")) %>%
    pivot_longer(
      cols = matches("^[0-9]{4}$"),
      names_to = "year",
      values_to = "value"
    ) %>%
    pivot_wider(names_from = Variable, values_from = value)

  complete_rows <- comparison %>%
    filter(if_all(all_of(variables), is.finite)) %>%
    mutate(
      difference = abs(
        .data[[aggregate_variable]] -
          .data[[component_variables[[1]]]] -
          .data[[component_variables[[2]]]]
      ),
      # OPEN-PROM writes these series to four decimal places. The absolute term admits the
      # largest rounding residual from the sample files; the relative term scales for large values.
      tolerance = 1.01e-4 + 1e-8 * abs(.data[[aggregate_variable]])
    )

  if (any(complete_rows$difference > complete_rows$tolerance)) {
    stop(
      "OPEN-PROM aggregate variable does not equal the sum of its CCS components: ",
      aggregate_variable,
      call. = FALSE
    )
  }
}


validate_openprom_units <- function(data, variables, expected_unit) {
  invalid_units <- data %>%
    filter(.data$Variable %in% variables, !is.na(.data$Unit), .data$Unit != expected_unit) %>%
    distinct(Unit) %>%
    pull(Unit)

  if (length(invalid_units) > 0) {
    stop(
      paste(variables, collapse = ", "), " must use '", expected_unit, "'. Found: ",
      paste(invalid_units, collapse = ", "),
      call. = FALSE
    )
  }
}


rename_openprom_variables <- function(data) {
  capacity_variables <- c(
    "Capacity|Electricity|Biofuels",
    "Capacity|Electricity|Biofuels|w/ CCS",
    "Capacity|Electricity|Biofuels|w/o CCS"
  )
  additions_variables <- c(
    "Capacity Additions|Electricity|Biofuels",
    "Capacity Additions|Electricity|Biofuels|w/ CCS",
    "Capacity Additions|Electricity|Biofuels|w/o CCS"
  )
  has_components <- all(c(capacity_variables, additions_variables) %in% unique(data$Variable))

  if (has_components) {
    validate_openprom_units(data, capacity_variables, "GW")
    validate_openprom_units(data, additions_variables, "GW/yr")
    validate_openprom_component_identity(data, capacity_variables[[1]], capacity_variables[2:3])
    validate_openprom_component_identity(data, additions_variables[[1]], additions_variables[2:3])

    # The aggregate already contains both components. Removing it prevents duplicate biomass jobs.
    data <- data %>%
      filter(!.data$Variable %in% c(capacity_variables[[1]], additions_variables[[1]]))
    data$Variable[data$Variable == capacity_variables[[2]]] <- "Capacity|Electricity|BECCS Base"
    data$Variable[data$Variable == capacity_variables[[3]]] <- "Capacity|Electricity|Biomass"
    data$Variable[data$Variable == additions_variables[[2]]] <- "Capacity Additions|Electricity|BECCS Base"
    data$Variable[data$Variable == additions_variables[[3]]] <- "Capacity Additions|Electricity|Biomass"
  } else {
    # Runs without CCS detail retain the original aggregate Biomass calculation.
    data$Variable[data$Variable == capacity_variables[[1]]] <- "Capacity|Electricity|Biomass"
    data$Variable[data$Variable == additions_variables[[1]]] <- "Capacity Additions|Electricity|Biomass"
    warning(
      "OPEN-PROM Biofuels capacity is not fully disaggregated by CCS status; ",
      "Biomass jobs will remain aggregated and BECCS jobs will not be reported separately."
    )
  }

  data
}

split_aggregate_additions <- function(data, aggregate_variable, component_variables) {
  years <- year_columns(data)
  aggregate <- data[data$Variable == aggregate_variable, , drop = FALSE]
  components <- data[data$Variable %in% component_variables, , drop = FALSE]

  if (nrow(aggregate) == 0 || nrow(components) == 0) {
    return(data)
  }

  split_rows <- list()
  key_cols <- c("Model", "Scenario", "Region")
  keys <- unique(aggregate[key_cols])

  for (i in seq_len(nrow(keys))) {
    key <- keys[i, , drop = FALSE]
    agg_idx <- which(aggregate$Model == key$Model & aggregate$Scenario == key$Scenario & aggregate$Region == key$Region)
    comp_idx <- which(components$Model == key$Model & components$Scenario == key$Scenario & components$Region == key$Region)
    if (length(agg_idx) != 1 || length(comp_idx) == 0) {
      next
    }

    comp_values <- as.matrix(sapply(components[comp_idx, years, drop = FALSE], as.numeric))
    totals <- colSums(comp_values, na.rm = TRUE)

    for (j in seq_along(comp_idx)) {
      row <- aggregate[agg_idx, , drop = FALSE]
      row$Variable <- sub("^Capacity\\|", "Capacity Additions|", components$Variable[comp_idx[j]])
      shares <- ifelse(totals > 0, comp_values[j, ] / totals, 0)
      row[years] <- as.list(as.numeric(unlist(aggregate[agg_idx, years, drop = FALSE])) * shares)
      split_rows[[length(split_rows) + 1]] <- row
    }
  }

  if (length(split_rows) == 0) {
    return(data)
  }

  rbind(data, do.call(rbind, split_rows))
}

add_openprom_region_mapping <- function(run_dir) {
  mapping_file <- file.path(run_dir, "data", "regionmappingOPDEV5.csv")
  if (!file.exists(mapping_file)) {
    warning("OPEN-PROM region mapping not found: ", mapping_file)
    return(invisible(FALSE))
  }

  openprom_mapping <- read.csv(mapping_file, sep = ";", check.names = FALSE)
  openprom_mapping <- openprom_mapping[, c("Region.Code", "ISO3.Code")]
  names(openprom_mapping) <- c("Region", "iso3")

  assign(
    "region_mappings",
    unique(rbind(region_mappings, openprom_mapping)),
    envir = .GlobalEnv
  )
  invisible(TRUE)
}

prepare_openprom_iamc <- function(input_mif) {
  data <- read.csv(input_mif, sep = ";", check.names = FALSE)
  data <- data[, names(data) != "", drop = FALSE]
  years <- year_columns(data)
  data[years] <- lapply(data[years], function(values) {
    values <- trimws(as.character(values))
    values[values %in% c("", "N/A")] <- NA_character_
    as.numeric(values)
  })
  data$scenario_model <- paste(data$Scenario, data$Model, sep = "-")

  data <- rename_openprom_variables(data)
  data <- split_aggregate_additions(
    data,
    "Capacity Additions|Electricity|Solar",
    c("Capacity|Electricity|Solar|PV", "Capacity|Electricity|Solar|CSP")
  )
  data <- split_aggregate_additions(
    data,
    "Capacity Additions|Electricity|Wind",
    c("Capacity|Electricity|Wind|Onshore", "Capacity|Electricity|Wind|Offshore")
  )
  data <- prepare_extraction_fallback(data)
  data <- append_oil_refinery_proxy(data)

  data
}

save_total_jobs_plot <- function(total_jobs, plots_dir) {
  selected_years <- c(2025L, 2030L, 2050L, 2070L)
  total_plot_data <- total_jobs %>%
    ungroup() %>%
    filter(!is.na(jobs)) %>%
    mutate(year = as.integer(year), jobs_million = jobs / 1e6) %>%
    filter(year %in% selected_years)

  if (nrow(total_plot_data) == 0) {
    warning("No total jobs data available for visualization.")
    return(character())
  }

  latest_year <- max(total_plot_data$year, na.rm = TRUE)
  plot_regions <- total_plot_data %>%
    filter(year == latest_year, Region != "World") %>%
    arrange(desc(jobs)) %>%
    slice_head(n = 12) %>%
    pull(Region) %>%
    as.character()

  plot_data <- total_plot_data %>%
    filter(Region %in% plot_regions) %>%
    mutate(
      year = factor(year, levels = selected_years),
      Region = factor(Region, levels = rev(plot_regions))
    )
  output_file <- file.path(plots_dir, "energy_jobs_total_top_regions.png")

  plot <- ggplot(plot_data, aes(x = Region, y = jobs_million, fill = year)) +
    geom_col(position = position_dodge(width = 0.82), width = 0.74) +
    coord_flip() +
    scale_fill_brewer(palette = "Dark2", drop = FALSE) +
    labs(
      title = "Total energy jobs by region",
      x = NULL,
      y = "Jobs (million)",
      fill = "Year"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.margin = margin(t = 8),
      axis.title.x = element_text(margin = margin(t = 10))
    )

  ggsave(output_file, plot = plot, width = 11, height = 8, dpi = 160)
  output_file
}

save_breakdown_plots <- function(energy_jobs, plots_dir) {
  selected_years <- c(2025L, 2030L, 2050L, 2070L)
  breakdown_data <- energy_jobs %>%
    filter(!is.na(jobs)) %>%
    mutate(year = as.integer(year), jobs_million = jobs / 1e6) %>%
    filter(year %in% selected_years) %>%
    mutate(year = factor(year, levels = selected_years))

  if (nrow(breakdown_data) == 0) {
    warning("No category/fuel jobs data available for visualization.")
    return(character())
  }

  category_file <- file.path(plots_dir, "energy_jobs_by_category_world.png")
  category_plot <- breakdown_data %>%
    filter(Region == "World") %>%
    group_by(year, category) %>%
    summarise(jobs_million = sum(jobs_million, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = year, y = jobs_million, fill = category)) +
    geom_col(width = 0.72) +
    labs(
      title = "Energy jobs by category",
      x = "Year",
      y = "Jobs (million)",
      fill = "Category"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  ggsave(category_file, plot = category_plot, width = 11, height = 7, dpi = 160)

  fuel_file <- file.path(plots_dir, "energy_jobs_by_fuel_world.png")
  fuel_plot <- breakdown_data %>%
    filter(Region == "World") %>%
    group_by(year, fuel) %>%
    summarise(jobs_million = sum(jobs_million, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = year, y = jobs_million, fill = fuel)) +
    geom_col(width = 0.72) +
    labs(
      title = "Energy jobs by fuel",
      x = "Year",
      y = "Jobs (million)",
      fill = "Fuel"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  ggsave(fuel_file, plot = fuel_plot, width = 11, height = 7, dpi = 160)

  c(category_file, fuel_file)
}

save_cdr_jobs_plot <- function(energy_jobs, plots_dir) {
  selected_years <- c(2030L, 2050L, 2070L, 2100L)
  cdr_fuels <- c("DACCS", "BECCS")

  cdr_plot_data <- energy_jobs %>%
    filter(
      Region == "World",
      fuel %in% cdr_fuels,
      year %in% selected_years,
      !is.na(jobs)
    ) %>%
    group_by(year, fuel, category) %>%
    summarise(jobs_thousand = sum(jobs, na.rm = TRUE) / 1e3, .groups = "drop") %>%
    mutate(
      fuel = factor(fuel, levels = cdr_fuels),
      year = factor(year, levels = selected_years)
    )

  if (nrow(cdr_plot_data) == 0) {
    warning("No DACCS or BECCS jobs data are available for the selected-year CDR plot.")
    return(character())
  }

  output_file <- file.path(plots_dir, "energy_jobs_cdr_selected_years.png")
  plot <- ggplot(cdr_plot_data, aes(x = year, y = jobs_thousand, fill = category)) +
    geom_col(width = 0.72) +
    facet_wrap(~fuel) +
    labs(
      x = "Year",
      y = "Jobs (thousand)",
      fill = "Category"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  ggsave(output_file, plot = plot, width = 9, height = 6, dpi = 160)
  output_file
}

save_cdr_key_regions_plot <- function(energy_jobs, plots_dir) {
  selected_years <- c(2030L, 2040L, 2050L, 2060L, 2070L)
  eu27_regions <- c(
    "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN",
    "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX",
    "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"
  )
  # OPEN-PROM reports China under model region code CHA. EU jobs are summed from the EU27
  # country results so that each member retains its country-specific jobs intensity.
  source_regions <- c(eu27_regions, "CHA", "USA")
  region_levels <- c("EU", "China", "US")
  cdr_fuels <- c("DACCS", "BECCS")

  plot_data <- energy_jobs %>%
    filter(
      Region %in% source_regions,
      fuel %in% cdr_fuels,
      year %in% selected_years,
      !is.na(jobs)
    ) %>%
    mutate(
      plot_region = case_when(
        .data$Region %in% eu27_regions ~ "EU",
        .data$Region == "CHA" ~ "China",
        .data$Region == "USA" ~ "US"
      )
    ) %>%
    group_by(plot_region, year, fuel, category) %>%
    summarise(jobs_thousand = sum(jobs, na.rm = TRUE) / 1e3, .groups = "drop") %>%
    mutate(
      plot_region = factor(plot_region, levels = region_levels),
      fuel = factor(fuel, levels = cdr_fuels),
      year = factor(year, levels = selected_years)
    )

  if (nrow(plot_data) == 0) {
    warning("No DACCS or BECCS jobs data are available for the selected regions and years.")
    return(character())
  }

  output_file <- file.path(plots_dir, "energy_jobs_cdr_key_regions_selected_years.png")
  plot <- ggplot(plot_data, aes(x = year, y = jobs_thousand, fill = category)) +
    geom_col(width = 0.72) +
    facet_grid(plot_region ~ fuel, scales = "free_y") +
    labs(
      title = "Carbon dioxide removal jobs in key regions",
      x = "Year",
      y = "Jobs (thousand)",
      fill = "Category"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  ggsave(output_file, plot = plot, width = 11, height = 9, dpi = 160)
  output_file
}

save_energy_jobs_visualizations <- function(total_jobs, energy_jobs, plots_dir, mode) {
  if (mode == "none") {
    return(character())
  }

  dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

  output_files <- character()
  if (mode %in% c("total", "all")) {
    output_files <- c(output_files, save_total_jobs_plot(total_jobs, plots_dir))
  }
  if (mode %in% c("breakdown", "all")) {
    output_files <- c(output_files, save_breakdown_plots(energy_jobs, plots_dir))
    output_files <- c(output_files, save_cdr_jobs_plot(energy_jobs, plots_dir))
    output_files <- c(output_files, save_cdr_key_regions_plot(energy_jobs, plots_dir))
  }

  output_files
}

source_definitions(file.path(repo_dir, "energy_jobs_iiasadb_clean.R"))
add_openprom_region_mapping(run_dir)
energy_jobs_iiasadb_regions <- create_iiasadb_job_intensity(
  region_mappings = region_mappings,
  energy_jobs_iso3 = energy_jobs_iso3
)

iamc_input_data <- prepare_openprom_iamc(input_mif)
daccs_assumptions <- load_daccs_jobs_assumptions(
  path = file.path(repo_dir, "daccs_jobs_assumptions.csv"),
  case = parsed$daccs_case
)
daccs_job_activity <- prepare_daccs_job_activity(
  iamc_input_data = iamc_input_data,
  assumptions = daccs_assumptions
)
daccs_job_intensity <- create_daccs_regional_job_intensity(
  base_job_intensity = energy_jobs_iiasadb_regions,
  assumptions = daccs_assumptions
)

beccs_assumptions <- load_beccs_jobs_assumptions(
  path = file.path(repo_dir, "beccs_jobs_assumptions.csv"),
  case = parsed$beccs_case
)
beccs_ccs_job_activity <- prepare_beccs_ccs_job_activity(
  iamc_input_data = iamc_input_data,
  assumptions = beccs_assumptions
)
beccs_base_job_intensity <- create_beccs_base_job_intensity(
  base_job_intensity = energy_jobs_iiasadb_regions
)
beccs_ccs_job_intensity <- create_beccs_ccs_regional_job_intensity(
  base_job_intensity = energy_jobs_iiasadb_regions,
  assumptions = beccs_assumptions
)

energy_jobs_iiasadb_regions <- bind_rows(
  energy_jobs_iiasadb_regions,
  daccs_job_intensity,
  beccs_base_job_intensity,
  beccs_ccs_job_intensity
)

additional_job_activity <- bind_rows(
  daccs_job_activity,
  beccs_ccs_job_activity
)

energy_jobs_iamc <- compute_energy_jobs_iiasadb(
  iamc_input_data = iamc_input_data,
  na_threshold = 0.60,
  technical_progress = TRUE,
  regional_convergence = TRUE,
  manufacturing = "current_shares",
  additional_job_activity = additional_job_activity
)

write.csv(energy_jobs_iamc, output_csv, row.names = FALSE)
message("Wrote ", normalizePath(output_csv, winslash = "/", mustWork = FALSE))

visualization_files <- save_energy_jobs_visualizations(
  total_jobs = total_jobs,
  energy_jobs = energy_jobs,
  plots_dir = plots_dir,
  mode = visualize_mode
)
if (length(visualization_files) > 0) {
  message("Wrote visualizations:")
  for (file in visualization_files) {
    message(" - ", normalizePath(file, winslash = "/", mustWork = FALSE))
  }
}

message("Note: Resource|Extraction|Coal/Gas/Oil is proxied from Primary Energy in Mtoe; Secondary Energy|Liquids|Oil is proxied from total Secondary Energy|Liquids in TWh and converted to EJ.")
message("DACCS jobs case: ", parsed$daccs_case, ". Assumptions are documented in daccs_jobs_assumptions.csv.")
message("BECCS jobs case: ", parsed$beccs_case, ". Assumptions are documented in beccs_jobs_assumptions.csv.")
