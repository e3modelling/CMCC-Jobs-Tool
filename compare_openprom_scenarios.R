# Compare energy-employment results from multiple OPEN-PROM scenarios.
#
# Usage:
#   Rscript compare_openprom_scenarios.R [results_dir] [--years=2030,2050,2070] [--include=all]
#   Rscript compare_openprom_scenarios.R [results_dir] --interactive
#
# The runner reads existing *_energy_jobs_iamc.csv files. It does not rerun
# OPEN-PROM or the employment calculation.

required_packages <- c(
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  "ggplot2",
  "RColorBrewer"
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

parse_integer_list <- function(value, option_name) {
  values <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  parsed <- suppressWarnings(as.integer(values))
  if (length(parsed) == 0 || any(is.na(parsed))) {
    stop(option_name, " must be a comma-separated list of years.", call. = FALSE)
  }
  sort(unique(parsed))
}

parse_arguments <- function(args) {
  options <- list(
    results_dir = NULL,
    output_dir = NULL,
    years = c(2030L, 2050L, 2070L),
    include = "all",
    interactive_selection = FALSE
  )
  positional <- character()

  for (arg in args) {
    if (startsWith(arg, "--output-dir=")) {
      options$output_dir <- sub("^--output-dir=", "", arg)
    } else if (startsWith(arg, "--years=")) {
      options$years <- parse_integer_list(sub("^--years=", "", arg), "--years")
    } else if (startsWith(arg, "--include=")) {
      options$include <- sub("^--include=", "", arg)
    } else if (arg == "--interactive") {
      options$interactive_selection <- TRUE
    } else {
      positional <- c(positional, arg)
    }
  }

  if (length(positional) > 1) {
    stop("Only one positional argument, results_dir, is supported.", call. = FALSE)
  }
  if (length(positional) == 1) {
    options$results_dir <- positional[[1]]
  }
  options
}

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(file_arg) == 0) NULL else sub("^--file=", "", file_arg[[1]])
repo_dir <- if (is.null(script_path)) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else {
  dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
}

parsed <- parse_arguments(commandArgs(trailingOnly = TRUE))
results_dir <- normalizePath(
  parsed$results_dir %||% file.path(repo_dir, "energy_jobs_results"),
  winslash = "/",
  mustWork = TRUE
)
output_dir <- parsed$output_dir %||% file.path(results_dir, "scenario_comparison")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

result_files <- list.files(
  results_dir,
  pattern = "_energy_jobs_iamc\\.csv$",
  full.names = TRUE,
  recursive = FALSE
)
if (length(result_files) < 2) {
  stop(
    "At least two *_energy_jobs_iamc.csv files are required in ",
    results_dir,
    call. = FALSE
  )
}

scenario_names <- sub("_energy_jobs_iamc\\.csv$", "", basename(result_files))
scenario_codes <- stringr::str_extract(scenario_names, "^S[0-9]+")
scenario_labels <- scenario_codes
scenario_labels[is.na(scenario_labels)] <- scenario_names[is.na(scenario_labels)]

# These names reproduce the OPEN-PROM case names used in UPTAKE Deliverable D4.3.
# They make the figures readable while the manifest retains each complete run name.
scenario_descriptions <- c(
  "S0" = "NPI portfolio reference",
  "S1" = "1.5\u00b0C full portfolio",
  "S2" = "BECCS-only engineered portfolio",
  "S3" = "BECCS CAPEX support",
  "S4" = "DACCS CAPEX support",
  "S5" = "BECCS support + BIO100",
  "S6" = "ETS-linked price case + BIO100"
)
scenario_description <- unname(scenario_descriptions[scenario_codes])
scenario_description[is.na(scenario_description)] <- scenario_names[is.na(scenario_description)]
scenario_display <- ifelse(
  is.na(scenario_codes),
  scenario_names,
  paste0(scenario_codes, " - ", scenario_description)
)

# Duplicate short codes are expanded to their full scenario names so that no
# result can be silently combined with a different run.
duplicate_labels <- unique(scenario_labels[duplicated(scenario_labels) | duplicated(scenario_labels, fromLast = TRUE)])
scenario_labels[scenario_labels %in% duplicate_labels] <- scenario_names[scenario_labels %in% duplicate_labels]

scenario_number <- ifelse(
  grepl("^S[0-9]+$", scenario_codes),
  as.numeric(sub("^S", "", scenario_codes)),
  Inf
)
manifest <- tibble::tibble(
  scenario_code = scenario_codes,
  scenario_label = scenario_labels,
  scenario_description = scenario_description,
  scenario_display = scenario_display,
  scenario_name = scenario_names,
  result_file = basename(result_files),
  result_path = result_files,
  scenario_number = scenario_number
) %>%
  arrange(scenario_number, scenario_name)

# S6 is intentionally omitted from employment-comparison figures because the
# available OPEN-PROM implementation does not provide a sufficiently distinct
# policy experiment for this comparison.
excluded_scenario_codes <- "S6"
excluded_rows <- manifest %>% filter(.data$scenario_code %in% excluded_scenario_codes)
if (nrow(excluded_rows) > 0) {
  message("Excluded from scenario comparison: ", paste(excluded_rows$scenario_display, collapse = ", "))
}
manifest <- manifest %>% filter(!.data$scenario_code %in% excluded_scenario_codes)

if (parsed$interactive_selection) {
  cat("\nAvailable scenario results:\n")
  for (index in seq_len(nrow(manifest))) {
    cat(sprintf(
      "  %d [%s]\n      Result: %s\n",
      index,
      manifest$scenario_display[[index]],
      manifest$scenario_name[[index]]
    ))
  }
  cat("\nEnter at least two scenario numbers separated by commas (for example 1,3,5): ")
  flush.console()
  selection_input <- readLines(con = "stdin", n = 1, warn = FALSE)
  if (length(selection_input) == 0 || !nzchar(trimws(selection_input))) {
    stop("No scenario selection was entered.", call. = FALSE)
  }

  selection_tokens <- strsplit(trimws(selection_input), "[,;[:space:]]+")[[1]]
  selected_indices <- suppressWarnings(as.integer(selection_tokens))
  invalid_tokens <- selection_tokens[is.na(selected_indices)]
  if (length(invalid_tokens) > 0) {
    stop(
      "Scenario selection must contain numbers from the displayed list. Invalid value(s): ",
      paste(invalid_tokens, collapse = ", "),
      call. = FALSE
    )
  }
  selected_indices <- unique(selected_indices)
  if (length(selected_indices) < 2) {
    stop("Select at least two scenarios for comparison.", call. = FALSE)
  }
  if (any(selected_indices < 1 | selected_indices > nrow(manifest))) {
    stop(
      "Scenario numbers must be between 1 and ", nrow(manifest), ".",
      call. = FALSE
    )
  }
  manifest <- manifest[selected_indices, , drop = FALSE]
} else if (tolower(trimws(parsed$include)) != "all") {
  requested <- trimws(strsplit(parsed$include, ",", fixed = TRUE)[[1]])
  requested <- requested[nzchar(requested)]
  if (length(requested) < 2) {
    stop("--include must identify at least two scenarios or use 'all'.", call. = FALSE)
  }
  if (any(toupper(requested) == "S6" | grepl("^S6_", toupper(requested)))) {
    stop("S6 is excluded from the scenario-comparison figures.", call. = FALSE)
  }

  matches_request <- tolower(manifest$scenario_code) %in% tolower(requested) |
    tolower(manifest$scenario_label) %in% tolower(requested) |
    tolower(manifest$scenario_name) %in% tolower(requested) |
    tolower(manifest$scenario_display) %in% tolower(requested)
  unmatched <- requested[
    !tolower(requested) %in% tolower(c(
      manifest$scenario_code,
      manifest$scenario_label,
      manifest$scenario_name,
      manifest$scenario_display
    ))
  ]
  if (length(unmatched) > 0) {
    stop("Unknown scenario(s) in --include: ", paste(unmatched, collapse = ", "), call. = FALSE)
  }
  manifest <- manifest[matches_request, , drop = FALSE]
}

if (nrow(manifest) < 2) {
  stop("The selected comparison must contain at least two scenarios.", call. = FALSE)
}

read_scenario_result <- function(
  path,
  scenario_name,
  scenario_label,
  scenario_display,
  selected_years
) {
  data <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  required_columns <- c("Region", "Model", "Scenario", "Variable")
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      basename(path), " is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  year_columns <- as.character(selected_years)
  missing_years <- setdiff(year_columns, names(data))
  if (length(missing_years) > 0) {
    stop(
      basename(path), " does not contain requested year(s): ",
      paste(missing_years, collapse = ", "),
      call. = FALSE
    )
  }

  embedded_scenarios <- unique(data$Scenario[!is.na(data$Scenario)])
  if (length(embedded_scenarios) != 1 || embedded_scenarios[[1]] != scenario_name) {
    warning(
      basename(path),
      " has a Scenario field that differs from its file name; the file name is used for comparison labels."
    )
  }

  data %>%
    select(all_of(c(required_columns, year_columns))) %>%
    mutate(
      scenario_name = scenario_name,
      scenario_label = scenario_label,
      scenario_display = scenario_display
    ) %>%
    pivot_longer(
      cols = all_of(year_columns),
      names_to = "year",
      values_to = "jobs"
    ) %>%
    mutate(
      year = as.integer(year),
      jobs = as.numeric(jobs)
    )
}

comparison_data <- bind_rows(lapply(seq_len(nrow(manifest)), function(index) {
  read_scenario_result(
    manifest$result_path[[index]],
    manifest$scenario_name[[index]],
    manifest$scenario_label[[index]],
    manifest$scenario_display[[index]],
    parsed$years
  )
}))

scenario_levels <- manifest$scenario_label
year_levels <- parsed$years
sum_jobs <- function(values) {
  if (all(is.na(values))) NA_real_ else sum(values, na.rm = TRUE)
}

comparison_data <- comparison_data %>%
  mutate(
    scenario_label = factor(scenario_label, levels = scenario_levels),
    year_factor = factor(year, levels = year_levels)
  )

base_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

scenario_colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(
  length(scenario_levels)
)
names(scenario_colors) <- scenario_levels

# World totals are already aggregated by the employment workflow. The plot
# compares that reported total directly and therefore avoids summing countries
# with overlapping model-region rows.
world_total <- comparison_data %>%
  filter(.data$Region == "World", .data$Variable == "Energy Jobs|Total") %>%
  group_by(scenario_label, scenario_name, scenario_display, year, year_factor) %>%
  summarise(jobs = sum_jobs(jobs), .groups = "drop") %>%
  mutate(jobs_million = jobs / 1e6)

expected_total_rows <- nrow(manifest) * length(year_levels)
if (nrow(world_total) != expected_total_rows || any(is.na(world_total$jobs))) {
  stop("World total jobs are incomplete for one or more selected scenarios and years.", call. = FALSE)
}

world_total_plot <- ggplot(
  world_total,
  aes(x = year_factor, y = jobs_million, fill = scenario_label)
) +
  geom_col(position = position_dodge(width = 0.80), width = 0.72) +
  scale_fill_manual(
    values = scenario_colors,
    breaks = scenario_levels,
    labels = manifest$scenario_display,
    drop = FALSE
  ) +
  labs(x = "Year", y = "Jobs (million)", fill = "Scenario") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  base_theme

world_total_file <- file.path(output_dir, "scenario_comparison_total_jobs_world.png")
ggsave(world_total_file, plot = world_total_plot, width = 13, height = 6.5, dpi = 180)

# CDR variables follow Energy Jobs|Category|Technology|Total. Restricting the
# category pattern excludes empty Extraction and Refinery combinations.
cdr_pattern <- "^Energy Jobs\\|(Construction|Manufacturing|O&M)\\|(DACCS|BECCS)\\|Total$"
world_cdr <- comparison_data %>%
  filter(.data$Region == "World", stringr::str_detect(.data$Variable, cdr_pattern)) %>%
  tidyr::extract(
    Variable,
    into = c("category", "technology"),
    regex = cdr_pattern,
    remove = FALSE
  ) %>%
  group_by(scenario_label, scenario_name, scenario_display, year, year_factor, technology, category) %>%
  summarise(jobs = sum_jobs(jobs), .groups = "drop") %>%
  mutate(
    jobs_thousand = jobs / 1e3,
    technology = factor(technology, levels = c("DACCS", "BECCS")),
    category = factor(category, levels = c("Construction", "Manufacturing", "O&M"))
  )

if (nrow(world_cdr) == 0) {
  stop("No World DACCS or BECCS employment rows were found.", call. = FALSE)
}

eu27_regions <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN",
  "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX",
  "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"
)
key_region_codes <- c(eu27_regions, "CHA", "USA")

# EU values sum country-level jobs so each member retains its regional labor
# intensity. China and the US use the OPEN-PROM CHA and USA result rows.
regional_cdr <- comparison_data %>%
  filter(
    .data$Region %in% key_region_codes,
    stringr::str_detect(.data$Variable, cdr_pattern)
  ) %>%
  tidyr::extract(
    Variable,
    into = c("category", "technology"),
    regex = cdr_pattern,
    remove = FALSE
  ) %>%
  mutate(
    plot_region = case_when(
      .data$Region %in% eu27_regions ~ "EU",
      .data$Region == "CHA" ~ "China",
      .data$Region == "USA" ~ "US"
    )
  ) %>%
  group_by(
    scenario_label,
    scenario_name,
    scenario_display,
    plot_region,
    technology,
    category,
    year,
    year_factor
  ) %>%
  summarise(jobs = sum_jobs(jobs), .groups = "drop") %>%
  mutate(
    jobs_thousand = jobs / 1e3,
    plot_region = factor(plot_region, levels = c("EU", "China", "US")),
    technology = factor(technology, levels = c("DACCS", "BECCS")),
    category = factor(category, levels = c("Construction", "Manufacturing", "O&M"))
  )

if (nrow(regional_cdr) == 0) {
  stop("No regional DACCS or BECCS employment rows were found.", call. = FALSE)
}

# One figure combines years, scenarios, technologies, and geographic scales.
# Free y-scales are applied by region row; DACCS and BECCS retain the same
# scale within each row and can therefore be compared directly.
combined_cdr <- bind_rows(
  world_cdr %>% mutate(plot_region = "World"),
  regional_cdr
) %>%
  group_by(
    scenario_label,
    scenario_name,
    scenario_display,
    plot_region,
    technology,
    year,
    year_factor
  ) %>%
  summarise(jobs = sum_jobs(jobs), .groups = "drop") %>%
  mutate(
    jobs_thousand = jobs / 1e3,
    plot_region = factor(plot_region, levels = c("World", "EU", "China", "US")),
    technology = factor(technology, levels = c("DACCS", "BECCS"))
  )

combined_cdr_plot <- ggplot(
  combined_cdr,
  aes(x = year_factor, y = jobs_thousand, fill = scenario_label)
) +
  geom_col(position = position_dodge(width = 0.80), width = 0.72) +
  facet_grid(plot_region ~ technology, scales = "free_y") +
  scale_fill_manual(
    values = scenario_colors,
    breaks = scenario_levels,
    labels = manifest$scenario_display,
    drop = FALSE
  ) +
  labs(x = "Year", y = "Jobs (thousand)", fill = "Scenario") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  base_theme

combined_cdr_file <- file.path(output_dir, "scenario_comparison_cdr_jobs.png")
ggsave(combined_cdr_file, plot = combined_cdr_plot, width = 14, height = 11, dpi = 180)

# Remove superseded plot files so an old comparison containing S6 cannot be
# mistaken for the current consolidated figure.
obsolete_plot_files <- file.path(
  output_dir,
  c(
    "scenario_comparison_cdr_jobs_world.png",
    "scenario_comparison_cdr_jobs_key_regions.png"
  )
)
invisible(file.remove(obsolete_plot_files[file.exists(obsolete_plot_files)]))

readr::write_csv(
  manifest %>% select(
    scenario_code,
    scenario_label,
    scenario_description,
    scenario_display,
    scenario_name,
    result_file
  ),
  file.path(output_dir, "scenario_comparison_manifest.csv")
)
readr::write_csv(
  world_total %>% select(scenario_label, scenario_display, scenario_name, year, jobs),
  file.path(output_dir, "scenario_comparison_total_jobs_world.csv")
)
readr::write_csv(
  world_cdr %>% select(
    scenario_label,
    scenario_display,
    scenario_name,
    year,
    technology,
    category,
    jobs
  ),
  file.path(output_dir, "scenario_comparison_cdr_jobs_world.csv")
)
readr::write_csv(
  regional_cdr %>% select(
    scenario_label,
    scenario_display,
    scenario_name,
    plot_region,
    year,
    technology,
    category,
    jobs
  ),
  file.path(output_dir, "scenario_comparison_cdr_jobs_key_regions.csv")
)
readr::write_csv(
  combined_cdr %>% select(
    scenario_label,
    scenario_display,
    scenario_name,
    plot_region,
    year,
    technology,
    jobs
  ),
  file.path(output_dir, "scenario_comparison_cdr_jobs.csv")
)

message("Compared scenarios: ", paste(scenario_levels, collapse = ", "))
message("Selected years: ", paste(year_levels, collapse = ", "))
message("Wrote scenario comparison outputs to ", output_dir)
message(" - ", world_total_file)
message(" - ", combined_cdr_file)
