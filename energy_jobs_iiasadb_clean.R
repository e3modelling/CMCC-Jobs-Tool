#Function to compute Energy System Jobs based on IAMC template input data

# Packages
require(tidyverse)
options(dplyr.summarise.inform = FALSE)
library(RColorBrewer)
library(readxl)
library(zoo)


# load all region mappings we want to potentially consider
#### regional aggregation to all relevant aggregations R5/10, common_regions, ...
region_mappings <- rbind(
  read.csv("regmap_r10.csv",col.names = c("Region","iso3")) %>%
    mutate(across(everything(),str_trim)), 
  read.csv("regmap_r5.csv",col.names = c("Region","iso3")) %>%
    mutate(across(everything(),str_trim)) %>% 
    mutate(Region = str_replace(Region, 'R5OECD', 'R5OECD90+EU')))
region_mappings <- rbind(
  region_mappings,
  data.frame(Region="World", iso3=unique(region_mappings$iso3)), 
  data.frame(Region=c("India", "China", "United States of America"), iso3=c("IND", "CHN", "USA")))

#### load energy_jobs_database completed at ISO3 level including IEA quantities created running process_energy_jobs_database_internal.R
energy_jobs_iso3 <- read.csv(file = file.path("energy_jobs_intensity.csv"))



#function: create a iiasadb region mapping based jobs intensity to be used to compute energy jobs
create_iiasadb_job_intensity <- function(region_mappings = region_mappings, energy_jobs_iso3  = energy_jobs_iso3){
# Combine regional mappings with  original file
energy_jobs_iso3 <-merge(energy_jobs_iso3, region_mappings, by = "iso3", all = T)
energy_jobs_iso3 <- energy_jobs_iso3 %>% group_by(fuel, category, Region) %>% mutate(value = weighted.mean(value, w = energyquantity+1e-12, na.rm = T)) %>% rename(jobs_per_unit = value) %>% select("category", "fuel", "jobs_per_unit", "Region") %>% distinct()
#convert units to match IAMC template: capacity are already GW, PJ energy to EJ
energy_jobs_iso3 <- energy_jobs_iso3 %>% mutate(jobs_per_unit = ifelse(category %in% c("Extraction", "Refinery"), jobs_per_unit * 1e+3, jobs_per_unit))
return(as.data.frame(energy_jobs_iso3))
}


# Load one documented technology case together with assumptions shared by all cases.
load_jobs_assumptions <- function(path, case = "reference", technology) {
  assumptions <- read_csv(path, show_col_types = FALSE)
  valid_cases <- assumptions %>%
    filter(case != "all") %>%
    pull(case) %>%
    unique()

  if (!case %in% valid_cases) {
    stop(
      "Invalid ", technology, " assumptions case: ", case,
      ". Use one of: ", paste(valid_cases, collapse = ", "),
      call. = FALSE
    )
  }

  assumptions %>% filter(.data$case %in% c("all", .env$case))
}


# Return one scalar assumption and fail early if the assumptions file is incomplete or ambiguous.
get_jobs_assumption <- function(assumptions, parameter, category = NULL, technology) {
  rows <- assumptions %>% filter(.data$parameter == .env$parameter)
  if (is.null(category)) {
    rows <- rows %>% filter(is.na(.data$category) | .data$category == "")
  } else {
    rows <- rows %>% filter(.data$category == .env$category)
  }

  if (nrow(rows) != 1 || is.na(rows$value[[1]])) {
    stop(
      "Expected exactly one ", technology, " assumption for parameter '", parameter,
      if (is.null(category)) "'." else paste0("' and category '", category, "'."),
      call. = FALSE
    )
  }

  as.numeric(rows$value[[1]])
}


load_daccs_jobs_assumptions <- function(path = "daccs_jobs_assumptions.csv", case = "reference") {
  load_jobs_assumptions(path = path, case = case, technology = "DACCS")
}


load_beccs_jobs_assumptions <- function(path = "beccs_jobs_assumptions.csv", case = "reference") {
  load_jobs_assumptions(path = path, case = case, technology = "BECCS")
}


get_daccs_assumption <- function(assumptions, parameter, category = NULL) {
  get_jobs_assumption(assumptions, parameter, category, technology = "DACCS")
}


get_beccs_assumption <- function(assumptions, parameter, category = NULL) {
  get_jobs_assumption(assumptions, parameter, category, technology = "BECCS")
}


# Derive the annual increase in the installed capture-capacity stock reported by OPEN-PROM.
compute_positive_capacity_additions <- function(capacity) {
  additions <- c(NA_real_, diff(capacity))

  # Equation: additions[t] = max(capacity[t] - capacity[t - 1], 0).
  # The first modeled stock is already operating and therefore receives no construction activity.
  additions[!is.na(additions)] <- pmax(additions[!is.na(additions)], 0)
  if (length(additions) > 0 && !is.na(capacity[[1]])) {
    additions[[1]] <- 0
  }

  additions
}


# Optional sensitivity that adds replacement cohorts after an assumed plant lifetime.
compute_daccs_cohort_additions <- function(capacity, lifetime) {
  additions <- rep(NA_real_, length(capacity))

  for (index in seq_along(capacity)) {
    if (is.na(capacity[[index]])) {
      next
    }

    surviving_indices <- which(
      seq_along(additions) < index &
        seq_along(additions) >= index - lifetime + 1
    )

    # Equation: surviving_capacity[t] = sum(additions from the previous L-1 years).
    surviving_capacity <- sum(additions[surviving_indices], na.rm = TRUE)

    # Equation: additions[t] = max(required_capacity[t] - surviving_capacity[t], 0).
    # The max prevents a temporary decline in modeled activity from creating negative construction jobs.
    additions[[index]] <- max(capacity[[index]] - surviving_capacity, 0)
  }

  additions
}


# Convert commissioning additions into capacity simultaneously under construction.
compute_capacity_under_construction <- function(additions, construction_period) {
  under_construction <- rep(NA_real_, length(additions))

  for (index in seq_along(additions)) {
    commissioning_window <- index:min(length(additions), index + construction_period - 1)

    # Equation: capacity_under_construction[t] = sum(additions commissioned from t to t + T - 1).
    # Rhodium reports average annual employment over a multi-year build. Each future commissioning
    # cohort contributes to employment in every construction year preceding operation.
    under_construction[[index]] <- if (all(is.na(additions[commissioning_window]))) {
      NA_real_
    } else {
      sum(additions[commissioning_window], na.rm = TRUE)
    }
  }

  under_construction
}


# Translate OPEN-PROM DACCS capture capacity into the activity quantities consumed by the common jobs engine.
prepare_daccs_job_activity <- function(iamc_input_data, assumptions) {
  variable_priority <- c(
    "Carbon Removal|Geological Storage|Direct Air Capture",
    "Carbon Capture|Geological Storage|Direct Air Capture",
    "Carbon Capture|Direct Air Capture"
  )
  available_variables <- variable_priority[variable_priority %in% unique(iamc_input_data$Variable)]

  if (length(available_variables) == 0) {
    warning("No OPEN-PROM DACCS capture-capacity variable was found; DACCS jobs will not be calculated.")
    return(NULL)
  }

  selected_variable <- available_variables[[1]]
  daccs_rows <- iamc_input_data %>% filter(.data$Variable == selected_variable)

  if (any(daccs_rows$Unit != "Mt CO2/yr", na.rm = TRUE)) {
    stop(
      "DACCS activity must use 'Mt CO2/yr'. Found: ",
      paste(unique(daccs_rows$Unit), collapse = ", "),
      call. = FALSE
    )
  }

  if (length(available_variables) > 1) {
    comparison <- iamc_input_data %>%
      filter(.data$Variable %in% available_variables) %>%
      select(-scenario_model, -Unit) %>%
      pivot_longer(
        cols = matches("^[0-9]{4}$"),
        names_to = "year",
        values_to = "removal"
      ) %>%
      group_by(Model, Scenario, Region, year) %>%
      summarise(
        spread = max(removal, na.rm = TRUE) - min(removal, na.rm = TRUE),
        .groups = "drop"
      )

    if (any(is.finite(comparison$spread) & comparison$spread > 1e-9)) {
      warning(
        "OPEN-PROM DACCS capture/removal variables differ. Using the highest-priority variable: ",
        selected_variable
      )
    }
  }

  # The five-year construction period follows Rhodium's DAC workforce assessment.
  construction_period <- as.integer(get_daccs_assumption(assumptions, "construction_period"))

  if (construction_period < 1) {
    stop("DACCS construction period must be at least one year.", call. = FALSE)
  }

  daccs_capacity <- daccs_rows %>%
    select(-scenario_model, -Unit) %>%
    pivot_longer(
      cols = matches("^[0-9]{4}$"),
      names_to = "year",
      values_to = "reported_capacity"
    ) %>%
    mutate(
      year = as.integer(year),
      reported_capacity = pmax(as.numeric(reported_capacity), 0)
    ) %>%
    arrange(Model, Scenario, Region, year)

  nonannual_series <- daccs_capacity %>%
    group_by(Model, Scenario, Region) %>%
    summarise(nonannual = any(diff(year) != 1L), .groups = "drop") %>%
    filter(nonannual)

  if (nrow(nonannual_series) > 0) {
    stop(
      "DACCS additions require consecutive annual OPEN-PROM results; a non-annual or duplicate year was found.",
      call. = FALSE
    )
  }

  daccs_capacity <- daccs_capacity %>%
    group_by(Model, Scenario, Region) %>%
    mutate(
      # Equation: operating_capacity[t] = OPEN-PROM installed capture capacity[t].
      # OPEN-PROM's V06CapCDR stock is reported in Mt CO2/yr and is already the annual capture
      # capacity subtracted from emissions. Applying an external capacity factor here would double-adjust it.
      # Model references: Q06CapCDR in modules/06_CO2/legacy/equations.gms and Q07EmissionsNet
      # in modules/07_Emissions/legacy/equations.gms.
      operating_capacity = reported_capacity,
      # Equation: additions[t] = max(operating_capacity[t] - operating_capacity[t - 1], 0).
      # OPEN-PROM accumulates V06CapCDR without retirements, so the central calculation follows its
      # modeled stock change. For a replacement sensitivity, use compute_daccs_cohort_additions()
      # with the documented lifetime in daccs_jobs_assumptions.csv.
      additions = compute_positive_capacity_additions(operating_capacity),
      capacity_under_construction = compute_capacity_under_construction(
        additions,
        construction_period
      )
    ) %>%
    ungroup()

  # Equation mapping: O&M uses operating capacity; Construction and Manufacturing use capacity
  # simultaneously under construction. This preserves Rhodium's average annual five-year job estimates.
  bind_rows(
    daccs_capacity %>%
      transmute(Model, Scenario, Region, year, fuel = "DACCS", category = "O&M", energyquantity = operating_capacity),
    daccs_capacity %>%
      transmute(Model, Scenario, Region, year, fuel = "DACCS", category = "Construction", energyquantity = capacity_under_construction),
    daccs_capacity %>%
      transmute(Model, Scenario, Region, year, fuel = "DACCS", category = "Manufacturing", energyquantity = capacity_under_construction)
  )
}


# Transfer the tool's existing regional labor-intensity pattern to a technology with US source factors.
create_cdr_regional_job_intensity <- function(base_job_intensity, assumptions, fuel) {
  categories <- c("Construction", "Manufacturing", "O&M")

  regional_multipliers <- base_job_intensity %>%
    filter(
      .data$category %in% categories,
      !.data$fuel %in% c("DACCS", "BECCS Base", "BECCS CCS"),
      is.finite(.data$jobs_per_unit),
      .data$jobs_per_unit > 0
    ) %>%
    group_by(category, fuel) %>%
    mutate(
      # Equation: relative_intensity[r,f,c] = intensity[r,f,c] / median_region(intensity[f,c]).
      # Taking ratios removes technology-specific units while retaining the tool's regional productivity pattern.
      technology_median = median(jobs_per_unit, na.rm = TRUE),
      relative_intensity = jobs_per_unit / technology_median
    ) %>%
    ungroup() %>%
    group_by(category, Region) %>%
    summarise(
      # Equation: regional_multiplier[r,c] = median_fuel(relative_intensity[r,f,c]).
      # The median limits the influence of technology-specific outliers in the existing database.
      regional_multiplier = median(relative_intensity, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(category) %>%
    mutate(
      usa_multiplier = case_when(
        any(Region == "USA") ~ regional_multiplier[match("USA", Region)],
        any(Region == "United States of America") ~ regional_multiplier[match("United States of America", Region)],
        TRUE ~ 1
      ),
      # Equation: normalized_multiplier[r,c] = regional_multiplier[r,c] / regional_multiplier[USA,c].
      # Rhodium's employment factors are US-based, so the US is the reference productivity level.
      regional_multiplier = regional_multiplier / usa_multiplier
    ) %>%
    ungroup() %>%
    select(category, Region, regional_multiplier)

  # Source URLs, units, and scopes are stored with each numeric value in the technology assumptions CSV.
  source_intensities <- assumptions %>%
    filter(.data$parameter == "jobs_intensity") %>%
    transmute(category, source_jobs_per_unit = as.numeric(value))

  tidyr::crossing(
    Region = unique(base_job_intensity$Region),
    category = categories
  ) %>%
    left_join(source_intensities, by = "category") %>%
    left_join(regional_multipliers, by = c("category", "Region")) %>%
    mutate(
      regional_multiplier = coalesce(regional_multiplier, 1),
      # Equation: CDR intensity[r,c] = US source intensity[c] * regional productivity multiplier[r,c].
      jobs_per_unit = source_jobs_per_unit * regional_multiplier,
      fuel = .env$fuel
    ) %>%
    select(category, fuel, jobs_per_unit, Region)
}


create_daccs_regional_job_intensity <- function(base_job_intensity, assumptions) {
  create_cdr_regional_job_intensity(base_job_intensity, assumptions, fuel = "DACCS")
}


# BECCS base-plant activity uses the existing Biomass coefficients. This preserves the jobs that
# the previous aggregate Biomass calculation already assigned to power capacity equipped with CCS.
create_beccs_base_job_intensity <- function(base_job_intensity) {
  base_job_intensity %>%
    filter(
      .data$fuel == "Biomass",
      .data$category %in% c("Construction", "Manufacturing", "O&M")
    ) %>%
    mutate(fuel = "BECCS Base")
}


create_beccs_ccs_regional_job_intensity <- function(base_job_intensity, assumptions) {
  create_cdr_regional_job_intensity(base_job_intensity, assumptions, fuel = "BECCS CCS")
}


# Read one IAMC variable as annual activity and enforce the unit required by the jobs coefficient.
prepare_iamc_activity <- function(iamc_input_data, variable, expected_unit, value_name) {
  rows <- iamc_input_data %>% filter(.data$Variable == .env$variable)

  if (nrow(rows) == 0) {
    return(NULL)
  }

  invalid_units <- unique(rows$Unit[!is.na(rows$Unit) & rows$Unit != expected_unit])
  if (length(invalid_units) > 0) {
    stop(
      variable, " must use '", expected_unit, "'. Found: ",
      paste(invalid_units, collapse = ", "),
      call. = FALSE
    )
  }

  rows %>%
    select(-any_of(c("scenario_model", "Unit"))) %>%
    pivot_longer(
      cols = matches("^[0-9]{4}$"),
      names_to = "year",
      values_to = value_name
    ) %>%
    mutate(year = as.integer(year))
}


# Translate OPEN-PROM power-sector BECCS capacity and CO2 capture into CCS activity rows.
prepare_beccs_ccs_job_activity <- function(iamc_input_data, assumptions) {
  capacity_variable <- "Capacity|Electricity|BECCS Base"
  additions_variable <- "Capacity Additions|Electricity|BECCS Base"
  capture_variable <- "Carbon Capture|Energy|Supply|Electricity|Biofuels"

  capacity <- prepare_iamc_activity(
    iamc_input_data, capacity_variable, "GW", "beccs_capacity_gw"
  )
  additions <- prepare_iamc_activity(
    iamc_input_data, additions_variable, "GW/yr", "beccs_additions_gw"
  )
  capture <- prepare_iamc_activity(
    iamc_input_data, capture_variable, "Mt CO2/yr", "power_capture_mt_per_year"
  )

  if (is.null(capacity) || is.null(additions) || is.null(capture)) {
    warning(
      "OPEN-PROM power-sector BECCS capacity, additions, or capture data are incomplete; ",
      "incremental BECCS CCS jobs will not be calculated."
    )
    return(NULL)
  }

  construction_period <- as.integer(
    get_beccs_assumption(assumptions, "construction_period")
  )
  if (construction_period < 1) {
    stop("BECCS construction period must be at least one year.", call. = FALSE)
  }

  keys <- c("Model", "Scenario", "Region", "year")
  beccs_activity <- capacity %>%
    select(all_of(keys), beccs_capacity_gw) %>%
    full_join(additions %>% select(all_of(keys), beccs_additions_gw), by = keys) %>%
    full_join(capture %>% select(all_of(keys), power_capture_mt_per_year), by = keys) %>%
    arrange(Model, Scenario, Region, year) %>%
    group_by(Model, Scenario, Region) %>%
    mutate(
      beccs_capacity_gw = pmax(as.numeric(beccs_capacity_gw), 0),
      beccs_additions_gw = pmax(as.numeric(beccs_additions_gw), 0),
      power_capture_mt_per_year = pmax(as.numeric(power_capture_mt_per_year), 0),
      # Equation: capture_rate[t] = power-sector biomass CO2 capture[t] / BECCS capacity[t].
      # OPEN-PROM supplies both quantities, allowing the conversion to retain regional plant performance.
      capture_rate_mt_per_gw = if_else(
        beccs_capacity_gw > 0,
        power_capture_mt_per_year / beccs_capacity_gw,
        NA_real_
      ),
      # A same-region nearest value covers commissioning years where additions occur before a positive
      # operating-capacity ratio is reported. The original OPEN-PROM ratios remain unchanged where present.
      capture_rate_mt_per_gw = zoo::na.locf(capture_rate_mt_per_gw, na.rm = FALSE),
      capture_rate_mt_per_gw = zoo::na.locf(capture_rate_mt_per_gw, fromLast = TRUE, na.rm = FALSE),
      # Equation: commissioned_capture_capacity[t] = electrical additions[t] * capture_rate[t].
      commissioned_capture_capacity = beccs_additions_gw * capture_rate_mt_per_gw,
      # Equation: capacity_under_construction[t] = sum of capture-capacity cohorts commissioned
      # from t through t + T - 1. Rhodium assumes four construction years for a BECCS project.
      # Source: https://rhg.com/research/the-benefits-of-innovation-an-assessment-of-the-economic-opportunities-of-highly-durable-carbon-dioxide-removal/
      capture_capacity_under_construction = compute_capacity_under_construction(
        commissioned_capture_capacity,
        construction_period
      )
    ) %>%
    ungroup()

  missing_rates <- beccs_activity %>%
    filter(.data$beccs_additions_gw > 0, is.na(.data$capture_rate_mt_per_gw))
  if (nrow(missing_rates) > 0) {
    stop(
      "BECCS capacity additions were found without a usable power-sector CO2 capture rate.",
      call. = FALSE
    )
  }

  # Rhodium reports average annual project-investment jobs during construction. Construction and
  # Manufacturing therefore use capture capacity under construction. O&M uses operating capture.
  bind_rows(
    beccs_activity %>%
      transmute(
        Model, Scenario, Region, year,
        fuel = "BECCS CCS", category = "Construction",
        energyquantity = capture_capacity_under_construction
      ),
    beccs_activity %>%
      transmute(
        Model, Scenario, Region, year,
        fuel = "BECCS CCS", category = "Manufacturing",
        energyquantity = capture_capacity_under_construction
      ),
    beccs_activity %>%
      transmute(
        Model, Scenario, Region, year,
        fuel = "BECCS CCS", category = "O&M",
        energyquantity = power_capture_mt_per_year
      )
  )
}



#main function to compute energy jobs for iiasadb template
compute_energy_jobs_iiasadb <- function(iamc_input_data, na_threshold = 0.60, technical_progress=T, regional_convergence=T, manufacturing="current_shares", additional_job_activity=NULL){
#na_treshold: threshold above which number of rows per model/scenario are included default: 0.6 or 60% of all entries
#manufacturing: "current_shares" or "domestic", manufacturing redistribution of job counts based on current production shares or where deployed
#technical_progress: progress in renewables based on van der Zwaan et al., 2013
#regional_convergence: convergence of productivity to the minimum level by 2100

  
# Technology
tech <- c("Biomass", "BECCS Base", "Coal", "Gas", "Hydro", "Nuclear", "Oil", "Solar|CSP", "Solar|PV", "Wind|Offshore", "Wind|Onshore")
# Category 
#### Construction & Manufacturing --> Capacity Additions
const_manu <- "Capacity Additions|Electricity|"
const_manu <- paste0(const_manu,tech) # Add ALL TECH
#### O&M -> Capacity
oam <- "Capacity|Electricity|"
oam <- paste0(oam,tech) # Add ALL TECH
#### Fuel production --> Extraction
fuelprod <- c("Resource|Extraction|Coal",
              "Resource|Extraction|Gas",
              "Resource|Extraction|Oil")
#### Refinery -> Secondary Energy: Biomass and Oil 
refine <- c("Secondary Energy|Liquids|Biomass", "Secondary Energy|Liquids|Oil")
# Combine variables for energy quantity #
combine <- c(const_manu, oam, fuelprod, refine)


########## STEP 1: Adjusting input data for missing values
#### Work-Around: Add extraction values for those missing models without reporting Extraction
scenarios_no_extraction <- setdiff(unique(iamc_input_data$scenario_model), unique((iamc_input_data %>% group_by(Model, Scenario, Region) %>% filter(str_detect(Variable, "^Resource[|]Extraction[|]")) %>% filter(!is.na(2050)) %>% ungroup())$scenario_model))
extract_issue <- iamc_input_data %>%
  filter(scenario_model %in% scenarios_no_extraction) %>%
  filter(grepl("Trade[|]Primary Energy[|]Coal[|]Volume|Trade[|]Primary Energy[|]Gas[|]Volume|Trade[|]Primary Energy[|]Oil[|]Volume|^Primary Energy[|]Coal$|^Primary Energy[|]Gas$|^Primary Energy[|]Oil$", Variable)) %>% 
  pivot_longer(-c(Model, Scenario, Region, Variable, Unit, scenario_model), names_to = "year", values_to = "energyquantity" ) %>% 
  pivot_wider(id_cols=c(Model, Scenario, Region, year, Unit, scenario_model), names_from = Variable, values_from = energyquantity) %>% 
  mutate(`Resource|Extraction|Coal` = `Primary Energy|Coal` + `Trade|Primary Energy|Coal|Volume`) %>% 
  mutate(`Resource|Extraction|Gas`  = `Primary Energy|Gas`  + `Trade|Primary Energy|Gas|Volume`) %>% 
  mutate(`Resource|Extraction|Oil`  = `Primary Energy|Oil`  + `Trade|Primary Energy|Oil|Volume`) %>% 
  select(-c(7:12)) %>% 
  pivot_longer(-c(Model, Scenario, Region, year, Unit, scenario_model), names_to = "Variable", values_to = "energyquantity" ) %>% 
  pivot_wider(id_cols=c(Model, Scenario, Region, Variable, Unit, scenario_model), names_from = "year", values_from = "energyquantity" )
iamc_input_data <- rbind(iamc_input_data, extract_issue)  

########## STEP 2: Filter and convert scenario data with selected variables
energy_jobs <- iamc_input_data %>% 
  select(-scenario_model) %>% 
  filter(Variable %in% combine) %>% 
  select(-Unit) %>% 
  pivot_longer(-c(Model, Scenario, Region, Variable), names_to = "year", values_to = "energyquantity" ) %>% 
  mutate(Variable = str_replace(Variable,'Solar[|]PV','Solar (PV)')) %>% 
  mutate(Variable = str_replace(Variable,'Solar[|]CSP','Solar (CSP)')) %>% 
  mutate(Variable = str_replace(Variable,'Wind[|]Offshore','Wind (OFF)')) %>% 
  mutate(Variable = str_replace(Variable,'Wind[|]Onshore','Wind (ON)')) %>% 
  mutate(Variable = str_replace(Variable,'Capacity[|]Electricity','O&M')) %>% 
  mutate(Variable = str_replace(Variable,'Capacity Additions[|]Electricity','Construction_Manufacturing')) %>% 
  mutate(Variable = str_replace(Variable,'Resource[|]Extraction','Extraction')) %>% 
  mutate(Variable = str_replace(Variable,'Secondary Energy[|]Liquids','Refinery')) %>% 
  separate(col = Variable, into = c("category", "fuel"), sep = "[|]") %>% 
  pivot_wider(id_cols=c(Model, Scenario, Region, fuel, year), names_from = category, values_from = energyquantity) %>% 
  mutate(Manufacturing = Construction_Manufacturing) %>% 
  rename(Construction = Construction_Manufacturing) %>% 
  pivot_longer(-c(Model, Scenario, Region, fuel, year), names_to = "category", values_to = "energyquantity") %>%
  mutate(year = as.integer(year))

# Non-power technologies can provide standardized activity rows and then use the same downstream
# intensity, technical-progress, regional-convergence, and jobs equations as the power technologies.
if(!is.null(additional_job_activity)){
energy_jobs <- bind_rows(energy_jobs, additional_job_activity)
}

########## STEP 3: compute total energy jobs and store data in iamc format
energy_jobs <- merge(energy_jobs, energy_jobs_iiasadb_regions, by = c("category", "fuel", "Region"), all = T)

#check which scenarios and models have too many NAs
energy_jobs <- energy_jobs %>% left_join(energy_jobs %>% filter(year==2050) %>% group_by(Model, Scenario) %>% summarize(share_na = sum(!is.na(energyquantity)) / n())) %>% filter(share_na>=na_threshold) %>% select(-share_na)

#OPTION 1: Redistribute manufacturing renewable equipment shares according to today's values
if(manufacturing=="current_shares"){
global_shares <- read.csv(("global_production_shares.csv"), check.names = FALSE)
global_shares <- full_join(global_shares, region_mappings, relationship = "many-to-many") %>%
  mutate(Region_ID = str_extract(Region, "R+[0-9]+")) %>%
  mutate(Region_ID = coalesce(Region_ID,Region)) %>%
  mutate(fuel = str_replace_all(fuel, "solar_pv", "Solar (PV)")) %>%
  mutate(fuel = str_replace_all(fuel, "nuclear", "Nuclear")) %>%
  mutate(category = str_replace_all(category, "manufacturing", "Manufacturing")) %>%
  mutate(category = str_replace_all(category, "extraction", "Extraction")) %>%
  group_by(fuel, category, Region, Region_ID) %>%
  summarise(sum_share = if(all(is.na(value))) NA_real_ else sum(value, na.rm = T))

redistribute <- energy_jobs %>%
  filter(category == "Manufacturing" & fuel == "Solar (PV)" | category == "Extraction" & fuel == "Nuclear") %>%
  mutate(Region_ID = str_extract(Region, "R+[0-9]+")) %>%
  mutate(Region_ID = coalesce(Region_ID,Region)) %>%
  group_by(category, fuel, Model, Scenario, Region_ID, year) %>%
  summarise(energyquantity = if(all(is.na(energyquantity))) NA_real_ else sum(energyquantity, na.rm = T))

# Redistribute according to key
redistribute <- full_join(redistribute, global_shares, relationship = "many-to-many") %>%
  mutate(energyquantity_red = energyquantity * (sum_share/100)) %>%
  ungroup(Region_ID) %>%
  select(-c(sum_share, Region_ID, energyquantity))

energy_jobs <- right_join(redistribute, energy_jobs) %>%
  mutate(energyquantity = ifelse(category == "Manufacturing" & fuel == "Solar (PV)" | category == "Extraction" & fuel == "Nuclear", energyquantity_red,energyquantity)) %>%
  select(-energyquantity_red)
}

#OPTION 2: adding technical progress in reducing enegry jobs intensity anually by 0.75% (van der Zwaan et al., 2013) for renewables and 0 for other technologies
if(technical_progress){
techprogress <- read_csv(I(
 "fuel, progress
  Wind (OFF), 0.0075
  Wind (ON), 0.0075
  Nuclear, 0.00
  Hydro, 0.00
  Coal, 0.00
  Oil, 0.00
  Gas, 0.00
  Biomass, 0.00
  BECCS Base, 0.00
  BECCS CCS, 0.00
  Solar (CSP), 0.0075
  Solar (PV), 0.0075
  DACCS, 0.00"
 ), show_col_types = FALSE)
# DACCS defaults to 0% annual labor-productivity progress because OPEN-PROM's 25% cost reduction
# per cumulative-capacity doubling is not evidence of the same reduction in labor requirements.
# Set DACCS to 0.0075 above to run the same annual sensitivity currently used for solar and wind.
# BECCS Base retains the existing Biomass assumption. BECCS CCS also defaults to 0% because the
# Rhodium employment range does not provide an annual labor-productivity trend. Set BECCS CCS to
# 0.0075 above to run the same optional sensitivity used for solar, wind, and documented for DACCS.
#apply learning per technology until 2050, then constant for now
energy_jobs <- energy_jobs %>% left_join(techprogress) %>% mutate(jobs_per_unit = ifelse(year>2020, jobs_per_unit*(1+progress)^(-(pmin(year,2050)-2020)), jobs_per_unit)) %>% select(-progress)
}

#OPTION 3: Convergence of jobs intensity to the minimum level by 2100
if(regional_convergence){
  year_start <- 2020
  year_convergence <- 2100

  convergence_targets <- energy_jobs %>%
    filter(year == year_convergence) %>%
    group_by(category, fuel, Model, Scenario) %>%
    summarise(
      minimum_jobs_per_unit = if (all(is.na(jobs_per_unit))) {
        NA_real_
      } else {
        min(jobs_per_unit, na.rm = TRUE)
      },
      .groups = "drop"
    )

  energy_jobs <- energy_jobs %>%
    left_join(convergence_targets,
              by = c("category", "fuel", "Model", "Scenario")) %>%
    group_by(category, fuel, Model, Scenario, Region) %>%
    mutate(
      baseline_jobs_per_unit = first(
        jobs_per_unit[year == year_start],
        default = NA_real_
      ),
      jobs_per_unit = case_when(
        is.na(jobs_per_unit) ~ jobs_per_unit,
        is.na(baseline_jobs_per_unit) | is.na(minimum_jobs_per_unit) ~ jobs_per_unit,
        year >= year_convergence ~ minimum_jobs_per_unit,
        year >= year_start ~ baseline_jobs_per_unit *
          (minimum_jobs_per_unit / baseline_jobs_per_unit) ^
          ((year - year_start) / (year_convergence - year_start)),
        TRUE ~ jobs_per_unit
      )
    ) %>%
    ungroup() %>%
    select(-minimum_jobs_per_unit, -baseline_jobs_per_unit)
}

# Equation used for every technology component: jobs[r,t,c] = activity[r,t,c] * jobs_intensity[r,t,c].
energy_jobs$jobs = energy_jobs$energyquantity*energy_jobs$jobs_per_unit

# Retain the calculation components for auditing. Reporting combines the base biomass plant and
# incremental capture chain under BECCS after each has passed through the common jobs engine.
assign("energy_jobs_components", energy_jobs, envir = .GlobalEnv)
energy_jobs <- energy_jobs %>%
  mutate(
    fuel = if_else(.data$fuel %in% c("BECCS Base", "BECCS CCS"), "BECCS", .data$fuel)
  ) %>%
  group_by(Model, Scenario, Region, fuel, category, year) %>%
  summarise(
    jobs = if (all(is.na(jobs))) NA_real_ else sum(jobs, na.rm = TRUE),
    .groups = "drop"
  )
# Total Energy Jobs
total_jobs <- energy_jobs %>% group_by(Model, Scenario, Region, year) %>% summarise(jobs = as.integer(if(all(is.na(jobs))) NA_real_ else sum(jobs, na.rm =TRUE)))
#Temporary storage in separate DFs
assign("total_jobs", total_jobs, envir = .GlobalEnv)
assign("energy_jobs", energy_jobs, envir = .GlobalEnv)

energy_jobs_iamc_format <-   as.data.frame(rbind(
total_jobs %>% mutate(Variable="Energy Jobs|Total") %>% pivot_wider(id_cols = c(Region, Model, Scenario, Variable), names_from = "year", values_from = "jobs"),
energy_jobs %>% mutate(Variable=paste0("Energy Jobs|",category,"|",fuel, "|Total")) %>% pivot_wider(id_cols = c(Region, Model, Scenario, Variable), names_from = "year", values_from = "jobs")
))
return(energy_jobs_iamc_format)
}







######### Run processing
#1) compute jobs intensity for desired iiasadb regions (run only once!)
energy_jobs_iiasadb_regions <- create_iiasadb_job_intensity(region_mappings = region_mappings, energy_jobs_iso3 = energy_jobs_iso3)

#2) apply to the loaded  IAMC snapshot 
## exemplary csv file of all illustrative pathways (8 in R10, 9 in R10)
iamc_input_data <- read.csv(file = file.path("iamc_input_data.csv"), check.names = FALSE)

energy_jobs_iamc <- compute_energy_jobs_iiasadb(iamc_input_data = iamc_input_data, na_threshold = 0.60, technical_progress=T, regional_convergence=T, manufacturing="current_shares")


# PLOT with IMP test data
ggplot(energy_jobs %>% filter(str_detect(Region, "R5")) %>% group_by(Model, fuel, category, year) %>% summarize(jobs=sum(jobs, na.rm = T)) %>% filter(year %in% c("2020", "2030","2040","2050"))) + geom_bar(aes(Model, jobs*1e-6, fill=category), position="stack", stat="identity") + scale_fill_brewer(palette = "Spectral") + xlab("") + facet_wrap(. ~ year, nrow = 1) + ylab("Jobs [in millions]") + labs(fill = "Category") + theme(legend.position = "bottom") + ggtitle("Energy Jobs by Category") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#plot jobs intensity for one scenario
ggplot(energy_jobs %>% filter(Scenario=="EN_NPi2020_900f" & Model=="AIM/CGE 2.2")) + geom_line(aes(year, log(jobs_per_unit), color=Region)) + facet_grid(category ~ fuel)
#look at renewables manufacturing
ggplot(energy_jobs %>% filter(category=="Manufacturing" & (str_detect(fuel, "Solar") | str_detect(fuel, "Wind")) & year==2050) %>% filter(Scenario=="EN_NPi2020_900f" & Model=="AIM/CGE 2.2")) + geom_bar(aes(year, jobs*1e-6, fill=Region), stat="identity") + facet_grid(Model ~ Scenario)




