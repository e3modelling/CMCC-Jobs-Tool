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
  read.csv("regmap_R10.csv",col.names = c("Region","iso3")) %>%
    mutate(across(everything(),str_trim)), 
  read.csv("regmap_R5.csv",col.names = c("Region","iso3")) %>%
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
# This does weigthed mean over region, fuel, category and keeps one value for all in group, then keeps only distinct
energy_jobs_iso3 <- energy_jobs_iso3 %>% group_by(fuel, category, Region) %>% mutate(value = weighted.mean(value, w = energyquantity+1e-12, na.rm = T)) %>% rename(jobs_per_unit = value) %>% select("category", "fuel", "jobs_per_unit", "Region") %>% distinct()
#convert units to match IAMC template: capacity are already GW, PJ energy to EJ
energy_jobs_iso3 <- energy_jobs_iso3 %>% mutate(jobs_per_unit = ifelse(category %in% c("Extraction", "Refinery"), jobs_per_unit * 1e+3, jobs_per_unit))
return(as.data.frame(energy_jobs_iso3))
}


#main function to compute energy jobs for iiasadb template
compute_energy_jobs_iiasadb <- function(iamc_input_data){

# Technology
tech <- c("Biomass", "Coal", "Gas", "Hydro", "Nuclear", "Oil", "Solar|CSP", "Solar|PV", "Wind|Offshore", "Wind|Onshore")
# Category 
#### Construction & Manufacturing --> Capacity Additions
const_manu <- "Capacity Additions|Electricity|"
const_manu <- paste0(const_manu,tech) # Add ALL TECH
#### O&M -> Capacity
oam <- "Capacity|Electricity|"
oam <- paste0(oam,tech) # Add ALL TECH
#### Fuel production --> Extraction: Coal, Gas, Oil
fuelprod <- unique(iamc_input_data$Variable[grepl("Resource[|]Extraction[|]", iamc_input_data$Variable)])
#### Refinery -> Secondary Energy: Biomass and Oil 
refine <- c("Secondary Energy|Liquids|Biomass", "Secondary Energy|Liquids|Oil")
# Combine variables for energy quantity #
combine <- c(const_manu, oam, fuelprod, refine)

########## STEP 1: Adjusting input data for missing values
#### Work-Around 1: Add extraction values for those missing models without reporting Extraction
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

#### Work-Around 2: Get Capacity for LED Scenario & for R-CurPol recover Oil
# Scenarios not reporting Cap
# scenarios_no_cap <- setdiff(unique(iamc_input_data$scenario_model), unique((iamc_input_data %>% group_by(Model, Scenario, Region) %>% filter(str_detect(Variable, "Capacity[|]Electricity[|]")) %>% filter(!is.na(2050)) %>% ungroup())$scenario_model))
gener <- "Secondary Energy|Electricity|"
gener <- paste0(gener,tech) # Add ALL TECH

led_original <- read.csv(file = file.path("led_original.csv"), check.names = FALSE) %>% 
  mutate(SCENARIO = str_replace(SCENARIO,'LowEnergyDemand_1.1','LowEnergyDemand_1.3_IPCC')) %>% 
  filter(VARIABLE %in% oam | VARIABLE %in% gener) %>% 
  filter(REGION != "World") %>% 
  rename_with(str_to_title) %>% 
  select(-Unit) %>%  
  pivot_longer(-c(Model, Scenario, Region, Variable), names_to = "year", values_to = "energyquantity" ) %>%
  pivot_wider(id_cols=c(Model, Scenario, Region, year), names_from = Variable, values_from = energyquantity) %>%
  select(Model, Scenario, Region, year, order(colnames(.))) %>% 
  mutate(year = as.integer(year)) %>% 
  # Exclude Secondary == 0 for mathematical reasons, year < 2020 beacuse LED Original not reporting Capacity there
  mutate(`Factor|Biomass`        =ifelse(year < 2020, NA_real_, ifelse(`Secondary Energy|Electricity|Biomass`       == 0, 0, `Capacity|Electricity|Biomass`        / `Secondary Energy|Electricity|Biomass`))) %>%
  mutate(`Factor|Coal`           =ifelse(year < 2020, NA_real_, ifelse(`Secondary Energy|Electricity|Coal`          == 0, 0, `Capacity|Electricity|Coal`           / `Secondary Energy|Electricity|Coal`))) %>%
  mutate(`Factor|Gas`            =ifelse(year < 2020, NA_real_, ifelse(`Secondary Energy|Electricity|Gas`           == 0, 0, `Capacity|Electricity|Gas`            / `Secondary Energy|Electricity|Gas`))) %>%
  mutate(`Factor|Hydro`          =ifelse(year < 2020, NA_real_, ifelse(`Secondary Energy|Electricity|Hydro`         == 0, 0, `Capacity|Electricity|Hydro`          / `Secondary Energy|Electricity|Hydro`))) %>%
  mutate(`Factor|Nuclear`        =ifelse(year < 2020, NA_real_, ifelse(`Secondary Energy|Electricity|Nuclear`       == 0, 0, `Capacity|Electricity|Nuclear`        / `Secondary Energy|Electricity|Nuclear`))) %>%
  mutate(`Factor|Oil`            =ifelse(year < 2020, NA_real_, ifelse(`Secondary Energy|Electricity|Oil`           == 0, 0, `Capacity|Electricity|Oil`            / `Secondary Energy|Electricity|Oil`))) %>%
  mutate(`Factor|Solar|PV`       =ifelse(year < 2020, NA_real_, ifelse(`Secondary Energy|Electricity|Solar|PV`      == 0, 0, `Capacity|Electricity|Solar|PV`       / `Secondary Energy|Electricity|Solar|PV`))) %>%
  mutate(`Factor|Wind|Onshore`   =ifelse(year < 2020, NA_real_, ifelse(`Secondary Energy|Electricity|Wind|Onshore`  == 0, 0, `Capacity|Electricity|Wind|Onshore`   / `Secondary Energy|Electricity|Wind|Onshore`))) %>%
  mutate(`Factor|Solar|CSP`      =ifelse(year < 2020, NA_real_, ifelse(`Secondary Energy|Electricity|Solar|CSP`     == 0, 0, `Capacity|Electricity|Solar|CSP`      / `Secondary Energy|Electricity|Solar|CSP`))) %>%
  mutate(`Factor|Wind|Offshore`  =ifelse(year < 2020, NA_real_, ifelse(`Secondary Energy|Electricity|Wind|Offshore` == 0, 0, `Capacity|Electricity|Wind|Offshore`  / `Secondary Energy|Electricity|Wind|Offshore`))) %>%
  select(-c(5:24)) %>% 
  pivot_longer(-c(Model, Scenario, Region, year), names_to = "Variable", values_to = "energyquantity" ) %>% 
  pivot_wider(id_cols=c(Model, Scenario, Variable, year), names_from = Region, values_from = energyquantity) %>% 
  rename(`R5OECD90+EU` = R2_North, R5ASIA = R2_South) %>% 
  mutate(R5LAM = R5ASIA, R5MAF = R5ASIA, R5REF = R5ASIA) %>% 
  pivot_longer(-c(Model, Scenario, year, Variable), names_to = "Region", values_to = "energyquantity" ) %>% 
  pivot_wider(id_cols=c(Model, Scenario, Region, year), names_from = Variable, values_from = energyquantity)
  
led <- iamc_input_data %>%
  filter(Scenario == "LowEnergyDemand_1.3_IPCC") %>% 
  filter(Variable %in% gener) %>% 
  select(-Unit) %>% 
  pivot_longer(-c(Model, Scenario, Region, Variable, scenario_model), names_to = "year", values_to = "energyquantity" ) %>%
  pivot_wider(id_cols=c(Model, Scenario, Region, year, scenario_model), names_from = Variable, values_from = energyquantity) 

led_final <- merge(led, led_original) %>% 
  select(Model, Scenario, Region, year, scenario_model, order(colnames(.))) %>% 
  mutate(`Capacity|Electricity|Biomass`        = `Factor|Biomass`        * `Secondary Energy|Electricity|Biomass`) %>%
  mutate(`Capacity|Electricity|Coal`           = `Factor|Coal`           * `Secondary Energy|Electricity|Coal`) %>%
  mutate(`Capacity|Electricity|Gas`            = `Factor|Gas`            * `Secondary Energy|Electricity|Gas`) %>%
  mutate(`Capacity|Electricity|Hydro`          = `Factor|Hydro`          * `Secondary Energy|Electricity|Hydro`) %>%
  mutate(`Capacity|Electricity|Nuclear`        = `Factor|Nuclear`        * `Secondary Energy|Electricity|Nuclear`) %>%
  mutate(`Capacity|Electricity|Oil`            = `Factor|Oil`            * `Secondary Energy|Electricity|Oil`) %>%
  mutate(`Capacity|Electricity|Solar|PV`       = `Factor|Solar|PV`       * `Secondary Energy|Electricity|Solar|PV`) %>%
  mutate(`Capacity|Electricity|Wind|Onshore`   = `Factor|Wind|Onshore`   * `Secondary Energy|Electricity|Wind|Onshore`) %>%
  mutate(`Capacity|Electricity|Solar|CSP`      = `Factor|Solar|CSP`      * `Secondary Energy|Electricity|Solar|CSP`) %>%
  mutate(`Capacity|Electricity|Wind|Offshore`  = `Factor|Wind|Offshore`  * `Secondary Energy|Electricity|Wind|Offshore`) %>% 
  select(-c(6:25)) %>% 
  pivot_longer(-c(Model, Scenario, Region, year, scenario_model), names_to = "Variable", values_to = "energyquantity" ) %>% 
  pivot_wider(id_cols=c(Model, Scenario, Region, Variable, scenario_model), names_from = "year", values_from = "energyquantity" )

# Filling in 2010 values from MESSAGE-GLOBIOM 1.0 - ADVANCE_Reference (no other MESSAGE-GLOBIOMix 1.0 with Capacity before 2020)
led_cap2010 <- read.csv(file = file.path("r5_mes_oam.csv"), check.names = FALSE) %>% 
  rename(filler = `2010`) %>% 
  select(-c(Model, Scenario))

led_final <- merge(led_final, led_cap2010) %>% 
  mutate(`2010` = filler) %>% 
  select(-filler)


iamc_input_data <- plyr::rbind.fill(iamc_input_data,led_final)


scenarios_no_capacity_oil <- setdiff(unique(iamc_input_data$scenario_model), unique((iamc_input_data %>% group_by(Model, Scenario, Region) %>% filter(str_detect(Variable, "^Capacity[|]Electricity[|]Oil$")) %>% filter(!is.na(2050)) %>% ungroup())$scenario_model))
cap_oil_issue <- iamc_input_data %>%
  filter(scenario_model %in% scenarios_no_capacity_oil) %>%
  filter(grepl("Capacity[|]Electricity[|]Oil", Variable)) %>% 
  mutate(Variable = "Capacity|Electricity|Oil")

iamc_input_data <- rbind(iamc_input_data, cap_oil_issue)  


#### Work-Around 3: Capacity Additions through Difference in Capacity
# Models not reporting Capadd
scenarios_no_capadd <- setdiff(unique(iamc_input_data$scenario_model), unique((iamc_input_data %>% group_by(Model, Scenario, Region) %>% filter(str_detect(Variable, "Capacity Additions[|]Electricity[|]")) %>% filter(!is.na(2050)) %>% ungroup())$scenario_model))
capadd_missing <- iamc_input_data %>%
  filter(scenario_model %in% scenarios_no_capadd) %>%
  filter(Variable %in% oam) %>% 
  select(-Unit) %>% 
  pivot_longer(-c(Model, Scenario, Region, Variable, scenario_model), names_to = "year", values_to = "energyquantity" ) %>%
  pivot_wider(id_cols=c(Model, Scenario, Region, year, scenario_model), names_from = Variable, values_from = energyquantity) %>% 
  mutate(year = as.numeric(year)) 


# ALTERNATIVE 2: Fixed Exponential Retirement Rates
lifetimes <- c(
  "technology, lifetime
  Wind (OFF), 30
  Wind (ON), 30
  Nuclear, 40
  Hydro, 45
  Coal, 40
  Oil, 25
  Gas, 25
  Biomass, 25
  Solar (CSP), 22
  Solar (PV), 22"
)

capadd_retexp <- read_csv(lifetimes) %>% 
  mutate(deprec = 1/(lifetime-(0.01/2)*lifetime^2)) %>% 
  arrange(technology) %>% 
  select(-lifetime) %>% 
  pivot_wider(names_from = technology, values_from = deprec) %>% 
  set_names(c("retire_biomass",
              "retire_coal",
              "retire_gas",
              "retire_hydro",
              "retire_nuclear",
              "retire_oil",
              "retire_solarcsp",
              "retire_solarpv",
              "retire_windoff",
              "retire_windon"))

capadd_fill_retexp <- cbind(capadd_missing, capadd_retexp) %>% 
  # CALCULATING CAPACITY ADDITIONS AS DIFFERENCE BETWEEN CAPACITY WITH RETIREMENT RATE as Cap,t+1 - (1-DR,t)*Cap,t
  group_by(Model, Scenario, Region, scenario_model) %>%
  arrange(year, .by_group = TRUE) %>%
  na.omit() %>% 
  mutate(`Capacity Additions|Electricity|Biomass`        = `Capacity|Electricity|Biomass`        - (1 - lag(retire_biomass,     default = NA_real_))   * lag(`Capacity|Electricity|Biomass`,          default = first(`Capacity|Electricity|Biomass`))) %>%
  mutate(`Capacity Additions|Electricity|Coal`           = `Capacity|Electricity|Coal`           - (1 - lag(retire_coal,        default = NA_real_))   * lag(`Capacity|Electricity|Coal`,             default = first(`Capacity|Electricity|Coal`))) %>%
  mutate(`Capacity Additions|Electricity|Gas`            = `Capacity|Electricity|Gas`            - (1 - lag(retire_gas,         default = NA_real_))   * lag(`Capacity|Electricity|Gas`,              default = first(`Capacity|Electricity|Gas`))) %>%
  mutate(`Capacity Additions|Electricity|Hydro`          = `Capacity|Electricity|Hydro`          - (1 - lag(retire_hydro,       default = NA_real_))   * lag(`Capacity|Electricity|Hydro`,            default = first(`Capacity|Electricity|Hydro`))) %>%
  mutate(`Capacity Additions|Electricity|Nuclear`        = `Capacity|Electricity|Nuclear`        - (1 - lag(retire_nuclear,     default = NA_real_))   * lag(`Capacity|Electricity|Nuclear`,          default = first(`Capacity|Electricity|Nuclear`))) %>%
  mutate(`Capacity Additions|Electricity|Oil`            = `Capacity|Electricity|Oil`            - (1 - lag(retire_oil,         default = NA_real_))   * lag(`Capacity|Electricity|Oil`,              default = first(`Capacity|Electricity|Oil`))) %>%
  mutate(`Capacity Additions|Electricity|Solar|CSP`      = `Capacity|Electricity|Solar|CSP`      - (1 - lag(retire_solarcsp,    default = NA_real_))   * lag(`Capacity|Electricity|Solar|CSP`,        default = first(`Capacity|Electricity|Solar|CSP`))) %>%
  mutate(`Capacity Additions|Electricity|Solar|PV`       = `Capacity|Electricity|Solar|PV`       - (1 - lag(retire_solarpv,     default = NA_real_))   * lag(`Capacity|Electricity|Solar|PV`,         default = first(`Capacity|Electricity|Solar|PV`))) %>%
  mutate(`Capacity Additions|Electricity|Wind|Offshore`  = `Capacity|Electricity|Wind|Offshore`  - (1 - lag(retire_windoff,     default = NA_real_))   * lag(`Capacity|Electricity|Wind|Offshore`,    default = first(`Capacity|Electricity|Wind|Offshore`))) %>%
  mutate(`Capacity Additions|Electricity|Wind|Onshore`   = `Capacity|Electricity|Wind|Onshore`   - (1 - lag(retire_windon,      default = NA_real_))   * lag(`Capacity|Electricity|Wind|Onshore`,     default = first(`Capacity|Electricity|Wind|Onshore`))) %>%
  select(c(1:4,26:35)) %>% 
  # Dividing Calculated Capacity Additions by reported time frame to get per year values as for true Capacity Additions
  mutate(timeframe = year - lag(year, default = first(year))) %>% 
  ungroup() %>% 
  mutate(across(c(6:15), ~ . /timeframe)) %>% 
  select(-timeframe) %>% 
  # Set calculated capacity additions to 0 because additions cannot be negative
  pivot_longer(-c(Model, Scenario, Region, year, scenario_model), names_to = "Variable", values_to = "energyquantity") %>% 
  mutate(energyquantity = ifelse(energyquantity < 0, 0, energyquantity)) %>%
  pivot_wider(id_cols=c(Model, Scenario, Region, Variable, scenario_model), names_from = "year", values_from = "energyquantity")

iamc_input_data <- plyr::rbind.fill(iamc_input_data,capadd_fill_retexp)


#### Work-Around 4: Split Wind if not reported separately
onshore_factor <- 1
# No split Capacity
scenarios_windcap <- setdiff(unique(iamc_input_data$scenario_model), unique((iamc_input_data %>% group_by(Model, Scenario, Region) %>% filter(Variable == "Capacity|Electricity|Wind|Onshore" | Variable == "Capacity|Electricity|Wind|Offshore") %>% filter(!is.na(2050)) %>% ungroup())$scenario_model))
# No split Capacity Additions
scenarios_windcapadd <- setdiff(unique(iamc_input_data$scenario_model), unique((iamc_input_data %>% group_by(Model, Scenario, Region) %>% filter(Variable == "Capacity Additions|Electricity|Wind|Onshore" | Variable == "Capacity Additions|Electricity|Wind|Offshore") %>% filter(!is.na(2050)) %>% ungroup())$scenario_model))
wind <- iamc_input_data %>%
  filter((Variable == "Capacity|Electricity|Wind" & scenario_model %in% scenarios_windcap) | (Variable == "Capacity Additions|Electricity|Wind" & scenario_model %in% scenarios_windcapadd)) %>% 
  pivot_longer(-c(Model, Scenario, Region, Variable, Unit, scenario_model), names_to = "year", values_to = "energyquantity") %>% 
  pivot_wider(id_cols=c(Model, Scenario, Region, year, Unit, scenario_model), names_from = Variable, values_from = energyquantity) %>% 
  mutate(`Capacity|Electricity|Wind|Onshore`   = `Capacity|Electricity|Wind` * onshore_factor) %>% 
  mutate(`Capacity|Electricity|Wind|Offshore`  = `Capacity|Electricity|Wind` * (1-onshore_factor)) %>% 
  mutate(`Capacity Additions|Electricity|Wind|Onshore`   = `Capacity Additions|Electricity|Wind` * onshore_factor) %>% 
  mutate(`Capacity Additions|Electricity|Wind|Offshore`  = `Capacity Additions|Electricity|Wind` * (1-onshore_factor)) %>% 
  select(-c(`Capacity|Electricity|Wind`,`Capacity Additions|Electricity|Wind`)) %>% 
  pivot_longer(-c(Model, Scenario, Region, year, Unit, scenario_model), names_to = "Variable", values_to = "energyquantity" ) %>% 
  pivot_wider(id_cols=c(Model, Scenario, Region, Variable, Unit, scenario_model), names_from = "year", values_from = "energyquantity") %>% 
  filter((Variable == "Capacity|Electricity|Wind|Onshore" & Unit == "GW") | 
  (Variable == "Capacity|Electricity|Wind|Offshore" & Unit == "GW") |
  (Variable == "Capacity Additions|Electricity|Wind|Offshore" & Unit == "GW/yr") |
  (Variable == "Capacity Additions|Electricity|Wind|Onshore" & Unit == "GW/yr"))

iamc_input_data <- rbind(iamc_input_data, wind) %>% select(-scenario_model)

assign("iamc_input_data_worked", iamc_input_data, envir = .GlobalEnv)


########## STEP 2: Filter and convert scenario data with selected variables
energy_jobs <- iamc_input_data %>% 
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


########## STEP 3: compute total energy jobs and store data in iamc format
energy_jobs <- merge(energy_jobs, energy_jobs_iiasadb_regions, by = c("category", "fuel", "Region"), all = T)
  

#### ADDITION 1: Manufacturing - Solar PV/ Extraction - Nuclear --> Allocation to correct regions
# Sum up energy quantity per region mapping to get global figure
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

# Then redistribute according to key
redistribute <- full_join(redistribute, global_shares, relationship = "many-to-many") %>%
  mutate(energyquantity_red = energyquantity * (sum_share/100)) %>%
  ungroup(Region_ID) %>%
  select(-c(sum_share, Region_ID, energyquantity))

energy_jobs <- right_join(redistribute, energy_jobs) %>%
  mutate(energyquantity_red = ifelse(category == "Manufacturing" & fuel == "Solar (PV)" | category == "Extraction" & fuel == "Nuclear", energyquantity_red,energyquantity))

# There is no Extraction Nuclear in IMP data


#### ADDITION 2: Technical Progress/ Efficiency Assimilation
security_cut <- c("energyquantity", "energyquantity_red", "jobs_per_unit")

# Technical Progress A: Calculating jobs without technical progress
energy_jobs_base <- energy_jobs %>% 
  mutate(base_nored = energyquantity*jobs_per_unit) %>% 
  mutate(base_red = energyquantity_red*jobs_per_unit) %>% 
  select(-any_of(security_cut))


# Technical Progress B: Tech specific progress from 2020 until 2050, then flat
techprogress <- read_csv(c(
  "fuel, progress
  Wind (OFF), 0.0075
  Wind (ON), 0.0075
  Nuclear, 0.00
  Hydro, 0.00
  Coal, 0.00
  Oil, 0.00
  Gas, 0.00
  Biomass, 0.00
  Solar (CSP), 0.0075
  Solar (PV), 0.0075"
)) 

energy_jobs_linear <- merge(energy_jobs,techprogress) %>% 
  mutate(jobs_per_unit = ifelse(year>2020, jobs_per_unit*(1+progress)^(-(pmin(year,2050)-2020)), jobs_per_unit)) %>%
  mutate(linear_nored = energyquantity*jobs_per_unit) %>% 
  mutate(linear_red = energyquantity_red*jobs_per_unit) %>% 
  select(-c(progress))


# Technical Progress C: Take the minimum per technology over all regions in 2050 and go there exponentially
year_convergence <- 2050
regional_convergence = T

energy_jobs_minima <- energy_jobs %>% 
  filter(!is.na(jobs_per_unit)) %>% 
  mutate(Region_ID = str_extract(Region, "R+[0-9]+")) %>% 
  group_by(category, fuel, Model, Scenario, Region_ID) %>% 
  mutate(jobs_per_unit=ifelse(year>=year_convergence, min(jobs_per_unit[year==year_convergence]), jobs_per_unit)) %>% 
  ungroup() %>% 
  group_by(category, fuel, Model, Scenario, Region) %>% mutate(jobs_per_unit = ifelse(year>=2020 & year<year_convergence, jobs_per_unit[year==2020]/( 1 + regional_convergence*( ((jobs_per_unit[year==2020])/jobs_per_unit[year==year_convergence])^(1/(year_convergence-2020)) - 1) )^(year-2020), jobs_per_unit)) %>% 
  ungroup() %>% 
  mutate(minima_nored = energyquantity*jobs_per_unit) %>% 
  mutate(minima_red = energyquantity_red*jobs_per_unit) %>%
  select(-c(any_of(security_cut), Region_ID))


# Technical Progress D: Tech specific progress from 2020 until 2050, then flat AND take the minimum per technology over all regions in 2050 and go there exponentially
energy_jobs_linmin <- energy_jobs_linear %>% 
  filter(!is.na(jobs_per_unit)) %>% 
  mutate(Region_ID = str_extract(Region, "R+[0-9]+")) %>% 
  group_by(category, fuel, Model, Scenario, Region_ID) %>% 
  mutate(jobs_per_unit=ifelse(year>=year_convergence, min(jobs_per_unit[year==year_convergence]), jobs_per_unit)) %>% 
  ungroup() %>% 
  group_by(category, fuel, Model, Scenario, Region) %>% mutate(jobs_per_unit = ifelse(year>=2020 & year<year_convergence, jobs_per_unit[year==2020]/( 1 + regional_convergence*( ((jobs_per_unit[year==2020])/jobs_per_unit[year==year_convergence])^(1/(year_convergence-2020)) - 1) )^(year-2020), jobs_per_unit)) %>% 
  ungroup() %>% 
  mutate(linmin_nored = energyquantity*jobs_per_unit) %>% 
  mutate(linmin_red = energyquantity_red*jobs_per_unit) %>%
  select(-c(any_of(security_cut), Region_ID, linear_nored, linear_red))


#### Bind together
energy_jobs_linear <- energy_jobs_linear %>% 
  select(-c(any_of(security_cut)))

energy_jobs <- merge(energy_jobs_base, energy_jobs_linear)
energy_jobs <- merge(energy_jobs, energy_jobs_minima)
energy_jobs <- merge(energy_jobs, energy_jobs_linmin) %>% 
  mutate(category = factor(category, levels=c("Construction", "Manufacturing", "O&M", "Extraction", "Refinery"))) %>% 
  mutate(fuel = factor(fuel, levels=c("Coal", "Gas", "Oil", "Hydro", "Biomass", "Nuclear", "Solar (CSP)", "Solar (PV)", "Wind (OFF)", "Wind (ON)")))


#### Total Energy Jobs
total_jobs <- energy_jobs %>% group_by(Model, Scenario, Region, year) %>% summarise(across(c(base_nored, base_red, linear_nored, linear_red, minima_nored, minima_red, linmin_nored, linmin_red), ~ as.integer(if(all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE))))


#### Temporary storage in separate DFs
assign("total_jobs", total_jobs, envir = .GlobalEnv)
assign("energy_jobs", energy_jobs, envir = .GlobalEnv)

energy_jobs_iamc_format <-   as.data.frame(rbind(
total_jobs %>% mutate(Variable="Energy Jobs|Total") %>% pivot_wider(id_cols = c(Region, Model, Scenario, Variable), names_from = "year", values_from = "linmin_red"),
energy_jobs %>% mutate(Variable=paste0("Energy Jobs|",category,"|",fuel, "|Total")) %>% pivot_wider(id_cols = c(Region, Model, Scenario, Variable), names_from = "year", values_from = "linmin_red")
))
return(energy_jobs_iamc_format)
}




########## STEP 4: Run processing
#1) compute jobs intensity for desired iiasadb regions (run only once!)
energy_jobs_iiasadb_regions <- create_iiasadb_job_intensity(region_mappings = region_mappings, energy_jobs_iso3 = energy_jobs_iso3)

#2) apply to the loaded  IAMC snapshot 
## exemplary csv file of all illustrative pathways (8 in R10, 9 in R10)
iamc_input_data <- read.csv(file = file.path("iamc_input_data.csv"), check.names = FALSE)

energy_jobs_iamc <- compute_energy_jobs_iiasadb(iamc_input_data = iamc_input_data)
































############################ SPECIFIC ANALYSIS FOR IMPs (EDITS) ###############################################
######### Identifying IMPs
imps <- read_excel(path = "AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx", 
                   sheet = "Illustrative_Pathways",
                   range = cell_cols("A:C")) %>% 
  rename(Model = model) %>% 
  rename(Scenario = scenario) %>% 
  mutate(IMP_marker = factor(IMP_marker, levels=c("CurPol", "ModAct", "Neg-2.0", "Ren-2.0", "GS", "Neg", "Ren", "LD", "SP"))) %>% 
  mutate(Type = "imp")

baselines <- read_csv(c(
  "Model, Scenario, IMP_marker
    COFFEE 1.1, EN_NPi2100, Neg
    REMIND-MAgPIE 2.1-4.2, SusDev_SDP-NPi, SP
    WITCH 5.0, CO_CurPol, GS
    MESSAGEix-GLOBIOM 1.0, CO_CurPol, LD
    REMIND-MAgPIE 2.1-4.3, DeepElec_SSP2_Npi, Ren")) %>%  
  mutate(Type = "baseline")

remind <- read_csv(c(
  "Model, Scenario, IMP_marker
    REMIND-MAgPIE 2.1-4.2, CEMICS_SSP2-1p5C-fullCDR, R-CDR
    REMIND-MAgPIE 2.1-4.2, NGFS2_Current Policies, R-CurPol")) %>% 
  mutate(Type = "remind")

scenarios <- rbind(imps, baselines, remind)

rm("imps", "baselines", "remind")
  

######### Long Format 
energy_jobs <- left_join(energy_jobs, scenarios) %>% 
  pivot_longer(-c(category, fuel, Region, Model, Scenario, year,IMP_marker, Type), names_to = "job_calc", values_to = "jobs") %>% 
  extract(job_calc, into = c("advance", "redis"), "(.*)_([^_]+)$") 

total_jobs <- left_join(total_jobs, scenarios) %>% 
  pivot_longer(-c(Region, Model, Scenario, year,IMP_marker, Type), names_to = "job_calc", values_to = "jobs") %>% 
  extract(job_calc, into = c("advance", "redis"), "(.*)_([^_]+)$") 


######### Standardizing (only for total jobs) by Capacity installed GW or GDP or Primary Energy PJ 
# Capacity
tech <- c("Biomass", "Coal", "Gas", "Hydro", "Nuclear", "Oil", "Solar|CSP", "Solar|PV", "Wind|Offshore", "Wind|Onshore")
oam <- "Capacity|Electricity|"
oam <- paste0(oam,tech) # Add ALL TECH

stand_cap <- iamc_input_data_worked %>% 
  filter(Variable %in% oam) %>% 
  select(-Unit) %>% 
  pivot_longer(-c(Model, Scenario, Region, Variable), names_to = "year", values_to = "energyquantity" ) %>% 
  group_by(Model, Scenario, Region, year) %>% 
  na.omit() %>% 
  summarize(cap=sum(energyquantity, na.rm = T))

# GDP
stand_gdp <- iamc_input_data_worked %>% 
  filter(Variable == "GDP|PPP") %>% 
  select(-Unit) %>% 
  pivot_longer(-c(Model, Scenario, Region, Variable), names_to = "year", values_to = "energyquantity" ) %>% 
  group_by(Model, Scenario, Region, year) %>% 
  na.omit() %>% 
  summarize(gdp=sum(energyquantity, na.rm = T))

# Primary Energy
tech <- c("Biomass", "Coal", "Gas", "Hydro", "Nuclear", "Oil", "Solar", "Wind")
prime_en <- "Primary Energy|"
prime_en <- paste0(prime_en,tech) # Add ALL TECH

stand_nrj <- iamc_input_data_worked %>% 
  filter(Variable %in% prime_en) %>% 
  select(-Unit) %>% 
  pivot_longer(-c(Model, Scenario, Region, Variable), names_to = "year", values_to = "energyquantity" ) %>% 
  group_by(Model, Scenario, Region, year) %>% 
  na.omit() %>% 
  summarize(nrj=sum(energyquantity, na.rm = T))

stand <- left_join(stand_gdp, stand_cap)
stand <- left_join(stand, stand_nrj) %>% 
  mutate(year = as.integer(year))

rm("stand_cap", "stand_gdp", "stand_nrj", "oam", "prime_en", "tech")

#### Hide PLots ####
# Plots
# stand_plot <- left_join(stand, scenarios)
# ggplot(subset(stand_plot, !is.na(IMP_marker)) %>% filter(str_detect(Region, "R5")) %>% filter(year >= 2020) %>%
#          group_by(Model, Scenario, IMP_marker, Type, year) %>% summarize(caps=sum(cap), na.rm = T)) +
#   geom_line(aes(as.numeric(year), caps*1e-3, color=IMP_marker, linetype=Type)) +
#   scale_color_brewer(palette = "Set1") +
#   labs(x="", y="Capacity in TW", color="IMP") +
#   ggtitle("Capacity per IMP R5")
# 
# ggplot(subset(stand_plot, !is.na(IMP_marker)) %>% filter(str_detect(Region, "R5")) %>% filter(year >= 2020) %>%
#          group_by(Model, Scenario, IMP_marker, Type, year) %>% summarize(gdps=sum(gdp), na.rm = T)) +
#   geom_line(aes(as.numeric(year), gdps*1e-3, color=IMP_marker, linetype=Type)) +
#   scale_color_brewer(palette = "Set1") +
#   labs(x="", y="GDP in Trillion USD", color="IMP") +
#   ggtitle("GDP per IMP R5")
# 
# ggplot(subset(stand_plot, !is.na(IMP_marker)) %>% filter(str_detect(Region, "R5")) %>% filter(year >= 2020) %>%
#          group_by(Model, Scenario, IMP_marker, Type, year) %>% summarize(nrjs=sum(nrj), na.rm = T)) +
#   geom_line(aes(as.numeric(year), nrjs, color=IMP_marker, linetype=Type)) +
#   scale_color_brewer(palette = "Set1") +
#   labs(x="", y="Primary Energy in EJ", color="IMP") +
#   ggtitle("Primary Energy per IMP R5")
# 
# rm("stand_plot")
#### END of HIDE ####

# Joining
total_jobs <- left_join(total_jobs, stand) 

# TYPE 1: For regional disaggregation
total_jobs_reg <- total_jobs %>% 
  mutate(across(c(gdp,cap,nrj), ~ jobs/ .)) %>% 
  rename(none = jobs) %>% 
  pivot_longer(-c(Model, Scenario, Region, year, IMP_marker, Type, advance, redis), names_to = "stand", values_to = "jobs")

# TYPE 2: For global values
total_jobs_glob <- total_jobs %>% 
  filter(str_detect(Region, "R5")) %>% 
  group_by(Model, Scenario, IMP_marker, Type, year, advance, redis) %>% 
  summarize(jobs = if(all(is.na(jobs))) NA_real_ else sum(jobs, na.rm = T),
            cap = if(all(is.na(cap))) NA_real_ else sum(cap, na.rm = T),
            gdp = if(all(is.na(gdp))) NA_real_ else sum(gdp, na.rm = T),
            nrj = if(all(is.na(nrj))) NA_real_ else sum(nrj, na.rm = T)) %>% 
  mutate(across(c(gdp,cap,nrj), ~ jobs/ .)) %>% 
  rename(none = jobs) %>% 
  pivot_longer(-c(Model, Scenario, year, IMP_marker, Type, advance, redis), names_to = "stand", values_to = "jobs")

rm("total_jobs", "stand")




#### Subtracting 2020
energy_jobs_2020 <- energy_jobs %>% 
  group_by(category, fuel, Model, Scenario, Region, IMP_marker, Type, advance, redis) %>%
  mutate(across(jobs, ~. - .[year == 2020])) %>% 
  rename(yes = jobs) %>% 
  ungroup()

energy_jobs <- left_join(energy_jobs, energy_jobs_2020) %>%
  rename(no = jobs) %>% 
  pivot_longer(-c(category, fuel, Region, Model, Scenario, year,IMP_marker, Type, advance, redis), names_to = "base20", values_to = "jobs") %>% 
  ungroup()


total_jobs_reg_2020 <- total_jobs_reg %>% 
  group_by(Model, Scenario, Region, IMP_marker, Type, advance, redis, stand) %>%
  mutate(across(jobs, ~. - .[year == 2020])) %>% 
  rename(yes = jobs) %>% 
  ungroup()

total_jobs_reg <- left_join(total_jobs_reg, total_jobs_reg_2020) %>%
    rename(no = jobs) %>% 
    pivot_longer(-c(Region, Model, Scenario, year,IMP_marker, Type, advance, redis, stand), names_to = "base20", values_to = "jobs") %>% 
    ungroup()


total_jobs_glob_2020 <- total_jobs_glob %>% 
  group_by(Model, Scenario, IMP_marker, Type, advance, redis, stand) %>%
  mutate(across(jobs, ~. - .[year == 2020])) %>% 
  rename(yes = jobs) %>% 
  ungroup()

total_jobs_glob <- left_join(total_jobs_glob, total_jobs_glob_2020) %>%
  rename(no = jobs) %>% 
  pivot_longer(-c(Model, Scenario, year,IMP_marker, Type, advance, redis, stand), names_to = "base20", values_to = "jobs") %>% 
  ungroup()


#### Subtracting CurPol
chosen_five <- c("GS", "Neg", "Ren", "LD", "SP")

energy_jobs_curpol <- energy_jobs %>% 
  filter(IMP_marker %in% chosen_five) %>% 
  group_by(category, fuel, year, Region, IMP_marker, advance, redis, base20) %>%
  mutate(across(jobs, ~. - .[Type == "baseline"])) %>% 
  rename(yes = jobs) %>% 
  ungroup()

energy_jobs <- left_join(energy_jobs, energy_jobs_curpol) %>%
  rename(no = jobs) %>% 
  pivot_longer(-c(category, fuel, Region, Model, Scenario, year,IMP_marker, Type, advance, redis, base20), names_to = "curpol", values_to = "jobs") %>% 
  ungroup()


total_jobs_reg_curpol <- total_jobs_reg %>% 
  filter(IMP_marker %in% chosen_five) %>% 
  group_by(year, Region, IMP_marker, advance, redis, base20, stand) %>%
  mutate(across(jobs, ~. - .[Type == "baseline"])) %>% 
  rename(yes = jobs) %>% 
  ungroup()

total_jobs_reg <- left_join(total_jobs_reg, total_jobs_reg_curpol) %>%
  rename(no = jobs) %>% 
  pivot_longer(-c(Region, Model, Scenario, year,IMP_marker, Type, advance, redis, base20, stand), names_to = "curpol", values_to = "jobs") %>% 
  ungroup()


total_jobs_glob_curpol <- total_jobs_glob %>% 
  filter(IMP_marker %in% chosen_five) %>% 
  group_by(year, IMP_marker, advance, redis, base20, stand) %>%
  mutate(across(jobs, ~. - .[Type == "baseline"])) %>% 
  rename(yes = jobs) %>% 
  ungroup()

total_jobs_glob <- left_join(total_jobs_glob, total_jobs_glob_curpol) %>%
  rename(no = jobs) %>% 
  pivot_longer(-c(Model, Scenario, year,IMP_marker, Type, advance, redis, base20, stand), names_to = "curpol", values_to = "jobs") %>% 
  ungroup()


rm("energy_jobs_2020", "total_jobs_reg_2020", "total_jobs_glob_2020", 
   "energy_jobs_curpol", "total_jobs_reg_curpol", "total_jobs_glob_curpol")  
  

chosen_nosen <- c("CurPol", "ModAct", "GS", "Neg", "Ren", "LD", "SP")
chosen_nocur <- c("ModAct", "GS", "Neg", "Ren", "LD", "SP")
chosen_nomod <- c("CurPol", "GS", "Neg", "Ren", "LD", "SP")
remind <- c("Ren", "SP", "R-CDR", "R-CurPol")
remind_plus <- c("GS", "Neg", "LD", "Ren", "SP", "R-CDR", "R-CurPol")








######### Checks
### Check count of categories
check_cat <- energy_jobs %>% 
  filter(str_detect(Region, "R5")) %>% 
  filter(year == "2020") %>% 
  filter(advance == "linmin") %>% 
  filter(redis == "red") %>% 
  filter(base20 == "no") %>% 
  filter(curpol == "no") %>%
  filter(!is.na(Type)) %>% 
  group_by(category, IMP_marker, Type) %>% 
  summarize(jobs = sum(jobs, na.rm =T))

# Refinery for all
# O&M missing for LED (1) --> RECOVERED
# Manufacturing & Construction missing for LED, LED B, Neg (3) --> RECOVERED
# Extraction missing for CurPol, ModAct, GS, GS B, Neg, Neg B, Neg-2.0, LD B, R-CP (9) --> RECOVERED

### From when are categories available?
check_min <- energy_jobs %>% 
  mutate(Region_ID = str_extract(Region, "R+[0-9]+")) %>%
  mutate(Region_ID = coalesce(Region_ID,Region)) %>% 
  filter(advance == "base", redis == "nored", Region_ID == "R5", base20 == "no", curpol == "no") %>% 
  group_by(category, Scenario, IMP_marker, Type) %>% 
  mutate_at(c('jobs'), ~na_if(., 0)) %>% 
  na.omit() %>% 
  summarize(early = min(year))
  
# LED: Extraction & Refinery & O&M (infilled) from 2010, Construction & Manufacturing from 2020
# Neg: Construction & Manufacturing from 2015, the rest from 2010

### All years in which categories are available
check_year <- energy_jobs %>% 
  mutate(Region_ID = str_extract(Region, "R+[0-9]+")) %>%
  mutate(Region_ID = coalesce(Region_ID,Region)) %>% 
  filter(advance == "base", redis == "nored", Region_ID == "R5", base20 == "no", curpol == "no") %>% 
  group_by(category, Scenario, IMP_marker, Type, year) %>% 
  na.omit() %>% 
  summarize(count = sum(jobs)) %>% 
  mutate(Origin = ifelse(IMP_marker %in% c("LD", "Neg") & category %in% c("Construction", "Manufacturing") & Type == "imp", "Calculated from Cap",
                         ifelse(IMP_marker == "Neg" & category %in% c("Construction", "Manufacturing") & Type == "baseline", "Calculated from Cap",
                         ifelse(IMP_marker == "LD" & category == "O&M" & year >= 2020 & Type == "imp", "Original LED Study",
                         ifelse(IMP_marker == "LD" & category == "O&M" & year == 2010 & Type == "imp", "MESSAGE-GLOBIOM 1.0",
                         ifelse(IMP_marker == "R-CurPol" & category == "Extraction" & Type == "remind", "Calculated from Trade",
                         ifelse(IMP_marker %in% c("LD", "GS", "Neg") & category == "Extraction" & Type == "baseline", "Calculated from Trade",
                         ifelse(IMP_marker %in% c("CurPol", "GS", "ModAct", "Neg", "Neg-2.0") & category == "Extraction" & Type == "imp", "Calculated from Trade", "Reported"))))))))
  
ggplot(check_year %>% filter(!is.na(Type)), aes(year, category)) +
  geom_tile(aes(fill = year, color = as.factor(Origin), width=0.7, height=0.7), size=2) +
  facet_grid(Type~IMP_marker) +
  scale_color_brewer(palette = "Set1")

rm("check_cat", "check_min", "check_year")



### Technologies available for all scenarios?
iamc_input_data_worked <- left_join(iamc_input_data_worked, scenarios)
# Technology
tech <- c("Biomass", "Coal", "Gas", "Hydro", "Nuclear", "Oil", "Solar|CSP", "Solar|PV", "Wind|Offshore", "Wind|Onshore")

#### Construction & Manufacturing --> Capacity Additions --> SP & Ren (REMIND) Wind missing (completed)
const_manu <- "Capacity Additions|Electricity|"
const_manu <- paste0(const_manu,tech) # Add ALL TECH
const_manu <- iamc_input_data_worked %>% filter(Variable %in% const_manu) %>% filter(str_detect(Region, "R5")) %>% select(-Region)
const_manu <- unique(const_manu[c("Model","Scenario","IMP_marker","Variable", "Type")])

# All
ggplot(const_manu, aes(Variable, IMP_marker)) +
  geom_tile(aes(fill = Variable, width=0.7, height=0.7), size=2) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(.~Type) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Wind
ggplot(iamc_input_data_worked %>% 
         filter(!is.na(Type)) %>%
         select(-Unit) %>% 
         pivot_longer(-c(Model, Scenario, Region, Variable, IMP_marker, Type), names_to = "year", values_to = "energyquantity") %>% 
         filter(year>=2020 & year<=2100) %>% 
         filter(str_detect(Region, "R5")) %>% 
         filter(Variable == "Capacity Additions|Electricity|Wind|Onshore" | 
                  Variable == "Capacity Additions|Electricity|Wind|Offshore" | 
                  Variable == "Capacity Additions|Electricity|Wind") %>% 
         na.omit()) + 
  geom_line(aes(as.numeric(year), energyquantity, linetype=Variable, color=IMP_marker)) + 
  facet_grid(Type~Region) +
  #scale_color_brewer(palette = "Set1") +
  labs(x="", y="GW/yr", color="IMP") + 
  ggtitle("Capacity Additions")



#### O&M -> Capacity --> SP & Ren (REMIND) Wind missing (completed) & R-CurPol Oil missing (completed)
oam <- "Capacity|Electricity|"
oam <- paste0(oam,tech) # Add ALL TECH
oam <- iamc_input_data_worked %>% filter(Variable %in% oam) %>% filter(str_detect(Region, "R5")) %>% select(-Region)
oam <- unique(oam[c("Model","Scenario","IMP_marker","Variable","Type")])

# All
ggplot(oam, aes(Variable, IMP_marker)) +
  geom_tile(aes(fill = Variable, width=0.7, height=0.7), size=2) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(.~Type) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Wind
ggplot(iamc_input_data_worked %>% 
         filter(!is.na(Type)) %>%
         select(-Unit) %>% 
         pivot_longer(-c(Model, Scenario, Region, Variable, IMP_marker, Type), names_to = "year", values_to = "energyquantity") %>% 
         filter(year>=2020 & year<=2100) %>% 
         filter(str_detect(Region, "R5")) %>% 
         filter(Variable == "Capacity|Electricity|Wind|Onshore" | 
                  Variable == "Capacity|Electricity|Wind|Offshore" | 
                  Variable == "Capacity|Electricity|Wind") %>% 
         na.omit()) + 
  geom_line(aes(as.numeric(year), energyquantity, linetype=Variable, color=IMP_marker)) + 
  facet_grid(Type~Region) +
  #scale_color_brewer(palette = "Set1") +
  labs(x="", y="GW/yr", color="IMP") + 
  ggtitle("Capacity")


#### Fuel production --> Extraction: Coal, Gas, Oil --> ALL!
fuelprod <- unique(iamc_input_data_worked$Variable[grepl("Resource[|]Extraction[|]", iamc_input_data_worked$Variable)])
fuelprod <- iamc_input_data_worked %>% filter(Variable %in% fuelprod) %>% filter(str_detect(Region, "R5")) %>% select(-Region)
fuelprod <- unique(fuelprod[c("Model","Scenario","IMP_marker","Variable","Type")])

ggplot(fuelprod, aes(Variable, IMP_marker)) +
  geom_tile(aes(fill = Variable, width=0.7, height=0.7), size=2) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(.~Type) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#### Refinery -> Secondary Energy: Biomass and Oil --> ALL!
refine <- c("Secondary Energy|Liquids|Biomass", "Secondary Energy|Liquids|Oil")
refine <- iamc_input_data_worked %>% filter(Variable %in% refine) %>% filter(str_detect(Region, "R5")) %>% select(-Region)
refine <- unique(refine[c("Model","Scenario","IMP_marker","Variable", "Type")])

ggplot(refine, aes(Variable, IMP_marker)) +
  geom_tile(aes(fill = Variable, width=0.7, height=0.7), size=2) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(.~Type) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

rm("tech","const_manu","oam","fuelprod","refine")











######### Plotting
### Mix in 2020 to compare with historical stats // For LED 2020 Capadd based on other Message Cap in 2010 as mentioned above
# By category IMPS
ggplot(subset(energy_jobs, !is.na(jobs)) %>%    
         filter(!is.na(Type)) %>% 
         filter(str_detect(Region, "R5") & year==2020) %>% 
         filter(advance == "linmin", redis == "red", base20 == "no", curpol == "no") %>% 
         group_by(IMP_marker, Type, fuel, category) %>% 
         summarise(jobs = if(all(is.na(jobs))) NA_real_ else sum(jobs, na.rm = T))) +
  geom_bar(aes(IMP_marker, jobs*1e-6, fill=category), position="stack", stat="identity") + 
  scale_fill_brewer(palette = "Spectral") + 
  labs(x="", y="Jobs [in millions]", fill="Category") + 
  facet_wrap(.~Type) +
  theme(legend.position = "bottom") + 
  ggtitle("2020 Mix R5 by Category")
# By fuel IMPS     
ggplot(subset(energy_jobs, !is.na(jobs)) %>%    
         filter(!is.na(Type)) %>% 
         filter(str_detect(Region, "R5") & year==2020) %>% 
         filter(advance == "linmin", redis == "red", base20 == "no", curpol == "no") %>% 
         group_by(IMP_marker, Type, fuel, category) %>% 
         summarise(jobs = if(all(is.na(jobs))) NA_real_ else sum(jobs, na.rm = T))) +
  geom_bar(aes(IMP_marker, jobs*1e-6, fill=fuel), position="stack", stat="identity") + 
  scale_fill_brewer(palette = "Spectral") + 
  labs(x="", y="Jobs [in millions]", fill="Fuel") + 
  facet_wrap(.~Type) +
  theme(legend.position = "bottom") + 
  ggtitle("2020 Mix R5 by Fuel")


### Linear total jobs 
# One scenario per model
ggplot(subset(total_jobs_glob, !is.na(jobs)) %>% 
         filter(IMP_marker %in% remind_plus) %>% 
         filter(!Type == "baseline") %>% 
         filter(year>=2020 & year<=2050) %>% 
         filter(advance == "linmin", redis == "red", base20 == "yes", curpol == "no", stand == "none")) +
  geom_line(aes(as.numeric(year), jobs*1e-6, color=IMP_marker)) + 
  scale_color_brewer(palette = "Set1") +
  labs(x="", y="Jobs [in millions]", color="IMP") + 
  ggtitle("Global Jobs Development per IMP R5 to Base Year 2020")
# Comparison between imp and baseline
ggplot(subset(total_jobs_glob, !is.na(jobs)) %>% 
         mutate(Type = factor(Type, levels=c("imp", "baseline"))) %>% 
         filter(IMP_marker %in% chosen_nomod) %>% 
         filter(year>=2020 & year<=2050) %>% 
         filter(advance == "linmin", redis == "red", base20 == "no", , curpol == "no", stand == "none")) +
  geom_line(aes(as.numeric(year), jobs*1e-6, color=IMP_marker, linetype = Type)) + 
  scale_color_brewer(palette = "Set1") +
  labs(x="", y="Jobs [in millions]", color="IMP") + 
  ggtitle("Global Jobs Development per IMP R5 Comparison")
# Regional split
ggplot(subset(total_jobs_reg, !is.na(jobs)) %>% 
         filter(IMP_marker %in% remind_plus) %>% 
         filter(!Type == "baseline") %>% 
         filter(str_detect(Region, "R5") & year>=2020 & year<=2050) %>% 
         filter(advance == "linmin", redis == "red", base20 == "yes", curpol == "no", stand == "none")) +
  geom_line(aes(as.numeric(year), jobs*1e-6, color=IMP_marker, linetype = Region)) + 
  scale_color_brewer(palette = "Set1") +
  labs(x="", y="Jobs [in millions]", color="IMP") + 
  ggtitle("Global Jobs Development per IMP R5 to Base Year 2020 and CurPol")



### Linear jobs split by type 
# By category IMPS (split for calculated / reported)
ggplot(subset(energy_jobs, !is.na(jobs)) %>%    
         filter(IMP_marker %in% remind_plus) %>% 
         filter(!Type == "baseline") %>% 
         filter(str_detect(Region, "R5") & year>=2020 & year<=2050) %>% 
         filter(advance == "linmin", redis == "red", base20 == "yes", curpol == "no") %>% 
         group_by(IMP_marker, category, year) %>% 
         summarise(jobs = if(all(is.na(jobs))) NA_real_ else sum(jobs, na.rm = T)) %>% 
         mutate(Origin = ifelse((IMP_marker %in% c("LD", "Neg") & category %in% c("Construction", "Manufacturing")) | (IMP_marker == "LD" & category == "O&M") | (IMP_marker %in% c("CurPol", "GS", "ModAct", "Neg", "Neg-2.0") & category == "Extraction"), "Calculated", "Reported"))) +
  geom_line(aes(as.numeric(year), jobs*1e-6, color=IMP_marker
                 #, linetype=Origin
                )) +
  facet_wrap(.~category) +
  scale_color_brewer(palette = "Set1") +
  labs(x="", y="Jobs [in millions]", color="IMP") + 
  ggtitle("Global Jobs Development per IMP R5 and Category to Base Year 2020")
# By fuel IMPS
ggplot(subset(energy_jobs, !is.na(jobs)) %>%    
         filter(IMP_marker %in% remind_plus) %>% 
         filter(!Type == "baseline") %>%
         filter(str_detect(Region, "R5") & year>=2020 & year<=2050) %>% 
         filter(advance == "linmin", redis == "red", base20 == "yes", curpol == "no") %>% 
         group_by(IMP_marker, fuel, year) %>% 
         summarise(jobs = if(all(is.na(jobs))) NA_real_ else sum(jobs, na.rm = T))) +
  geom_line(aes(as.numeric(year), jobs*1e-6, color=IMP_marker)) +
  facet_wrap(.~fuel, scales="free") +
  scale_color_brewer(palette = "Set1") +
  labs(x="", y="Jobs [in millions]", color="IMP") + 
  ggtitle("Global Jobs Development per IMP R5 and Fuel to Base Year 2020")



### Mix over multiple years
# By category IMPS without 2020
ggplot(subset(energy_jobs, !is.na(jobs)) %>%    
         filter(IMP_marker %in% remind_plus) %>% 
         filter(!Type == "baseline") %>% 
         filter(str_detect(Region, "R5") & year %in% c("2030","2040","2050")) %>% 
         filter(advance == "linmin", redis == "red", base20 == "yes", curpol == "no") %>% 
         group_by(IMP_marker, fuel, category, year) %>% 
         summarise(jobs = if(all(is.na(jobs))) NA_real_ else sum(jobs, na.rm = T))) +
  geom_bar(aes(IMP_marker, jobs*1e-6, fill=category), position="stack", stat="identity") + 
  facet_wrap(.~year, nrow = 1) +
  scale_fill_brewer(palette = "Spectral") + 
  labs(x="", y="Jobs [in millions]", fill="Category") + 
  theme(legend.position = "bottom") + 
  ggtitle("Global Jobs Development per IMP R5 and Category to Base Year 2020")
# By fuel IMPS without 2020
ggplot(subset(energy_jobs, !is.na(jobs)) %>%    
         filter(IMP_marker %in% remind_plus) %>% 
         filter(!Type == "baseline") %>% 
         filter(str_detect(Region, "R5") & year %in% c("2030","2040","2050")) %>% 
         filter(advance == "linmin", redis == "red", base20 == "yes", curpol == "no") %>% 
         group_by(IMP_marker, fuel, category, year) %>% 
         summarise(jobs = if(all(is.na(jobs))) NA_real_ else sum(jobs, na.rm = T))) +
  geom_bar(aes(IMP_marker, jobs*1e-6, fill=fuel), position="stack", stat="identity") + 
  facet_wrap(.~year) +
  scale_fill_brewer(palette = "Spectral") + 
  labs(x="", y="Jobs [in millions]", fill="Fuel") + 
  theme(legend.position = "bottom") + 
  ggtitle("Global Jobs Development per IMP R5 and Fuel to Base Year 2020")



### Maps
library(sf)
#library(RColorBrewer)
# Reading world shape file
geom_iso <- giscoR::gisco_get_countries()
colnames(geom_iso)[3] <- "iso3"
# Combine crossover with shape file
geom_regions <- merge(geom_iso, region_mappings, by = "iso3", all = T) %>% 
  filter(iso3 != "ATA")
# Projection
target_crs <- '+proj=eqearth +wktext'
# Shrink data to be plotted
geom_plot <- total_jobs_reg %>% 
  filter(year %in% c(2030,2050)) %>%   
  filter(advance == "linmin", redis == "red", base20 == "yes", curpol == "no", stand == "none")
# Combine with deaths
geom_final <- merge(geom_plot, geom_regions, by = "Region", all = T)


# Plot
ggplot(geom_final %>% 
         filter(IMP_marker %in% remind_plus) %>% 
         filter(!Type == "baseline") %>% 
         filter(str_detect(Region, "R5"))) +               
  facet_grid(factor(year) ~ factor(IMP_marker)) +             
  geom_sf(aes(fill = jobs*1e-6, geometry = geometry)) +           
  coord_sf(datum = target_crs, expand = FALSE, clip = "off") +
  scale_fill_gradient2(name = "Jobs Added [in millions]", low = "red", high = "blue") +
  theme_bw() +
  theme(legend.position = "bottom", legend.text = element_text(size=6)) +
  ggtitle("Global Jobs Development per IMP R5 to Base Year 2020")







### COMPARISON
# 2a Tech Progress
ggplot(subset(total_jobs_glob, !is.na(jobs)) %>% 
         filter(IMP_marker %in% remind_plus) %>% 
         filter(!Type == "baseline") %>% 
         filter(year>=2020 & year<=2050) %>% 
         filter(redis == "red", base20 == "yes", curpol == "no", stand == "none")) + 
  geom_line(aes(as.numeric(year), jobs*1e-6, color=IMP_marker)) + 
  facet_wrap(.~advance) +
  scale_color_brewer(palette = "Set1") +
  labs(x="", y="Jobs [in millions]", color="IMP") + 
  ggtitle("Global Jobs Development per IMP R5 to Base Year 2020")

# 3a Redis All
ggplot(subset(total_jobs_reg, !is.na(jobs)) %>% 
         filter(IMP_marker %in% remind_plus) %>% 
         filter(!Type == "baseline") %>% 
         filter(str_detect(Region, "R10") & year>=2020 & year<=2050) %>% 
         filter(advance == "linmin", base20 == "yes", curpol == "no", stand == "none")) + 
  geom_line(aes(as.numeric(year), jobs*1e-6, color=IMP_marker)) + 
  facet_grid(Region~redis) +
  scale_color_brewer(palette = "Set1") +
  labs(x="", y="Jobs [in millions]", color="IMP") + 
  ggtitle("Global Jobs Redistribution per IMP R5 to Base Year 2020")
# 3b Redis PV
ggplot(subset(energy_jobs, !is.na(jobs)) %>%    
         filter(IMP_marker %in% remind_plus) %>% 
         filter(!Type == "baseline") %>% 
         filter(str_detect(Region, "R10") & year>=2020 & year<=2050) %>% 
         filter(fuel == "Solar (PV)" & category == "Manufacturing" & advance == "linmin" & base20 == "yes" & curpol == "no") %>% 
         group_by(IMP_marker, year, redis, Region) %>% 
         summarise(jobs = if(all(is.na(jobs))) NA_real_ else sum(jobs, na.rm = T))) +
  geom_line(aes(as.numeric(year), jobs*1e-6, color=IMP_marker)) +
  facet_grid(Region~redis) +
  scale_color_brewer(palette = "Set1") +
  labs(x="", y="Jobs [in millions]", color="IMP") + 
  ggtitle("Global Jobs Development per IMP R5 for Solar (PV) to Base Year 2020")
# 3c Redis Comp
ggplot(subset(total_jobs_glob, !is.na(jobs)) %>% 
         filter(IMP_marker %in% remind_plus) %>% 
         filter(!Type == "baseline") %>% 
         filter(year %in% c("2030", "2050", "2070", "2100")) %>% 
         filter(advance == "linmin" & stand == "none" & base20 == "yes" & curpol == "no")) + 
  geom_bar(aes(IMP_marker, jobs*1e-6, fill = redis), stat="identity", position=position_dodge()) +
  facet_wrap(.~year, nrow = 1) +
  scale_fill_brewer(palette = "Set1") +
  labs(x="", y="Jobs [in millions]", fill="Redistribution") + 
  theme(legend.position = "bottom") +
  ggtitle("Global Jobs Redistribution per IMP R5 to Base Year 2020")



# 4a All Dashed
ggplot(subset(total_jobs_glob, !is.na(jobs)) %>% 
         filter(IMP_marker %in% remind_plus) %>% 
         filter(!Type == "baseline") %>% 
         filter(year>=2020 & year<=2050) %>% 
         filter(stand == "none", base20 == "yes", curpol == "no")) + 
  geom_line(aes(as.numeric(year), jobs*1e-6, color=IMP_marker, linetype=advance)) + 
  facet_wrap(.~redis) +
  scale_color_brewer(palette = "Set1") +
  labs(x="", y="Jobs [in millions]", color="IMP") + 
  ggtitle("Global Jobs Development per IMP R5 to Base Year 2020")
# 4b All Grid
ggplot(subset(total_jobs_glob, !is.na(jobs)) %>% 
         filter(IMP_marker %in% remind_plus) %>% 
         filter(!Type == "baseline") %>% 
         filter(year>=2020 & year<=2050) %>% 
         filter(stand == "none", base20 == "yes", curpol == "no")) + 
  geom_line(aes(as.numeric(year), jobs*1e-6, color=IMP_marker)) + 
  facet_grid(redis~advance) +
  scale_color_brewer(palette = "Set1") +
  labs(x="", y="Jobs [in millions]", color="IMP") + 
  ggtitle("Global Jobs Development per IMP R5 to Base Year 2020")
# 4c 2020
ggplot(subset(energy_jobs, !is.na(jobs)) %>%    
         filter(IMP_marker %in% remind_plus) %>% 
         filter(!Type == "baseline") %>% 
         filter(str_detect(Region, "R5") & year==2020) %>% 
         group_by(IMP_marker, fuel, category, year, redis, advance) %>% 
         filter(base20 == "no", curpol == "no") %>% 
         summarise(jobs = if(all(is.na(jobs))) NA_real_ else sum(jobs, na.rm = T))) +
  geom_bar(aes(IMP_marker, jobs*1e-6, fill=category), position="stack", stat="identity") + 
  facet_grid(advance~redis) +
  scale_fill_brewer(palette = "Spectral") + 
  labs(x="", y="Jobs [in millions]", fill="Category") + 
  theme(legend.position = "bottom") + 
  ggtitle("2020 Mix R5 by Category")



# 5a Standardization
ggplot(subset(total_jobs_glob, !is.na(jobs)) %>% 
         filter(IMP_marker %in% remind_plus) %>% 
         filter(!Type == "baseline") %>% 
         filter(year>=2020 & year<=2050) %>% 
         filter(advance == "linmin" & redis == "red" & base20 == "yes" & curpol == "no")) + 
  geom_line(aes(as.numeric(year), jobs, color=IMP_marker)) + 
  facet_wrap(.~stand, scales="free") +
  scale_color_brewer(palette = "Set1") +
  labs(x="", y="", color="IMP") + 
  ggtitle("Global Jobs Development per IMP R5 to Base Year 2020")



### PARALLEL PLOT
saveRDS(total_jobs_glob, "../../X_Comparison/total_jobs_glob.rds")

### DATA
saveRDS(energy_jobs, "../data_ex/energy_jobs.rds")

