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



#main function to compute energy jobs for iiasadb template
compute_energy_jobs_iiasadb <- function(iamc_input_data, na_threshold = 0.60, technical_progress=T, regional_convergence=T, manufacturing="current_shares"){
#na_treshold: threshold above which number of rows per model/scenario are included default: 0.6 or 60% of all entries
#manufacturing: "current_shares" or "domestic", manufacturing redistribution of job counts based on current production shares or where deployed
#technical_progress: progress in renewables based on van der Zwaan et al., 2013
#regional_convergence: convergence of productivity to minimum in 2050 (or other year speified below)

  
# Technology
tech <- c("Biomass", "Coal", "Gas", "Hydro", "Nuclear", "Oil", "Solar|CSP", "Solar|PV", "Wind|Offshore", "Wind|Onshore")
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
#apply learning per technology until 2050, then constant for now
energy_jobs <- energy_jobs %>% left_join(techprogress) %>% mutate(jobs_per_unit = ifelse(year>2020, jobs_per_unit*(1+progress)^(-(pmin(year,2050)-2020)), jobs_per_unit)) %>% select(-progress)
}

#OPTION 3: Convergence of jobs per unit of energy to the minimum in 2050
#convergence simple
if(regional_convergence){
  year_convergence <- 2050
  energy_jobs <- energy_jobs %>% filter(!is.na(jobs_per_unit)) %>% mutate(Region_ID = str_extract(Region, "R+[0-9]+")) %>% group_by(category, fuel, Model, Scenario, Region_ID) %>% mutate(jobs_per_unit=ifelse(year>=year_convergence, min(jobs_per_unit[year==year_convergence]), jobs_per_unit)) %>% ungroup() %>% group_by(category, fuel, Model, Scenario, Region) %>% mutate(jobs_per_unit = ifelse(year>=2020 & year<year_convergence, jobs_per_unit[year==2020]/( 1 + regional_convergence*( ((jobs_per_unit[year==2020])/jobs_per_unit[year==year_convergence])^(1/(year_convergence-2020)) - 1) )^(year-2020), jobs_per_unit)) %>% ungroup() %>% select(-Region_ID)
}

# Calculating jobs
energy_jobs$jobs = energy_jobs$energyquantity*energy_jobs$jobs_per_unit
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




