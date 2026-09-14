#####################
# Cut scenario data #
#      May 2023     #
#####################
# Preparing shortened csv file for variable search

#########  Setup
library(dplyr)
library(vroom)
library(readxl)
library(tidyr)
require(tidyverse)

rm(list = ls())
setwd("~/Documents/EDITS/")

##### Loading IMP markers
imps <- read_excel(path = "Y_AR6_Scenarios/R10/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx", 
                   sheet = "Illustrative_Pathways",
                   range = cell_cols("A:C")) %>% 
  unite("scenario_model", scenario:model, sep = "-", remove = FALSE) %>% 
  select(-IMP_marker)


baselines <- read_csv(c(
    "model, scenario
    COFFEE 1.1, EN_NPi2100
    REMIND-MAgPIE 2.1-4.2, SusDev_SDP-NPi
    WITCH 5.0, CO_CurPol
    MESSAGEix-GLOBIOM 1.0, CO_CurPol
    REMIND-MAgPIE 2.1-4.3, DeepElec_SSP2_Npi")) %>%   
  unite("scenario_model", scenario:model, sep = "-", remove = FALSE)

remind <- read_csv(c(
  "model, scenario
    REMIND-MAgPIE 2.1-4.2, CEMICS_SSP2-1p5C-fullCDR
    REMIND-MAgPIE 2.1-4.2, NGFS2_Current Policies")) %>% 
  unite("scenario_model", scenario:model, sep = "-", remove = FALSE)

scenarios <- rbind(imps, baselines, remind)

rm("imps", "baselines", "remind")


  
  

### R10
r10 <- vroom(file = "Y_AR6_Scenarios/R10/AR6_Scenarios_Database_R10_regions_v1.1.csv") %>% 
  unite("scenario_model", Scenario:Model, sep = "-", remove = FALSE)
r10_select <- r10 %>% filter(scenario_model %in% scenarios$scenario_model)
check10 <- unique(r10_select[c("Model","Scenario")])

# ### R6
# r6 <- vroom(file = "Y_AR6_Scenarios/R6/AR6_Scenarios_Database_R6_regions_v1.1.csv") %>% 
#   unite("scenario_model", Scenario:Model, sep = "-", remove = FALSE)
# r6_select <- r6 %>% filter(scenario_model %in% scenarios$scenario_model)
# check6 <- unique(r6_select[c("Model","Scenario")])

### R5
r5 <- vroom(file = "Y_AR6_Scenarios/R5/AR6_Scenarios_Database_R5_regions_v1.1.csv") %>% 
  mutate(Model = str_replace(Model,'MESSAGEix-GLOBIOM_1.0','MESSAGEix-GLOBIOM 1.0')) %>% 
  unite("scenario_model", Scenario:Model, sep = "-", remove = FALSE)
r5_select <- r5 %>% filter(scenario_model %in% scenarios$scenario_model)
check5 <- unique(r5_select[c("Model","Scenario")])


###### Combine CSVs
iamc_input_data <- rbind.fill(r5_select,r10_select)

write.csv(iamc_input_data, file.path("Employment/energy-jobs-dataset-iiasadb/iamc_input_data.csv"), row.names = F)


#### MESSAGE CHECKS ####
tech <- c("Biomass", "Coal", "Gas", "Hydro", "Nuclear", "Oil", "Solar|CSP", "Solar|PV", "Wind|Offshore", "Wind|Onshore")
#### Construction & Manufacturing --> Capacity Additions
const_manu <- "Capacity Additions|Electricity|"
const_manu <- paste0(const_manu,tech) # Add ALL TECH
#### O&M -> Capacity
oam <- "Capacity|Electricity|"
oam <- paste0(oam,tech) # Add ALL TECH
#### Fuel production --> Extraction: Coal, Gas, Oil
fuelprod <- unique(r5$Variable[grepl("Resource[|]Extraction[|]", r5$Variable)])
#### Refinery -> Secondary Energy: Biomass and Oil 
refine <- c("Secondary Energy|Liquids|Biomass", "Secondary Energy|Liquids|Oil")

combine <- c(const_manu, oam, fuelprod, refine)

r5_messsage <- r5 %>%
  filter(str_detect(Model, "MESSAGEix-GLOBIOM"))

check_mes <- unique(r5_messsage[c("Model","Scenario")])

curpol <- r5 %>%  filter(Model == "MESSAGEix-GLOBIOM_1.0" & Scenario == "CO_CurPol") %>% 
  filter(Variable %in% combine | grepl("Trade[|]Primary Energy[|]Coal[|]Volume|Trade[|]Primary Energy[|]Gas[|]Volume|Trade[|]Primary Energy[|]Oil[|]Volume|^Primary Energy[|]Coal$|^Primary Energy[|]Gas$|^Primary Energy[|]Oil$", Variable))

r5_mes_compare <- r5 %>%
  filter(scenario_model == "ADVANCE_Reference-MESSAGE-GLOBIOM 1.0" |
           scenario_model == "SSP2-Baseline-MESSAGE-GLOBIOM 1.0") %>%
  filter(Variable %in% oam) %>%
  select(c(2:5), `2010`) %>%
  pivot_wider(id_cols=c(Model, Region, Variable), names_from = Scenario, values_from = `2010`)
#### CHECKS END ####

r5_mes_oam <- r5 %>% 
  filter(scenario_model == "ADVANCE_Reference-MESSAGE-GLOBIOM 1.0") %>% 
  filter(Variable %in% oam) %>% 
  select(c(2:5), `2010`)

write.csv(r5_mes_oam, file.path("Employment/r5_mes_oam.csv"), row.names = F)


