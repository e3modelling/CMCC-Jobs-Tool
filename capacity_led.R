############################## Comparing calculated Capacity by summing up regions with Original LED study Capacity
library(dplyr)
library(vroom)
library(readxl)
library(tidyverse)
library(tidyr)

setwd("~/Documents/EDITS/Employment/energy-jobs-dataset-iiasadb")
rm(list = ls())

tech <- c("Biomass", "Coal", "Gas", "Hydro", "Nuclear", "Oil", "Solar|CSP", "Solar|PV", "Wind|Offshore", "Wind|Onshore")
oam <- "Capacity|Electricity|"
oam <- paste0(oam,tech) # Add ALL TECH

# Starting with loaded energy jobs from energy_jobs_iiasadb.R
calculated_cap <- energy_jobs %>% 
  filter(Scenario == "LowEnergyDemand_1.3_IPCC") %>% 
  select(-c("jobs_per_unit", "jobs")) %>% 
  filter(category == "O&M") %>% 
  arrange(fuel, Region, year) %>% 
  select(-c("category", "Model", "Scenario")) %>% 
  pivot_wider(id_cols=c(fuel, year), names_from = Region, values_from = energyquantity) %>% 
  rename(North = "R5OECD90+EU") %>% 
  mutate(South = R5ASIA + R5MAF + R5LAM + R5REF) %>% 
  select(-c(R5ASIA, R5MAF, R5LAM, R5REF)) %>% 
  mutate(type = "Calculated") %>% 
  pivot_longer(-c(fuel, year, type), names_to = "Region", values_to = "energy")

  

led_original <- read.csv(file = file.path("led_original.csv"), check.names = FALSE) %>% 
  filter(VARIABLE %in% oam) %>% 
  filter(REGION != "World") %>% 
  rename_with(str_to_title) %>% 
  select(-Unit) %>% 
  select(where(~sum(!is.na(.x)) > 0)) %>% 
  mutate(Variable = str_replace(Variable,'Capacity[|]Electricity','O&M')) %>% 
  mutate(Variable = str_replace(Variable,'Solar[|]PV','Solar (PV)')) %>% 
  mutate(Variable = str_replace(Variable,'Solar[|]CSP','Solar (CSP)')) %>% 
  mutate(Variable = str_replace(Variable,'Wind[|]Offshore','Wind (OFF)')) %>% 
  mutate(Variable = str_replace(Variable,'Wind[|]Onshore','Wind (ON)')) %>% 
  separate(col = Variable, into = c("category", "fuel"), sep = "[|]") %>% 
  select(-c("Model", "Scenario", "category", "2000", "2005")) %>% 
  pivot_longer(-c(Region, fuel), names_to = "year", values_to = "energyquantity") %>% 
  pivot_wider(id_cols=c(fuel, year), names_from = Region, values_from = energyquantity) %>% 
  rename(North = R2_North) %>% 
  rename(South = R2_South) %>% 
  mutate(type = "Original") %>% 
  pivot_longer(-c(fuel, year, type), names_to = "Region", values_to = "energy")

combined <- bind_rows(led_original, calculated_cap)



### Plot
ggplot(combined %>% filter(fuel == "Biomass")) + 
  geom_line(aes(as.numeric(year), energy, color=Region, linetype = type)) + 
  facet_wrap(.~fuel) +
  scale_color_brewer(palette = "Set1") +
  labs(x="", y="GW", linetype = "Linetype") +
  ggtitle("Capacity")



