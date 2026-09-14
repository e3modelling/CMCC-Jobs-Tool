# Script to process energy jobs data
# First version: 10 October 2019
# Authors: Johannes Emmerling and Sandeep Pai

#requires two input files in the same directory: 
energy_jobs_database_file <- "energy_jobs_database.xlsx"
data_historical_values_file <- "witch/data_historical_values.gdx"
#directory to store graphs and output
resdir <- "results"

# Add WEO energy data => compute total sum!
# Add IEA World Energy Balances for historical analysis from witch/build/data_historical_values.gdx file
#which year to use:
year_weo = 2016
year_irena = 2018
year_platts = 2015



#First, get energy data
if(file.exists("witch/data_historical_from_witch_preloaded.RData")){
  load("witch/data_historical_from_witch_preloaded.RData")
}else{
  require(gdxtools)
  gdxhistfile <- gdx(data_historical_values_file)
  K_EN_irena <- gdxhistfile["k_en_valid_irena"]
  K_EN_platts <- gdxhistfile["k_en_valid_platts_tot"]
  Q_OUT <- gdxhistfile["q_out_valid_weo"]
  Q_FUEL <- gdxhistfile["q_fuel_valid_weo"]
  Q_EN <- gdxhistfile["q_en_valid_weo"]
  Q_FEN <- gdxhistfile["q_fen_valid_weo"]
  Q_OUT_TYPE <- gdxhistfile["q_out_type_valid_weo"]
  TPES <- gdxhistfile["tpes_valid_weo"]
  save.image("witch/data_historical_from_witch_preloaded.RData")
}

#Install and load required packages
require_package <- function(package, loading = T){
  if (!package %in% rownames(installed.packages())) try(install.packages(package, repos = "http://cran.rstudio.com"), silent = TRUE)
  if (loading) suppressPackageStartupMessages(library(package,character.only = T, quietly = TRUE))
}
pkgs <- c('tidyverse', 'openxlsx', 'countrycode', 'data.table', 'GGally', 'maps', 'viridis', 'sf', 'rnaturalearth', 'scatterpie')
res <- lapply(pkgs, require_package)
#function to save plots
saveplot <- function(plotname, width=7, height=5, text_size=10, figure_format="png"){
  if(!dir.exists(resdir)){dir.create(resdir)} #create directory for graphs
  print(ggplot2::last_plot())# + labs(title=plotname) + theme(text = element_text(size=text_size), legend.position=legend_position, legend.direction = legend_direction, legend.key = element_rect(colour = NA), legend.title=element_blank()) ); 
  ggsave(filename=file.path(resdir,paste0(gsub("[ |_|-]", "_", plotname),".",figure_format)), plot = ggplot2::last_plot(), width=width, height=height)
}
### END OF FUNCTIONS ###


#Load main Excel file
loaddata = read.xlsx(xlsxFile = energy_jobs_database_file, sheet = "FinalAllcountries", startRow = 2)
loaddata <- loaddata %>% select(-Comments, -Job.Types)

rawdata <- loaddata %>% filter(!is.na(`Country.-.region.name`)) 
rawdata <- rawdata %>% mutate(country = sub("\\-.*", "", `Country.-.region.name`)) %>% mutate(iso3 = countrycode(sourcevar = country, origin = "country.name", destination = "iso3c", warn = F)) %>% select(-`Country.-.region.name`, -country)


#get good category names
catfuelnames <- paste0(t(rawdata[1:2,])[,1],"-",t(rawdata[1:2,])[,2])
names(rawdata) <- sub("NA-NA", "iso3", catfuelnames)
rawdata <- rawdata[-(1:2), , drop = FALSE]

#convert to long format
rawdata_long <- reshape2::melt(rawdata, id.vars = "iso3", variable.name = "variable")  
rawdata_long <- rawdata_long %>% mutate(value = as.numeric(value)) 

rawdata_long <- rawdata_long %>% mutate(fullname = variable) %>% separate(variable, c("fuel","category"), sep = "-")

#For wind_offshore take wind_onshore if 0 (=NA)
rawdata_long <- rawdata_long %>% group_by(iso3, category) %>% mutate(value=ifelse(fuel=="wind_off",ifelse(value==0, mean(value[fuel=="wind_on"]), value), value))

#All zeros are missing values
rawdata_long <- rawdata_long %>% filter(value!=0)


#Convert all units to PJ and GW
#here just extraction for all fuels, and only  refinery
#refinery, .0611     # kbarrels/day -> PJ/day
fuel_conv_units <- "fuel, conv
coal_hard, 22.74       # Mt -> PJ
coal_lignite, 10    # Mt -> PJ
oil, 0.0418         # ktoe -> PJ
oil_unconv, 0.0418  # ktoe -> PJ
gas, 0.0418         # ktoe -> PJ
gas_unconv, 0.0418  # ktoe -> PJ
biodiesel, 0.0327   # Ml -> PJ
ethanol, 0.0214     # Ml -> PJ
"
fuel_conv_units <- read_csv(fuel_conv_units, comment = "#")
rawdata_long <- merge(rawdata_long, fuel_conv_units, by="fuel", all=T)
rawdata_long <- rawdata_long %>%
  mutate(value = ifelse(category=="extraction" & fuel %in% fuel_conv_units$fuel, value / conv, value)) %>%
  mutate(value = ifelse(category == "refinery", value / (.00601 *365), value)) %>%
  select(fuel,category,iso3,value)


conv_unconv <- Q_OUT_TYPE %>% filter(year == year_weo) %>% mutate(fuel=case_when(f=="oil" & type=="conv" ~ "oil", f=="oil" & type=="nonconv" ~ "oil_unconv", f=="coal" & type=="nonconv" ~ "coal_lignite", f=="coal" & type=="conv" ~ "coal_hard")) %>% select(-f, -type, -year) %>% rename(share=value)
rawdata_long <- rawdata_long %>% left_join(conv_unconv) %>% mutate(share=ifelse(is.na(share), 1, share)) %>% mutate(fuel = ifelse(str_detect(fuel, "solar|wind|hydro"), fuel, str_replace(fuel,"\\_.*",""))) %>% group_by(iso3,fuel,category) %>% dplyr::summarize(value=weighted.mean(value, w = share, na.rm = T)) %>% ungroup() %>% as.data.frame()

#For hydro, compute weighted average using shares of large and small hydro where available:
hydro_shares <- read.xlsx(xlsxFile = energy_jobs_database_file, sheet = "Hydro", rows = seq(31,36), cols = c(1, 2, 3)) 
continents <- as.data.frame(codelist$iso3c)
continents <- continents %>% dplyr::rename(iso3c="codelist$iso3c") %>% filter(!is.na(iso3c)) %>% mutate(continent=countrycode(iso3c, origin = "iso3c", destination = "continent")) %>% dplyr::rename(iso3=iso3c) %>% mutate(iso3=as.character(iso3))
hydro_shares <- continents %>% full_join(melt(as.data.table(hydro_shares), id.vars = "continent", variable.name = "fuel", value.name = "share") %>% mutate(fuel=as.character(fuel))) %>% select(-continent)
#apply Hydro relative shares
rawdata_long <- rawdata_long %>% left_join(hydro_shares) %>% mutate(share=ifelse(is.na(share), 1, share)) %>% mutate(fuel = ifelse(str_detect(fuel, "hydro"), str_replace(fuel,"\\_.*",""), fuel)) %>% group_by(iso3,fuel,category) %>% dplyr::summarize(value=weighted.mean(value, w = share, na.rm = T)) %>% ungroup() %>% as.data.frame()

#biofuels: assign as trbiofuel (traditional biofuel)
rawdata_long <- rawdata_long %>% mutate(fuel = ifelse(str_detect(fuel, "ethanol|biodiesel"), "trbiofuel", fuel)) %>% 
  group_by(iso3,fuel,category) %>% dplyr::summarize(value=mean(value, na.rm = T)) %>% ungroup() %>% as.data.frame()

#Get Job units correct
# Apply 17% of .17 for ethanol and biodiesel jobs to only have direct jobs (not indirect nor induced)
#Based on Urbanchuk, J. M. (2019) paper. We consider only Agriculture & Ethanol production jobs  
rawdata_long <- rawdata_long %>% mutate(value = ifelse(fuel %in% c("biodiesel", "ethanol") & category=="refinery", value * 0.17, value))


add_fossil_manufacturing = T
#Add manufacturing for coal, gas, and nuclear power plants (based on Dominich et al. https://link.springer.com/chapter/10.1007/978-3-030-05843-2_10) Table 10.1
#Job years/MW: Coal	5.1, Gas	2.9, Nuclear	1.3
#assign for USA and China (oecd/non-oecd) which then will be assigned to the total world number
#For oil, assume it is the same as gas power plants
manufacturing_dominich <-    fread("fuel, category, value
                                    coal,    manufacturing, 5.1
                                    gas,     manufacturing, 2.9
                                    oil,     manufacturing, 2.9
                                    nuclear, manufacturing, 1.3")
manufacturing_dominich <- rbind(manufacturing_dominich %>% mutate(iso3="USA"), manufacturing_dominich %>% mutate(iso3="CHN"))
if(add_fossil_manufacturing) rawdata_long <- rbind(rawdata_long, manufacturing_dominich %>% mutate(value=value*1e3))
#For Construction & Manufacturing: convert job years to jobs. The below is the construction times (Rutovitz, 2015) (for wind: wind_on:2, wind_off 4: take value of 3)
# https://opus.lib.uts.edu.au/bitstream/10453/43718/1/Rutovitzetal2015Calculatingglobalenergysectorjobsmethodology.pdf
construction_time <- "fuel, years
coal, 5 
gas, 2 
oil, 2
nuclear, 10
biomass, 2
hydro, 2
solar_pv, 1
solar_csp, 1
wind_on, 2
wind_off, 4
geothermal, 2"
construction_time <- read_csv(construction_time)
rawdata_long <- merge(rawdata_long, construction_time, by = "fuel", all=T)
rawdata_long <- rawdata_long %>% mutate(value=ifelse(category %in% c("manufacturing", "construction"), value / years, value)) %>% select(-years) %>% filter(!is.na(value))
setcolorder(rawdata_long,c('iso3','category','fuel','value'))
energy_jobs_intensity <- rawdata_long





###############  COMPUTE HISTORICAL VALUES BASED ON WEO #######################




############Manufacturing (el-pv) & Refinery (oil) & Extraction (uranium): Check how global pool values should be assigned to countries ########
#Oil refinery
refinery <- read.xlsx(xlsxFile = energy_jobs_database_file, sheet = "Global_Refining", startRow = 4, rows = seq(4,62), cols = c(1, 35)) #for 2013
refinery <- refinery %>% mutate(iso3 = countrycode(X1, "country.name", "iso3c", warn = F))
refinery <- refinery %>% filter(!is.na(iso3)) %>% mutate(value=`2013`) %>% select(-X1, -`2013`)
#check correlations with extraction or consumption of oil (lates year is 2013)
refinery_merged <- merge(refinery_long %>% rename(refinery=value), Q_OUT %>% filter(f == "oil" & year == 2013) %>% select(-f) %>% rename(extraction=value), by = c("iso3", "year"))
refinery_merged <- merge(refinery_merged, Q_FUEL %>% filter(fuel == "oil" & year == 2013) %>% select(-fuel) %>% rename(consumption=value), by = c("iso3", "year"))
print(ggpairs(refinery_merged %>% select(-year, -iso3) %>% mutate(refinery=log(refinery), extraction=log(extraction), consumption=log(consumption)), aes(alpha = 0.4)))
saveplot("refinery oil extraction consumption correlation.png")
#Correlation of refinery with consumption is 0.916, with extraction only 0.094 => leave it at consumption level, no global pool!
#Solar PV Panels 
global_pool_share <- read.xlsx(xlsxFile = energy_jobs_database_file, sheet = "Global_Manufacturing_solar_pv", startRow = 7,rows = seq(7,17))
global_pool_share <- global_pool_share %>% mutate(iso3 = countrycode(X1, "country.name", "iso3c", warn = F))
#Europe is mostly Germany
global_pool_share <- global_pool_share %>% mutate(iso3=ifelse(X1=="Europe", "DEU", iso3)) %>% select(-Region,-Worldwide)
global_pool_share <- global_pool_share %>% select(-X1) %>% filter(!is.na(X2)) %>% rename(value=X2)
#Remove other (8% of PV manufacturing and recompute shares)
global_pool_share <- global_pool_share %>% filter(!is.na(iso3)) %>% mutate(value=value / sum(value) * 100) %>% mutate(fuel="solar_pv", category="manufacturing")
setcolorder(global_pool_share,c('iso3','fuel','value'))
#Now add uranium extraction (shares based on last year extraction, for now not reserves)
uranium_share <- read.xlsx(xlsxFile = energy_jobs_database_file, sheet = "Uranium",rows = seq(39,46), cols = c(1,2))
uranium_share <- uranium_share %>% mutate(iso3 = countrycode(country, "country.name", "iso3c", warn = F)) %>% select(-country) %>% dplyr::rename(value = extraction_share) %>% mutate(fuel="nuclear", category="extraction")
global_pool_share <- rbind(global_pool_share, uranium_share)



#combine capacity from IRENA and PLATTS
K_EN <- rbind(K_EN_platts %>% filter(year==year_platts), K_EN_irena %>% filter(!(jreal %in% unique(K_EN_platts$jreal))) %>% filter(year==year_irena))
#for Capacity addidions (I_EN), for now take annual average over the change between 2010 and 2015 for platts and 2018 for IRENA
I_EN <- rbind(K_EN_platts %>% group_by(iso3, jreal) %>% dplyr::mutate(value=pmax(0,(value-dplyr::lag(value))/5)) %>% filter(year==year_platts), K_EN_irena %>% filter(!(jreal %in% unique(K_EN_platts$jreal))) %>% group_by(iso3, jreal) %>% dplyr::mutate(value=pmax(value-dplyr::lag(value), 0)) %>% filter(year==year_irena))
#Extraction: last year is 2016
Q_OUT <- Q_OUT %>% filter(year==year_weo)
#Consumption (only for Oil refinery)
Q_FUEL <- Q_FUEL %>% filter(year==year_weo & (fuel=="oil" | fuel=="trbiofuel"))

#harmonize WEO data
TW2GW=1e3
TWh2PJ=3.6

Q_OUT <- Q_OUT %>% dplyr::rename(fuel=f) %>% mutate(value=value*TWh2PJ) %>% mutate(category="extraction") %>% mutate(fuel=gsub("uranium", "nuclear", fuel))
Q_FUEL <- Q_FUEL %>% mutate(value=value*TWh2PJ) %>% mutate(category="refinery") #for refinery

#for Oil refinery, instead take actual refinery capacity from above (OPEC table) (year is year_weo (while in fact last year is 2013) to be compatible with taking the WEO FUEL data)
refinery_PJ <- refinery %>% mutate(value = value * (.00601 *365)) %>% mutate(year=year_weo, category="refinery", fuel="oil")
Q_FUEL <- rbind(Q_FUEL %>% filter(fuel!="oil"), refinery_PJ)

#K_EN and I_EN:
map_jreal_fuel <- "jreal, fuel
elcigcc, coal
elgastr, gas
elhydro, hydro
elnuclear, nuclear
eloil, oil
elpb, biomass
elpc, coal
elgeo, geothermal
elpv, solar_pv
elwindon, wind_on
elwindoff, wind_off
elbagasse, NA
elbioenergy, NA
elbiofuel, trbiofuel
elbiogas, biogas
elcsp, solar_csp
elmarine, NA
elothersolidbiofuels, NA
elpumpstorage, NA
elrenhydro, NA
elrentotal, NA
elsolar, NA
elwaste, NA
elwind, NA
"
map_jreal_fuel <- read_csv(map_jreal_fuel)
K_EN <- K_EN %>% left_join(map_jreal_fuel) %>% group_by(year,iso3,fuel) %>% summarize(value=sum(value)) %>% mutate(value=value*TW2GW) %>% filter(!is.na(fuel)) %>% mutate(category="oem") %>% as.data.frame()
I_EN <- I_EN %>% left_join(map_jreal_fuel) %>% group_by(year,iso3,fuel) %>% summarize(value=sum(value)) %>% mutate(value=value*TW2GW) %>% filter(!is.na(fuel)) %>% as.data.frame()
#I_EN for construction and manufacturing
I_EN <- rbind(I_EN %>% mutate(category="construction"), I_EN %>% mutate(category="manufacturing")) 

#full data
EN_DATA <- rbind(Q_OUT, I_EN, K_EN, Q_FUEL)

#combine energy with jobs data
energy_jobs <- merge(EN_DATA, energy_jobs_intensity %>% rename(jobs_per_unit=value), by = c("fuel", "category", "iso3"), all = T)
#for NAs in value set to zero, year to the yar of other in the same category
energy_jobs <- energy_jobs %>% group_by(category,fuel) %>% mutate(value=ifelse(is.na(value), 0, value), year=ifelse(is.na(year), mean(as.numeric(year), na.rm = T), year)) %>% ungroup()

#Compute total number of jobs
energy_jobs <- energy_jobs %>% mutate(jobs=jobs_per_unit*value) 

#Exceptions: For Solar PV Manufacturing attribute global production using the current manufacturing shares of countries; same for Uranium (nuclear extraction), take global sum of extraction (=consumption globally) and distribute by shares
energy_jobs <- energy_jobs %>% left_join(global_pool_share %>% rename(share=value)) %>% group_by(year, category, fuel) %>% mutate(jobs=ifelse(((category=="manufacturing" & fuel=="solar_pv") | (category=="extraction" & fuel=="nuclear")), jobs_per_unit * share/100 * sum(value, na.rm = T), jobs)) %>% ungroup() %>% select(-share) 



#Exceptions: for refinery (now coded as "refinery" for "oil" (only refinery) and "trbiofuel" (production of biofuel and refinery)) -> use Total Oil Consumption (PES) instead of Q_OUT  
#energy_jobs <- energy_jobs %>% left_join(Q_FUEL) %>% mutate(value = ifelse((category=="refinery" & (fuel=="oil" | fuel=="trbiofuel")), PES, value)) %>% mutate(jobs=jobs_per_unit*value) %>% select(-PES)
 

############### Save data for modeling input ##########
write.csv(energy_jobs_intensity, file.path(resdir, "energy_jobs_intensity.csv"), row.names = F)
write.csv(global_pool_share, file.path(resdir, "global_production_shares.csv"), row.names = F)












################ Figures and Analysis #################

ggplot(energy_jobs_intensity) + geom_point(aes(fuel, value, color=iso3)) + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(category ~ ., nrow = 1)
saveplot("jobs all categories", width = 12, height = 5)

#Map of data availability
world_map <- map_data("world")
world_map <- world_map %>% mutate(iso3=countrycode(region, "country.name", "iso3c", warn = F))
world_map <- full_join(energy_jobs_intensity %>% group_by(iso3) %>% summarize(job_data_points=length(value)), world_map, by = "iso3")
world_map <- world_map %>% mutate(job_data_points=ifelse(is.na(job_data_points),0,job_data_points))
ggplot(world_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = job_data_points ), color = "black") +
  scale_fill_gradient2(low = "white", high = "blue")
#  scale_fill_viridis_c(option = "C")
saveplot("data availability map", width = 12, height = 5)

#Current and historical energy jobs
print(sum(energy_jobs$jobs, na.rm = T))
ggplot(energy_jobs) + geom_bar(aes(category, jobs, fill=fuel), position="stack", stat="identity")
saveplot("jobs last year total")
ggplot(energy_jobs) + geom_bar(aes(fuel, jobs, fill=category), position="stack", stat="identity")
saveplot("jobs last year total by fuel")
ggplot(energy_jobs) + geom_bar(aes(iso3, jobs, fill=fuel), position="stack", stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
saveplot("jobs last year total by country")

#show total number of current jobs
print(sum(energy_jobs$jobs, na.rm = T))











# using the WITCH module, complete a full set of iso3 data using the same extrapolations as in the module for global coverage also for the historical data
energy_jobs_iso3_completed <- energy_jobs

#Add OECD and witch regions
energy_jobs_iso3_completed <- energy_jobs_iso3_completed %>% mutate(oecd=ifelse(iso3 %in% c("AUS", "AUT", "BEL", "CAN", "CHL", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA"), "oecd", "non-oecd"))
energy_jobs_iso3_completed <- energy_jobs_iso3_completed %>% left_join(read.xlsx(energy_jobs_database_file, sheet = "witch17regmap"))

#first take weighted mean across WITCH17 cregions (like using witchtools)
energy_jobs_iso3_completed <- energy_jobs_iso3_completed %>% group_by(fuel, category, year, witch17) %>% mutate(jobs_per_unit = ifelse(is.na(jobs_per_unit), weighted.mean(jobs_per_unit, w = value+1e-12, na.rm = T), jobs_per_unit))

# for NA values take corresponding oecd/non oecd mean
energy_jobs_iso3_completed <- energy_jobs_iso3_completed %>% group_by(fuel, category, year, oecd) %>% mutate(jobs_per_unit = ifelse(is.na(jobs_per_unit), weighted.mean(jobs_per_unit, w = value+1e-12, na.rm = T), jobs_per_unit))

#construction & OEM of oil fired power plants equal to gas
energy_jobs_iso3_completed <- energy_jobs_iso3_completed %>% group_by(category, year, iso3) %>% mutate(jobs_per_unit = ifelse(category %in% c("construction", "oem"), ifelse(fuel=="oil", jobs_per_unit[fuel=="gas"], jobs_per_unit), jobs_per_unit))
#no oem/construction power plants for biogas nor trbiofuel
energy_jobs_iso3_completed <- energy_jobs_iso3_completed %>% mutate(jobs_per_unit = ifelse(category %in% c("construction", "oem"), ifelse(fuel %in% c("biogas", "trbiofuel"), 0, jobs_per_unit), jobs_per_unit))
#manufacturing only for biomass, hydro, solar, and wind
manufacturing_sectors <- c("biomass", "hydro", "solar_pv", "solar_csp", "wind_on", "wind_off")
if(add_fossil_manufacturing) manufacturing_sectors <- c("biomass", "hydro", "solar_pv", "solar_csp", "wind_on", "wind_off", "coal", "gas", "nuclear", "oil")
energy_jobs_iso3_completed <- energy_jobs_iso3_completed %>% mutate(jobs_per_unit = ifelse(category %in% c("manufacturing"), ifelse(fuel %in% manufacturing_sectors, jobs_per_unit, 0), jobs_per_unit))
# geothermal
energy_jobs_iso3_completed <- energy_jobs_iso3_completed %>% mutate(jobs_per_unit = ifelse(fuel %in% c("geothermal"), 0, jobs_per_unit))
#now remove geothermal and biogas which are for now used
energy_jobs_iso3_completed <- energy_jobs_iso3_completed %>% filter(!(fuel %in% c("geothermal", "biogas")))

#recompute global jobs for all iso3 countries for which the quantity data is available based on the extrapolation of the witch module
energy_jobs_iso3_completed <- energy_jobs_iso3_completed %>% mutate(jobs=jobs_per_unit*value)
#get PES to compute jobs intensity
energy_jobs_iso3_completed <- energy_jobs_iso3_completed %>% full_join(TPES %>% filter(year==year_weo) %>% select(-year) %>% rename(TPES=value) %>% mutate(TPES=TPES*TWh2PJ))
energy_jobs_iso3_completed$jobs_intensity <- energy_jobs_iso3_completed$jobs/energy_jobs_iso3_completed$TPES
#show total number of current jobs
print(sum(energy_jobs_iso3_completed$jobs, na.rm = T))


# World Map of historical data
energy_jobs_iso3_completed_for_map <- energy_jobs_iso3_completed %>% select(-year, -oecd, -witch17)
#country average intensity
energy_jobs_iso3_completed_for_map_summarized <- energy_jobs_iso3_completed_for_map %>% group_by(iso3) %>% summarize(jobs_intensity=sum(jobs)/mean(TPES), total_jobs=sum(jobs, na.rm = T))
#Map of data availability
world_map <- map_data("world")
world_map <- world_map %>% mutate(iso3=countrycode(region, "country.name", "iso3c", warn = F))
world_map_enint <- full_join(energy_jobs_iso3_completed_for_map_summarized, world_map, by = "iso3")
ggplot(world_map_enint, aes(long, lat, group = group))+
  geom_polygon(aes(fill = jobs_intensity, text=paste('Country: ', region,' <br>Jobs intensity: ',round(jobs_intensity,1), 'jobs/PJ <br>Total Jobs: ',round(total_jobs/1000,1), 'thousand')), color = "black") +
#  scale_fill_gradientn(colours = rainbow(7))
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  xlab("") + ylab("") + ggtitle("Energy Intensity [jobs/PJ] in 2020")
saveplot("interactive map", width = 12, height = 5)



#add Europe map with added pie share charts
europe <- ne_countries(scale = "medium", returnclass = "sf", continent = "europe")
sf::sf_use_s2(FALSE)
world_points <- st_centroid(europe)
# extract labels
world_points <- cbind(europe, st_coordinates(st_centroid(europe$geometry)))
#add my data to country map
europe <- europe %>% left_join(energy_jobs_iso3_completed_for_map_summarized %>% dplyr::rename(iso_a3=iso3))
p_eu <- ggplot(data = europe) + geom_sf(aes(fill = jobs_intensity)) +  scale_fill_viridis_c(option = "plasma", direction = -1) + xlab("") + ylab("") + ggtitle("Energy Intensity [jobs/PJ] in 2020") + coord_sf(xlim = c(-10, 40), ylim = c(35, 72))
#now just grey
p_eu <- ggplot(data = europe) + geom_sf(aes()) + xlab("") + ylab("") + ggtitle("Energy Jobs in Europe") + coord_sf(xlim = c(-10, 40), ylim = c(35, 72))
#add data for small pie charts
data_for_pie <- energy_jobs_iso3_completed %>% ungroup() %>% select(-year) %>% mutate(jobs=ifelse(is.na(jobs),0,jobs)) %>% group_by(fuel,iso3) %>% summarize(totjobs=sum(jobs, na.rm = T)) %>% pivot_wider(id_cols = c(iso3), names_from = fuel, values_from = totjobs) %>% replace(is.na(.), 0) %>% mutate(total_energy_jobs=coal+gas+oil+biomass+hydro+nuclear+wind_on+wind_off+trbiofuel+solar_pv+solar_csp) %>% mutate(totlabel=ifelse(total_energy_jobs>99999,paste0(round(total_energy_jobs*1e-6,1),"M"),paste0(round(total_energy_jobs*1e-3,1),"k")))
data_for_pie <- data_for_pie %>% dplyr::rename(iso_a3=iso3) %>% right_join(world_points) %>% filter(!is.na(total_energy_jobs))
data_for_pie <- data_for_pie %>% filter(pop_est>5000000) #only more then 10 million people
data_for_pie <- data_for_pie %>% mutate(X=ifelse(iso_a3=="FRA", 3, X), Y=ifelse(iso_a3=="FRA", 47, Y)) #France is off
p_eu + geom_scatterpie(aes(x=X, y=Y, group=iso_a3),pie_scale=1, data=data_for_pie, cols=c("biomass", "coal", "gas", "hydro", "nuclear", "oil", "wind_on", "wind_off", "solar_pv", "solar_csp", "trbiofuel"), alpha=.8) + scale_fill_manual(values=c("solar_pv"="yellow", "solar_csp"="darkgoldenrod2", "hydro"="blue", "nuclear"="cyan", "wind_on"="orange", "wind_off"="orange3", "coal"="black", "gas"="brown", "oil"="darkorchid4", "biomass"="darkgreen", "trbiofuel"="chartreuse3"), name="Energy Technology") + 
  geom_label(aes(x=X, y=Y, group=iso_a3, label=totlabel), color="grey9", size=5,data=data_for_pie, alpha=0.7)
saveplot("europe map", width = 12, height = 10)





create_animated_graph <- T
if(create_animated_graph){
require(plotly)
require(htmlwidgets)
p_map <- ggplotly(tooltip = c("text"))
#then export as html in the subfolder "dataviz"

#pie chart for each country
iso3_plot <- "USA"
ggplot(energy_jobs_iso3_completed %>% filter(iso3==iso3_plot) %>% group_by(iso3,fuel) %>% summarize(jobs=sum(jobs)) %>% arrange(jobs) %>% mutate(label_position=ifelse(is.na(lag(jobs)), jobs, cumsum(jobs) - .5*(cumsum(jobs)-lag(cumsum(jobs)))))) + geom_bar(aes(x = iso3, y = jobs, fill=reorder(fuel, -jobs)), alpha = 0.5, position="stack", stat="identity") + scale_fill_manual(values=c("solar_pv"="yellow", "solar_csp"="darkgoldenrod2", "hydro"="blue", "nuclear"="cyan", "wind_on"="orange", "wind_off"="orange3", "coal"="black", "gas"="brown", "oil"="darkorchid4", "biomass"="darkgreen", "trbiofuel"="chartreuse3"), name="Energy Technology") + xlab("") +  ylab("") +
theme_minimal()+theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  ) +
  theme(axis.text.x=element_blank()) + theme(axis.text.y=element_blank()) +
  geom_text(aes(x=iso3, y = label_position, label = round(jobs/1000)), size=6) +
  coord_polar("y", start=0)

ggsave(str_glue("dataviz/country_chart_{iso3_plot}.png"))


#Now nice names
nicename <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  if(x=="Oem") x <- "O&M"
  if(str_detect(x, "_")) x <- paste0(str_split(x, "_")[[1]][1], " (", toupper(str_split(x, "_")[[1]][2]), ")")
  return(x)
}
color_palette_type <- c("construction"="red", "extraction"="black", "manufacturing"="blue", "oem"="orange", "refinery"="darkgreen")
color_palette_fuel <- c("solar_pv"="yellow", "solar_csp"="darkgoldenrod2", "hydro"="blue", "nuclear"="cyan", "wind_on"="orange", "wind_off"="orange3", "coal"="black", "gas"="brown", "oil"="darkorchid4", "biomass"="darkgreen", "trbiofuel"="chartreuse3")
names(color_palette_fuel) <- unlist(lapply(names(color_palette_fuel), nicename))
names(color_palette_type) <- unlist(lapply(names(color_palette_type), nicename))
#rowwise, very slow!!!
energy_jobs_iso3_completed <- energy_jobs_iso3_completed %>% rowwise() %>% mutate(category=nicename(category), fuel=nicename(fuel)) %>% as.data.frame()


#bubble chart across fuels and types for the USA
ggplot(energy_jobs_iso3_completed %>% filter(iso3==iso3_plot & jobs>0) %>% group_by(iso3,fuel,category) %>% summarize(jobs=sum(jobs))) + geom_point(aes(x = category, y = fuel, color=fuel, size=jobs/1000)) + scale_color_manual(values=c("solar_pv"="yellow", "solar_csp"="darkgoldenrod2", "hydro"="blue", "nuclear"="cyan", "wind_on"="orange", "wind_off"="orange3", "coal"="black", "gas"="brown", "oil"="darkorchid4", "biomass"="darkgreen", "trbiofuel"="chartreuse3"), name="Energy Technology") + xlab("") +  ylab("") + ggtitle(str_glue("Energy Jobs in {iso3_plot} 2020")) + labs(size="Jobs [Tsd.]") + theme_bw()
ggsave(str_glue("dataviz/bubble_chart_{iso3_plot}.png"))

#}

#Combined bubble and pie chart for global values and showing countries
ggplot(energy_jobs_iso3_completed %>% group_by(fuel, category, witch17) %>% summarize(jobs=sum(jobs, na.rm = T)) %>% ungroup() %>% group_by(fuel,category) %>% mutate(job_region_share=jobs/sum(jobs, na.rm = T), totjobs_fuel_cat=sum(jobs, na.rm=T)) %>% mutate(piesize=log10(totjobs_fuel_cat)),aes(x = factor(1),y=job_region_share,fill=witch17)) + 
  geom_bar(width=1, stat = "identity") + facet_grid(category ~ fuel)+
  coord_polar(theta = "y") +
  scale_fill_manual(values = c(usa="darkblue",te="gold2",mena="darkgoldenrod4",ssa="goldenrod",sasia="darkorange2",china="deeppink3",easia="orangered",laca="#fbb714",india="#fbf003",europe="blue",indonesia="lightsalmon3",mexico="slateblue2",brazil="tomato4",canada="blueviolet",jpnkor="darkseagreen",oceania="forestgreen",southafrica="indianred3",seasia="orangered", "Global Pool"="black"), name = "Region") +
  theme_bw() + scale_x_discrete("",breaks=NULL) + scale_y_continuous("",breaks=NULL) + 
  ggtitle("Relative share across regions of all job types and technologies") #
#+ theme(panel.border=element_blank(),strip.background=element_blank(),panel.grid=element_blank())
#theme(panel.border=element_blank(),strip.text=element_blank(),strip.background=element_blank(),legend.position="none",panel.grid=element_blank())

#different sizes (See https://stackoverflow.com/questions/8612920/pie-charts-in-ggplot2-with-variable-pie-sizes)
#add TOTAL values per technology
energy_jobs_iso3_completed_withtotal <- rbind(energy_jobs_iso3_completed %>% select(fuel, year, iso3, witch17, jobs, category), energy_jobs_iso3_completed %>% group_by(fuel, year, iso3, witch17) %>% summarize(jobs=sum(jobs)) %>% mutate(category="Total")) %>% ungroup()

ggplot(energy_jobs_iso3_completed_withtotal %>% filter(jobs>0) %>% group_by(fuel, category, witch17) %>% summarize(jobs=sum(jobs, na.rm = T)) %>% ungroup() %>% group_by(fuel,category) %>% mutate(job_region_share=jobs/sum(jobs, na.rm = T), totjobs_fuel_cat=sum(jobs, na.rm=T)) %>% mutate(piesize=log10(totjobs_fuel_cat)) %>% mutate(totlabel=ifelse(totjobs_fuel_cat>99999,paste0(round(totjobs_fuel_cat*1e-6,1),"M"),paste0(round(totjobs_fuel_cat*1e-3,1),"k"))), aes(x = (piesize)/2,y=job_region_share,fill=witch17, width=(piesize))) + 
  geom_bar(stat = "identity", position="fill") + facet_grid(category ~ fuel)+
  coord_polar(theta = "y") +
  scale_fill_manual(values = c(usa="darkblue",te="gold2",mena="darkgoldenrod4",ssa="goldenrod",sasia="darkorange2",china="deeppink3",easia="orangered",laca="#fbb714",india="#fbf003",europe="blue",indonesia="lightsalmon3",mexico="slateblue2",brazil="tomato4",canada="blueviolet",jpnkor="darkseagreen",oceania="forestgreen",southafrica="indianred3",seasia="orangered", "Global Pool"="black"), name = "Region") +
  theme_bw() + scale_x_discrete("",breaks=NULL) + scale_y_continuous("",breaks=NULL) + 
  ggtitle("Jobs across regions in 2020") +
  theme(legend.key = element_blank(), strip.background = element_rect(colour="black", fill="#FFFFFF")) +
geom_text(aes(x=0, y = 0, label = totlabel), size=3, color="grey25") + 
  theme(
    legend.title = element_text(color = "black", size = 10),
    legend.text = element_text(color = "black", size = 9),
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(0.5,"cm"),
    strip.text = element_text(face="bold", size=6)
  )
ggsave(str_glue("results/bubble_pie_chart_global_2020.pdf"), width = 8, height = 4)

}



############### Save data for modeling input, now taking completed ISO3 dataset and overwrite csv file ##########
energy_jobs_intensity_fulliso3 <- energy_jobs_iso3_completed %>% ungroup() %>% select(-value) %>% rename(value=jobs_per_unit) %>% select_at(names(energy_jobs_intensity)) %>% as.data.frame()
write.csv(energy_jobs_intensity_fulliso3, file.path(resdir, "energy_jobs_intensity.csv"), row.names = F)


#now also save full iso3 dataset of total job in the latest available year (2016)
write.csv(energy_jobs_iso3_completed %>% ungroup() %>% select(fuel, category, iso3, jobs) %>% mutate(jobs=round(jobs)) %>% filter(!is.na(jobs)) %>% as.data.frame(), file.path(resdir, "energy_jobs_total_jobs_last_year.csv"), row.names = F)

#for iiasadb
energy_jobs_intensity_fulliso3_iiasadb <- energy_jobs_iso3_completed %>% ungroup() %>% rename(energyquantity=value, value=jobs_per_unit) %>% select_at(c(names(energy_jobs_intensity), "energyquantity")) %>% as.data.frame()
write.csv(energy_jobs_intensity_fulliso3_iiasadb, file.path("energy_jobs_intensity.csv"), row.names = F)





