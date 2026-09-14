# Create build/data_mod_energy_jobs.gdx

# Load packages
library(witchtools)

pkgs <- c('data.table','stringr','stringi','countrycode','here','docopt')
res <- lapply(pkgs,require_package)
require_gdxtools()
igdx(dirname(Sys.which('gams'))) # Please have gams in your PATH!

'usage: make_data_mod_energy_jobs.R [-w witch_dir] [-d <witch-data dir>]
options:
-w <dir> witch directory
-d <dir> data directory' -> doc
opts <- docopt(doc)

data_dir = ifelse(is.null(opts[["d"]]), here::here("..","witch-data"), as.character(opts["d"]))
witch_dir = ifelse(is.null(opts[["d"]]), here::here(), as.character(opts["w"]))

if (!dir.exists(data_dir)) {
  stop(paste0("data directory '", data_dir, "' does not exist!\n"), call. = FALSE)
}

params = list()
sets = list()

###############################################################################

#read csv files from energy_jobs folder 
energy_jobs_intensity <- fread(file.path(data_dir, 'energy_jobs', "energy_jobs_intensity.csv"))
TW2GW=1e3; TWh2PJ=3.6
#convert units: (all values in jobs per GW (construction, oem, manufacturing) or PJ (extraction and (oem&oil for refinery)))
energy_jobs_intensity$value <-  ifelse((energy_jobs_intensity$category=="extraction" | energy_jobs_intensity$category=="refinery"), energy_jobs_intensity$value * TWh2PJ, energy_jobs_intensity$value * TW2GW)
#clean variables
energy_jobs_intensity$year = 2005
setcolorder(energy_jobs_intensity, c('category', 'fuel', 'year', 'iso3', 'value'))
setnames(energy_jobs_intensity, "fuel", "*")
setnames(energy_jobs_intensity, "category", "*")
#global pool for manufacturing production
global_production_shares <- fread(file.path(data_dir, 'energy_jobs', "global_production_shares.csv"))
global_production_shares$year = 2005
setcolorder(global_production_shares, c('category', 'fuel', 'year', 'iso3', 'value'))
setnames(global_production_shares, "fuel", "*")
setnames(global_production_shares, "category", "*")

params = c(params,list(energy_jobs_intensity = energy_jobs_intensity,
                       global_production_shares = global_production_shares))


meta_param = data.table(param = 'energy_jobs_intensity',
                        var = c('nagg','nweight', 'missing_values'),
                        value = c('mean','gdp', 'NA'))

meta_param0  = data.table(param = 'global_production_shares',
                          var = c('nagg','nweight', 'missing_values'),
                          value = c('sum','gdp', 'NA'))

meta_param = rbindlist(list(meta_param, meta_param0)) 

# Skip extrapolation for all parameters
meta_param0 = data.table(param = names(params), 
                         var = 'extrap', 
                         value = 'skip') 

meta_param = rbindlist(list(meta_param, meta_param0))

sets = c(sets,list(meta_param = meta_param))

invisible(write.gdx(file.path(witch_dir,'input','build','data_mod_energy_jobs.gdx'), params = params, sets = sets))
