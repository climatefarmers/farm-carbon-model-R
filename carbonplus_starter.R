# Test run

# Small script to run carbon plus estimations. 
# Calls carbonplus_trigger_Rscript.R

rm(list=ls())

library(tidyverse)
library(jsonlite)
library(rjson) # for reading in the example farm data
library(here) # for having relative a relative path

source("carbonplus_main.R")

# Read in init_file.json from sensitive-data folder

init_file <- fromJSON(file = here("../sensitive-data", "init_file.json"))

settings_testing <- list(
  n_runs = 2,
  se_field_carbon_in = 0.1,
  se_inputs_nonfarm = 0.025,
  grazing_used = 'min', # One of 'min' (minimum), 'rep' (reported) or 'calc' (calculated). 
  debug_mode = FALSE,
  save2mongoDB = FALSE,
  copy_year_currmonit_to_future = TRUE,
  server = "dev",  # One of: "prod", "dev", "test"
  bare_bl_type = "reported", # One of: "envzone", "reported", "none". USE REPORTED, NOT ENVZONE!
  monitoring_run = TRUE, # TRUE/FALSE. If TRUE, not a prediction run and years after curr_monit_year will be excluded from the output
  curr_monit_year = 2,  # Current monitoring year: what is the last project year for which monitoring data has been provided
  use_test_climate = TRUE,
  predict_amp_effect = FALSE,
  spinup_years = 30,
  proj_start_year = 2021
)
settings <- settings_testing

# # Uncomment block below for monitoring production runs ------
# settings_monitoring <- list(
#   n_runs = 100,
#   se_field_carbon_in = 0.1,
#   se_inputs_nonfarm = 0.025,
#   grazing_used = 'min', # One of 'min' (minimum), 'rep' (reported) or 'calc' (calculated).
#   debug_mode = FALSE,
#   save2mongoDB = FALSE,
#   copy_year_currmonit_to_future = TRUE,
#   server = "dev",  # One of: "prod", "dev", "test"
#   bare_bl_type = "reported", # One of: "envzone", "reported", "none". USE REPORTED, NOT ENVZONE!
#   monitoring_run = TRUE, # TRUE/FALSE. If TRUE, not a prediction run and years after curr_monit_year will be excluded from the output
#   curr_monit_year = 2,  # Current monitoring year: what is the last project year for which monitoring data has been provided
#   use_test_climate = FALSE,
#   predict_amp_effect = FALSE,
#   spinup_years = 300,
#   proj_start_year = 2021
# )
# settings <- settings_monitoring


## Run with mongoDB data -------------------------------------------------------

# farmId <- 
# out <- carbonplus_main(init_file=init_file, settings=settings, farmId=farmId)

# farmIds <- c(
# list of farm_IDs
# )
# 
# for (farmId in farmIds){
#   carbonplus_main(init_file=init_file, settings=settings, farmId=farmId)
# }


## Run with json file ----------------------------------------------------------

jsonfile = here("example-farm-data","example-farm-monitoringData.json")
out <- carbonplus_main(init_file=init_file, settings=settings, JSONfile = jsonfile)

# json_files <- c(
# list of json files
# )
# 
# for (jsonfile in json_files){
#   carbonplus_main(init_file=init_file, settings=settings, JSONfile = jsonfile)
# }

