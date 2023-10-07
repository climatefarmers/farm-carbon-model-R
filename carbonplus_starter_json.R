# Test run

# Small script to run carbon plus estimations. 
# Calls carbonplus_trigger_Rscript.R

rm(list=ls())

library(tidyverse)
library(jsonlite)

source("carbonplus_main.R")

settings_testing <- list(
  n_runs = 30,
  se_field_carbon_in = 0.1,
  se_inputs_nonfarm = 0.025,
  grazing_used = 'calc', # One of 'min' (minimum), 'rep' (reported) or 'calc' (calculated). 
  debug_mode = FALSE,
  save2mongoDB = FALSE,
  copy_yearX_to_following_years_landUse = TRUE,
  copy_yearX_to_following_years_livestock = TRUE,
  yearX_landuse = 2,
  yearX_livestock = 2,
  server = "dev",  # One of: "prod", "dev", "test"
  bare_bl_type = "reported", # One of: "envzone", "reported", "none". USE REPORTED, NOT ENVZONE!
  monitoring_run = TRUE, # (NOT YET IMPLEMENTED) TRUE/FALSE. If TRUE, not a prediction run and years after curr_monit_year will be excluded from the output
  curr_monit_year = 2,  # Current monitoring year: what is the last project year for which monitoring data has been provided
  use_test_climate = TRUE
)

settings <- settings_testing

sensitive_data_loc <- "../sensitive-data"
init_file <- fromJSON(file.path(sensitive_data_loc,"init_file.json"))

jsonfile <- "../../data/pioneer_farms/farm_data/2023-09-14_corrected_before_animal_numbers_update/Alves4_bb393d6d-f952-474e-a790-5486365d929b.json"
out <- carbonplus_main(init_file=init_file, settings=settings, JSONfile = jsonfile)

# json_files <- c(
#   "../../data/pioneer_farms/farm_data/2023-09-14_corrected_before_animal_numbers_update/Alves1_3f916c12-3a2c-4904-91cb-bb64e6fb0832.json",
#   "../../data/pioneer_farms/farm_data/2023-09-14_corrected_before_animal_numbers_update/Alves2_f67333e8-34a9-4030-93af-766f49d01310.json",
#   "../../data/pioneer_farms/farm_data/2023-09-14_corrected_before_animal_numbers_update/Alves3_584b48dc-0e5d-4ecc-b7d4-9acf281faaba.json",
#   "../../data/pioneer_farms/farm_data/2023-09-14_corrected_before_animal_numbers_update/Alves4_bb393d6d-f952-474e-a790-5486365d929b.json"
#   # "../../data/pioneer_farms/farm_data/Troya_edf5cce8-eee2-40a8-af32-520d2b93ab5c.json",
#   # "../../data/pioneer_farms/farm_data/Valente_7fe9ced2-73b8-45aa-b6a2-a9ede144ca1b.json"
# )

# for (jsonfile in json_files){
#   carbonplus_main(init_file=init_file, settings=settings, JSONfile = jsonfile)
# }