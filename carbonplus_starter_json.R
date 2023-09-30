# Test run

# Small script to run carbon plus estimations. 
# Calls carbonplus_trigger_Rscript.R

rm(list=ls())

library(tidyverse)
library(jsonlite)

source("carbonplus_main.R")

settings_testing <- list(
  n_runs = 5,
  se_field_carbon_in = 0.1,
  se_inputs_nonfarm = 0.025,
  use_calculated_grazing = FALSE,
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
jsonfile <- "../../data/pioneer_farms/farm_data/Alves4_bb393d6d-f952-474e-a790-5486365d929b.json"
out <- carbonplus_main(init_file=init_file, settings=settings, JSONfile = jsonfile)
