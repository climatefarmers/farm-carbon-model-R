# Test run

# Small script to run carbon plus estimations. 
# Calls carbonplus_trigger_Rscript.R

rm(list=ls())

library(tidyverse)
library(jsonlite)

source("carbonplus_main.R")
settings_testing <- list(
  n_runs = 2,
  se_field_carbon_in = 0.001,
  get_grazing_estimates = TRUE,
  debug_mode = FALSE,
  save2mongoDB = FALSE,
  copy_yearX_to_following_years_landUse = TRUE,
  copy_yearX_to_following_years_livestock = TRUE,
  yearX_landuse = 2,
  yearX_livestock = 2,
  server = "dev",  # One of: "prod", "dev", "test"
  bare_bl_type = "reported", # One of: "envzone", "reported", "none". USE REPORTED, NOT ENVZONE!
  monitoring_run = TRUE, # (NOT YET IMPLEMENTED) TRUE/FALSE. If TRUE, not a prediction run and years after curr_monit_year will be excluded from the output
  curr_monit_year = 2  # Current monitoring year: what is the last project year for which monitoring data has been provided
)

sensitive_data_loc <- "../sensitive-data"
init_file <- fromJSON(file.path(sensitive_data_loc,"init_file.json"))
jsonfile <- "../../data/pioneer_farms/farm_data/FranciscoAlves_4_Dev20230707.json"
out <- carbonplus_main(init_file=init_file, settings=settings, JSONfile = jsonfile)
