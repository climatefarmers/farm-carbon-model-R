# Test run

# Small script to run carbon plus estimations. 
# Calls carbonplus_trigger_Rscript.R

rm(list=ls())

library(tidyverse)
library(jsonlite)

source("carbonplus_main.R")

settings_testing <- list(
  n_runs = 2,
  se_field_carbon_in = 0.0,
  get_grazing_estimates = TRUE,
  debug_mode = TRUE,
  save2mongoDB = FALSE,
  copy_yearX_to_following_years_landUse = FALSE,
  copy_yearX_to_following_years_livestock = FALSE,
  yearX_landuse = 1,
  yearX_livestock = 1,
  server = "dev",  # One of: "prod", "dev", "test"
  bare_bl_type = "reported", # One of: "envzone", "reported", "none". USE REPORTED, NOT ENVZONE!
  monitoring = TRUE, # Is this a monitoring run? If TRUE, no prediction adjustments are done until curr_monit_year. In particular, AMP projections using pasture_efficiency are deactivated.
  curr_monit_year = 2  # Current monitoring year: what is the last year for which monitoring data has been provided
)

sensitive_data_loc <- "../sensitive-data"
init_file <- fromJSON(file.path(sensitive_data_loc,"init_file.json"))
jsonfile <- "../../data/farms/ManuealTroyaCantos_Dev20230707.json"
out <- carbonplus_main(init_file=init_file, settings=settings, JSONfile = jsonfile)
