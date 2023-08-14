# Test run

# Small script to run carbon plus estimations. 
# Calls carbonplus_trigger_Rscript.R

rm(list=ls())

library(tidyverse)
library(jsonlite)

source("carbonplus_main.R")

settings <- list(
  n_runs = 2,
  se_field_carbon_in=0.10,
  get_grazing_estimates=TRUE,
  debug_mode = FALSE, 
  save2mongoDB = FALSE,
  yearX_landuse=1, 
  yearX_livestock=1, 
  copy_yearX_to_following_years_landUse=FALSE,
  copy_yearX_to_following_years_livestock=FALSE,
  server="dev"  # One of: "prod", dev", "test"
)

sensitive_data_loc <- "../sensitive-data"
init_file <- fromJSON(file.path(sensitive_data_loc,"init_file.json"))
jsonfile <- "../../data/farms/ManuealTroyaCantos_Dev20230707.json"
out <- carbonplus_main(init_file=init_file, settings=settings, JSONfile = jsonfile)
