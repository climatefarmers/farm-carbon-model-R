# Test run

# Small script to run carbon plus estimations. 
# Calls carbonplus_trigger_Rscript.R

rm(list=ls())

library(tidyverse)
library(jsonlite)

source("carbonplus_main.R")

sensitive_data_loc <- "../sensitive-data"
init_file <- fromJSON(file.path(sensitive_data_loc,"init_file.json"))
jsonfile <- "../../data/farms/ManuealTroyaCantos_Dev20230707.json"
out <- carbonplus_main(init_file=init_file, JSONfile = jsonfile)
