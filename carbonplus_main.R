carbonplus_main <- function(init_file, settings, farmId=NA, JSONfile=NA){
  
  ####################################################################
  # This script has the following functions:
  # - prepare log files
  # - set general model settings
  # - get farm data from mongoDB
  # - get farm environmental zone
  # - read in factors
  # - process input data
  # - call the soil model and emissions calculations
  # - upload resulting co2eq to mongoDB
  ####################################################################
  # Settings should be a list with:
  # n_runs: Integer. The number of runs for getting uncertainties. Using 100 for production runs.
  # sd_field_carbon_in: Positive decimal. standard error of the carbon inputs. Using 0.1 as default.
  # debug_mode: Skip some steps. For now just skip fetching and use dummy climate data.
  # save2mongoDB: Set to TRUE for production runs to save to database
  # # To copy the practice of a single year to all others
  # copy_year_currmonit_to_future: Completed data up to year 10 with latest monitored year
  # server: Server to use. One of: "prod", dev", "test"
  # bareground: How baseline bare ground values should be determined: "envzone": uses a regional common practice, "reported": uses the reported current practice (year0) or "none": bare ground always FALSE
  ####################################################################
  
  
  ## Loading libraries ---------------------------------------------------------
  
  library(pacman)
  p_load('pacman', 'SoilR', 'mongolite', 'dplyr', 'tidyr', 'tidyverse',
         'soilassessment', 'readr','aws.s3', 'log4r', 'jsonlite',
         'httr', 'logger', 'ncdf4', 'ncdf4.helpers')
  
  
  ## Prepare log files ---------------------------------------------------------
  # clear and prepare log directory
  if(!dir.exists('logs')) {dir.create('logs')}
  unlink(file.path("logs", "*"), recursive = TRUE)
  dir.create(file.path("logs", "inputs"))
  dir.create(file.path("logs", "outputs"))
  
  my_logfile = file.path('logs', paste(farmId,'__',str_replace_all(Sys.time(), c(" "="__", ":"="_")),'.log',sep=""))
  my_console_appender = console_appender(layout = default_log_layout())
  my_file_appender = file_appender(my_logfile, append = TRUE, 
                                   layout = default_log_layout())
  my_logger <- log4r::logger(threshold = "INFO", 
                             appenders= list(my_console_appender,my_file_appender))
  log4r::info(my_logger, paste("farmId = ",farmId,sep=""))
  
  ## Checking model settings -------------------------------------------------------
  
  if(settings$debug_mode & settings$save2mongoDB) {stop("Need to set debug_mode to FALSE when setting save2mongoDB to TRUE.")}
  
  ## Fetching Data -----------------------------------------------------------
  
  # Set environmental variables for AWS 
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = init_file$AWS_ACCESS_KEY_ID,
    "AWS_SECRET_ACCESS_KEY" = init_file$AWS_SECRET_ACCESS_KEY,
    "AWS_DEFAULT_REGION" = init_file$AWS_DEFAULT_REGION
  )
  
  init_file=fromJSON("../sensitive-data/init_file.json")
  source(file.path("soil","run_soil_model.R"), local = TRUE)
  source(file.path("emissions_leakage", "call_lca.R"), local = TRUE)
  source(file.path("test_functions.R"), local = TRUE)
  
  ## Get the farm data from the JSON file or MongoDB ---------------------------
  
  # Check that only one source of farm data was provided
  if(!is.na(farmId) & !is.na(JSONfile)){
    stop("Both farmId AND JSON files were passed to the model. Please choose only one.")
  }
  
  if(is.na(farmId) & is.na(JSONfile)){
    stop("Both farmId AND JSON files are missing. One must be passed.")
  }
  
  if(!is.na(JSONfile)){
    JSONfile_entered <- TRUE
    farms_everything <- fromJSON(JSONfile)
    farmId <- farms_everything$farmInfo$farmId
  }
  
  if(!is.na(farmId)) {
    if(settings$server == "prod") {
      connection_string = init_file$connection_string_prod
      db <- "carbonplus_production_db"
    } else if(settings$server == "dev") {
      connection_string = init_file$connection_string_cfdev
      db <- "carbonplusdb"
    } else if(settings$server == "test") {
      connection_string = init_file$connection_string_test
      db <- "test_server_db"
    } else {stop("Wrong value for variable: server")}
    farms_collection = mongo(collection="farms", db=db, url=connection_string) # farms_collection = mongo(collection="farms_backups", db=db, url=connection_string) 
    farms_everything = farms_collection$find(paste('{"farmInfo.farmId":"',farmId,'"}',sep=""))
  }
  
  
  # Checking correctness and unicity of farmIds
  if (is.null(farms_everything$farmInfo)){ # Can this be TRUE? Because already used above to select the data. Move to above?
    log4r::error(my_logger, "farmId wasn't found.")
  } else if (length(farms_everything$farmInfo$farmId)>1){
    log4r::error(my_logger, 
                 paste("Multiple identical farmIds were found. Number of farmIds matching =",
                       length(farms_everything$farmInfo$farmId),".", sep="")
    )
  } else if (farms_everything$farmInfo$farmId==farmId){
    log4r::info(my_logger, paste("farm with farmId = ",farmId," has been read succesfully. 
                                  \nMail adress = ",farms_everything$farmInfo$email,'.', sep=""))
  }
  
  # Selecting only the first case if more than one farmId match in mongoDB
  if (length(farms_everything$farmInfo$farmId)>1){
    farms_everything = farms_collection$find(paste('{"farmInfo.farmId":"',farmId,'"}',sep=""),
                                             limit = 1)
    log4r::info(my_logger,paste("After multiple matches, only the first profile with farmId = ",
                                farmId," was selected.",sep=""))
  }
  
  
  ## Fetching pedo-climatic zone -----------------------------------------------
  
  farm_parameters <- mongo(collection="farmparameters", 
                           db="carbonplus_production_db", 
                           url=init_file$connection_string_prod
  )
  farm_EnZ <-  farm_parameters$find(paste('{"farmId":"',farmId,'"}',sep=""))
  if (length(unique(farm_EnZ$enz))==1){
    farm_EnZ = unique(farm_EnZ$enz)
    log4r::info(
      my_logger, 
      paste("farmparameters collection contain unique info on EnZ for farmId", farmId, sep=" ")
    )
  } else if (length(unique(farm_EnZ$enz))==0){
    log4r::error(
      my_logger, 
      paste("Caution: farmparameters collection doesn't contain info on EnZ for farmId", 
            farmId, 
            sep=" ")
    )
  } else if (length(unique(farm_EnZ$enz))>1){
    log4r::error(
      my_logger, 
      paste("Caution: farmparameters collection content SEVERAL EnZ for farmId",
            farmId,"leading to conflicts",
            sep=" ")
    )
  }
  
  
  ## Processing Inputs ------------------------------------------------------------
  
  source("mongodb_extraction_functions.R", local = TRUE)
  
  ## Extracting livestock, landUseSummaryOrPractices
  livestock <- farms_everything$liveStock
  landUseSummaryOrPractices <- farms_everything$landUse$landUseSummaryOrPractices
  
  ## If set, copy data from a specific year to following years (disabled for monitoring / credit issuance runs!)
  
  if (settings$copy_year_currmonit_to_future){
    for(i in c(settings$curr_monit_year+1:10)){
      landUseSummaryOrPractices[[1]][[paste0("year", i)]] <- 
        landUseSummaryOrPractices[[1]][[paste0("year", settings$curr_monit_year)]]
      livestock[["futureManagement"]][[1]][[paste0("year",i)]] <-
        livestock[["futureManagement"]][[1]][[paste0("year", settings$curr_monit_year)]]
    }
    log4r::info(my_logger, paste("MODIF: EVERY PARCELS: Data from year", settings$curr_monit_year,
                                 "was pasted to every following years"))
  }
  
  ## Reading in calculation factors from csv files
  animal_factors <- read_csv(file.path("data", "carbon_share_manure.csv"), show_col_types = FALSE) %>%
    filter(type=="manure") %>% mutate(species = source)
  crop_factors <- read_csv(file.path("data", "crop_factors.csv"), show_col_types = FALSE)
  grazing_factors <- read_csv(file.path("data", "grazing_factors.csv"), show_col_types = FALSE)
  manure_factors <- read_csv(file.path("data", "carbon_share_manure.csv"), show_col_types = FALSE)
  natural_area_factors <- read_csv(file.path( "data", "natural_area_factors.csv"), show_col_types = FALSE) %>% 
    filter(pedo_climatic_area==farm_EnZ)
  pasture_factors <- read_csv(file.path("data", "pasture_factors.csv"), show_col_types = FALSE)
  tilling_factors <- read_csv(file.path("data", "tilling_factors.csv"), show_col_types = FALSE)
  soil_cover_factors <- read_csv(file.path("data", "soil_cover_factors.csv"), show_col_types = FALSE)
  agroforestry_factors <- read_csv(file.path("data", "agroforestry_factors.csv"), show_col_types = FALSE) 
  co2eq_factors <- read_csv(file.path("data", "co2eq_factors.csv"), show_col_types = FALSE)
  fertilizer_factors <- read_csv(file.path("data", "fertilizer_factors.csv"), show_col_types = FALSE)
  fuel_factors <- read_csv(file.path("data", "fuel_factors.csv"), show_col_types = FALSE)
  tree_factors <- read_csv(file.path("data", "agroforestry_factors.csv"), show_col_types = FALSE)
  methane_factors <- read_csv(file.path("data", "methane_emission_factors.csv"), show_col_types = FALSE) %>%
    filter(climate == natural_area_factors$climate_zone) %>% select(-climate)
  n2o_emission_factors <- read_csv(file.path("data", "n2o_emission_factors.csv"), show_col_types = FALSE)
  
  print("Finished reading factors.")
  
  factors <- list(
    animal_factors = animal_factors,
    crop_factors = crop_factors,
    grazing_factors = grazing_factors,
    manure_factors = manure_factors,
    natural_area_factors = natural_area_factors,
    pasture_factors = pasture_factors,
    tilling_factors = tilling_factors,
    soil_cover_factors = soil_cover_factors,
    agroforestry_factors = agroforestry_factors,
    co2eq_factors = co2eq_factors,
    fertilizer_factors = fertilizer_factors,
    fuel_factors = fuel_factors,
    tree_factors = tree_factors,
    methane_factors = methane_factors,
    n2o_emission_factors = n2o_emission_factors
  )
  
  # Extraction of inputs per parcel and scenario
  parcel_inputs <- get_parcel_inputs(landUseSummaryOrPractices)  # Parcel information
  landUseType <- get_land_use_type(landUseSummaryOrPractices, parcel_inputs)
  livestock_inputs <- get_livestock_table(livestock, animal_factors)
  grazing_tables <- get_grazing_amounts(landUseSummaryOrPractices, livestock, animal_factors, parcel_inputs, livestock_inputs, settings$grazing_used)  # grazing data
  grazing_monthly <- grazing_tables[[1]]
  grazing_yearly <- grazing_tables[[2]]
  orgamendments_inputs <- get_orgamendments_inputs(landUseSummaryOrPractices)  # Organic amendments: hay, compost, manure
  animal_inputs <- get_animal_inputs(grazing_yearly, livestock_inputs, parcel_inputs)  # Animal manure
  crop_inputs <- get_crop_inputs(landUseSummaryOrPractices, parcel_inputs, crop_factors, grazing_yearly, grazing_monthly)  # Crops and residues
  pasture_inputs <- get_pasture_inputs(landUseSummaryOrPractices, grazing_factors, pasture_factors, farm_EnZ, grazing_yearly, grazing_monthly, my_logger, parcel_inputs)
  fertilizer_inputs <- get_fertilizer_inputs(landUseSummaryOrPractices)
  fuel_inputs <- get_fuel_inputs(farms_everything$energyUsage)
  tree_inputs <- get_tree_inputs(landUseSummaryOrPractices)
  bare_field_inputs <- get_bareground_inputs(landUseSummaryOrPractices, soil_cover_factors, farm_EnZ, settings$bare_bl_type)
  tilling_inputs <- get_tilling_inputs(landUseSummaryOrPractices, tilling_factors, farm_EnZ)
  
  # Deactivated. If active should lead to errors and not warnings.
  # # Check input data for validity
  # check_animal_data(animal_inputs, animal_factors)
  # check_crop_data(crop_inputs, crop_factors)
  # check_fertilizer_data(fertilizer_inputs, fertilizer_factors)
  # check_fuel_data(fuel_inputs, fuel_factors)
  # check_manure_data(orgamendments_inputs, manure_factors)  
  
  # Collect inputs as list
  inputs <- list(
    parcel_inputs = parcel_inputs,
    # landUseType = landUseType, # Not required and needs fixing as output info
    livestock_inputs = livestock_inputs,
    grazing_monthly = grazing_monthly,
    grazing_yearly = grazing_yearly,
    orgamendments_inputs = orgamendments_inputs, 
    animal_inputs = animal_inputs, 
    crop_inputs = crop_inputs,
    pasture_inputs = pasture_inputs,
    fertilizer_inputs = fertilizer_inputs,
    fuel_inputs = fuel_inputs,
    tree_inputs = tree_inputs,
    bare_field_inputs = bare_field_inputs,
    tilling_inputs = tilling_inputs
  )
  
  print("Finished extracting inputs.")
  
  ## Running the soil model and emissions calculations -------------------------
  
  lca_out <- call_lca(init_file=init_file,
                      farm_EnZ = farm_EnZ,
                      inputs = inputs,
                      factors = factors)
  
  emissions_yearly_total <- lca_out[['emissions_yearly_total']]
  emissions_yearly_sources <- lca_out[['emissions_yearly_sources']]
  emissions_parcels_yearly_animals <- lca_out[['emissions_parcels_yearly_animals']]
  productivity_table <- lca_out[['productivity_table']]
  
  soil_results_out <- run_soil_model(init_file=init_file,
                                     farms_everything=farms_everything,
                                     farm_EnZ=farm_EnZ,
                                     inputs=inputs,
                                     factors=factors,
                                     settings=settings
  )
  
  soil_results_yearly <- soil_results_out$step_in_table_final
  soil_results_monthly <- soil_results_out$farm_results_final
  
  yearly_results <- soil_results_yearly %>%
    mutate(CO2eq_soil_final=yearly_CO2diff_final,
           CO2eq_soil_mean=yearly_CO2diff_mean,
           CO2eq_soil_sd=yearly_CO2diff_sd) %>% 
    select(cal_year, scenario, CO2eq_soil_final, CO2eq_soil_mean, CO2eq_soil_sd)
  yearly_results$CO2eq_emissions <- emissions_yearly_total$emissions_diff_tCO2_eq[2:11]
  yearly_results$CO2eq_leakage <- emissions_yearly_total$leakage_tCO2_eq[2:11]
  
  yearly_results <- yearly_results %>%
    mutate(CO2eq_total = CO2eq_soil_final - CO2eq_emissions - CO2eq_leakage)
  
  readLines(my_logfile)
  
  
  ## Push results to mongoDB ---------------------------------------------------
  
  if(settings$save2mongoDB) {
    # Get code version and time info
    tag <- system2(command = "git", args = "describe", stdout = TRUE)
    full_tag <- paste0("R-model-version: ", tag)
    currentTime <- format(Sys.time(), "%Y-%m-%d %H:%M")
    currentYear <- format(Sys.time(), "%Y")
    
    farms_everything$modelInfo <- data.frame(
      modelVersion=full_tag,
      resultsGenerationYear=currentYear,
      resultsGenerationTime=currentTime
    )
    
    farms_everything$modelResults <- data.frame(
      yearlyCO2eqTotal=NA,
      yearlyCO2eqSoil=NA,
      yearlyCO2eqEmissions=NA,
      yearlyCO2eqLeakage=NA,
      yearlyCO2eqEmissions_detailed=NA,
      yearlyProductivity=NA
    )
    
    farms_everything$modelResults$yearlyCO2eqTotal=list(c(yearly_results$CO2eq_total))
    farms_everything$modelResults$yearlyCO2eqSoil=list(c(yearly_results$CO2eq_soil_final))
    farms_everything$modelResults$yearlyCO2eqEmissions=list(c(yearly_results$CO2eq_emissions))
    farms_everything$modelResults$yearlyCO2eqLeakage=list(c(yearly_results$CO2eq_leakage))
    farms_everything$modelResults$yearlyCO2eqEmissions_detailed=list(c(emissions_yearly_sources))
    farms_everything$modelResults$yearlyProductivity=list(c(productivity))
    
    farms_everything$modelSettings <- data.frame(settings) 
    
    # Upload to database
    carbonresults_collection = mongo(collection="carbonresults", db=db, url=connection_string)
    carbonresults_collection$insert(farms_everything)
    
  }
  
  ## Log Messages --------------------------------------------------------------
  
  log4r::info(my_logger,'Total soil CO2eq: ', 
              sum(yearly_results$CO2eq_soil_final),
              '.\nCredits per year (before emission reductions): ', 
              list(yearly_results$CO2eq_soil_final),
              '.\nArea considered: ', round(sum(parcel_inputs$area), 2), ' ha.', 
              "\nNumber of runs: ", settings$n_runs,
              ".\nGrazing estimations by CF (Y/N): ", settings$use_calculated_grazing,
              "\nStandard deviation used for extrinsic uncertainty of practices (Cinputs): ",
              settings$se_field_carbon_in,
              if(settings$copy_year_currmonit_to_future) {
                paste0("\nDuplicated and applied land use from year", settings$curr_monit_year," to following years in all parcels.")
              }
  )
  
  
  ## Write data to files -----------------------------------------------------
  
  file_prefix <- paste0(farmId, "_", farms_everything$farmInfo$farmManagerLastName, '_') # farms_everything$farmInfo$farmManagerFirstName
  
  # Prepare some tables
  soil_results_out$all_results_final <- soil_results_out$all_results_final %>%
    rename(scen = scenario)
  
  # Function to mark monitored years and write out data
  write_out <- function(data, path) {
    for(name in names(data)) {
      out <- data[[name]]
      out$monitored <- FALSE
      if("scenario" %in% colnames(out)) {
        f <- c('baseline', paste0("year", 0:settings$curr_monit_year))
        out$monitored[out$scenario %in% f] <- TRUE
      }
      if("year" %in% colnames(out)) {
        f <- 0:settings$curr_monit_year
        out$monitored[out$year %in% f] <- TRUE
      }
      if("time" %in% colnames(out)) {
        f <- ymd(paste0(settings$proj_start_year+(settings$curr_monit_year-1), "-12-31"))
        out$monitored[out$time <= f] <- TRUE
      }
      # out <- out[out$monitored,] # option to remove non-monitored years
      write_csv(out, file.path(path, paste0(file_prefix, name, ".csv")))
    }
  }
  
  inputs <- append(inputs, c(
    model_settings = list(data.frame(settings)),
    soil_inputs = list(soil_results_out$soil_inputs),
    climate_inputs = list(soil_results_out$climate_inputs))
  )

  # Inputs
  write_out(inputs, file.path("logs", "inputs"))
  
  # Outputs
  outputs <- list(
    parcel_Cinputs = soil_results_out$parcel_Cinputs,
    SOC_baseline_and_project_totals = soil_results_out$step_in_table_final,
    SOC_baseline_and_project_parcels = soil_results_out$all_results_final,
    yearly_results = yearly_results,
    productivity_table = productivity_table,
    emissions_yearly_sources = emissions_yearly_sources,
    emissions_parcels_yearly_animals = emissions_parcels_yearly_animals
  )
  write_out(outputs, file.path("logs", "outputs"))
  
  
  ## Plotting ------------------------------------------------------------------
  
  name<-paste0("Results_farm_", farmId)
  
  png(filename = file.path('logs', paste0(file_prefix, 'timeseries', '.png')))
  graph <- ggplot(data = soil_results_monthly, aes(x = time, y = SOC_farm_mean, colour=scenario)) +
    geom_line()+
    #geom_errorbar(aes(ymin=SOC_farm_mean-SOC_farm_sd, ymax=SOC_farm_mean+SOC_farm_sd), width=.1) +
    scale_color_manual(values = c("darkred","#5CB85C"),labels = c("Modern-day","Holistic"))+
    theme(legend.position = "bottom")+
    labs(title = name)+
    xlab("Time")+
    ylab("SOC (in tonnes per hectare)")
  print(graph)
  dev.off()
  
  png(filename = file.path('logs', paste0(file_prefix, 'barplot', '.png')))
  histogram <- ggplot(yearly_results, aes(x=cal_year, group = 1)) +
    geom_bar(aes(y=CO2eq_soil_mean), stat="identity", fill="#5CB85C", alpha=0.7) +
    geom_errorbar(aes(ymin = CO2eq_soil_mean-1.96*CO2eq_soil_sd,
                      ymax = CO2eq_soil_mean+1.96*CO2eq_soil_sd, color = "95% CI"), colour="black", width=.5, show.legend = T) +
    geom_bar(aes(y=CO2eq_emissions), stat="identity", fill="#0C785C", alpha=0.7) +
    geom_bar(aes(y=CO2eq_total), stat="identity", fill="darkred", alpha=0.7) +
    xlab("Time")+
    ylab("Number of certificates issuable (per year)")
  print(histogram)
  dev.off()
  
  ## Rename output dir ---------------------------------------------------------
  # newsystime <- format(Sys.time(),"%Y-%m-%d-%H-%M-%S")
  out_dir <- paste0("outputs_", farmId)
  unlink(out_dir, recursive = TRUE)
  file.rename("logs", out_dir)
  
  ## End function --------------------------------------------------------------
  return(yearly_results)
  
  
}
