carbonplus_main <- function(init_file, settings, db_farmId=NA, JSONfile=NA){
  
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
  p_load('pacman', 'SoilR', 'mongolite', 'tidyverse',
         'soilassessment', 'aws.s3', 'log4r', 'jsonlite',
         'httr', 'logger', 'ncdf4', 'ncdf4.helpers')
  
  
  ## Prepare log files ---------------------------------------------------------
  # clear and prepare log directory
  if(!dir.exists('logs')) {dir.create('logs')}
  unlink(file.path("logs", "*"), recursive = TRUE)
  dir.create(file.path("logs", "inputs"))
  dir.create(file.path("logs", "outputs"))
  
  my_logfile = file.path('logs', paste('out', str_replace_all(Sys.time(), c(" "="__", ":"="_")),'.log',sep=""))
  my_console_appender = console_appender(layout = default_log_layout())
  my_file_appender = file_appender(my_logfile, append = TRUE, 
                                   layout = default_log_layout())
  my_logger <- log4r::logger(threshold = "INFO", 
                             appenders= list(my_console_appender,my_file_appender))
  
  ## Checking model settings -------------------------------------------------------
  
  if(settings$debug_mode & settings$save2mongoDB) {stop("Must set debug_mode to FALSE when setting save2mongoDB to TRUE.")}
  
  ## Fetching Data -----------------------------------------------------------
  
  init_data <- fromJSON(init_file)
  
  # Set environmental variables for AWS 
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = init_data$AWS_ACCESS_KEY_ID,
    "AWS_SECRET_ACCESS_KEY" = init_data$AWS_SECRET_ACCESS_KEY,
    "AWS_DEFAULT_REGION" = init_data$AWS_DEFAULT_REGION
  )
  
  source(file.path("soil","run_soil_model.R"), local = TRUE)
  source(file.path("emissions_leakage", "call_lca.R"), local = TRUE)
  # source(file.path("test_functions.R"), local = TRUE) # Delete?
  
  ## Get the farm data from the JSON file or MongoDB ---------------------------

  # Check that only one source of farm data was provided
  if(!is.na(db_farmId) & !is.na(JSONfile)){
    stop("Both farmId AND JSON files were passed to the model. Please choose only one.")
  }

  if(is.na(db_farmId) & is.na(JSONfile)){
    stop("Both farmId AND JSON files are missing. One must be passed.")
  }

  if(!is.na(JSONfile)){
    monitoringData <- fromJSON(JSONfile)
  } else {
    if(settings$server == "prod") {
      connection_string = init_data$connection_string_prod
      db <- "carbonplus_production_db"
    } else if(settings$server == "dev") {
      connection_string = init_data$connection_string_cfdev
      db <- "carbonplusdb"
    } else if(settings$server == "test") {
      connection_string = init_data$connection_string_test
      db <- "test_server_db"
    } else {stop("Wrong value for variable: server")}
    farm_collection = mongo(collection="farms", db=db, url=connection_string) # farm_collection = mongo(collection="farms_backups", db=db, url=connection_string)
    monitoringData = farm_collection$find(paste0('{"farmId":"',db_farmId,'"}'))
  }
  
  farmId <- monitoringData$farmInfo$farmId
  log4r::info(my_logger, paste("farmId = ",farmId,sep=""))
  
  # Check if data has been loaded and that there is only one instance with the corresponding farm Id:
  # TO BE COMPLETED!
  # if (...?){
  #   log4r::error(my_logger, paste0("Multiple instances with farmID =, ", famId, " were found on server ", settings$server))
  # }
  
  ## Fetching pedo-climatic zone -----------------------------------------------
  
  # farm_pars_collection <- mongo(collection="farmparameters", db=db, url=connection_string)
  # farm_parameters <-  farm_pars_collection$find(paste0('{"farmId":"',farmId,'"}'))
  # 
  # # Check if missing. Delete?
  # if (length(unique(farm_parameters$enz))==0){
  #   log4r::error(my_logger, paste0("ERROR: farmparameters collection doesn't contain info on EnZ for farmId: ", farmId)
  #   )
  # }
  
  farm_EnZ <- "Mediterranean north" # For dev purposes only.
  
  ## Processing Inputs ------------------------------------------------------------
  
  source("mongodb_extraction_functions.R", local = TRUE)
  
  # To delete or modify? Possibility to create projections by copying data to future years.
  ## If set, copy data from a specific year to following years (disabled for monitoring / credit issuance runs!)
  # if (settings$copy_year_currmonit_to_future){
  #   DO SOMETHING HERE
  #   log4r::info(my_logger, SOME MESSAGE)
  # }

  ## Reading in calculation factors from csv files
  factors_animals <- read_csv(file.path("data", "factors_animals.csv"), show_col_types = FALSE)
  factors_crops <- read_csv(file.path("data", "factors_crops.csv"), show_col_types = FALSE)
  factors_natural_area <- read_csv(file.path( "data", "factors_natural_area.csv"), show_col_types = FALSE) %>% 
    filter(pedo_climatic_area==farm_EnZ)
  factors_pastures <- read_csv(file.path("data", "factors_pastures.csv"), show_col_types = FALSE)
  factors_tillage <- read_csv(file.path("data", "factors_tillage.csv"), show_col_types = FALSE)
  factors_co2eq <- read_csv(file.path("data", "factors_co2eq.csv"), show_col_types = FALSE)
  factors_fertilizer <- read_csv(file.path("data", "factors_fertilizer.csv"), show_col_types = FALSE)
  factors_fuel <- read_csv(file.path("data", "factors_fuel.csv"), show_col_types = FALSE)
  factors_trees <- read_csv(file.path("data", "factors_trees.csv"), show_col_types = FALSE)
  factors_methane <- read_csv(file.path("data", "factors_methane.csv"), show_col_types = FALSE) %>%
    filter(climate == factors_natural_area$climate_zone) %>% select(-climate)
  factors_n2o_emission <- read_csv(file.path("data", "factors_n2o_emission.csv"), show_col_types = FALSE)
  factors_others <- read_csv(file.path("data", "factors_others.csv"), show_col_types = FALSE)
  
  print("Finished reading factors.")

  factors <- list(
    factors_animals = factors_animals,
    factors_crops = factors_crops,
    factors_natural_area = factors_natural_area,
    factors_pastures = factors_pastures,
    factors_tillage = factors_tillage,
    factors_co2eq = factors_co2eq,
    factors_fertilizer = factors_fertilizer,
    factors_fuel = factors_fuel,
    factors_trees = factors_trees,
    factors_methane = factors_methane,
    factors_n2o_emission = factors_n2o_emission,
    factors_others = factors_others
  )

  ## Fixed farm and parcel inputs
  fixed_farm_inputs <- get_fixed_farm_inputs(monitoringData)
  fixed_parcel_inputs <- get_fixed_parcel_inputs(monitoringData)
  periods <- get_periods(monitoringData, fixed_farm_inputs$project_start_year) # defines project and baseline years based on the given project start year
  
  ## Yearly farm inputs
  fuel_inputs_direct <- get_fuel_inputs_direct(monitoringData, periods) 
  fuel_inputs_indirect <- get_fuel_inputs_indirect(monitoringData, periods)
  fertilizer_inputs <- get_fertilizer_inputs(monitoringData, periods)
  
  in_farm_livestock_table <- get_in_farm_livestock_table(monitoringData, periods, factors_animals)
  out_farm_livestock_table <- get_out_farm_livestock_table(monitoringData, periods, factors_animals)
  
  ## Yearly parcel inputs
  orgamendments_inputs <- get_orgamendments_inputs(monitoringData, periods)
  grazing_table <- get_grazing_table(monitoringData, periods, fixed_parcel_inputs, in_farm_livestock_table, out_farm_livestock_table, orgamendments_inputs)  # grazing data
  grazing_monthly <- grazing_table[[1]]
  grazing_yearly <- grazing_table[[2]] 
  animal_inputs <- get_animal_inputs(grazing_yearly, in_farm_livestock_table, out_farm_livestock_table, periods)
  
  
  ## TO-DO
  #landUseType <- get_land_use_type(landUseSummaryOrPractices, parcel_inputs)
  annual_crop_inputs <- get_annual_crop_inputs(monitoringData, parcel_inputs, factors_crops, grazing_yearly, grazing_monthly, periods)  # Crops and residues
  pasture_inputs <- get_pasture_inputs(landUseSummaryOrPractices, factors_pastures, farm_EnZ, grazing_yearly, grazing_monthly, my_logger, parcel_inputs)
  tree_inputs <- get_tree_inputs(landUseSummaryOrPractices) #TO-DO
  perennial_crop_inputs <- get_perennial_crop_inputs(monitoringData, fixed_parcel_inputs, periods)
  
  bare_field_inputs <- get_bareground_inputs(landUseSummaryOrPractices, soil_cover_factors, farm_EnZ, settings$bare_bl_type)
  tilling_inputs <- get_tilling_inputs(landUseSummaryOrPractices, factors_tillage, farm_EnZ)
  
  # Deactivated. If active should lead to errors and not warnings.
  # # Check input data for validity
  # check_animal_data(animal_inputs, factors_animals)
  # check_crop_data(crop_inputs, factors_crops)
  # check_fertilizer_data(fertilizer_inputs, factors_fertilizer)
  # check_fuel_data(fuel_inputs, factors_fuel)
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
  
  lca_out <- call_lca(init_data=init_data,
                      farm_EnZ = farm_EnZ,
                      inputs = inputs,
                      factors = factors)
  
  emissions_yearly_total <- lca_out[['emissions_yearly_total']]
  emissions_yearly_sources <- lca_out[['emissions_yearly_sources']]
  emissions_parcels_yearly_animals <- lca_out[['emissions_parcels_yearly_animals']]
  productivity_table <- lca_out[['productivity_table']]
  
  soil_results_out <- run_soil_model(init_data=init_data,
                                     farms_everything=farms_everything,
                                     farm_EnZ=farm_EnZ,
                                     inputs=inputs,
                                     factors=factors,
                                     settings=settings
  )

  soil_results_yearly <- soil_results_out$soc_farm
  soil_results_monthly <- soil_results_out$soc_monthly
  
  yearly_results <- soil_results_yearly %>%
    mutate(CO2eq_soil_gain_95conf = CO2_gain_95conf,
           CO2eq_soil_cum = CO2_cum,
           CO2eq_soil_gain_mean = CO2_gain_mean,
           CO2eq_soil_gain_sd = CO2_gain_sd) %>% 
    select(year, cal_year, CO2eq_soil_gain_95conf, CO2eq_soil_cum, CO2eq_soil_gain_mean, CO2eq_soil_gain_sd)
  yearly_results$CO2eq_emissions <- emissions_yearly_total$emissions_diff_tCO2_eq
  yearly_results$CO2eq_leakage <- emissions_yearly_total$leakage_tCO2_eq
  
  yearly_results <- yearly_results %>%
    mutate(CO2eq_total = CO2eq_soil_gain_95conf - CO2eq_emissions - CO2eq_leakage)
  
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
    farms_everything$modelResults$yearlyCO2eqSoil=list(c(yearly_results$CO2eq_soil_gain_95conf))
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
              sum(yearly_results$CO2eq_soil_gain_95conf),
              '.\nCredits per year (before emission reductions): ', 
              list(yearly_results$CO2eq_soil_gain_95conf[-1]),
              '.\nArea considered: ', round(sum(parcel_inputs$area), 2), ' ha.', 
              "\nNumber of runs: ", settings$n_runs,
              "\nStandard error used for extrinsic uncertainty of practices (Cinputs): ",
              settings$se_field_carbon_in,
              if(settings$copy_year_currmonit_to_future) {
                paste0("\nCopied farm data from year", settings$curr_monit_year," to following years in all parcels.")
              }
  )
  
  
  ## Write data to files -----------------------------------------------------
  
  file_prefix <- paste0(farmId, "_", farms_everything$farmInfo$farmManagerLastName, '_') # farms_everything$farmInfo$farmManagerFirstName
  
  # Function to mark monitored years and write out data
  write_out <- function(data, path) {
    for(name in names(data)) {
      out <- data[[name]]
      if(nrow(out) == 0) next
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
    SOC_baseline_and_project_totals = soil_results_out$soc_farm,
    SOC_baseline_and_project_parcels = soil_results_out$soc_parcels,
    yearly_results = yearly_results,
    productivity_table = productivity_table,
    emissions_yearly_sources = emissions_yearly_sources,
    emissions_parcels_yearly_animals = emissions_parcels_yearly_animals
  )
  write_out(outputs, file.path("logs", "outputs"))
  
  
  ## Plotting ------------------------------------------------------------------
  
  name<-paste0("Results_farm_", farmId)

  soil_results_monthly <- soil_results_monthly %>% 
    mutate(time = paste0(cal_year,'-',month), time = ym(time))
  png(filename = file.path('logs', paste0(file_prefix, 'timeseries', '.png')))
  graph <- ggplot(data = soil_results_monthly, aes(x = time, y = SOC_ha, colour=scenario)) +
    geom_line()+
    #geom_errorbar(aes(ymin=SOC_farm_mean-SOC_farm_sd, ymax=SOC_farm_mean+SOC_farm_sd), width=.1) +
    scale_color_manual(values = c("darkred","#5CB85C"),labels = c("baseline","project"))+
    theme(legend.position = "bottom")+
    labs(title = name)+
    xlab("Time")+
    ylab("SOC (in tonnes per hectare)")
  print(graph)
  dev.off()
  
  png(filename = file.path('logs', paste0(file_prefix, 'barplot1', '.png'))) 
  barplot1 <- ggplot(yearly_results, aes(x=cal_year, group = 1)) +
    geom_bar(aes(y=CO2eq_soil_gain_mean), stat="identity", fill="brown", alpha=0.7) +
    geom_errorbar(aes(ymin = CO2eq_soil_gain_mean-1.96*CO2eq_soil_gain_sd,
                      ymax = CO2eq_soil_gain_mean+1.96*CO2eq_soil_gain_sd, color = "95% CI"), colour="black", width=.5, show.legend = T) +
    geom_bar(aes(y=CO2eq_total), stat="identity", fill="green", alpha=0.7) +
    geom_bar(aes(y=CO2eq_leakage), stat="identity", fill="red", alpha=0.7) +
    geom_bar(aes(y=CO2eq_emissions), stat="identity", fill="blue", alpha=0.7) +
    xlab("Time")+
    ylab("Number of credits issuable (per year)")
  print(barplot1)
  dev.off()
  
  png(filename = file.path('logs', paste0(file_prefix, 'barplot2', '.png'))) 
  barplot_data <- yearly_results %>% mutate(CO2eq_emission_red = -CO2eq_emissions) %>%
    select(-c(CO2eq_soil_cum, CO2eq_soil_gain_mean, CO2eq_soil_gain_sd, CO2eq_emissions)) %>%
    pivot_longer(!c(year, cal_year))
  barplot2 <- ggplot() + geom_bar(data = barplot_data, aes(x = cal_year, y = value, fill = name), position = "dodge", stat = "identity")
  print(barplot2)
  dev.off()
  
  ## Rename output dir ---------------------------------------------------------
  # newsystime <- format(Sys.time(),"%Y-%m-%d-%H-%M-%S")
  out_dir <- paste0("outputs_", farmId)
  unlink(out_dir, recursive = TRUE)
  file.rename("logs", out_dir)
  
  ## End function --------------------------------------------------------------
  return(yearly_results)
  
}

