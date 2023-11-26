run_soil_model <- function(init_file, farms_everything, farm_EnZ, inputs, factors, settings){ 
  
  ## Log starting run message
  log4r::info(my_logger, "run_soil_model.R started running")
  
  ## Sourcing code from files
  source(file.path("soil", "model_semiArid_functions.R"), local = TRUE)
  source(file.path("soil", "modified_semiArid_functions.R"), local = TRUE)
  source(file.path("soil", "calc_functions_soil_modelling.R"), local = TRUE)
  source("weather_data_pulling_functions.R", local = TRUE)
  
  landUseSummaryOrPractices <- farms_everything$landUse$landUseSummaryOrPractices
  
  ## Get weather data --- To be moved to ouside function
  # Mean coordinates
  latlon_farm <- c(latitude = mean(inputs$parcel_inputs$latitude), longitude = mean(inputs$parcel_inputs$longitude))
  
  # Just for testing
  # weather_data <- read_csv(file.path("data", "test_weather_data.csv"), show_col_types = FALSE) # debug line
  # 
  # ## Extracting climate from different periods
  # past_weather <- data.frame(
  #   month = weather_data$month,
  #   evap = weather_data$past_evap,
  #   pevap = weather_data$past_pevap,
  #   precipitation = weather_data$past_precipitation,
  #   temperature = weather_data$past_temperature
  # )
  # climate_rcp4.5 <- data.frame(
  #   month = weather_data$month,
  #   evap = weather_data$future_evap_rcp4.5,
  #   pevap = weather_data$future_pevap_rcp4.5,
  #   precipitation = weather_data$future_precipitation_rcp4.5,
  #   temperature = weather_data$future_temperature_rcp4.5
  # )
  
  if(settings$debug_mode | settings$use_test_climate){  # will skip fetching climate data and use dummy data if debug_mode is set
    climate_data <- read_csv(file.path("data", "climate_data_1950_2022_Alves4.csv"), show_col_types = FALSE) # For testing only
  } else {
    climate_data <- get_past_weather_data(init_file, latlon_farm["latitude"], latlon_farm["longitude"], "1950_2022", averaged=FALSE)
    # weather_data <- rbind(get_past_weather_data(init_file, latlon_farm["latitude"], latlon_farm["longitude"], "1950_2022"),
    #                    # get_past_weather_data(init_file, latlon_farm["latitude"], latlon_farm["longitude"], "2021"),
    #                    # get_past_weather_data(init_file, latlon_farm["latitude"], latlon_farm["longitude"], "2022"),
    #                    get_future_weather_data(init_file, latlon_farm["latitude"], latlon_farm["longitude"], scenario="rcp4.5"),
    #                    get_future_weather_data(init_file, latlon_farm["latitude"], latlon_farm["longitude"], scenario="rcp8.5"))
  }
  
  climate_periods <- get_climate_periods(climate_data = climate_data, proj_start_year = as.numeric(farms_everything$farmInfo$startYear))
  mean_past_climate <- climate_periods$mean_past_climate
  mean_recent_climate <- climate_periods$mean_recent_climate
  
  # Set climate for different runs:
  spinup_climate <- climate_periods$mean_past_climate
  present_climate <- climate_periods$mean_past_climate

  ## Choosing model version based on climate
  model_version <- ifelse(sum(present_climate$precipitation) / sum(present_climate$pevap) < 0.65 &
                            sum(present_climate$precipitation) < 600, "Semi-arid", "Normal")

  ## Soil data ---
  # Gets soil data from https://maps.isric.org/ (AWS)
  SOC_df = s3read_using(FUN = read_csv, object = paste("s3://soil-modelling/soil_variables/", farmId, "/ocs.csv", sep=""))
  clay_df = s3read_using(FUN = read_csv, object = paste("s3://soil-modelling/soil_variables/", farmId, "/clay.csv", sep=""))
  silt_df = s3read_using(FUN = read_csv, object = paste("s3://soil-modelling/soil_variables/", farmId, "/silt.csv", sep=""))
  bdod_df = s3read_using(FUN = read_csv, object = paste("s3://soil-modelling/soil_variables/", farmId, "/bdod.csv", sep=""))
  
  # Fill soil maps data frame. Waited for values from soil maps
  
  # soilMapsData = data.frame(SOC=mean(SOC_df$`ocs_0-30cm_mean`), SOC_Q0.05=mean(SOC_df$`ocs_0-30cm_Q0.05`), SOC_Q0.95=mean(SOC_df$`ocs_0-30cm_Q0.95`), 
  #                           clay=mean(clay_df$`clay_5-15cm_mean`)/10, clay_Q0.05=mean(clay_df$`clay_5-15cm_Q0.05`)/10, clay_Q0.95=mean(clay_df$`clay_5-15cm_Q0.95`)/10, 
  #                           silt=mean(silt_df$`silt_5-15cm_mean`)/10, silt_Q0.05=mean(silt_df$`silt_5-15cm_Q0.05`)/10, silt_Q0.95=mean(silt_df$`silt_5-15cm_Q0.95`)/10, 
  #                           bulk_density=mean(bdod_df$`bdod_5-15cm_mean`)/100, bdod_Q0.05=mean(bdod_df$`bdod_5-15cm_Q0.05`)/100, bdod_Q0.95=mean(bdod_df$`bdod_5-15cm_Q0.95`)/100)# waiting for values from soil maps
  
  # Lower clay correlates with higher credits, so we take the 75th percentile of all values in the farm area as a conservative approach.
  soilMapsData = data.frame(
    SOC = quantile(SOC_df$`ocs_0-30cm_mean`, 0.05),
    clay = quantile(clay_df$`clay_5-15cm_mean`/10, 0.75),
    silt = mean(silt_df$`silt_5-15cm_mean`)/10, 
    bulk_density = mean(bdod_df$`bdod_5-15cm_mean`)/100
  )
  
  # Final soil inputs
  soil_inputs <- get_soil_inputs(landUseSummaryOrPractices, farms_everything$soilAnalysis, soilMapsData)
  
  # ## Calculating the average soil parameters among parcels
  # mean_SOC = mean(soil_inputs$SOC)
  # mean_clay = mean(soil_inputs$clay)
  # mean_silt = mean(soil_inputs$silt)
  # mean_bulk_density = mean(soil_inputs$bulk_density)
  
  ################# Calculations of C inputs per parcel and scenario
  
  # Attention: YEARLY C inputs are calculated, naming misleading
  baseline_chosen="baseline"
  parcel_Cinputs <- data.frame(parcel_ID=c(), 
                               scenario=c(),
                               year=c(),
                               orgamendments_Cinputs=c(), 
                               agroforestry_Cinput=c(), 
                               animal_Cinput=c(), 
                               crop_Cinputs=c(), 
                               pasture_Cinputs=c()
  )
  
  # load("parcel_Cinputs.RData")  # for testing only
  
  years <- 0:10
  
  for(p in 1:length(inputs$parcel_inputs$parcel_ID)) {
    
    parcel <- inputs$parcel_inputs$parcel_ID[p]
    
    for(year in years){
      scenario <- paste0("year", year)
      if(year == 0) scenario <- baseline_chosen
      orgamendments_Cinputs <- get_monthly_Cinputs_orgamendments(inputs$orgamendments_inputs, factors$manure_factors, scenario, parcel)
      agroforestry_Cinputs <- 0 # get_monthly_Cinputs_agroforestry(inputs$tree_inputs, factors$agroforestry_factors, scenario, parcel, lat_farmer) # TREES NOT COUNTED BEFORE GOOD CHECK OF DATA QUALITY
      animal_Cinputs <- get_monthly_Cinputs_animals(inputs$animal_inputs, factors$animal_factors, scenario, parcel)
      crop_Cinputs <- get_monthly_Cinputs_crop(inputs$crop_inputs, factors$crop_factors, scenario, parcel, farm_EnZ)
      pasture_Cinputs <- get_monthly_Cinputs_pasture(inputs$pasture_inputs, factors$pasture_factors, scenario, parcel, year, settings)
      
      parcel_Cinputs_temp <- data.frame(parcel_ID = parcel, 
                                        scenario = scenario,
                                        year = year,
                                        orgamendments_Cinputs=orgamendments_Cinputs, 
                                        agroforestry_Cinputs=agroforestry_Cinputs, 
                                        animal_Cinputs=animal_Cinputs, 
                                        crop_Cinputs=crop_Cinputs, 
                                        pasture_Cinputs=pasture_Cinputs
      )
      parcel_Cinputs <- rbind(parcel_Cinputs, parcel_Cinputs_temp)
    }
  }
  
  parcel_Cinputs <- parcel_Cinputs %>% mutate(Cinputs_ha = orgamendments_Cinputs + agroforestry_Cinputs + animal_Cinputs + crop_Cinputs + pasture_Cinputs)
  
  if (length(apply(is.na(parcel_Cinputs), 2, which))==0){
    log4r::info(my_logger, 'parcel C inputs calculations have no NAs.', sep=" ")
  } else {
    log4r::error(my_logger, paste(length(apply(is.na(parcel_Cinputs), 2, which)), 'NAs were found in parcel C inputs calculation results.'))
  }
  
  # ################# Calculations of additional C inputs compared to baseline per parcel and scenario -- Fernando: NOT USED FURTHER IN CODE!
  # 
  # parcel_Cinputs_addition = merge(x= parcel_Cinputs, 
  #                                 y= inputs$parcel_inputs %>% 
  #                                   select(parcel_ID, area) %>%
  #                                   mutate(farm_frac= paste(round(area/sum(area)*100), '%')), by="parcel_ID") %>% 
  #   group_by(parcel_ID, farm_frac) %>% 
  #   mutate(Cinput_per_ha_project = sum(Cinputs_ha[scenario!=baseline_chosen & scenario!="year0"])/10) %>%
  #   filter(scenario==baseline_chosen) %>%
  #   mutate(additional_Cinput_per_ha = round(Cinput_per_ha_project - Cinputs_ha, 2), 
  #          relative_increase=paste(as.character(ifelse(Cinputs_ha==0, NA, as.integer((Cinput_per_ha_project - Cinputs_ha) / Cinputs_ha*100))), '%'), 
  #          additional_Cinput_total = round(unique(area)*(Cinput_per_ha_project - Cinputs_ha), 1)) 
  # additional_Cinput_total_farm = sum(parcel_Cinputs_addition$additional_Cinput_total)
  # parcel_Cinputs_addition = parcel_Cinputs_addition %>%
  #   mutate(absolute_contribution_perc = round(100*additional_Cinput_total / additional_Cinput_total_farm)) %>%
  #   select(parcel_ID, farm_frac, additional_Cinput_per_ha, relative_increase, additional_Cinput_total, absolute_contribution_perc)
  # 
  # ## Calculation of total c inputs for the whole farm -- Fernando: NOT BEING USED FURTHER IN CODE!
  # # Sum over all parcels
  # yearly_Cinputs_farm = merge(x= parcel_Cinputs, 
  #                             y= inputs$parcel_inputs, 
  #                             by="parcel_ID") %>%
  #   group_by(scenario) %>%
  #   summarise(Cinputs_ha=sum(Cinputs_ha*area), 
  #             orgamendments_Cinputs=sum(area*orgamendments_Cinputs), 
  #             animal_Cinputs=sum(area*animal_Cinputs), 
  #             crop_Cinputs=sum(area*crop_Cinputs), 
  #             pasture_Cinputs=sum(area*pasture_Cinputs), 
  #             agroforestry_Cinputs=sum(area*agroforestry_Cinputs))
  # 
  
  
  ################# Initialisation by making the model reach SOC of natural areas of the pedo-climatic area
  
  ## Pulling DMP/RPM ratios from different kind of land use in corresponding pedoclimatic area 
  dr_ratio_agroforestry = unique(factors$natural_area_factors$dr_ratio_agroforestry)
  dr_ratio_non_irrigated = unique(factors$natural_area_factors$dr_ratio_non_irrigated)
  dr_ratio_irrigated = unique(factors$natural_area_factors$dr_ratio_irrigated)
  
  ## Building a mean input dataframe to feed RothC
  # Mean value for each model input parameter
  mean_input <- data.frame(
    run = rep(0, 12),
    dr_ratio = c(dr_ratio_non_irrigated, rep(NA, 11)),
    bare = c(logical(12)),
    soil_thick = c(30, rep(NA, 11)), # modelled for 30 cm depth as recommended in IPCC Guidelines 2006
    SOC = c(soilMapsData$SOC, rep(NA, 11)),
    clay = c(soilMapsData$clay, rep(NA, 11)),
    silt = c(soilMapsData$silt, rep(NA, 11)),
    bulk_density = c(soilMapsData$bulk_density, rep(NA, 11)),
    pE = c(0.75, rep(NA, 11)), # mean potential transpiration to open-pan evap convertion rate
    tilling_factor = c(1.0, rep(NA, 11))
  )
  
  ## Get SOC steady state using mean values ----
  # This section gets soil C close to steady state so spin-ups are not
  # required for every nth run.
  
  SOC_start <- data.frame(
    parcel_ID = inputs$parcel_inputs$parcel_ID,
    DPM = NA, RPM = NA, BIO = NA, HUM = NA, IOM = NA
    )
  
  batch <- mean_input
  
  for(i in 1:nrow(inputs$parcel_inputs)) {

    parcel = inputs$parcel_inputs$parcel_ID[i]
    batch$dr_ratio = ifelse((soil_inputs %>% filter(parcel_ID==parcel))$irrigation==TRUE, dr_ratio_irrigated, dr_ratio_non_irrigated)

    # Select values for the baseline scenario
    batch$field_carbon_in <- (parcel_Cinputs %>% filter (scenario==baseline_chosen & parcel_ID==parcel))$Cinputs_ha
    batch$bare = (inputs$bare_field_inputs %>% filter(scenario == baseline_chosen & parcel_ID == parcel))$bareground
    batch$tilling_factor = (inputs$tilling_inputs %>% filter(scenario==baseline_chosen & parcel_ID==parcel))$tilling_factor

    # Get starting soil content
    starting_soil_content <- estimate_starting_soil_content(SOC = batch$SOC[1], clay = batch$clay[1]) / 2

    # Run spinup to equilibrium using baseline data
    time_horizon = settings$spinup_years

    modelSOC_spinup <- calc_carbon_over_time(time_horizon,
                                          field_carbon_in = rep(batch$field_carbon_in[1], time_horizon),
                                          dr_ratio = rep(batch$dr_ratio[1], time_horizon),
                                          bare = batch$bare,
                                          temp = spinup_climate$temperature,
                                          precip = spinup_climate$precipitation,
                                          evap = spinup_climate$evap,
                                          soil_thick = batch$soil_thick[1],
                                          clay = batch$clay[1],
                                          pE = batch$pE[1],
                                          PS = starting_soil_content,
                                          tilling_factor = batch$tilling_factor[1],
                                          version=model_version,
                                          silt = batch$silt[1],
                                          bulk_density = batch$bulk_density[1])
    SOC_start[SOC_start$parcel_ID == parcel, 2:6] <- as.numeric(tail(modelSOC_spinup, 1))[c(1:5)]
  }
  
  ## Prepare for multiple runs sampling from input probability distributions
  
  # Remove uncertainty for testing
  if(settings$debug_mode) {
    settings$se_inputs_nonfarm <- 0.0
    settings$se_field_carbon_in <- 0.0
  }
  
  se_inputs_nonfarm <- settings$se_inputs_nonfarm
  se_field_carbon_in <- settings$se_field_carbon_in
  
  se=data.frame(field_carbon_in=se_field_carbon_in, 
                dr_ratio = se_inputs_nonfarm, 
                temp = se_inputs_nonfarm, 
                precip = se_inputs_nonfarm, 
                evap = se_inputs_nonfarm, 
                soil_thick = se_inputs_nonfarm, 
                SOC = se_inputs_nonfarm,
                clay = se_inputs_nonfarm,
                silt = se_inputs_nonfarm,
                bulk_density = se_inputs_nonfarm,
                pE = se_inputs_nonfarm, 
                tilling_factor = se_inputs_nonfarm)
  
  # Data frame for holding all results
  all_results <- data.frame()
  
  # Initializing run counter
  run_ID <- 0
  
  ## Start model runs ----
  for (n in c(1:settings$n_runs)){
    
    run_ID <- run_ID + 1
    # Dataframe for all results of one run
    all_results_batch <- data.frame()
    
    # Choice of a random factor to normally randomize input values
    batch_coef <- data.frame(field_carbon_in = rnorm(1, 1, se$field_carbon_in), 
                             dr_ratio = rnorm(1, 1, se$dr_ratio), 
                             temp = rnorm(1, 1, se$temp), 
                             precip = rnorm(1, 1, se$precip), 
                             evap = rnorm(1, 1, se$evap), 
                             soil_thick = rnorm(1, 1, se$soil_thick), 
                             SOC = rnorm(1, 1, se$SOC), 
                             clay = rnorm(1, 1, se$clay), 
                             pE = rnorm(1, 1, se$pE), 
                             tilling_factor = rnorm(1, 1, se$tilling_factor), 
                             silt = rnorm(1, 1, se$silt), 
                             bulk_density = rnorm(1, 1, se$bulk_density))
    
    # Apply factors to inputs average
    batch <- data.frame(run=run_ID, 
                        bare = mean_input$bare, 
                        soil_thick = mean_input$soil_thick * batch_coef$soil_thick, 
                        SOC = mean_input$SOC * batch_coef$SOC, 
                        clay = mean_input$clay * batch_coef$clay, 
                        silt = mean_input$silt * batch_coef$silt, 
                        bulk_density = mean_input$bulk_density * batch_coef$bulk_density, 
                        pE = mean_input$pE * batch_coef$pE, 
                        tilling_factor = mean_input$tilling_factor * batch_coef$tilling_factor
    )
    
    spinup_climate_batch <- spinup_climate %>% mutate(
      year = year, 
      temp = temperature * batch_coef$temp,
      precip = precipitation * batch_coef$precip,
      evap = evap * batch_coef$evap
    )
    
    present_climate_batch <- present_climate %>% mutate(
      year = year, 
      temp = temperature * batch_coef$temp,
      precip = precipitation * batch_coef$precip,
      evap = evap * batch_coef$evap
    )
    
    for(i in 1:nrow(inputs$parcel_inputs)) {
      
      # Define parcel fixed values
      parcel = inputs$parcel_inputs$parcel_ID[i]
      farm_frac = inputs$parcel_inputs$area[i]/sum(inputs$parcel_inputs$area)
      batch_parcel_Cinputs = parcel_Cinputs %>% mutate(Cinputs_ha=Cinputs_ha*batch_coef$field_carbon_in)
      batch$dr_ratio = ifelse((soil_inputs %>% filter(parcel_ID==parcel))$irrigation==TRUE, dr_ratio_irrigated, dr_ratio_non_irrigated) * batch_coef$dr_ratio
      
      # Select values for the baseline scenario
      batch$field_carbon_in <- (batch_parcel_Cinputs %>% filter (scenario==baseline_chosen & parcel_ID==parcel))$Cinputs_ha
      batch$bare = (inputs$bare_field_inputs %>% filter(scenario == baseline_chosen & parcel_ID == parcel))$bareground
      batch$tilling_factor = (inputs$tilling_inputs %>% filter(scenario==baseline_chosen & parcel_ID==parcel))$tilling_factor

      # Get starting soil content
      starting_soil_content <- as.numeric(SOC_start[SOC_start$parcel_ID == parcel, 2:6])

      # Run spinup to equilibrium using baseline data
      time_horizon = 20

      modelSOC_steadystate <- calc_carbon_over_time(time_horizon, 
                                            field_carbon_in = rep(batch$field_carbon_in[1], time_horizon), 
                                            dr_ratio = rep(batch$dr_ratio[1], time_horizon), 
                                            bare = batch$bare,
                                            temp = spinup_climate_batch$temp, 
                                            precip = spinup_climate_batch$precip, 
                                            evap = spinup_climate_batch$evap, 
                                            soil_thick = batch$soil_thick[1], 
                                            clay = batch$clay[1], 
                                            pE = batch$pE[1], 
                                            PS = starting_soil_content, 
                                            tilling_factor = batch$tilling_factor[1], 
                                            version=model_version, 
                                            silt = batch$silt[1], 
                                            bulk_density = batch$bulk_density[1])
      steadystate_soil_content <- as.numeric(tail(modelSOC_steadystate, 1))[c(1:5)]
      
      if(n==1) {
        ## Print the last spinup to check for equilibrium ----
        graph <- ggplot(data = modelSOC_steadystate, aes(x=1:nrow(modelSOC_steadystate), y=TOT)) +
          geom_line() +
          theme(legend.position = "bottom") +
          # labs(title = "Model spinup for an example farm plot") +
          xlab("Months") +
          ylab("SOC (in tonnes per hectare)") +
          ylim(0, 30)
        print(graph)
      }
      
      # Run a single year for historical baseline
      time_horizon = 1
      modelSOC_baseline <- calc_carbon_over_time(time_horizon, 
                                              field_carbon_in = rep(batch$field_carbon_in[1], time_horizon), 
                                              dr_ratio = rep(batch$dr_ratio[1], time_horizon), 
                                              bare = batch$bare,
                                              temp = present_climate_batch$temp, 
                                              precip = present_climate_batch$precip, 
                                              evap = present_climate_batch$evap, 
                                              soil_thick = batch$soil_thick[1], 
                                              clay = batch$clay[1], 
                                              pE = batch$pE[1], 
                                              PS = steadystate_soil_content, 
                                              tilling_factor = batch$tilling_factor[1], 
                                              version=model_version, 
                                              silt = batch$silt[1], 
                                              bulk_density = batch$bulk_density[1])
      baseline_soil_content <- as.numeric(tail(modelSOC_baseline, 1))[c(1:5)]
      
      # Run the projected baseline scenario
      time_horizon = 10
      modelSOC_baseline_proj <- calc_carbon_over_time(time_horizon, 
                                                   field_carbon_in = rep(batch$field_carbon_in[1], time_horizon), 
                                                   dr_ratio = rep(batch$dr_ratio[1], time_horizon), 
                                                   bare = batch$bare,
                                                   temp = present_climate_batch$temp, 
                                                   precip = present_climate_batch$precip, 
                                                   evap = present_climate_batch$evap, 
                                                   soil_thick = batch$soil_thick[1], 
                                                   clay = batch$clay[1], 
                                                   pE = batch$pE[1], 
                                                   PS = baseline_soil_content, 
                                                   tilling_factor = batch$tilling_factor[1], 
                                                   version=model_version, 
                                                   silt = batch$silt[1], 
                                                   bulk_density = batch$bulk_density[1])
      modelSOC_baseline_proj <- rbind(modelSOC_baseline, modelSOC_baseline_proj)
      
      
      # Project scenario run
      starting_project_soil_content <- baseline_soil_content
      modelSOC_project <- modelSOC_baseline
      for (N in 1:10){
        # print(paste(run_ID, parcel, N)) # debugging line
        batch_parcel_Cinputs = parcel_Cinputs %>% mutate(Cinputs_ha=Cinputs_ha*batch_coef$field_carbon_in)
        batch$field_carbon_in <- (batch_parcel_Cinputs %>% filter (scenario==paste("year", N, sep="") & parcel_ID==parcel))$Cinputs_ha
        batch$bare = (inputs$bare_field_inputs %>% filter(scenario == paste("year", N, sep="") & parcel_ID == parcel))$bareground
        batch$tilling_factor = (inputs$tilling_inputs %>% filter(scenario==paste("year", N, sep="") & parcel_ID==parcel))$tilling_factor
        time_horizon = 1
        
        modelSOC_project_yearly <- calc_carbon_over_time(time_horizon, 
                                                       field_carbon_in = rep(batch$field_carbon_in[1], time_horizon), 
                                                       dr_ratio = rep(batch$dr_ratio[1], time_horizon), 
                                                       bare = batch$bare,
                                                       temp = present_climate_batch$temp, 
                                                       precip = present_climate_batch$precip, 
                                                       evap = present_climate_batch$evap, 
                                                       soil_thick = batch$soil_thick[1], 
                                                       clay = batch$clay[1], 
                                                       pE = batch$pE[1], 
                                                       PS = starting_project_soil_content, 
                                                       tilling_factor = batch$tilling_factor[1], 
                                                       version=model_version, 
                                                       silt = batch$silt[1], 
                                                       bulk_density = batch$bulk_density[1])
        starting_project_soil_content <- as.numeric(tail(modelSOC_project_yearly , 1))[c(1:5)]
        modelSOC_project <- rbind(modelSOC_project, modelSOC_project_yearly) 
      }

      all_results_batch_temp <- data.frame(run=run_ID, #, modelSOC_baseline_proj$TOT#, rep("current", 120)
                                           parcel_ID=rep(parcel, 264), 
                                           year=rep(0:10, each=12),
                                           month=rep(seq(1:12), 11),
                                           SOC=c(modelSOC_baseline_proj$TOT, modelSOC_project$TOT), 
                                           scenario=c(rep("baseline", 132), rep("project", 132)), 
                                           farm_frac=rep(farm_frac, 264))
      all_results_batch <- rbind(all_results_batch, all_results_batch_temp) 
      
    }
    
    all_results <- rbind(all_results_batch, all_results)
    print(paste("Run ", n, "over", settings$n_runs, "done"))
    
  } # End of model runs
  
  ## Calculations of CO2 ERR per year ----

  # Make a dataframe with monthly means
  soc_parcels_monthly <- all_results %>% group_by(parcel_ID, year, month, scenario) %>%
    summarise_all(mean) %>% mutate(SOC_rel = SOC * farm_frac)
  soc_monthly <- soc_parcels_monthly %>% group_by(year, month, scenario) %>%
    summarise(SOC_ha = sum(SOC_rel)) %>% 
    mutate(SOC_abs = SOC_ha * sum(inputs$parcel_inputs$area))
  # Add calendar year
  soc_monthly <- soc_monthly %>% mutate(cal_year = settings$proj_start_year + (year-1))
  
  # Select December, calculate absolute SOC values and restructure (remove scenario column)
  soc_allruns_parcels <- all_results[all_results$month == 12, ]
  soc_allruns_parcels <- left_join(soc_allruns_parcels, inputs$parcel_inputs[, c('parcel_ID', 'area')], by = 'parcel_ID') %>%
    mutate(SOC_abs = SOC * area)
  soc_allruns_parcels <- soc_allruns_parcels %>%
    group_by(parcel_ID, run, year, month, area) %>%
    summarise(
      SOC_ha_pr = SOC[2], SOC_ha_bl = SOC[1], SOC_abs_pr = SOC_abs[2], 
      SOC_abs_bl = SOC_abs[1], SOC_abs_pbdiff = SOC_abs[2]-SOC_abs[1]
      )
  
  # Summarize over runs and add soil C input information
  soc_parcels <- soc_allruns_parcels %>% group_by(parcel_ID, year) %>% summarise_all(mean) %>% select(-c(run, month))
  soc_parcels <- left_join(soc_parcels, parcel_Cinputs[,c('parcel_ID', 'year', 'Cinputs_ha')], by = c('parcel_ID', 'year'))
  soc_parcels <- soc_parcels %>% mutate(Cinputs_abs = Cinputs_ha * area)
  # Add calendar year
  soc_parcels <- soc_parcels %>% mutate(cal_year = settings$proj_start_year + (year-1))

  # Summarize over parcels and calculate CO2 values
  soc_allruns_farm <- soc_allruns_parcels %>% group_by(run, year, month) %>% select(-parcel_ID) %>% 
    summarise(SOC_sum_pr = sum(SOC_abs_pr), SOC_sum_bl = sum(SOC_abs_bl)) %>%
    mutate(SOC_abs_pbdiff = SOC_sum_pr - SOC_sum_bl, CO2 = SOC_abs_pbdiff * 44/12)
  soc_allruns_farm$CO2_gain <- c(0, diff(soc_allruns_farm$CO2))
  soc_allruns_farm$CO2_gain[soc_allruns_farm$year == 0] <- 0
  
  # Summarize over runs and calculate mean, sd, 95% conf and yearly values
  soc_farm <- soc_allruns_farm %>% group_by(year) %>% 
    summarise(SOC_pbdiff = mean(SOC_abs_pbdiff), CO2_cum = mean(CO2), 
              CO2_gain_mean = mean(CO2_gain), CO2_gain_sd = sd(CO2_gain)) %>%
    mutate(CO2_gain_95conf = CO2_gain_mean - CO2_gain_sd * 1.96)
  
  # Add farm level C input variable
  farm_Cinput <- soc_parcels %>% select(c(year, Cinputs_abs)) %>% group_by(year) %>% summarise(Cinputs_abs = sum(Cinputs_abs))
  soc_farm <- right_join(farm_Cinput, soc_farm)
  # Add calendar year
  soc_farm <- soc_farm %>% mutate(cal_year = settings$proj_start_year + (year-1))

  ## Return values ----
  return(list(soc_farm=soc_farm,
              soc_parcels=soc_parcels,
              soc_monthly=soc_monthly,
              parcel_Cinputss=parcel_Cinputs,
              soil_inputs=soil_inputs,
              climate_inputs=present_climate))
}
