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
  
  if(settings$debug_mode | settings$use_test_climate){  # will skip fetching climate data and use dummy data if debug_mode is set
    climate_data <- read_csv(file.path("data", "climate_data_1950_2022.csv"), show_col_types = FALSE) # For testing only
  } else {
    climate_data <- get_past_weather_data(init_file, latlon_farm["latitude"], latlon_farm["longitude"], "1950_2022", averaged=FALSE)
    # climate_data=rbind(get_past_weather_data(init_file, latlon_farm["latitude"], latlon_farm["longitude"], "1950_2022"), 
    #                    # get_past_weather_data(init_file, latlon_farm["latitude"], latlon_farm["longitude"], "2021"), 
    #                    # get_past_weather_data(init_file, latlon_farm["latitude"], latlon_farm["longitude"], "2022"), 
    #                    get_future_weather_data(init_file, latlon_farm["latitude"], latlon_farm["longitude"], scenario="rcp4.5"), 
    #                    get_future_weather_data(init_file, latlon_farm["latitude"], latlon_farm["longitude"], scenario="rcp8.5"))
  }
  
  ## Extracting climate from different periods
  
  # Average of all past climate data since start
  mean_past_climate <- climate_data %>% group_by(month) %>% 
    summarise(temperature=mean(temperature),
              precipitation=mean(precipitation),
              evap=mean(evap),
              pevap=mean(pevap))
  
  # Averaged recent climate (10 last years of data)
  nr_cd <- nrow(climate_data) 
  i_cd <- nr_cd - (10*12)  # index for last 10 years of data
  
  mean_recent_climate <- climate_data[i_cd:nr_cd, ] %>% group_by(month) %>% 
    summarise(temperature=mean(temperature),
              precipitation=mean(precipitation),
              evap=mean(evap),
              pevap=mean(pevap))
  
  # Climate for the project period (future years use the averaged recent climate)
  climate_proj <- data.frame()
  proj_start_year <- as.numeric(farms_everything$farmInfo$startYear)

  for(i in 1:10) {
    if(i <= settings$curr_monit_year) {
      curr_year <- proj_start_year + (i-1)
      climate_proj_temp <- climate_data[format(climate_data$date, "%Y") == curr_year, ]
      climate_proj_temp <- climate_proj_temp %>% mutate(year = i) %>% select(-c(date, days_in_a_month, scenario))
    } else {
      climate_proj_temp <- mean_recent_climate %>% mutate(year = i)
    }
    climate_proj <- rbind(climate_proj, climate_proj_temp)
  }
  
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
  
  # Lower clay correlates with higher credits, so we take the 95th percentile of all values in the farm area as a conservative approach.
  soilMapsData = data.frame(
    SOC = quantile(SOC_df$`ocs_0-30cm_mean`, 0.05),
    clay = quantile(clay_df$`clay_5-15cm_mean`/10, 0.95),
    silt = mean(silt_df$`silt_5-15cm_mean`)/10, 
    bulk_density = mean(bdod_df$`bdod_5-15cm_mean`)/100
    )
  
  # Final soil inputs
  soil_inputs <- get_soil_inputs(landUseSummaryOrPractices, farms_everything$soilAnalysis, soilMapsData)
  
  ################# Calculations of C inputs per parcel and scenario
  
  # Attention: YEARLY C inputs are calculated, naming misleading
  baseline_chosen="baseline"
  parcel_Cinputs <- data.frame(parcel_ID=c(), 
                               scenario=c(), 
                               orgamendments_Cinputs=c(), 
                               agroforestry_Cinput=c(), 
                               animal_Cinput=c(), 
                               crop_Cinputs=c(), 
                               pasture_Cinputs=c()
  )
  
  # load("parcel_Cinputs.RData")  # for testing only
  
  years <- 0:10
  
  for(parcel in unique(inputs$parcel_inputs$parcel_ID)){
    
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
                                        orgamendments_Cinputs=orgamendments_Cinputs, 
                                        agroforestry_Cinputs=agroforestry_Cinputs, 
                                        animal_Cinputs=animal_Cinputs, 
                                        crop_Cinputs=crop_Cinputs, 
                                        pasture_Cinputs=pasture_Cinputs
      )
      parcel_Cinputs<-rbind(parcel_Cinputs, parcel_Cinputs_temp)
    }
  }
  
  parcel_Cinputs <- parcel_Cinputs %>% mutate(tot_Cinputs = orgamendments_Cinputs + agroforestry_Cinputs + animal_Cinputs + crop_Cinputs + pasture_Cinputs)
  
  if (length(apply(is.na(parcel_Cinputs), 2, which))==0){
    log4r::info(my_logger, 'parcel C inputs calculations have no NAs.', sep=" ")
  } else {
    log4r::error(my_logger, paste(length(apply(is.na(parcel_Cinputs), 2, which)), 'NAs were found in parcel C inputs calculation results.'))
  }
  
  ################# Calculations of additional C inputs compared to baseline per parcel and scenario -- Fernando: NOT USED FURTHER IN CODE!
  
  parcel_Cinputs_addition = merge(x= parcel_Cinputs, 
                                  y= inputs$parcel_inputs %>% 
                                    select(parcel_ID, area) %>%
                                    mutate(farm_frac= paste(round(area/sum(area)*100), '%')), by="parcel_ID") %>% 
    group_by(parcel_ID, farm_frac) %>% 
    mutate(Cinput_per_ha_project = sum(tot_Cinputs[scenario!=baseline_chosen & scenario!="year0"])/10) %>%
    filter(scenario==baseline_chosen) %>%
    mutate(additional_Cinput_per_ha = round(Cinput_per_ha_project - tot_Cinputs, 2), 
           relative_increase=paste(as.character(ifelse(tot_Cinputs==0, NA, as.integer((Cinput_per_ha_project - tot_Cinputs) / tot_Cinputs*100))), '%'), 
           additional_Cinput_total = round(unique(area)*(Cinput_per_ha_project - tot_Cinputs), 1)) 
  additional_Cinput_total_farm = sum(parcel_Cinputs_addition$additional_Cinput_total)
  parcel_Cinputs_addition = parcel_Cinputs_addition %>%
    mutate(absolute_contribution_perc = round(100*additional_Cinput_total / additional_Cinput_total_farm)) %>%
    select(parcel_ID, farm_frac, additional_Cinput_per_ha, relative_increase, additional_Cinput_total, absolute_contribution_perc)
  
  ## Calculation of total c inputs for the whole farm -- Fernando: NOT BEING USED FURTHER IN CODE!
  # Sum over all parcels
  yearly_Cinputs_farm = merge(x= parcel_Cinputs, 
                              y= inputs$parcel_inputs, 
                              by="parcel_ID") %>%
    group_by(scenario) %>%
    summarise(tot_Cinputs=sum(tot_Cinputs*area), 
              orgamendments_Cinputs=sum(area*orgamendments_Cinputs), 
              animal_Cinputs=sum(area*animal_Cinputs), 
              crop_Cinputs=sum(area*crop_Cinputs), 
              pasture_Cinputs=sum(area*pasture_Cinputs), 
              agroforestry_Cinputs=sum(area*agroforestry_Cinputs))
  
  
  
  ################# Initialisation by making the model reach SOC of natural areas of the pedo-climatic area
  
  ## Calculating the average soil parameters among parcels
  mean_SOC = mean(soil_inputs$SOC)
  mean_clay = mean(soil_inputs$clay)
  mean_silt = mean(soil_inputs$silt)
  mean_bulk_density = mean(soil_inputs$bulk_density)
  
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
    pE = c(0.75, rep(NA, 11)), # mean potential transpiration to open-pan evaporation convertion rate
    tilling_factor = c(1.0, rep(NA, 11))
  )
  
  ## Standard error for each input
  # Modelling perform several times with different inputs
  # Let's define standard deviation for each input representing extrinsic uncertainty of the model
  
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
  
  ## Initialising data structures
  # Data frame per year that includes soc per year, co2 per year and co2 difference per year
  step_in_table <- data.frame(run=c(), 
                              year=c(), 
                              baseline_step_SOC_per_hectare=c(), 
                              holistic_step_SOC_per_hectare=c(), 
                              baseline_step_total_CO2=c(), 
                              holistic_step_total_CO2=c(), 
                              yearly_CO2diff=c()
  )
  
  # Data frame that includes total soc per parcel per scenario 
  all_results <- data.frame(run=c(), parcel_ID=c(), time=c(), SOC=c(), scenario=c(), farm_frac=c())
  # Data frame that includes total soc per farm
  farm_results <- data.frame(run=c(), time=c(), scenario=c(), SOC_farm=c())
  
  ## Choosing model version
  model_version <- ifelse(sum(mean_recent_climate$precipitation) / sum(mean_recent_climate$pevap) < 0.65 &
                            sum(mean_recent_climate$precipitation) < 600, "Semi-arid", "Normal")
  
  ## Initialization of model runs
  # Initializing run counter
  run_ID <- 0
  
  ## Model runs
  for (n in c(1:settings$n_runs)){
    
    run_ID <- run_ID + 1
    all_results_batch <- data.frame(run=c(), parcel_ID=c(), time=c(), SOC=c(), scenario=c(), farm_frac=c())
    farm_results_batch <- data.frame(run=c(), time=c(), SOC_farm=c(), scenario=c())
    
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
    
    # No longer using future climate scenarios. They do not makes sense for 10 year prediction and actual certified credits are calculated for past years anyway
    
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
    
    mean_past_climate_batch <- mean_past_climate %>% mutate(
      year = year, 
      temp = temperature * batch_coef$temp,
      precip = precipitation * batch_coef$precip,
      evap = evap * batch_coef$evap
    )
    
    mean_recent_climate_batch <- mean_recent_climate %>% mutate(
      year = year, 
      temp = temperature * batch_coef$temp,
      precip = precipitation * batch_coef$precip,
      evap = evap * batch_coef$evap
    )
    
    climate_proj_batch <- climate_proj %>% mutate(
      year = year, 
      temp = temperature * batch_coef$temp,
      precip = precipitation * batch_coef$precip,
      evap = evap * batch_coef$evap
    )
    
    for(i in 1:nrow(inputs$parcel_inputs)) {
      
      # Define parcel fixed values
      parcel = inputs$parcel_inputs$parcel_ID[i]
      farm_frac = inputs$parcel_inputs$area[i]/sum(inputs$parcel_inputs$area)
      batch_parcel_Cinputs = parcel_Cinputs %>% mutate(tot_Cinputs=tot_Cinputs*batch_coef$field_carbon_in)
      batch$dr_ratio = ifelse((soil_inputs %>% filter(parcel_ID==parcel))$irrigation==TRUE, dr_ratio_irrigated, dr_ratio_non_irrigated) * batch_coef$dr_ratio
      
      # Select values for the baseline scenario
      batch$field_carbon_in <- (batch_parcel_Cinputs %>% filter (scenario==baseline_chosen & parcel_ID==parcel))$tot_Cinputs
      batch$bare = (inputs$bare_field_inputs %>% filter(scenario == baseline_chosen & parcel_ID == parcel))$bareground
      batch$tilling_factor = (inputs$tilling_inputs %>% filter(scenario==baseline_chosen & parcel_ID==parcel))$tilling_factor
      starting_soil_content = estimate_starting_soil_content(SOC=batch$SOC[1], clay=batch$clay[1])
      
      # Get starting soil content
      starting_soil_content <- estimate_starting_soil_content(SOC = batch$SOC[1], clay = batch$clay[1]) / 2
      
      # Run spinup to equilibrium using baseline data
      time_horizon = 300
      C0_df_spinup <- calc_carbon_over_time(time_horizon, 
                                            field_carbon_in = rep(batch$field_carbon_in[1], time_horizon), 
                                            dr_ratio = rep(batch$dr_ratio[1], time_horizon), 
                                            bare = batch$bare,
                                            temp = mean_recent_climate_batch$temp, 
                                            precip = mean_recent_climate_batch$precip, 
                                            evap = mean_recent_climate_batch$evap, 
                                            soil_thick = batch$soil_thick[1], 
                                            clay = batch$clay[1], 
                                            pE = batch$pE[1], 
                                            PS = starting_soil_content, 
                                            tilling_factor = batch$tilling_factor[1], 
                                            version=model_version, 
                                            silt = batch$silt[1], 
                                            bulk_density = batch$bulk_density[1])
      spinup_soil_content <- as.numeric(tail(C0_df_spinup, 1))[c(1:5)]
      
      # Run a single year for historical baseline
      time_horizon = 1
      C0_df_baseline <- calc_carbon_over_time(time_horizon, 
                                              field_carbon_in = rep(batch$field_carbon_in[1], time_horizon), 
                                              dr_ratio = rep(batch$dr_ratio[1], time_horizon), 
                                              bare = batch$bare,
                                              temp = mean_recent_climate_batch$temp, 
                                              precip = mean_recent_climate_batch$precip, 
                                              evap = mean_recent_climate_batch$evap, 
                                              soil_thick = batch$soil_thick[1], 
                                              clay = batch$clay[1], 
                                              pE = batch$pE[1], 
                                              PS = spinup_soil_content, 
                                              tilling_factor = batch$tilling_factor[1], 
                                              version=model_version, 
                                              silt = batch$silt[1], 
                                              bulk_density = batch$bulk_density[1])
      baseline_soil_content <- as.numeric(tail(C0_df_baseline, 1))[c(1:5)]
      
      
      # Run the projected baseline scenario
      time_horizon = 10
      C0_df_baseline_proj <- calc_carbon_over_time(time_horizon, 
                                                   field_carbon_in = rep(batch$field_carbon_in[1], time_horizon), 
                                                   dr_ratio = rep(batch$dr_ratio[1], time_horizon), 
                                                   bare = batch$bare,
                                                   temp = mean_recent_climate_batch$temp, 
                                                   precip = mean_recent_climate_batch$precip, 
                                                   evap = mean_recent_climate_batch$evap, 
                                                   soil_thick = batch$soil_thick[1], 
                                                   clay = batch$clay[1], 
                                                   pE = batch$pE[1], 
                                                   PS = baseline_soil_content, 
                                                   tilling_factor = batch$tilling_factor[1], 
                                                   version=model_version, 
                                                   silt = batch$silt[1], 
                                                   bulk_density = batch$bulk_density[1])
      C0_df_baseline_proj <- rbind(C0_df_baseline, C0_df_baseline_proj)
      
      
      # Project scenario run
      starting_holistic_soil_content <- baseline_soil_content
      C0_df_holistic <- C0_df_baseline
      for (N in 1:10){
        batch_parcel_Cinputs = parcel_Cinputs %>% mutate(tot_Cinputs=tot_Cinputs*batch_coef$field_carbon_in)
        batch$field_carbon_in <- (batch_parcel_Cinputs %>% filter (scenario==paste("year", N, sep="") & parcel_ID==parcel))$tot_Cinputs
        batch$bare = (inputs$bare_field_inputs %>% filter(scenario == paste("year", N, sep="") & parcel_ID == parcel))$bareground
        batch$tilling_factor = (inputs$tilling_inputs %>% filter(scenario==paste("year", N, sep="") & parcel_ID==parcel))$tilling_factor
        time_horizon = 1
        C0_df_holistic_yearly <- calc_carbon_over_time(time_horizon, 
                                                       field_carbon_in = rep(batch$field_carbon_in[1], time_horizon), 
                                                       dr_ratio = rep(batch$dr_ratio[1], time_horizon), 
                                                       bare = batch$bare,
                                                       temp = mean_recent_climate_batch$temp, 
                                                       precip = mean_recent_climate_batch$precip, 
                                                       evap = mean_recent_climate_batch$evap, 
                                                       soil_thick = batch$soil_thick[1], 
                                                       clay = batch$clay[1], 
                                                       pE = batch$pE[1], 
                                                       PS = starting_holistic_soil_content, 
                                                       tilling_factor = batch$tilling_factor[1], 
                                                       version=model_version, 
                                                       silt = batch$silt[1], 
                                                       bulk_density = batch$bulk_density[1])
        starting_holistic_soil_content <- as.numeric(tail(C0_df_holistic_yearly , 1))[c(1:5)]
        C0_df_holistic <- rbind(C0_df_holistic, C0_df_holistic_yearly) 
      }
      
      all_results_batch_temp <- data.frame(run=run_ID, #, C0_df_baseline_proj$TOT#, rep("current", 120)
                                           parcel_ID=rep(parcel, 264), 
                                           time=rep(seq(as.Date("2020-1-1"), as.Date("2030-12-31"), by = "month"), 2), 
                                           SOC=c(C0_df_baseline_proj$TOT, C0_df_holistic$TOT), 
                                           scenario=c(rep("baseline", 132), rep("holistic", 132)), 
                                           farm_frac=rep(farm_frac, 264))
      all_results_batch <- rbind(all_results_batch, all_results_batch_temp) 
      
    }
    
    farm_results_batch <- data.frame(unique(all_results_batch %>% group_by(time, scenario) %>% mutate(SOC_farm=sum(SOC * farm_frac)) %>% select(run, time, scenario, SOC_farm)))
    months <- format(farm_results_batch$time, format="%m")
    step_in_results <- farm_results_batch[months==12, ]  # SOC content at end of year (December, month 12) is selected.
    step_in_results <- step_in_results %>% mutate(year = format(time, format="%Y"))
    step_baseline <- diff(step_in_results$SOC_farm[step_in_results$scenario=="baseline"])
    step_holistic <- diff(step_in_results$SOC_farm[step_in_results$scenario=="holistic"])
    year_temp <- step_in_results$year[step_in_results$scenario=="holistic"][-1]
    
    step_in_table_temp <- data.frame(run=run_ID, 
                                     year=year_temp, 
                                     baseline_step_SOC_per_hectare=step_baseline, 
                                     holistic_step_SOC_per_hectare=step_holistic, 
                                     baseline_step_total_CO2=step_baseline * sum(inputs$parcel_inputs$area) * 44 / 12, 
                                     holistic_step_total_CO2=step_holistic * sum(inputs$parcel_inputs$area) * 44 / 12) %>%
      mutate(yearly_CO2diff=holistic_step_total_CO2-baseline_step_total_CO2)
    step_in_table <- rbind(step_in_table, step_in_table_temp)
    
    all_results <- rbind(all_results_batch, all_results)
    farm_results <- rbind(farm_results_batch, farm_results)
    print(paste("Run ", n, "over", settings$n_runs, "done"))
    
  } # End of model runs
  
  ## Final data frames by taking the average over the runs
  # Results of soc and co2 per year
  step_in_table_final <- step_in_table %>% group_by(year) %>% 
    summarise(yearly_CO2diff_mean=mean(yearly_CO2diff), 
              yearly_CO2diff_sd=sd(yearly_CO2diff), 
              baseline_step_total_CO2_mean=mean(baseline_step_total_CO2), 
              baseline_step_total_CO2_var=var(baseline_step_total_CO2), 
              holistic_step_total_CO2_mean=mean(holistic_step_total_CO2), 
              holistic_step_total_CO2_var=var(holistic_step_total_CO2), 
              cov_step_total_CO2=cov(baseline_step_total_CO2, holistic_step_total_CO2), 
              sd_diff=sqrt(baseline_step_total_CO2_var+holistic_step_total_CO2_var-2*cov_step_total_CO2) #this equal yearly_CO2diff_sd
    )%>%
    mutate(yearly_CO2diff_final=round(yearly_CO2diff_mean-1.96*yearly_CO2diff_sd)) %>%
    select(year, yearly_CO2diff_final, yearly_CO2diff_mean, 
           yearly_CO2diff_sd, baseline_step_total_CO2_mean, 
           baseline_step_total_CO2_var, holistic_step_total_CO2_mean, 
           holistic_step_total_CO2_var, cov_step_total_CO2, sd_diff)
  
  # Results of soc per parcel per scenario/year
  all_results_final <- all_results %>% group_by(scenario, parcel_ID, time, farm_frac) %>% 
    summarise(SOC_mean=mean(SOC), SOC_sd=sd(SOC)) %>%
    select(parcel_ID, farm_frac, time, scenario, SOC_mean, SOC_sd)
  
  # Results of soc on farm level
  farm_results_final <- farm_results %>% group_by(time, scenario) %>% 
    summarise(SOC_farm_mean=mean(SOC_farm), 
              SOC_farm_sd=sd(SOC_farm)) %>%
    select(time, scenario, SOC_farm_mean, SOC_farm_sd)
  
  
  if (length(apply(is.na(step_in_table_final), 2, which))==0){
    log4r::info(my_logger, 'soil_run_model.R calculations ran smoothly.', sep=" ")
  } else {
    log4r::error(my_logger, 'NAs in results.')
  }
  
  # Print the last spinup to check for equilibrium
  graph <- ggplot(data = C0_df_spinup, aes(x=1:nrow(C0_df_spinup), y=TOT)) +
    geom_line() +
    theme(legend.position = "bottom") +
    # labs(title = "Model spinup for an example farm plot") +
    xlab("Months") +
    ylab("SOC (in tonnes per hectare)") +
    ylim(0, 30)
  print(graph)
  
  return(list(step_in_table_final=step_in_table_final, 
              farm_results_final=farm_results_final, 
              all_results_final=all_results_final,
              parcel_Cinputs=parcel_Cinputs))
}
