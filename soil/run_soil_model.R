run_soil_model <- function(init_file, farms_everything, farm_EnZ, inputs, factors, n_runs, se_field_carbon_in){ 
  
  ## Log starting run message
  log4r::info(my_logger, "run_soil_model.R started running")

  ## Sourcing code from files
  source(file.path("soil", "model_semiArid_functions.R"), local = TRUE)
  source(file.path("soil", "modified_semiArid_functions.R"), local = TRUE)
  source(file.path("soil", "calc_functions_soil_modelling.R"), local = TRUE)
  source("weather_data_pulling_functions.R", local = TRUE)
  
  list2env(inputs, envir = environment())
  list2env(factors, envir = environment())
  
  landUseSummaryOrPractices <- farms_everything$landUse$landUseSummaryOrPractices
  soilAnalysis <- farms_everything$soilAnalysis
  
  ## Get weather data ---
  # Mean coordinates
  latlon_farm <- c(latitude = mean(parcel_inputs$latitude), longitude = mean(parcel_inputs$longitude))
  if(exists("debug_mode")) {
    if(debug_mode){  # will skip fetching climate data and use dummy data if debug_mode is set
      weather_data <- read_csv(file.path("data", "test_weather_data.csv"), show_col_types = FALSE) # For testing only
    } else {
      weather_data=cbind(get_past_weather_data(init_file, latlon_farm["latitude"], latlon_farm["longitude"]), 
                         get_future_weather_data(init_file, latlon_farm["latitude"], latlon_farm["longitude"], scenario="rcp4.5"), 
                         get_future_weather_data(init_file, latlon_farm["latitude"], latlon_farm["longitude"], scenario="rcp8.5"))
    }
  }
  
  ## Soil data ---
  # Gets soil data from https://maps.isric.org/ (AWS)
  OCS_df = s3read_using(FUN = read_csv, object = paste("s3://soil-modelling/soil_variables/", farmId, "/ocs.csv", sep=""))
  clay_df = s3read_using(FUN = read_csv, object = paste("s3://soil-modelling/soil_variables/", farmId, "/clay.csv", sep=""))
  silt_df = s3read_using(FUN = read_csv, object = paste("s3://soil-modelling/soil_variables/", farmId, "/silt.csv", sep=""))
  bdod_df = s3read_using(FUN = read_csv, object = paste("s3://soil-modelling/soil_variables/", farmId, "/bdod.csv", sep=""))
  # Fill soil maps data frame
  soilMapsData = data.frame(SOC=mean(OCS_df$`ocs_0-30cm_mean`), SOC_Q0.05=mean(OCS_df$`ocs_0-30cm_Q0.05`), SOC_Q0.95=mean(OCS_df$`ocs_0-30cm_Q0.95`), 
                            clay=mean(clay_df$`clay_5-15cm_mean`)/10, clay_Q0.05=mean(clay_df$`clay_5-15cm_Q0.05`)/10, clay_Q0.95=mean(clay_df$`clay_5-15cm_Q0.95`)/10, 
                            silt=mean(silt_df$`silt_5-15cm_mean`)/10, silt_Q0.05=mean(silt_df$`silt_5-15cm_Q0.05`)/10, silt_Q0.95=mean(silt_df$`silt_5-15cm_Q0.95`)/10, 
                            bulk_density=mean(bdod_df$`bdod_5-15cm_mean`)/100, bdod_Q0.05=mean(bdod_df$`bdod_5-15cm_Q0.05`)/100, bdod_Q0.95=mean(bdod_df$`bdod_5-15cm_Q0.95`)/100)# waiting for values from soil maps
  # Final soil inputs
  soil_inputs <- get_soil_inputs(landUseSummaryOrPractices, soilAnalysis, soilMapsData)
  
  ## Tilling inputs
  tilling_inputs <- get_tilling_inputs(landUseSummaryOrPractices, tilling_factors, farm_EnZ)
  
  
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
  
  scenarios <- c(do.call(paste0, expand_grid("year", 1:10)), baseline_chosen)

  for(parcel in unique(parcel_inputs$parcel_ID)){
    
    for(scenario in scenarios){
      
      orgamendments_Cinputs <- get_monthly_Cinputs_orgamendments(orgamendments_inputs, manure_factors, scenario, parcel)
      agroforestry_Cinputs <- 0 # get_monthly_Cinputs_agroforestry(agroforestry_inputs, agroforestry_factors, scenario, parcel, lat_farmer) # TREES NOT COUNTED BEFORE GOOD CHECK OF DATA QUALITY
      animal_Cinputs <- get_monthly_Cinputs_animals(animal_inputs, animal_factors, scenario, parcel)
      crop_Cinputs <- get_monthly_Cinputs_crop(crop_inputs, crop_factors, scenario, parcel, farm_EnZ)
      pasture_Cinputs <- get_monthly_Cinputs_pasture(pasture_inputs, pasture_factors, scenario, parcel)
      
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
                                  y= parcel_inputs %>% 
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
                              y= parcel_inputs, 
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
  dr_ratio_agroforestry = unique(natural_area_factors$dr_ratio_agroforestry)
  dr_ratio_non_irrigated = unique(natural_area_factors$dr_ratio_non_irrigated)
  dr_ratio_irrigated = unique(natural_area_factors$dr_ratio_irrigated)
  
  ## Building a mean input dataframe to feed RothC
  # Mean value for each model input parameter
  mean=c(list(rep(0, 12)), 
         list(c(dr_ratio_non_irrigated, rep(NA, 11))), 
         list(as.factor(c(logical(12)))), 
         list(weather_data$past_temperature), 
         list(weather_data$future_temperature_rcp4.5), 
         list(weather_data$past_precipitation), 
         list(weather_data$future_precipitation_rcp4.5), 
         list(weather_data$past_evap), 
         list(weather_data$future_evap_rcp4.5), 
         list(c(30, rep(NA, 11))), # modelled for 30 cm depth as recommended in IPCC Guidelines 2006
         list(c(soilMapsData$SOC, rep(NA, 11))), 
         list(c(soilMapsData$clay, rep(NA, 11))), 
         list(c(soilMapsData$silt, rep(NA, 11))), 
         list(c(soilMapsData$bulk_density, rep(NA, 11))), 
         list(c(0.75, rep(NA, 11))), # mean potential transpiration to open-pan evaporation convertion rate
         list(c(1.0, rep(NA, 11))))
  colnames_ranges=c("run", "dr_ratio", "bare", "past_temp", "future_temp", "past_precip", "future_precip", "past_evap", "future_evap", "soil_thick", "SOC", "clay", "silt", "bulk_density", "pE", "tilling_factor")
  mean_input = data.frame(mean)
  colnames(mean_input) = colnames_ranges
  
  ## Standard deviation for each input
  # Modelling perform several times with different inputs
  # Let's define standard deviation for each input representing extrinsic uncertainty of the model
  se=data.frame(field_carbon_in=se_field_carbon_in, 
                dr_ratio = 0.025, 
                temp = 0.025, 
                precip = 0.025, 
                evap = 0.025, 
                soil_thick = 0.025, 
                SOC = 0.025, #(soilMapsData$SOC_Q0.95-soilMapsData$SOC_Q0.05)/3, 
                clay = 0.025, #(soilMapsData$clay_Q0.95-soilMapsData$clay_Q0.05)/5, # to be refined
                silt = 0.025, #(soilMapsData$silt_Q0.95-soilMapsData$silt_Q0.05)/5, 
                bulk_density = 0.025, #(soilMapsData$bdod_Q0.95-soilMapsData$bdod_Q0.05)/3, 
                pE = 0.025, 
                tilling_factor = 0.025)
  
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
  all_results<-data.frame(run=c(), parcel_ID=c(), time=c(), SOC=c(), scenario=c(), farm_frac=c())
  # Data frame that includes total soc per farm
  farm_results<-data.frame(run=c(), time=c(), scenario=c(), SOC_farm=c())
  
  ## Chossing model version
  model_version = ifelse(sum(weather_data$past_precipitation) / sum(weather_data$past_pevap) < 0.65 &
                           sum(weather_data$past_precipitation) < 600, "Semi-arid", "Normal")
  
  ## Initialisation of model runs
  # Initialising run counter
  run_ID = 0
  
  ## Model runs
  for (n in c(1:n_runs)){
    run_ID = run_ID + 1
    all_results_batch <- data.frame(run=c(), parcel_ID=c(), time=c(), SOC=c(), scenario=c(), farm_frac=c())
    farm_results_batch <- data.frame(run=c(), time=c(), SOC_farm=c(), scenario=c())
    #Choice of a random factor to normally randomize input values
    batch_coef=data.frame(field_carbon_in = rnorm(1, 1, se$field_carbon_in), 
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
    
    #Choose randomly one of the two climate scenario
    climate_scenario = ifelse(sample(0:1, 1)==0, 'rcp4.5', 'rcp8.5')
    if (climate_scenario=='rcp4.5'){
      mean_input$future_temp = weather_data$future_temperature_rcp4.5
      mean_input$future_precip = weather_data$future_precipitation_rcp4.5
      mean_input$future_evap = weather_data$future_evap_rcp4.5
    }
    if (climate_scenario=='rcp8.5'){
      mean_input$future_temp = weather_data$future_temperature_rcp8.5
      mean_input$future_precip = weather_data$future_precipitation_rcp8.5
      mean_input$future_evap = weather_data$future_evap_rcp8.5
    }
    #Apply factors to inputs average
    batch=data.frame(run=run_ID, 
                     bare = mean_input$bare, 
                     past_temp = mean_input$past_temp*batch_coef$temp, 
                     past_precip = mean_input$past_precip*batch_coef$precip, 
                     past_evap = mean_input$past_evap*batch_coef$evap, 
                     future_temp = mean_input$future_temp*batch_coef$temp, 
                     future_precip = mean_input$future_precip*batch_coef$precip, 
                     future_evap = mean_input$future_evap*batch_coef$evap, 
                     soil_thick = mean_input$soil_thick*batch_coef$soil_thick, 
                     SOC = mean_input$SOC*batch_coef$SOC, 
                     clay = mean_input$clay*batch_coef$clay, 
                     silt = mean_input$silt*batch_coef$silt, 
                     bulk_density = mean_input$bulk_density*batch_coef$bulk_density, 
                     pE = mean_input$pE*batch_coef$pE, 
                     tilling_factor = mean_input$tilling_factor*batch_coef$tilling_factor)
    batch = data.frame(batch)
    
    for(i in c(1:nrow(parcel_inputs))){
      browser()
      parcel = parcel_inputs$parcel_ID[i]
      farm_frac = parcel_inputs$area[i]/sum(parcel_inputs$area)
      #Select parcel's fixed values
      batch_parcel_Cinputs = parcel_Cinputs %>% mutate(tot_Cinputs=tot_Cinputs*batch_coef$field_carbon_in)
      batch$dr_ratio = ifelse((soil_inputs %>% filter(parcel_ID==parcel))$irrigation==TRUE, dr_ratio_irrigated, dr_ratio_non_irrigated) * batch_coef$dr_ratio
      # choice of scenario = baseline
      batch$field_carbon_in <- (batch_parcel_Cinputs %>% filter (scenario==baseline_chosen & parcel_ID==parcel))$tot_Cinputs
      batch$bare = (bare_field_inputs %>% filter(scenario == baseline_chosen & parcel_ID == parcel))$bareground
      batch$tilling_factor = (tilling_inputs %>% filter(scenario==baseline_chosen & parcel_ID==parcel))$tilling_factor
      starting_soil_content = estimate_starting_soil_content(SOC=batch$SOC[1], clay=batch$clay[1]) 
      time_horizon = 1
      C0_df <- calc_carbon_over_time(time_horizon, 
                                     field_carbon_in = rep(batch$field_carbon_in[1], time_horizon), 
                                     dr_ratio = rep(batch$dr_ratio[1], time_horizon), 
                                     bare = batch$bare,
                                     temp = batch$future_temp, 
                                     precip = batch$future_precip, 
                                     evap = batch$future_evap, 
                                     soil_thick = batch$soil_thick[1], 
                                     clay = batch$clay[1], 
                                     pE = batch$pE[1], 
                                     PS = starting_soil_content, 
                                     tilling_factor = batch$tilling_factor[1], 
                                     version=model_version, 
                                     silt = batch$silt[1], 
                                     bulk_density = batch$bulk_density[1])
      starting_soil_content <- as.numeric(tail(C0_df, 1))[c(1:5)]
      time_horizon = 10
      C0_df_mdf <- calc_carbon_over_time(time_horizon, 
                                         field_carbon_in = rep(batch$field_carbon_in[1], time_horizon), 
                                         dr_ratio = rep(batch$dr_ratio[1], time_horizon), 
                                         bare = batch$bare,
                                         temp = batch$future_temp, 
                                         precip = batch$future_precip, 
                                         evap = batch$future_evap, 
                                         soil_thick = batch$soil_thick[1], 
                                         clay = batch$clay[1], 
                                         pE = batch$pE[1], 
                                         PS = starting_soil_content, 
                                         tilling_factor = batch$tilling_factor[1], 
                                         version=model_version, 
                                         silt = batch$silt[1], 
                                         bulk_density = batch$bulk_density[1])
      
      N_1 = 1 #first year of future modelling
      batch_parcel_Cinputs = parcel_Cinputs %>% mutate(tot_Cinputs=tot_Cinputs*batch_coef$field_carbon_in)
      batch$field_carbon_in <- (batch_parcel_Cinputs %>% filter (scenario==paste("year", N_1, sep="") & parcel_ID==parcel))$tot_Cinputs
      batch$bare = (bare_field_inputs %>% filter(scenario == paste("year", N_1, sep="") & parcel_ID == parcel))$bareground
      batch$tilling_factor = (tilling_inputs %>% filter(scenario==paste("year", N_1, sep="") & parcel_ID==parcel))$tilling_factor
      time_horizon = 1
      C0_df_holistic_yearly <- calc_carbon_over_time(time_horizon, 
                                                     field_carbon_in = rep(batch$field_carbon_in[1], time_horizon), 
                                                     dr_ratio = rep(batch$dr_ratio[1], time_horizon), 
                                                     bare = batch$bare,
                                                     temp = batch$future_temp, 
                                                     precip = batch$future_precip, 
                                                     evap = batch$future_evap, 
                                                     soil_thick = batch$soil_thick[1], 
                                                     clay = batch$clay[1], 
                                                     pE = batch$pE[1], 
                                                     PS = starting_soil_content, 
                                                     tilling_factor = batch$tilling_factor[1], 
                                                     version=model_version, 
                                                     silt = batch$silt[1], 
                                                     bulk_density = batch$bulk_density[1])
      starting_holistic_soil_content <- as.numeric(tail(C0_df_holistic_yearly , 1))[c(1:5)]
      C0_df_holistic = C0_df_holistic_yearly
      # next years
      for (N in c((N_1+1):10)){
        batch_parcel_Cinputs = parcel_Cinputs %>% mutate(tot_Cinputs=tot_Cinputs*batch_coef$field_carbon_in)
        batch$field_carbon_in <- (batch_parcel_Cinputs %>% filter (scenario==paste("year", N, sep="") & parcel_ID==parcel))$tot_Cinputs
        batch$bare = (bare_field_inputs %>% filter(scenario == paste("year", N_1, sep="") & parcel_ID == parcel))$bareground
        batch$tilling_factor = (tilling_inputs %>% filter(scenario==paste("year", N, sep="") & parcel_ID==parcel))$tilling_factor
        time_horizon = 1
        C0_df_holistic_yearly <- calc_carbon_over_time(time_horizon, 
                                                       field_carbon_in = rep(batch$field_carbon_in[1], time_horizon), 
                                                       dr_ratio = rep(batch$dr_ratio[1], time_horizon), 
                                                       bare = batch$bare,
                                                       temp = batch$future_temp, 
                                                       precip = batch$future_precip, 
                                                       evap = batch$future_evap, 
                                                       soil_thick = batch$soil_thick[1], 
                                                       clay = batch$clay[1], 
                                                       pE = batch$pE[1], 
                                                       PS = starting_holistic_soil_content, 
                                                       tilling_factor = batch$tilling_factor[1], 
                                                       version=model_version, 
                                                       silt = batch$silt[1], 
                                                       bulk_density = batch$bulk_density[1])
        starting_holistic_soil_content <- as.numeric(tail(C0_df_holistic_yearly , 1))[c(1:5)]
        C0_df_holistic= rbind(C0_df_holistic, C0_df_holistic_yearly)
      }
      
      all_results_batch <- rbind(all_results_batch, data.frame(run=run_ID, 
                                                               parcel_ID=rep(parcel, 264), 
                                                               time=rep(seq(as.Date("2020-1-1"), as.Date("2030-12-31"), by = "month"), 2), 
                                                               SOC=c(C0_df$TOT, C0_df_mdf$TOT, C0_df$TOT, C0_df_holistic$TOT), 
                                                               scenario=c(rep("baseline", 132), rep("holistic", 132)), 
                                                               farm_frac=rep(farm_frac, 264))) #, C0_df_baseline$TOT#, rep("current", 120)
    }
    
    farm_results_batch <- data.frame(unique(all_results_batch %>% group_by(time, scenario) %>% mutate(SOC_farm=sum(SOC * farm_frac)) %>% select(run, time, scenario, SOC_farm)))
    months <- format(farm_results_batch$time, format="%m")
    step_in_results <- farm_results_batch[months==12, ]  # SOC content at end of year (December, month 12) is selected.
    step_in_results <- step_in_results %>% mutate(year = format(time, format="%Y"))
    step_baseline <- diff(step_in_results$SOC_farm[step_in_results$scenario=="baseline"])
    step_holistic <- diff(step_in_results$SOC_farm[step_in_results$scenario=="holistic"])
    year_temp <- step_in_results$year[step_in_results$scenario=="holistic"][2:11]
    
    step_in_table <- rbind(step_in_table, (data.frame(run=run_ID, 
                                                      year=year_temp, 
                                                      baseline_step_SOC_per_hectare=step_baseline, 
                                                      holistic_step_SOC_per_hectare=step_holistic, 
                                                      baseline_step_total_CO2=step_baseline*sum(parcel_inputs$area) * 44 / 12, 
                                                      holistic_step_total_CO2=step_holistic*sum(parcel_inputs$area) * 44 / 12) %>%
                                             mutate(yearly_CO2diff=holistic_step_total_CO2-baseline_step_total_CO2)))
    all_results <- rbind(all_results_batch, all_results)
    farm_results <- rbind(farm_results_batch, farm_results)
    print(paste("Run ", n, "over", n_runs, "done"))
    
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
  
  ## Write out land use type
  write.csv(landUseType, file.path("logs", 
                                   paste("landUseType_", 
                                         farms_everything$farmInfo$farmManagerFirstName, 
                                         farms_everything$farmInfo$farmManagerLastName, 
                                         ".csv", sep="")
  ), row.names = FALSE)
  
  
  if (length(apply(is.na(step_in_table_final), 2, which))==0){
    log4r::info(my_logger, 'soil_run_model.R calculations ran smoothly.', sep=" ")
  } else {
    log4r::error(my_logger, 'NAs in results.')
  }
  
  return(list(step_in_table_final=step_in_table_final, 
              farm_results_final=farm_results_final, 
              all_results_final=all_results_final))
}
