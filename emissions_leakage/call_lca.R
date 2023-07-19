#main in function form

#### TO DO: fix n2o_n_fixing & leakage functions so that it is calc and displayed
#### TO DO: include pastures and compst ?? to n2o_n_fixing
#### TO DO LATER: include n2o_n_fixing & compost import to leakage

call_lca <- function(init_file, farms_everything, farm_EnZ){
  ## This life cycle analysis function for getting the farm emissions
  ## is meant to be called by passing it the init_file and farm data directly.
  
  ## Log start running messages
  log4r::info(my_logger, "run_lca.R started running for all scenario.")
  
  ## Define paths
  climatic_zone_loc <- init_file$climatic_zone_loc

  source(file.path("emissions_leakage", "calc_functions.R"), local = TRUE)
  source(file.path("emissions_leakage", "results_functions.R"), local = TRUE)
  source(file.path("emissions_leakage", "agroforestry_functions.R"), local = TRUE)
  source(file.path("emissions_leakage", "leakage_functions.R"), local = TRUE)
  source(file.path("emissions_leakage", "test_functions.R"), local = TRUE)
  source("mongodb_extraction_functions.R", local = TRUE)

  fuel_object = farms_everything$energyUsage
  livestock = farms_everything$liveStock
  landUseSummaryOrPractices = farms_everything$landUse$landUseSummaryOrPractices
  soilAnalysis = farms_everything$soilAnalysis
  livestock = farms_everything$liveStock
  landUseSummaryOrPractices = farms_everything$landUse$landUseSummaryOrPractices
  soilAnalysis = farms_everything$soilAnalysis
  
  ## If TRUE, copies the practice of a single year to all others
  if (copy_yearX_to_following_years_landUse == TRUE){
    #last_year_to_duplicate = 1
    for(i in c(last_year_to_duplicate+1:10)){landUseSummaryOrPractices[[1]][[paste("year",i,sep="")]]=landUseSummaryOrPractices[[1]][[paste("year",last_year_to_duplicate,sep="")]]}
    log4r::info(my_logger, paste("MODIF: EVERY PARCELS: Data from year", last_year_to_duplicate,
                                 "was pasted to every following years", sep=" "))
  }
  if (copy_yearX_to_following_years_livestock == TRUE){
    #last_year_to_duplicate = 1
    for(i in c(last_year_to_duplicate+1:10)){livestock[["futureManagement"]][[1]][[paste("year",i,sep="")]]=livestock[["futureManagement"]][[1]][[paste("year",last_year_to_duplicate,sep="")]]}
    log4r::info(my_logger, paste("MODIF: LIVESTOCK: Data from year", last_year_to_duplicate,
                                 "was pasted to every following years", sep=" "))
  }

  ## Read in lca data
  animal_factors <- read_csv(file.path("data", "carbon_share_manure.csv")) %>% filter(type=="manure") %>% 
    rename(species=manure_source)
  co2eq_factors <- read_csv(file.path("data", "co2eq_factors.csv"))
  crop_factors <- read_csv(file.path("data", "crop_factors.csv"))
  fertilizer_factors <- read_csv(file.path("data", "fertilizer_factors.csv"))
  fuel_factors <- read_csv(file.path("data", "fuel_factors.csv"))
  tree_factors <- read_csv(file.path("data", "agroforestry_factors.csv"))
  manure_factors <- read_csv(file.path("data", "carbon_share_manure.csv"))
  natural_area_factors <- read_csv(file.path("data", "natural_area_factors.csv")) %>%
    filter(pedo_climatic_area==farm_EnZ)
  pasture_factors <- read_csv(file.path("data", "pasture_factors.csv"))
  climate_zone <- unique(natural_area_factors$climate_zone)
  climate_wet_or_dry <- unique(natural_area_factors$climate_wet_or_dry)
  methane_factors <- read_csv(file.path("data", "methane_emission_factors.csv")) %>% filter(climate == climate_zone) %>% select(-climate)
  grazing_factors <- read_csv(file.path("data", "grazing_factors.csv"))
  
  parcel_data <- get_parcel_inputs(landUseSummaryOrPractices)
  total_grazing_table = get_total_grazing_table(landUseSummaryOrPractices,livestock, animal_factors=manure_factors %>% filter(type=="manure") %>% 
                                                  rename(species=manure_source), parcel_data)
  crop_data <- get_crop_inputs(landUseSummaryOrPractices, pars)
  crop_data <- get_baseline_crop_inputs(landUseSummaryOrPractices, crop_data, crop_factors, my_logger, farm_EnZ)
  pasture_data <- get_pasture_inputs(landUseSummaryOrPractices, grazing_factors, farm_EnZ, total_grazing_table, my_logger, parcel_data, pars)
  fertilizer_data <- get_fertilizer_inputs(landUseSummaryOrPractices)
  fuel_data <- get_fuel_inputs(fuel_object)
  add_manure_data <- get_add_manure_inputs(landUseSummaryOrPractices)
  tree_data <- get_agroforestry_inputs(landUseSummaryOrPractices)
  animal_data <- get_animal_inputs(landUseSummaryOrPractices,livestock, parcel_data)
  
  ## Check input data for validity
  check_animal_data(animal_data, animal_factors)
  check_crop_data(crop_data, crop_factors)
  check_fertilizer_data(fertilizer_data, fertilizer_factors)
  check_fuel_data(fuel_data, fuel_factors)
  check_manure_data(add_manure_data, manure_factors)  
  

  ## Calculation of yearly results
  # Preparation of data frames
  CO2emissions_detailed_yearly_results = data.frame(scenario_selected = c(), source = c(), value = c(), 
                              gas = c(), co2eq_factor = c(), kgCO2_eq = c())
  productivity_table = data.frame(year = c(), crop = c(), productivity = c())
  
  years <- seq(0,10) # year sequence
  scenarios <- c("baseline",paste0("year", c(1:10)))
  
  # merge in factors into lca data
  for (i in years){
    scenario_selected <- scenarios[i+1]
    animals <- merge(filter(animal_data, scenario==scenario_selected), animal_factors, by = "species", all.x = TRUE)
    animals <- merge(animals, methane_factors, by = c("species" = "species", "grazing_management" = "grazing_management", "productivity" = "productivity"), all.x = TRUE)
    n_fixing_species_crop <- merge(filter(crop_data, scenario==scenario_selected), crop_factors, by = "crop", all.x = TRUE)
    n_fixing_species_crop <- merge(n_fixing_species_crop, parcel_data, by = "parcel_ID", all.x = TRUE)
    #n_fixing_species_pasture <- merge(filter(pasture_data, scenario==scenario_selected), pasture_factors, by = "grass", all.x = TRUE)
    #n_fixing_species_pasture <- merge(n_fixing_species_pasture, parcel_data, by = "parcel_ID", all.x = TRUE)
    fertilizers <- merge(filter(fertilizer_data, scenario==scenario_selected), fertilizer_factors, by = "fertilizer_type", all.x = TRUE)
    if(nrow(fuel_data)>0){
      fuel <- merge(filter(fuel_data, scenario==scenario_selected), fuel_factors, by = "fuel_type", all.x = TRUE)
    }else{fuel<-data.frame(fuel_data)}
    amendments <- merge(filter(add_manure_data, scenario==scenario_selected), manure_factors, by = "manure_source", all.x = TRUE)
    amendments <- merge(filter(amendments, scenario==scenario_selected), parcel_data, by = "parcel_ID", all.x = TRUE)
    # Run through calculations
    fertilizers <- n2o_fertilizer(fertilizers, ef_fertilizer = 0.011) 
    animals<- ch4_enteric_fermentation(animals)
    animals<- n2o_manure_deposition_direct(animals, climate_wet_or_dry=climate_wet_or_dry)
    animals<- n2o_manure_deposition_indirect(animals, climate_wet_or_dry=climate_wet_or_dry)
    animals<- ch4_manure_deposition(animals)
    n_fixing_species_crop <- n2o_n_fixing_species_crop(n_fixing_species_crop, field_area = field_area)
    # not using pasture n fixation 
    # n_fixing_species_pasture <- n2o_n_fixing_species_pasture(n_fixing_species_pasture, field_area = field_area)
    fuel <- co2_fuel_consumption(fuel)
    # add leakage calculations
    leakage <- manure_leakage(amendments)
    yearly_productivity <- productivity_crops(crop_data, scenario_selected, farm_EnZ)
    productivity_table <- rbind(productivity_table,
                                get_yearly_productivity_table(productivity_table, crop_data, scenario_selected, farm_EnZ))
    # Clean Results 
    if (nrow(fertilizers) > 0){
      fertilizer_results <- fertilizers %>% select(fertilizer_type, n2o_fertilizer)}else{
      fertilizer_results <- create_empty_dataframe(c("fertilizer_type", "n2o_fertilizer"))
    }
    if (nrow(animals) > 0){  
      animal_results <- animals%>% select(species, ch4_manure_dep, ch4_ent_ferm, n2o_urine_dung_indirect, n2o_urine_dung_direct)}else{
      animal_results <- create_empty_dataframe(c("species", "ch4_manure_dep", "ch4_ent_ferm", "n2o_urine_dung_indirect", "n2o_urine_dung_direct"))
    }
    if (nrow(fuel) > 0){  
      fuel_results <- fuel %>% select(fuel_type, co2_fuel)}else{
      fuel_results <- create_empty_dataframe(c("fuel_type", "co2_fuel"))
    }
    if (nrow(n_fixing_species_crop) > 0){
      crop_results <- n_fixing_species_crop %>% select(crop, n2o_n_fixing)}else{
      crop_results <- create_empty_dataframe(c("crop", "n2o_n_fixing"))
    }
    # # not using pasture n fixation 
    # if (nrow(n_fixing_species_pasture) > 0){pasture_results <- n_fixing_species_pasture %>% select(grass, n2o_n_fixing)}else{
    #   pasture_results <- create_empty_dataframe(c("grass", "n2o_n_fixing"))
    # }
    fertilizer_results_sum <- fertilizer_results %>% summarise(n2o_fertilizer = sum(n2o_fertilizer))
    animal_results_sum <- animal_results %>% summarise(ch4_manure_dep = sum(ch4_manure_dep),
                                                       ch4_ent_ferm = sum(ch4_ent_ferm),
                                                       n2o_manure_deposition = sum(n2o_urine_dung_indirect)+sum(n2o_urine_dung_direct))
    fuel_results_sum <- fuel_results %>% summarise(co2_fuel = sum(co2_fuel))
    crop_results_sum <- crop_results %>% summarise(n2o_n_fixing = sum(n2o_n_fixing))
    # pasture_results_sum <- # not using pasture n fixation # pasture_results %>% summarise(n2o_n_fixing = sum(n2o_n_fixing))
    #n_fixing_sum <- crop_results_sum + pasture_results_sum
    leakage_sum <- leakage %>% summarise(co2_leakage = sum(co2_leakage))
    crops_productivity <- data.frame("co2_crops_productivity_tCO2eq"=c(yearly_productivity))
    # Summarise Results
    all_results <- bind_rows(fertilizer_results_sum, animal_results_sum, fuel_results_sum, crop_results_sum, leakage_sum) %>% #, crops_productivity) %>% 
      pivot_longer(cols = everything(), names_to = "source") %>% 
      filter(!is.na(value)) %>% 
      mutate(gas = substr(source,1,3),
             source = substr(source, 5, nchar(source))) %>% 
      left_join(co2eq_factors, by = "gas") %>% 
      mutate(kgCO2_eq = co2eq_factor * value)
    all_results$scenario_selected=scenario_selected
    all_results$year = i
    CO2emissions_detailed_yearly_results = rbind(CO2emissions_detailed_yearly_results, 
                                                  all_results)
  }

  CO2emissions_detailed_yearly_results <- CO2emissions_detailed_yearly_results %>%
    mutate(tCO2_eq=kgCO2_eq/1000) %>% select(!c(kgCO2_eq))
  
  write_csv(productivity_table, file.path("logs", paste0(farmId,"_productivity_table.csv")))
  write_csv(CO2emissions_detailed_yearly_results, file.path("logs", paste0(farmId,"_CO2emissions_detailed_yearly_results.csv")))
  
  yearly_aggregated_results = CO2emissions_detailed_yearly_results %>% group_by(year) %>% 
    filter(source != "leakage" & source != "crops_productivity_tCO2eq") %>%
    summarise(emissions_tCO2_eq=sum(tCO2_eq))
  
  yearly_aggregated_results$leakage_tCO2_eq <- round((CO2emissions_detailed_yearly_results %>% group_by(year) %>% 
                                                 filter(source=="leakage") %>%
                                                 summarise(leakage_tCO2_eq=tCO2_eq))$leakage_tCO2_eq)
  
  # Fernando: The lines below are not working!!!
  # summarise(total_emissions_without_leakage_tCO2_eq=sum(kgCO2_eq)*1e-3) # Fernando: what is this line doing here!
  # yearly_aggregated_results$crops_productivity_tCO2eq = (CO2emissions_detailed_yearly_results %>% group_by(scenario_selected) %>% 
  #                                                filter(source=="crops_productivity_tCO2eq")%>%
  #                                                summarise(leakage_tCO2_eq=kgCO2_eq))$leakage_tCO2_eq
  
  yearly_aggregated_results$emissions_diff_tCO2_eq <- 
    round(yearly_aggregated_results$emissions_tCO2_eq -
    yearly_aggregated_results$emissions_tCO2_eq[1])
  
  return(list(emissions=yearly_aggregated_results, 
              emissions_detailed=CO2emissions_detailed_yearly_results,
              productivity=productivity_table))
}


create_empty_dataframe <- function(column_names = c()){
  
  df = matrix(ncol = length(column_names), nrow = 0)
  colnames(df)  <- column_names
  df <- as.data.frame(df)
  
  return(df)
  
}

