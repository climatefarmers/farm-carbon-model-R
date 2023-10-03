#main in function form

#### TO DO: fix n2o_n_fixing & leakage functions so that it is calc and displayed
#### TO DO: include pastures and compst ?? to n2o_n_fixing
#### TO DO LATER: include n2o_n_fixing & compost import to leakage

call_lca <- function(init_file, farm_EnZ, inputs, factors){
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
  
  climate_wet_or_dry <- unique(factors$natural_area_factors$climate_wet_or_dry)
  
  ## Calculation of yearly results
  # Preparation of data frames
  emissions_yearly_sources = data.frame(scenario_selected = c(), source = c(), value = c(), 
                                        gas = c(), co2eq_factor = c(), kgCO2_eq = c())
  
  emissions_parcels_yearly_animals <- create_empty_dataframe(c('parcel_ID', 'year', 'ch4_manure', 'ch4_ent_ferm', 'n2o_manure'))
  
  productivity_table = data.frame(year = c(), crop = c(), productivity = c())
  
  years <- seq(0,10) # year sequence
  scenarios <- c("baseline", paste0("year", c(1:10)))
  
  # merge in factors into lca data
  for (i in years){
    scenario_selected <- scenarios[i+1]
    animals <- merge(filter(inputs$animal_inputs, scenario==scenario_selected), factors$animal_factors, by = "species", all.x = TRUE)
    animals <- merge(animals, factors$methane_factors, by = c("species", "grazing_management", "productivity"), all.x = TRUE)
    n_fixing_species_crop <- merge(filter(inputs$crop_inputs, scenario==scenario_selected), factors$crop_factors, by = "crop", all.x = TRUE)
    n_fixing_species_crop <- merge(n_fixing_species_crop, inputs$parcel_inputs, by = "parcel_ID", all.x = TRUE)
    #n_fixing_species_pasture <- merge(filter(inputs$pasture_inputs, scenario==scenario_selected), factors$pasture_factors, by = "grass", all.x = TRUE)
    #n_fixing_species_pasture <- merge(n_fixing_species_pasture, inputs$parcel_inputs, by = "parcel_ID", all.x = TRUE)
    fertilizers <- merge(filter(inputs$fertilizer_inputs, scenario==scenario_selected), factors$fertilizer_factors, by = "fertilizer_type", all.x = TRUE)
    if(nrow(inputs$fuel_inputs)>0){
      fuel <- merge(filter(inputs$fuel_inputs, scenario==scenario_selected), factors$fuel_factors, by = "fuel_type", all.x = TRUE)
    }else{fuel<-data.frame(inputs$fuel_inputs)}
    amendments <- merge(filter(inputs$orgamendments_inputs, scenario==scenario_selected), factors$manure_factors, by = "source", all.x = TRUE)
    amendments <- merge(filter(amendments, scenario==scenario_selected), inputs$parcel_inputs, by = "parcel_ID", all.x = TRUE)
    # Run through calculations
    fertilizers <- n2o_fertilizer(fertilizers, ef_fertilizer = 0.011) 
    animals<- ch4_enteric_fermentation(animals)
    animals<- n2o_manure_direct(animals, climate_wet_or_dry=climate_wet_or_dry)
    animals<- n2o_manure_indirect(animals, climate_wet_or_dry=climate_wet_or_dry)
    animals<- ch4_manure(animals)
    browser()
    n_fixing_species_crop <- n2o_n_fixing_species_crop(n_fixing_species_crop = n_fixing_species_crop, ef_n = 0.0125, field_area = field_area)
    # not using pasture n fixation 
    # n_fixing_species_pasture <- n2o_n_fixing_species_pasture(n_fixing_species_pasture, field_area = field_area)
    fuel <- co2_fuel_consumption(fuel)
    # add leakage calculations
    leakage <- manure_leakage(amendments)
    yearly_productivity <- productivity_crops(inputs$crop_inputs, scenario_selected, farm_EnZ, inputs$parcel_inputs)
    productivity_table <- rbind(productivity_table,
                                get_yearly_productivity_table(productivity_table, inputs$crop_inputs, scenario_selected, farm_EnZ))
    
    # Get the detailed emissions per parcel  
    if (nrow(fertilizers) > 0){
      parcel_emissions_fertilizer <- fertilizers %>% select(parcel_ID, fertilizer_type, n2o_fertilizer)} else {
        parcel_emissions_fertilizer <- create_empty_dataframe(c("parcel_ID", "fertilizer_type", "n2o_fertilizer"))
      }
    if (nrow(animals) > 0){
      parcel_emissions_animals <- animals %>% select(parcel_ID, species, ch4_manure, ch4_ent_ferm, n2o_manure_indirect, n2o_manure_direct)} else {
        parcel_emissions_animals <- create_empty_dataframe(c("parcel_ID", "species", "ch4_manure", "ch4_ent_ferm", "n2o_manure_indirect", "n2o_manure_direct"))
      }
    if (nrow(n_fixing_species_crop) > 0){
      parcel_emissions_crop <- n_fixing_species_crop %>% select(parcel_ID, crop, n2o_n_fixing)} else {
        parcel_emissions_crop <- create_empty_dataframe(c("parcel_ID", "crop", "n2o_n_fixing"))
      }
    # Farm level emissions
    if (nrow(fuel) > 0){  
      emissions_fuel <- fuel %>% select(fuel_type, co2_fuel)} else {
        emissions_fuel <- create_empty_dataframe(c("fuel_type", "co2_fuel"))
      }
    
    # # not using pasture n fixation 
    # if (nrow(n_fixing_species_pasture) > 0){pasture_results <- n_fixing_species_pasture %>% select(grass, n2o_n_fixing)}else{
    #   pasture_results <- create_empty_dataframe(c("grass", "n2o_n_fixing"))
    # }

    # Results by parcels
    parcel_emissions_animals_sum <- parcel_emissions_animals %>% group_by(parcel_ID) %>% 
      summarise(year = i, 
                ch4_manure = sum(ch4_manure),
                ch4_ent_ferm = sum(ch4_ent_ferm),
                n2o_manure_direct = sum(n2o_manure_direct),
                n2o_manure_indirect = sum(n2o_manure_indirect)
      )
    
    emissions_parcels_yearly_animals <- rbind(emissions_parcels_yearly_animals, parcel_emissions_animals_sum)
    
    parcel_emissions_crop_sum <- parcel_emissions_crop %>% group_by(parcel_ID) %>% summarise(n2o_n_fixing = sum(n2o_n_fixing))
    
    # Total by source
    emissions_fertilizer_total <- parcel_emissions_fertilizer %>% summarise(n2o_fertilizer = sum(n2o_fertilizer))
    
    emissions_animals_total <- parcel_emissions_animals %>% summarise(ch4_manure = sum(ch4_manure),
                                                                      ch4_ent_ferm = sum(ch4_ent_ferm),
                                                                      n2o_manure_direct = sum(n2o_manure_direct),
                                                                      n2o_manure_indirect = sum(n2o_manure_indirect)
                                                                      )
    
    emissions_fuel_total <- emissions_fuel %>% summarise(co2_fuel = sum(co2_fuel))
    
    emissions_crop_total <- parcel_emissions_crop %>% summarise(n2o_n_fixing = sum(n2o_n_fixing))
    
    # pasture_results_sum <- # not using pasture n fixation # pasture_results %>% summarise(n2o_n_fixing = sum(n2o_n_fixing))
    #n_fixing_sum <- parcel_emissions_crop_sum + pasture_results_sum
    leakage_sum <- leakage %>% summarise(co2_leakage = sum(co2_leakage))
    crops_productivity <- data.frame("co2_crops_productivity_tCO2eq"=c(yearly_productivity))
    # Summarise Results
    all_results <- bind_rows(emissions_fertilizer_total, emissions_animals_total, emissions_fuel_total, emissions_crop_total, leakage_sum) %>% #, crops_productivity) %>% 
      pivot_longer(cols = everything(), names_to = "source") %>% 
      filter(!is.na(value)) %>% 
      mutate(gas = substr(source,1,3),
             source = substr(source, 5, nchar(source))) %>% 
      left_join(factors$co2eq_factors, by = "gas") %>% 
      mutate(kgCO2_eq = co2eq_factor * value)
    all_results$scenario_selected=scenario_selected
    all_results$year = i
    emissions_yearly_sources = rbind(emissions_yearly_sources, 
                                     all_results)
  }
  
  emissions_yearly_sources <- emissions_yearly_sources %>%
    mutate(tCO2_eq = kgCO2_eq/1000) %>% select(!c(kgCO2_eq))
  
  emissions_yearly_total = emissions_yearly_sources %>% group_by(year) %>% 
    filter(source != "leakage" & source != "crops_productivity_tCO2eq") %>%
    summarise(emissions_tCO2_eq=sum(tCO2_eq))
  
  emissions_yearly_total$leakage_tCO2_eq <- round((emissions_yearly_sources %>% group_by(year) %>% 
                                                     filter(source=="leakage") %>%
                                                     summarise(leakage_tCO2_eq=tCO2_eq))$leakage_tCO2_eq)
  
  # Fernando: The lines below are not working!!!
  # summarise(total_emissions_without_leakage_tCO2_eq=sum(kgCO2_eq)*1e-3) # Fernando: what is this line doing here!
  # emissions_yearly_total$crops_productivity_tCO2eq = (emissions_yearly_sources %>% group_by(scenario_selected) %>% 
  #                                                filter(source=="crops_productivity_tCO2eq")%>%
  #                                                summarise(leakage_tCO2_eq=kgCO2_eq))$leakage_tCO2_eq
  
  emissions_yearly_total$emissions_diff_tCO2_eq <- 
    round(emissions_yearly_total$emissions_tCO2_eq - emissions_yearly_total$emissions_tCO2_eq[1])

  return(
    list(emissions_yearly_total = emissions_yearly_total, 
         emissions_yearly_sources = emissions_yearly_sources,
         productivity_table = productivity_table,
         emissions_parcels_yearly_animals= emissions_parcels_yearly_animals)
  )
}


create_empty_dataframe <- function(column_names = c()){
  
  df = matrix(ncol = length(column_names), nrow = 0)
  colnames(df)  <- column_names
  df <- as.data.frame(df)
  
  return(df)
  
}

