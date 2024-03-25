library(tidyr)

### Calculation of added manure input: Carbon input due to manure/compost/hay daily spreading over a grazed field
# YEARLY
get_monthly_Cinputs_orgamendments <- function (orgamendments_inputs, orgamendments, scenario_chosen, parcel){
  
  orgamendments_inputs = filter(orgamendments_inputs, scenario==scenario_chosen & parcel_ID==parcel)
  
  if(nrow(orgamendments_inputs)==0){return(0)}
  
  orgamendments = merge(x = orgamendments_inputs, y = orgamendments, by = "source", all.x = TRUE) %>% 
    mutate (tC_inputs_orgamendments = quantity_t_ha * carbon_content)
  
  tC_inputs_orgamendments = sum(orgamendments$tC_inputs_orgamendments)
  
  return(tC_inputs_orgamendments)
}

### Calculation of animal input: Carbon input due to manure daily spreading over a grazed field
# YEARLY
# TO-DO: I think we can get rid of the for loop. So far I just changed some variable names and excluded factors_animals from the function
# as the necessary factor c_kg_per_year_per_animal is already in the animal_inputs table. 
get_yearly_Cinputs_animals <- function (animal_inputs, scenario_chosen, parcel){
  
  animal_inputs <- filter(animal_inputs, scenario == scenario_chosen & parcel_name == parcel & amount > 0)
  
  if(nrow(animal_inputs)==0){return(0)}
  
  animals <- mutate (C_inputs_manure_kg_per_ha_per_year = amount*c_kg_per_year_per_animal / area * grazing_days / 365)
  
  tC_inputs_per_ha_per_year = sum(animals$C_inputs_manure_kg_per_ha_per_year) * 1e-3
  
  return(tC_inputs_per_ha_per_year)
}

### Calculation of agroforestry input
# YERLY
get_monthly_Cinputs_agroforestry <- function (tree_inputs, agroforestry_factors, scenario_chosen, parcel, lat_farmer){
  
  tree_inputs = filter(tree_inputs,scenario==scenario_chosen & parcel_ID==parcel)
  
  if(nrow(tree_inputs)==0){ return(0) }
  
  # Difference in factors depending on climatic zone
  zone=ifelse(lat_farmer<57,"Temperate","Boreal")
  trees = merge(x = tree_inputs, 
                y = filter(agroforestry_factors,climatic_zone==zone), by = "tree_species", all.x = TRUE) %>% 
    # n_trees removed from input template # mutate (tree_density=ifelse(is.na(tree_density)==FALSE,tree_density,ifelse(is.na(n_trees)==FALSE,n_trees/area,typical_tree_density))) %>%
    # Calculation of c input depending on data availability and dbh
    mutate (tC_inputs_tree_per_ha_per_year = ifelse(!is.na(tree_density) & !is.na(dbh) & !is.na(a_bg_over30) & !is.na(b_bg_over30) & !is.na(b_bg_below30), 
                                                  ifelse(dbh>29,tree_density*(a_bg_over30+b_bg_over30*dbh)*C_frac_dry*root_turnover_rate,
                                                         tree_density*(b_bg_below30*dbh**2.5)*C_frac_dry*root_turnover_rate),
                                                  ifelse(!is.na(tree_density) & !is.na(forest_biomass_kg) & !is.na(rs_ratio), 
                                                         tree_density*forest_biomass_kg*rs_ratio*C_frac_dry*root_turnover_rate*1e-3,
                                                         paste("Insufficient input data for",tree_species))))
  tC_inputs_per_ha_per_year = sum(as.numeric(trees$tC_inputs_tree_per_ha_per_year))
  return(tC_inputs_per_ha_per_year)
}

### Calculation of pasture input: Carbon input from pasture biomass turnover
# YEARLY
get_monthly_Cinputs_pasture <- function (pasture_inputs, factors_pastures, scenario_chosen, parcel, year, settings){

  pasture <- pasture_inputs %>% filter(scenario==scenario_chosen & parcel_ID==parcel)
  
  if(nrow(pasture)==0){ return(0) }

  annual_factors <- factors_pastures %>% filter(grass == 'Generic grasses', pasture_type == "annual")
  perennial_factors <- factors_pastures %>% filter(grass == 'Generic grasses', pasture_type == "perennial")

  if(year > settings$curr_monit_year & settings$predict_amp_effect) {
    annual_pastures <- 
      merge(x = pasture, y = annual_factors, by = "grass", all.x = TRUE) %>% 
      mutate(c_input_shoot = (dry_residue + dry_grazing * 0.15) * pasture_efficiency * AMP_baseline_factor * dry_c) %>%
      mutate(c_input_root = dry_agb_peak * pasture_efficiency * AMP_baseline_factor * r_s_ratio * dry_c * bg_turnover) %>%
      mutate(c_inputs = c_input_shoot + c_input_root)
    
    perennial_pastures <- 
      merge(x = pasture, y = perennial_factors, by = "grass", all.x = TRUE) %>% 
      mutate(c_input_shoot = (dry_grazing * 0.15 + dry_agb_peak * pasture_efficiency * AMP_baseline_factor * ag_turnover) * dry_c) %>%
      mutate(c_input_root = dry_agb_peak * pasture_efficiency * AMP_baseline_factor * r_s_ratio * dry_c * bg_turnover) %>%
      mutate(c_inputs = c_input_shoot + c_input_root)
  } else {
    annual_pastures <- 
      merge(x = pasture, y = annual_factors, by = "grass", all.x = TRUE) %>% 
      mutate(c_input_shoot = (dry_residue + dry_grazing * 0.15) * dry_c) %>%
      mutate(c_input_root = dry_agb_peak * r_s_ratio * dry_c * bg_turnover) %>%
      mutate(c_inputs = c_input_shoot + c_input_root)
    
    perennial_pastures <- 
      merge(x = pasture, y = perennial_factors, by = "grass", all.x = TRUE) %>% 
      mutate(c_input_shoot = (dry_grazing * 0.15 + dry_agb_peak * ag_turnover) * dry_c) %>%
      mutate(c_input_root = dry_agb_peak * r_s_ratio * dry_c * bg_turnover) %>%
      mutate(c_inputs = c_input_shoot + c_input_root)
  }

  tC_inputs_per_ha_per_year <- sum(perennial_pastures$c_inputs * perennial_pastures$perennial_frac, na.rm=T) +
    sum(annual_pastures$c_inputs * (1 - annual_pastures$perennial_frac), na.rm=T)
  
  return(tC_inputs_per_ha_per_year)
}

### Calculation of crop input: Carbon input from cash crops and cover crops biomass turnover rates
# YEARLY
get_monthly_Cinputs_crop <- function (crop_inputs, factors_crops, scenario_chosen, parcel, farm_EnZ){
  
  crops <- filter(crop_inputs, scenario == scenario_chosen & parcel_ID == parcel)
  
  if(nrow(crops)==0) { return(0) }
  
  factors <- filter(factors_crops, pedo_climatic_area == farm_EnZ | is.na(pedo_climatic_area) == TRUE)

  crops <- merge(x = crops, 
                 y = factors,
                 by = "crop", all.x = TRUE)
  
  crops <- crops %>%
    mutate(c_shoot = dry_residue * dry_c) %>%
    mutate(c_root = dry_agb_peak * dry_c * r_s_ratio) %>%
    mutate(c_inputs = c_shoot * s_turnover + c_root * r_turnover)
  
  tC_inputs_per_ha_per_year = sum(crops$c_inputs)
  
  return(tC_inputs_per_ha_per_year)
}
