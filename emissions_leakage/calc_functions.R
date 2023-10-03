# Initial calculations all in kg -> convert to tonnes later

## Calculation: co2 fuel consumption
co2_fuel_consumption <- function(
  fuel_data
){
  if(nrow(fuel_data) > 0){
    fuel_data <- fuel_data %>% 
      mutate(co2_fuel = value_l * ef_fuel_kg_l)
  }else{
    warning("No fuel data provided - or included in project")
  }
  return(fuel_data)
  
}

## Calculation: ch4 enteric fermentation
ch4_enteric_fermentation <- function(
  animal_data
){
  if(nrow(animal_data) > 0){
    animal_data <- animal_data %>% 
      mutate(ch4_ent_ferm = n_animals * (grazing_days / 365) * ef_enteric_fermentation_kg_head) # add non-grazing days
  }else{
    warning("No animal data provided - or included in project")
  }
  return(animal_data)
  
}

ch4_manure <- function(
  animal_data
){
  if(nrow(animal_data) > 0){
    animal_data <- animal_data %>% 
      mutate(ch4_manure = n_animals * grazing_days/365 * ef_methane_kg_head) # add non-grazing days
  }else{
    warning("No animal data provided - or included in project")
  }
  return(animal_data)
  
}

n2o_n_fixing_species_crop <- function(
  n_fixing_species_crop,
  ef_n = 0.01,
  field_area = NA
){
  if(nrow(n_fixing_species_crop) > 0){ # add pastures and update the crop residue calc function
    n_fixing_species_crop <- n_fixing_species_crop %>% 
      mutate(agr = dry_residue, 
             bgr = dry_agb_peak * r_s_ratio,
             n2o_n_fixing = (area * n_fixing_frac * (agr * n_ag + bgr * n_bg)) * ef_n * (44/28) * 1000 # 1000 converts t to kg
             )
  } else {
    warning("No n-fixing crop species data provided - or included in project")
  }
  
  return(n_fixing_species_crop)
}

n2o_n_fixing_species_pasture <- function(
  n_fixing_species_pasture,
  ef_n = 0.01,
  field_area = NA
){
  if(nrow(n_fixing_species_pasture) > 0){
    n_fixing_species_pasture <- n_fixing_species_pasture %>%
    annual_pastures <- filter(n_fixing_species_pasture, pasture_type=="annual") %>% 
      mutate(n_input_shoot= (dry_residual+dry_yield*0.15)*pasture_efficiency*n_ag) %>%
      mutate(n_input_root= pasture_efficiency*ag_dry_peak*r_s_ratio*n_bg*bg_turnover) %>%
      mutate(n2o_n_fixing = area * (n_input_shoot + n_input_root)*n_fixing_frac*(1-perennial_frac)* ef_n * (44/28) * 1000)
    perennial_pastures <- filter(n_fixing_species_pasture, pasture_type=="perennial") %>% 
      mutate(n_input_shoot= (dry_yield*0.15+pasture_efficiency*ag_dry_peak*ag_turnover)*n_ag) %>%
      mutate(n_input_root= pasture_efficiency*ag_dry_peak*r_s_ratio*n_bg*bg_turnover) %>%
      mutate(n2o_n_fixing = area * (n_input_shoot + n_input_root)*n_fixing_frac*perennial_frac* ef_n * (44/28) * 1000)
    n_fixing_species_pasture = rbind(annual_pastures,perennial_pastures)
  }else{
  warning("No n-fixing pasture species data provided - or included in project")
  }
  return(n_fixing_species_pasture)
}



n2o_fertilizer <- function(
  fertilizer_data,
  ef_fertilizer = 0.011
){
  
  if(nrow(fertilizer_data) > 0){
    fertilizer_data <- fertilizer_data %>% 
      mutate(n2o_fertilizer = quantity_t_ha*1e3 *  n_content_perc/100 * (1 - volatile_fraction) * ef_fertilizer * (44/28))
  }else{
    warning("No Fertilizer data provided - or included in project")
  }
  return(fertilizer_data)
  
}

n2o_manure_indirect <- function( # add non-grazing days
  animal_data,
  frac_gasm = 0.21,
  frac_leach = 0.24,
  ef_4 = 0.01,
  ef_5 = 0.011,
  climate_wet_or_dry
){
  if (climate_wet_or_dry == "wet"){ef_4=0.014}
  if (climate_wet_or_dry == "dry"){ef_4=0.005}
  if(nrow(animal_data) > 0){
    animal_data <- animal_data %>% 
      mutate(n2o_manure_indirect = n_animals * n_excretion_rate_kg_1000am * 365 * mass_kg_per_animal / 1000 *
               (frac_gasm * ef_4 + frac_leach * ef_5))
  }else{
    warning("No Animal data provided - or included in project")
  }
  return(animal_data)
}

n2o_manure_direct <- function(
  animal_data,
  ef_3_pasture = 0.004,
  ef_3_deep_bedding=0.01,
  climate_wet_or_dry
){
  if (climate_wet_or_dry == "wet"){ef_3_pasture=0.006}
  if (climate_wet_or_dry == "dry"){ef_3_pasture=0.002}
  if(nrow(animal_data) > 0){
    animal_data <- animal_data %>% 
      mutate(n2o_manure_direct = n_animals * n_excretion_rate_kg_1000am * mass_kg_per_animal / 1000 * 
               (grazing_days * ef_3_pasture + (365 - grazing_days) * ef_3_deep_bedding))
  }else{
    warning("No Animal data provided - or included in project")
  }
  return(animal_data)
}

