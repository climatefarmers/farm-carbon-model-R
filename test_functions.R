# tests


check_animal_data <- function(animal_data, factors_animals){
  
  animals <- unique(factors_animals$species)
  missing_animals <- setdiff(animal_data$species, animals)
  
  if(length(missing_animals) > 0){warning(paste(missing_animals, " not in the database "))}  
    
}

check_crop_data <- function(crop_data, factors_crops){
  
  crops <- unique(factors_crops$crop)
  missing_crops <- setdiff(crop_data$crop, crops)
  
  if(length(missing_crops) > 0){warning(paste(missing_crops, " not in the database "))}  
  
}

check_fertilizer_data <- function(fertilizer_data, factors_fertilizer){
  
  fertilizers <- unique(factors_fertilizer$fertilizer_type)
  missing_fertilizers <- setdiff(fertilizer_data$fertilizer_type, fertilizers)
  
  if(length(missing_fertilizers) > 0){warning(paste(missing_fertilizers, " not in the database "))}  
  
}

check_fuel_data <- function(fuel_data, factors_fuel){
  
  fuels <- unique(factors_fuel$fuel_type)
  missing_fuels <- setdiff(fuel_data$fuel_type, fuels)
  
  if(length(missing_fuels) > 0){warning(paste(missing_fuels, " not in the database "))}  
  
}

check_manure_data <- function(orgamendments_data, manure_factors){
  
  manures <- unique(manure_factors$source)
  missing_manures <- setdiff(orgamendments_data$source, manures)
  
  if(length(missing_manures) > 0){warning(paste(missing_manures, "manure data not in the database "))}  
  
}
