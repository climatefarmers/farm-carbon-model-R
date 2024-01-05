# tests


check_animal_data <- function(animal_data, animal_factors){
  
  animals <- unique(animal_factors$species)
  missing_animals <- setdiff(animal_data$species, animals)
  
  if(length(missing_animals) > 0){warning(paste(missing_animals, " not in the database "))}  
    
}

check_crop_data <- function(crop_data, crop_factors){
  
  crops <- unique(crop_factors$crop)
  missing_crops <- setdiff(crop_data$crop, crops)
  
  if(length(missing_crops) > 0){warning(paste(missing_crops, " not in the database "))}  
  
}

check_fertilizer_data <- function(fertilizer_data, fertilizer_factors){
  
  fertilizers <- unique(fertilizer_factors$fertilizer_type)
  missing_fertilizers <- setdiff(fertilizer_data$fertilizer_type, fertilizers)
  
  if(length(missing_fertilizers) > 0){warning(paste(missing_fertilizers, " not in the database "))}  
  
}

check_fuel_data <- function(fuel_data, fuel_factors){
  
  fuels <- unique(fuel_factors$fuel_type)
  missing_fuels <- setdiff(fuel_data$fuel_type, fuels)
  
  if(length(missing_fuels) > 0){warning(paste(missing_fuels, " not in the database "))}  
  
}

check_manure_data <- function(orgamendments_data, manure_factors){
  
  manures <- unique(manure_factors$source)
  missing_manures <- setdiff(orgamendments_data$source, manures)
  
  if(length(missing_manures) > 0){warning(paste(missing_manures, "manure data not in the database "))}  
  
}

check_animal_data_2 <- function(livestock, animal_factors){
  
  year_strings <- paste0("year", 0:10)
  err = 0
  for (y in c(0:10)){
    year_str <- year_strings[y+1]
    if(y == 0) {
      livestock_scenario = livestock[["currentManagement"]][[1]]
    } else {
      livestock_scenario = livestock[["futureManagement"]][[1]][[year_str]] 
    }
    for (k in c(1:nrow(livestock_scenario))){
      species <- livestock_scenario$species[k]
      if (is.na(species)) {next}  # This line should not be needed if data is input correctly. Consider eventual removal.
      if (species %in% animal_factors$species == FALSE) { # This line should not be needed if data input mathces parameter csv
        warning(paste(species, "not in factors table or mispelled."))
        err = err +1
      }
    }
    if (err > 0){stop("Mismatch between animal data input and animal factors. STOP.")}
  }
  
}
