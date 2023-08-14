#MongoDB parameters extraction functions

### TOOL FUNCTIONS
## Helper function to convert inputs to numeric
new.as_numeric <- function(input){
  for(i in c(1:length(input))){
    if(is.null(input[i])){
      input[i]=0
    } else if(is.na(input[i])){
      input[i]=0
    } else if(input[i]==""){
      input[i]=0
    }
  }
  return(as.numeric(sub(",", ".", input, fixed = TRUE)))
}

## Helper function to extract latitude
extract_latitude_landUseSummaryOrPractices <- function(landUseSummaryOrPractices, parcel_index = i){
  #takes a landUseSummaryOrPractices from farms collection and the inde corresping to the parcel.
  #extracts the mean latitude of parcel's corners
  latitudes = c()
  for (parcels in landUseSummaryOrPractices[[1]]$coordinates){
    for (i in c(1:nrow(parcels))){
      latitudes <- append(latitudes,parcels[[i,2]])
    }
  }
  return(mean(latitudes))
}

## Helper function to extract longitude
extract_longitude_landUseSummaryOrPractices <- function(landUseSummaryOrPractices, parcel_index = i){
  #takes a landUseSummaryOrPractices from farms collection and a parcel index
  #extracts the mean longitude of parcel's corners
  longitudes = c()
  for (parcels in landUseSummaryOrPractices[[1]]$coordinates){
    for (i in c(1:nrow(parcels))){
      longitudes <- append(longitudes,parcels[[i,1]])
    }
  }
  return(mean(longitudes))
}

## Helper function to calculate animal_inputs: extracts total grazing amount
extract_total_grazing_amount <- function(landUseSummaryOrPractices, year = j, area){
  # Takes a landUseSummaryOrPractices from farms collection
  # Extracts the overall grazing yield and bale grazing yield from the whole farm
  
  year_str <- paste0('year', year)
  bale_grazing <- 0
  grazing <- 0
  
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    
    isBaleGrazing <- landUseSummaryOrPractices[[1]][[year_str]]$baleGrazing[i]
    
    if (is.na(isBaleGrazing) | !bale_grazing) {
      bale_grazing = bale_grazing + 0
    } else if (isBaleGrazing){
      # The following line adds the bale grazing of all parcels after subtracting residues left on field 
      baleResidue <- new.as_numeric(landUseSummaryOrPractices[[1]][[year_str]]$residueLeftAfterBaleGrazing[i]) / 100
      bale_grazing <- bale_grazing + new.as_numeric(landUseSummaryOrPractices[[1]][[year_str]]$hayStrawApplication[i]) *
        (1 - ifelse(is.na(baleResidue), 0.15, baleResidue))
    } 
    
    for (k in c(1:12)){
      monthlyGrazing <- new.as_numeric( landUseSummaryOrPractices[[1]][[year_str]]$grazingYield[i][[1]][[k]] )
      grazing = grazing + monthlyGrazing
    }
    
  }
  
  return((bale_grazing + grazing) * area)
}

## Helper function to calculate animal_inputs: extracts grazing amount per parcel 
extract_grazing_amount_parcel_i <- function(landUseSummaryOrPractices, parcel_index, year, area){
  #takes a landUseSummaryOrPractices from farms collection and a parcel index i
  #extracts grazing yield and bale grazing yield from parcel index
  
  year_str <- paste0('year', year)
  
  isBaleGrazing <- landUseSummaryOrPractices[[1]][[year_str]]$baleGrazing[parcel_index]
  baleResidue <- new.as_numeric(landUseSummaryOrPractices[[1]][[year_str]]$residueLeftAfterBaleGrazing[parcel_index])/100
  hayStraw <- new.as_numeric(landUseSummaryOrPractices[[1]][[year_str]]$hayStrawApplication[parcel_index])
  
  if (is.na(isBaleGrazing) | !isBaleGrazing){ # Fernando: remove? If there is hey input than it cannot be FALSE
    bale_grazing <- 0
  } else if (isBaleGrazing){
    bale_grazing = hayStraw * (1 - ifelse(is.na(baleResidue), 0.15, baleResidue))
  }
  
  grazing = 0
  for (k in c(1:12)){
    monthlyGrazing <- new.as_numeric(landUseSummaryOrPractices[[1]][[year_str]]$grazingYield[parcel_index][[1]][[k]])
    grazing = grazing + monthlyGrazing
  }
  return((bale_grazing + grazing) * area)
}

## Helper function to extract total grazing and bale grazing yield from the whole farm over all years
get_total_grazing_table <- function(landUseSummaryOrPractices, livestock, animal_factors, parcel_inputs){
  
  #takes a landUseSummaryOrPractices from farms collection
  #extracts the overall grazing yield and bale grazing yield from the whole farm
  total_grazing_table = data.frame(
    scenario = c(),
    bale_grazing_total = c(),
    grazing_total = c(),
    grazing_non_arable_lands = c()
    )
  

  for (year in c(0:10)){
    year_str <- paste0('year', year)
    bale_grazing <- 0
    grazing <- 0
    grazing_non_arable_lands <- 0
    
    # Bale grazing from hay application
    for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
      
      isBaleGrazing <- landUseSummaryOrPractices[[1]][[year_str]]$baleGrazing[i]
      baleResidue <- new.as_numeric(landUseSummaryOrPractices[[1]][[year_str]]$residueLeftAfterBaleGrazing[i])/100
      hayStraw <- new.as_numeric(landUseSummaryOrPractices[[1]][[year_str]]$hayStrawApplication[i])
      landUseType <- landUseSummaryOrPractices[[1]][[year_str]]$landUseType[i]
      
      # Add the bale grazing of all parcels after subtracting residues left on field 
      if (is.na(isBaleGrazing) | !bale_grazing) { # Fernando: this condition doesn't make sense. If ther was hay put out, then there was bale grazing.
        bale_grazing <- bale_grazing + 0
      } else if (isBaleGrazing){
        bale_grazing <- bale_grazing + hayStraw * (1 - ifelse(is.na(baleResidue), 0.15, baleResidue)) * parcel_inputs$area[i]
      } 
      
      for (k in c(1:12)){
        monthlyGrazing <- new.as_numeric(landUseSummaryOrPractices[[1]][[year_str]]$grazingYield[i][[1]][[k]])
        # grazing yield from monthly grazing yield data
          grazing <- grazing + monthlyGrazing * parcel_inputs$area[i]
          # grazing yield of non-arable land from monthly grazing yield data
          if (landUseType!="Arablecrops"){
            grazing_non_arable_lands <- 
              grazing_non_arable_lands + monthlyGrazing * parcel_inputs$area[i]
          }
      }
    }
    
    total_grazing_table = rbind(total_grazing_table,data.frame(
      scenario = c(paste("year" ,year, sep="")), 
      bale_grazing_total = c(bale_grazing), 
      grazing_total = c(grazing),
      grazing_non_arable_lands = c(grazing_non_arable_lands)))
  }
  
  # Supposed estimated grazing needs :
  animals = data.frame(scenario = c(), species = c(), n_animals = c(), grazing_days = c())
  status ="currentManagement"
  
  for (k in c(1:nrow(livestock[[status]][[1]]))){
    if (is.na(livestock[[status]][[1]]$species[[k]])){next}
    animals = rbind(animals, data.frame(
      scenario = c("year0"),
      species = c(livestock[[status]][[1]]$species[[k]]),
      n_animals = c(new.as_numeric(livestock[[status]][[1]]$numberOfHeads[[k]])), 
      grazing_days = c(new.as_numeric(livestock[[status]][[1]]$grazingOrPasturedDaysPerYear[[k]]))
    ))
  }
  
  status = "futureManagement"
  for (year in c(1:10)){
    year_str <- paste0('year', year)
    for (k in c(1:nrow(livestock[[status]][[1]][[year_str]]))){
      if (is.na(livestock[[status]][[1]][[year_str]]$species[[k]])){next}
      animals <- rbind(animals, data.frame(
        scenario = year_str, 
        species = c(livestock[[status]][[1]][[year_str]]$species[[k]]),
        n_animals = c(new.as_numeric(livestock[[status]][[1]][[year_str]]$numberOfHeads[[k]])), 
        grazing_days = c(new.as_numeric(livestock[[status]][[1]][[year_str]]$grazingOrPasturedDaysPerYear[[k]]))
      ))
    }
  }
  
  animals = merge(x = animals, y = animal_factors, by = "species", all.x = TRUE)
  animal_needs_table = animals %>%
    mutate(yearly_grazing_needs_tDM = n_animals * mass_kg_per_animal * grazing_days * 0.025 / 1000)
  total_grazing_needs_table = animal_needs_table  %>%
    group_by(scenario) %>%
    summarise(expected_grazing_needs_tDM = sum(yearly_grazing_needs_tDM))
  total_grazing_table = merge(x = total_grazing_table, y = total_grazing_needs_table, by = "scenario", all.x = TRUE) 
  total_grazing_table = total_grazing_table %>%
    mutate(relative_difference_perc = paste(round((grazing_total-expected_grazing_needs_tDM)/expected_grazing_needs_tDM*100),"%"),
           expected_grazing_needs_tDM_pastures=expected_grazing_needs_tDM*grazing_non_arable_lands/grazing_total,
           pasture_weighted_bale_grazing=bale_grazing_total*grazing_non_arable_lands/grazing_total # distributing bale grazing as grazing yields are distributed. May be improved
    )
  
  return(total_grazing_table)
}

# Helper function that extracts crop type per month per parcel:
# In case of crop rotation there can be two different cash crops within one year (cash crop 1 & cash crop 2)
get_monthly_cash_crop <- function(parcel_index = i, year_chosen){
  crop=rep(NA,12)
  for (k in c(1:12)){
    if(!is.na(year_chosen$cashCrop1MonthlyData[[parcel_index]][k]) &
       year_chosen$cashCrop1MonthlyData[[parcel_index]][k] != "-"){ # cash crop 1 checked for month k
      if(!is.na(year_chosen$cashCrop2MonthlyData[[parcel_index]][k]) &
         year_chosen$cashCrop2MonthlyData[[parcel_index]][k] != "-"){ # case of conflict, we assume that the correct is cash crop 1
        log4r::error(my_logger, paste('Two different cash crops checked for month ',k,' in parcel ',landUseSummaryOrPractices[[1]]$parcelName[i],'.',sep='')) # flag
      } else {
        crop[k] = year_chosen$cashCrop1MonthlyData[[parcel_index]][k]
      }
    } else if (!is.na(year_chosen$cashCrop2MonthlyData[[parcel_index]][k]) &
               year_chosen$cashCrop2MonthlyData[[parcel_index]][k] != "-"){
      crop[k] = year_chosen$cashCrop2MonthlyData[[parcel_index]][k]
    } else {
      crop[k] = NA # crop[k] stays NA
    }
  }
  return(crop)
}

# UNDER CONSTRUCTION:
detect_crop_rotations <- function(landUseSummaryOrPractices, parcel_index = i){
  ### Get landUse data for a parcel i
  ### Return a year where rotations starts 
  ### and a number of years of a cycle. 0 means no rotations? 1 means no change in management
  # Listing arablecrop years
  list_arablecrop_years = c()
  for (year in c(0:10)){
    year_str <- paste0('year', year)
    if(landUseSummaryOrPractices[[1]][[year_str]]$landUseType[i]=="Arablecrops"){
      list_arablecrop_years = c(list_arablecrop_years,year)
    }
  }
  # If occurrence of arable crops, looking for crop rotations
  if (length(list_arablecrop_years) > 0){
    df_crops = data.frame(year = c(), crops = c())
    for (year in c(0:10)){
      year_str <- paste0('year', year)
      year_chosen = landUseSummaryOrPractices[[1]][[year_str]]
      df_crops = rbind(df_crops, data.frame(
        year = year,
        crops = unique(na.omit(get_monthly_cash_crop(i, year_chosen)))
      ))
    }
  }
}

## Helper function to get clay content in %
# if soil samples available: farmer's input (%)
# else: soil maps
get_clay_content <- function(soilAnalysis, soilMapsData){
  if (is.null(soilAnalysis$clayContentPercent)){
    return(soilMapsData$clay)
  } else if (soilAnalysis$clayContentPercent==""){
    return(soilMapsData$clay)
  } else { # variable found and a value is provided
    if(5<new.as_numeric(soilAnalysis$clayContentPercent) & new.as_numeric(soilAnalysis$clayContentPercent)<80){ # assumed to be %
      return(new.as_numeric(soilAnalysis$clayContentPercent))
    } else {
      log4r::error(my_logger, paste("Clay content input = ", 
                                    new.as_numeric(soilAnalysis$clayContentPercent),
                                    "%. Check unit/values with farmer.", sep=""))
    }
  }
}

## Helper function to get silt content in %
# if soil samples available: farmer's input (%)
# else: soil maps
get_silt_content <- function(soilAnalysis, soilMapsData){
  if (is.null(soilAnalysis$siltContentPercent)){
    return(soilMapsData$silt)
  } else if (soilAnalysis$siltContentPercent==""){
    return(soilMapsData$silt)
  } else { # variable found and a value is provided
    if(5<new.as_numeric(soilAnalysis$siltContentPercent) & new.as_numeric(soilAnalysis$siltContentPercent)<80){ # assumed to be %
      return(new.as_numeric(soilAnalysis$siltContentPercent))
    } else {
      log4r::error(my_logger, paste("silt content input = ", 
                                    new.as_numeric(soilAnalysis$siltContentPercent),
                                    "%. Check unit/values with farmer.", sep=""))
    }
  }
}

## Helper function to get carbon content in kg/ha?
get_SOC_content <- function(soilAnalysis, soilMapsData){
  if (is.null(soilAnalysis$clayContentPercent) & is.null(soilAnalysis$organicMatterContent)){ #case that SOC & SOM variables weren't found
    return(soilMapsData$SOC)
  }
  if (!is.null(soilAnalysis$carbonContent) & is.null(soilAnalysis$organicMatterContent)){ 
    if (soilAnalysis$organicMatterContent==""){ # case that SOC variable wasn't found and SOM wasn't known
      return(soilMapsData$SOC)
    }
    if(8<new.as_numeric(soilAnalysis$organicMatterContent) & new.as_numeric(soilAnalysis$organicMatterContent)<80 & soilAnalysis$organicMatterContentMetric!="%"){ # SOC in t/ha = g/kg
      return(new.as_numeric(soilAnalysis$organicMatterContent)*0.55)
    } 
    if (0.7<new.as_numeric(soilAnalysis$organicMatterContent) & new.as_numeric(soilAnalysis$organicMatterContent)<8){ #SOC in %
      return(new.as_numeric(soilAnalysis$organicMatterContent)*5.5)
    } else {
      log4r::error(my_logger, paste("OM content input = ", new.as_numeric(soilAnalysis$organicMatterContent),
                                    soilAnalysis$organicMatterContentMetric,
                                    ". Check unit/values with farmer.", sep=""))
    }
  }
  if (soilAnalysis$carbonContent!=""){ # SOC variable exists and a value was entered
    if(4<new.as_numeric(soilAnalysis$carbonContent) & new.as_numeric(soilAnalysis$carbonContent)<40){ # SOC in t/ha = g/kg
      return(new.as_numeric(soilAnalysis$carbonContent))
    } 
    if (0.35<new.as_numeric(soilAnalysis$carbonContent) & new.as_numeric(soilAnalysis$carbonContent)<4){ #SOC in %
      return(new.as_numeric(soilAnalysis$carbonContent)*10)
    } else {
      log4r::error(my_logger, paste("SOC content input = ", new.as_numeric(soilAnalysis$carbonContent),
                                    soilAnalysis$carbonContentMetric,
                                    ". Check unit/values with farmer.", sep=""))
    }
  } else { # SOC variable exists and no value was entered
    return(soilMapsData$SOC)
  }
}


## Helper function to get bulk density
get_bulk_density <- function(soilAnalysis, soilMapsData){
  if (is.null(soilAnalysis$bulkDensity)){
    return(soilMapsData$bulk_density)
  } else if (soilAnalysis$bulkDensity==""){
    return(soilMapsData$bulk_density)
  } else { # variable found and a value is provided
    if(0.7<new.as_numeric(soilAnalysis$bulkDensity) & new.as_numeric(soilAnalysis$bulkDensity)<2){
      return(new.as_numeric(soilAnalysis$bulkDensity))
    } else if (700<new.as_numeric(soilAnalysis$bulkDensity) & new.as_numeric(soilAnalysis$bulkDensity)<2000){
      return(new.as_numeric(soilAnalysis$bulkDensity)*1e-3)
    } else {
      log4r::error(my_logger, paste("bulk density input = ", 
                                    new.as_numeric(soilAnalysis$bulkDensity),
                                    ". Check unit/values with farmer.", sep=""))
    }
  }
}

### GET INPUT FUNCTIONS
get_orgamendments_inputs = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts manure application inputs dataframe 
  orgamendments_inputs = data.frame(parcel_ID = c(), scenario = c(), source = c(), 
                                 quantity_t_ha = c(), imported_frac = c(), remaining_frac = c())
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    for (j in c(0:10)){
      year_str <- paste0('year', j)
      year_chosen = landUseSummaryOrPractices[[1]][[paste('year', j, sep="")]]
      if(!is.na(year_chosen$manureApplication[i])){
        # Manure (animal dung)
        if (year_chosen$manureApplication[i]>=0){
          orgamendments_inputs <- rbind(orgamendments_inputs,data.frame(
            parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
            scenario = c(paste('year', j, sep="")), 
            source = c("Other Cattle"), # AN UNFOLDING LIST OF MANURE TYPE MIGHT HAVE TO BE ADDED TO UI
            quantity_t_ha = c(new.as_numeric(year_chosen$manureApplication[i])), 
            imported_frac = c(ifelse(is.null(year_chosen$percentManureImported[i]),0,
                                     ifelse(is.na(year_chosen$percentManureImported[i]),0,
                                            new.as_numeric(year_chosen$percentManureImported[i])/100))),
            remaining_frac = c(1)))
        }
      }
      # Compost
      if(!is.na(year_chosen$compostApplication[i])){
        if (year_chosen$compostApplication[i]>=0){
          orgamendments_inputs <- rbind(orgamendments_inputs,data.frame(
            parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
            scenario = c(year_str), 
            source = c("Green compost"), # WARNING the fact that compost entry is GREEN compost might have to be specified
            quantity_t_ha = c(new.as_numeric(year_chosen$compostApplication[i])), 
            imported_frac = c(ifelse(is.null(year_chosen$percentCompostImported[i]),0,
                                     ifelse(is.na(year_chosen$percentCompostImported[i]),0,
                                            new.as_numeric(year_chosen$percentCompostImported[i])/100))),
            remaining_frac = c(1)))
        }
      }
      # Hay
      if(!is.na(year_chosen$hayStrawApplication[i])){
        if (year_chosen$hayStrawApplication[i]>=0){
          orgamendments_inputs <- rbind(orgamendments_inputs, data.frame(
            parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
            scenario = c(year_str), 
            source = c("Hay"),
            quantity_t_ha = c(new.as_numeric(year_chosen$hayStrawApplication[i])), 
            imported_frac = c(ifelse(is.null(year_chosen$percentageOfHayStrawImported[i]),0,
                                     ifelse(is.na(year_chosen$percentageOfHayStrawImported[i]),0,
                                            new.as_numeric(year_chosen$percentageOfHayStrawImported[i])/100))),
            remaining_frac = c(ifelse(is.null(year_chosen$baleGrazing[i]), 1, # case were variable isn't found
                                      ifelse(is.na(year_chosen$baleGrazing[i]), 1, # case were variable had no value
                                             ifelse(year_chosen$baleGrazing[i]==TRUE, # case were baleGrazing happens
                                                    ifelse(landUseSummaryOrPractices[[1]][[paste('year', j, sep="")]]$residueLeftAfterBaleGrazing[i]=="10-15", 12.5, # single case hand fix
                                                           new.as_numeric(landUseSummaryOrPractices[[1]][[paste('year', j, sep="")]]$residueLeftAfterBaleGrazing[i]))/100,
                                                    1)))))) #case were no grazing happens meaning it is 100% amended to the soil
        }
      }
    }
  }
  orgamendments_inputs <- rbind(orgamendments_inputs, orgamendments_inputs%>%
                               filter(scenario=='year0')%>%
                               mutate(scenario='baseline')) # Manure addition baseline is based on previous years
  return(orgamendments_inputs)
}

get_agroforestry_inputs = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts agroforestry inputs dataframe 
  agroforestry_inputs = data.frame(parcel_ID = c(), scenario = c(), tree_species = c(), other_name = c(), dbh = c(),
                                   tree_density = c(), area = c())
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    for (j in c(0:10)){
      year_str <- paste0('year', j)
      row_index = 0
      c = c()
      for (tree in landUseSummaryOrPractices[[1]][[year_str]]$typeOfTrees[i][[1]]$treeName){
        row_index = row_index + 1
        if (!is.na(tree)){
          if (tree!=""){ #filter out if no tree information given
            c = append(c, row_index)
          }
        }
      }
      typeOfTrees = landUseSummaryOrPractices[[1]][[year_str]]$typeOfTrees[i][[1]][c,]
      if(nrow(typeOfTrees)>0){
        for (k in c(1:nrow(typeOfTrees))){
          agroforestry_inputs <- rbind(agroforestry_inputs,data.frame(
            parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
            scenario = c(year_str), 
            tree_species = c(typeOfTrees$treeName[[k]]),
            other_name = c(typeOfTrees$otherTreeName[[k]]),
            dbh = c(new.as_numeric(typeOfTrees$treeAvgDBH[[k]])), 
            tree_density = c(new.as_numeric(typeOfTrees$avgNoOfTrees[[k]])), 
            area = c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000)))
        }
        if (j==0){ #baseline based on pre-project trees
          for (k in c(1:nrow(typeOfTrees))){
            agroforestry_inputs <- rbind(agroforestry_inputs,data.frame(
              parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
              scenario = c("baseline"), 
              tree_species = c(typeOfTrees$treeName[[k]]),
              other_name = c(typeOfTrees$otherTreeName[[k]]),
              dbh = c(new.as_numeric(typeOfTrees$treeAvgDBH[[k]])), 
              tree_density = c(new.as_numeric(typeOfTrees$avgNoOfTrees[[k]])), 
              area = c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000)))
          }
        }
      }
    }
  }
  NA_rows = nrow(agroforestry_inputs)-nrow(na.omit(agroforestry_inputs))
  if(NA_rows>0){
    log4r::error(my_logger, paste('WARNING: ',NA_rows,' rows contained NAs in agroforestry_inputs.', paste=''))
  }
  return(na.omit(agroforestry_inputs))
}

get_animal_inputs = function(landUseSummaryOrPractices,livestock, parcel_inputs){
  # takes landUseSummaryOrPractices & livestock from farms collection
  # extracts animal inputs dataframe 
  animal_inputs = data.frame(parcel_ID = c(), scenario = c(), species = c(), n_animals = c(), grazing_days = c(),
                             area = c(), grazing_management = c(), productivity = c())
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    status ="currentManagement"
    for (k in c(1:nrow(livestock[[status]][[1]]))){
      if (is.na(livestock[[status]][[1]]$species[[k]])){next}
      animal_inputs <- rbind(animal_inputs,data.frame(
        parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
        scenario = c(paste('year',0,sep="")), 
        species = c(livestock[[status]][[1]]$species[[k]]),
        # n_animal is the total number of animal from a farm weighted by grazing yield fraction of the parcel
        n_animals = c(ifelse(extract_total_grazing_amount(landUseSummaryOrPractices,0,parcel_inputs$area[i])==0,0,
                             new.as_numeric(livestock[[status]][[1]]$numberOfHeads[[k]])*
                               extract_grazing_amount_parcel_i(landUseSummaryOrPractices,i,0,parcel_inputs$area[i])/
                               extract_total_grazing_amount(landUseSummaryOrPractices,0,parcel_inputs$area[i]))), 
        grazing_days = c(new.as_numeric(livestock[[status]][[1]]$grazingOrPasturedDaysPerYear[[k]])), 
        area = c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000), #WARNING, SHOULD TAKE FROM PARCEL INPUT DIRECTLY
        grazing_management = c("Daily Spread"), 
        productivity = c("Low Productivity"))) # WARNING, NEEDED FOR LCA SHOULD PRODUCTIVITY INFO COMES FROM FARMER OR DEDUCED FROM MANAGEMENT?
    }
    # baseline based on year 0 livestock
    for (k in c(1:nrow(livestock[[status]][[1]]))){
      if (is.na(livestock[[status]][[1]]$species[[k]])){next}
      animal_inputs <- rbind(animal_inputs,data.frame(
        parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
        scenario = c("baseline"), 
        species = c(livestock[[status]][[1]]$species[[k]]),
        # n_animal is the total number of animal from a farm weighted by grazing yield fraction of the parcel
        n_animals = c(ifelse(extract_total_grazing_amount(landUseSummaryOrPractices,0,parcel_inputs$area[i])==0,0,
                             new.as_numeric(livestock[[status]][[1]]$numberOfHeads[[k]])*
                               extract_grazing_amount_parcel_i(landUseSummaryOrPractices,i,0,parcel_inputs$area[i])/
                               extract_total_grazing_amount(landUseSummaryOrPractices,0,parcel_inputs$area[i]))), 
        grazing_days = c(new.as_numeric(livestock[[status]][[1]]$grazingOrPasturedDaysPerYear[[k]])), 
        area = c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000),
        grazing_management = c("Daily Spread"), 
        productivity = c("Low Productivity"))) # WARNING, SHOULD PRODUCTIVITY INFO COMES FROM FARMER OR DEDUCED FROM MANAGEMENT?
    }
    status = "futureManagement"
    for (year in c(1:10)){
      year_str <- paste0('year', year)
      scenario = c(year_str)
      for (k in c(1:nrow(livestock[[status]][[1]][[scenario]]))){
        if (is.na(livestock[[status]][[1]][[scenario]]$species[[k]])){next}
        animal_inputs <- rbind(animal_inputs,data.frame(
          parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
          scenario = scenario, 
          species = c(livestock[[status]][[1]][[scenario]]$species[[k]]),
          # n_animal is the total number of animal from a farm weighted by grazing yield fraction of the parcel
          n_animals = c(ifelse(extract_total_grazing_amount(landUseSummaryOrPractices,year,parcel_inputs$area[i])==0,0,
                               new.as_numeric(livestock[[status]][[1]][[scenario]]$numberOfHeads[[k]])*
                                 extract_grazing_amount_parcel_i(landUseSummaryOrPractices,i,year,parcel_inputs$area[i])/
                                 extract_total_grazing_amount(landUseSummaryOrPractices,year,parcel_inputs$area[i]))), 
          grazing_days = c(new.as_numeric(livestock[[status]][[1]][[scenario]]$grazingOrPasturedDaysPerYear[[k]])), 
          area = c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000),
          grazing_management = c("Daily Spread"), 
          productivity = c("Low Productivity"))) # WARNING, SHOULD PRODUCTIVITY INFO COMES FROM FARMER OR DEDUCED FROM MANAGEMENT?
      }
    }
  }
  return(animal_inputs)
}

get_bare_field_inputs = function(landUseSummaryOrPractices, soil_cover_data, farm_EnZ){
  # takes landUseSummaryOrPractices from farms collection
  # extracts bare soil inputs dataframe 
  bare_field_inputs = data.frame(parcel_ID = c(), scenario = c())
  # one column per month
  for (k in c(1:12)){
    bare_field_inputs[[paste("bare_profile_", k, sep="")]] = c()
  }
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    bare_field_inputs_temp <- data.frame(parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
                                         scenario = c('baseline'))
    bare_field_inputs <- rbind(bare_field_inputs, cbind(bare_field_inputs_temp, soil_cover_data %>% filter(pedo_climatic_area == farm_EnZ) %>%
                                                          select(-country,-pedo_climatic_area)))
    for (j in c(0:10)){
      year_chosen = landUseSummaryOrPractices[[1]][[paste('year', j, sep="")]]
      bare_field_inputs_temp = data.frame(parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
                                          scenario = c(paste('year', j, sep="")))
      for (k in c(1:12)){
        bare_field_inputs_temp[[paste("bare_profile_", k, sep="")]] = ifelse(
          year_chosen$bareSoilFallow[[1]][[k]]==TRUE, TRUE, FALSE)
      }
      bare_field_inputs <- rbind(bare_field_inputs, bare_field_inputs_temp)
    }
  }
  return(bare_field_inputs)
}

get_crop_inputs <- function(landUseSummaryOrPractices, parcel_inputs, crop_factors, get_grazing_estimates){

  crop_inputs = data.frame(scenario = c(), parcel_ID = c(), crop = c(), harvest = c(), 
                           grazing = c(), residue = c(),  agb_peak = c())
  for (j in c(0:10)){ # years
    year_str <- paste0('year', j)
    year_chosen = landUseSummaryOrPractices[[1]][[year_str]]
    for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
      # we exclude holistic grazing compatible land-uses (no pasture efficiency coef will be used in crop inputs)
      if (year_chosen$landUseType[i]=="Arablecrops"){
        # creating the data frame storing monthly yield and residue
        monthly_harvest = data.frame(crop=logical(12), 
                                           coverCrop=logical(12), 
                                           productiveFallow=logical(12),
                                           harvest=logical(12),
                                           residue=logical(12))
        # getting actual data
        monthly_harvest$crop = get_monthly_cash_crop(parcel_index = i, year_chosen)
        monthly_harvest$coverCrop = year_chosen$coverCropMonthlyData[[i]]
        monthly_harvest$productiveFallow = year_chosen$productiveFallow[[i]]
        monthly_harvest$grazing = new.as_numeric(year_chosen$grazingYield[[i]])
        monthly_harvest$harvest = new.as_numeric(year_chosen$harvestYield[[i]])
        monthly_harvest$residue = new.as_numeric(year_chosen$estimationAfterResidueGrazingHarvest[[i]])
        
        # If get_grazing_estimates is TRUE, total grazing yield is calculated here, replacing input values. Recommended.
        if (get_grazing_estimates){
          grazing_table_temp = total_grazing_table %>% filter(scenario==year_str)
          if (grazing_table_temp$bale_grazing_total>grazing_table_temp$expected_grazing_needs_tDM){
            log4r::error(my_logger,"WARNING ! Bale grazing alone exceeds expected grazing needs, to be checked.")
            stop("Error related to bale grazing numbers. See log file..")
          }
          if (grazing_table_temp$grazing_total==0){
            # grazing arbitrarily equally distributed over grazed land, 2 month a year (6 months apart) if no grazing yield announced
            # The distribution over 2 months is meant to reflect that grazing over a particular parcel happens a couple of times a year.
            # This probably doesn't affect the model calculations.
            yearly_grazing_per_ha = (grazing_table_temp$expected_grazing_needs_tDM - grazing_table_temp$bale_grazing_total)/sum(parcel_inputs$area) 
            monthly_harvest$grazing <- c(1/2 * yearly_grazing_per_ha, rep(0,5), 1/2 * yearly_grazing_per_ha, rep(0,5))
          } else {
            # grazing arbitrarily equally distributed over time weighted by parcel grazing yield relatively to farm level, if known
            expected_grazing_on_arable <- ((grazing_table_temp$expected_grazing_needs_tDM - grazing_table_temp$expected_grazing_needs_tDM_pastures) - 
                                             (grazing_table_temp$bale_grazing_total - grazing_table_temp$pasture_weighted_bale_grazing)) # expected grazing yield arable lands after deduction of bale grazing distributed in arable lands 
            grazing_on_parcel_fraction <- (sum(monthly_harvest$grazing) * parcel_inputs$area[i]) / (grazing_table_temp$grazing_total - grazing_table_temp$grazing_non_arable_lands)
            yearly_grazing_per_ha <- expected_grazing_on_arable * grazing_on_parcel_fraction / parcel_inputs$area[i] # Grazing yield arable lands
            
            monthly_harvest$grazing <- c(1/2 * yearly_grazing_per_ha, rep(0,5), 1/2 * yearly_grazing_per_ha, rep(0,5))
          }
        }

        # Check if input values are fresh or dry.
        # Note: the code is not working with fresh plant inputs! Dry fraction in crop factors was wrongly implemented. Air-dry/harvest-dry values should be submitted.
        if (is.na(year_chosen$yieldsResiduesDryOrFresh[i])){
          log4r::info(my_logger, paste("WARNING: dryOrFresh is NA in parcel ",landUseSummaryOrPractices[[1]]$parcelName[i],
                                       " for year ",j,". Assumed to be dry.", sep=""))
        }
        if(year_chosen$yieldsResiduesDryOrFresh[i]=="Fresh") {
          stop("Code not working with fresh plant inputs! Dry fraction in crop factors unrealistic!")
        }
        
        # case of cash crop with no grazing
        for (crop_chosen in unique(monthly_harvest$crop)){
          
          if(!is.na(crop_chosen)) {
            crop_monthly <- monthly_harvest %>% filter(crop==crop_chosen)
            yield_sums <- crop_monthly$harvest + crop_monthly$grazing + crop_monthly$residue
            harvest = sum((crop_monthly)$harvest)
            grazing = sum(new.as_numeric((crop_monthly)$grazing))
            residue = sum((crop_monthly)$residue)
            crop_inputs_temp <-data.frame(scenario = c(year_str),
                                          parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]),
                                          crop = crop_chosen,
                                          harvest = harvest, 
                                          grazing = grazing, 
                                          residue = residue+grazing * 0.15, 
                                          agb_peak = max(yield_sums)
            )
            crop_inputs <- rbind(crop_inputs, crop_inputs_temp)
          } else { # crop_chosen = NA, meaning no cash crop
            other_monthly <- monthly_harvest %>% filter(is.na(crop))
            yield_sum <- other_monthly$harvest + other_monthly$grazing + other_monthly$residue
            harvest = sum(other_monthly$harvest)
            grazing = sum(new.as_numeric((other_monthly)$grazing))
            residue = sum((other_monthly)$residue)
            crop_inputs_temp <- data.frame(scenario = c(year_str),
                                           parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]),
                                           # assumed to be "generic grass" if no cash crop
                                           crop = "Non-N-fixing dry forages",# SHOULD WE DIFFERENTIATE PRODUCTIVE FALLOW AND COVER CROPS ?
                                           harvest = harvest, 
                                           grazing = grazing, 
                                           residue = residue+grazing * 0.15, 
                                           agb_peak = max(yield_sum)
            )
            crop_inputs <- rbind(crop_inputs, crop_inputs_temp)
          }
        }
      }
    }
  }
  
  # Merge with dry factor by crop and correct for moisture content
  crop_inputs <- merge(x = crop_inputs, y = crop_factors %>% select(crop, dry), by = "crop", all.x = TRUE)
  crop_inputs$dry[is.na(crop_inputs$dry)] <- crop_factors$dry[crop_factors$crop == "Other"] # Use option 'Other' if no crop match was found.
  crop_inputs <- crop_inputs %>% mutate(
    dry_harvest = harvest * dry,
    dry_grazing = grazing * dry,
    dry_residue = residue * dry,
    dry_agb_peak = agb_peak * dry
    )
  
  # Set baseline to be equal to year0
  crop_inputs <- rbind(crop_inputs, crop_inputs %>% filter(scenario=='year0') %>% mutate(scenario='baseline'))
  
  return(crop_inputs)
}


## The commented function below was written by Jeremie to set a baseline using common practices data.
## Not used because it was decided it introduces too much uncertainty.
# get_baseline_crop_inputs <- function(landUseSummaryOrPractices, crop_inputs, crop_data, my_logger, farm_EnZ){
#   
#   if (nrow(crop_inputs)==0){ # no crops previously found
#     return(crop_inputs) # so no crop baselines to be created, returned empty
#   }
#   for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
#     #ONLY ARABLECROPS LANDUSE TYPE IS CURRENTLY CONSIDERED IN get_crop_inputs
#     #SHOULD BE REFINED IN CASE OF CASH CROPS UNDER AGROFORESTRY
#     # if(landUseSummaryOrPractices[[1]][['year0']]$landUseType[i]=="Agroforestry" |
#     #    landUseSummaryOrPractices[[1]][['year0']]$landUseType[i]=="Forestry" ){
#     #   # We assume that for the above land uses soil cover management baseline is the current state
#     #   crop_inputs <- rbind(crop_inputs,crop_inputs%>%
#     #                          filter(parcel_ID==landUseSummaryOrPractices[[1]]$parcelName[i], scenario=='year0')%>%
#     #                          mutate(scenario='baseline')) # arable crop baseline is based on previous years
#     # }
#     if(landUseSummaryOrPractices[[1]][['year0']]$landUseType[i]=="Arablecrops"){
#       # AT THE MOMENT PERMANENT COVER CROPS ARE ALSO ASSOCIATED TO CEREAL-BASELINE
#       if(landUseSummaryOrPractices[[1]]$year0$applyingThesePracticesInYears[i]==""){
#         log4r::error(my_logger,"No value found for 'applyingThesePracticesInYears'.")
#         stop("No value found for 'applyingThesePracticesInYears'.")
#       } else if (new.as_numeric(landUseSummaryOrPractices[[1]]$year0$applyingThesePracticesInYears[i])>3){
#         # choice that if an arable crop has been run for more than 3 years in a way, this way must be the baseline
#         crop_inputs <- rbind(crop_inputs,crop_inputs%>%
#                                filter(parcel_ID==landUseSummaryOrPractices[[1]]$parcelName[i], scenario=='year0')%>%
#                                mutate(scenario='baseline')) # arable crop baseline is based on previous years
#       } else {
#         # If 3 years or less, assume common practices. Use provided wheat yield data if given. Else take from factors table.
#         if(nrow(crop_inputs %>% filter(crop=='Wheat' | crop=='Winter wheat' | crop=='Spring wheat'))>0){#if we have wheat data from the farmer
#           crop_inputs_temp <- crop_inputs %>% filter(crop=='Wheat' | crop=='Winter wheat' | crop=='Spring wheat') %>%
#             summarize(parcel_ID=landUseSummaryOrPractices[[1]]$parcelName[i], scenario='baseline',
#                       crop = 'Wheat',
#                       harvest=mean(dry_agb_peak)*0.95, fresh = mean(fresh_agb_peak)*0.95,
#                       dry_grazing=0, fresh_grazing=0,
#                       dry_residue=mean(dry_agb_peak)*0.05, fresh_residue=mean(fresh_agb_peak)*0.05, #assumption that only 5% of aboveground biomass  is left-on-site
#                       dry_agb_peak=mean(dry_agb_peak), fresh_agb_peak=mean(fresh_agb_peak))
#           crop_inputs <- rbind(crop_inputs, crop_inputs_temp)
#         } else {
#           # if no wheat yield data is provided by the farmer
#           dry_agb_peak = (crop_data %>% filter(pedo_climatic_area==farm_EnZ))$ag_dm_peak
#           crop_inputs_temp <- data.frame(parcel_ID=landUseSummaryOrPractices[[1]]$parcelName[i], scenario='baseline',
#                                          crop = 'Wheat',
#                                          harvest=mean(dry_agb_peak)*0.95, fresh = 0,
#                                          dry_grazing=0, fresh_grazing=0,
#                                          dry_residue=mean(dry_agb_peak)*0.05, fresh_residue=0, #assumption that only 5% of aboveground biomass is left-on-site
#                                          dry_agb_peak=mean(dry_agb_peak), fresh_agb_peak=0)
#           crop_inputs <- rbind(crop_inputs, crop_inputs_temp)
#         }
#       }
#     }
#   }
#   
#   return(crop_inputs)
# }


get_fertilizer_inputs = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts fertilizer inputs dataframe 
  fertilizer_inputs = data.frame(parcel_ID = c(), field_area = c(), scenario = c(), usage_boolean=c(), 
                                 fertilizer_type=c(), quantity_t_ha=c(), n_content_perc=c())
  list_missing_data = c()
  
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    for (j in c(0:10)){
      
      parcel_id <- landUseSummaryOrPractices[[1]]$parcelName[i]
      year_str <- paste0('year',j)
      
      # Determine the field area depending on input source
      use_manual_area <- landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i]
      if(is.na(use_manual_area) |  is.null(use_manual_area) | !use_manual_area) {
        field_area <- new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000 # if no corrected value was provided by the farmer
      } else {
        field_area <- new.as_numeric(landUseSummaryOrPractices[[1]]$manuallyEnteredArea[i])/10000 # add a verification of consistency here?
      }
      
      year_chosen = landUseSummaryOrPractices[[1]][[year_str]]
      
      fertilizer_temp <- data.frame(
        parcel_ID = parcel_id, 
        field_area = field_area,
        scenario = year_str,
        usage_boolean = year_chosen$syntheticFertilizer$usage[i],
        fertilizer_type = "synthetic", # here gathering data from the synthetic fertilizer dashboard entry
        quantity_t_ha = ifelse(year_chosen$syntheticFertilizer$usage[i]==TRUE, new.as_numeric(year_chosen$syntheticFertilizer$tonsPerYear[i]),0),
        n_content_perc=ifelse(year_chosen$syntheticFertilizer$usage[i]==TRUE, new.as_numeric(year_chosen$syntheticFertilizer$percentOfNitrogen[i]),0)
      )
      fertilizer_inputs <- rbind(fertilizer_inputs, fertilizer_temp)
      
      if (j==0){
        fertilizer_inputs <- rbind(fertilizer_inputs,data.frame(
          parcel_ID = parcel_id, 
          field_area = ifelse(is.null(landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i]),
                              c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000),
                              ifelse(is.na(landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i]) |
                                       landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i] == FALSE, # means that no corrected value was provided by the farmer
                                     c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000),
                                     c(new.as_numeric(landUseSummaryOrPractices[[1]]$manuallyEnteredArea[i])/10000))), # add a verification of consistence here
          scenario = c("baseline"),
          usage_boolean = year_chosen$syntheticFertilizer$usage[i],
          fertilizer_type = "synthetic", # here gathering data from the synthetic fertilizer dashboard entry
          quantity_t_ha = ifelse(year_chosen$syntheticFertilizer$usage[i]==TRUE, new.as_numeric(year_chosen$syntheticFertilizer$tonsPerYear[i]),0),
          n_content_perc=ifelse(year_chosen$syntheticFertilizer$usage[i]==TRUE, new.as_numeric(year_chosen$syntheticFertilizer$percentOfNitrogen[i]),0)))
      }
      last_index = nrow(fertilizer_inputs)
      if (fertilizer_inputs$usage_boolean[last_index]==TRUE){
        if (fertilizer_inputs$quantity_t_ha[last_index]==0){
          list_missing_data = c(list_missing_data,paste(fertilizer_inputs$parcel_ID[last_index],
                                                        ' (year',j,"): quantity_t_ha missing", sep=""))
        }
        if (fertilizer_inputs$n_content_perc[last_index]==0){
          list_missing_data = c(list_missing_data,paste(fertilizer_inputs$parcel_ID[last_index],
                                                        ' (year',j,"): n_content_perc missing", sep=""))
        }
      }
    }
  }
  if (length(list_missing_data)>0){
    log4r::error(my_logger, paste('WARNING: Fertilizer data: ',list(list_missing_data),'.', paste=''))
  }
  return(fertilizer_inputs)
}

get_fuel_inputs = function(fuel){
  # extracts fuel inputs dataframe 
  fuel_inputs = data.frame(scenario = c(), typeOfFuel = c(), amountInLiters = c())
  status ="currentFuelUsage"
  for (k in c(1:nrow(fuel[[status]][[1]]))){
    if (is.na(fuel[[status]][[1]]$typeOfFuel[[k]]) | fuel[[status]][[1]]$typeOfFuel[[k]]==""){next}
    fuel_inputs <- rbind(fuel_inputs,data.frame(
      scenario = c(paste('year',0,sep="")), 
      fuel_type = fuel[[status]][[1]]$typeOfFuel[[k]],
      value_l = new.as_numeric(fuel[[status]][[1]]$amountInLiters[[k]])))
    fuel_inputs <- rbind(fuel_inputs,data.frame(
      scenario = c("baseline"), 
      fuel_type = fuel[[status]][[1]]$typeOfFuel[[k]],
      value_l = new.as_numeric(fuel[[status]][[1]]$amountInLiters[[k]])))
  }
  status = "futureFuelUsage"
  for (year in c(1:10)){
    year_str <- paste0('year', year)
    for (k in c(1:nrow(fuel[[status]][[1]]))){
      if (is.na(fuel[[status]][[1]]$typeOfFuel[[k]]) | fuel[[status]][[1]]$typeOfFuel[[k]]==""){next}
      fuel_inputs <- rbind(fuel_inputs,data.frame(
        scenario = c(year_str), 
        fuel_type = fuel[[status]][[1]]$typeOfFuel[[k]],
        value_l = new.as_numeric(fuel[[status]][[1]]$amountInLiters[[k]])))
    }
  }
  return(fuel_inputs)
}

## Helper function to extract land use type (not used!)
get_land_use_type <- function(landUseSummaryOrPractices, parcel_inputs){
  landUseType = data.frame(parcel_ID = c(), area = c(), uniqueLandUseType_Yes_No = c(), landUseType = c())
  temp_df=data.frame(landUseType_year0 = rep("-",nrow(parcel_inputs)))
  for (j in c(0:10)){
    year_str <- paste0('year', j)
    temp_df[[paste("landUseType_year",j,sep="")]]=landUseSummaryOrPractices[[1]][[year_str]]$landUseType
  }
  temp_df = data.frame(t(temp_df))
  colnames(temp_df) = landUseSummaryOrPractices[[1]]$parcelName
  for (i in c(1:nrow(parcel_inputs))){
    landUseType = rbind(landUseType, data.frame(
      parcel_ID = parcel_inputs$parcel_ID[i], 
      area = parcel_inputs$area[i],
      uniqueLandUseType_Yes_No = ifelse(nrow(unique(temp_df[i]))==1, TRUE, FALSE),
      landUseType = ifelse(nrow(unique(temp_df[i]))==1, as.character(unique(temp_df[i])),
                           as.character(temp_df[i]))
    ))
  }
  return(landUseType)
}

get_parcel_inputs <- function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts parcels input dataframe 
  
  parcel_inputs = data.frame(parcel_ID = c(), area = c(), longitude = c(),latitude=c())
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    parcel_inputs <- rbind(parcel_inputs,data.frame(
      parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
      area = ifelse(is.null(landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i]),
                    c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000),
                    ifelse(is.na(landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i]) |
                             landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i] == FALSE, # means that no corrected value was provided by the farmer
                           c(new.as_numeric(landUseSummaryOrPractices[[1]]$area[i])/10000),
                           c(new.as_numeric(landUseSummaryOrPractices[[1]]$manuallyEnteredArea[i])/10000))), # add a verification of consistence here
      longitude = c(new.as_numeric(extract_longitude_landUseSummaryOrPractices(landUseSummaryOrPractices,i))),
      latitude=c(new.as_numeric(extract_latitude_landUseSummaryOrPractices(landUseSummaryOrPractices,i)))))
    
  }
  return(parcel_inputs)
}

get_pasture_inputs <- function(landUseSummaryOrPractices, grazing_factors, pasture_factors, farm_EnZ, total_grazing_table, my_logger, parcel_inputs, get_grazing_estimates){
  # Takes landUseSummaryOrPractices from farms collection
  # Extracts yield and residues left on site when grazing happened
  pasture_efficiency_potential_difference = unique(
    (grazing_factors %>% filter(pedo_climatic_area==farm_EnZ))$pasture_efficiency_potential_difference
  )
  
  dw_dry <- (pasture_factors %>% filter(grass == 'Generic annual grasses') %>% select(dw_dry))[[1]]
  dw_fresh <- (pasture_factors %>% filter(grass == 'Generic annual grasses') %>% select(dw_fresh))[[1]]
  
  pasture_inputs = data.frame(
    scenario = c(), parcel_ID = c(), grass = c(), perennial_frac = c(), 
    n_fixing_frac = c(), grazing = c(), residue = c(),
    harvest = c(), agb_peak = c(), dry_grazing = c(), dry_residue = c(),
    dry_harvest = c(), dry_agb_peak = c(), pasture_efficiency = c()
  )
  
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    
    year0_is_AMP <- landUseSummaryOrPractices[[1]][['year0']]$adaptiveMultiPaddockGrazing[i]
    if(is.na(year0_is_AMP)) {year0_is_AMP <- FALSE} # Workaround if value is missing. Should not be allowed. To be enforced at data collection.
    
    year0_since_years <- landUseSummaryOrPractices[[1]][['year0']]$applyingThesePracticesInYears[i]
    
    if(year0_since_years==""){
      log4r::error(my_logger, "Number of years that practices have been applied until now is NOT entered.")
    } else {
      baseline_since_years = new.as_numeric(year0_since_years)
    }
    if(year0_is_AMP){
      AMP_years_baseline <- baseline_since_years
      AMP_years_current <- AMP_years_baseline
    } else {
      AMP_years_baseline <- 0
      AMP_years_current <- 0
    }
    
    years_lost_by_till = 3
    years_lost_by_mintill = 1
    
    for (j in c(0:10)){
      year_str <- paste0('year', j)
      year_chosen <- landUseSummaryOrPractices[[1]][[year_str]]
      year_is_AMP <- landUseSummaryOrPractices[[1]][[year_str]]$adaptiveMultiPaddockGrazing[i]
      if(is.na(year_is_AMP)) {year_is_AMP <- FALSE} # Workaround if value is missing. Should not be allowed. To be enforced at data collection.
      
      if(j>0) { # Only apply to project years
        # Counting AMP years to calculate related efficiency
        # Efficiency is assumed to be reversible. If AMP is not happening, efficiency will go backward.
        # AMP related productivity benefits get penalized in case of till or minimum till
        till=unlist(year_chosen$tillingEvent[i])
        minTill=unlist(year_chosen$minimumTillingEvent[i])
        if(sum(till) > 0) { # in case of conventional tillage over AMP grassland 
          AMP_years_current <- AMP_years_current - years_lost_by_till
        } else if(sum(minTill) > 0) { # in case of minimum tillage over AMP grassland
          AMP_years_current <- AMP_years_current - years_lost_by_mintill
        } else {
          if(year_is_AMP) {
            AMP_years_current <- AMP_years_current + 1
          } else {
            AMP_years_current <- AMP_years_current - 1
          }
        }
        if(AMP_years_current < 0) {AMP_years_current <- 0}
      }
      
      # Calculation of pasture_efficiency: an index of enhanced productivity due to AMP grazing
      # Efficiency increases with time towards a plateau.
      # 0.36 factor allows to reach 2/3 of potential efficiency after 3 years of AMP
      pasture_efficiency <- 1 + pasture_efficiency_potential_difference *
        (1-exp(-0.36*AMP_years_current))
      AMP_baseline_factor <- 1 /  # This is used later to get the baseline productivity if AMP started in the past.
        (1 + pasture_efficiency_potential_difference * (1-exp(-0.36*AMP_years_baseline)))
      
      # Selecting the type of land use where grazing management affects pasture efficiency the most
      # Monthly yield and residue (to avoid double-counting we will only look at grasslands) Fernando: the condition below includes also woody crops.
      if (year_chosen$landUseType[i]!='Arablecrops'){
        
        # monthly yield and residue
        monthly_nonarables <- data.frame(grazing=rep(0,12),residue=rep(0,12))
        for (k in c(1:12)){
          monthly_nonarables$grazing[k] <- new.as_numeric(year_chosen$grazingYield[i][[1]][[k]])
          monthly_nonarables$residue[k] <- new.as_numeric(year_chosen$estimationAfterResidueGrazingHarvest[i][[1]][[k]])
          monthly_nonarables$harvest[k] <- new.as_numeric(year_chosen$harvestYield[i][[1]][[k]])
        }
        
        # If get_grazing_estimates is TRUE total grazing yield is calculated here, replacing reported values.
        if (get_grazing_estimates){
          grazing_table_temp = total_grazing_table %>% filter(scenario==paste0('year', j))
          if (grazing_table_temp$bale_grazing_total > grazing_table_temp$expected_grazing_needs_tDM){
            log4r::error(my_logger,"WARNING ! Bale grazing alone overcomes expected grazing needs, to be checked.")
            stop("Error related to bale grazing numbers. See log file..")
          }
          if (grazing_table_temp$grazing_total == 0){
            yearly_grazing_per_ha = (grazing_table_temp$expected_grazing_needs_tDM - grazing_table_temp$bale_grazing_total) / sum(parcel_inputs$area)
            monthly_nonarables$grazing <- c(1/2 *yearly_grazing_per_ha, rep(0,5), 1/2 * yearly_grazing_per_ha, rep(0,5))
          } else {
            # grazing arbitrarily equally distributed over time weighted by parcel grazing yield relatively to farm level, if known
            expected_grazing_on_pasture <- (grazing_table_temp$expected_grazing_needs_tDM_pastures - grazing_table_temp$pasture_weighted_bale_grazing)
            grazing_on_parcel_fraction <- sum(monthly_nonarables$grazing) * parcel_inputs$area[i] / grazing_table_temp$grazing_non_arable_lands
            yearly_grazing_per_ha <-  expected_grazing_on_pasture * grazing_on_parcel_fraction / parcel_inputs$area[i]
            
            monthly_nonarables$grazing <- c(1/2 * yearly_grazing_per_ha, rep(0,5), 1/2 * yearly_grazing_per_ha, rep(0,5))
          }
        }

        ### building df for C inputs calculation
        
        yield_sums <- monthly_nonarables$grazing+monthly_nonarables$residue+monthly_nonarables$harvest
        
        if (farm_EnZ == "Mediterranean north" | farm_EnZ == "Mediterranean south"){ # env zones with 2 grass growing seasons
          endWinterSeason = 5 # month index
          endSummerSeason = 10 # month index
          agb_peak <- max((yield_sums)[c(1:endWinterSeason,endSummerSeason:12)]) +
            max((yield_sums)[c(endWinterSeason:endSummerSeason)])
        } else { # assuming a single growing season
          agb_peak <- max(yield_sums)
        }
        
        pasture_df_temp <- data.frame(scenario = c(paste0('year',j)),
                                      parcel_ID = landUseSummaryOrPractices[[1]]$parcelName[i], 
                                      grass = "Generic grasses",
                                      perennial_frac = AMP_years_current * 0.02,
                                      n_fixing_frac = 0, # WARNING: TO BE AUTOMATED FOR CO2-EMISSION BALANCE 
                                      grazing = sum(monthly_nonarables$grazing), 
                                      residue = sum(monthly_nonarables$residue), 
                                      harvest = sum(monthly_nonarables$harvest),
                                      agb_peak = agb_peak, 
                                      pasture_efficiency = pasture_efficiency,
                                      AMP_baseline_factor = AMP_baseline_factor
        )

        # Get fresh or dry value.
        dryOrFresh <- year_chosen$yieldsResiduesDryOrFresh[i]
        if(is.null(dryOrFresh)) dryOrFresh <- NA
        if (!(dryOrFresh %in% c("Dry", "Fresh"))){
          log4r::info(my_logger, 
                      paste0("WARNING: dryOrFresh value not found for parcel ",
                             landUseSummaryOrPractices[[1]]$parcelName[i],
                             " for year ", j,". Assuming Fresh."))
          dryOrFresh <- "Fresh"
          }

        # Correct for moisture content
        if(dryOrFresh == 'Dry') { dw <- dw_dry } else { dw <- dw_fresh }
        
        pasture_df_temp %>% mutate(
          dry_harvest = harvest * dw,
          dry_grazing = grazing * dw,
          dry_residue = residue * dw,
          dry_agb_peak = agb_peak * dw
        )
        pasture_inputs <- rbind(pasture_inputs, pasture_df_temp)
      }
    }
  }
  
  return(pasture_inputs)
}

get_soil_inputs = function(landUseSummaryOrPractices, soilAnalysis, soilMapsData){
  # takes landUseSummaryOrPractices from farms collection
  # extracts parcels input dataframe 
  soil_inputs = data.frame(parcel_ID = c(), scenario = c(), clay = c(), irrigation=c())
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    for (j in c(0:10)){
      year_str <- paste0('year', j)
      soil_inputs <- rbind(soil_inputs,data.frame(
        parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]),
        scenario = c(year_str),
        clay = c(get_clay_content(soilAnalysis, soilMapsData)),
        silt = c(get_silt_content(soilAnalysis, soilMapsData)),
        SOC = c(get_SOC_content(soilAnalysis, soilMapsData)),
        bulk_density = c(get_bulk_density(soilAnalysis, soilMapsData)),
        irrigation = c(ifelse(is.null(landUseSummaryOrPractices[[1]][[year_str]]$irrigation[i]),FALSE,
                              ifelse(is.na(landUseSummaryOrPractices[[1]][[year_str]]$irrigation[i]),FALSE,
                                     landUseSummaryOrPractices[[1]][[year_str]]$irrigation[i])))))
      if (j==0){
        soil_inputs <- rbind(soil_inputs,data.frame(
          parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]),
          scenario = c("baseline"),
          clay = c(get_clay_content(soilAnalysis, soilMapsData)),
          silt = c(get_silt_content(soilAnalysis, soilMapsData)),
          SOC = c(get_SOC_content(soilAnalysis, soilMapsData)),
          bulk_density = c(get_bulk_density(soilAnalysis, soilMapsData)),
          irrigation = c(ifelse(is.null(landUseSummaryOrPractices[[1]][[year_str]]$irrigation[i]),FALSE,
                                ifelse(is.na(landUseSummaryOrPractices[[1]][[year_str]]$irrigation[i]),FALSE,
                                       landUseSummaryOrPractices[[1]][[year_str]]$irrigation[i])))))
      }
    }
  }
  return(soil_inputs)
}

get_tilling_inputs = function(landUseSummaryOrPractices, tilling_factors, farm_EnZ){ 
  # takes landUseSummaryOrPractices from farms collection, farm_country (from farmInfo) and tilling factors table
  # extracts tilling inputs dataframe 
  tilling_factor = (tilling_factors %>% filter(pedo_climatic_area == farm_EnZ))$tilling_factor
  minimum_tillage_factor = (tilling_factors %>% filter(pedo_climatic_area == farm_EnZ))$minimum_tillage_factor
  tilling_inputs = data.frame(parcel_ID = c(), scenario = c(), tilling_factor = c())
  for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
    tilling_inputs <- rbind(tilling_inputs, data.frame(
      parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
      scenario = c('baseline'),
      tilling_factor = c(tilling_factor))) # assumption
    for (j in c(0:10)){
      year_str <- paste0('year', j)
      year_chosen = landUseSummaryOrPractices[[1]][[year_str]]
      for (k in c(1:12)){
        tilling_inputs <- rbind(tilling_inputs, data.frame(
          parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
          scenario = c(year_str),
          tilling_factor = c(ifelse(year_chosen$tillingEvent[i][[1]][[k]]==TRUE, tilling_factor, 
                                    ifelse(year_chosen$minimumTillingEvent[i][[1]][[k]]==TRUE, minimum_tillage_factor, 1)))))
      }
    }
  }
  tilling_inputs = tilling_inputs %>% group_by(parcel_ID, scenario) %>%
    summarise(tilling_factor = max(tilling_factor)) # ATM JUST TAKE THE MAX IMPACT EVENT
  return(tilling_inputs)
}
