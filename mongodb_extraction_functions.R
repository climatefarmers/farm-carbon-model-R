#MongoDB parameters extraction functions

### TOOL FUNCTIONS
## Helper function to convert inputs to numeric
missing_to_zero <- function(input){
  if(length(input)==0) return(0)
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

missing_to_na <- function(input){
  if(length(input)==0) return(NA)
  for(i in c(1:length(input))){
    if(is.null(input[i])){
      input[i]=NA
    } else if(is.na(input[i])){
      input[i]=NA
    } else if(input[i]==""){
      input[i]=NA
    }
  }
  return(as.numeric(sub(",", ".", input, fixed = TRUE)))
}

missing_to_value <- function(input, value=NULL){
  if(length(input)==0) return(NA)
  for(i in c(1:length(input))){
    if(is.null(input[i])){
      if(is.null(value)) {input[i]=NA} else {input[i]=value}
    } else if(is.na(input[i])){
      if(is.null(value)) {input[i]=NA} else {input[i]=value}
    } else if(input[i]==""){
      if(is.null(value)) {input[i]=NA} else {input[i]=value}
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

# ## Helper function to calculate animal_inputs: extracts total grazing amount
# extract_total_grazing_amount <- function(landUseSummaryOrPractices, year, area){
#   # Takes a landUseSummaryOrPractices from farms collection
#   # Extracts the overall grazing yield and bale grazing yield from the whole farm
#   
#   year_str <- paste0('year', year)
#   bale_grazing <- 0
#   grazing <- 0
#   year_chosen <- landUseSummaryOrPractices[[1]][[year_str]]
#   
#   for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
#     
#     isBaleGrazing <- year_chosen$baleGrazing[i]
#     baleResidue <- missing_to_zero(year_chosen$residueLeftAfterBaleGrazing[i]) / 100
#     hayStraw <- missing_to_zero(year_chosen$hayStrawApplication[i])
#     
#     if (is.na(isBaleGrazing) | !bale_grazing) {
#       bale_grazing = bale_grazing + 0
#     } else if (isBaleGrazing){
#       # Add bale grazing of all parcels after subtracting residues left on field 
#       bale_grazing <- bale_grazing + hayStraw * (1 - ifelse(is.na(baleResidue), 0.15, baleResidue))
#     }
#     
#     for (k in c(1:12)){
#       grazing_month <- missing_to_zero(year_chosen$grazingYield[i][[1]][[k]] )
#       grazing = grazing + grazing_month
#     }
#     
#   }
#   
#   return((bale_grazing + grazing) * area)
# }

# ## Helper function to calculate animal_inputs: extracts grazing amount per parcel 
# extract_grazing_amount_parcel_i <- function(landUseSummaryOrPractices, parcel_index, year, area){
#   #takes a landUseSummaryOrPractices from farms collection and a parcel index i
#   #extracts grazing yield and bale grazing yield from parcel index
#   
#   year_str <- paste0('year', year)
#   
#   isBaleGrazing <- landUseSummaryOrPractices[[1]][[year_str]]$baleGrazing[parcel_index]
#   baleResidue <- missing_to_zero(landUseSummaryOrPractices[[1]][[year_str]]$residueLeftAfterBaleGrazing[parcel_index])/100
#   hayStraw <- missing_to_zero(landUseSummaryOrPractices[[1]][[year_str]]$hayStrawApplication[parcel_index])
#   
#   if (is.na(isBaleGrazing) | !isBaleGrazing){ # Fernando: If there is hay input can this be FALSE? 
#     bale_grazing <- 0
#   } else if (isBaleGrazing){
#     bale_grazing = hayStraw * (1 - ifelse(is.na(baleResidue), 0.15, baleResidue))
#   }
#   
#   grazing = 0
#   for (k in c(1:12)){
#     grazing_month <- missing_to_zero(landUseSummaryOrPractices[[1]][[year_str]]$grazingYield[parcel_index][[1]][[k]])
#     grazing = grazing + grazing_month
#   }
#   return((bale_grazing + grazing) * area)
# }

get_livestock_table <- function(livestock) {
  
  year_strings <- paste0("year", 0:10)
  
  animals = expand.grid(
    scenario = year_strings, 
    species = animal_factors$species, 
    n_animals = 0, 
    grazing_days = 0
  )
  
  for (y in c(0:10)){
    year_str <- year_strings[y+1]
    if(y == 0) {
      livestock_scenario = livestock[["currentManagement"]][[1]]
    } else {
      livestock_scenario = livestock[["futureManagement"]][[1]][[year_str]] 
    }
    n_animals <- missing_to_zero(livestock_scenario$numberOfHeads)
    grazing_days <- missing_to_zero(livestock_scenario$grazingOrPasturedDaysPerYear)
    for (k in c(1:nrow(livestock_scenario))){
      species <- livestock_scenario$species[k]
      if (is.na(species)) {next}  # This line should not be needed if data is input correctly. Consider eventual removal.
      ind <- animals$scenario == year_str & animals$species == species
      animals$n_animals[ind] <- animals$n_animals[ind] + n_animals[k]  # adding to deal with current situation that same species can be given more than once.
      animals$grazing_days[ind] <- max(animals$grazing_days[ind], grazing_days[k])  # This line doesn't work well with mutliple entries of same species with different days grazing. Using max otherwise entries with 0 nulify all.
    }
  }
  animals <- animals[animals$n_animals!=0, ] # removing all rows that do not apply.
  
  return(animals)
}

## Helper function to extract total grazing and bale grazing yield from the whole farm over all years
get_grazing_amounts <- function(landUseSummaryOrPractices, livestock, animal_factors, parcel_inputs, livestock_table, use_calculated_grazing){
  # Extracts the overall grazing and fodder (bale) grazing from the whole farm

  year_strings = paste0("year", 0:10)
  
  parcel_names <- parcel_inputs$parcel_ID
  
  # Dry weight
  dw_dry <- 1  # dry fraction of fully dehydrated plant material
  dw_fresh <- (pasture_factors %>% filter(grass == 'Generic annual grasses') %>% select(dw_fresh))[[1]]  # dry fraction of estimated yields
  
  # Prepare data frames
  grazing_yearly <- expand_grid(
    parcel = parcel_names,
    year = 0:10,
    landuse = NA,
    grazing_rep = NA,
    fodder = NA,
    fodder_eaten = NA,
    fodder_residue = NA,
    forage_rep = NA,
    grazing_calc = NA,
    forage_calc = NA,
    grazing = NA,
    forage = NA,
    grazing_ha = NA,
    forage_ha = NA,
    fodder_residue_ha = NA
  )
  
  grazing_monthly <- expand_grid(
    parcel = parcel_names,
    year = 0:10,
    month = 1:12,
    grazing_rep = NA
  )
  
  # Get reported grazing numbers ß----
  for (y in c(0:10)){
    
    # Variables for yearly table
    year_str <- paste0('year', y)
    fodder_eaten <- 0
    grazing_rep <- 0
    year_chosen <- landUseSummaryOrPractices[[1]][[year_str]]
    
    # Bale grazing from hay application
    for (i in 1:length(parcel_names)){
      
      parcel <- parcel_names[i]
      # if(parcel=="Cabeço dos Sobreiros") browser()
      baleResidue <- missing_to_na(year_chosen$residueLeftAfterBaleGrazing[i])/100
      if(is.na(baleResidue)) {baleResidue <- 0.15}  # 15% assumed residue left of fodder on field
      fodder_parcel <- missing_to_zero(year_chosen$hayStrawApplication[i]) * parcel_inputs$area[i]
      landUseType <- year_chosen$landUseType[i]
      
      # Add to total bale grazed after subtracting residues left on field 
    
      fodder_eaten_parcel <- fodder_parcel * (1 - baleResidue)
      fodder_residue_parcel <- fodder_parcel * baleResidue
      grazing_parcel <- 0
      
      for (k in c(1:12)){
        # grazing from monthly grazing data
        grazing_month <- missing_to_zero(year_chosen$grazingYield[i][[1]][[k]])
        ind <- (grazing_monthly$parcel == parcel & grazing_monthly$year == y & grazing_monthly$month == k)
        grazing_monthly$grazing_rep[ind] <- grazing_month
        # getting sum for parcel
        grazing_parcel <- grazing_parcel + grazing_month * parcel_inputs$area[i]
      }
      
      # Correct for moisture content
      dryOrFresh <- year_chosen$yieldsResiduesDryOrFresh[i]
      if(is.null(dryOrFresh)) dryOrFresh <- NA
      if (!(dryOrFresh %in% c("Dry", "Fresh"))) { dryOrFresh <- "Fresh" }
      if(dryOrFresh == 'Fresh') { dw <- dw_fresh } else { dw <- dw_dry }
      grazing_parcel <- grazing_parcel * dw

      ind <- (grazing_yearly$parcel==parcel & grazing_yearly$year==y)
      grazing_yearly$landuse[ind] <- landUseType
      grazing_yearly$grazing_rep[ind] <- grazing_parcel
      grazing_yearly$fodder[ind] <- fodder_parcel
      grazing_yearly$fodder_eaten[ind] <- fodder_eaten_parcel 
      grazing_yearly$fodder_residue[ind] <- fodder_residue_parcel
      
    }
  }
  
  ## Get dry weight grazing calculated from animal data ----
  
  # Merge with factors and calculate the grazing per animal type
  animals <- merge(x = livestock_table, y = animal_factors, by = "species", all.x = TRUE)
  animals <- animals %>%
    mutate(yearly_grazing_needs_tDM = n_animals * mass_kg_per_animal / 1000 * 0.025 * grazing_days) # 0.025 is the 2.5% of animal mass required as feed every day
  # Sum up the calculated grazing of all animal types for each year
  grazing_yearly_calc <- animals  %>% group_by(scenario) %>%
    summarise(grazing_yearly_calc = sum(yearly_grazing_needs_tDM))

  ## Distribute the yearly calculated grazing to the different parcels
  for(y in 0:10) {
    year_str <- paste0('year', y)
    grazing_calc_year <- sum(grazing_yearly_calc$grazing_yearly_calc[grazing_yearly_calc$scenario==year_str])
    grazing_rep_year <- sum(grazing_yearly$grazing_rep[grazing_yearly$year==y])
    if(grazing_rep_year == 0) {
      # In years with no reported grazing distribute calculated grazing among parcels weighted by area
      for(i in 1:length(parcel_names)) {
        parcel <- parcel_names[i]
        area_parcel <- parcel_inputs$area[parcel_inputs$parcel_ID == parcel]
        area_total <- sum(parcel_inputs$area)
        ind <- (grazing_yearly$year == y & grazing_yearly$parcel == parcel)
        grazing_yearly$grazing_calc[ind] <- grazing_calc_year * area_parcel / area_total
      }
    } else {
      # In years with reported grazing, distribute calculated grazing to proportionally match the reported grazing
      ind <- grazing_yearly$year == y
      grazing_yearly$grazing_calc[ind] <- grazing_calc_year * grazing_yearly$grazing_rep[ind] / grazing_rep_year
    }
  }
  
  grazing_yearly <- grazing_yearly %>% 
    mutate(forage_rep = grazing_rep - fodder_eaten)
  grazing_yearly <- grazing_yearly %>% 
    mutate(forage_calc = grazing_calc - fodder_eaten)
  grazing_yearly$scenario <- paste0("year",grazing_yearly$year)
  
  # Select what grazing to use depending on settings (could be changed to depend on reported data availability)
  if(use_calculated_grazing) {
    grazing_yearly$grazing <- grazing_yearly$grazing_calc
    grazing_yearly$forage <- grazing_yearly$forage_calc
  } else {
    grazing_yearly$grazing <- grazing_yearly$grazing_rep
    grazing_yearly$forage <- grazing_yearly$forage_rep
  }
  
  # Calculate the values per hectare
  for(i in 1:length(parcel_names)) {
    grazing_yearly$grazing_ha <- grazing_yearly$grazing / parcel_inputs$area[i]
    grazing_yearly$forage_ha <- grazing_yearly$forage / parcel_inputs$area[i]
    grazing_yearly$fodder_residue_ha <- grazing_yearly$fodder_residue / parcel_inputs$area[i]
  }
  
  return(list(grazing_monthly=grazing_monthly, grazing_yearly=grazing_yearly))
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
    if(5<missing_to_zero(soilAnalysis$clayContentPercent) & missing_to_zero(soilAnalysis$clayContentPercent)<80){ # assumed to be %
      return(missing_to_zero(soilAnalysis$clayContentPercent))
    } else {
      log4r::error(my_logger, paste("Clay content input = ", 
                                    missing_to_zero(soilAnalysis$clayContentPercent),
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
    if(5<missing_to_zero(soilAnalysis$siltContentPercent) & missing_to_zero(soilAnalysis$siltContentPercent)<80){ # assumed to be %
      return(missing_to_zero(soilAnalysis$siltContentPercent))
    } else {
      log4r::error(my_logger, paste("silt content input = ", 
                                    missing_to_zero(soilAnalysis$siltContentPercent),
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
    if(8<missing_to_zero(soilAnalysis$organicMatterContent) & missing_to_zero(soilAnalysis$organicMatterContent)<80 & soilAnalysis$organicMatterContentMetric!="%"){ # SOC in t/ha = g/kg
      return(missing_to_zero(soilAnalysis$organicMatterContent)*0.55)
    } 
    if (0.7<missing_to_zero(soilAnalysis$organicMatterContent) & missing_to_zero(soilAnalysis$organicMatterContent)<8){ #SOC in %
      return(missing_to_zero(soilAnalysis$organicMatterContent)*5.5)
    } else {
      log4r::error(my_logger, paste("OM content input = ", missing_to_zero(soilAnalysis$organicMatterContent),
                                    soilAnalysis$organicMatterContentMetric,
                                    ". Check unit/values with farmer.", sep=""))
    }
  }
  if (soilAnalysis$carbonContent!=""){ # SOC variable exists and a value was entered
    if(4<missing_to_zero(soilAnalysis$carbonContent) & missing_to_zero(soilAnalysis$carbonContent)<40){ # SOC in t/ha = g/kg
      return(missing_to_zero(soilAnalysis$carbonContent))
    } 
    if (0.35<missing_to_zero(soilAnalysis$carbonContent) & missing_to_zero(soilAnalysis$carbonContent)<4){ #SOC in %
      return(missing_to_zero(soilAnalysis$carbonContent)*10)
    } else {
      log4r::error(my_logger, paste("SOC content input = ", missing_to_zero(soilAnalysis$carbonContent),
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
    if(0.7<missing_to_zero(soilAnalysis$bulkDensity) & missing_to_zero(soilAnalysis$bulkDensity)<2){
      return(missing_to_zero(soilAnalysis$bulkDensity))
    } else if (700<missing_to_zero(soilAnalysis$bulkDensity) & missing_to_zero(soilAnalysis$bulkDensity)<2000){
      return(missing_to_zero(soilAnalysis$bulkDensity)*1e-3)
    } else {
      log4r::error(my_logger, paste("bulk density input = ", 
                                    missing_to_zero(soilAnalysis$bulkDensity),
                                    ". Check unit/values with farmer.", sep=""))
    }
  }
}

### GET INPUT FUNCTIONS
get_orgamendments_inputs = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts manure application inputs dataframe 
  parcel_names <- landUseSummaryOrPractices[[1]]$parcelName
  orgamendments_inputs = data.frame(parcel_ID = c(), scenario = c(), source = c(), 
                                    quantity_t_ha = c(), imported_frac = c(), remaining_frac = c())
  
  for (i in c(1:length(parcel_names))){
    for (j in c(0:10)){
      year_str <- paste0('year', j)
      year_chosen = landUseSummaryOrPractices[[1]][[year_str]]
      
      # Data check and correction (should not be necessary: data checks should be done upstream)
      manureApplication <- missing_to_zero(year_chosen$manureApplication[i])
      if(is.na(manureApplication) | manureApplication < 0) { manureApplication <- 0 }
      compostApplication <- missing_to_zero(year_chosen$compostApplication[i])
      if(is.na(compostApplication) | compostApplication < 0) { compostApplication <- 0 }
      hayStrawApplication <- missing_to_zero(year_chosen$hayStrawApplication[i])
      if(is.na(hayStrawApplication) | hayStrawApplication < 0) { hayStrawApplication <- 0 }
      percentManureImported <- missing_to_na(year_chosen$percentManureImported[i])
      if(is.null(percentManureImported)) {percentManureImported <- 100}
      if(is.na(percentManureImported)) {percentManureImported <- 100}
      percentCompostImported <- missing_to_na(year_chosen$percentCompostImported[i])
      if(is.null(percentCompostImported)) {percentCompostImported <- 100}
      if(is.na(percentCompostImported)) {percentCompostImported <- 100}
      percentHayStrawImported <- missing_to_na(year_chosen$percentageOfHayStrawImported[i])
      if(is.null(percentHayStrawImported)) {percentHayStrawImported <- 100}
      if(is.na(percentHayStrawImported)) {percentHayStrawImported <- 100}
      isBaleGrazing <- year_chosen$baleGrazing[i]
      if(is.null(isBaleGrazing)) {isBaleGrazing <- FALSE}
      if(is.na(isBaleGrazing)) {isBaleGrazing <- FALSE}
      baleResidue <- missing_to_zero(year_chosen$residueLeftAfterBaleGrazing[i])
      if(is.null(baleResidue)) {baleResidue <- NA}
      if(is.na(baleResidue)) {baleResidue <- 15} # 15 is the default % residue left after grazing 
      
      # Manure (animal dung)
      orgamendments_temp <- data.frame(
        parcel_ID = parcel_names[i], 
        scenario = year_str, 
        source = "Other Cattle", # AN UNFOLDING LIST OF MANURE TYPE MIGHT HAVE TO BE ADDED TO UI
        quantity_t_ha = manureApplication,
        imported_frac = percentManureImported/100,
        remaining_frac = 1
      )
      orgamendments_inputs <- rbind(orgamendments_inputs, orgamendments_temp)
      
      # Compost
      orgamendments_temp <- data.frame(
        parcel_ID = parcel_names[i], 
        scenario = year_str, 
        source = "Green compost",
        quantity_t_ha = compostApplication,
        imported_frac = percentCompostImported/100,
        remaining_frac = 1
      )
      orgamendments_inputs <- rbind(orgamendments_inputs, orgamendments_temp)
      
      # Hay. Warning: this used as the value for bale hay/fodder. Should be changed to straw amendments and fodder done separately
      orgamendments_temp <- data.frame(
        parcel_ID = parcel_names[i], 
        scenario = year_str, 
        source = "Hay",
        quantity_t_ha = hayStrawApplication,
        imported_frac = percentHayStrawImported/100,
        remaining_frac = ifelse(isBaleGrazing, baleResidue/100, 1)
      )
      orgamendments_inputs <- rbind(orgamendments_inputs, orgamendments_temp)
      
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
  parcel_names <- landUseSummaryOrPractices[[1]]$parcelName
  agroforestry_inputs = data.frame(parcel_ID = c(), scenario = c(), tree_species = c(), other_name = c(), dbh = c(),
                                   tree_density = c(), area = c())
  for (i in c(1:length(parcel_names))){
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
            parcel_ID = c(parcel_names[i]), 
            scenario = c(year_str), 
            tree_species = c(typeOfTrees$treeName[[k]]),
            other_name = c(typeOfTrees$otherTreeName[[k]]),
            dbh = c(missing_to_zero(typeOfTrees$treeAvgDBH[[k]])), 
            tree_density = c(missing_to_zero(typeOfTrees$avgNoOfTrees[[k]])), 
            area = c(missing_to_zero(landUseSummaryOrPractices[[1]]$area[i])/10000)))
        }
        if (j==0){ #baseline based on pre-project trees
          for (k in c(1:nrow(typeOfTrees))){
            agroforestry_inputs <- rbind(agroforestry_inputs,data.frame(
              parcel_ID = c(parcel_names[i]), 
              scenario = c("baseline"), 
              tree_species = c(typeOfTrees$treeName[[k]]),
              other_name = c(typeOfTrees$otherTreeName[[k]]),
              dbh = c(missing_to_zero(typeOfTrees$treeAvgDBH[[k]])), 
              tree_density = c(missing_to_zero(typeOfTrees$avgNoOfTrees[[k]])), 
              area = c(missing_to_zero(landUseSummaryOrPractices[[1]]$area[i])/10000)))
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

get_animal_inputs = function(grazing_yearly, livestock_table, parcel_inputs){
  # takes landUseSummaryOrPractices & livestock from farms collection
  # extracts animal inputs dataframe
  
  parcel_names <- parcel_inputs$parcel_ID
  animal_inputs = data.frame()
  
  for (i in c(1:length(parcel_names))){
    
    for (y in c(0:10)){
      
      year_str <- paste0('year', y)

      grazing_year <- grazing_yearly %>% filter(scenario == year_str) %>% select(grazing, parcel)
      grazing_year_total <- sum(grazing_year$grazing)
      grazing_year_parcel <- grazing_year$grazing[grazing_year$parcel == parcel_names[i]]
      
      livestock_temp <- livestock_table %>% filter(scenario == year_str)

      for (k in 1:nrow(livestock_temp)){

        n_animals <- livestock_temp$n_animals[k]
        n_animals_parcel <- ifelse(grazing_year_total == 0, 0, n_animals * grazing_year_parcel / grazing_year_total)
        
        animal_inputs_temp <- data.frame(
          parcel_ID = parcel_names[i], 
          scenario = year_str, 
          species = livestock_temp$species[[k]],
          n_animals = n_animals_parcel,  # n_animal is the total number of animal from a farm weighted by grazing fraction of the parcel
          grazing_days = livestock_temp$grazing_days[k], 
          area = parcel_inputs$area[i],
          grazing_management = "Daily Spread", 
          productivity = "Low Productivity"
        )
        
        animal_inputs <- rbind(animal_inputs, animal_inputs_temp)
        
      }
    }
  }
  
  # Set baseline to be equal to year0
  animal_inputs <- rbind(animal_inputs, animal_inputs %>% filter(scenario=='year0') %>% mutate(scenario='baseline'))
  return(animal_inputs)
}

get_bareground_inputs = function(landUseSummaryOrPractices, soil_cover_data, farm_EnZ, bare_bl_type){
  # takes landUseSummaryOrPractices from farms collection
  # extracts bare soil inputs dataframe
  
  parcel_names <- landUseSummaryOrPractices[[1]]$parcelName
  bareground_inputs <- expand_grid(parcel_ID = parcel_names, year = 0:10, month = 1:12)
  bareground_inputs$bareground <- NA
  bareground_inputs$scenario <- NA
  
  for (i in c(1:length(parcel_names))){
    
    for (j in c(0:10)){
      
      year_str <- paste0('year', j)
      
      bare_monthly_reported = landUseSummaryOrPractices[[1]][[year_str]]$bareSoilFallow[i] %>% unlist()
      
      index <- bareground_inputs$parcel_ID == parcel_names[i] & bareground_inputs$year == j
      
      bareground_inputs <- bareground_inputs %>% mutate(
        bareground = replace(bareground, index, bare_monthly_reported),
        scenario = replace(scenario, index, year_str)
      )
    }
  }
  
  bare_monthly_envzone <- soil_cover_data %>% 
    filter(pedo_climatic_area == farm_EnZ) %>%
    select(-country,-pedo_climatic_area) %>% slice(1) %>% 
    unlist(use.names = FALSE)
  
  bareground_inputs$bareground_envzone <- bareground_inputs$bareground
  bareground_inputs$bareground_reported <- bareground_inputs$bareground
  bareground_inputs$bareground_none <- FALSE
  
  bareground_inputs_bl <- bareground_inputs %>% filter(year == 0) %>% mutate(scenario = 'baseline')
  bareground_inputs <- rbind(bareground_inputs, bareground_inputs_bl)
  
  bareground_inputs$bareground_envzone[bareground_inputs$scenario == 'baseline'] <-
    bare_monthly_envzone
  
  if(bare_bl_type=="envzone"){
    # The baseline bare ground values are set equal to common values for the region
    bareground_inputs$bareground <- bareground_inputs$bareground_envzone
  } else if(bare_bl_type == "reported") {
    # The baseline bare ground values are set equal to year 0
    bareground_inputs$bareground <- bareground_inputs$bareground_reported
  } else if(bare_bl_type == "none") {
    # In this case, a baseline is created but all bare ground values are set to false (avoids bare ground effect)
    bareground_inputs$bareground <- bareground_inputs$bareground_none
  }
  
  # write_csv(x = bareground_inputs[bareground_inputs$year < 3, ], file = "bareground_inputs.csv") # for debugging
  return(bareground_inputs)
}

# Old version
# get_bareground_inputs = function(landUseSummaryOrPractices, soil_cover_data, farm_EnZ){
#   # takes landUseSummaryOrPractices from farms collection
#   # extracts bare soil inputs dataframe 
# 
#   bare_field_inputs = data.frame(parcel_ID = c(), scenario = c())
#   # one column per month
#   for (k in c(1:12)){
#     bare_field_inputs[[paste("bare_profile_", k, sep="")]] = c()
#   }
#   for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
#     bare_field_inputs_temp <- data.frame(parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
#                                          scenario = c('baseline'))
#     bare_field_inputs <- rbind(bare_field_inputs, cbind(bare_field_inputs_temp, soil_cover_data %>% filter(pedo_climatic_area == farm_EnZ) %>%
#                                                           select(-country,-pedo_climatic_area)))
#     for (j in c(0:10)){
#       year_chosen = landUseSummaryOrPractices[[1]][[paste('year', j, sep="")]]
#       bare_field_inputs_temp = data.frame(parcel_ID = c(landUseSummaryOrPractices[[1]]$parcelName[i]), 
#                                           scenario = c(paste('year', j, sep="")))
#       for (k in c(1:12)){
#         bare_field_inputs_temp[[paste("bare_profile_", k, sep="")]] = ifelse(
#           year_chosen$bareSoilFallow[[1]][[k]]==TRUE, TRUE, FALSE)
#       }
#       bare_field_inputs <- rbind(bare_field_inputs, bare_field_inputs_temp)
#     }
#   }
#   return(bare_field_inputs)
# }


get_crop_inputs <- function(landUseSummaryOrPractices, parcel_inputs, crop_factors, use_calculated_grazing, grazing_yearly){
  
  year_strings <- paste0("year", 0:10)
  parcel_names <- parcel_inputs$parcel_ID
  
  crop_inputs <- data.frame()
  
  for (j in c(0:10)){ # cycle through years
    
    year_str <- paste0('year', j)
    year_chosen = landUseSummaryOrPractices[[1]][[year_str]]
    
    for (i in c(1:length(parcel_names))){
      
      # Excluding non-arable parcels: no holistic grazing compatible land-uses (no pasture efficiency coef will be used in crop inputs)
      if (!year_chosen$landUseType[i]=="Arablecrops") { next }

      # Creating the data frame storing monthly yield and residue
      monthly_harvest = data.frame(
        crop = rep(NA, 12), 
        coverCrop = rep(NA, 12), 
        productiveFallow = rep(NA, 12),
        harvest = rep(NA, 12),
        residue = rep(NA, 12)
      )
      
      # Getting actual data
      monthly_harvest$crop = get_monthly_cash_crop(parcel_index = i, year_chosen)
      monthly_harvest$coverCrop = year_chosen$coverCropMonthlyData[[i]]
      monthly_harvest$productiveFallow = year_chosen$productiveFallow[[i]]
      monthly_harvest$grazing = missing_to_zero(year_chosen$grazingYield[[i]])
      monthly_harvest$harvest = missing_to_zero(year_chosen$harvestYield[[i]])
      monthly_harvest$residue = missing_to_zero(year_chosen$estimationAfterResidueGrazingHarvest[[i]])
      
      # If use_calculated_grazing is TRUE, total grazing yield is calculated here, replacing input values.
      if (use_calculated_grazing){
        yearly_grazing_ha <- grazing_yearly %>% filter(scenario==year_str & parcel == parcel_names[i]) %>% select(grazing_ha)
        split_grazing <- 1/2 * yearly_grazing_ha
        monthly_harvest$grazing <- c(split_grazing, 0, 0, 0, 0, 0, split_grazing, 0, 0, 0, 0, 0)
      }
      
      # Check if input values are fresh or dry.
      # Fresh is interpreted as weight at harvest. Dry as fully dry.
      dryOrFresh <- year_chosen$yieldsResiduesDryOrFresh[i]
      if(is.null(dryOrFresh)) dryOrFresh <- NA
      if (!(dryOrFresh %in% c("Dry", "Fresh"))){
        if(i == 1) log4r::info(my_logger,  paste0("WARNING: dryOrFresh value not found for year ", j,". Assuming: Fresh."))
        dryOrFresh <- "Fresh"
      }
      
      # case of cash crop with no grazing
      for (crop_chosen in unique(monthly_harvest$crop)){
        
        if(is.na(crop_chosen)) {
          crop_monthly <- monthly_harvest %>% filter(is.na(crop))
          crop_chosen <- "Non-N-fixing dry forages"
        } else {
          crop_monthly <- monthly_harvest %>% filter(crop==crop_chosen)
        }
        
        yield_sums <- crop_monthly$harvest + crop_monthly$grazing + crop_monthly$residue
        harvest <- sum(crop_monthly$harvest)
        grazing <- sum(missing_to_zero(crop_monthly$grazing))
        residue_general <- sum(crop_monthly$residue)
        residue_grazing <- grazing * 0.15  # 15% considered dropped during grazing
        
        crop_inputs_temp <- data.frame(scenario = year_str,
                                       parcel_ID = parcel_names[i],
                                       crop = crop_chosen,
                                       harvest = harvest, 
                                       grazing = grazing, 
                                       residue = residue_general + residue_grazing,
                                       agb_peak = max(yield_sums),
                                       dry_fresh = dryOrFresh
        )
        
        # Merge crop with previous crops 
        crop_inputs <- rbind(crop_inputs, crop_inputs_temp)
      }
    }
  }
  
  # If arable parcels found, correct for dry weight
  if(length(crop_inputs) > 0) {
    crop_inputs <- merge(x = crop_inputs, y = crop_factors %>% select(crop, dw_fresh), by = "crop", all.x = TRUE)
    crop_inputs$dw_fresh[is.na(crop_inputs$dw_fresh)] <- crop_factors$dw_fresh[crop_factors$crop == "Other"] # Using option 'Other' if no crop match was found
    # Select the dw correction according to dry or fresh variable.
    crop_inputs$dry_weight <- 1
    ind <- crop_inputs$dry_fresh == "Fresh"
    crop_inputs$dry_weight[ind] <- crop_inputs$dw_fresh[ind]
    crop_inputs <- crop_inputs %>% mutate(
      dry_harvest = harvest * dry_weight,
      dry_grazing = grazing * dry_weight,
      dry_residue = residue * dry_weight,
      dry_agb_peak = agb_peak * dry_weight
    )
  } else { # In the case there weren't any arable parcels
    crop_inputs <- expand_grid(
        scenario = year_strings,
        parcel_ID = parcel_names,
        crop = "Non-N-fixing dry forages",
        harvest = 0, 
        grazing = 0, 
        residue = 0,  
        agb_peak = 0,
        dry_harvest = 0,
        dry_grazing = 0,
        dry_residue = 0,
        dry_agb_peak = 0
    )
  }
  
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
#       } else if (missing_to_zero(landUseSummaryOrPractices[[1]]$year0$applyingThesePracticesInYears[i])>3){
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
        field_area <- missing_to_zero(landUseSummaryOrPractices[[1]]$area[i])/10000 # if no corrected value was provided by the farmer
      } else {
        field_area <- missing_to_zero(landUseSummaryOrPractices[[1]]$manuallyEnteredArea[i])/10000 # add a verification of consistency here?
      }
      
      year_chosen = landUseSummaryOrPractices[[1]][[year_str]]
      
      fertilizer_temp <- data.frame(
        parcel_ID = parcel_id, 
        field_area = field_area,
        scenario = year_str,
        usage_boolean = year_chosen$syntheticFertilizer$usage[i],
        fertilizer_type = "synthetic", # here gathering data from the synthetic fertilizer dashboard entry
        quantity_t_ha = ifelse(year_chosen$syntheticFertilizer$usage[i]==TRUE, missing_to_zero(year_chosen$syntheticFertilizer$tonsPerYear[i]),0),
        n_content_perc=ifelse(year_chosen$syntheticFertilizer$usage[i]==TRUE, missing_to_zero(year_chosen$syntheticFertilizer$percentOfNitrogen[i]),0)
      )
      fertilizer_inputs <- rbind(fertilizer_inputs, fertilizer_temp)
      
      if (j==0){
        fertilizer_inputs <- rbind(fertilizer_inputs,data.frame(
          parcel_ID = parcel_id, 
          field_area = ifelse(is.null(landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i]),
                              c(missing_to_zero(landUseSummaryOrPractices[[1]]$area[i])/10000),
                              ifelse(is.na(landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i]) |
                                       landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i] == FALSE, # means that no corrected value was provided by the farmer
                                     c(missing_to_zero(landUseSummaryOrPractices[[1]]$area[i])/10000),
                                     c(missing_to_zero(landUseSummaryOrPractices[[1]]$manuallyEnteredArea[i])/10000))), # add a verification of consistence here
          scenario = c("baseline"),
          usage_boolean = year_chosen$syntheticFertilizer$usage[i],
          fertilizer_type = "synthetic", # here gathering data from the synthetic fertilizer dashboard entry
          quantity_t_ha = ifelse(year_chosen$syntheticFertilizer$usage[i]==TRUE, missing_to_zero(year_chosen$syntheticFertilizer$tonsPerYear[i]),0),
          n_content_perc=ifelse(year_chosen$syntheticFertilizer$usage[i]==TRUE, missing_to_zero(year_chosen$syntheticFertilizer$percentOfNitrogen[i]),0)))
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
      value_l = missing_to_zero(fuel[[status]][[1]]$amountInLiters[[k]])))
    fuel_inputs <- rbind(fuel_inputs,data.frame(
      scenario = c("baseline"), 
      fuel_type = fuel[[status]][[1]]$typeOfFuel[[k]],
      value_l = missing_to_zero(fuel[[status]][[1]]$amountInLiters[[k]])))
  }
  status = "futureFuelUsage"
  for (year in c(1:10)){
    year_str <- paste0('year', year)
    for (k in c(1:nrow(fuel[[status]][[1]]))){
      if (is.na(fuel[[status]][[1]]$typeOfFuel[[k]]) | fuel[[status]][[1]]$typeOfFuel[[k]]==""){next}
      fuel_inputs <- rbind(fuel_inputs,data.frame(
        scenario = c(year_str), 
        fuel_type = fuel[[status]][[1]]$typeOfFuel[[k]],
        value_l = missing_to_zero(fuel[[status]][[1]]$amountInLiters[[k]])))
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
                    c(missing_to_zero(landUseSummaryOrPractices[[1]]$area[i])/10000),
                    ifelse(is.na(landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i]) |
                             landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i] == FALSE, # means that no corrected value was provided by the farmer
                           c(missing_to_zero(landUseSummaryOrPractices[[1]]$area[i])/10000),
                           c(missing_to_zero(landUseSummaryOrPractices[[1]]$manuallyEnteredArea[i])/10000))), # add a verification of consistence here
      longitude = c(missing_to_zero(extract_longitude_landUseSummaryOrPractices(landUseSummaryOrPractices,i))),
      latitude=c(missing_to_zero(extract_latitude_landUseSummaryOrPractices(landUseSummaryOrPractices,i)))))
    
  }
  return(parcel_inputs)
}

get_pasture_inputs <- function(landUseSummaryOrPractices, grazing_factors, pasture_factors, farm_EnZ, grazing_yearly, my_logger, parcel_inputs, use_calculated_grazing){
  # Takes landUseSummaryOrPractices from farms collection
  # Extracts yield and residues left on site when grazing happened
  
  pasture_efficiency_potential_difference = unique(
    (grazing_factors %>% filter(pedo_climatic_area==farm_EnZ))$pasture_efficiency_potential_difference
  )
  
  # Dry weights
  dw_dry <- 1  # dry fraction of fully dehydrated plant material
  dw_fresh <- (pasture_factors %>% filter(grass == 'Generic annual grasses') %>% select(dw_fresh))[[1]]  # dry fraction of harvest weight
  
  pasture_inputs = data.frame()
  
  parcel_names <- parcel_inputs$parcel_ID
  
  year_strings <- paste0('year', 0:10)
  
  for (i in c(1:length(parcel_names))){
    
    year0_is_AMP <- landUseSummaryOrPractices[[1]][['year0']]$adaptiveMultiPaddockGrazing[i]
    if(is.na(year0_is_AMP)) {year0_is_AMP <- FALSE} # Workaround if value is missing. Should not be allowed. To be enforced at data collection.
    
    year0_since_years <- landUseSummaryOrPractices[[1]][['year0']]$applyingThesePracticesInYears[i]
    
    if(year0_since_years==""){
      log4r::error(my_logger, "Number of years that practices have been applied until now is NOT entered.")
    } else {
      baseline_since_years = missing_to_zero(year0_since_years)
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
      year_str <- year_strings[j+1]
      year_chosen <- landUseSummaryOrPractices[[1]][[year_str]]
      year_is_AMP <- year_chosen$adaptiveMultiPaddockGrazing[i]
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
      if (year_chosen$landUseType[i] == 'Arablecrops') { next }
      
      # monthly yield and residue
      monthly_nonarables <- data.frame(grazing=rep(0,12),residue=rep(0,12))
      for (k in c(1:12)){
        monthly_nonarables$grazing[k] <- missing_to_zero(year_chosen$grazingYield[i][[1]][[k]])
        monthly_nonarables$residue[k] <- missing_to_zero(year_chosen$estimationAfterResidueGrazingHarvest[i][[1]][[k]])
        monthly_nonarables$harvest[k] <- missing_to_zero(year_chosen$harvestYield[i][[1]][[k]])
      }
      
      # If use_calculated_grazing is TRUE total grazing yield is calculated here, replacing reported values.
      if (use_calculated_grazing){
        
        grazing_table_temp <- total_grazing_table %>% filter(scenario==paste0('year', j))
        grazing_calc <- grazing_table_temp$grazing_calc
        fodder_eaten <- grazing_table_temp$fodder_eaten
        grazing_calc_pastures <- grazing_table_temp$grazing_calc_pastures
        balegrazing_pastures_reported <- grazing_table_temp$pasture_weighted_bale_grazing
        grazing_total_reported <- grazing_table_temp$grazing_total
        grazing_pastures_reported <- grazing_table_temp$grazing_non_arable_lands
        
        if (fodder_eaten > grazing_calc){
          log4r::error(my_logger,"WARNING ! Bale grazing alone overcomes expected grazing needs, to be checked.")
          stop("Error related to bale grazing numbers. See log file..")
        }
        if (grazing_total_reported == 0){
          yearly_grazing_per_ha <- (grazing_calc - fodder_eaten) / sum(parcel_inputs$area)
          monthly_nonarables$grazing <- c(1/2 *yearly_grazing_per_ha, rep(0,5), 1/2 * yearly_grazing_per_ha, rep(0,5))
        } else {
          # grazing arbitrarily equally distributed over time weighted by parcel grazing yield relatively to farm level, if known
          expected_grazing_on_pasture <- (grazing_calc_pastures - balegrazing_pastures_reported)
          if(expected_grazing_on_pasture == 0) {
            monthly_nonarables$grazing <- rep(0, 12)
          } else {
            grazing_on_parcel_fraction <- sum(monthly_nonarables$grazing) * parcel_inputs$area[i] / grazing_pastures_reported
            yearly_grazing_per_ha <-  expected_grazing_on_pasture * grazing_on_parcel_fraction / parcel_inputs$area[i]
            monthly_nonarables$grazing <- c(1/2 * yearly_grazing_per_ha, rep(0,5), 1/2 * yearly_grazing_per_ha, rep(0,5))
          }
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
      
      # Get fresh or dry value.
      dryOrFresh <- year_chosen$yieldsResiduesDryOrFresh[i]
      if(is.null(dryOrFresh)) dryOrFresh <- NA
      if (!(dryOrFresh %in% c("Dry", "Fresh"))){
        if(i == 1) log4r::info(my_logger, paste0("WARNING: dryOrFresh value not found for year ", j,". Assuming Fresh."))
        dryOrFresh <- "Fresh"
      }
      if(dryOrFresh == 'Fresh') { dw <- dw_fresh } else { dw <- dw_dry }
      
      pasture_temp <- data.frame(scenario = c(paste0('year',j)),
                                    parcel_ID = parcel_names[i], 
                                    grass = "Generic grasses",
                                    perennial_frac = AMP_years_current * 0.02,
                                    n_fixing_frac = 0, # WARNING: TO BE AUTOMATED FOR CO2-EMISSION BALANCE 
                                    grazing = sum(monthly_nonarables$grazing), 
                                    residue = sum(monthly_nonarables$residue), 
                                    harvest = sum(monthly_nonarables$harvest),
                                    agb_peak = agb_peak, 
                                    pasture_efficiency = pasture_efficiency,
                                    AMP_baseline_factor = AMP_baseline_factor,
                                    dry_weight = dw
                                    )
      
      pasture_inputs <- rbind(pasture_inputs, pasture_temp)
      
    }
  }
  
  # Calculate the dry weights
  pasture_inputs <- pasture_inputs %>% mutate(
    dry_harvest = harvest * dry_weight,
    dry_grazing = grazing * dry_weight,
    dry_residue = residue * dry_weight,
    dry_agb_peak = agb_peak * dry_weight
  )
  
  if(length(pasture_inputs)==0) {
    pasture_inputs <- expand_grid(
      scenario = year_strings, parcel_ID = parcel_names, grass = "Generic grasses", perennial_frac = 0, 
      n_fixing_frac = 0, grazing = 0, residue = 0,
      harvest = 0, agb_peak = 0, dry_grazing = 0, dry_residue = 0,
      dry_harvest = 0, dry_agb_peak = 0, pasture_efficiency = 0, dry_weight =0
    )
  }
  
  # Set baseline to be equal to year0
  pasture_inputs <- rbind(pasture_inputs, pasture_inputs %>% filter(scenario=='year0') %>% mutate(scenario='baseline'))
  
  return(pasture_inputs)
}

get_soil_inputs = function(landUseSummaryOrPractices, soilAnalysis, soilMapsData){
  # takes landUseSummaryOrPractices from farms collection
  # extracts parcels input dataframe 
  parcel_names <- landUseSummaryOrPractices[[1]]$parcelName
  soil_inputs = data.frame(parcel_ID = c(), scenario = c(), clay = c(), irrigation=c())
  for (i in c(1:length(parcel_names))){
    for (j in c(0:10)){
      year_str <- paste0('year', j)
      soil_inputs <- rbind(soil_inputs,data.frame(
        parcel_ID = c(parcel_names[i]),
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
          parcel_ID = c(parcel_names[i]),
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
  parcel_names <- landUseSummaryOrPractices[[1]]$parcelName
  tilling_factor = (tilling_factors %>% filter(pedo_climatic_area == farm_EnZ))$tilling_factor
  minimum_tillage_factor = (tilling_factors %>% filter(pedo_climatic_area == farm_EnZ))$minimum_tillage_factor
  tilling_inputs = data.frame(parcel_ID = c(), scenario = c(), tilling_factor = c())
  for (i in c(1:length(parcel_names))){
    tilling_inputs <- rbind(tilling_inputs, data.frame(
      parcel_ID = c(parcel_names[i]), 
      scenario = c('baseline'),
      tilling_factor = c(tilling_factor))) # assumption
    for (j in c(0:10)){
      year_str <- paste0('year', j)
      year_chosen = landUseSummaryOrPractices[[1]][[year_str]]
      for (k in c(1:12)){
        tilling_inputs <- rbind(tilling_inputs, data.frame(
          parcel_ID = c(parcel_names[i]), 
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
