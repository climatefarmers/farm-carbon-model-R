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

get_mean_longitude <- function(coordinates) {
  longitude = c()
  for (i in coordinates){
    longitude <- append(longitude,i[[1]][1])
  }
  return(mean(longitude))
}

get_mean_latitude <- function(coordinates) {
  latitude = c()
  for (i in coordinates){
    latitude <- append(latitude,i[[2]][1])
  }
  return(mean(latitude))
}

get_in_farm_livestock_table<- function(monitoringData, periods, factors_animals) {
  
  # Data frame for livestock inputs
  
  in_farm_livestock_table <- data.frame()
  
  for (year in monitoringData$yearlyFarmData) {
    for (species in year$livestock$inFarm) {
      for (category in species$category) {
        
        in_farm_livestock_table_temp <- data.frame(
          year               = year$year,
          species            = species$species, #mandatory, no null values
          category           = category$name, #mandatory, no null values
          amount             = ifelse (category$amount == -9999,0,category$amount), 
          fertilityRate      = ifelse(species$fertilityRate == -9999, 0, species$fertilityRate), 
          grazing_days       = ifelse (species$grazingDays == -9999, 0, species$grazingDays),
          grazing_management = year$livestock$grazingManagement, #mandatory, no null values
          manure_treatment   = species$manureTreatment #mandatory, no null values
        )
        
        in_farm_livestock_table <- bind_rows(in_farm_livestock_table, in_farm_livestock_table_temp)
      }
    }
  }
  
  in_farm_livestock_table <- left_join(in_farm_livestock_table, factors_animals, by = c("species", "category")) %>% 
    mutate(grazing_days = ifelse(grazing_days > days_on_farm_per_year, days_on_farm_per_year, grazing_days))
  
  # TO-DO: Write error message with logr4
  if (sum(is.na(in_farm_livestock_table)) > 0) {
    stop("Error: there are NA values in in_Farm_livestocck_inputs")
  }
  in_farm_livestock_table <- left_join(in_farm_livestock_table, periods, by = "year")
  
  return(in_farm_livestock_table)
  # year_strings <- paste0("year", 0:10)
  # 
  # animals = expand.grid(
  #   scenario = year_strings, 
  #   species = factors_animals$species, 
  #   n_animals = 0, 
  #   grazing_days = 0
  # )
  # 
  # for (y in c(0:10)){
  #   year_str <- year_strings[y+1]
  #   if(y == 0) {
  #     livestock_scenario = livestock[["currentManagement"]][[1]]
  #   } else {
  #     livestock_scenario = livestock[["futureManagement"]][[1]][[year_str]] 
  #   }
  #   n_animals <- missing_to_zero(livestock_scenario$numberOfHeads)
  #   grazing_days <- missing_to_zero(livestock_scenario$grazingOrPasturedDaysPerYear)
  #   for (k in c(1:nrow(livestock_scenario))){
  #     species <- livestock_scenario$species[k]
  #     if (is.na(species)) {next}  # This line should not be needed if data is input correctly. Consider eventual removal.
  #     ind <- animals$scenario == year_str & animals$species == species
  #     animals$n_animals[ind] <- animals$n_animals[ind] + n_animals[k]  # adding to deal with current situation that same species can be given more than once.
  #     animals$grazing_days[ind] <- max(animals$grazing_days[ind], grazing_days[k])  # This line doesn't work well with mutliple entries of same species with different days grazing. Using max otherwise entries with 0 nulify all.
  #   }
  # }
  # animals <- animals[animals$n_animals!=0, ] # removing all rows that do not apply.
  # 
  # # Limit the grazing days to not be higher than days on farm (this should be dealt with at dashboard level)
  # animals <- merge(animals, factors_animals[,c('species', 'days_on_farm_per_year')])
  # ind <- animals$grazing_days > animals$days_on_farm_per_year
  # animals$grazing_days[ind] <- animals$days_on_farm_per_year[ind] 
  # 
  # return(animals)
}

get_out_farm_livestock_table <- function(monitoringData, periods, factors_animals) {
  
  # Data frame that only contains species, e.g. cattle, swine, chicken etc.  
  species <- data.frame()
  
  # Data frame for out farm livestock inputs
  out_farm_livestock_table <- data.frame()
  
  # For not full data set
  for (year in monitoringData$yearlyFarmData) {
    for (type in unique(factors_animals$species)) {
      species_temp <- data.frame(
        year    = year$year,
        species = type
      )
      species <- bind_rows(species, species_temp)
    }
    for (out_farm_species in year$livestock$outFarm) {
      out_farm_livestock_table_temp <- data.frame(
        year         = year$year,
        species      = ifelse (is_null(out_farm_species$species), NA, out_farm_species$species), # not mandatory
        amount       = ifelse (out_farm_species$amount == -9999, 0, out_farm_species$amount), # not mandatory
        grazing_days = ifelse (out_farm_species$grazingDays == -9999, 0, out_farm_species$grazingDays), # not mandatory
        grazing_management = year$livestock$grazingManagement
      )
      out_farm_livestock_table <- bind_rows(out_farm_livestock_table, out_farm_livestock_table_temp)
    }
  }
  out_farm_factors_animals <- factors_animals %>%
    filter(category == "general")
  out_farm_livestock_table <- right_join(out_farm_livestock_table, species, by = c("year", "species")) %>%
    mutate(amount = ifelse (is.na(amount), 0, amount)) %>%
    mutate(grazing_days = ifelse (is.na(grazing_days), 0, grazing_days))
  out_farm_livestock_table <- left_join(out_farm_livestock_table, out_farm_factors_animals, by = "species")
  
  return(out_farm_livestock_table)
}

## Function to extract expected grazing from livestock and fodder from the whole farm over all years
# Definitions:
# Fodder: hay and straw that was additionally applied to the parcel to feed animals
# Grazing: amount of biomass that is eaten by animals
# forage: amount of growing biomass (on the parcel) that is eaten by the animals
get_grazing_table <- function(monitoringData, periods, fixed_parcel_inputs, in_farm_livestock_table, out_farm_livestock_table, orgamendments_inputs){

  # Data frame for yearly grazing inputs
  grazing_yearly <- data.frame()
  
  # Data frame for monthly grazing inputs
  grazing_monthly <- data.frame()
  
  # Loop through years and parcels to extract yearly fodder and monthly grazing information
  for (year in monitoringData$yearlyFarmData){
        for (parcel in year$parcelLevelData){
          
      # Data extraction of monthly grazing information
      for (month in c(1:12)){
        was_grazed_month <- ifelse(parcel$yearParcelData$grazingMonthlyEvent[month] == TRUE, 1, 0)
        grazing_monthly_temp <- data.frame(
          year = year$year,
          parcel_name = parcel$parcelFixedValues$parcelName,
          month = month,
          was_grazed_month = was_grazed_month
        )
        grazing_monthly <- bind_rows(grazing_monthly, grazing_monthly_temp)
      }
    }
  }
  
  ### PARCEL INFORMATION & FODDER
  ## Extract parcel information and fodder information from organamendment inputs and fixed parcel inputs
  grazing_yearly_temp <- orgamendments_inputs %>%
    filter(type == "fodder") %>%
    select(year, parcel_name, fodder = amount, fodder_imported_frac = imported_frac) %>%
    left_join(select(fixed_parcel_inputs, c("parcel_name", "area", "area_units")), by = "parcel_name")
  
  ### GRAZING NEEDS
  ## Calculate total grazing needs for in and out farm animals based on animal amounts and grazing days
  in_farm_yearly_grazing_needs <- in_farm_livestock_table %>%
    select(year, category, amount, grazing_days, mass_kg_per_animal) %>%
    mutate(yearly_grazing_needs_tDM = amount * mass_kg_per_animal / 1000 * 0.025 * grazing_days) %>%
    group_by(year) %>%
    summarise(in_farm_yearly_grazing_needs = sum(yearly_grazing_needs_tDM)) # 0.025 is the 2.5% of animal mass required as feed every day
  
  out_farm_yearly_grazing_needs <- out_farm_livestock_table %>%
    select(year, species, amount, grazing_days, mass_kg_per_animal) %>%
    mutate(yearly_grazing_needs_tDM = amount * mass_kg_per_animal / 1000 * 0.025 * grazing_days) %>%
    group_by(year) %>%
    summarise(out_farm_yearly_grazing_needs = sum(yearly_grazing_needs_tDM)) # 0.025 is the 2.5% of animal mass required as feed every day
  
  # Sum up the calculated grazing of all animal types for each year
  yearly_grazing_needs <- in_farm_yearly_grazing_needs  %>% 
    mutate(out_farm_yearly_grazing_needs = out_farm_yearly_grazing_needs$out_farm_yearly_grazing_needs) %>%
    mutate(yearly_grazing_needs = in_farm_yearly_grazing_needs + out_farm_yearly_grazing_needs)
  
  ## Distribute the yearly calculated grazing needs to the different parcels
  # Identify parcels with grazing
  # Note that only parcels with grazing are considered here. 
  # A parcel with grazing is defined as a parcel that had at least one grazing event in a respective year.
  grazed_parcels <- grazing_monthly %>%
    group_by(year, parcel_name) %>%
    summarise(was_grazed_parcel = ifelse(sum(was_grazed_month) > 0, 1,0))
  
  # Get total area of all parcels with grazing and the respective fraction of a particular parcel per year
  grazing_area <- grazing_yearly_temp %>%
    merge(grazed_parcels, by = c("year", "parcel_name")) %>%
    filter(was_grazed_parcel == 1) %>%
    group_by(year) %>%
    distinct(parcel_name, .keep_all = TRUE) %>%
    mutate(area_grazed_total = sum(area)) %>%
    mutate(area_grazed_frac = area / area_grazed_total) %>%
    select(parcel_name, area, area_grazed_total, area_grazed_frac)
  
  ## Merge fodder and grazing information in one data frame
  grazing_yearly_temp <- left_join(grazing_yearly_temp, select(grazing_area, c("year", "parcel_name", "area_grazed_total", "area_grazed_frac")), by = c("year","parcel_name")) %>%
    left_join(yearly_grazing_needs, by = "year") %>%
    mutate(area_grazed_total = ifelse(is.na(area_grazed_total), 0, area_grazed_total)) %>%
    mutate(area_grazed_frac = ifelse(is.na(area_grazed_frac), 0, area_grazed_frac)) %>%
    # Grazing per parcel
    mutate(grazing_needs = yearly_grazing_needs * area_grazed_frac) %>%
    mutate(units_grazing = "tonnes")
  
  ### FORAGE AND RESIDUES
  ## Calculate forage and residue based on given fodder information and calculated grazing needs using the following equations:
  # if (fodder eaten <= grazing demand):
  # fodder_eaten = 0.85 * fodder
  # fodder_residue = 0.15 * fodder
  # forage = (grazing_needs - fodder_eaten)/0.85 #TO-DISCUSS -> Different assumption than in the old model: 
    # grazing_needs - fodder_eaten represents the forage_eaten (growing biomass that was eaten by animals)
    # total forage (total biomass that is growing?? term might be misleading) is calculated as the forage eaten divided by 0.85
    # forage residue is then derived from that as 0.15 * forage
  # forage_eaten = 0.85 * forage
  # forage_residue = 0.15 * forage
  # 
  # if (fodder eaten > grazing demand):
  # fodder_eaten = grazing_needs
  # fodder_residue = grazing_needs/0.85 * 0.15 + fodder - (fodder_eaten + fodder_residue)
  # forage_eaten = 0
  # forage_residue = 0
  # forage = 0
  
  grazing_yearly <- grazing_yearly_temp %>%
    mutate(fodder_eaten = fodder * 0.85) %>%
    mutate(fodder_eaten = ifelse (fodder_eaten <= grazing_needs, fodder_eaten, grazing_needs)) %>%
    mutate(fodder_residue = fodder * 0.15) %>%
    mutate(fodder_residue = ifelse (fodder_eaten <= grazing_needs, fodder_residue, grazing_needs/0.85 * 0.15 + fodder - (fodder_eaten + fodder_residue))) %>%
    # Forage
    mutate(forage = ifelse (fodder_eaten <= grazing_needs, (grazing_needs - fodder_eaten)/0.85, 0)) %>%
    mutate(forage_eaten = 0.85 * forage) %>%
    mutate(forage_residue = 0.15 * forage) %>%
    # Values per ha
    mutate(grazing_needs_ha = grazing_needs / area) %>%
    mutate(fodder_residue_ha = fodder_residue / area) %>%
    mutate(forage_ha = forage / area) %>%
    mutate(forage_residue_ha = forage_residue / area)
  
  ## Distribute yeary grazing per hectare to the respective months per parcel per year
  grazing_monthly <- grazing_monthly %>% 
    group_by(year, parcel_name) %>% 
    mutate (number_grazing_months = sum(was_grazed_month)) %>%
    merge(grazing_yearly %>% select(parcel_name, year, grazing_needs_ha, fodder_residue_ha, forage_ha, forage_residue_ha), by = c("parcel_name", "year")) %>%
    mutate(grazing_monthly_ha = ifelse(was_grazed_month == 1, grazing_needs_ha / number_grazing_months, 0)) %>%
    mutate(fodder_residue_monthly_ha = ifelse(was_grazed_month == 1, fodder_residue_ha / number_grazing_months, 0)) %>%
    mutate(forage_monthly_ha = ifelse(was_grazed_month == 1, forage_ha / number_grazing_months, 0)) %>%
    mutate(forage_residue_monthly_ha = ifelse(was_grazed_month == 1, forage_residue_ha / number_grazing_months, 0)) %>%
    select(-grazing_needs_ha,-forage_residue_ha,-fodder_residue_ha,-forage_ha) # maybe keep columns for understanding
  
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
get_orgamendments_inputs = function(monitoringData, periods) {

  # Data frame for all OM subtypes to ensure data availability for all years and parcels
  OM_subtypes <- data.frame(
    type = c("biochar", 
             "manure", "manure", "manure", "manure", "manure", "manure", 
             "compost", "compost", "compost", "compost",
             "mulch", "mulch", "mulch", "mulch",
             "slurry", "slurry", "slurry",
             "other",
             "fodder"),
    sub_type = c("-", 
                 "cattle manure", "chicken manure", "horse manure", "pig manure", "sheep manure", "-", 
                 "green/plant-based compost", "manure-based compost", "mushroom compost", "-",
                 "hay/straw", "leaves", "wood bark/wood chips", "-",
                 "cattle slurry", "pig slurry", "-",
                 "-",
                 "-")
  )
  
  # Data frame for fianl OM inputs
  final_OM_inputs <- data.frame()
  
  # Data frame for imported OM inputs
  imported_OM_inputs <- data.frame()
  
  # Data frame for added OM inputs
  added_OM_inputs <- data.frame()
  
  for (year in monitoringData$yearlyFarmData) {
    
    for (amendment in year$importedOrganicMatter) {
      imported_OM_inputs_temp <- data.frame(
        year             = year$year,
        type             = amendment$type,
        imported_percent = amendment$percentImported,
        imported_frac    = amendment$percentImported/100
      )
      imported_OM_inputs <- bind_rows(imported_OM_inputs, imported_OM_inputs_temp)
    }
    
    for (parcel in year$parcelLevelData) {
      
      for (amendment in parcel$yearParcelData$addedOrganicMatter) {
        added_OM_inputs_temp <- data.frame(
          year        = year$year,
          parcel_name = parcel$parcelFixedValues$parcelName,
          parcel_ID   = parcel$parcelFixedValues$parcelID,
          type        = ifelse (is_null (amendment$type), NA, amendment$type),
          sub_type    = ifelse (is_null(amendment$subType), NA, amendment$subType),
          other       = ifelse (is_null(amendment$other), NA, amendment$other),
          amount      = ifelse (amendment$amount < 0, "0", amendment$amount),
          units       = ifelse (is_null (amendment$units), NA, amendment$units),
          area        = fixed_parcel_inputs$area[fixed_parcel_inputs$parcel_name == parcel$parcelFixedValues$parcelName],
          amount_ha   = ifelse (amendment$amount < 0, "0", amendment$amount/fixed_parcel_inputs$area[fixed_parcel_inputs$parcel_name == parcel$parcelFixedValues$parcelName])
        )
        added_OM_inputs <- bind_rows(added_OM_inputs, added_OM_inputs_temp)
      }
      
      for (i in 1:length(OM_subtypes$sub_type)) {
        final_OM_inputs_temp <- data.frame(
          year        = year$year,
          parcel_name = parcel$parcelFixedValues$parcelName,
          parcel_ID   = parcel$parcelFixedValues$parcelID,
          type        = OM_subtypes$type[i],
          sub_type    = OM_subtypes$sub_type[i]
        )
        final_OM_inputs <- bind_rows(final_OM_inputs, final_OM_inputs_temp)
      }
    }
  }
  
  final_OM_inputs <- left_join(final_OM_inputs, added_OM_inputs, by = c("year", "parcel_name", "parcel_ID", "type", "sub_type")) %>% 
    mutate(amount = ifelse(is.na(amount), 0, amount))
  final_OM_inputs <- left_join(final_OM_inputs, imported_OM_inputs, by = c("year", "type"))
  final_OM_inputs <- left_join(final_OM_inputs, periods, by = c("year"))
  
  return(final_OM_inputs)
    
  
  # parcel_names <- landUseSummaryOrPractices[[1]]$parcelName
  # orgamendments_inputs = data.frame(parcel_ID = c(), scenario = c(), source = c(), 
  #                                   quantity_t_ha = c(), imported_frac = c(), remaining_frac = c())
  
  # for (i in c(1:length(parcel_names))){
  #   for (j in c(0:10)){
  #     year_str <- paste0('year', j)
  #     year_chosen = landUseSummaryOrPractices[[1]][[year_str]]
      
      # Data check and correction (should not be necessary: data checks should be done upstream)
      # manureApplication <- missing_to_zero(year_chosen$manureApplication[i])
      # if(is.na(manureApplication) | manureApplication < 0) { manureApplication <- 0 }
      # compostApplication <- missing_to_zero(year_chosen$compostApplication[i])
      # if(is.na(compostApplication) | compostApplication < 0) { compostApplication <- 0 }
      # hayStrawApplication <- missing_to_zero(year_chosen$hayStrawApplication[i])
      # if(is.na(hayStrawApplication) | hayStrawApplication < 0) { hayStrawApplication <- 0 }
      # percentManureImported <- missing_to_na(year_chosen$percentManureImported[i])
      # if(is.null(percentManureImported)) {percentManureImported <- 100}
      # if(is.na(percentManureImported)) {percentManureImported <- 100}
      # percentCompostImported <- missing_to_na(year_chosen$percentCompostImported[i])
      # if(is.null(percentCompostImported)) {percentCompostImported <- 100}
      # if(is.na(percentCompostImported)) {percentCompostImported <- 100}
      # percentHayStrawImported <- missing_to_na(year_chosen$percentageOfHayStrawImported[i])
      # if(is.null(percentHayStrawImported)) {percentHayStrawImported <- 100}
      # if(is.na(percentHayStrawImported)) {percentHayStrawImported <- 100}
      # isBaleGrazing <- year_chosen$baleGrazing[i]
      # if(is.null(isBaleGrazing)) {isBaleGrazing <- FALSE}
      # if(is.na(isBaleGrazing)) {isBaleGrazing <- FALSE}
      # baleResidue <- missing_to_zero(year_chosen$residueLeftAfterBaleGrazing[i])
      # if(is.null(baleResidue)) {baleResidue <- NA}
      # if(is.na(baleResidue)) {baleResidue <- 15} # 15 is the default % residue left after grazing 
      
      # # Manure (animal dung)
      # orgamendments_temp <- data.frame(
      #   parcel_ID = parcel_names[i], 
      #   scenario = year_str, 
      #   source = "Other Cattle", # AN UNFOLDING LIST OF MANURE TYPE MIGHT HAVE TO BE ADDED TO UI
      #   quantity_t_ha = manureApplication,
      #   imported_frac = percentManureImported/100,
      #   remaining_frac = 1
      # )
      # orgamendments_inputs <- rbind(orgamendments_inputs, orgamendments_temp)
      # 
      # # Compost
      # orgamendments_temp <- data.frame(
      #   parcel_ID = parcel_names[i], 
      #   scenario = year_str, 
      #   source = "Green compost",
      #   quantity_t_ha = compostApplication,
      #   imported_frac = percentCompostImported/100,
      #   remaining_frac = 1
      # )
      # orgamendments_inputs <- rbind(orgamendments_inputs, orgamendments_temp)
      # 
      # # Hay. Warning: this used as the value for bale hay/fodder. Should be changed to straw amendments and fodder done separately
      # orgamendments_temp <- data.frame(
      #   parcel_ID = parcel_names[i], 
      #   scenario = year_str, 
      #   source = "Hay",
      #   quantity_t_ha = hayStrawApplication,
      #   imported_frac = percentHayStrawImported/100,
      #   remaining_frac = ifelse(isBaleGrazing, baleResidue/100, 1)
      # )
      # orgamendments_inputs <- rbind(orgamendments_inputs, orgamendments_temp)
      
  #   }
  #   
  # }
  # 
  # orgamendments_inputs <- rbind(orgamendments_inputs, orgamendments_inputs%>%
  #                                 filter(scenario=='year0')%>%
  #                                 mutate(scenario='baseline')) # Manure addition baseline is based on previous years
  # return(orgamendments_inputs)
}


get_tree_inputs = function(landUseSummaryOrPractices){
  # takes landUseSummaryOrPractices from farms collection
  # extracts agroforestry inputs dataframe 
  parcel_names <- landUseSummaryOrPractices[[1]]$parcelName
  tree_inputs = data.frame(parcel_ID = c(), scenario = c(), tree_species = c(), other_name = c(), dbh = c(),
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
          tree_inputs <- rbind(tree_inputs,data.frame(
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
            tree_inputs <- rbind(tree_inputs,data.frame(
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
  NA_rows = nrow(tree_inputs)-nrow(na.omit(tree_inputs))
  if(NA_rows>0){
    log4r::error(my_logger, paste('WARNING: ',NA_rows,' rows contained NAs in tree_inputs.', paste=''))
  }
  return(na.omit(tree_inputs))
}


get_animal_inputs = function(grazing_yearly, in_farm_livestock_table, out_farm_livestock_table, periods){
  # takes landUseSummaryOrPractices & livestock from farms collection
  # extracts animal inputs dataframe
  
  # Data farme for animal inputs from manure
  animal_inputs <- data.frame()
  
  # Full data frame for in farm animal inputs
  in_farm_animal_inputs_temp <- in_farm_livestock_table %>%
    select(year, species, category, amount, grazing_days, grazing_management, manure_treatment, c_kg_per_year_per_animal) %>%
    merge(select(grazing_yearly,c("year", "parcel_name", "area", "yearly_grazing_needs", "grazing_needs")), by = "year") %>%
    rename(grazing_total = yearly_grazing_needs, grazing_parcel = grazing_needs) %>%
    # Number of animals weighted by grazing per parcel
    mutate(amount_animals_parcel = amount * grazing_parcel / grazing_total) %>%
    select(1,2,3,4,9,12,11,13,5,6,7,8,9,10)
  
  # Reduced data frame for in farm animal inputs
  in_farm_animal_inputs <- in_farm_animal_inputs_temp %>%
    select(year, parcel_name, species, category, amount_animals_parcel, grazing_days, grazing_management, manure_treatment, c_kg_per_year_per_animal, area)
  
  # Full data frame for out farm animal inputs
  out_farm_animal_inputs_temp <- out_farm_livestock_table %>%
    select(year, species, amount, grazing_days, grazing_management, c_kg_per_year_per_animal) %>%
    merge(select(grazing_yearly,c("year", "parcel_name", "area", "yearly_grazing_needs", "grazing_needs")), by = "year") %>%
    # Number of animals weighted by grazing per parcel
    rename(grazing_total = yearly_grazing_needs, grazing_parcel = grazing_needs) %>%
    mutate(amount_animals_parcel = amount * grazing_parcel / grazing_total)  
  
  # Reduced data frame for out farm animal inputs
  out_farm_animal_inputs <- out_farm_animal_inputs_temp %>%
    select(year, parcel_name, species, amount_animals_parcel, grazing_days, grazing_management, c_kg_per_year_per_animal, area)
  
  # Combine in farm and out farm animal inputs
  animal_inputs <- bind_rows(in_farm_animal_inputs, out_farm_animal_inputs) %>%
    mutate(category = ifelse(is.na(category), "general", category)) %>%
    left_join(periods, by = "year")

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


get_crop_inputs <- function(monitoringData, parcel_inputs, factors_crops, grazing_yearly, grazing_monthly, periods){
  
  year_strings <- paste0("year", 0:10)
  parcel_names <- parcel_inputs$parcel_ID
  
  crop_inputs <- data.frame()
  
  for (j in c(0:10)){ # cycle through years
    
    year_str <- paste0('year', j)
    year_chosen = landUseSummaryOrPractices[[1]][[year_str]]
    
    for (i in c(1:length(parcel_names))){
      
      parcel <- parcel_names[i]
      
      # Excluding non-arable parcels: no project grazing compatible land-uses (no pasture efficiency coef will be used in crop inputs)
      if (!year_chosen$landUseType[i]=="Arablecrops") { next }
      
      # Creating the data frame storing monthly yield and residue
      monthly_harvest = data.frame(
        crop = rep(NA, 12), 
        coverCrop = rep(NA, 12), 
        productiveFallow = rep(NA, 12),
        harvest = rep(NA, 12),
        residue = rep(NA, 12)
      )
      
      # Getting input data (grazing was calculated previously)
      monthly_harvest$crop <- get_monthly_cash_crop(parcel_index = i, year_chosen)
      monthly_harvest$coverCrop <- year_chosen$coverCropMonthlyData[[i]]
      monthly_harvest$productiveFallow <- year_chosen$productiveFallow[[i]]
      monthly_harvest$grazing <- grazing_monthly$grazing_final[grazing_monthly$parcel == parcel & grazing_monthly$year == j]
      monthly_harvest$harvest <- missing_to_zero(year_chosen$harvestYield[[i]])
      monthly_harvest$residue <- missing_to_zero(year_chosen$estimationAfterResidueGrazingHarvest[[i]])
      
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
          crop_chosen <- "Generic Plant Mixture"
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
    crop_inputs <- merge(x = crop_inputs, y = factors_crops %>% select(crop, dw_fresh), by = "crop", all.x = TRUE)
    crop_inputs$dw_fresh[is.na(crop_inputs$dw_fresh)] <- factors_crops$dw_fresh[factors_crops$crop == "Other"] # Using option 'Other' if no crop match was found
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
      crop = "Generic Plant Mixture",
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
## Keeping for potential use
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


get_fertilizer_inputs = function(monitoringData, periods) {

  # Data frame for fertilzer inputs
  fertilizer_inputs = data.frame()
  
  for (year in monitoringData$yearlyFarmData) {
    for (entry in year$fertilizerUsage) {
      
      fertilizer_inputs_temp <- data.frame(
        year      = year$year,
        amount    = ifelse(entry$amount == -9999, 0, entry$amount),
        units     = ifelse(is_null(entry$units), "-", entry$units),
        percent_n = ifelse(entry$percentN == -9999, 0, entry$percentN)
      )
      fertilizer_inputs <- bind_rows(fertilizer_inputs, fertilizer_inputs_temp)
    }
  }
  
  fertilizer_inputs <- left_join(fertilizer_inputs, periods, by = "year")
  
  return (fertilizer_inputs)
  
  # for (i in c(1:length(landUseSummaryOrPractices[[1]]$parcelName))){
  #   for (j in c(0:10)){
  #     
  #     parcel_id <- landUseSummaryOrPractices[[1]]$parcelName[i]
  #     year_str <- paste0('year',j)
  #     
  #     # Determine the field area depending on input source
  #     use_manual_area <- landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i]
  #     if(is.na(use_manual_area) |  is.null(use_manual_area) | !use_manual_area) {
  #       field_area <- missing_to_zero(landUseSummaryOrPractices[[1]]$area[i])/10000 # if no corrected value was provided by the farmer
  #     } else {
  #       field_area <- missing_to_zero(landUseSummaryOrPractices[[1]]$manuallyEnteredArea[i])/10000 # add a verification of consistency here?
  #     }
  #     
  #     year_chosen = landUseSummaryOrPractices[[1]][[year_str]]
  #     
  #     fertilizer_temp <- data.frame(
  #       parcel_ID = parcel_id, 
  #       field_area = field_area,
  #       scenario = year_str,
  #       usage_boolean = year_chosen$syntheticFertilizer$usage[i],
  #       fertilizer_type = "synthetic", # here gathering data from the synthetic fertilizer dashboard entry
  #       quantity_t_ha = ifelse(year_chosen$syntheticFertilizer$usage[i]==TRUE, missing_to_zero(year_chosen$syntheticFertilizer$tonsPerYear[i]),0),
  #       n_content_perc=ifelse(year_chosen$syntheticFertilizer$usage[i]==TRUE, missing_to_zero(year_chosen$syntheticFertilizer$percentOfNitrogen[i]),0)
  #     )
  #     fertilizer_inputs <- rbind(fertilizer_inputs, fertilizer_temp)
  #     
  #     if (j==0){
  #       fertilizer_inputs <- rbind(fertilizer_inputs,data.frame(
  #         parcel_ID = parcel_id, 
  #         field_area = ifelse(is.null(landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i]),
  #                             c(missing_to_zero(landUseSummaryOrPractices[[1]]$area[i])/10000),
  #                             ifelse(is.na(landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i]) |
  #                                      landUseSummaryOrPractices[[1]]$usingManuallyEnteredArea[i] == FALSE, # means that no corrected value was provided by the farmer
  #                                    c(missing_to_zero(landUseSummaryOrPractices[[1]]$area[i])/10000),
  #                                    c(missing_to_zero(landUseSummaryOrPractices[[1]]$manuallyEnteredArea[i])/10000))), # add a verification of consistence here
  #         scenario = c("baseline"),
  #         usage_boolean = year_chosen$syntheticFertilizer$usage[i],
  #         fertilizer_type = "synthetic", # here gathering data from the synthetic fertilizer dashboard entry
  #         quantity_t_ha = ifelse(year_chosen$syntheticFertilizer$usage[i]==TRUE, missing_to_zero(year_chosen$syntheticFertilizer$tonsPerYear[i]),0),
  #         n_content_perc=ifelse(year_chosen$syntheticFertilizer$usage[i]==TRUE, missing_to_zero(year_chosen$syntheticFertilizer$percentOfNitrogen[i]),0)))
  #     }
  #     last_index = nrow(fertilizer_inputs)
  #     if (fertilizer_inputs$usage_boolean[last_index]==TRUE){
  #       if (fertilizer_inputs$quantity_t_ha[last_index]==0){
  #         list_missing_data = c(list_missing_data,paste(fertilizer_inputs$parcel_ID[last_index],
  #                                                       ' (year',j,"): quantity_t_ha missing", sep=""))
  #       }
  #       if (fertilizer_inputs$n_content_perc[last_index]==0){
  #         list_missing_data = c(list_missing_data,paste(fertilizer_inputs$parcel_ID[last_index],
  #                                                       ' (year',j,"): n_content_perc missing", sep=""))
  #       }
  #     }
  #   }
  # }
  # if (length(list_missing_data)>0){
  #   log4r::error(my_logger, paste('WARNING: Fertilizer data: ',list(list_missing_data),'.', paste=''))
  # }
  # return(fertilizer_inputs)
}


get_fuel_inputs_direct = function(monitoringData, periods){
  
  # Data frame for direct fuel inputs
  fuel_inputs_direct <- data.frame()
  
  # For loop to extract fuel inputs
  # Data entry mandatory. That's why is_null request is not necessary.
  for (year in monitoringData$yearlyFarmData) {
    for (fuel_index in 1: length(year$fuelUsage$direct)) {
      
      fuel_inputs_direct_temp <- data.frame(
        year          = year$year, 
        typeOfFuel    = ifelse(fuel_index == 1, "diesel", "petrol"),
        amount        = year$fuelUsage$direct[[fuel_index]]$amount,
        units         = year$fuelUsage$direct[[fuel_index]]$units
      )
      fuel_inputs_direct <- bind_rows(fuel_inputs_direct, fuel_inputs_direct_temp)
    }
  }
  
  fuel_inputs_direct <- left_join(fuel_inputs_direct, periods, by = "year")
  
  return(fuel_inputs_direct)
}

get_fuel_inputs_indirect = function(monitoringData, periods) {
  
  # Indirect fuel inputs data frame 
  fuel_inputs_indirect <- data.frame()
  
  # For loop to extract fuel inputs
  for (year in monitoringData$yearlyFarmData) {
    for (service in year$fuelUsage$indirect) {
      
      year_temp <- year$year
      service_temp <- ifelse (is_null(service$service), NA, service$service) # not mandatory 
      area_temp <- ifelse (service$area$amount == -9999, 0, service$area$amount) # not mandatory
      units_temp <- ifelse (is_null(service$area$units), NA, service$area$units) # not mandatory
      service_detail_temp <- ifelse (is_null(service$serviceDetail), NA, service$serviceDetail) # not mandatory
      service_category_temp <- ifelse (is_null(service$serviceCategory), NA, service$serviceCategory) # not mandatory
      
      fuel_inputs_indirect_temp <- data.frame(
        year             = year_temp, 
        service          = service_temp,
        area             = area_temp,
        units            = units_temp,
        servie_detail    = service_detail_temp,
        service_category = service_category_temp
      )
      
      fuel_inputs_indirect <- bind_rows(fuel_inputs_indirect, fuel_inputs_indirect_temp)
    }
  }
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

get_fixed_farm_inputs <- function(monitoringData) {
  
  fixed_farm_inputs <- data.frame(farm_id            = monitoringData$farmId,
                                  email              = monitoringData$email,
                                  farm_id_cf         = monitoringData$uniqueCfFarmId,
                                  project_start_year = monitoringData$projectStartYear)
  return(fixed_farm_inputs)
  
}

get_fixed_parcel_inputs <- function(monitoringData) {
  
  # Data frame for fixed parcel inputs
  fixed_parcel_inputs = data.frame()
  
  # Loop through parcels and extract fixed parcel inputs
  for (parcel in monitoringData$yearlyFarmData[[1]]$parcelLevelData) {
    
    fixed_parcel_inputs_temp <- data.frame(
      parcel_name       = parcel$parcelFixedValues$parcelName, # mandatory entry
      parcel_ID         = parcel$parcelFixedValues$parcelID, # mandatory entry
      area_geo          = parcel$parcelFixedValues$areaGeoFile$area, #"mandatory" entry
      area_geo_units    = parcel$parcelFixedValues$areaGeoFile$units, #"mandatory" entry
      area_manual       = parcel$parcelFixedValues$areaManualEntry$area, # not mandatory entry; default = -9999
      area_manual_units = ifelse(is_null(parcel$parcelFixedValues$areaManualEntry$units), "-", parcel$parcelFixedValues$areaManualEntry$units), # not mandatory
      use_manual_area   = ifelse(parcel$parcelFixedValues$areaManualEntry$area != -9999, TRUE, FALSE), #TO-CHECK: Do we always use manual area if available?
      area              = ifelse(parcel$parcelFixedValues$areaManualEntry$area != -9999, 
                                parcel$parcelFixedValues$areaManualEntry$area/1000, 
                                parcel$parcelFixedValues$areaGeoFile$area/1000), # To-DO: Integrate unit check?
      area_units         = "hectares",
      longitude         = get_mean_longitude(parcel$parcelFixedValues$coordinates), 
      latitude          = get_mean_latitude(parcel$parcelFixedValues$coordinates)
    )
    
    fixed_parcel_inputs <- bind_rows(fixed_parcel_inputs, fixed_parcel_inputs_temp)
  }
  return(fixed_parcel_inputs)
}

## Function extracts baseline and project years based on the project start year
# Function needs refinement for the following cases
# - more than three years of data for the baseline (take only the three years right before the project start year)
# - count for baseline and project years baseline-01, baseline-02, baseline-03, project-01, project-02, project-03, ... 
get_periods <- function(monitoringData, project_start_year) {
  
  periods <- data.frame()
  
  count_baseline_years <- -3
  count_project_years <- 1
  
  for (year in monitoringData$yearlyFarmData) {
    if (year$year >= project_start_year - 3) {
      
      if (count_baseline_years < 0) {
        year <- year$year
        period <- count_baseline_years
        count_baseline_years = count_baseline_years + 1

      } else {
        year <- year$year
        period <- count_project_years
        count_project_years = count_project_years + 1
      }
      periods_temp <- data.frame(
        year = year,
        period = period
      )
      periods <- bind_rows(periods, periods_temp)
    }
  }
  
  return(periods)
}


get_pasture_inputs <- function(landUseSummaryOrPractices, grazing_factors, factors_pastures, farm_EnZ, grazing_yearly, grazing_monthly, my_logger, parcel_inputs){
  # Takes landUseSummaryOrPractices from farms collection
  # Extracts yield and residues left on site when grazing happened
  
  pasture_efficiency_potential_difference = unique(
    (grazing_factors %>% filter(pedo_climatic_area==farm_EnZ))$pasture_efficiency_potential_difference
  )
  
  # Dry weights
  dw_dry <- 1  # dry fraction of fully dehydrated plant material
  dw_fresh <- (factors_pastures %>% filter(grass == 'Generic annual grasses') %>% select(dw_fresh))[[1]]  # dry fraction of harvest weight
  
  pasture_inputs = data.frame()
  
  parcel_names <- parcel_inputs$parcel_ID
  
  year_strings <- paste0('year', 0:10)
  
  for (i in c(1:length(parcel_names))){
    
    parcel <- parcel_names[i]
    
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
      monthly_nonarables <- data.frame(grazing=rep(NA,12), residue=NA, harvest=NA)
      monthly_nonarables$grazing <- grazing_monthly$grazing_final[grazing_monthly$parcel == parcel & grazing_monthly$year == j]
      monthly_nonarables$residue <- missing_to_zero(year_chosen$estimationAfterResidueGrazingHarvest[i][[1]])
      monthly_nonarables$harvest <- missing_to_zero(year_chosen$harvestYield[i][[1]])
      
      ### building df for C inputs calculation
      
      yield_sums <- monthly_nonarables$grazing+monthly_nonarables$residue+monthly_nonarables$harvest
      
      if (farm_EnZ == "Mediterranean north" | farm_EnZ == "Mediterranean south"){ # env zones with 2 grass growing seasons
        winter_months = c(1,2,3,4,5,11,12) # month index
        summer_months = c(6,7,8,9,10) # month index
        agb_peak <- max(yield_sums[winter_months]) + max(yield_sums[summer_months])
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


get_tilling_inputs = function(landUseSummaryOrPractices, factors_tillage, farm_EnZ){ 
  # takes landUseSummaryOrPractices from farms collection, farm_country (from farmInfo) and tilling factors table
  # extracts tilling inputs dataframe 
  parcel_names <- landUseSummaryOrPractices[[1]]$parcelName
  tilling_factor = (factors_tillage %>% filter(pedo_climatic_area == farm_EnZ))$tilling_factor
  minimum_tillage_factor = (factors_tillage %>% filter(pedo_climatic_area == farm_EnZ))$minimum_tillage_factor
  tilling_inputs = data.frame(parcel_ID = c(), scenario = c(), tillage = c(), tilling_factor = c())
  for (i in c(1:length(parcel_names))){
    for (j in c(0:10)){
      year_str <- paste0('year', j)
      year_chosen = landUseSummaryOrPractices[[1]][[year_str]]
      if(any(year_chosen$tillingEvent[i][[1]])==TRUE){
        tf <- tilling_factor
        t <- "full tillage"
      } else if(any(year_chosen$minimumTillingEvent[i][[1]])==TRUE) {
        tf <- minimum_tillage_factor
        t <- "minimal tillage"
      } else {
        tf <- 1
        t <- "no tillage"
      }
      tilling_inputs <- rbind(tilling_inputs, data.frame(
        parcel_ID = c(parcel_names[i]), 
        scenario = c(year_str),
        tillage = t,
        tilling_factor = tf
      ))
      # for (k in c(1:12)){
      #   if(year_chosen$tillingEvent[i][[1]][[k]]==TRUE){
      #     tf <- tilling_factor
      #     t <- "full tillage"
      #   } else if(year_chosen$minimumTillingEvent[i][[1]][[k]]==TRUE) {
      #     tf <- minimum_tillage_factor
      #     t <- "minimal tillage"
      #   } else {
      #     tf <- 1
      #     t <- "no tillage"
      #   }
      #   tilling_inputs <- rbind(tilling_inputs, data.frame(
      #     parcel_ID = c(parcel_names[i]), 
      #     scenario = c(year_str),
      #     tillage = t,
      #     tilling_factor = tf
      #     ))
      # }
    }
  }
  # tilling_inputs = tilling_inputs %>% group_by(parcel_ID, scenario) %>%
  #   summarise(tilling_factor = max(tilling_factor)) # ATM JUST TAKE THE MAX IMPACT EVENT
  tilling_inputs <- rbind(tilling_inputs, tilling_inputs %>% filter(scenario=='year0') %>% mutate(scenario='baseline'))
  return(tilling_inputs)
}
