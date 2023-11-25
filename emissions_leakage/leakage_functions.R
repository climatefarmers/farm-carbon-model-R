# leakage functions

manure_leakage <- function(orgamendments_data){
  
  if(nrow(orgamendments_data) > 0){
  orgamendments_data <- orgamendments_data %>% 
    mutate(co2_leakage = quantity_t_ha*1e3 * imported_frac * area * carbon_content * .12 * (44/12)) %>% 
    select(source, co2_leakage)
  }
  else{
      orgamendments_data <- create_empty_dataframe(c("source", "co2_leakage"))
      warning("No additional manure data provided")
    }
  
  
  return(orgamendments_data)
  
}

productivity_crops <- function(crop_inputs, scenario_selected, farm_EnZ, parcel_inputs){
  # TODO: Incorporate this as part of the monitoring report. 
  # This should check the changes in production and check whether there has been a reduction in productivity. 
  # This can only happen when there are results to compare to. 
  ## Join crop data /ha and area in parcel_input
  
  crops <- merge(x = filter(crop_inputs,scenario==scenario_selected), 
                 y = parcel_inputs, by = "parcel_ID", all.x = TRUE)
  crops <- merge(x = crops, 
                 y = filter(crop_factors,pedo_climatic_area==farm_EnZ | is.na(pedo_climatic_area)==TRUE), by = "crop", all.x = TRUE)
  crops <- crops %>% mutate(productivity = (dry_harvest + dry_grazing * area * dry_c))
  farm_productivity = sum(crops$productivity) * 44/12
  return(farm_productivity)
}

get_yearly_productivity_table <- function(productivity_table, crop_inputs, scenario_selected, farm_EnZ){
  # TODO: Incorporate this as part of the monitoring report. 
  # This should check the changes in production and check whether there has been a reduction in productivity. 
  # This can only happen when there are results to compare to. 
  ## Join crop data /ha and area in parcel_input
  
  # Fernando: Issue: The code doesn't seem to make sense 
  crops <- merge(x = filter(crop_inputs,scenario==scenario_selected), 
                 y = parcel_inputs, by = "parcel_ID", all.x = TRUE)
  crops <- merge(x = crops, 
                 y = filter(crop_factors, pedo_climatic_area==farm_EnZ | is.na(pedo_climatic_area)==TRUE), by = "crop", all.x = TRUE)
  if(nrow(crops)==0){
    productivity_table = data.frame(year = c(), crop = c(), productivity = c())
    return(productivity_table)
    }
  crops$scenario=scenario_selected
  productivity_table <- crops %>% group_by(scenario,crop) %>% summarise(productivity = 44/12 * sum(dry_harvest + dry_grazing * area * dry_c))
  return(productivity_table)
}
