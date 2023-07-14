# leakage functions


# Carbon share of manure come from: https://www.researchgate.net/publication/225385126_Carbon_resources_of_residue_and_manure_in_Japanese_farmland_soils/figures?lo=1

manure_leakage <- function(add_manure_data){
  
  if(nrow(add_manure_data) > 0){
  add_manure_data <- add_manure_data %>% 
    mutate(co2_leakage = quantity_t_ha*1e3 * imported_frac * area * carbon_content * .12 * (44/12)) %>% 
    select(manure_source, co2_leakage)
  }
  else{
      add_manure_data <- create_empty_dataframe(c("manure_source", "co2_leakage"))
      warning("No additional manure data provided")
    }
  
  
  return(add_manure_data)
  
}

productivity_crops <- function(crop_data, scenario_selected, farm_EnZ){
  # TODO: Incorporate this as part of the monitoring report. 
  # This should check the changes in production and check whether there has been a reduction in productivity. 
  # This can only happen when there are results to compare to. 
  ## Join crop data /ha and area in parcel_input
  
  crops <- merge(x = filter(crop_data,scenario==scenario_selected), 
                 y = parcel_data, by = "parcel_ID", all.x = TRUE)
  crops <- merge(x = crops, 
                 y = filter(crop_factors,pedo_climatic_area==farm_EnZ | is.na(pedo_climatic_area)==TRUE), by = "crop", all.x = TRUE)
  crops <- crops %>% mutate(productivity = (dry_yield + dry_grazing_yield + (fresh_yield + fresh_grazing_yield)*dry)*area*dry_c)
  farm_productivity = sum(crops$productivity)*44/12
  return(farm_productivity)
}
get_yearly_productivity_table <- function(productivity_table, crop_data, scenario_selected, farm_EnZ){
  # TODO: Incorporate this as part of the monitoring report. 
  # This should check the changes in production and check whether there has been a reduction in productivity. 
  # This can only happen when there are results to compare to. 
  ## Join crop data /ha and area in parcel_input
  
  # Fernando: Issue: The code doesn't seem to make sense 
  crops <- merge(x = filter(crop_data,scenario==scenario_selected), 
                 y = parcel_data, by = "parcel_ID", all.x = TRUE)
  crops <- merge(x = crops, 
                 y = filter(crop_factors,pedo_climatic_area==farm_EnZ | is.na(pedo_climatic_area)==TRUE), by = "crop", all.x = TRUE)
  if(nrow(crops)==0){
    productivity_table = data.frame(year = c(), crop = c(), productivity = c())
    return(productivity_table)}
  crops$scenario=scenario_selected
  productivity_table <- crops %>%group_by(scenario,crop) %>% summarise(productivity = 44/12*sum((dry_yield + dry_grazing_yield + (fresh_yield + fresh_grazing_yield)*dry)*area*dry_c))
  return(productivity_table)
}
