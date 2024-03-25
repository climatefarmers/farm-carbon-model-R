# Weather functions

library(ncdf4)
library(tidyverse)
library("ncdf4.helpers")
library(aws.s3)

get_past_weather_data <- function(init_data, lat_farmer, lon_farmer, period, averaged = TRUE){
  # Period options: "1950_2021", "1950_2022"
  
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = init_data$AWS_ACCESS_KEY_ID,
    "AWS_SECRET_ACCESS_KEY" = init_data$AWS_SECRET_ACCESS_KEY,
    "AWS_DEFAULT_REGION" = init_data$AWS_DEFAULT_REGION
  )
  
  # Reading the data from AWS s3
  file_name <- paste0("ERA5_Land_monthly_averaged_data_", period,".nc")
  climate_data = s3read_using(FUN = nc_open, object = paste0(init_data$weatherDB_loc, file_name))

  # Data extraction from netcdf object
  evap <- ncvar_get(climate_data, varid = "e",
                        start= c(which.min(abs(climate_data$dim$longitude$vals - lon_farmer)), # look for closest lon
                                 which.min(abs(climate_data$dim$latitude$vals - lat_farmer)),  # look for closest lat
                                 1),
                        count = c(1,1,-1))
  pevap <- ncvar_get(climate_data, varid = "pev",
                    start= c(which.min(abs(climate_data$dim$longitude$vals - lon_farmer)),
                             which.min(abs(climate_data$dim$latitude$vals - lat_farmer)),  # look for closest lat
                             1),
                    count = c(1,1,-1))
  precipitation <- ncvar_get(climate_data, varid = "tp",
                                 start= c(which.min(abs(climate_data$dim$longitude$vals - lon_farmer)), # look for closest long
                                          which.min(abs(climate_data$dim$latitude$vals - lat_farmer)),  # look for closest lat
                                          1),
                                 count = c(1,1,-1))
  temperature <- ncvar_get(climate_data, varid = "t2m",
                               start= c(which.min(abs(climate_data$dim$longitude$vals - lon_farmer)), # look for closest long
                                        which.min(abs(climate_data$dim$latitude$vals - lat_farmer)),  # look for closest lat
                                        1),
                               count = c(1,1,-1))
  
  # Creating a dataframe to hold the data
  obsdata_date <- as.character(nc.get.time.series(climate_data))
  data_out <- data.frame(date = as.Date(obsdata_date))
  data_out['month'] <- unlist(as.integer(strsplit(format(data_out$date,'%m'),'-')))
  days_in_a_month <- data.frame(month=c(1:12), days_in_a_month=c(31,28.25,31,30,31,30,31,31,30,31,30,31))
  data_out <- left_join(data_out, days_in_a_month, by="month")
  
  # Unit corrections
  data_out <- data_out %>% mutate(
    evap = - c(evap) * 1e3 * days_in_a_month,
    pevap = - c(pevap) * 1e3 * days_in_a_month,
    precipitation = c(precipitation) * 1e3 * days_in_a_month,
    temperature = c(temperature) - 273.15
  )

  if(averaged) {
    # Summarize to monthly means
    data_out <- data_out %>% group_by(month) %>% 
      summarise(temperature=mean(temperature),
                precipitation=mean(precipitation),
                evap=mean(evap),
                pevap=mean(pevap))
  }

  # Add a period variable
  data_out$scenario <- period
  
  return(data_out)
}

get_future_weather_data <- function(init_data, lat_farmer, lon_farmer, scenario = "rcp4.5"){
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = init_data$AWS_ACCESS_KEY_ID,
    "AWS_SECRET_ACCESS_KEY" = init_data$AWS_SECRET_ACCESS_KEY,
    "AWS_DEFAULT_REGION" = init_data$AWS_DEFAULT_REGION
  )
  if (scenario=="rcp8.5"){
    climate_data = s3read_using(FUN = nc_open, object = paste(init_data$weatherDB_loc,"ERA5_Land_extrapolated_future_rcp8.5.nc",sep=""))
  } else if(scenario=="rcp4.5"){
    climate_data = s3read_using(FUN = nc_open, object = paste(init_data$weatherDB_loc,"ERA5_Land_extrapolated_future_rcp4.5.nc",sep=""))
  } else{stop("wrong scenario spelling")}
  
  
  evap<- ncvar_get(climate_data, varid = "evap",
                          start= c(which.min(abs(climate_data$dim$lon$vals - lon_farmer)), # look for closest long
                                   which.min(abs(climate_data$dim$lat$vals - lat_farmer)),  # look for closest lat
                                   1),
                          count = c(1,1,-1))
  pevap<- ncvar_get(climate_data, varid = "pevap",
                    start= c(which.min(abs(climate_data$dim$lon$vals - lon_farmer)),
                             which.min(abs(climate_data$dim$lat$vals - lat_farmer)),  # look for closest lat
                             1),
                    count = c(1,1,-1))
  precipitation<- ncvar_get(climate_data, varid = "rainfall",
                                   start= c(which.min(abs(climate_data$dim$lon$vals - lon_farmer)), # look for closest long
                                            which.min(abs(climate_data$dim$lat$vals - lat_farmer)),  # look for closest lat
                                            1),
                                   count = c(1,1,-1))
  temperature<- ncvar_get(climate_data, varid = "temperature",
                                 start= c(which.min(abs(climate_data$dim$lon$vals - lon_farmer)), # look for closest long
                                          which.min(abs(climate_data$dim$lat$vals - lat_farmer)),  # look for closest lat
                                          1),
                                 count = c(1,1,-1))
  
  data_out <- data.frame(month = climate_data$dim$time$vals)
  data_out$temperature <- temperature
  data_out$precipitation <- precipitation
  data_out$evap <- evap
  data_out$pevap <- pevap
  
  # Add scenario
  data_out$scenario <- scenario
  
  return(data_out)
}

# yearly <- data.frame(date= as.Date(obsdata_date), 
#                      pevap = pevap*1e3*30.4,
#                      evap=evap*1e3*30.4,
#                      tp=tp*1e3*30.4,
#                      temp=temp-273.15)
# yearly['year'] <- unlist(strsplit(format(yearly$date,'%Y'),'-'))
# yearly = yearly %>% group_by(year) %>% 
#   summarise(temp_monthly=mean(temp),
#             precip_monthly=sum(tp),
#             evap_monthly=-sum(evap))

get_climate_periods <- function(climate_data, proj_start_year) {
  
  # Average of all past climate data since start
  mean_past_climate <- climate_data %>% group_by(month) %>% 
    summarise(temperature=mean(temperature),
              precipitation=mean(precipitation),
              evap=mean(evap),
              pevap=mean(pevap))
  
  # Averaged recent climate (10 last years of data)
  nr_cd <- nrow(climate_data) 
  i_cd <- nr_cd - (10*12)  # index for last 10 years of data
  
  mean_recent_climate <- climate_data[i_cd:nr_cd, ] %>% group_by(month) %>% 
    summarise(temperature=mean(temperature),
              precipitation=mean(precipitation),
              evap=mean(evap),
              pevap=mean(pevap))
  
  # Climate for the project period (future years use the averaged recent climate)
  climate_proj <- data.frame()
  
  for(i in 1:10) {
    if(i <= settings$curr_monit_year) {
      curr_year <- proj_start_year + (i-1)
      climate_proj_temp <- climate_data[format(climate_data$date, "%Y") == curr_year, ]
      climate_proj_temp <- climate_proj_temp %>% mutate(year = i) %>% select(-c(date, days_in_a_month, scenario))
    } else {
      climate_proj_temp <- mean_recent_climate %>% mutate(year = i)
    }
    climate_proj <- rbind(climate_proj, climate_proj_temp)
  }
  
  return(
    list(
      mean_past_climate = mean_past_climate,
      mean_recent_climate = mean_recent_climate,
      climate_proj =climate_proj
    )
  )
}