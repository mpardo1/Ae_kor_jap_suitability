# Code to process ERA5 land data
rm(list = ls())
library(terra)
library(tidyverse)
library(data.table)

# Process rainfall data Europe ----------------------------------------------
Path <- "/home/marta/Documentos/PHD/2025/Aedes_land/data/climate/"
list_f <- list.files(Path, pattern="grib")

# Filter only summer months from May to September
list_f <- c(list_f[list_f %like% c("rain05")],
list_f[list_f %like% c("rain06")],
list_f[list_f %like% c("rain07")],
list_f[list_f %like% c("rain08")],
list_f[list_f %like% c("rain09")])

# function to agg by year rainfall
agg_yearly <- function(year_n){
  # Filter a specific year
  list_f_f <- list_f[list_f %like% year_n]
  
  # Read raster
  rast_rain <- rast(paste0(Path,list_f_f[[1]]))
  # plot(rast_rain)
  
  # Aggregate by month
  rast_rain_mean <- tapp(rast_rain, index= "month", fun = "mean" )
  rast_rain_sum <- tapp(rast_rain, index= "month", fun = "sum" )
  
  # Run through all the months and aggregate
  for(i in c(2:length(list_f_f))){
    print(paste0("i:",i))
    # Read raster
    aux <- rast(paste0(Path,list_f_f[[i]]))
    # plot(rast_rain)
    
    # Aggregate by month
    aux_mean <- tapp(aux, index= "month", fun = "mean" )
    aux_sum <- tapp(aux, index= "month", fun = "sum" )
    
    # Join raster
    rast_rain_mean <- c(rast_rain_mean, aux_mean)
    rast_rain_sum <- c(rast_rain_sum, aux_sum)
    
  }
  
  # Aggregate by year
  rast_rain_mean <- mean(rast_rain_mean)
  # plot(rast_rain_mean)
  rast_rain_sum <- sum(rast_rain_sum)
  # plot(rast_rain_sum)
  return(list(rast_rain_mean, rast_rain_sum))
}

# Run through all the years
rast_rain <- agg_yearly(2012)
rast_mean <- rast_rain[[1]]
rast_sum <- rast_rain[[2]]
for(year_n in c(2012:2023)){
  print(paste0("year:", year_n))
  aux_rain <- agg_yearly(year_n)
  aux_mean <- rast_rain[[1]]
  aux_sum <- rast_rain[[2]]
  
  # Join
  rast_mean <- c(rast_mean, aux_mean)
  rast_sum <-  c(rast_sum, aux_sum)
}

# Average all years
rast_mean <- mean(rast_mean)
rast_sum <-  mean(rast_sum)
plot(rast_mean)
plot(rast_sum)

# Transform to data frame and safe
df_mean <- terra::as.data.frame(rast_mean, xy = TRUE)
df_sum <- terra::as.data.frame(rast_sum, xy = TRUE)
saveRDS(df_mean, "/home/marta/Documentos/PHD/2025/Aedes_land/data/climate/rast_yearly_mean_rainfall_May_Sept_2012-2023_EU.Rds")
saveRDS(df_sum, "/home/marta/Documentos/PHD/2025/Aedes_land/data/climate/rast_yearly_sum_rainfall_May_Sept_2012-2023_EU.Rds")

# Extract and process weather 2024 --------------------------------------
Path <- "/home/marta/aedes_korjap_suitability/data/Heavy_files/era5/clim_2024/"
list_temp <- list.files(Path, pattern = "temp")

# Read raster
rast_temp <- rast(paste0(Path, list_temp[[1]]))
rast_temp <- tapp(rast_temp, index= "month", fun = "mean")
for(i in c(2:length(list_temp))){
  # Read raster
  rast_aux <- rast(paste0(Path, list_temp[[i]]))
  rast_aux <- tapp(rast_aux, index= "month", fun = "mean")
  # Join with other rasters
  rast_temp <- c(rast_temp, rast_aux)
}

# Transform to data frame
df_temp <- as.data.frame(rast_temp, xy = TRUE)
df_temp <- df_temp %>%  pivot_longer(cols = starts_with("m_"),
                                     names_to = "month",
                                     names_prefix = "m_",
                                     values_to = "temp")
# Save RDs
saveRDS(df_temp, paste0(Path, "temp_2024_EU.Rds"))

# Same with rainfall
list_rain <- list.files(Path, pattern = "rain")

# Read raster
rast_rain <- rast(paste0(Path, list_rain[[1]]))
rast_rain <- mean(rast_rain)
for(i in c(2:length(list_rain))){
  # Read raster
  rast_aux <- rast(paste0(Path, list_rain[[i]]))
  rast_aux <- mean(rast_aux)
  # Join with other rasters
  rast_rain <- c(rast_rain, rast_aux)
}

# Transform to data frame
df_rain <- as.data.frame(rast_rain, xy = TRUE)
colnames(df_rain)[3:ncol(df_rain)] <- paste0("rain_",c(1:12))
df_rain <- df_rain %>%  pivot_longer(cols = starts_with("rain_"),
                                     names_to = "month",
                                     names_prefix = "rain_",
                                     values_to = "rain")
# Save RDs
saveRDS(df_rain, paste0(Path, "rain_2024_EU.Rds"))

# Join both data sets rainfall and temp
df_clim <- df_temp %>%  left_join(df_rain)

# Save RDs
saveRDS(df_clim, paste0(Path, "clim_2024_EU.Rds"))
