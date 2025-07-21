#Code to compute the percentage of each type of land cover for each cell in Europe
# We use as a template raster the ERA5 land and project the land cover from Corine 
# to this template while computing the percentage of each land cover.
rm(list = ls())
#' Percentages by grid
library(tidyverse)
library(sf)
library(janitor)
library(dplyr)
library(lubridate)
library(terra)
library(eurostat)

# Directories ------------------------------------------------------------------
loc.output <- paste0(getwd(), "/OUTPUT/")
loc.heavy <- paste0(getwd(), "/Heavy_files/")
loc.output <- paste0("U:/aedes_korjap_suitability", "/OUTPUT/")
loc.heavy <- paste0("U:/aedes_korjap_suitability", "/Heavy_files/")

sf::sf_use_s2(FALSE)

# Getting a ERA5 template ------------------------------------------------------
# https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land?tab=overview
pname <- "ERA5_EU_hourly_"
tmp_temp <- rast(paste0(loc.heavy, pname, "2021.grib"), "t2m")
# tmp_temp <- rast(paste0(loc.heavy, pname, "2021.nc"), "t2m")
template <- tmp_temp[[1]] # Raster to include the mosquito data

# from Corine landcover: https://land.copernicus.eu/en/products/corine-land-cover/clc2018/download-by-area
## Load raster ------------------------------------------------------------------
path <- paste0(loc.heavy, "/U2018_CLC2018_V2020_20u1.tif")
landcover <- rast(path)
landcover <- project(landcover, crs(template))
# plot(landcover) # Plot the raster

df_cat <- levels(landcover)[[1]]

# # Trial ------------------------------------------------------------------------
# 
# galicia <- mapSpain::esp_get_prov("Galicia") %>% st_transform(crs(landcover))
# plot(st_geometry(galicia))
# galicia_landcover <- raster::crop(landcover, galicia)
# plot(galicia_landcover)
# galicia_landcover <- project(galicia_landcover, crs(template))

## Compute rater percentage for each landcover ---------------------------------

# Function to process each chunk
process_chunk <- function(chunk_idx, df_cat, landcover, template, loc.output) {
  cat("Processing chunk:", df_cat[chunk_idx, 2], "\n")
  
  # Filter each category
  rast_cat <- landcover %in% df_cat[chunk_idx, 2]
  rast_cat <- as.numeric(rast_cat)
  
  # Projection and weighted sum of pixels
  rast_aux <- terra::resample(rast_cat, template, method= "sum")
  
  # Generating file name
  filename <- paste0(loc.output, make_clean_names(df_cat[chunk_idx, 2]), "_rast.rds")
  
  # Verify no empty name
  if (filename != "") {
    # Guardar el resultado en un archivo .rds
    cat("Saving: ", filename, "\n")
    saveRDS(rast_aux, file = filename)
  } else {
    cat("Error: Filename is empty for category", chunk_idx, "\n")
  }
  
  # Free memory
  rm(rast_aux)
  gc()
}

# Categories
df_cat <- levels(landcover)[[1]]

# Total number of rows in df_cat
n <- nrow(df_cat)

# Process by categories
for (i in 1:n) {
  process_chunk(i,df_cat, landcover, template, loc.output)
}

# Transform output into percentage ------------------------------------
# Read processed files with landcover sum of squares for each type of landcover
Path <- paste0(loc.output, "/land_cover_raster/")
list_files <- list.files(Path, pattern = "rast")

# Raster with the sum of all squares of landcover fit in one square of the template
rast_test <- landcover
values(rast_test) <- 1
rast_test <- terra::resample(rast_test,template, method= "sum")
saveRDS(rast_test, paste0(loc.output, "tot_rast.Rds"))

# Read raster with all ones landcover extent and size cells.
rast_test <- readRDS(paste0(loc.output, "tot_rast.Rds"))
# terra::plot(rast_test)

# Loop through all files and join with the total
for (i in c(1:length(list_files))){
  rast_aux <- readRDS(paste0(Path,list_files[i]))
  rast_test <- c(rast_test,rast_aux)
}

# Transform into a Data frame
rast_df <- terra::as.data.frame(rast_test, xy=TRUE)

# Change columns names
colnames(rast_df)[3] <- "Total"
colnames(rast_df)[c(1:2)] <- c("lon", "lat")
colnames(rast_df)[c(4:(ncol(rast_df)))] = substr(list_files,1,10)

# Compute percentage of small cell inside each cell template
for(i in 4:(ncol(rast_df))){
  rast_df[,i] <- rast_df[,i] / rast_df[,3]
}

# Sum all the values of each type of landcover
rast_df$sum <- rowSums(rast_df[,c(4:ncol(rast_df))])

# Save as Rds data frame with percentages
saveRDS(rast_df,paste0(loc.heavy,"df_perc_landcover_EU.Rds"))
rast_df <- readRDS(paste0(loc.heavy,"df_perc_landcover_EU.Rds"))

# Check to see if it makes sense ---------------------------------------------
# Plot output percentage
rast_sum <- terra::rast(rast_df[,c(1,2,49)])
terra::plot(rast_sum)

# Filt landcover raster and plot
landcover_filt <- landcover %in% 19
terra::plot(landcover_filt)

# Transform into a Data frame
landcover_filt_df <- terra::as.data.frame(landcover_filt, xy=TRUE)
terra::plot(rast_sum[[48]])
