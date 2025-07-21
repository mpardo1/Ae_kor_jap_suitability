############################# Preparing MA datasets ############################
# Here we extract the MA data and create the pseudo-absence through sampling effort
library(tidyverse)
library(dplyr)
library(terra)
library(sf)
library(parallel)
library(data.table)

rm(list = ls())

# Directories ------------------------------------------------------------------
loc.output <- paste0(getwd(), "/OUTPUT/")
loc.heavy <- paste0(getwd(), "/Heavy_files/")
loc.clc <- paste0(getwd(), "/OUTPUT/land_cover_raster/")

# Template raster --------------------------------------------------------------
# We need a reference raster to calculate future pixel ID and lon-lat grid
template <- readRDS(paste0(loc.clc, "coniferous_forest_rast.rds"))
plot(template)
res(template)
crs(template)

# Load resports (downloaded by Marta Pardo)
df_jap <- readRDS(paste0(loc.heavy, "japonicus_reports_MA.Rds"))
df_kor <- readRDS(paste0(loc.heavy, "koreicus_reports_MA.Rds"))

df_jap <- readRDS(paste0(loc.heavy, "japonicus_reports_MA.Rds")) %>%
  mutate(
    ae_jap = 1,
    ae_kor = NA
  )
df_kor <- readRDS(paste0(loc.heavy, "koreicus_reports_MA.Rds")) %>%
  mutate(
    ae_jap = NA,
    ae_kor = 1
  )

df_ma <- rbind(df_jap, df_kor)
rm(df_jap, df_kor)

# Checking the ID pixels: Do the pixels from clc data allign with the template rasters? 
df_ma_sf <- df_ma %>% 
  drop_na(lon, lat) %>% # missing values on coords are not allow for transformation --> remove it
  st_as_sf(coords = c("lon", "lat"), crs = 4236, remove = FALSE) %>%
  st_transform(crs(template))

# Extracting values from clc data; remember keep the pixel ID 
example <- data.frame(
  pixel_id = terra::cellFromXY(template, st_coordinates(df_ma_sf)),
  values = terra::extract(template, df_ma_sf), 
  lon = terra::xyFromCell(template, terra::cellFromXY(template, st_coordinates(df_ma_sf)))[, 1], # Longitud
  lat = terra::xyFromCell(template, terra::cellFromXY(template, st_coordinates(df_ma_sf)))[, 2]  # Latitud
)

# Join aedes and pixel ID
ma <- st_drop_geometry(df_ma_sf) %>%
  mutate(
    pixel_id = example$pixel_id, # Here, our MA presence with pixel ID
    longitude = example$lon,
    latitude = example$lat
  ) 

rm(example, df_ma_sf, df_ma)

## Calculating the pseudo-absence: sampling effort ------------------------------
# We need Europe shp 
europe <- rnaturalearth::ne_countries(continent = "europe") %>%
  st_transform(4326) %>%
  filter(sovereignt != "Russia")
plot(st_geometry(europe))

trs_row <- read_csv("https://github.com/Mosquito-Alert/sampling_effort_data/raw/main/sampling_effort_daily_cellres_025.csv.gz") %>%
  as_tibble() %>%
  mutate(
    date = as_date(date),
    y = year(as_date(date)),
    m = month(as_date(date)),
    n_total_reports = n_reports_albopictus + n_reports_bite + n_reports_culex + n_reports_japonicus + n_reports_aegypti + n_reports_koreicus,
    n_total_reporters = n_reporters_albopictus + n_reporters_bite + n_reporters_culex + n_reporters_japonicus + n_reporters_aegypti + n_reporters_koreicus
  ) %>%
  filter(y %in% c("2020", "2021", "2022", "2023")) %>%
  st_as_sf(coords = c("masked_lon", "masked_lat"), remove = FALSE, crs = 4326)

index_intersects <- st_intersects(trs_row, europe) 
index_intersects <- lengths(index_intersects) > 0

trs_row <- trs_row[index_intersects, ] # Only selecting European data N =258861

# Grouping by day and tiger cell
trs_daily = trs_row %>%
  rename(SEev = SE_expected) %>%
  st_drop_geometry() %>%
  group_by(TigacellID, masked_lon, masked_lat, date) %>%
  summarize(
    SEev = sum(SEev),
    SE = 1-prod(1-SE), # This comes directly fom John's script: one usser at least send one report
    n_total_reports = sum(n_total_reports),
    n_total_reporters = sum(n_total_reporters)
  ) %>%
  ungroup() 

se_cell <- trs_daily %>%  
  drop_na(masked_lon, masked_lat) %>% # missing values on coords are not allow for transformation --> remove it
  st_as_sf(coords = c("masked_lon", "masked_lat"), crs = 4236, remove = FALSE) %>%
  st_transform(crs(template))
plot(st_geometry(se_cell))

example <- data.frame(
  pixel_id = terra::cellFromXY(template, st_coordinates(se_cell)),
  values = terra::extract(template, se_cell), 
  lon = terra::xyFromCell(template, terra::cellFromXY(template, st_coordinates(se_cell)))[, 1], # Longitud
  lat = terra::xyFromCell(template, terra::cellFromXY(template, st_coordinates(se_cell)))[, 2]  # Latitud
)
example$values.ID <- NULL
example$values.LABEL3 <- NULL

# Join SE and pixel ID
se_cell <- st_drop_geometry(se_cell) %>%
  mutate(
    pixel_id = example$pixel_id,
    lon =  example$lon,
    lat = example$lat
  ) %>%
  group_by(pixel_id, date, lon, lat) %>% # If temporal dimension wants to be added ---> group_by(pixel, date)
  summarise(
    SE = mean(SE, na.rm = TRUE)
  ) %>% 
  mutate(y = year(date)) %>%
  group_by(y, pixel_id, lon, lat) %>%
  summarise(
    SE_sum_year = sum(SE, na.rm = TRUE)
  ) %>%
  group_by(pixel_id, lon, lat) %>%
  summarise(
    SE_sum = sum(SE_sum_year, na.rm = TRUE),
    SE_mean = mean(SE_sum_year, na.rm = TRUE)
  )
rm(example)

# Filtering SE for Europe ------------------------------------------------------
se_cell %>%
  pivot_longer(cols = c(SE_sum, SE_mean), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, fill = variable)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~ variable, scales = "free") +
  theme_classic() +
  labs(x = "Value", y = "Count")
SE_sum_filter <- median(se_cell$SE_sum)
SE_mean_filter <- median(se_cell$SE_mean)

## Plotting the SE -------------------------------------------------------------
# IMPORTANT! We have to remove the pixels where the species were found
pixel_with_aedes <- which(se_cell$pixel_id %in% ma$pixel_id)
se_cell <- se_cell[-pixel_with_aedes, ]

se_cell <- se_cell %>% drop_na(SE_sum, SE_mean)
rm(pixel_with_aedes)

se_cell$SE_sum[se_cell$SE_sum == 0] <- NA
se_cell$SE_mean[se_cell$SE_mean == 0] <- NA

a1 <- se_cell %>% # Without filter
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = SE_sum)) +
  geom_sf(data = europe, aes(fill = "transparent"), fill = "transparent") +
  scale_fill_distiller("Propensity", palette = "Spectral", 
                       na.value = "white") +
  labs(title = "Sampling Effort (SE) in Europe (SUM)",
       x = "Longitude",
       y = "Latitude",
       color = "Sampling Effort") +
  xlim(c(-30, 43)) +
  ylim(c(30, 75)) +
  theme_classic()

a2 <- se_cell %>% # With filter
  filter(SE_sum > SE_sum_filter) %>%
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = SE_sum)) +
  geom_sf(data = europe, aes(fill = "transparent"), fill = "transparent") +
  scale_fill_distiller("Propensity", palette = "Spectral", 
                       na.value = "white") +
  labs(title = "Sampling Effort (SE) in Europe (SUM)",
       x = "Longitude",
       y = "Latitude",
       color = "Sampling Effort") +
  xlim(c(-30, 43)) +
  ylim(c(30, 75)) +
  theme_classic()

ggpubr::ggarrange(a1, a2, nrow = 1, ncol = 2)

b1 <- se_cell %>%
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = SE_mean)) +
  scale_fill_distiller("Propensity", palette = "Spectral", 
                       na.value = "white") +
  geom_sf(data = europe, aes(fill = "transparent"), fill = "transparent") +
  labs(title = "Sampling Effort (SE) in Europe (without filter)",
       x = "Longitude",
       y = "Latitude",
       color = "Sampling Effort") +
  xlim(c(-30, 43)) +
  ylim(c(30, 75)) +
  theme_classic()

b2 <- se_cell %>%
  filter(SE_mean > SE_mean_filter) %>%
  # filter(SE_mean > 0.75) %>%
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = SE_mean)) +
  scale_fill_distiller("Propensity", palette = "Spectral", 
                       na.value = "white") +
  geom_sf(data = europe, aes(fill = "transparent"), fill = "transparent") +
  labs(title = "Sampling Effort (SE) in Europe (with filter)",
       x = "Longitude",
       y = "Latitude",
       color = "Sampling Effort") +
  xlim(c(-30, 43)) +
  ylim(c(30, 75)) +
  theme_classic()

ggpubr::ggarrange(b1, b2, nrow = 1, ncol = 2)

summary(ma)
ma_jap <- ma %>% 
  dplyr::select(-ae_kor) %>%
  drop_na(ae_jap) %>% # I want to keep this NA (because it are not zeros)
  dplyr::select(-creation_date) %>%
  group_by(pixel_id, lon, lat) %>%
  summarise(
    ae_jap = sum(ae_jap, na.rm = TRUE)
  ) %>% mutate(
    ae_jap = ae_jap > 0
  ) 

ma_kor <- ma %>% 
  dplyr::select(-ae_jap) %>%
  drop_na(ae_kor) %>% # I want to keep this NA (because it are not zeros)
  dplyr::select(-creation_date) %>%
  group_by(pixel_id, lon, lat) %>%
  summarise(
    ae_kor = sum(ae_kor, na.rm = TRUE)
  ) %>% mutate(
    ae_kor = ae_kor > 0
  ) 

ma <- merge(ma_jap, ma_kor, by = c( "pixel_id", "lon", "lat"), all = TRUE)
rm(ma_jap, ma_kor)

# Adding pseudo-absences -------------------------------------------------------
se_cell <- se_cell %>%
  # filter(SE_mean > SE_mean_filter) %>% # Filtering the SE for pseudo-absences
  filter(SE_mean > quantile(se_cell$SE_mean, prob = 0.75)) %>% # Filtering the SE for pseudo-absences
  dplyr::select(-SE_sum) %>%
  mutate(
    ae_jap = FALSE,
    ae_kor = FALSE
  )
ma$SE_mean = NA

ma_with_pseudo <- rbind(ma, se_cell)
summary(ma_with_pseudo)

ma_with_pseudo <- ma_with_pseudo %>%
  mutate(trap_type = "MA")

sum(duplicated(ma_with_pseudo))

# saveRDS(ma_with_pseudo, file = "aedes_ma_with_pseudo.rds")
saveRDS(ma_with_pseudo, file = "aedes_ma_with_pseudo_Q3.rds")
