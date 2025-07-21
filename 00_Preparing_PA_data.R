############################ Preparing a global data ###########################

library(tidyverse)
library(readxl)
library(data.table)
library(janitor)
library(sf)

rm(list = ls())

# Directory --------------------------------------------------------------------

loc.data <- paste0(getwd(), "/data/")
loc.output<- paste0(getwd(), "/OUTPUT/")
loc.fig<- paste0(getwd(), "/FIGURES/")

# Loading dataframes -----------------------------------------------------------
files_names <- list.files(loc.data)
files_names 

## Hungary ---------------------------------------------------------------------
hun_20_22 <- read_excel(paste0(loc.data, "Ae_japonicus_HUN_2020-2022.xlsx"),
                          col_names = TRUE , col_types = c("guess", "guess","numeric",
                                                           "numeric", "date",   
                                                           "guess","numeric"))
colnames(hun_20_22) <- c("municipality", "site", "latitude", "longitude", "end_date", 
                         "trap_method", "n_japonicus")
str(hun_20_22)

hun_16_23 <- read_excel(paste0(loc.data, "Ae_koreicus_HUN_2016-2023.xlsx"),
                        col_names = TRUE , col_types = c("guess", "guess","numeric",
                                                         "numeric", "date",   
                                                         "guess","numeric"))
colnames(hun_16_23) <- c("municipality", "site", "latitude", "longitude", "end_date", 
                         "trap_method", "n_koreicus")
str(hun_16_23)

# Checking duplicates
sum(duplicated(hun_20_22)) # No duplicates

# Checking duplicates
sum(duplicated(hun_16_23)) # there is three samples duplicated
hun_16_23 <- hun_16_23[-which(duplicated(hun_16_23)),]

# Adding sampling effort following the intructions of Kornélia:
# I wrote the collection dates in the table, i.e., when I collected the trap (EVS, ovitrap stick) from the field or used aspirator for HLC.
# EVS traps always operate for one night. For example, if the collection date in the table is 13/7/2016, that means the trap was installed the day before, in the afternoon (12/7/2016), and collected in the morning (13/7/2016).
# In the case of HLC (human landing collection), the collection date means the given day when we collected the mosquito with an aspirator (so no operation time).
# In the case of ovitraps, they operated 7 days (1 week) before collection, so, e.g it was installed on 16/8/2020 and collected on 23/8/2020.

hun_16_23 <- hun_16_23 %>% 
  mutate(
    trap_method = ifelse(trap_method == "EVS_Trap", "EVSTrap", trap_method)
  ) %>%
  mutate(
    trapping_effort = case_when(
      trap_method == "EVSTrap" ~ 1,
      trap_method == "HLC" ~ 0, # Less than one day (maibe indicate a fraction, e.g., 0.1)
      trap_method == "Ovitrap" ~ 7,
    )
  )

hun_20_22 <- hun_20_22 %>%
  mutate(
    trapping_effort = case_when(
      trap_method == "EVSTrap" ~ 1,
      trap_method == "Ovitrap" ~ 7,
    )
  )

## Austria ---------------------------------------------------------------------
austria <- read_excel(paste0(loc.data, "AT_Bakran-Lebl_Mosquito_Ovitrap_2020-2023.xlsx"),
                        col_names = TRUE , col_types = c("guess", "guess", "numeric",
                                                         "numeric", "numeric", "guess",
                                                         "guess", "guess", "guess",
                                                         "guess", "guess", "date",
                                                         "date", "guess", "numeric",  
                                                         "guess", "guess")) %>%
  dplyr::select(-Position, -Site_name, -Method_Identification) %>% # not informative
  janitor::clean_names()
str(austria)

## Italy -----------------------------------------------------------------------
italy <- read_excel(paste0(loc.data, "Italian AIMs.xls")) %>%
  janitor::clean_names() %>%
  dplyr::select(data, region_nut2, municipality_lau1, id, waypoint_e, waypoint_n,
                collection, ae_koreicus, ae_japonicus, ae_albopictus, breeding_type) %>%
  mutate(
    data = as.Date(data, format="%m/%d/%y"),
    waypoint_e = as.numeric(gsub(",", ".", waypoint_e)),
    waypoint_n = as.numeric(gsub(",", ".", waypoint_n))
  )
str(italy)

# Checking duplicates
sum(duplicated(italy)) # there is 117 samples duplicated
italy <- italy[-which(duplicated(italy)),]

# Checking coordinates information
sum(is.na(italy$waypoint_e))
sum(is.na(italy$waypoint_n))

# Adding trap type following the instructions of Fabrizio Montarsi:
# CATU: Regarding the “collection” column with various levels (e.g., negative, eggs, 
# BG-sentinel, etc.). I have red in your paper that the method was mainly larval
# dipper but in the data frame there is other categories (e.g., CDC, BG-sentinel). 
# So, I expect the rest of the categories not related to adult traps (e.g., pupae,
# larvae, negative) correspond with dipper method. Are we wrong?
# FABRIZIO: Correct. They are all larval samplings.
italy <-italy %>%
  mutate(
    trap_type = case_when(
      collection %in% c("BG-Sentinel", "BG-Sentinel + CO2") ~ "BG traps",
      collection %in% c("CDC-CO2")  ~ "CDC",
      breeding_type %in% c("ovitrap") ~ "Ovitrap",
      breeding_type %in% c("CDC", "CDC-light") ~ "Ovitrap",
      breeding_type %in% c("BG-sentinel") ~ "BG traps",
      .default = "larval sampling"
      )
  ) %>% dplyr::select(-breeding_type)

# Adding sampling effort following the instructions of Fabrizio Montarsi:
# CATU: In addition, we need a measure related to sampling effort. Typically, we  use the 
# exposure days of the different traps in our models. However, in the data frame, 
# we only see one date (presumably the collection date), so is it possible to obtain 
# the start date of the sampling in the case of adult trapping? Alternatively, did 
# you use a constant sampling effort for each method? For example, do BG-sentinels work 
# for a fixed number of days? In the case of the sipper method, I understand there is no 
# trapping effort, isn't it?
# FABRIZIO: For adult trapping the collections made by BG-sentinels mean “BG baited 
# with lure working for 24h”, while the collections made by CDC mean “CDC baited with
# CO2 (dry ice) working approx. for 12h, from the evening to the morning of the day
# after. Larvae were collected from different sized containers, therefore a standardization 
# is impossible. 
italy <-italy %>%
  mutate(
    trapping_effort = case_when(
      collection %in% c("BG-Sentinel", "BG-Sentinel + CO2") ~ 1,
      collection %in% c("CDC-CO2")  ~ 0.5,
      trap_type %in% c("Ovitrap") ~ 1
    ),
    atractor = case_when(
      collection %in% c("BG-Sentinel") ~ "Lurr",
      collection %in% c("CDC-CO2", "BG-Sentinel + CO2")  ~ "CO2"
    )
  )

## Spain -----------------------------------------------------------------------
ba_2020 <- read_excel(paste0(loc.data, 'Spain_VEO_2020_ovitraps_Barcelonaxlsx.xlsx'),
                     skip = 3, col_names = TRUE ) %>%
  janitor::clean_names() %>%
  dplyr::select(country, province, city, trap_type, longitude, latitude, start_date, 
                end_date, trapping_effort, species) 

gi_2021 <- read_excel(paste0(loc.data, 'Spain_VEO_2021_ovitraps_Girona.xlsx'),
                      skip = 3, col_names = TRUE ) %>%
  janitor::clean_names() %>%
  dplyr::select(country, province, city, trap_type, longitude, latitude, start_date, 
                end_date, trapping_effort, species) 

gi_2022 <- read_excel(paste0(loc.data, 'Spain_VEO_2022_ovitraps_Girona.xlsx'),
                      skip = 3, col_names = TRUE ) %>%
  janitor::clean_names() %>%
  dplyr::select(country, province, city, trap_type, longitude, latitude, start_date, 
                end_date, trapping_effort, species) 

## Germany ---------------------------------------------------------------------
# We have to create psudoabsence by random method (Barbet-Massin et al 2012)
# see: https://biomodhub.github.io/biomod2/articles/vignette_pseudoAbsences.html
# CTA, BRT, RF : same amount of PA as available presences
ger_kor <- read.csv(paste0(loc.data, 'Ae koreicus_Mueckenatlas_2015-2022.csv'),
                      sep = ";", dec = ",", header = TRUE ) %>%
  janitor::clean_names() %>%
  mutate(
    date = as.Date(as.POSIXct(date, format = "%d.%m.%Y")),
    longitude = as.numeric(gsub("\\.(?=[^.]*$)", "", longitude, perl = TRUE)), 
    latitude = as.numeric(gsub("\\.(?=[^.]*$)", "", latitude, perl = TRUE)),
    ae_kor = 1
  ) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4236, remove = FALSE)

# germany <- rnaturalearth::ne_countries(country = "Germany", scale = "small") %>%
#   st_transform(4236)
# plot(st_geometry(germany))
# 
# set.seed(12346789)
# random_pts <- st_sample(germany, size = nrow(ger_kor), type = "random")
# random_pts <- st_as_sf(data.frame(geometry = random_pts))
# 
# # Calculating distances
# dists <- st_distance(random_pts, ger_kor)
# dists_true <- random_pts[apply(dists, 1, function(x) all(x > 250)), ] # Minimum distance 250 metros
# 
# # Only if it is the case:
# while(nrow(dists_true) < nrow(ger_kor)) {
#   extra <- st_sample(germany, size = num_puntos - nrow(dists_true), type = "random")
#   extra<- st_as_sf(data.frame(geometry = extra))
# 
#   dists_extra <- st_distance(extra, ger_kor)
#   dists_extra_true <- extra_puntos_sf[apply(dists_extra, 1, function(x) all(x > 250)), ]
# 
#   dists_true <- rbind(dists_true, dists_extra_true)
# }
# dists_true$date <- sample(seq(min(ger_kor$date, na.rm = TRUE), 
#                               max(ger_kor$date, na.rm = TRUE), by = "day"), 
#                           nrow(dists_true), replace = TRUE)
# 
# # plot(st_geometry(germany))
# # plot(st_geometry(ger_kor), add = TRUE)
# # plot(st_geometry(dists_true), add = TRUE)
# ger_kor <- st_drop_geometry(ger_kor[,-1])
# ger_kor_0 <- data.frame(
#   latitude = st_coordinates(dists_true)[,2],
#   longitude = st_coordinates(dists_true)[,1],
#   date = dists_true$date,
#   ae_kor = 0
# )
# 
# ger_kor <- rbind(ger_kor, ger_kor_0)

ger_jap <- read.csv(paste0(loc.data, 'Ae japonicus_Mueckenatlas_2012-2023.csv'),
                    sep = ";", dec = ",", header = TRUE ) %>%
  janitor::clean_names() %>%
  mutate(
    date = as.Date(date),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude),
    ae_jap = 1
  ) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4236, remove = FALSE)

# set.seed(6352475)
# random_pts <- st_sample(germany, size = nrow(ger_jap), type = "random")
# random_pts <- st_as_sf(data.frame(geometry = random_pts))
# 
# # Calculating distances
# dists <- st_distance(random_pts, ger_jap)
# dists_true <- random_pts[apply(dists, 1, function(x) all(x > 250)), ] # Minimum distance 250 metros
# 
# # Only if it is the case:
# while(nrow(dists_true) < nrow(ger_jap)) {
#   extra <- st_sample(germany, size = nrow(ger_jap) - nrow(dists_true), type = "random")
#   extra <- st_as_sf(data.frame(geometry = extra))
# 
#   dists_extra <- st_distance(extra, ger_jap)
#   dists_extra_true <- extra[apply(dists_extra, 1, function(x) all(x > 250)), ]
#   dists_extra_true <- st_as_sf(data.frame(geometry = dists_extra_true))
#   
#   dists_true <- rbind(dists_true, dists_extra_true)
# }
# 
# dists_true$date <- sample(seq(min(ger_jap$date, na.rm = TRUE), 
#                               max(ger_jap$date, na.rm = TRUE), by = "day"), 
#                           nrow(dists_true), replace = TRUE)
# 
# # plot(st_geometry(germany))
# # plot(st_geometry(ger_jap), add = TRUE)
# # plot(st_geometry(dists_true), add = TRUE)
# ger_jap <- st_drop_geometry(ger_jap[,-1])
# ger_jap_0 <- data.frame(
#   latitude = st_coordinates(dists_true)[,2],
#   longitude = st_coordinates(dists_true)[,1],
#   date = dists_true$date,
#   ae_jap = 0
# )
# 
# ger_jap <- rbind(ger_jap, ger_jap_0)

ger_kor <- st_drop_geometry(ger_kor)
ger_jap <- st_drop_geometry(ger_jap)

german <- merge(ger_jap, ger_kor, by = c("latitude", "longitude", "date", "species"), all = TRUE)
# german[is.na(german)] <- 0

rm(ger_jap_0, ger_kor_0, dists_extra_true, dists_true, extra, germany,
   dists, dists_extra, random_pts)

# Normalizing data: same columns -----------------------------------------------
names(hun_20_22)
hun_20_22_df <- data.frame(
  country = "Hungary",
  province = NA,
  district = NA,
  municipality = hun_20_22$municipality,
  site = hun_20_22$site,
  longitude = hun_20_22$longitude,
  latitude = hun_20_22$latitude,
  start_date = NA,
  end_date = as.Date(hun_20_22$end_date),
  trapping_effort = hun_20_22$trapping_effort,
  trap_type = hun_20_22$trap_method,
  atractor = NA,
  collection_type = NA,
  ae_jap = if_else(hun_20_22$n_japonicus > 0, 1 ,0),
  # ae_kor = 0
  ae_kor = NA
)

names(hun_16_23)
hun_16_23_df <- data.frame(
  country = "Hungary",
  province = NA,
  district = NA,
  municipality = hun_16_23$municipality,
  site = hun_16_23$site,
  longitude = hun_16_23$longitude,
  latitude = hun_16_23$latitude,
  start_date =as.Date(NA),
  end_date = as.Date(hun_16_23$end_date),
  trapping_effort = hun_16_23$trapping_effort,
  trap_type = hun_16_23$trap_method,
  atractor = NA,
  collection_type = NA,
  # ae_jap = 0,
  ae_jap = NA,
  ae_kor = if_else(hun_16_23$n_koreicus > 0, 1 ,0)
)

names(italy) # Here seems that all variables were considered on analysis so there are absences (zeros)
italy_df <- data.frame(
  country = "Italy",
  province = NA,
  district = NA,
  municipality = italy$municipality_lau1,
  site = NA,
  longitude = italy$waypoint_e,
  latitude = italy$waypoint_n,
  start_date = as.Date(NA),
  end_date = as.Date(italy$data),
  trapping_effort = italy$trapping_effort, 
  trap_type = italy$trap_type,
  atractor = italy$atractor,
  collection_type = italy$collection,
  ae_jap = if_else(is.na(italy$ae_japonicus) == TRUE, 0, 1 ),
  ae_kor = if_else(is.na(italy$ae_koreicus) == TRUE, 0, 1 )
  # ae_albo = if_else(is.na(italy$ae_albopictus) == TRUE, 0, 1 )
)

names(austria) # Here, there are true zeros as well
austria_df <- data.frame(
  country = "Austria",
  province = austria$province,
  district = austria$district,
  municipality = NA,
  site = austria$site,
  longitude = austria$longitude,
  latitude = austria$latitude,
  start_date = as.Date(austria$date_start),
  end_date = as.Date(austria$date_end),
  trapping_effort = as.Date(austria$date_end) - as.Date(austria$date_start),
  trap_type = "Ovitrap", # see the name of the csv
  atractor = NA,
  collection_type = NA,
  ae_jap = if_else(grepl("japonicus", austria$species), 1, 0),
  ae_kor = if_else(grepl("koreicus", austria$species), 1, 0)
)

names(ba_2020) # Here, there are true zeros as well
ba_2020_df <- data.frame(
  country = ba_2020$country,
  province = ba_2020$province,
  district = NA,
  municipality = ba_2020$city,
  site = NA,
  longitude = ba_2020$longitude,
  latitude = ba_2020$latitude,
  start_date = as.Date(ba_2020$start_date),
  end_date = as.Date(ba_2020$end_date),
  trapping_effort = as.numeric(as.Date(ba_2020$end_date) - as.Date(ba_2020$start_date)),
  trap_type = "Ovitrap", # see the name of the csv
  atractor = NA,
  collection_type = NA,
  ae_jap = if_else(grepl("japonicus", ba_2020$species), 1, 0),
  ae_kor = if_else(grepl("koreicus", ba_2020$species), 1, 0)
)

names(gi_2021) # Here, there are true zeros as well
gi_2021_df <- data.frame(
  country = gi_2021$country,
  province = gi_2021$province,
  district = NA,
  municipality = gi_2021$city,
  site = NA,
  longitude = as.numeric(gi_2021$longitude),
  latitude = as.numeric(gi_2021$latitude),
  start_date = as.Date(gi_2021$start_date),
  end_date = as.Date(gi_2021$end_date),
  trapping_effort = as.numeric(as.Date(gi_2021$end_date) - as.Date(gi_2021$start_date)),
  trap_type = "Ovitrap", # see the name of the csv
  atractor = NA,
  collection_type = NA,
  ae_jap = if_else(grepl("japonicus", gi_2021$species), 1, 0),
  ae_kor = if_else(grepl("koreicus", gi_2021$species), 1, 0)
)

names(gi_2022) # Here, there are true zeros as well
gi_2022_df <- data.frame(
  country = gi_2022$country,
  province = gi_2022$province,
  district = NA,
  municipality = gi_2022$city,
  site = NA,
  longitude = as.numeric(gi_2022$longitude),
  latitude = as.numeric(gi_2022$latitude),
  start_date = as.Date(gi_2022$start_date),
  end_date = as.Date(gi_2022$end_date),
  trapping_effort = as.numeric(as.Date(gi_2022$end_date) - as.Date(gi_2022$start_date)),
  trap_type = "Ovitrap", # see the name of the csv
  atractor = NA,
  collection_type = NA,
  ae_jap = if_else(grepl("japonicus", gi_2022$species), 1, 0),
  ae_kor = if_else(grepl("koreicus", gi_2022$species), 1, 0)
)

names(german) 
german_df <- data.frame(
  country = "Germany",
  province = NA,
  district = NA,
  municipality = NA,
  site = NA,
  longitude = german$longitude,
  latitude = german$latitude,
  start_date = NA,
  end_date = as.Date(german$date),
  trapping_effort = NA,
  trap_type = "Mückenatlas",
  atractor = NA,
  collection_type = NA,
  ae_jap = german$ae_jap,
  ae_kor = german$ae_kor
)
# Checking locations on maps ---------------------------------------------------
europe <- eurostat::get_eurostat_geospatial(resolution = 10, 
                                            nuts_level = 0, 
                                            year = 2021) %>%
  st_transform(4326)

# Hungrary
hun <- rbind(hun_16_23_df, hun_20_22_df)
ggplot() +
  geom_sf(data = europe, aes(fill = NAME_LATN), fill = "white", show.legend = FALSE) +
  geom_point(data = hun %>% filter(ae_jap == 1 ), 
             aes(x = longitude, y = latitude), color = "#8FBC8F", alpha = 0.4, size = 2) +
  geom_point(data = hun %>% filter(ae_kor == 1), 
             aes(x = longitude, y = latitude), color = "#6E7B8B", alpha = 0.4, size = 2) +
  ylim(c(43, 48)) +
  xlim(c(15, 21)) +
  theme_classic() +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) 

# Italy
ggplot() +
  geom_sf(data = europe, aes(fill = NAME_LATN), fill = "white", show.legend = FALSE) +
  geom_point(data = italy_df %>% filter(ae_jap == 1 ), 
             aes(x = longitude, y = latitude), color = "#8FBC8F", alpha = 0.4, size = 2) +
  geom_point(data = italy_df %>% filter(ae_kor == 1), 
             aes(x = longitude, y = latitude), color = "#6E7B8B", alpha = 0.2, size = 2) +
  ylim(c(42, 47)) +
  xlim(c(7, 19)) +
  theme_classic() +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) 

# Austria
ggplot() +
  geom_sf(data = europe, aes(fill = NAME_LATN), fill = "white", show.legend = FALSE) +
  geom_point(data = austria_df %>% filter(ae_jap == 1 ), 
             aes(x = longitude, y = latitude), color = "#8FBC8F", alpha = 0.4, size = 2) +
  geom_point(data = austria_df %>% filter(ae_kor == 1), 
             aes(x = longitude, y = latitude), color = "#6E7B8B", alpha = 0.2, size = 2) +
  ylim(c(44, 50)) +
  xlim(c(7, 20)) +
  theme_classic() +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) 

# Spain --> no positives

# Germany
ggplot() +
  geom_sf(data = europe, aes(fill = NAME_LATN), fill = "white", show.legend = FALSE) +
  geom_point(data = german_df %>% filter(ae_jap == 1 ), 
             aes(x = longitude, y = latitude), color = "#8FBC8F", alpha = 0.4, size = 2) +
  geom_point(data = german_df %>% filter(ae_kor == 1), 
             aes(x = longitude, y = latitude), color = "#6E7B8B", alpha = 0.2, size = 2) +
  ylim(c(46, 55)) +
  xlim(c(4, 15)) +
  theme_classic() +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) 

# Joinning all
dataframes_list <- list(hun_16_23_df, hun_20_22_df, italy_df, austria_df, ba_2020_df,
                        gi_2021_df, gi_2022_df, german_df)
combined_df <- do.call(rbind, dataframes_list)

sum(duplicated(combined_df))
combined_df <- combined_df[-which(duplicated(combined_df)),]

ggplot() +
  geom_sf(data = europe, aes(fill = NAME_LATN), fill = "white", show.legend = FALSE) +
  geom_point(data = combined_df %>% filter(ae_jap == 1 ), 
             aes(x = longitude, y = latitude), color = "#8FBC8F", alpha = 0.4, size = 1) +
  geom_point(data = combined_df %>% filter(ae_kor == 1), 
             aes(x = longitude, y = latitude), color = "#6E7B8B", alpha = 0.2, size = 1) +
  theme_classic() +
  ylim(c(35, 72)) +
  xlim(c(-21, 50)) +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) 

saveRDS(combined_df, paste0(loc.output, "ae_jap_kor_cleaned.rds"))

# Reducing data for RF analysis ------------------------------------------------
aedes <- readRDS(paste0(loc.output, "ae_jap_kor_cleaned.rds"))
# Raster template for pixel_id
template <- readRDS(paste0(loc.clc, "coniferous_forest_rast.rds")) 
# plot(template)
# res(template)
# crs(template)

template_df <- data.frame(
  pixel_id = cells(template), # Obtaining pixel ID
  lon = xyFromCell(template, cells(template))[,1], # X coordinates
  lat = xyFromCell(template, cells(template))[,2], # Y coordinates
  value <- na.omit(values(template)) # Values; NA omit to avoid empty data
)

# Checking the ID pixels: Do the pixels from clc data allign with the template rasters? 
aedes_sf <- readRDS(paste0(loc.output, "ae_jap_kor_cleaned.rds")) %>% 
  drop_na(longitude, latitude) %>% # missing values on coords are not allow for transformation --> remove it
  st_as_sf(coords = c("longitude", "latitude"), crs = 4236, remove = FALSE) %>%
  st_transform(crs(template))

# Extracting values from clc data; remember keep the pixel ID 
example <- data.frame(
  pixel_id = terra::cellFromXY(template, st_coordinates(aedes_sf)),
  values = terra::extract(template, aedes_sf)
)
example$values.ID <- NULL
example$values.LABEL3 <- NULL

# Join aedes and pixel ID
aedes <- st_drop_geometry(aedes_sf) %>%
  mutate(
    pixel_id = example$pixel_id
  )
aedes <- merge(aedes, template_df %>%
                 dplyr::select(pixel_id, lon, lat), by = "pixel_id", all.x = TRUE)

ae_jap <- aedes %>% 
  dplyr::select(-ae_kor) %>%
  drop_na(ae_jap) %>% # I want to keep this NA (because it are not zeros)
  group_by(pixel_id, lon, lat, trap_type) %>%
  summarise(
    ae_jap = sum(ae_jap, na.rm = TRUE)
  ) %>% mutate(
    ae_jap = ae_jap > 0
  ) 

ae_kor <- aedes %>% 
  dplyr::select(-ae_jap) %>%
  drop_na(ae_kor) %>% # I want to keep this NA (because it are not zeros)
  group_by(pixel_id, lon, lat, trap_type) %>%
  summarise(
    ae_kor = sum(ae_kor, na.rm = TRUE)
  ) %>% mutate(
    ae_kor = ae_kor > 0
  ) 

aedes <- merge(ae_jap, ae_kor, by = c( "pixel_id", "lon", "lat", "trap_type"), all = TRUE)
rm(ae_jap, ae_kor)

saveRDS(aedes, paste0(loc.output, "ae_jap_kor_traps_reduced.rds"))

# Some plots to understand the data --------------------------------------------
## Mapping time intervals ------------------------------------------------------
for(c in c("Hungary", "Italy", "Austria", "Spain", "Germany")){
  # Germany has no sampling Effort
  print(c)
  aedes %>%
    filter(trap_type != "larval sampling" & country == c) %>%
    mutate(
      start_date = as.Date(ifelse(is.na(start_date), end_date - days(as.integer(trapping_effort)), start_date))
    ) %>%
    drop_na(end_date) %>%
    mutate(longitude = as.numeric(longitude), latitude = as.numeric(latitude)) %>%
    group_by(country) %>%
    mutate(trap_name = paste0("trap_", dense_rank(paste(longitude, latitude)))) %>%
    ungroup() %>%
    dplyr::select(country, longitude, latitude, trap_name, start_date, end_date) %>%
    ggplot(aes(x = start_date, y = trap_name, color = as.factor(year(end_date)))) +
    geom_segment(aes(xend = end_date, yend = trap_name), colour = "black", stat = "identity") +
    geom_point(size = 2) +
    geom_point(aes(x = end_date), size = 2) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_colour_discrete(name = "Years") +
    labs(
      title = c,
      x = "Month",
      y = "BG Trap name"
    ) +
    theme_bw() +
    facet_wrap(~ lubridate::year(end_date), scales = "free")
  if (c == "Austria"){
    ggsave(filename = paste0(loc.fig, "trapping_effort_trap_names_", c, ".png"),
           width = 15, height = 22)
  } else {
    ggsave(filename = paste0(loc.fig, "trapping_effort_trap_names_", c, ".png"),
           width = 12.5, height = 10) 
  }
}

