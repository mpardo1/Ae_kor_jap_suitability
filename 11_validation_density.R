# Code to compute the European suitability maps for each species.
rm(list = ls())
library(eurostat)
library(terra)
library(tidyverse)
library(data.table)
library(sf)
library(giscoR)
library(ggpubr)
library(paletteer)
pth = "U:"
pth = "~"

# Load data frmo 06_map_europe_rm.R
Path <- "/home/marta/aedes_korjap_suitability/data/Heavy_files/"
rm_agg_year <- readRDS(paste0(Path, "whole_year_RM_monthly_mean_2024_kor_jap.Rds"))

# Template raster Era5Land
Path <-"/home/marta/aedes_korjap_suitability/data/Heavy_files/era5/clim_2024/ERA5_EU_hourly_temp032024.grib"
temp_rast <- rast(Path)

# Transform to raster the data frame with suitable months
rm_agg_year_kor <- rast(rm_agg_year[,c("x", "y", "sum_kor_all")], crs = crs(temp_rast))
rm_agg_year_jap <- rast(rm_agg_year[,c("x", "y", "sum_jap_all")], crs = crs(temp_rast))

plot(rm_agg_year_kor)

# Load traps locations 
Path <- "/home/marta/aedes_korjap_suitability/data/Heavy_files/observations/test.rds"
trap_df <- readRDS(Path)

# Select number of ones and select zeros with the same size
size_1_kor <- nrow(trap_df[trap_df$ae_kor == 1,c("lon", "lat")])
size_1_jap <- nrow(trap_df[trap_df$ae_jap == 1,c("lon", "lat")])
trap_df_kor_0 <- trap_df[trap_df$ae_kor == 0,c("lon", "lat")]
trap_df_jap_0 <- trap_df[trap_df$ae_jap == 0,c("lon", "lat")]
trap_df_kor_0 <- trap_df_kor_0[sample(c(1:nrow(trap_df_kor_0)),size_1_kor),]
trap_df_jap_0 <- trap_df_jap_0[sample(c(1:nrow(trap_df_jap_0)),size_1_jap),]

# Filter
trap_df_kor_1 <- trap_df[trap_df$ae_kor == 1,c("lon", "lat")]
trap_df_jap_1 <- trap_df[trap_df$ae_jap == 1,c("lon", "lat")]

# Transform points to vect
points_vect_kor_1 <- vect(trap_df_kor_1, geom = c("lon", "lat"), crs = crs(temp_rast))
points_vect_jap_1 <- vect(trap_df_jap_1, geom = c("lon", "lat"), crs = crs(temp_rast))
points_vect_kor_0 <- vect(trap_df_kor_0, geom = c("lon", "lat"), crs = crs(temp_rast))
points_vect_jap_0 <- vect(trap_df_jap_0, geom = c("lon", "lat"), crs = crs(temp_rast))

# Extract values
values_kor_1 <- terra::extract(rm_agg_year_kor, points_vect_kor_1)
values_jap_1 <- terra::extract(rm_agg_year_jap, points_vect_jap_1)
values_kor_0 <- terra::extract(rm_agg_year_kor, points_vect_kor_0)
values_jap_0 <- terra::extract(rm_agg_year_jap, points_vect_jap_0)

# Merge extracted values with points_df
result_kor_1 <- cbind(trap_df_kor_1, values_kor_1[, -1])
result_jap_1 <- cbind(trap_df_jap_1, values_jap_1[, -1])
result_kor_0 <- cbind(trap_df_kor_0, values_kor_0[, -1])
result_jap_0 <- cbind(trap_df_jap_0, values_jap_0[, -1])

# Change name
colnames(result_kor_1)[3] <- "suit_months"
colnames(result_jap_1)[3] <- "suit_months"
colnames(result_kor_0)[3] <- "suit_months"
colnames(result_jap_0)[3] <- "suit_months"

# Remove na
result_kor_1 <- result_kor_1[!is.na(result_kor_1$suit_months),]
result_jap_1 <- result_jap_1[!is.na(result_jap_1$suit_months),]
result_kor_0 <- result_kor_0[!is.na(result_kor_0$suit_months),]
result_jap_0 <- result_jap_0[!is.na(result_jap_0$suit_months),]

# Compute mean
mean(result_kor_1$suit_months)
mean(result_jap_1$suit_months)
# Fake data
df_fake <-data.frame(x=0, y=0,
                     category = c("0","1"))

# Plot density 1/0 Ae.japonicus
col_kor <- "#440154"
col_jap <- "#FDE725"
plot_jap <- ggplot() +
  geom_density(data = result_jap_1, aes(suit_months),
               color = col_jap, fill =col_jap, alpha= 0.5)+
  geom_density(data = result_jap_0, aes(suit_months),
               color =col_kor, fill =col_kor, alpha= 0.5) +
  geom_point(data=df_fake, aes(x,y,color = category),
             size=1.4, fill = NA , na.rm=TRUE) +
  scale_color_manual(name = "",
                     values= c(col_kor, col_jap)) +
  xlab("Number of suitable months") +
  ylab("Density") +
  theme_bw() +
  theme(  legend.position = c(0.9,0.8),
         plot.title = element_text(size = 15, face="italic"), # Change title size
         axis.title.x = element_text(size = 15), # Change x-axis title size
         axis.title.y = element_text(size = 15), # Change y-axis title size
         axis.text.x = element_text(size = 15), # Change x-axis text size
         axis.text.y = element_text(size = 15),
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 12)) +
  ggtitle("Ae. japonicus")

# Plot density 1/0 Ae.koreicus
plot_kor <- ggplot() +
  geom_density(data = result_kor_1, aes(suit_months),
               color = col_jap, fill =col_jap, alpha= 0.5)+
  geom_density(data = result_kor_0, aes(suit_months),
               color =col_kor, fill =col_kor, alpha= 0.5) +
  geom_point(data=df_fake, aes(x,y,color = category),
             size=1.4, fill = NA , na.rm=TRUE) +
  scale_color_manual(name = "",
                     values= c(col_kor, col_jap)) +
  xlab("Number of suitable months") +
  ylab("Density") +
  theme_bw() +
  theme(  legend.position = c(0.9,0.8),
         plot.title = element_text(size = 15, face="italic"), # Change title size
         axis.title.x = element_text(size = 15), # Change x-axis title size
         axis.title.y = element_text(size = 15), # Change y-axis title size
         axis.text.x = element_text(size = 15), # Change x-axis text size
         axis.text.y = element_text(size = 15),
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 12)) +
  ggtitle("Ae. koreicus")

# Plot density 1 Ae.japonicus and Ae. koreicus
# Fake data
df_fake <-data.frame(x=0, y=0,
                     category = c("Ae. japonicus","Ae. koreicus"))
col_jap <- "#31B57B"
col_kor <- "#33628D"
plot_kor_jap <- ggplot() +
  geom_density(data = result_jap_1, aes(suit_months),
               color = col_jap, fill =col_jap, alpha= 0.5)+
  geom_density(data = result_kor_1, aes(suit_months),
               color =col_kor, fill =col_kor, alpha= 0.5) +
  geom_point(data=df_fake, aes(x,y,color = category),
             size=1.7, fill = NA , na.rm=TRUE) +
  scale_color_manual(name = "",
                     values= c(col_jap, col_kor)) +
  xlab("Number of suitable months") +
  ylab("Density") +
  theme_bw() +
  theme( legend.position = c(0.9,0.8),
         plot.title = element_text(size = 15), # Change title size
         axis.title.x = element_text(size = 15), # Change x-axis title size
         axis.title.y = element_text(size = 15), # Change y-axis title size
         axis.text.x = element_text(size = 15), # Change x-axis text size
         axis.text.y = element_text(size = 15),
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 12)) +
  ggtitle("Presence")

# arrange plots
gg_dens <- ggarrange(ggarrange(plot_jap,plot_kor,  labels = c("a", "b")),
          plot_kor_jap, ncol = 1, labels = c("", "c"))
gg_dens

# Save plots
ggsave("~/Documentos/PHD/2025/Aedes_land/Plots/density_validation.pdf",
       width =10, height = 8)

ggsave("~/Documentos/PHD/2025/Aedes_land/Plots/density_validation.png",
       width =10, height = 8, dpi = 300)
