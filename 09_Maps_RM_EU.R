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
source(paste0(pth,"/aedes_korjap_suitability/funcR0.R"))

# Template raster Era5Land
Path <-"/home/marta/aedes_korjap_suitability/data/Heavy_files/era5/clim_2024/ERA5_EU_hourly_temp032024.grib"
temp_rast <- rast(Path)

# Load EU countries and compute the perimeter
library(rnaturalearth)

# European map NUTS3
EU <- get_eurostat_geospatial(nuts_level=0, year=2016)
plot(EU[,"NUTS_ID"])

# Load climate 2024 # Extracted from 071_Process_ERA5_2023.R
Path <- "/home/marta/aedes_korjap_suitability/data/Heavy_files/era5/clim_2024/"
clim_df <- setDT(readRDS(paste0(Path, "clim_2024_EU.Rds")))
clim_df$temp <- clim_df$temp - 273.15
clim_df$rain <- clim_df$rain*1000

# Transform to raster and mask non eu countries
clim_df_filt <- data.frame()
for(i in c(1:12)){
  print(paste0("i:",i))
  # Rasterize to remove non-EU countries
  clim_df_r <- rast(clim_df[clim_df$month == i,c(1,2,4,5)], crs = crs(temp_rast))
  EU_r <- st_transform(EU, crs(clim_df_r))
  # plot(EU_r)

  # Mask non-EU countries
  clim_df_r <- mask(crop(clim_df_r, EU_r), EU_r )
  # plot(clim_df)

  # Transform to data frame and add month
  aux_df <- as.data.frame(clim_df_r, xy=TRUE)
  aux_df$month <- i

  # Join to the df
  clim_df_filt <- rbind(clim_df_filt,aux_df)

}

# # Save rds
# saveRDS(clim_df_filt, "/home/marta/Documentos/PHD/2025/VEO/clim_2024_EU.Rds")

# Test
ggplot(clim_df_filt[clim_df_filt$month == 3,]) +
  geom_raster(aes(x=x, y = y, fill = rain)) +
  scale_fill_viridis_c()

# # Template raster
# clim_df_r <- rast(clim_df[clim_df$month == 4,c(1,2,4,5)], crs = crs(temp_rast))

# Load landcover -----------------------------------------------
Path <- "/home/marta/aedes_korjap_suitability/data/Heavy_files/landcover/jap_predictedprob_train_filterzeros_spatial_norain2_SEQ3_notraptype.rds"
landcover_jap <- readRDS(Path)[,c(2,3,4)]
colnames(landcover_jap) <- c("x","y","lc_jap")

Path <- "/home/marta/aedes_korjap_suitability/data/Heavy_files/landcover/kor_predictedprob_train_filterzeros_spatial_norain2_SEQ3_notraptype.rds"
landcover_kor <- readRDS(Path)[,c(2,3,4)]
colnames(landcover_kor) <- c("x","y","lc_kor")

# Test
ggplot(landcover_kor) + geom_raster(aes(x=x, y = y, fill = lc_kor)) +
  scale_fill_viridis_c()
ggplot(landcover_jap) + geom_raster(aes(x=x, y = y, fill = lc_jap)) +
  scale_fill_viridis_c()

# # NA values are in real zero
# landcover_jap[which(is.na(landcover_jap$lc_jap) ), ]$lc_jap <- 0
# landcover_kor[which(is.na(landcover_kor$lc_kor) ), ]$lc_kor <- 0

# Transform them into a raster to after join them
rast_lc_jap_r <- rast(landcover_jap, type = "xyz", crs = crs(clim_df_r))
rast_lc_jap_r <- resample(rast_lc_jap_r, clim_df_r, method= "cubic")
plot(rast_lc_jap_r)
rast_lc_jap_r <- mask(rast_lc_jap_r, EU)
rast_lc_kor_r <- rast(landcover_kor, type = "xyz", crs = crs(clim_df_r))
rast_lc_kor_r <- resample(rast_lc_kor_r, clim_df_r, method= "cubic")
rast_lc_kor_r <- mask(rast_lc_kor_r, EU)
plot(rast_lc_kor_r)

# Transform to raster
rast_lc_jap <- as.data.frame(rast_lc_jap_r, xy=TRUE)
rast_lc_kor <- as.data.frame(rast_lc_kor_r, xy=TRUE)

# Join data set landcover and climates -----------------------------------
clim_lc_df <- setDT(clim_df_filt %>%  left_join(rast_lc_jap) %>%  left_join(rast_lc_kor))

# NAs in landcover are in real zeros
clim_lc_df[is.na(clim_lc_df$lc_jap) & !is.na(clim_lc_df$temp),]$lc_jap <- 0
clim_lc_df[is.na(clim_lc_df$lc_kor) & !is.na(clim_lc_df$temp),]$lc_kor <- 0

# Panel Landcover ---------------------------------------------
# pal <- rev(paletteer_c("grDevices::Heat", 13))
# pal <- rev(paletteer_c("grDevices::OrRd", 13))
# pal <- (paletteer_c("ggthemes::Spectral", 13))
pal <- rev(paletteer_c("grDevices::Spectral", 12))
# pal <- rev(paletteer_c("grDevices::RdYlBu", 12))
# pal <- paletteer_c("ggthemes::Temperature Diverging", 13)
# pal <- paletteer_c("ggthemes::Sunset-Sunrise Diverging", 13)
# pal <- paletteer_c("grDevices::Zissou 1", 13)
# pal <- c(pal[6:18])
pal <- colorRampPalette(pal)
letsize = 16
size_per = 0.6
plot_jap_lc <- ggplot() +
  geom_raster(data = rast_lc_jap, aes(x = x, y = y, 
                                      fill = lc_jap),alpha = 1) +
  geom_sf(data = EU, fill = NA, color = "black", size = 1.5)+
  scale_fill_gradientn(colors = pal(20),
                       name = "Landcover \nsuitability\n",
                       na.value = "#FCFCFC",
                       limits = c(0,1)) +
  ylim(c(35,73)) + xlim(c(-30,45)) +
  xlab("") + ylab("") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks.length = unit(0, "null"),
        axis.ticks.margin = unit(0, "null"), 
        legend.box = "horizontal",
        legend.title= element_text(hjust=0.5))

plot_kor_lc <- ggplot() +
  geom_raster(data = rast_lc_kor, aes(x = x, y = y, 
                                      fill = lc_kor),alpha = 1) +
  geom_sf(data = EU, fill = NA, color = "black", size = 1.5)+
  scale_fill_gradientn(colors = pal(20),
                       name = "Landcover \nsuitability",
                       na.value = "#FCFCFC",
                       limits = c(0,1)) +
  ylim(c(35,73)) + xlim(c(-30,45)) +
  xlab("") + ylab("") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks.length = unit(0, "null"),
        axis.ticks.margin = unit(0, "null"), 
        legend.box = "horizontal")

# Extract legend to arrange plots nicely
leg_land <- get_legend(plot_jap_lc )

# Arrange and save
ggarrange(plot_jap_lc+ ggtitle("Aedes japonicus") +
            theme(legend.position = "none",
                  plot.title=element_text(face="italic")),
          plot_kor_lc + ggtitle("Aedes koreicus") +
            theme(legend.position = "none",
                  plot.title=element_text(face="italic")),
          leg_land,
          widths = c(1,1,0.17) , nrow = 1, ncol = 3)

# Save plots
ggsave("~/Documentos/PHD/2025/Aedes_land/Plots/landcover_suitability_Zissou.pdf",
       width =10, height = 5)

ggsave("~/Documentos/PHD/2025/Aedes_land/Plots/landcover_suitability_Zissou.png",
       width = 10, height = 5, dpi = 300)

# Only temperature
clim_lc_df[, R0_kor_t := mapply(R0_func_kor_only_temp, temp)]
clim_lc_df[, R0_jap_t := mapply(R0_func_jap_only_temp, temp)]

# With rainfall no landcover
clim_lc_df[, R0_kor_r := mapply(R0_func_kor_lc_hat, temp, rain,1)]
clim_lc_df[, R0_jap_r := mapply(R0_func_jap_lc_hat, temp, rain,1)]

# With temperature, rainfall and landcover multiplying fecundity
clim_lc_df[, R0_kor_lc := mapply(R0_func_kor, temp, rain,lc_kor)]
clim_lc_df[, R0_jap_lc := mapply(R0_func_jap, temp, rain,lc_jap)]

# # With temperature, rainfall and landcover multiplying hatching rate
clim_lc_df[, R0_kor_lc_hat := mapply(R0_func_kor_lc_hat, temp, rain,lc_kor)]
clim_lc_df[, R0_jap_lc_hat := mapply(R0_func_jap_lc_hat, temp, rain,lc_jap)]

# Test
ggplot(clim_lc_df[clim_lc_df$month==8 & clim_lc_df$R0_jap_r>1,]) +
  geom_raster(aes(x=x, y = y, fill = R0_jap_lc)) + scale_fill_viridis_c()
ggplot(clim_lc_df[clim_lc_df$month==7,]) +
  geom_raster(aes(x=x, y = y, fill = R0_kor_lc_hat)) + scale_fill_viridis_c()

# Normalize RM and multiply by Landcover
clim_lc_df$norm_rm_jap <- clim_lc_df$R0_jap_r/max(clim_lc_df$R0_jap_r, na.rm=TRUE)
clim_lc_df$norm_rm_kor <- clim_lc_df$R0_kor_r/max(clim_lc_df$R0_kor_r, na.rm=TRUE)
clim_lc_df$R0_jap_lc_norm <- clim_lc_df$norm_rm_jap*clim_lc_df$lc_jap
clim_lc_df$R0_kor_lc_norm <- clim_lc_df$norm_rm_kor*clim_lc_df$lc_kor

# Monthly  Plot--------------------------------------------
library(latex2exp)
monthly_plots <- function(month_n, esp){
  max_rm_jap <- max(c(clim_lc_df$R0_jap_lc), na.rm=TRUE)
  max_rm_kor <- max(c(clim_lc_df$R0_kor_lc), na.rm=TRUE)
  
  if(esp == "japonicus"){
    ggplot(clim_lc_df[clim_lc_df$month==month_n,]) +
      geom_raster(aes(x=x, y = y, fill = R0_jap_lc)) + 
      geom_sf(data = EU, fill = NA, color = "black", size = 1.5)+
      scale_fill_gradientn(colors = pal(20),
                           name = TeX("R$_{M}$\n"),
                           na.value = "#FCFCFC",
                           limits = c(0,max_rm_jap+0.01)) +
      ylim(c(35,73)) + xlim(c(-25,45)) +
      xlab("") + ylab("") +
      theme_minimal() + ggtitle(paste0("Month:", month_n)) +
      theme(panel.grid = element_blank(),
            panel.border = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null"),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.line = element_blank(),
            axis.ticks.length = unit(0, "null"),
            axis.ticks.margin = unit(0, "null"), 
            legend.box = "horizontal")
  }else{
    ggplot(clim_lc_df[clim_lc_df$month==month_n,]) +
      geom_raster(aes(x=x, y = y, fill = R0_kor_lc)) + 
      geom_sf(data = EU, fill = NA, color = "black", size = 1.5)+
      scale_fill_gradientn(colors = pal(20),
                           name = TeX("R$_{M}$ \n"),
                           na.value = "#FCFCFC",
                           limits = c(0,max_rm_kor+0.01)) +
      ylim(c(35,73)) + xlim(c(-25,45)) +
      xlab("") + ylab("") + ggtitle(paste0("Month:", month_n)) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            panel.border = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null"),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.line = element_blank(),
            axis.ticks.length = unit(0, "null"),
            axis.ticks.margin = unit(0, "null"), 
            legend.box = "horizontal")
  }
  
}

# Arrange monthly plots --------------------------------------------------
# Koreicus
ggarrange(monthly_plots(5, "koreicus"), monthly_plots(6, "koreicus"), 
          monthly_plots(7, "koreicus"), monthly_plots(8, "koreicus"),
          monthly_plots(9, "koreicus"), monthly_plots(10, "koreicus"),
          nrow = 2, ncol = 3,
          common.legend =TRUE)

# Save plots
ggsave("~/Documentos/PHD/2025/Aedes_land/Plots/monthly_2024_R_M_kor.pdf",
       width =8.17, height = 6.4)

ggsave("~/Documentos/PHD/2025/Aedes_land/Plots/monthly_2024_R_M_kor.png",
       width =8.17, height = 6.4, dpi = 300)

# Japnicus
ggarrange(monthly_plots(5, "japonicus"), monthly_plots(6, "japonicus"), 
          monthly_plots(7, "japonicus"), monthly_plots(8, "japonicus"),
          monthly_plots(9, "japonicus"), monthly_plots(10, "japonicus"),
          nrow = 2, ncol = 3,
          common.legend =TRUE)

# Save plots
ggsave("~/Documentos/PHD/2025/Aedes_land/Plots/monthly_2024_R_M_jap.pdf",
       width =8.17, height = 6.4)

ggsave("~/Documentos/PHD/2025/Aedes_land/Plots/monthly_2024_R_M_jap.png",
       width =8.17, height = 6.4, dpi = 300)

# # Aggregate annually
# clim_lc_df$bool_kor_t <- ifelse(clim_lc_df$R0_kor_t>1,1,0)
# clim_lc_df$bool_jap_t <- ifelse(clim_lc_df$R0_jap_t>1,1,0)
# clim_lc_df$bool_kor_r <- ifelse(clim_lc_df$R0_kor_r>1,1,0)
# clim_lc_df$bool_jap_r <- ifelse(clim_lc_df$R0_jap_r>1,1,0)
# clim_lc_df$bool_kor_all <- ifelse(clim_lc_df$R0_kor_lc>1,1,0)
# clim_lc_df$bool_jap_all <- ifelse(clim_lc_df$R0_jap_lc>1,1,0)
# clim_lc_df$bool_kor_all_lc_hat <- ifelse(clim_lc_df$R0_kor_lc_hat>1,1,0)
# clim_lc_df$bool_jap_all_lc_hat <- ifelse(clim_lc_df$R0_jap_lc_hat>1,1,0)
# clim_lc_df$bool_kor_all_norm <- ifelse(clim_lc_df$R0_kor_lc_norm>0,1,0)
# clim_lc_df$bool_jap_all_norm <- ifelse(clim_lc_df$R0_jap_lc_norm>0,1,0)
# clim_lc_df$bool_kor_all_norm_max <- ifelse(clim_lc_df$R0_kor_lc>1/max(clim_lc_df$R0_kor_r, na.rm=TRUE),1,0)
# clim_lc_df$bool_jap_all_norm_max <- ifelse(clim_lc_df$R0_jap_lc>1/max(clim_lc_df$R0_jap_r, na.rm=TRUE),1,0)
# clim_lc_df[,bool_kor_all_norm_foll := ifelse(any(month == 8 & bool_kor_all_norm == 0),0,bool_kor_all_norm), by = .(x, y)]
# clim_lc_df[,bool_jap_all_norm_foll := ifelse(any(month == 8 & bool_jap_all_norm == 0),0,bool_jap_all_norm), by = .(x, y)]
# rm_agg_year <- clim_lc_df[,.(sum_kor_t = sum(bool_kor_t),
#                              sum_jap_t = sum(bool_jap_t),
#                           sum_kor_r = sum(bool_kor_r),
#                           sum_jap_r = sum(bool_jap_r),
#                           sum_kor_all = sum(bool_kor_all),
#                           sum_jap_all = sum(bool_jap_all),
#                           sum_kor_all_lc_hat = sum(bool_kor_all_lc_hat),
#                           sum_jap_all_lc_hat = sum(bool_jap_all_lc_hat),
#                           sum_kor_all_norm = sum(bool_kor_all_norm),
#                           sum_jap_all_norm = sum(bool_jap_all_norm),
#                           sum_kor_all_norm_max = sum(bool_kor_all_norm_max),
#                           sum_jap_all_norm_max = sum(bool_jap_all_norm_max),
#                           sum_kor_all_norm_foll = sum(bool_kor_all_norm_foll),
#                           sum_jap_all_norm_foll = sum(bool_jap_all_norm_foll)), by = .(x,y) ]
# #
# # # Save the data frame
# Path <- "/home/marta/aedes_korjap_suitability/data/Heavy_files/"
# saveRDS(rm_agg_year, paste0(Path, "whole_year_RM_monthly_mean_2024_kor_jap.Rds"))

# Read RDS with results
rm_agg_year <- readRDS(paste0(Path, "whole_year_RM_monthly_mean_2024_kor_jap.Rds"))

# Cut Europe shapefile
clim_df_r <- rast(clim_df[clim_df$month == 4,c(1,2,4,5)], crs = crs(temp_rast))
EU_r <- st_transform(EU, crs(clim_df_r))
clim_df_r <- mask(crop(clim_df_r, EU_r), EU_r )
EU <- st_crop(EU, ext(clim_df_r))
plot(EU)

# Remove non EU
rm_agg_year_sf <- st_as_sf(rm_agg_year, coords = c("x", "y"), crs = 4326)
rm_agg_year_sf <- rm_agg_year_sf[EU, ]
rm_agg_year <- as.data.frame(rm_agg_year_sf)

# Add lon and lat back as regular columns
coords <- st_coordinates(rm_agg_year_sf)
rm_agg_year$x <- coords[, 1]
rm_agg_year$y <- coords[, 2]

# Plot the Eu map ------------------------------------------------------
letsize = 16
size_per = 0.6

# Function to plot sum months
# Input: var: variable name on the data frame
#        pal: paleta de colores
# output: Map with the number of suitable months
plot_summonths <- function(var, pal){
  ind <- which(colnames(rm_agg_year) == var)
  rm_agg_year$aux <- rm_agg_year[,ind]
  plot_map <- ggplot() +
    geom_raster(data = rm_agg_year, aes(x = x, y = y, 
                                        fill = as.factor(aux)),alpha = 1) +
    geom_sf(data = EU, fill = NA, color = "black", size = 1.5)+
    scale_fill_manual(values = pal,
                      name = "Nº suitable \n months",
                      limits = factor(seq(0,12,1)),
                      na.value = "#FCFCFC") +
    # ylim(c(30,73)) + xlim(c(-30,40)) +
    xlab("") + ylab("") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "null"),
          panel.margin = unit(c(0, 0, 0, 0), "null"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks.length = unit(0, "null"),
          axis.ticks.margin = unit(0, "null"), 
          legend.box = "horizontal")
  
  return(plot_map)
}

# Select palette
# pal <- rev(paletteer_c("grDevices::OrRd", 13))
# # pal <- (paletteer_c("grDevices::Geyser", 13))
# pal <- (paletteer_c("ggthemes::Temperature Diverging", 13))
# pal <- c(pal[3:18])
# pal <- rev(paletteer_c("grDevices::RdYlGn", 18))
# pal <- rev(paletteer_c("grDevices::Spectral", 18))
# pal <- c(pal[6:18])
# pal <- paletteer_c("grDevices::Lajolla", 13)
# pal <- rev(paletteer_c("grDevices::YlOrRd", 13))
# pal <- rev(paletteer_c("grDevices::Heat", 13))
# pal <- rev(paletteer_c("grDevices::heat.colors", 13))
# pal <- rev(paletteer_c("grDevices::Inferno", 13))
# pal <- rev(paletteer_c("grDevices::PinkYl", 13))
# pal <- paletteer_c("ggthemes::Blue-Green Sequential", 13)
# pal <- c("white",pal)
# pal <- paletteer_c("ggthemes::Temperature Diverging", 18)
pal <- rev(paletteer_c("grDevices::RdYlBu", 13))
# pal <- rev(paletteer_c("grDevices::Spectral", 13))
# pal <- paletteer_c("grDevices::Zissou 1", 13)
# pal <- paletteer_c("grDevices::Zissou 1", 18)
# pal <- paletteer_c("ggthemes::Sunset-Sunrise Diverging", 18)
# pal <- pal[6:18]

# add Na layer
rm_agg_year[nrow(rm_agg_year)+1,] <- c(11,11,11,11,11,11,11,11,11,11,11,11,11,11,NA,NA,NA)

# Maps temperature and rainfall
plot_kor_t <- plot_summonths("sum_kor_t",pal)
plot_jap_t <- plot_summonths("sum_jap_t",pal)

ggarrange(plot_jap_t + ggtitle("Aedes japonicus"),
          plot_kor_t + ggtitle("Aedes koreicus"),
          common.legend = TRUE)

# Plot the Eu map no landcover
plot_kor <- plot_summonths("sum_kor_r", pal)
plot_jap <- plot_summonths("sum_jap_r",pal)

# Plot with landcover inside RM
plot_kor_all <- plot_summonths("sum_kor_all",pal)
plot_jap_all <- plot_summonths("sum_jap_all", pal)

# Plot with landcover* normalized RM
plot_kor_all_norm <- plot_summonths("sum_kor_all_norm", pal)
plot_jap_all_norm <- plot_summonths("sum_jap_all_norm", pal)

# Plot with landcover* normalized RM threshold 1
plot_kor_all_norm_max <- plot_summonths("sum_kor_all_norm_max", pal)
plot_jap_all_norm_max <- plot_summonths("sum_jap_all_norm_max", pal)

# Plot with landcover multiplying hatching rate
plot_kor_all_lc_hat <- plot_summonths("sum_kor_all_lc_hat", pal)
plot_jap_all_lc_hat <- plot_summonths("sum_jap_all_lc_hat", pal)

# Extract Legend 
leg_sum <- get_legend(ggplot(rm_agg_year) +
                        geom_raster(aes(x = x, y = y, 
                                        fill = as.factor(sum_jap_r)),alpha = 1) +
                        scale_fill_manual(values = pal,
                                          name = "Nº suitable \n months",
                                          limits = factor(seq(0,12,1)),
                                          na.value = "#FCFCFC")+
                        theme(legend.position = "left",
                              legend.text = element_text(size = 12),
                              legend.title = element_text(size = 13)))

# Arrange plots
ggarrange(ggarrange(
          plot_jap_t + #ggtitle("Aedes japonicus") + 
            theme(legend.position = "none",
                  plot.title=element_text(face="italic", size = 14)),
          plot_jap + #ggtitle("Aedes japonicus") +
            theme(legend.position = "none",
                  plot.title=element_text(face="italic", size = 14)),
          plot_jap_all +# ggtitle("Aedes japonicus") + 
            theme(legend.position = "none",
          plot.title=element_text(face="italic", size = 14)),
          plot_kor_t + #ggtitle("Aedes koreicus") +
            theme(legend.position = "none",
                  plot.title=element_text(face="italic", size = 14)),
          
          plot_kor +# ggtitle("Aedes koreicus") +
            theme(legend.position = "none",
                  plot.title=element_text(face="italic", size = 14)),
          plot_kor_all + #ggtitle("Aedes koreicus") +
            theme(legend.position = "none",
                  plot.title=element_text(face="italic", size = 14)),
          widths = c(1,1,1),
          common.legend = FALSE, ncol = 3, nrow = 2, labels = c("a", "b", "c", "d", "e", "f")),
          NULL, leg_sum,
          widths= c(1,0.01,0.1), ncol = 3)

# Save plots
ggsave("~/Documentos/PHD/2025/Aedes_land/Plots/2024_maps_RdYlBu.pdf",
       width =9, height = 5)

ggsave("~/Documentos/PHD/2025/Aedes_land/Plots/2024_maps_RdYlBuwhole.png",
       width = 11.5, height = 5, dpi = 300)

# Add traps locations to plots ---------------------------------------------
Path <- "/home/marta/aedes_korjap_suitability/data/Heavy_files/observations/test.rds"
trap_df <- readRDS(Path)

# Koreicus
plot_kor_trap <- plot_kor +
  geom_point(data = trap_df[ !is.na(trap_df$ae_kor) & trap_df$ae_kor == 1, ],
             aes(x=lon, y=lat), color = "black", alpha = 0.5, size = 0.3) 
  

# Japonicus
plot_jap_trap <- plot_jap +
  geom_point(data = trap_df[!is.na(trap_df$ae_jap)  & trap_df$ae_jap == 1, ],
             aes(lon, lat), color = "black", size = 0.3, alpha= 0.3) 

# Koreicus
rm_agg_year[nrow(rm_agg_year)+1,] <- c(NA,NA,NA,NA,NA,NA,NA,NA,10,10,NA,NA,NA,NA,NA,NA,NA)
rm_agg_year[nrow(rm_agg_year)+1,] <- c(NA,NA,NA,NA,NA,NA,NA,NA,11,11,NA,NA,NA,NA,NA,NA,NA)
rm_agg_year[nrow(rm_agg_year)+1,] <- c(NA,NA,NA,NA,NA,NA,NA,NA,12,12,NA,NA,NA,NA,NA,NA,NA)

ycord <- 55
plot_kor_trap_all <- plot_kor_all +
  geom_point(data = trap_df[ !is.na(trap_df$ae_kor) & trap_df$ae_kor == 1, ],
             aes(x=lon, y=lat), color = "black", size = 0.3) +
  ylim(c(35,55)) + xlim(c(-10,30))

# Japonicus
plot_jap_trap_all <- plot_jap_all +
  geom_point(data = trap_df[!is.na(trap_df$ae_jap)  & trap_df$ae_jap == 1, ],
             aes(lon, lat), color = "black", size = 0.3) +
  ylim(c(35,55)) + xlim(c(-10,30)) 

plot_jap_trap_all

# Arrange with all variables
ggarrange(ggarrange(plot_jap_trap_all+ ggtitle("Aedes japonicus") +
                      theme(legend.position = "none",
                            plot.title=element_text(face="italic",
                                                    size = 14)),
                    plot_kor_trap_all+ ggtitle("Aedes koreicus") +
                      theme(legend.position = "none",
                            plot.title=element_text(face="italic",
                                                    size = 14)),
                    ncol = 2,nrow =1),
          leg_sum, widths = c(1,0.1))

# Save plots
ggsave("~/Documentos/PHD/2025/Aedes_land/Plots/2024_maps_with_traps.pdf",
       width =11.6, height = 4.4)

ggsave("~/Documentos/PHD/2025/Aedes_land/Plots/2024_maps_with_traps.png",
       width =11.6, height = 4.4, dpi = 300)

