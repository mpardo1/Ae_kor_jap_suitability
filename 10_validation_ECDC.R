rm(list=ls())
library(sf)
library(ggplot2)
library(dplyr)

# Read file pa species EU ECDC
Path <- "/home/marta/aedes_korjap_suitability/data/Heavy_files/observations/status/status.shp"
pa_eu <- read_sf(Path)

# Plot pa koreicus
ggplot(pa_eu[pa_eu$codeLevel == 3 &
               pa_eu$koreicus != "introduced_MA"  ,]) +
  geom_sf(aes(fill = as.factor(koreicus))) +
  xlim(c(-20,40)) + ylim(c(30,70))

ggplot(pa_eu[pa_eu$codeLevel == 3 &
               pa_eu$japonicus != "introduced_MA"  ,]) +
  geom_sf(aes(fill = as.factor(japonicus))) +
  xlim(c(-20,40)) + ylim(c(30,70))

# Yearly koreicus japonicus 2024
Path <- "/home/marta/aedes_korjap_suitability/data/Heavy_files/"
rm_agg_year <- readRDS(paste0(Path, "whole_year_RM_monthly_mean_2024_kor_jap.Rds"))

# Transform data frame into sf object
rm_agg_sf <- st_as_sf(rm_agg_year, coords = c("x", "y"), crs = st_crs(pa_eu))

# Check projection
rm_agg_sf <- st_transform(rm_agg_sf, crs = st_crs(pa_eu))

# Split data frame between absent and presence
pa_eu_1_jap <- pa_eu[pa_eu$japonicus %in% c("established","introduced_ECDC"),]
pa_eu_0_jap <- pa_eu[pa_eu$japonicus %in% c("absent"),]
pa_eu_1_kor <- pa_eu[pa_eu$koreicus %in% c("established","introduced_ECDC"),]
pa_eu_0_kor <- pa_eu[pa_eu$koreicus %in% c("absent"),]

# asign each point to a geometry
points_to_sf <- st_join(rm_agg_sf[,c("sum_jap_all", "sum_kor_all")],
                        pa_eu[pa_eu$japonicus %in%
                                c("established","introduced_ECDC","absent") |
                                pa_eu$koreicus %in%
                                c("established","introduced_ECDC","absent"),
                              c("koreicus", "japonicus","locCode" )])
points_to_sf <- points_to_sf[!is.na(points_to_sf$locCode),]

# Change columjaponicus and koreicus to 1-0
points_to_sf$japonicus <- ifelse(points_to_sf$japonicus %in%
                                   c("established"),1,0)
points_to_sf$koreicus <- ifelse(points_to_sf$koreicus %in%
                                   c("established"),1,0)

# Colors for plots
col_0 <- "#440154"
col_1 <- "#FDE725"

# Plot density koreicus
ggplot(points_to_sf) +
  geom_density(aes(sum_kor_all, group = as.factor(koreicus),
                   fill = as.factor(koreicus)), alpha = 0.5) +
                 scale_fill_manual(values = c(col_0,col_1),name = "")  +
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
  ggtitle("Ae. koreicus")
               
# Japonicus
ggplot(points_to_sf) +
  geom_density(aes(sum_jap_all, group = as.factor(japonicus),
                   fill = as.factor(japonicus)), alpha = 0.5)+
  scale_fill_manual(values = c(col_jap,col_kor),name = "")+
  scale_fill_manual(values = c(col_0,col_1),name = "")  +
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
  ggtitle("Ae. koreicus")


# Extract lon lat coordinates
points_to_sf <- points_to_sf %>% 
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>% st_drop_geometry()
points_to_sf <- pa_eu[,"locCode"] %>% left_join(points_to_sf)

# Add fake columns for legend
points_to_sf[nrow(points_to_sf)+1,] <- points_to_sf[nrow(points_to_sf),]
points_to_sf[nrow(points_to_sf)+1,] <- points_to_sf[nrow(points_to_sf),]
points_to_sf[nrow(points_to_sf)+1,] <- points_to_sf[nrow(points_to_sf),]
points_to_sf[nrow(points_to_sf)+1,] <- points_to_sf[nrow(points_to_sf),]
points_to_sf[nrow(points_to_sf)-3,]$sum_jap_all <- 9
points_to_sf[nrow(points_to_sf)-2,]$sum_jap_all <- 10
points_to_sf[nrow(points_to_sf)-1,]$sum_jap_all <- 11
points_to_sf[nrow(points_to_sf),]$sum_jap_all <- 12

# Plot
library(paletteer)
pal <- rev(paletteer_c("grDevices::RdYlBu", 13))
kor_ecdc <- ggplot() +
  geom_raster(data =  points_to_sf, aes(x,y,fill=as.factor(sum_kor_all)))+
  geom_sf(data = unique(points_to_sf[points_to_sf$koreicus == 1,c("koreicus")]),
          aes(color = as.factor(koreicus)), fill = NA, size = 2) +
  xlim(c(-20,45)) + ylim(c(30,60)) +
  scale_fill_manual(values = pal,
                    name = "Nº suitable \n months",
                    limits = factor(seq(0,12,1)),
                    na.value = "#FCFCFC", guide = "none") +
  scale_color_manual(values = "red", name = "", guide="none" ) +
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
        legend.box = "horizontal") +
  theme_void() +
  ylim(c(35,55)) + xlim(c(-10,30)) 

jap_ecdc <- ggplot() +
  geom_raster(data =  points_to_sf, aes(x,y,fill=as.factor(sum_jap_all)))+
  geom_sf(data = unique(points_to_sf[points_to_sf$japonicus == 1,c("japonicus")]),
          aes(color = as.factor(japonicus)), fill = NA, size = 2) +
  xlim(c(-20,45)) + ylim(c(30,60)) +
  scale_fill_manual(values = pal,
                    name = "Nº suitable \n months",
                    limits = factor(seq(0,12,1)),
                    na.value = "#FCFCFC") +
  scale_color_manual(values = "red", guide="none") +
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
        legend.box = "horizontal") +
  theme_void() +
  ylim(c(35,55)) + xlim(c(-10,30)) 

# Arrange plots
library(ggpubr)
leg <- get_legend(jap_ecdc)
ggarrange(jap_ecdc + ggtitle("Aedes japonicus") +
            theme(legend.position = "none",
                  plot.title=element_text(face="italic",
                                          size = 14)),
          kor_ecdc + ggtitle("Aedes koreicus") +
            theme(legend.position = "none",
                  plot.title=element_text(face="italic",
                                          size = 14)),
          leg, widths = c(1,1,0.2), ncol = 3)

# Save plots
ggsave("~/Documentos/PHD/2025/Aedes_land/Plots/2024_maps_with_ECDC.pdf",
       width =11.6, height = 4.4)

ggsave("~/Documentos/PHD/2025/Aedes_land/Plots/2024_maps_with_ECDC.png",
       width =11.6, height = 4.4, dpi = 300)
