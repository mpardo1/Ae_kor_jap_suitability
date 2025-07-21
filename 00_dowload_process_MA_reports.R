# Download data from MA: https://labs.mosquitoalert.com/metadata_public_portal/README.html
# To transform it into PA
# Load required libraries
rm(list=ls())
library(jsonlite)  # For JSON handling
library(dplyr)     
library(mapSpain)
library(ggplot2)
library(sf)
library(jsonlite)  # For JSON handling

# Read list of files 
Path <- "/home/marta/aedes_korjap_suitability/data/Heavy_files/reports/"
list_files <- list.files(Path)

# Function to read json reports and filter
filter_repots <- function(ind, species_name){
  df_rep <- fromJSON(paste0(Path, list_files[[ind]]))
  
  # Filter reports of adults
  df_rep <- df_rep[df_rep$type == "adult",]
  
  # Filter reports koreicus or japonicus
  head(df_rep)
  df_rep <- df_rep[!is.na(df_rep$movelab_annotation_euro$class_label) &
                     df_rep$movelab_annotation_euro$class_label == species_name,
                   c("lon", "lat")]
  
  return(df_rep)
  
}

df_kor <- filter_repots(1, "aedes-koreicus")
df_jap <- filter_repots(1, "aedes-japonicus")

# Run through all the files
for (i in c(2:length(list_files))) {
  df_aux1 <- filter_repots(i, "aedes-koreicus")
  df_aux2 <- filter_repots(i, "aedes-japonicus")
  
  df_kor <- rbind(df_kor, df_aux1)
  df_jap <- rbind(df_jap, df_aux2)
}

# Save files
saveRDS(df_jap, "data/Heavy_files/japonicus_reports_MA.Rds")
saveRDS(df_kor, "data/Heavy_files/koreicus_reports_MA.Rds")

# Test
ggplot(df_jap)+ geom_point(aes(lon, lat, color = "black"))
ggplot(df_kor)+ geom_point(aes(lon, lat, color = "black"))

# Data from traps EU process by Catu generated in 07_Joining_datasets.R -----
path <- "/home/marta/aedes_korjap_suitability/OUTPUT/aedes_rain.rds"
traps_PA <- readRDS(path)

# Filter ones per species
df_kor_trap <- traps_PA[traps_PA$ae_kor == 1,c("longitude", "latitude")]
df_jap_trap <- traps_PA[traps_PA$ae_jap == 1,c("longitude", "latitude")]

# Match names columns
colnames(df_kor_trap) <- c("lon", "lat")
colnames(df_jap_trap) <- c("lon", "lat")

# Test
ggplot(df_kor_trap) + geom_point(aes(lon, lat, color = "black"))
ggplot(df_jap_trap) + geom_point(aes(lon, lat, color = "black"))

# Join by row the data sets
df_kor <- rbind(df_kor,df_kor_trap)
df_jap <- rbind(df_jap,df_jap_trap )
df_kor$PA <- 1 
df_jap$PA <- 1 

# Filter lon lat points outside Europe
df_kor <- df_kor[df_kor$lon < 70,]
df_jap <- df_jap[df_jap$lon < 70,]

# Load suitability maps ----------------------------------------
df_suit_days <- readRDS("data/df_suit_days.Rds")
head(df_suit_days)
df_suit_days <- df_suit_days %>%  group_by(x,y) %>% 
  summarise(sum_kor =sum(sum_kor),
            sum_jap =sum(sum_jap),
            sum_kor_jt =sum(sum_kor_jt),
            sum_jap_jt =sum(sum_jap_jt),
            mean_kor =mean(mean_kor),
            mean_jap =mean(mean_jap),
            mean_kor_jt =mean(mean_kor_jt),
            mean_jap_jt =mean(mean_jap_jt)
  )

# Plot suitability maps + PA
library(ggpubr)
suit_PA_maps <- ggarrange(ggplot(df_suit_days) +
  geom_tile(aes(x,y, color = sum_kor)) + 
  scale_color_distiller(palette = "Spectral", name = "Suitable days") +
  geom_point(data = df_kor, aes(lon, lat),
             color = "black", size = 0.1, alpha = 0.3) +
  theme_bw() + ggtitle("Aedes koreicus") + theme(legend.position = "bottom"),
ggplot(df_suit_days) +
  geom_tile(aes(x,y, color = sum_jap)) + 
  scale_color_distiller(palette = "Spectral", name = "Suitable days") +
  geom_point(data = df_jap, aes(lon, lat),
             color = "black", size = 0.1, alpha = 0.3) +
  theme_bw() + ggtitle("Aedes japonicus") + theme(legend.position = "bottom"),
ggplot(df_suit_days) +
  geom_tile(aes(x,y, color = mean_kor)) + 
  scale_color_distiller(palette = "Spectral", name = "Mean RM") +
  geom_point(data = df_kor, aes(lon, lat),
             color = "black", size = 0.1, alpha = 0.3) +
  theme_bw() + ggtitle("Aedes koreicus") + theme(legend.position = "bottom"),
ggplot(df_suit_days) +
  geom_tile(aes(x,y, color = mean_jap)) + 
  scale_color_distiller(palette = "Spectral", name = "Mean RM") +
  geom_point(data = df_jap, aes(lon, lat),
             color = "black", size = 0.1, alpha = 0.3) +
  theme_bw() + ggtitle("Aedes japonicus") + theme(legend.position = "bottom"))

ggsave("data/Heavy_files/suit_days_jap_kor_PA.png", plot = suit_PA_maps, width = 12,
       height = 10, dpi = 300)
ggsave("data/Heavy_files/suit_days_jap_kor_PA.pdf", plot = suit_PA_maps, width = 12,
       height = 10, dpi = 300)
