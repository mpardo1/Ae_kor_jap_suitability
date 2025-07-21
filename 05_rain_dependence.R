rm(list = ls())
library(eurostat)
library(terra)
library(tidyverse)
library(data.table)
library(sf)
library(giscoR)
library(eurostat)

# Computers path
pth = "U:"
pth = "~"

# Load functions
source(paste0(pth,"/aedes_korjap_suitability/funcR0.R"))

# Load observations data
Path <- "data/dataset_Pardo-Araujo_et_al_2025_vf.csv"
trap_df <- read.csv(Path)
# trap_df[is.na(trap_df$SE_mean) & (trap_df$ae_kor == FALSE & trap_df$ae_jap == FALSE ),]

# Threshold mean_SE 
as.numeric(quantile(trap_df$SE_mean, na.rm=TRUE)[4])
tresh_mean_SE <- as.numeric(quantile(trap_df$SE_mean, na.rm=TRUE)[4])

# Load mean rainfall
rast_rain <- rast(readRDS("/home/marta/Documentos/PHD/2025/Aedes_land/data/climate/rast_yearly_mean_rainfall_May_Sept_2012-2023_EU.Rds"))
rast_rain <- rast_rain*1000
plot(rast_rain)

# European map NUTS3
EU <- get_eurostat_geospatial(nuts_level=0, year=2016)
plot(EU[,"NUTS_ID"])

# Separate species and convert to cluster
trap_kor <- trap_df[!is.na(trap_df$ae_kor) &
                      (trap_df$SE_mean > 0.9 | is.na(trap_df$SE_mean )),c(2,3,6)]
trap_kor_vec <- vect(trap_kor, geom = c("lon", "lat") ,  crs = crs(rast_rain))
trap_jap <- trap_df[!is.na(trap_df$ae_jap) &
                      (trap_df$SE_mean > 0.9 | is.na(trap_df$SE_mean )),c(2,3,5)]
trap_jap_vec <- vect(trap_jap, geom = c("lon", "lat") , crs = crs(rast_rain))

# Plot traps
kor_traps <- ggplot() +
  geom_sf(data = EU, fill = NA, color = "black") +
  geom_point(data=trap_kor, aes(lon,lat, color = ae_kor), size = 0.3) + 
  scale_color_viridis_d(name = "",
                        labels = c("0", "1")) +
  xlim(c(-10,35)) +
  ylim(c(35,70)) +
  theme_void() + ggtitle("Ae. koreicus") +
  theme(plot.title = element_text(face = "italic", size = 14),
        legend.title = element_text(face = "italic"),
        legend.text = element_text(size = 14))+  # small points in the plot # small points in the plot
  guides(color = guide_legend(override.aes = list(size = 4)))  


jap_traps <- ggplot() + geom_sf(data = EU, fill = NA, color = "black") +
  geom_point(data=trap_jap, aes(lon,lat, color = ae_jap), size = 0.3) + 
  scale_color_viridis_d(name = "",
                        labels = c("0", "1")) +
  xlim(c(-10,35)) +
  ylim(c(35,70)) +
  theme_void() + ggtitle("Ae. japonicus") +
    theme(plot.title = element_text(face = "italic", size = 14),
          legend.title = element_text(face = "italic"),
          legend.text = element_text(size = 14))+  # small points in the plot
  guides(color = guide_legend(override.aes = list(size = 4)))  

# Arrange both plots
library(ggpubr)

# Extract the legend
leg <- get_legend(jap_traps)

ggarrange(jap_traps + theme(legend.position = "none"),
          kor_traps + theme(legend.position = "none"),
          leg,ncol = 3, nrow = 1,  widths =c(1,1,0.2))

# Save plots
ggsave("~/Documentos/PHD/2025/Aedes_land/traps_0_1_filt.pdf",
        width =9.4, height = 5)

ggsave("~/Documentos/PHD/2025/Aedes_land/traps_0_1_filt.png",
        width = 9.4, height = 5, dpi = 300)

# Join two rasters
rast_rain <- aggregate(rast_rain, fact=2, fun=mean)
extracted <- terra::extract(rast_rain,trap_kor_vec, cells=TRUE, xy = TRUE)
df_kor_rain <- cbind(trap_kor[extracted$ID,], extracted[,-1])
extracted <- terra::extract(rast_rain,trap_jap_vec, cells=TRUE, xy = TRUE)
df_jap_rain <- cbind(trap_jap[extracted$ID,], extracted[,-1])

# Remove points with false in ae_kor less than 100km of the positive
df_aux_1 <- df_kor_rain[df_kor_rain$ae_kor == TRUE,c("lon", "lat")]
df_aux_0 <- df_kor_rain[df_kor_rain$ae_kor == FALSE,c("lon", "lat")]
df_aux_1$ae_kor <- TRUE
df_aux_0$ae_kor <- FALSE
colnames(df_aux_1) <- colnames(df_aux_0) <- c("lon", "lat", "ae_kor")
df_kor_01 <- rbind(df_aux_1,df_aux_0)

# Join with rainfall data
extracted <- terra::extract(rast_rain,vect(df_kor_01), cells=TRUE, xy = TRUE)
df_kor_rain <- cbind(df_kor_01[extracted$ID,], extracted[,-1])
ggplot(df_kor_rain) + geom_point(aes(lon,lat,color = ae_kor), size = 0.3)

# Aggregate by rain
df_kor_rain$mean <- round(df_kor_rain$mean,2)
df_kor_rain_agg <- df_kor_rain %>%  group_by(mean) %>% 
  summarise(ae_kor = max(ae_kor, na.rm=TRUE))
df_jap_rain$mean <- round(df_jap_rain$mean,2)
df_jap_rain_agg <- df_jap_rain %>%  group_by(mean) %>% 
  summarise(ae_jap = max(ae_jap, na.rm=TRUE))

min(df_kor_rain_agg$ae_kor)
ggplot(df_kor_rain_agg) + geom_point(aes(mean, ae_kor))

# Koreicus -----------------------------------------------------------
# Balance 0 and 1
df_kor_rain_0 <- df_kor_rain_agg[df_kor_rain_agg$ae_kor == 0,]
df_kor_rain_1 <- df_kor_rain_agg[df_kor_rain_agg$ae_kor == 1,]

# Check number of rows zeros and ones
nrow(df_kor_rain_0)
nrow(df_kor_rain_1)
comb_df_kor_bal <- rbind(df_kor_rain_1,
                         df_kor_rain_0[sample(c(1:nrow(df_kor_rain_0)),
                                              nrow(df_kor_rain_1)),]/2)

# comb_df_kor_bal <- rbind(df_kor_rain_1,
                         # df_kor_rain_0)
# comb_df_kor_bal <- rbind(df_kor_rain_1,
#                          df_kor_rain_0,
#                          df_kor_rain_1,
#                          df_kor_rain_0)

# Fit rainfall curve unimodal
library(minpack.lm)
for(i in c(1:10)){
  comb_df_kor_bal <- rbind(comb_df_kor_bal,c(runif(1,0,1e-8),0))

}

Fit_rainfall_kor <- nlsLM(ae_kor ~ (((1+e0)*exp(-evar*(mean-eopt)^2))/(exp(-evar*(mean - eopt)^2) + e0)),
                        data = comb_df_kor_bal,
                        start = list(evar = 0.1,
                                     eopt= 5,
                                     e0 = 0.005),
                        lower = c(evar = 0,eopt=0,e0=0),
                        control=list(maxiter = 2000))
summary(Fit_rainfall_kor)

# Data frame with output of the fitting
vec <- seq(0,12,0.1)
pars <- Fit_rainfall_kor$m$getAllPars()
out <- (((1+pars[3])*exp(-pars[1]*(vec-
                                     pars[2])^2))/(exp(-pars[1]*(vec -
                                                                   pars[2])^2) + pars[3]))
df_fit <- data.frame(vec,out)

# Plot
uni_kor <- ggplot() +
  geom_line(data= df_fit, aes(vec,out), size = 1) +
  geom_point(data=comb_df_kor_bal,
             aes(mean, ae_kor), size = 1, color = "#A9DBB8") +
  theme_bw()

uni_kor

# Fit rainfall sigmoid
library(minpack.lm)
Fit_rainfall_kor_sig <- nlsLM(ae_kor ~ 1/(1+exp(-a*(mean-b))),
                              data = comb_df_kor_bal,
                              start = list(a = 0.1,
                                           b= 3),
                              control=list(maxiter = 2000))
summary(Fit_rainfall_kor_sig)

# Data frame with output of the fitting
vec <- seq(0,12,0.1)
pars <- Fit_rainfall_kor_sig$m$getAllPars()
out <- 1/(1+exp(-pars[1]*(vec-pars[2])))
df_fit <- data.frame(vec,out)

# Plot
sig_kor <- ggplot() +
  geom_line(data= df_fit, aes(vec,out), size = 1) +
  geom_point(data=comb_df_kor_bal,
             aes(mean, ae_kor), size = 1, color = "#A9DBB8") +
  theme_bw()
sig_kor

# Arrange
ggarrange(uni_kor, sig_kor)

# Compare AIC value
AIC(Fit_rainfall_kor_sig,Fit_rainfall_kor)

# Japonicus -----------------------------------------------------
# Balance 0 and 1
df_jap_rain_0 <- df_jap_rain_agg[df_jap_rain_agg$ae_jap == 0,]
df_jap_rain_1 <- df_jap_rain_agg[df_jap_rain_agg$ae_jap == 1,]

# Balance 0 and 1
comb_df_jap_bal <- rbind(df_jap_rain_1,
                         df_jap_rain_0)

# Fit rainfall curve unimodal
library(minpack.lm)
Fit_rainfall_jap <- nlsLM(ae_jap ~ (((1+e0)*exp(-evar*(mean-eopt)^2))/(exp(-evar*(mean - eopt)^2) + e0)),
                              data = comb_df_jap_bal,
                              start = list(evar = 0.1,
                                           eopt= 3,
                                           e0 = 0.5),
                              control=list(maxiter = 2000))
summary(Fit_rainfall_jap)

# Run model with parameter estimated
vec <- seq(0,12,0.1)
pars <- Fit_rainfall_jap$m$getAllPars()
out <- (((1+pars[3])*exp(-pars[1]*(vec-pars[2])^2))/(exp(-pars[1]*(vec - pars[2])^2) + pars[3]))
df_fit <- data.frame(vec,out)

# Plot
uni_jap <- ggplot() +
  geom_line(data= df_fit, aes(vec,out), size = 1) +
  geom_point(data=comb_df_jap_bal,
             aes(mean, ae_jap), size = 1, color = "#7CA5B8") +
  theme_bw()
uni_jap

# Sigmoid function
Fit_rainfall_jap_sig <- nlsLM(ae_jap ~ 1/(1+exp(-a*(mean-b))),
                              data = comb_df_jap_bal,
                              start = list(a = 6,b= 3),
                              control=list(maxiter = 2000))
summary(Fit_rainfall_jap_sig)

# Run model with paramteres estimated
vec <- seq(0,12,0.1)
pars <- Fit_rainfall_jap_sig$m$getAllPars()
out <- 1/(1+exp(-pars[1]*(vec-pars[2])))
df_fit <- data.frame(vec,out)

# Plot
sig_jap <- ggplot() +
  geom_line(data= df_fit, aes(vec,out), size = 1) +
  geom_point(data=comb_df_jap_bal,
             aes(mean, ae_jap), size = 1, color = "#7CA5B8") +
  theme_bw()
sig_jap

# Compute AIC value two models
AIC(Fit_rainfall_jap_sig, Fit_rainfall_jap)
AIC(Fit_rainfall_kor_sig,Fit_rainfall_kor)

# Arrange both plots
library(ggpubr)
ggarrange(uni_jap + ggtitle("Aedes japonicus") +ylab("Hatching rate") +
            xlab("") +
            theme(plot.title=element_text(face="italic",size = 14),
                  legend.text = element_text(size = 12),
                  axis.title.x = element_text(size = 12),
                  axis.title.y = element_text(size = 12),
                  axis.text.x = element_text(size = 12),
                  axis.text.y = element_text(size = 12)) , 
          uni_kor + ggtitle("Aedes koreicus")+xlab("") +
            ylab("") +
            theme(plot.title=element_text(face="italic",size = 14),
                  legend.text = element_text(size = 12),
                  axis.title.x = element_text(size = 12),
                  axis.title.y = element_text(size = 12),
                  axis.text.x = element_text(size = 12),
                  axis.text.y = element_text(size = 12)),
          sig_jap + ggtitle("Aedes japonicus") +xlab("Mean daily rain (mm)") +
            ylab("Hatching rate") +
            theme(plot.title=element_text(face="italic",size = 14),
                  legend.text = element_text(size = 12),
                  axis.title.x = element_text(size = 12),
                  axis.title.y = element_text(size = 12),
                  axis.text.x = element_text(size = 12),
                  axis.text.y = element_text(size = 12)) , 
          sig_kor + ggtitle("Aedes koreicus")+ xlab("Mean daily rain (mm)") +
            ylab("") +
            theme(plot.title=element_text(face="italic",size = 14),
          legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12)), ncol = 2, nrow= 2)

# Save plots
ggsave("~/Documentos/PHD/2025/Aedes_land/fits_jap_kor_hatching_v2.pdf",
       width =9.4, height = 4)

ggsave("~/Documentos/PHD/2025/Aedes_land/fits_jap_kor_hatching_v2.png",
       width = 9.4, height = 4, dpi = 300)

