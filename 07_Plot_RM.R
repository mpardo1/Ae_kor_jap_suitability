## Code to plot the R_M curves for Aedes koreicus and Aedes japonicus
rm(list=ls())
library(ggplot2)
library("ggpubr")
library(data.table)
library("ggsci")
source("~/aedes_korjap_suitability/funcR0.R")

# R_M ---------------------------------------------------------------------
vec <- seq(5,40,0.001)
koreicus <- sapply(vec,R0_func_kor_only_temp)
japonicus <- sapply(vec,R0_func_jap_only_temp) 

# Create a data frame with all the species
df_out_kor_jap <- data.frame(vec,
                     koreicus = koreicus,
                     japonicus = japonicus)
df_out_kor_jap <- reshape2::melt( df_out_kor_jap, id.vars = "vec")

# Check thermal range for both species ------------------------------------
# Koreicus
esp = "koreicus"
min_temp_kor <- min(df_out_kor_jap[which(df_out_kor_jap$variable == esp &
                   df_out_kor_jap$value >1), "vec"], na.rm=TRUE)
max_temp_kor <- max(df_out_kor_jap[which(df_out_kor_jap$variable == esp &
                   df_out_kor_jap$value >1), "vec"])
max_r <- max(df_out_kor_jap[which(df_out_kor_jap$variable == esp &
                            df_out_kor_jap$value >1), "value"])
opt_temp_kor <- df_out_kor_jap[which(df_out_kor_jap$value == max_r), "vec"]

# Japonicus
esp = "japonicus"
min_temp_jap <- min(df_out_kor_jap[which(df_out_kor_jap$variable == esp &
                           df_out_kor_jap$value >1), "vec"])
max_temp_jap <- max(df_out_kor_jap[which(df_out_kor_jap$variable == esp &
                           df_out_kor_jap$value >1), "vec"])
max_r <- max(df_out_kor_jap[which(df_out_kor_jap$variable == esp &
                                    df_out_kor_jap$value >1), "value"])
opt_temp_jap <- df_out_kor_jap[which(df_out_kor_jap$value == max_r), "vec"]

# Print results
print(paste0("Ae. koreicus. Thermal range: [", min_temp_kor, ",",
             max_temp_kor, "]  ",
             "Optimal temperature: ", opt_temp_kor))
print(paste0("Ae. japonicus. Thermal range: [", min_temp_jap, ",",
             max_temp_jap, "]  ",
             "Optimal temperature: ", opt_temp_jap))

# Plot two of the species
library(RColorBrewer)
name_pal = "Set1"
display.brewer.pal(2, name_pal)
pal <- brewer.pal(2, name_pal)
letsize = 16
legtext  =16
col_jap <- "#31B57B"
col_kor <- "#33628D"
size_line <- 1.3
library("latex2exp")
plot_temp <- ggplot(df_out_kor_jap) + 
  geom_line(aes(vec,value, color=variable), size = size_line) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  ylab(TeX("$R_M$")) + 
  scale_color_manual(name = "", values =c("japonicus" = col_jap,
                                          "koreicus" = col_kor),
                     labels = c(expression(italic("Ae. koreicus")),
                                expression(italic("Ae. japonicus")))) +
  # scale_color_bmj(name = "",
  #                       labels = c(expression(italic("Ae. koreicus")),
  #                                  expression(italic("Ae. japonicus")))) +
  xlab("Temperature (Cº)") +
  scale_x_continuous(breaks = seq(5,41,4)) +
  theme_bw() + theme(legend.position = c(0.15,0.8),
                     text = element_text(size = letsize),
                     legend.text.align = 0,
                     legend.title = element_text(face = "italic"),
                     legend.text = element_text(size = legtext))

plot_temp

# Plot two of the species rainfall vs RM ----------------------------------
vec <- seq(0,16,0.001)
koreicus <- sapply(vec,function(x){R0_func_kor(opt_temp_kor,x,1)})
japonicus <- sapply(vec,function(x){R0_func_jap(opt_temp_jap,x,1)}) 

# Create a data frame with all the species
df_out_kor_jap_rain <- data.frame(vec,
                             koreicus = koreicus,
                             japonicus = japonicus)
df_out_kor_jap_rain <- reshape2::melt( df_out_kor_jap_rain, id.vars = "vec")

# Plot
plot_rain <- ggplot(df_out_kor_jap_rain) + 
  geom_line(aes(vec,value, color=variable), size = size_line) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  ylab(TeX("$R_M$")) + 
  scale_color_manual(name = "", values =c(col_kor,col_jap),
                     labels = c(expression(italic("Ae. koreicus")),
                                expression(italic("Ae. japonicus")))) +
  # scale_color_bmj(name = "", 
  #                    labels = c(expression(italic("Ae. koreicus")),
  #                               expression(italic("Ae. japonicus")))) +
  xlab("Rain (mm)") +
  theme_bw() + theme(legend.position = "none",
                     text = element_text(size = letsize),
                     legend.text.align = 0,
                     legend.title = element_text(face = "italic"),
                     legend.text = element_text(size = legtext))

plot_rain

# Check rainfall ranges
esp = "japonicus"
min_rain_jap <- min(df_out_kor_jap_rain[which(df_out_kor_jap_rain$variable == esp &
                                                df_out_kor_jap_rain$value >1),]$vec, na.rm =TRUE)
max_rain_jap <- max(df_out_kor_jap_rain[which(df_out_kor_jap_rain$variable == esp &
                                                df_out_kor_jap_rain$value >1),]$vec, na.rm =TRUE)

esp = "koreicus"
min_rain_kor <- min(df_out_kor_jap_rain[which(df_out_kor_jap_rain$variable == esp &
                                                df_out_kor_jap_rain$value >1),]$vec, na.rm =TRUE)
max_rain_kor <- max(df_out_kor_jap_rain[which(df_out_kor_jap_rain$variable == esp &
                                           df_out_kor_jap_rain$value >1),]$vec, na.rm =TRUE)

# Print results
print(paste0("Ae. koreicus. Rainfall range: [", min_rain_kor, ",",
             max_rain_kor, "]"))
print(paste0("Ae. japonicus. Rainfall range: [", min_rain_jap, ",",
             max_rain_jap, "]  "))

# Plot two of the species landcover vs RM ----------------------------------
vec <- seq(0,1,0.001)
koreicus <- sapply(vec,function(x){R0_func_kor(opt_temp_kor,5,x)})
japonicus <- sapply(vec,function(x){R0_func_jap(opt_temp_jap,5,x)}) 

# Create a data frame with all the species
df_out_kor_jap_lc <- data.frame(vec,
                                  koreicus = koreicus,
                                  japonicus = japonicus)
df_out_kor_jap_lc <- reshape2::melt( df_out_kor_jap_lc, id.vars = "vec")

# Plot
plot_lc <- ggplot(df_out_kor_jap_lc) + 
  geom_line(aes(vec,value, color=variable), size = size_line) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  ylab(TeX("$R_M$")) + 
  scale_color_manual(name = "", values =c(col_kor,col_jap),
                     labels = c(expression(italic("Ae. koreicus")),
                                expression(italic("Ae. japonicus")))) +
  # scale_color_bmj(name = "", 
  #                 labels = c(expression(italic("Ae. koreicus")),
  #                            expression(italic("Ae. japonicus")))) +
  xlab("Landcover suitability") +
  theme_bw() + theme(legend.position = "none",
                     text = element_text(size = letsize),
                     legend.text.align = 0,
                     legend.title = element_text(face = "italic"),
                     legend.text = element_text(size = legtext))

plot_lc

# Arrange both plots
ggarrange(plot_temp, plot_rain,plot_lc,nrow = 1, common.legend = TRUE,
          labels = c("a","b","c"))

# Save plots
ggsave("~/Documentos/PHD/2025/Aedes_land/Plots/RM_jap_kor.pdf",
       width =14.5, height = 5)

ggsave("~/Documentos/PHD/2025/Aedes_land/Plots/RM_jap_kor.png",
       width = 14.5, height =5, dpi = 300)
# 
# # Plot four of the species ---------------------------------------------
# albopictus <- sapply(vec,R0_func_alb,rain = 8, hum= 500)
# aegypti <- sapply(vec,R0_func_aeg,rain =8, hum= 500) 
# # All four species
# df_out <- data.frame(vec,
#                      koreicus = koreicus,
#                      japonicus = japonicus,
#                      albopictus = albopictus,
#                      aegypti = aegypti)
# 
# df_out <- reshape2::melt( df_out, id.vars = "vec")
# library(RColorBrewer)
# name_pal = "Set1"
# display.brewer.pal(4, name_pal)
# pal <- brewer.pal(4, name_pal)
# letsize = 16
# library("latex2exp")
# plot_temp <- ggplot(df_out) + 
#   geom_line(aes(vec,value, color=variable), size = 1) +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
#   ylab(TeX("$R_M$")) + 
#   scale_color_manual(name = "", values =pal,
#                      labels = c(expression(italic("Ae. koreicus")),
#                                 expression(italic("Ae. japonicus")),
#                                 expression(italic("Ae. albopictus")),
#                                 expression(italic("Ae. aegypti")))) +
#   # ,
#   # expression(italic("Ae. japonicus")))) +
#   xlab("Temperature (Cº)") +
#   scale_x_continuous(breaks = seq(5,41,4)) +
#   theme_bw() + theme(legend.position = c(0.15,0.8),
#                      text = element_text(size = letsize),
#                      legend.text.align = 0)
# 
# plot_temp

# Heat map temperature and landcover --------------------------------------
library(data.table)
# Create grid 
temp <- seq(0, 40, length.out = 500)
lc <- seq(0, 1, length.out = 500)
grid_df <- setDT(expand.grid(temp = temp, lc = lc))
grid_df$rain <- 8
grid_df[,R0_jap := mapply(R0_func_jap, temp, rain,lc)]
grid_df[,R0_kor := mapply(R0_func_kor, temp, rain,lc)]
lc_kor <- ggplot(grid_df) +
  geom_point(aes(temp, lc, color = R0_kor)) +
  scale_color_gradient2(
    low = "blue",    # Negative values
    mid = "white",   # Zero
    high = "red",    # Positive values
    midpoint = 1,
    name = TeX("$R_M$")
  ) 

lc_jap <- ggplot(grid_df) +
  geom_point(aes(temp, lc, color = R0_jap)) +
  scale_color_gradient2(
    low = "blue",    # Negative values
    mid = "white",   # Zero
    high = "red",    # Positive values
    midpoint = 1,
    name = TeX("$R_M$")
  ) 

# Heat map temp versus rain ---------------------------------------
temp <- seq(0, 40, length.out = 200)
rain <- seq(0, 14, length.out = 200)
grid_df <- setDT(expand.grid(temp = temp, rain = rain))
grid_df$lc <- 1
grid_df[,R0_jap := mapply(R0_func_jap, temp, rain,lc)]
grid_df[,R0_kor := mapply(R0_func_kor, temp, rain,lc)]
delt = 1.e-2
rain_kor <- ggplot(grid_df) +
  geom_point(aes(temp, rain, color = R0_kor)) +
  scale_color_gradient2(
    low = "blue",    # Negative values
    mid = "white",   # Zero
    high = "red",    # Positive values
    midpoint = 1,
    name = TeX("$R_M$")
  ) 

rain_jap <- ggplot(grid_df) +
  geom_point(aes(temp, rain, color = R0_jap)) +
  scale_color_gradient2(
    low = "blue",    # Negative values
    mid = "white",   # Zero
    high = "red",    # Positive values
    midpoint = 1,
    name = TeX("$R_M$")
  ) 

library(ggpubr)
ggarrange(lc_kor + ggtitle(expression(italic("Ae. koreicus")))+
            ylab("Land Cover") +
            xlab("") +
            theme_bw() +
            theme(text = element_text(size = letsize),
                  legend.text.align = 0,
                  legend.title = element_text(face = "italic"),
                  legend.text = element_text(size = legtext)),
          lc_jap + ggtitle(expression(italic("Ae. japonicus")))+
            ylab("Land Cover") +
            xlab("") +
            theme_bw() +
            theme(text = element_text(size = letsize),
                  legend.text.align = 0,
                  legend.title = element_text(face = "italic"),
                  legend.text = element_text(size = legtext)),
          rain_kor+
            ylab("Rain") +
            xlab("Temperature") +
            theme_bw() +
            theme(text = element_text(size = letsize),
                  legend.text.align = 0,
                  legend.title = element_text(face = "italic"),
                  legend.text = element_text(size = legtext)), 
          rain_jap+
            ylab("Rain") +
            xlab("Temperature") +
            theme_bw() +
            theme(text = element_text(size = letsize),
                  legend.text.align = 0,
                  legend.title = element_text(face = "italic"),
                  legend.text = element_text(size = legtext)))



# Save plots
ggsave("~/Documentos/PHD/2025/Aedes_land/Plots/RM_phase_space_jap_kor.pdf",
       width =7, height = 5)

ggsave("~/Documentos/PHD/2025/Aedes_land/Plots/RM_phase_space_jap_kor.png",
       width = 7, height =5, dpi = 300)
