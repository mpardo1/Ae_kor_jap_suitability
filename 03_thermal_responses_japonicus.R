## Compute the thermal responses for Aedes Japonicus from literature data
# compare also different functions and compute its AIC value
rm(list= ls())
library(thermPerf)
library(ggplot2)
library(tidyverse)
library(nls2)
## Data taken from https://parasitesandvectors.biomedcentral.com/articles/10.1186/s13071-018-2659-1
## https://www.researchgate.net/publication/235430511_The_ecology_of_the_exotic_mosquito_Ochlerotatus_Finlaya_japonicus_japonicus_Theobald_1901_Diptera_Culicidae_and_an_examination_of_its_role_in_the_West_Nile_virus_cycle_in_New_Jersey
Path <- "~/aedes_korjap_suitability/data/japonicus/adult_larva_lifespan.csv"
Japonicus <- read.csv(Path)

head(Japonicus)
Japonicus$lifespan <- Japonicus$Age_adult_death_mean_female  
plot_deltaA <- ggplot(Japonicus) + 
  geom_point(aes(Temp,lifespan)) + theme_bw()
plot_deltaA

## Linear Fit
Fitting_deltaA <- nls(lifespan ~ cont*Temp + cont1,
                      data = Japonicus,
                      start = list(cont = 0.001, cont1 = 0.0))
summary(Fitting_deltaA)

mod <- function(te){
  c <- as.numeric(Fitting_deltaA$m$getPars()[1])
  c1 <- as.numeric(Fitting_deltaA$m$getPars()[2])
  c*te+c1
}

# Quadratic Fit
Fitting_deltaA_quad <- nls(lifespan ~ cont*Temp^2 + cont1*Temp +cont2,
                           data = Japonicus,
                           start = list(cont = 0.001, cont1 = 0.0, cont2 = 0.0))

summary(Fitting_deltaA_quad)

# Compute the AIC value
AIC(Fitting_deltaA,Fitting_deltaA_quad)

# Create data frame to plot
vec <- seq(0,45,0.001)
df_out_deltaA <- data.frame(temp_ae = vec,
                            deltaA_jap <- sapply(vec, mod))
colnames(df_out_deltaA) <- c("temp_ae","deltaA_jap")
df_out_deltaA[which(df_out_deltaA$deltaA_jap < 0 ),2] <- 0

# Optimal value
df_out_deltaA[df_out_deltaA$deltaA_jap == max(df_out_deltaA$deltaA_jap, na.rm = TRUE),]

# Create plot
plotdeltaA <- ggplot(df_out_deltaA) +
  geom_line(aes(temp_ae,deltaA_jap), size = 0.7) +
  geom_point(data = Japonicus,
             aes(x = Temp,y = lifespan),
             size = 1.1, color = "red") +
  xlim(c(0,45))  +
  ylab("Adult Life Span") + xlab("Temperature (Cº)") +
  theme_bw()
plotdeltaA

####----------- +/- SD-------------####
# Mean - sd
mod_min <- function(te){
  c <- as.numeric(Fitting_deltaA$m$getPars()[1]) - 
    summary(Fitting_deltaA)$coefficients[1,2]
  c1 <- as.numeric(Fitting_deltaA$m$getPars()[2])-
    summary(Fitting_deltaA)$coefficients[2,2]
  c*te+c1
}

vec <- seq(0,45,0.001)
df_out_deltaA_min <- data.frame(temp_ae = vec,
                                deltaA_jap <- sapply(vec, mod_min))
colnames(df_out_deltaA_min) <- c("temp_ae","deltaA_jap")
# df_out_deltaA_min[which(df_out_deltaA_min$deltaA_jap < 0),2] <- 0
df_out_deltaA_min$group <- "min"

ggplot(df_out_deltaA_min) + 
  geom_line(aes(temp_ae,deltaA_jap))

### Mean + sd
mod_max <- function(te){
  c <- as.numeric(Fitting_deltaA$m$getPars()[1]) +
    summary(Fitting_deltaA)$coefficients[1,2]
  c1 <- as.numeric(Fitting_deltaA$m$getPars()[2]) + 
    summary(Fitting_deltaA)$coefficients[2,2]
  c*te+c1
}

vec <- seq(0,45,0.001)
df_out_deltaA_max <- data.frame(temp_ae = vec,
                                deltaA_jap <- sapply(vec, mod_max))
colnames(df_out_deltaA_max) <- c("temp_ae","deltaA_jap")
# df_out_deltaA_max[which(df_out_deltaA_max$deltaA_jap < 0),2] <- 0
df_out_deltaA_max$group <- "max"
df_out_deltaA$group <- "mean"

# Plot all three curves together
df_out_deltaA <- rbind(df_out_deltaA_max,
                       df_out_deltaA_min,
                       df_out_deltaA)

plotdeltaA <- ggplot(df_out_deltaA) +
  geom_line(aes(temp_ae,deltaA_jap,
                color = group,
                group = group, 
                alpha = group), size = 0.7) +
  geom_point(data = Japonicus,aes(x = Temp,y = lifespan),
             size = 1.1, color = "black") +
  scale_color_manual(values=c("red", "blue", "red")) + 
  scale_alpha_manual(values = c(0.5,1,0.5)) +
  xlim(c(5,35)) + ylim(c(0,80)) +
  guides( color =FALSE, alpha = FALSE) +
  ylab("Adult life span") + xlab("Temperature (Cº)") +
  theme_bw() 

plotdeltaA 

# Colors
jame_col <- "#A73030"
german_col <- "#EFC000"
line_col <- "#0072B5"
grey_col <- "#E5E5E5"
  
# add grey ribbon --------------------------------------------
df_out_deltaA_w <- reshape(df_out_deltaA, idvar ="temp_ae" ,
                           timevar = "group", direction = "wide")

plotdeltaA_w <- ggplot(df_out_deltaA_w, aes(x=temp_ae,y=deltaA_jap.min)) +
  geom_line(aes(y=deltaA_jap.min), color = grey_col, size = 0.7, alpha=0.5) +
  geom_line(aes(y=deltaA_jap.max), color = grey_col, size = 0.7, alpha=0.5) +
  geom_line(aes(y=deltaA_jap.mean), color = line_col, size = 0.7) +
  geom_ribbon(data = df_out_deltaA_w,aes(ymin=deltaA_jap.min,
                                         ymax=deltaA_jap.max), fill=grey_col, alpha=0.5) +
  geom_point(data = Japonicus,aes(x = Temp,y = lifespan),
             size = 1.1, color = german_col) +
  xlim(c(5,33)) +  ylim(c(-20,80)) + 
  guides( color =FALSE, alpha = FALSE) +
  ylab("Adult life span") + xlab("Temperature (Cº)") +
  theme_bw() 
plotdeltaA_w  

###----------------------------------------------
# Path <- "~/Kor_jap/data/japonicus/japonicus_temp_developmenttime.csv"
# developL <- read.csv(Path)
# head(developL)
# 
# n_points <- 8
# r1 <- rnorm(n_points,as.numeric(gsub(",", ".",developL$First_instar_mean[1])),
#                    as.numeric(gsub(",", ".",developL$First_instar_sd[1])) )
# r2 <- rnorm(n_points,as.numeric(gsub(",", ".",developL$First_instar_mean[2])),
#             as.numeric(gsub(",", ".",developL$First_instar_sd[2])) )
# r3 <- rnorm(n_points,as.numeric(gsub(",", ".",developL$First_instar_mean[3])),
#             as.numeric(gsub(",", ".",developL$First_instar_sd[3])) )
# r4 <- rnorm(n_points,as.numeric(gsub(",", ".",developL$First_instar_mean[4])),
#             as.numeric(gsub(",", ".",developL$First_instar_sd[4])) )
# r5 <- rnorm(n_points,as.numeric(gsub(",", ".",developL$First_instar_mean[5])),
#             as.numeric(gsub(",", ".",developL$First_instar_sd[5])) )
# 
# developL <- data.frame(Temp = sort(rep(developL[,1],n_points)),
#                        First_instar_mean = c(r1,r2,r3,r4,r5))
#
# saveRDS(developL, "~/Kor_jap/data/japonicus/developL.Rds")
# developL$First_instar_mean <- 1/developL$First_instar_mean

###### Random sample from a gaussian distribution, since if we take the mean
# there are only 4 points in the data, and the fit it is overfitting
# We do it once since each iteration the curves are different. So we save the random sample.
# tiene sentido como lo estoy congiendo por que si ves el texto asociado pone que
# first instar es desde huevo a first instar, es decir cuanto tarda en hatch.
developL <- readRDS( "~/aedes_korjap_suitability/data/japonicus/developL.Rds")

plot_dE <- ggplot(developL) + 
  geom_point(aes(Temp,First_instar_mean)) + theme_bw()
plot_dE

# Compute the fit for the Briere function:
Fitting_dE <- nls(First_instar_mean ~ cont*Temp*(Temp-cont1)*(cont2-Temp)^(1/2) ,
                  data = developL,
                  start = list(cont = 0.00035, cont1 = 9.5, cont2 = 36))

summary(Fitting_dE)

# Compute the fit for the Quadratic function:
Fitting_dE_quad <- nls(First_instar_mean ~ cont*(Temp-cont1)*(Temp - cont2) ,
                       data = developL,
                       start = list(cont = 0.00035, cont1 = 9.5, cont2 = 36))

summary(Fitting_dE_quad)

# Compute the fit for the Linear function:
Fitting_dE_lin <- nls(First_instar_mean ~ cont*Temp + cont1 ,
                      data = developL,
                      start = list(cont = 0.00035, cont1 = 0))

summary(Fitting_dE_lin)

# Compute the AIC value for the three models:
AIC(Fitting_dE_lin,Fitting_dE,Fitting_dE_quad)

# Fucntion with fitted thermal responses
mod <- function(te){
  c <- as.numeric(Fitting_dE$m$getPars()[1])
  c1 <- as.numeric(Fitting_dE$m$getPars()[2])
  c2 <- as.numeric(Fitting_dE$m$getPars()[3])
  c*te*(te-c1)*(c2-te)^(1/2)
}

# Evaluate points uniform to plot
vec <- seq(0,45,0.001)
df_out_dE  <- data.frame(temp_ae = vec,
                         dE_jap <- sapply(vec, mod))
colnames(df_out_dE) <- c("temp_ae","dE_jap")
df_out_dE[which(df_out_dE$dE_jap < 0),2] <- 0
df_out_dE$group <- "mean"
plotdE <- ggplot(df_out_dE) +
  geom_line(aes(temp_ae,dE_jap), size = 0.7) +
  geom_point(data = developL,aes(Temp,First_instar_mean), size = 1.1, color = "red") +
  xlim(c(0,40)) + ylim(c(0,0.7)) +  
  ylab("Develop rate from Egg to Larva") + xlab("Temperature (Cº)") +
  theme_bw()
plotdE

# Compute optimal value
df_out_dE[df_out_dE$dE_jap == max(df_out_dE$dE_jap, na.rm =TRUE),]

####----------- +/- SD-------------####
# Mean - sd
mod_min <- function(te){
  c <- as.numeric(Fitting_dE$m$getPars()[1]) - summary(Fitting_dE)$coefficients[1,2]
  c1 <- as.numeric(Fitting_dE$m$getPars()[2])- summary(Fitting_dE)$coefficients[2,2]
  c2 <- as.numeric(Fitting_dE$m$getPars()[3])- summary(Fitting_dE)$coefficients[3,2]
  c*te*(te-c1)*(c2-te)^(1/2)
}

vec <- seq(0,45,0.001)
df_out_dE_min  <- data.frame(temp_ae = vec,
                             dE_jap <- sapply(vec, mod_min))
colnames(df_out_dE_min) <- c("temp_ae","dE_jap")
df_out_dE_min[which(df_out_dE_min$dE_jap < 0),2] <- 0
df_out_dE_min$group <- "min"

### Mean + sd
mod_max <- function(te){
  c <- as.numeric(Fitting_dE$m$getPars()[1]) + summary(Fitting_dE)$coefficients[1,2]
  c1 <- as.numeric(Fitting_dE$m$getPars()[2]) + summary(Fitting_dE)$coefficients[2,2]
  c2 <- as.numeric(Fitting_dE$m$getPars()[3]) + summary(Fitting_dE)$coefficients[3,2]
  c*te*(te-c1)*(c2-te)^(1/2)
}

vec <- seq(0,45,0.001)
df_out_dE_max  <- data.frame(temp_ae = vec,
                             dE_jap <- sapply(vec, mod_max))
colnames(df_out_dE_max) <- c("temp_ae","dE_jap")
df_out_dE_max[which(df_out_dE_max$dE_jap < 0),2] <- 0
df_out_dE_max$group <- "max"

# Plot all three curves together
df_out_dE <- rbind(df_out_dE_max,df_out_dE_min, df_out_dE)
plotdE <- ggplot(df_out_dE) +
  geom_line(aes(temp_ae,dE_jap,
                color = group,
                group = group, 
                alpha = group), size = 0.7) +
  geom_point(data = developL,aes(Temp,First_instar_mean),
             size = 1.1, color = "black") +
  scale_color_manual(values=c("red", "blue", "red")) + 
  scale_alpha_manual(values = c(0.5,1,0.5)) +
  xlim(c(5,36)) + ylim(c(0,0.7)) + 
  guides( color =FALSE, alpha = FALSE) +
  ylab("Develop rate from Egg to Larva") + xlab("Temperature (Cº)") +
  theme_bw()
plotdE

# ####-----------CI-------------####
# new.data <- data.frame(Temp=seq(5, 35.9, by = 0.1))
# interval <- as_tibble(predFit(Fitting_dE, newdata = new.data,
#                               interval = "confidence", level= 0.9)) %>%
#   mutate(Temp = new.data$Temp)
# 
# p1 <-  ggplot(data = developL,
#               aes(x = Temp,y = First_instar_mean)) +
#   geom_point(size = 0.7)
# 
# plotdeltaA <- p1 +
#   geom_line(data = df_out_dE, aes(temp_ae,dE_jap), size = 0.7) +
#   geom_ribbon(data=interval, aes(x=Temp, ymin=lwr, ymax=upr),
#               alpha=0.5, inherit.aes=F, fill="blue") +
#   xlim(c(0,40))  + ylim(c(0,0.5)) +
#   ylab("Adult mortality rate") + xlab("Temperature (Cº)") +
#   theme_bw()
# 
# plotdeltaA

# add grey ribbon --------------------------------------------
df_out_dE_w <- reshape(df_out_dE, idvar ="temp_ae" ,
                       timevar = "group", direction = "wide")

plotdEjap_w <- ggplot(df_out_dE_w, aes(x=temp_ae,y=dE_jap.min)) +
  geom_line(aes(y=dE_jap.min), color = grey_col, size = 0.7, alpha=0.5) +
  geom_line(aes(y=dE_jap.max), color = grey_col, size = 0.7, alpha=0.5) +
  geom_line(aes(y=dE_jap.mean), color = line_col, size = 0.7) +
  geom_ribbon(data = df_out_dE_w,aes(ymin=dE_jap.min,
                                     ymax=dE_jap.max), fill=grey_col, alpha=0.5) +
  geom_point(data = developL,aes(Temp,First_instar_mean),
             size = 1.1, color = jame_col) +
  xlim(c(5,36)) + ylim(c(0,0.7)) + 
  guides( color =FALSE, alpha = FALSE) +
  ylab("Develop rate from Egg to Larva") + xlab("Temperature (Cº)") +
  theme_bw()
plotdEjap_w  

#--------------------------------------------------------
# Paper Germany:
Path <- "~/aedes_korjap_suitability/data/japonicus/adult_larva_lifespan.csv"
Japonicus <- read.csv(Path)
head(Japonicus)
## Aunque ponga male es female
# Coger esta variable tiene sentido por que en el experimento cuentan como dia cero
# cuando la larva tiene como maximo 24h. Es decir este tiempo es de larva a adulto
Japonicus$FemaledL <- 1/(Japonicus$Age_emergence_female_mean) 
Japonicus <- Japonicus[,c("Temp","FemaledL")]
# Thesis Jamesina
# Japonicus <- data.frame(Temp <- numeric(), FemaledL <- numeric())
Japonicus <- rbind(Japonicus, c(10,1/140.8))
Japonicus <- rbind(Japonicus, c(16,1/84))
Japonicus <- rbind(Japonicus, c(22,1/31.3))
Japonicus <- rbind(Japonicus, c(28,1/17))
Japonicus <- rbind(Japonicus, c(34,0))
colnames(Japonicus) <-  c("Temp", "FemaledL")
plot_dL <- ggplot(Japonicus) + 
  geom_point(aes(Temp,FemaledL)) + theme_bw()
plot_dL

# Briere function fit    
Fitting_dL <- nls(FemaledL ~ cont*Temp*(Temp-cont1)*(cont2-Temp)^(1/2) ,
                  data = Japonicus, algorithm = "port",
                  start = list(cont = 0.0035, cont1 = 9.5, cont2 = 36), 
                  lower=c(7e-05,min(developL$Temp)-5,max(developL$Temp)+0.1), upper=c(1,min(developL$Temp)-0.1,max(developL$Temp)+4))

summary(Fitting_dL)

# Linear function fit
Fitting_dL_lin <- nls(FemaledL ~ cont*Temp + cont1,
                      data = Japonicus,
                      start = list(cont = 0.00035, cont1 = 0))
summary(Fitting_dL_lin)

# Compute the AIC value
AIC(Fitting_dL,Fitting_dL_lin)
mod <- function(te){
  c <- as.numeric(Fitting_dL$m$getPars()[1])
  c1 <- as.numeric(Fitting_dL$m$getPars()[2])
  c2 <- as.numeric(Fitting_dL$m$getPars()[3])
  c*te*(te-c1)*(c2-te)^(1/2)
}

vec <- seq(0,45,0.001)
df_out_dL  <- data.frame(temp_ae = vec,
                         dL_jap <- sapply(vec, mod))

colnames(df_out_dL) <- c("temp_ae","dL_jap")
df_out_dL[which(df_out_dL$dL_jap < 0),2] <- 0

plotdL <- ggplot(df_out_dL) +
  geom_line(aes(temp_ae,dL_jap), size = 0.7) +
  geom_point(data = Japonicus,aes(Temp,FemaledL), size = 1.1, color = "red") +
  xlim(c(0,45)) + 
  ylab("Develop rate from Larva to Adult") + xlab("Temperature (Cº)") +
  theme_bw()
plotdL

# Optimal value
df_out_dL[!is.na(df_out_dL$dL_jap) &
            df_out_dL$dL_jap == max(df_out_dL$dL_jap, na.rm = TRUE), ]

####----------- +/- SD-------------####
# Mean - sd
mod_min <- function(te){
  c <- as.numeric(Fitting_dL$m$getPars()[1]) - 
    summary(Fitting_dL)$coefficients[1,2]
  c1 <- as.numeric(Fitting_dL$m$getPars()[2])- 
    summary(Fitting_dL)$coefficients[2,2]
  c2 <- as.numeric(Fitting_dL$m$getPars()[3])- 
    summary(Fitting_dL)$coefficients[3,2]
  c*te*(te-c1)*(c2-te)^(1/2)
}

vec <- seq(0,45,0.001)
df_out_dL_min  <- data.frame(temp_ae = vec,
                             dL_jap <- sapply(vec, mod_min))

colnames(df_out_dL_min) <- c("temp_ae","dL_jap")
df_out_dL_min[which(df_out_dL_min$dL_jap < 0),2] <- 0
df_out_dL_min$group <- "min"

### Mean + sd
mod_max <- function(te){
  c <- as.numeric(Fitting_dL$m$getPars()[1]) + 
    summary(Fitting_dL)$coefficients[1,2]
  c1 <- as.numeric(Fitting_dL$m$getPars()[2]) +
    summary(Fitting_dL)$coefficients[2,2]
  c2 <- as.numeric(Fitting_dL$m$getPars()[3]) + 
    summary(Fitting_dL)$coefficients[3,2]
  c*te*(te-c1)*(c2-te)^(1/2)
}

vec <- seq(0,45,0.001)
df_out_dL_max  <- data.frame(temp_ae = vec,
                             dL_jap <- sapply(vec, 
                                              mod_max))

colnames(df_out_dL_max) <- c("temp_ae","dL_jap")
df_out_dL_max[which(df_out_dL_max$dL_jap < 0),2] <- 0
df_out_dL_max$group <- "max"
df_out_dL$group <- "mean"

# Plot all three curves together
df_out_dL <- rbind(df_out_dL_min,
                   df_out_dL_max,
                   df_out_dL)
plotdL <- ggplot(df_out_dL) +
  geom_line(aes(temp_ae,dL_jap,
                color = group,
                group = group, 
                alpha = group), size = 0.7) +
  geom_point(data = Japonicus,aes(Temp,FemaledL),
             size = 1.1, color = "black") +
  scale_color_manual(values=c("red", "blue", "red")) + 
  scale_alpha_manual(values = c(0.5,1,0.5)) +
  xlim(c(0,45)) + 
  ylab("Develop rate from Larva to Adult") +
  guides(color = FALSE, alpha = FALSE) +
  xlab("Temperature (Cº)") + ylab("Develop rate from Egg to Larva") + xlab("Temperature (Cº)") +
  theme_bw()
plotdL

# add grey ribbon --------------------------------------------
df_out_dL_w <- reshape(df_out_dL, idvar ="temp_ae" ,
                       timevar = "group", direction = "wide")

plotdL_w <- ggplot(df_out_dL_w, aes(x=temp_ae,y=dL_jap.min)) +
  geom_line(aes(y=dL_jap.min), color = grey_col, size = 0.7, alpha=0.5) +
  geom_line(aes(y=dL_jap.max), color = grey_col, size = 0.7, alpha=0.5) +
  geom_line(aes(y=dL_jap.mean), color = line_col, size = 0.7) +
  geom_ribbon(data = df_out_dL_w,aes(ymin=dL_jap.min,
                                     ymax=dL_jap.max), fill=grey_col, alpha=0.5) +
  geom_point(data = Japonicus,aes(Temp,FemaledL),
             size = 1.1, color = german_col) +
  xlim(c(0,45)) + 
  ylab("Develop rate from Larva to Adult") +
  guides(color = FALSE, alpha = FALSE) +
  xlab("Temperature (Cº)") + ylab("Develop rate from Egg to Larva") + xlab("Temperature (Cº)") +
  theme_bw()
plotdL_w 

###----------------------------------------------
## Paper Germany:
# https://parasitesandvectors.biomedcentral.com/articles/10.1186/s13071-018-2659-1
Lmortality <- data.frame(Temp = c(0,5,10,12,14,15,17,19,20,23,25,26,27,28,29,31),
                         mean_mort_perc = c(100,99.5,16,38.5,18,15,19,29.5,11.3,48.5,13.8,6,41.5,12.5,70.5,87.5),
                         sd_mort_perc = c(0,1.1,5.5,14.2,4.8,7.9,9.6,12.4,6.5,27.6,8.4,5.2,31.1,7.7,22.2,6.4))
Lmortality$mean_mort_perc = Lmortality$mean_mort_perc/100
Lmortality[nrow(Lmortality) +1,] <- c(10,0.5,0)
Lmortality[nrow(Lmortality) +1,] <- c(16,1-0.6,0)
Lmortality[nrow(Lmortality) +1,] <- c(22,1-0.27,0)
Lmortality[nrow(Lmortality) +1,] <- c(28,1-0.33,0)
Lmortality[nrow(Lmortality) +1,] <- c(34,1,0)
Lmortality[nrow(Lmortality) +1,] <- c(40,1,0)

head(Lmortality)

plot_deltaL <- ggplot(Lmortality) + 
  geom_point(aes(Temp,mean_mort_perc)) + theme_bw()
plot_deltaL

## Quadratic normal fit
Fitting_deltaL <- nls(mean_mort_perc ~ cont*Temp^2 + cont1*Temp + cont2,
                      data = Lmortality,
                      start = list(cont = 15, cont1 = -20,
                                   cont2 = 30))

summary(Fitting_deltaL)

# Linear function fit
Fitting_deltaL_lin <- nls(mean_mort_perc ~ cont*Temp + cont1,
                          data = Lmortality,
                          start = list(cont = 0, cont1 = 0))

summary(Fitting_deltaL_lin)

# AIC for the two models:
AIC(Fitting_deltaL,Fitting_deltaL_lin)

mod <- function(te){
  c <- as.numeric(Fitting_deltaL$m$getPars()[1])
  c1 <- as.numeric(Fitting_deltaL$m$getPars()[2])
  c2 <- as.numeric(Fitting_deltaL$m$getPars()[3])
  c*te^2+c1*te+c2
}

vec <- seq(0,45,0.001)
df_out_deltaL <- data.frame(temp_ae = vec,
                            deltaL_jap <- sapply(vec, mod))

colnames(df_out_deltaL) <- c("temp_ae","deltaL_jap")
df_out_deltaL[which(df_out_deltaL$deltaL_jap < 0),2] <- 0

plotdeltaL <- ggplot(df_out_deltaL) +
  geom_line(aes(temp_ae,deltaL_jap), size = 0.8) +
  geom_point(data = Lmortality,aes(Temp,mean_mort_perc),
             size = 1.1, color = "red") +
  ylab("Larva mortality rate") + xlab("Temperature (Cº)") +
  theme_bw()
plotdeltaL

df_out_deltaL[df_out_deltaL$deltaL_jap == min(df_out_deltaL$deltaL_jap),]

####----------- +/- SD-------------####
# Mean - sd
mod_min <- function(te){
  c <- as.numeric(Fitting_deltaL$m$getPars()[1]) - 
    summary(Fitting_deltaL)$coefficients[1,2]
  c1 <- as.numeric(Fitting_deltaL$m$getPars()[2])- 
    summary(Fitting_deltaL)$coefficients[2,2]
  c2 <- as.numeric(Fitting_deltaL$m$getPars()[3])- 
    summary(Fitting_deltaL)$coefficients[3,2]
  c*te^2+c1*te+c2
}

vec <- seq(0,45,0.001)
df_out_deltaL_min <- data.frame(temp_ae = vec,
                                deltaL_jap <- sapply(vec,
                                                     mod_min))

colnames(df_out_deltaL_min) <- c("temp_ae","deltaL_jap")
df_out_deltaL_min$group <- "min"

### Mean + sd
mod_max <- function(te){
  c <- as.numeric(Fitting_deltaL$m$getPars()[1]) + 
    summary(Fitting_deltaL)$coefficients[1,2]
  c1 <- as.numeric(Fitting_deltaL$m$getPars()[2])+ 
    summary(Fitting_deltaL)$coefficients[2,2]
  c2 <- as.numeric(Fitting_deltaL$m$getPars()[3])+ 
    summary(Fitting_deltaL)$coefficients[3,2]
  c*te^2+c1*te+c2
}

vec <- seq(0,45,0.001)
df_out_deltaL_max <- data.frame(temp_ae = vec,
                                deltaL_jap <- sapply(vec,
                                                     mod_max))

colnames(df_out_deltaL_max) <- c("temp_ae","deltaL_jap")
df_out_deltaL_max[which(df_out_deltaL_max$deltaL_jap < 0),2] <- 0
df_out_deltaL_max$group <- "max"
df_out_deltaL$group <- "mean"

# Plot all three curves together
df_out_deltaL <- rbind(df_out_deltaL_min,
                       df_out_deltaL_max,
                       df_out_deltaL)

plotdeltaL <- ggplot(df_out_deltaL) +
  geom_line(aes(temp_ae,deltaL_jap,
                color = group,
                group = group, 
                alpha = group), size = 0.8) +
  geom_point(data = Lmortality,aes(Temp,mean_mort_perc),
             size = 1.1, color = "black") +
  ylab("Larva mortality rate") + xlab("Temperature (Cº)") +
  scale_color_manual(values=c("red", "blue", "red")) + 
  scale_alpha_manual(values = c(0.5,1,0.5)) +
  xlim(c(5,35)) + ylim(c(-1,2)) + 
  guides(color = FALSE, alpha = FALSE) +
  theme_bw()
plotdeltaL

# add grey ribbon --------------------------------------------
df_out_deltaL_w <- reshape(df_out_deltaL, idvar ="temp_ae" ,
                           timevar = "group", direction = "wide")

plotdeltaL_w <- ggplot(df_out_deltaL_w, aes(x=temp_ae,y=deltaL_jap.min)) +
  geom_line(aes(y=deltaL_jap.min), color = grey_col, size = 0.7, alpha=0.5) +
  geom_line(aes(y=deltaL_jap.max), color = grey_col, size = 0.7, alpha=0.5) +
  geom_line(aes(y=deltaL_jap.mean), color = line_col, size = 0.7) +
  geom_ribbon(data = df_out_deltaL_w,aes(ymin=deltaL_jap.min,
                                         ymax=deltaL_jap.max), fill=grey_col, alpha=0.5) +
  geom_point(data = Lmortality,aes(Temp,mean_mort_perc),
             size = 1.1, color = german_col) +
  ylab("Larva mortality rate") + xlab("Temperature (Cº)") +
  xlim(c(5,35)) + ylim(c(-0.6,2.3)) + 
  guides(color = FALSE, alpha = FALSE) +
  theme_bw()
plotdeltaL_w 

# Delta E ---------------------------------------
# Data from: https://www.researchgate.net/publication/235430511_The_ecology_of_the_exotic_mosquito_Ochlerotatus_Finlaya_japonicus_japonicus_Theobald_1901_Diptera_Culicidae_and_an_examination_of_its_role_in_the_West_Nile_virus_cycle_in_New_Jersey
delta_E_df <- data.frame(temp = c(10,16,22,28,34,40),
                         delta_E = c(1-0.493,1-0.971,1-0.95,1-0.892,1-0.367,1))
plot(delta_E_df)

## Quadratic normal fit
Fitting_deltaE <- nls(delta_E ~ cont*temp^2 + cont1*temp + cont2,
                      data = delta_E_df,
                      start = list(cont = 0.3, cont1 = 0.5, cont2=0.3))

summary(Fitting_deltaE)

# Linear function fit
Fitting_deltaE_lin <- nls(delta_E ~ cont*temp + cont1,
                          data = delta_E_df,
                          start = list(cont = 0, cont1 = 0))

summary(Fitting_deltaE_lin)

# AIC for the two models:
AIC(Fitting_deltaE,Fitting_deltaE_lin)

mod <- function(te){
  c <- as.numeric(Fitting_deltaE$m$getPars()[1])
  c1 <- as.numeric(Fitting_deltaE$m$getPars()[2])
  c2 <- as.numeric(Fitting_deltaE$m$getPars()[3])
  c*te^2+c1*te+c2
}

vec <- seq(0,45,0.001)
df_out_deltaE <- data.frame(temp_ae = vec,
                            deltaE_jap <- sapply(vec, mod))

colnames(df_out_deltaE) <- c("temp_ae","deltaE_jap")
df_out_deltaE[which(df_out_deltaE$deltaE_jap < 0),2] <- 0

plotdeltaE <- ggplot(df_out_deltaE) +
  geom_line(aes(temp_ae,deltaE_jap), size = 0.8) +
  geom_point(data = delta_E_df,aes(temp,delta_E),
             size = 1.1, color = "red") +
  ylab("Larva mortality rate") + xlab("Temperature (Cº)") +
  theme_bw()
plotdeltaE

df_out_deltaE[df_out_deltaE$deltaE_jap == min(df_out_deltaE$deltaE_jap, na.rm = TRUE),]

####----------- +/- SD-------------####
# Mean - sd
mod_min <- function(te){
  c <- as.numeric(Fitting_deltaE$m$getPars()[1]) - 
    summary(Fitting_deltaE)$coefficients[1,2]
  c1 <- as.numeric(Fitting_deltaE$m$getPars()[2])- 
    summary(Fitting_deltaE)$coefficients[2,2]
  c2 <- as.numeric(Fitting_deltaE$m$getPars()[3])- 
    summary(Fitting_deltaE)$coefficients[3,2]
  c*te^2+c1*te+c2
}

vec <- seq(0,45,0.001)
df_out_deltaE_min <- data.frame(temp_ae = vec,
                                deltaE_jap <- sapply(vec,
                                                     mod_min))

colnames(df_out_deltaE_min) <- c("temp_ae","deltaE_jap")
df_out_deltaE_min$group <- "min"

### Mean + sd
mod_max <- function(te){
  c <- as.numeric(Fitting_deltaE$m$getPars()[1]) + 
    summary(Fitting_deltaE)$coefficients[1,2]
  c1 <- as.numeric(Fitting_deltaE$m$getPars()[2])+ 
    summary(Fitting_deltaE)$coefficients[2,2]
  c2 <- as.numeric(Fitting_deltaE$m$getPars()[3])+ 
    summary(Fitting_deltaE)$coefficients[3,2]
  c*te^2+c1*te+c2
}

vec <- seq(0,45,0.001)
df_out_deltaE_max <- data.frame(temp_ae = vec,
                                deltaE_jap <- sapply(vec,
                                                     mod_max))

colnames(df_out_deltaE_max) <- c("temp_ae","deltaE_jap")
df_out_deltaE_max[which(df_out_deltaE_max$deltaE_jap < 0),2] <- 0
df_out_deltaE_max$group <- "max"
df_out_deltaE$group <- "mean"

# Plot all three curves together
df_out_deltaE <- rbind(df_out_deltaE_min,
                       df_out_deltaE_max,
                       df_out_deltaE)

# add grey ribbon --------------------------------------------
df_out_deltaE_w <- reshape(df_out_deltaE, idvar ="temp_ae" ,
                           timevar = "group", direction = "wide")

plotdeltaE_w <- ggplot(df_out_deltaE_w, aes(x=temp_ae,y=deltaE_jap.min)) +
  geom_line(aes(y=deltaE_jap.min), color = grey_col, size = 0.7, alpha=0.5) +
  geom_line(aes(y=deltaE_jap.max), color = grey_col, size = 0.7, alpha=0.5) +
  geom_line(aes(y=deltaE_jap.mean), color = line_col, size = 0.7) +
  geom_ribbon(data = df_out_deltaE_w,aes(ymin=deltaE_jap.min,
                                         ymax=deltaE_jap.max), fill=grey_col, alpha=0.5) +
  geom_point(data = delta_E_df,aes(temp,delta_E),
             size = 1.1, color = jame_col) +
  ylab("Egg mortality rate") + xlab("Temperature (Cº)") +
  xlim(c(5,35)) + ylim(c(-1.5,2.4)) + 
  guides(color = FALSE, alpha = FALSE) +
  theme_bw()
plotdeltaE_w 

###----------------------------------------------
library(ggpubr)
library(latex2exp)
sizelet = 14
ggarrange(
          plotdeltaA_w +
            theme(text = element_text(size = sizelet)) +
            ylab(TeX("Adult life span, lf")) ,
          plotdEjap_w  +
            theme(text = element_text(size = sizelet))  +
            ylab(TeX("Egg development rate, d$_E$"))  ,
          plotdeltaE_w +
            theme(text = element_text(size = sizelet)) +
            ylab(TeX("Egg mortality rate, \\delta$_E$"))  ,
          plotdL_w +
            theme(text = element_text(size = sizelet)) +
            ylab(TeX("Larva development rate, d$_L$")) ,
          plotdeltaL_w + ylim(c(-0.7,2.2)) +
            theme(text = element_text(size = sizelet)) +
            ylab(TeX("Larva mortality rate, \\delta$_L$"))   )

# Save panel
ggsave("/home/marta/Documentos/PHD/2025/Aedes_land/Plots/thermal_responses_jap.png", 
      width = 12.00, height = 7.00, 
       dpi = 300)

ggsave("/home/marta/Documentos/PHD/2025/Aedes_land/Plots/thermal_responses_jap.pdf", 
       width = 12.00, height = 7.00)
