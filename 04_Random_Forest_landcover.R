################################# Random Forest ################################
library(tidyverse)
library(terra)
library(dplyr)
library(sf)

library(randomForestSRC)
library(caret)
library(pROC)
library(ggplot2)

rm(list = ls())

sf_use_s2(FALSE)
# Directories ------------------------------------------------------------------
loc.output <- paste0(getwd(), "/OUTPUT/")
loc.fig <- paste0(getwd(), "/FIGURES/")
loc.heavy <- paste0(getwd(), "/Heavy_files/")
loc.clc <- paste0(getwd(), "/OUTPUT/land_cover_raster/")
loc.data <- paste0(getwd(), "/data/")

# Loading data -----------------------------------------------------------------
Path <- "data/dataset_Pardo-Araujo_et_al_2025_vf.csv"
aedes <- read.csv(Path) %>%
  mutate(
    ae_jap = as.factor(case_when(ae_jap == TRUE ~ 1, 
                                 ae_jap == FALSE ~ 0, 
                                 .default = ae_jap)),
    ae_kor = as.factor(case_when(ae_kor == TRUE ~ 1, 
                                 ae_kor == FALSE ~ 0, 
                                 .default = ae_kor))
  ) 
aedes$country <- NULL
aedes$data_source <- NULL
aedes$X <- NULL
aedes$X.1 <- NULL
aedes <- aedes[!duplicated(aedes), ]

aedes_sf <- aedes %>% st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# We need a template with pixel ID and lat-lon data
template <- readRDS(paste0(loc.clc, "coniferous_forest_rast.rds"))
template <- data.frame(
  pixel_id = terra::cellFromXY(template, st_coordinates(aedes_sf)),
  values = terra::extract(template, aedes_sf), 
  lon = terra::xyFromCell(template, terra::cellFromXY(template, st_coordinates(aedes_sf)))[, 1], # Longitud
  lat = terra::xyFromCell(template, terra::cellFromXY(template, st_coordinates(aedes_sf)))[, 2]  # Latitud
) %>%
  dplyr::select(-values.ID, -values.LABEL3)


aedes <- merge(aedes, template,  by = c("lon", "lat"), all.x = TRUE) 

# Checking map
europe <- eurostat::get_eurostat_geospatial(resolution = 10,
                                            nuts_level = 0,
                                            year = 2021) %>%
  st_transform(4326)
ggplot() +
  geom_sf(data = europe, aes(fill = NAME_LATN), fill = "white", show.legend = FALSE) +
  geom_point(data = aedes %>% filter(ae_jap == "1" ),
             aes(x = lon, y = lat), color = "#8FBC8F", alpha = 0.4, size = 1) +
  geom_point(data = aedes %>% filter(ae_kor == "1"),
             aes(x = lon, y = lat), color = "#6E7B8B", alpha = 0.2, size = 1) +
  theme_classic() +
  ylim(c(35, 72)) +
  xlim(c(-21, 50)) +
  labs(
    x = "Longitude",
    y = "Latitude"
  )

# Presence absence maps
ggplot() +
  geom_sf(data = europe, aes(fill = NAME_LATN), fill = "white", show.legend = FALSE) +
  geom_point(data = aedes  %>% drop_na(ae_jap), aes(x = lon, y = lat, color = ae_jap), alpha = 0.4, size = 0.2) +
  theme_classic() +
  ylim(c(35, 72)) +
  xlim(c(-21, 50)) +
  labs(
    x = "Longitude",
    y = "Latitude"
  )
ggplot() +
  geom_sf(data = europe, aes(fill = NAME_LATN), fill = "white", show.legend = FALSE) +
  geom_point(data = aedes %>% drop_na(ae_kor), aes(x = lon, y = lat, color = ae_kor), alpha = 0.4, size = 0.2) +
  theme_classic() +
  ylim(c(35, 72)) +
  xlim(c(-21, 50)) +
  labs(
    x = "Longitude",
    y = "Latitude"
  )

# Adding CLC data --------------------------------------------------------------
clc_per <- readRDS(paste0(loc.heavy, "df_perc_landcover_EU.Rds")) # Data calculated with other script
clc_per <- clc_per %>%
  mutate(
    pixel_id = row.names(clc_per)
  )

# Join aedes and clc_per
clc_per <- clc_per %>% dplyr::select(-lon, -lat, -Total, -sum) # Cleaning the data
aedes <- merge(aedes, clc_per, by = "pixel_id", all.x = TRUE)

# Checking data with maps
europe <- eurostat::get_eurostat_geospatial(resolution = 10,
                                            nuts_level = 0,
                                            year = 2021) %>%
  st_transform(4326)
ggplot() +
  geom_sf(data = europe, aes(fill = NAME_LATN), fill = "white", show.legend = FALSE) +
  geom_point(data = aedes %>% filter(ae_jap == TRUE),
             aes(x = lon, y = lat), color = "#8FBC8F", alpha = 0.4, size = 1) +
  geom_point(data = aedes %>% filter(ae_kor == TRUE),
             aes(x = lon, y = lat), color = "#6E7B8B", alpha = 0.2, size = 1) +
  theme_classic() +
  ylim(c(35, 72)) +
  xlim(c(-21, 50)) +
  labs(
    x = "Longitude",
    y = "Latitude"
  )

# Splitting the data -----------------------------------------------------------
set.seed(123454321)
split <- sample.split(aedes, SplitRatio = 0.8) 

train_total <- subset(aedes, split == "TRUE") 
test_total <- subset(aedes, split == "FALSE") 

aedes <- train_total

saveRDS(test_total, file = paste0(loc.output, "test.rds"))

# Ae. japonicus ----------------------------------------------------------------
summary(aedes$ae_jap)

aedes <- aedes %>% dplyr::select(-ae_kor)
aedes <- aedes[!duplicated(aedes), ]

## Modeling process: categorical RF ---------------------------------------------
# Find optimal mtry and nodesize parameters
set.seed(123456541)
mod_form <- as.formula(paste("ae_jap ~ ", 
                             paste(corine, collapse = " + ")))
op <- tune(mod_form, data = aedes, sampsize = 999, ntree = 100, seed = 1234321)
print(op$optimal) 

## Feature selection -----------------------------------------------------------
imb_model <- rfsrc(mod_form, data = aedes,
                   importance = TRUE,
                   rfq =  TRUE, 
                   ntree = 999, nodesize = op$optimal[1], mtry = op$optimal[2],
                   perf.type = "gmean", splitrule = "auc",
                   seed = 123456541)
imb_model

imb_model_selection <- var.select(object = imb_model, method="md", 
                                  ntree = 999, nodesize = op$optimal[1], mtry = op$optimal[2],
                                  seed = 123456541) 

# Reloading the model with optimal parametrization
mod_form <- as.formula(paste("ae_jap ~ ", 
                             paste(imb_model_selection$topvars, collapse = " + ")))
mod_form

set.seed(123454321)
op <- tune(mod_form, data = aedes,sampsize = 999, ntree = 100, seed = 1234321)
print(op$optimal) 

imb_model <- rfsrc(mod_form, data = aedes,
                   importance = TRUE,
                   rfq =  TRUE, 
                   ntree = 3000, nodesize = op$optimal[1], mtry = op$optimal[2],
                   perf.type = "gmean", splitrule = "auc",
                   seed = 123456541)
imb_model

plot(imb_model, plots.one.page = TRUE)
plot(vimp(imb_model, seed = 123456541))

err_df <- as.data.frame(imb_model$err.rate)
err_df$ntree <- 1:nrow(err_df)
err_df <- err_df[, c("all", "ntree")] %>% drop_na()

errorjap <- ggplot(err_df, aes(x = ntree, y = all)) +
  geom_line() +
  labs(x = "Number of Trees", y = "OOB Error Rate") +
  theme_classic(base_size = 10, base_family = "Helvetica")

var_imp <- data.frame(
  variable = rownames(imb_model$importance),
  importance = imb_model$importance
) %>%
  arrange(desc(importance.all), variable)

vimpjap <- ggplot(var_imp, aes(y =  reorder(variable, importance.all), x = importance.all)) +
  geom_col() +
  labs(y = "Variable", 
       x = "Importance"
  ) +
  theme_classic(base_size = 10, base_family = "Helvetica")

ggpubr::ggarrange(errorjap, vimpjap, labels = c("a", "b"))

saveRDS(imb_model, file = paste0(loc.heavy, "jap_model_train.rds"))

## Partial plots ---------------------------------------------------------------
var_names = names(imb_model$xvar)

for (i in seq_along(var_names)) {
  png(paste0(loc.fig, "Ae_japonicus/train/", var_names[i], ".png"), bg = "white")
  plot.variable(imb_model, xvar.names=var_names[i], partial=TRUE, show.plots=TRUE,
                target = "1")
  dev.off()
}

## Model Validation ------------------------------------------------------------
# Remember: RF does not work if NAs are present
set.seed(123454321)

train <- train_total %>% drop_na(c(ae_jap, names(imb_model$xvar)))
test <- test_total  %>% drop_na(c(ae_jap, names(imb_model$xvar)))

imb_model_train <- imb_model

# Predictions test 
test_pred <- predict.rfsrc(imb_model_train, newdata = test, outcome = "test", perf.type = "brier")

# Model performance: ROC 
# ROC curve will allow us to visualize the true positive rate (sensitivity) vs the
# false positive rate (specificity)
roc.test <- pROC::roc(test$ae_jap, test_pred$predicted[,2])
auc(roc.test)
plot(roc.test, col = "steelblue", main = "Unweighted Random Forest")

ggroc(roc.test, colour = 'steelblue', size = 2) +
  geom_abline(colour = "black", slope=1, intercept = 1, alpha = 0.7) +
  ggtitle(paste0('ROC Curve Random Forest ', 
                 '(AUC = ', round(auc(roc.test), 4), ')')) +
  theme_classic()

ggsave(file = paste0(loc.fig, "jap_roc_train.png"), units = "cm", 
       width = 30, height = 20)

## Europe prediction: testing the japonicus plot -------------------------------
# Using a template created from the aggration analysis; check heavy files folder
template <- readRDS(paste0(loc.clc, "coniferous_forest_rast.rds"))

# clc
clc_per <- readRDS(paste0(loc.heavy, "df_perc_landcover_EU.Rds")) 
clc_per <- clc_per %>%
  mutate(
    pixel_id = row.names(clc_per)
  )
clc_per <- clc_per[, names(imb_model$xvar)]

# Predictions
europe_pred <- predict.rfsrc(imb_model, newdata = clc_per, 
                             perf.type = "brier")

template_df <- data.frame(
  pixel_id = cells(template), # Obtaining pixel ID
  lon = xyFromCell(template, cells(template))[,1], # X coordinates
  lat = xyFromCell(template, cells(template))[,2], # Y coordinates
  value = europe_pred$predicted[,2] 
)
template_df$value <- ifelse(template_df$value == template_df$value[1], NA, template_df$value)

template_sf <- st_as_sf(template_df, coords = c("lon", "lat"), crs = st_crs(europe), remove = FALSE)
inside_europe <- lengths(st_intersects(template_sf, europe)) > 0
template_sf$value[is.na(template_sf$value) & inside_europe] <- 0
template_df <- template_sf %>%
  st_drop_geometry()
saveRDS(template_df, paste0(loc.output, "jap_predictedprob_train.rds"))

europe_jap <- ggplot(template_df) +
  geom_raster(aes(x = lon, y = lat, fill = value), interpolate = TRUE, alpha = 0.8) +
  scale_fill_distiller(palette = "Spectral", direction = -1, 
                       name = "Suitability\n", na.value="white") +
  geom_sf(data = europe, fill = "transparent", color = "grey20", size = 0.3) +
  coord_sf(xlim = c(-24, 50), ylim = c(35, 72), expand = FALSE) +
  theme_classic(base_size = 10, base_family = "Helvetica") +
  labs(
    x = "Longitude",
    y = "Latitude")

ggsave(file = paste0(loc.fig, "jap_inference_train.png"), units = "cm", 
       width = 30, height = 15)

# Ae. koreicus ----------------------------------------------------------------
summary(aedes$ae_kor)

aedes <- aedes %>% dplyr::select(-ae_jap)
aedes <- aedes[!duplicated(aedes), ]

## Modeling process: categorical RF ---------------------------------------------
# Find optimal mtry and nodesize parameters
set.seed(123456541)
mod_form <- as.formula(paste("ae_kor ~ ", 
                             paste(corine, collapse = " + ")))
op <- tune(mod_form, data = aedes, sampsize = 999, ntree = 100, seed = 1234321)
print(op$optimal) 

## Feature selection -----------------------------------------------------------
imb_model <- rfsrc(mod_form, data = aedes,
                   importance = TRUE,
                   rfq =  TRUE, 
                   ntree = 999, nodesize = op$optimal[1], mtry = op$optimal[2],
                   perf.type = "gmean", splitrule = "auc",
                   seed = 123456541)
imb_model

imb_model_selection <- var.select(object = imb_model, method="md", 
                                  ntree = 999, nodesize = op$optimal[1], mtry = op$optimal[2],
                                  seed = 123456541) 

# Reloading the model with the optimal parametrization
mod_form <- as.formula(paste("ae_kor ~ ", 
                             paste(imb_model_selection$topvars, collapse = " + ")))
mod_form

set.seed(123454321)
op <- tune(mod_form, data = aedes,sampsize = 999, ntree = 100, seed = 1234321)
print(op$optimal) 

imb_model <- rfsrc(mod_form, data = aedes,
                   importance = TRUE,
                   rfq =  TRUE, 
                   ntree = 3000, nodesize = op$optimal[1], mtry = op$optimal[2],
                   perf.type = "gmean", splitrule = "auc",
                   seed = 123456541)
imb_model

plot(imb_model, plots.one.page = TRUE)
plot(vimp(imb_model, seed = 123456541))

err_df <- as.data.frame(imb_model$err.rate)
err_df$ntree <- 1:nrow(err_df)
err_df <- err_df[, c("all", "ntree")] %>% drop_na()

errorkor <- ggplot(err_df, aes(x = ntree, y = all)) +
  geom_line() +
  labs(x = "Number of Trees", y = "OOB Error Rate") +
  theme_classic(base_size = 10, base_family = "Helvetica")

var_imp <- data.frame(
  variable = rownames(imb_model$importance),
  importance = imb_model$importance
) %>%
  arrange(desc(importance.all), variable)

vimpkor <- ggplot(var_imp, aes(y =  reorder(variable, importance.all), x = importance.all)) +
  geom_col() +
  labs(y = "Variable", 
       x = "Importance"
  ) +
  theme_classic(base_size = 10, base_family = "Helvetica")

ggpubr::ggarrange(errorjap, vimpjap, 
                  errorkor, vimpkor, 
                  labels = c("a", "b", "c", "d"))

ggsave(file = paste0(loc.fig, "RF_summary_OBB_and_vimp2_train_SEQ3.pdf"), 
       width = 18, height = 16, dpi = 600, units = "cm", device = cairo_pdf)

saveRDS(imb_model, file = paste0(loc.heavy, "kor_model_train.rds"))

## Partial plots ---------------------------------------------------------------
var_names = names(imb_model$xvar)

for (i in seq_along(var_names)) {
  
  png(paste0(loc.fig, "Ae_koreicus/train/", var_names[i], ".png"), bg = "white")
  plot.variable(imb_model, xvar.names=var_names[i], partial=TRUE, show.plots=TRUE,
                target = "1")
  dev.off()
}

## Model Validation ------------------------------------------------------------
# Remember: RF does not work if NAs are present
# Splitting the data
set.seed(123454321)

train <- train_total %>% drop_na(c(ae_kor, names(imb_model$xvar)))
test <- test_total  %>% drop_na(c(ae_kor, names(imb_model$xvar)))

imb_model_train <- imb_model

# Predictions test 
test_pred <- predict.rfsrc(imb_model_train, newdata = test, outcome = "test", perf.type = "brier")

# Model performance: ROC 
# ROC curve will allow us to visualize the true positive rate (sensitivity) vs the
# false positive rate (specificity)
roc.test <- pROC::roc(test$ae_kor, test_pred$predicted[,2])
auc(roc.test)
plot(roc.test, col = "steelblue", main = "Unweighted Random Forest")

ggroc(roc.test, colour = 'steelblue', size = 2) +
  geom_abline(colour = "black", slope=1, intercept = 1, alpha = 0.7) +
  ggtitle(paste0('ROC Curve Random Forest ', 
                 '(AUC = ', round(auc(roc.test), 4), ')')) +
  theme_classic()

ggsave(file = paste0(loc.fig, "kor_roc_train.png"), units = "cm", 
       width = 30, height = 20)

## Europe prediction: testing the koreicus plot --------------------------------
# Using a template created from the aggration analysis; check heavy files folder
template <- readRDS(paste0(loc.clc, "coniferous_forest_rast.rds"))

# clc
clc_per <- readRDS(paste0(loc.heavy, "df_perc_landcover_EU.Rds")) 
clc_per <- clc_per %>%
  mutate(
    pixel_id = row.names(clc_per)
  )
clc_per <- clc_per[, names(imb_model$xvar)]

europe_pred <- predict.rfsrc(imb_model, newdata = clc_per, 
                             perf.type = "brier")

template_df <- data.frame(
  pixel_id = cells(template), # Obtaining pixel ID
  lon = xyFromCell(template, cells(template))[,1], # X coordinates
  lat = xyFromCell(template, cells(template))[,2], # Y coordinates
  value = europe_pred$predicted[,2] 
)
template_df$value <- ifelse(template_df$value == template_df$value[1], NA, template_df$value)

template_sf <- st_as_sf(template_df, coords = c("lon", "lat"), crs = st_crs(europe), remove = FALSE)
inside_europe <- lengths(st_intersects(template_sf, europe)) > 0
template_sf$value[is.na(template_sf$value) & inside_europe] <- 0
template_df <- template_sf %>%
  st_drop_geometry()

saveRDS(template_df, paste0(loc.output, "kor_predictedprob_train.rds"))

europe_kor <- ggplot(template_df) +
  geom_raster(aes(x = lon, y = lat, fill = value), interpolate = TRUE, alpha = 0.8) +
  scale_fill_distiller(palette = "Spectral", direction = -1, 
                       name = "Suitability\n", na.value="white") +
  geom_sf(data = europe, fill = "transparent", color = "grey20", size = 0.3) +
  coord_sf(xlim = c(-24, 50), ylim = c(35, 72), expand = FALSE) +
  theme_classic(base_size = 10, base_family = "Helvetica") +
  labs(
    x = "Longitude",
    y = "Latitude")

ggsave(file = paste0(loc.fig, "kor_inference_train.png"), units = "cm", 
       width = 30, height = 15)

ggpubr::ggarrange(europe_jap, europe_kor, common.legend = TRUE, legend = "right", labels = c("a", "b"),
                  label.y = 0.75 )

ggsave(file = paste0(loc.fig, "Europe_inference_RF2_train_SEQ3.pdf"), 
       width = 18, height = 16, dpi = 600, units = "cm", device = cairo_pdf)
