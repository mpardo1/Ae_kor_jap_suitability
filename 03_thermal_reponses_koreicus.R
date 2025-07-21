## Compute the thermal responses for Aedes Koreicus from literature data
# compare also different functions and compute its AIC value
rm(list= ls())
library(ggplot2)
library(tidyverse)
library(nls2)

# Load data ------------------------------------------------------------------
# Data taken from https://parasitesandvectors.biomedcentral.com/articles/10.1186/s13071-019-3772-5
Path_dir <- "~/aedes_korjap_suitability/data/Koreicus_temp/"
dir_kor <- list.files(Path_dir)

# Load different tables
lf_gono_df_r <- read.csv(paste0(Path_dir,dir_kor[1]))
develop_time_df_r <- read.csv(paste0(Path_dir,dir_kor[2]))
hat_surv_rate_df_r <- read.csv(paste0(Path_dir,dir_kor[3]))

# Aggregate the larva to adult stage in one --------------------------------------
extract_bounds <- function(range_str) {
  bounds <- as.numeric(unlist(strsplit(gsub("[()]", "", range_str), "-")))
  return(bounds)
}

# Apply this to each range column
comp_lu <- function(df){
    df$range_8_lower <- sapply(df$range_8, function(x) extract_bounds(x)[1])
    df$range_8_upper <- sapply(df$range_8, function(x) extract_bounds(x)[2])
    df$range_13_lower <- sapply(df$range_13, function(x) extract_bounds(x)[1])
    df$range_13_upper <- sapply(df$range_13, function(x) extract_bounds(x)[2])
    df$range_18_lower <- sapply(df$range_18, function(x) extract_bounds(x)[1])
    df$range_18_upper <- sapply(df$range_18, function(x) extract_bounds(x)[2])
    df$range_23_lower <- sapply(df$range_23, function(x) extract_bounds(x)[1])
    df$range_23_upper <- sapply(df$range_23, function(x) extract_bounds(x)[2])
    df$range_28_lower <- sapply(df$range_28, function(x) extract_bounds(x)[1])
    df$range_28_upper <- sapply(df$range_28, function(x) extract_bounds(x)[2])
    df$range_33_lower <- sapply(df$range_33, function(x) extract_bounds(x)[1])
    df$range_33_upper <- sapply(df$range_33, function(x) extract_bounds(x)[2])
  return(df)
}

# Repeat for other range columns (e.g., range_13, etc.)
develop_time_df_r <- comp_lu(develop_time_df_r)
hat_surv_rate_df_r <- comp_lu(hat_surv_rate_df_r)

# Sum from egg to pupa
agg_ci <- function(df){
    df_out <- df[1,c(1:13)]
    df_out[2,] <- c("L",0,"",0,"",0,"",0,"",0,"",0,"")
    sum_lower <- sum(df[c(2:nrow(df)), "range_8_lower"], na.rm = TRUE)
    sum_upper <- sum(df[c(2:nrow(df)), c("range_8_upper")], na.rm = TRUE)
    df_out$mean_8[2] <- as.numeric(sum(df[c(2:nrow(df)), c("mean_8")], na.rm = TRUE))
    df_out$range_8[2] <- paste0("(", sum_lower, "-", sum_upper, ")")
    sum_lower <- sum(df[c(2:nrow(df)), c("range_13_lower")], na.rm = TRUE)
    sum_upper <- sum(df[c(2:nrow(df)), c("range_13_upper")], na.rm = TRUE)
    df_out$mean_13[2] <- as.numeric(sum(df[c(2:nrow(df)), c("mean_13")], na.rm = TRUE))
    df_out$range_13[2] <- paste0("(", sum_lower, "-", sum_upper, ")")
    sum_lower <- sum(df[c(2:nrow(df)), c("range_18_lower")], na.rm = TRUE)
    sum_upper <- sum(df[c(2:nrow(df)), c("range_18_upper")], na.rm = TRUE)
    df_out$mean_18[2] <- as.numeric(sum(df[c(2:nrow(df)), c("mean_18")], na.rm = TRUE))
    df_out$range_18[2] <- paste0("(", sum_lower, "-", sum_upper, ")")
    sum_lower <- sum(df[c(2:nrow(df)), c("range_23_lower")], na.rm = TRUE)
    sum_upper <- sum(df[c(2:nrow(df)), c("range_23_upper")], na.rm = TRUE)
    df_out$mean_23[2] <- as.numeric(sum(df[c(2:nrow(df)), c("mean_23")], na.rm = TRUE))
    df_out$range_23[2] <- paste0("(", sum_lower, "-", sum_upper, ")")
    sum_lower <- sum(df[c(2:nrow(df)), c("range_28_lower")], na.rm = TRUE)
    sum_upper <- sum(df[c(2:nrow(df)), c("range_28_upper")], na.rm = TRUE)
    df_out$mean_28[2] <- as.numeric(sum(df[c(2:nrow(df)), c("mean_28")], na.rm = TRUE))
    df_out$range_28[2] <- paste0("(", sum_lower, "-", sum_upper, ")")
    sum_lower <- sum(df[c(2:nrow(df)), c("range_33_lower")], na.rm = TRUE)
    sum_upper <- sum(df[c(2:nrow(df)), c("range_33_upper")], na.rm = TRUE)
    df_out$mean_33[2] <- as.numeric(sum(df[c(2:nrow(df)), c("mean_33")], na.rm = TRUE))
    df_out$range_33[2] <- paste0("(", sum_lower, "-", sum_upper, ")")
    
  return(df_out)
}

agg_ci_perc <- function(df){
  df_out <- df[1,c(1:13)]
  df_out[2,] <- c("L",0,"",0,"",0,"",0,"",0,"",0,"")
  sum_lower <- prod((df[c(2:nrow(df)), "range_8_lower"])/100, na.rm = TRUE)*100
  sum_upper <- prod((df[c(2:nrow(df)), c("range_8_upper")])/100, na.rm = TRUE)*100
  df_out$mean_8[2] <- as.numeric(prod((df[c(2:nrow(df)), c("mean_8")])/100, na.rm = TRUE)*100)
  df_out$range_8[2] <- paste0("(", sum_lower, "-", sum_upper, ")")
  sum_lower <- prod((df[c(2:nrow(df)), c("range_13_lower")])/100, na.rm = TRUE)*100
  sum_upper <- prod((df[c(2:nrow(df)), c("range_13_upper")])/100, na.rm = TRUE)*100
  df_out$mean_13[2] <- as.numeric(prod((df[c(2:nrow(df)), c("mean_13")])/100, na.rm = TRUE)*100)
  df_out$range_13[2] <- paste0("(", sum_lower, "-", sum_upper, ")")
  sum_lower <- prod((df[c(2:nrow(df)), c("range_18_lower")])/100, na.rm = TRUE)*100
  sum_upper <- prod((df[c(2:nrow(df)), c("range_18_upper")])/100, na.rm = TRUE)*100
  df_out$mean_18[2] <- as.numeric(prod((df[c(2:nrow(df)), c("mean_18")])/100, na.rm = TRUE)*100)
  df_out$range_18[2] <- paste0("(", sum_lower, "-", sum_upper, ")")
  sum_lower <- prod((df[c(2:nrow(df)), c("range_23_lower")])/100, na.rm = TRUE)*100
  sum_upper <- prod((df[c(2:nrow(df)), c("range_23_upper")])/100, na.rm = TRUE)*100
  df_out$mean_23[2] <- as.numeric(prod((df[c(2:nrow(df)), c("mean_23")])/100, na.rm = TRUE)*100)
  df_out$range_23[2] <- paste0("(", sum_lower, "-", sum_upper, ")")
  sum_lower <- prod((df[c(2:nrow(df)), c("range_28_lower")])/100, na.rm = TRUE)*100
  sum_upper <- prod((df[c(2:nrow(df)), c("range_28_upper")])/100, na.rm = TRUE)*100
  df_out$mean_28[2] <- as.numeric(prod((df[c(2:nrow(df)), c("mean_28")])/100, na.rm = TRUE)*100)
  df_out$range_28[2] <- paste0("(", sum_lower, "-", sum_upper, ")")
  sum_lower <- prod((df[c(2:nrow(df)), c("range_33_lower")])/100, na.rm = TRUE)*100
  sum_upper <- prod((df[c(2:nrow(df)), c("range_33_upper")])/100, na.rm = TRUE)*100
  df_out$mean_33[2] <- as.numeric(prod((df[c(2:nrow(df)), c("mean_33")])/100, na.rm = TRUE)*100)
  df_out$range_33[2] <- paste0("(", sum_lower, "-", sum_upper, ")")
  
  return(df_out)
}

# Create the new confidence interval as a string
develop_time_df_r <- agg_ci(develop_time_df_r)
hat_surv_rate_df_r <- agg_ci_perc(hat_surv_rate_df_r)

# Extract SD ---------------------------------------------------------
# Function to extract lower and upper bounds from a range string
extract_bounds <- function(range_str) {
  range_str <- gsub("[()]", "", range_str)
  bounds <- as.numeric(unlist(strsplit(range_str, "-")))
  return(bounds)
}

# Apply the function to calculate the standard deviation for each temperature
calculate_std_dev <- function(mean_value, range_str) {
  if (is.na(range_str) || range_str == "") return(NA)
  
  bounds <- extract_bounds(range_str)
  margin_of_error <- (bounds[2] - bounds[1]) / 2
  std_dev <- margin_of_error / 1.96
  return(std_dev)
}

# Adding standard deviation columns to the data frame
lf_gono_df_r$std_dev_18 <- mapply(calculate_std_dev, lf_gono_df_r$mean_18, 
                                  lf_gono_df_r$range_18)
lf_gono_df_r$std_dev_23 <- mapply(calculate_std_dev, lf_gono_df_r$mean_23,
                                  lf_gono_df_r$range_23)
lf_gono_df_r$std_dev_28 <- mapply(calculate_std_dev, lf_gono_df_r$mean_28,
                                  lf_gono_df_r$range_28)
lf_gono_df_r$std_dev_33 <- mapply(calculate_std_dev, lf_gono_df_r$mean_33,
                                  lf_gono_df_r$range_33)

develop_time_df_r$std_dev_8 <- mapply(calculate_std_dev, develop_time_df_r$mean_8,
                                       develop_time_df_r$range_8)
develop_time_df_r$std_dev_13 <- mapply(calculate_std_dev, develop_time_df_r$mean_13,
                                      develop_time_df_r$range_13)
develop_time_df_r$std_dev_18 <- mapply(calculate_std_dev, develop_time_df_r$mean_18,
                                       develop_time_df_r$range_18)
develop_time_df_r$std_dev_23 <- mapply(calculate_std_dev, develop_time_df_r$mean_23,
                                       develop_time_df_r$range_23)
develop_time_df_r$std_dev_28 <- mapply(calculate_std_dev, develop_time_df_r$mean_28,
                                       develop_time_df_r$range_28)
develop_time_df_r$std_dev_33 <- mapply(calculate_std_dev, develop_time_df_r$mean_33,
                                       develop_time_df_r$range_33)

hat_surv_rate_df_r$std_dev_8 <- mapply(calculate_std_dev, hat_surv_rate_df_r$mean_8,
                                       hat_surv_rate_df_r$range_8)
hat_surv_rate_df_r$std_dev_13 <- mapply(calculate_std_dev, hat_surv_rate_df_r$mean_13,
                                       hat_surv_rate_df_r$range_13)
hat_surv_rate_df_r$std_dev_18 <- mapply(calculate_std_dev, hat_surv_rate_df_r$mean_18,
                                       hat_surv_rate_df_r$range_18)
hat_surv_rate_df_r$std_dev_23 <- mapply(calculate_std_dev, hat_surv_rate_df_r$mean_23,
                                       hat_surv_rate_df_r$range_23)
hat_surv_rate_df_r$std_dev_28 <- mapply(calculate_std_dev, hat_surv_rate_df_r$mean_28,
                                       hat_surv_rate_df_r$range_28)
hat_surv_rate_df_r$std_dev_33 <- mapply(calculate_std_dev, hat_surv_rate_df_r$mean_33,
                                       hat_surv_rate_df_r$range_33)

# Generate random points ---------------------------------------------------
generate_random_points <- function(mean_value, std_dev, n_points) {
  if (is.na(mean_value) || is.na(std_dev)) return(rep(NA, n_points))
  return(rnorm(n_points, mean = mean_value, sd = std_dev))
}

# Generating random points for each temperature
set.seed(123)  # For reproducibility
n_points <- 120  # Number of points

lf_gono_df_rand <- rbind(data.frame(lf = generate_random_points(lf_gono_df_r$mean_18[1],
                                                     lf_gono_df_r$std_dev_18[1], n_points),
                              gono = generate_random_points(lf_gono_df_r$mean_18[2],
                                                          lf_gono_df_r$std_dev_18[2], n_points),
                              temp = rep(18,n_points)),
                        data.frame(lf = generate_random_points(lf_gono_df_r$mean_23[1],
                                                          lf_gono_df_r$std_dev_23[1], n_points),
                              gono = generate_random_points(lf_gono_df_r$mean_23[2],
                                                            lf_gono_df_r$std_dev_23[2], n_points),
                              temp = rep(23,n_points)),
                        data.frame(lf = generate_random_points(lf_gono_df_r$mean_28[1],
                                                          lf_gono_df_r$std_dev_28[1], n_points),
                              gono = generate_random_points(lf_gono_df_r$mean_28[2],
                                                            lf_gono_df_r$std_dev_28[2], n_points),
                              temp = rep(28,n_points)),
                        data.frame(lf = generate_random_points(lf_gono_df_r$mean_33[1],
                                                          lf_gono_df_r$std_dev_33[1], n_points),
                              gono = generate_random_points(lf_gono_df_r$mean_33[2],
                                                            lf_gono_df_r$std_dev_33[2], n_points),
                              temp = rep(33,n_points)
                              ))

develop_time_df_rand <- rbind(data.frame(E = generate_random_points(as.numeric(develop_time_df_r$mean_8[1]),
                                                                    develop_time_df_r$std_dev_8[1], n_points),
                                         L = generate_random_points(as.numeric(develop_time_df_r$mean_8[2]),
                                                                    develop_time_df_r$std_dev_8[2], n_points),
                                         temp = rep(8,n_points)),
                              data.frame(E = generate_random_points(as.numeric(develop_time_df_r$mean_13[1]),
                                                                    develop_time_df_r$std_dev_13[1], n_points),
                                         L = generate_random_points(as.numeric(develop_time_df_r$mean_13[2]),
                                                                    develop_time_df_r$std_dev_13[2], n_points),
                                         temp = rep(13,n_points)),
                              data.frame(E = generate_random_points(as.numeric(develop_time_df_r$mean_18[1]),
                                                                     develop_time_df_r$std_dev_18[1], n_points),
                                    L = generate_random_points(as.numeric(develop_time_df_r$mean_18[2]),
                                                                  develop_time_df_r$std_dev_18[2], n_points),
                                    temp = rep(18,n_points)),
                         data.frame(E = generate_random_points(as.numeric(develop_time_df_r$mean_23[1]),
                                                                develop_time_df_r$std_dev_23[1], n_points),
                                    L = generate_random_points(as.numeric(develop_time_df_r$mean_23[2]),
                                                                  develop_time_df_r$std_dev_23[2], n_points),
                                    temp = rep(23,n_points)),
                         data.frame(E = generate_random_points(as.numeric(develop_time_df_r$mean_28[1]),
                                                                develop_time_df_r$std_dev_28[1], n_points),
                                    L = generate_random_points(as.numeric(develop_time_df_r$mean_28[2]),
                                                                  develop_time_df_r$std_dev_28[2], n_points),
                                    temp = rep(28,n_points)),
                         data.frame(E = generate_random_points(as.numeric(develop_time_df_r$mean_33[1]),
                                                                develop_time_df_r$std_dev_33[1], n_points),
                                    L = generate_random_points(as.numeric(develop_time_df_r$mean_33[2]),
                                                                  develop_time_df_r$std_dev_33[2], n_points),
                                    temp = rep(33,n_points)
                         ))

hat_surv_rate_df_r_rand <- rbind(data.frame(E = generate_random_points(as.numeric(hat_surv_rate_df_r$mean_8[1]),
                                                                       hat_surv_rate_df_r$std_dev_8[1], n_points),
                                         L = generate_random_points(as.numeric(hat_surv_rate_df_r$mean_8[2]),
                                                                    hat_surv_rate_df_r$std_dev_8[2], n_points),
                                         temp = rep(8,n_points)),
                              data.frame(E = generate_random_points(as.numeric(hat_surv_rate_df_r$mean_13[1]),
                                                                    hat_surv_rate_df_r$std_dev_13[1], n_points),
                                         L = generate_random_points(as.numeric(hat_surv_rate_df_r$mean_13[2]),
                                                                    hat_surv_rate_df_r$std_dev_13[2], n_points),
                                         temp = rep(13,n_points)),
                              data.frame(E = generate_random_points(as.numeric(hat_surv_rate_df_r$mean_18[1]),
                                                                    hat_surv_rate_df_r$std_dev_18[1], n_points),
                                         L = generate_random_points(as.numeric(hat_surv_rate_df_r$mean_18[2]),
                                                                    hat_surv_rate_df_r$std_dev_18[2], n_points),
                                         temp = rep(18,n_points)),
                              data.frame(E = generate_random_points(as.numeric(hat_surv_rate_df_r$mean_23[1]),
                                                                    hat_surv_rate_df_r$std_dev_23[1], n_points),
                                         L = generate_random_points(as.numeric(hat_surv_rate_df_r$mean_23[2]),
                                                                    hat_surv_rate_df_r$std_dev_23[2], n_points),
                                         temp = rep(23,n_points)),
                              data.frame(E = generate_random_points(as.numeric(hat_surv_rate_df_r$mean_28[1]),
                                                                    hat_surv_rate_df_r$std_dev_28[1], n_points),
                                         L = generate_random_points(as.numeric(hat_surv_rate_df_r$mean_28[2]),
                                                                    hat_surv_rate_df_r$std_dev_28[2], n_points),
                                         temp = rep(28,n_points)),
                              data.frame(E = generate_random_points(as.numeric(hat_surv_rate_df_r$mean_33[1]),
                                                                    hat_surv_rate_df_r$std_dev_33[1], n_points),
                                         L = generate_random_points(as.numeric(hat_surv_rate_df_r$mean_33[2]),
                                                                    hat_surv_rate_df_r$std_dev_33[2], n_points),
                                         temp = rep(33,n_points)
                              ))

# # Function to filter df ----------------------------------------------------
# filt_df <- function(df_aux){
#   
#   if(ncol(df_aux) > 9){
#     df_filt <- df_aux[, c(1,2,4,6,8,10,12)]
#     if(df_filt[2,4] >50){
#       df_filt[nrow(df_filt)+1,] <-c("LP" ,
#                                     as.numeric(((colSums(df_filt[c(2,3,4,5,6)
#                                                                  ,c(2:7)]))/500)*100))
#     }else{
#       df_filt[nrow(df_filt)+1,] <-c("LP" ,
#                                     colSums(df_filt[c(2,3,4,5,6),c(2:7)]))
#     }
#     df_filt <- reshape::melt(df_filt, id = c("Stage.4.C"))
#   }else{
#     df_filt <- reshape::melt(df_aux[, c(1,2,4,6,8)])
#   }
#   
#   df_filt$variable <- as.numeric(substr(df_filt$variable,6,8))
#   colnames(df_filt) <- c("Stage", "temp", "variable")
#   df_filt$variable <- as.numeric(df_filt$variable)
#   df_filt <- as.data.frame(reshape(df_filt,idvar = "temp",
#                                    timevar = "Stage",direction = "wide"))
#   colnames(df_filt) <- sub(".*\\.", "", colnames(df_filt))
#   attr(df_filt, "reshapeWide") <- NULL
#   return(df_filt)
# }
# 
# # Transform each data frame
# lf_gono_df <- filt_df(lf_gono_df_r)
# develop_time_df <- filt_df(develop_time_df_r)
# hat_surv_rate_df <- filt_df(hat_surv_rate_df_r)

# Plot different data
head(lf_gono_df_rand)
ggplot(lf_gono_df_rand) + geom_point(aes(temp, lf, color = "Lf")) +
  geom_point(aes(temp, gono, color = "Gonotrophic"))

# Development rate plot
head(develop_time_df_rand)
ggplot(develop_time_df_rand) +
  geom_point(aes(temp, E, color = "E")) +
  geom_point(aes(temp, L, color = "L")) 

# Survival plot
head(hat_surv_rate_df_r_rand)
ggplot(hat_surv_rate_df_r_rand) +
  geom_point(aes(temp, E, color = "E")) +
  geom_point(aes(temp, L, color = "L")) 

## Fit Different variables  --------------------------------------------------
# Functions to plot fitted models --------------------------------------------
# Linear
mod_lin <- function(Fitted_model,te){
  c <- as.numeric(Fitted_model$m$getPars()[1])
  c1 <- as.numeric(Fitted_model$m$getPars()[2])
  c*te+c1
}

# Quadratic Normal
mod_quadN <- function(Fitted_model,te){
  c <- as.numeric(Fitted_model$m$getPars()[1])
  c1 <- as.numeric(Fitted_model$m$getPars()[2])
  c2 <- as.numeric(Fitted_model$m$getPars()[3])
  c*te^2+c1*te+c2
}

# Quadratic
mod_quad <- function(Fitted_model,te){
  c <- as.numeric(Fitted_model$m$getPars()[1])
  c1 <- as.numeric(Fitted_model$m$getPars()[2])
  c2 <- as.numeric(Fitted_model$m$getPars()[3])
  -c*(te-c1)*(te-c2)
}

# Briere
mod_Briere <- function(Fitted_model,te){
  c <- as.numeric(Fitted_model$m$getPars()[1])
  c1 <- as.numeric(Fitted_model$m$getPars()[2])
  c2 <- as.numeric(Fitted_model$m$getPars()[3])
  c*te*(te-c1)*(c2-te)^(1/2)
}

# Plot simulations
plot_mod <- function(df_realdata, Fitted_model, type_mod){
  # Function to compute the plot with real data and model fitted
  # Input: . df_realdata: real data from laboratory data frame with column temp and variable
  #        . Fitted_model: model output from nls function.
  #        . type_mod: type model. Options: lin, quad, quadN, briere
  # Output: plot with real data and model
  
  # Vec of temperatures
  vec <- seq(0,45,0.001)
  if( type_mod == "lin"){
    df_out_mod <- data.frame(temp = vec,
                             var = sapply(vec,
                                          function(temp) mod_lin(Fitted_model,temp)))
  }else if(type_mod == "quad"){
    df_out_mod <- data.frame(temp = vec,
                             var = sapply(vec,
                                          function(temp) mod_quad(Fitted_model,temp)))
  }else if(type_mod == "quadN"){
    df_out_mod <- data.frame(temp = vec,
                             var = sapply(vec,
                                          function(temp) mod_quadN(Fitted_model,temp)))
  }else if(type_mod == "briere"){
    df_out_mod <- data.frame(temp = vec,
                             var = sapply(vec,
                                          function(temp) mod_Briere(Fitted_model,temp)))
  }
  
  # Plot
  colnames(df_realdata) <- c("temp", "var")
  plot <- ggplot() + geom_point(data= df_realdata, aes(temp, var), color = "red") +
    geom_line(data=df_out_mod, aes(temp, var), color = "black")
  return(list(df_out_mod,plot))
}

# Plot to draw model with ribbon confidence intervals parameters and data
Plot_ribbon <- function(df_realdata, Fitted_model, type_mod){
  # Function to compute the plot with real data and model fitted with grey ribbon
  # Input: . df_realdata: real data from laboratory data frame with column temp and variable
  #        . Fitted_model: model output from nls function.
  #        . type_mod: type model. Options: lin, quad, quadN, briere
  # Output: plot with real data and model with ribbon
  
  # Function to compute model
  mod <- function(Fitted_model,te, type_mod){
    c <- as.numeric(Fitted_model$m$getPars()[1])
    c1 <- as.numeric(Fitted_model$m$getPars()[2])
    if(type_mod == "lin"){
      out <- c*te+c1
    }else if(type_mod == "quadN"){
      c2 <- as.numeric(Fitted_model$m$getPars()[3])
      out <- c*te^2+c1*te+c2
    }else if(type_mod == "quad"){
      c2 <- as.numeric(Fitted_model$m$getPars()[3])
      out <- -c*(te-c1)*(te-c2)
    }else if(type_mod == "briere"){
      c2 <- as.numeric(Fitted_model$m$getPars()[3])
      out <- c*te*(te-c1)*(c2-te)^(1/2)
    }
    
    return(out)
  }
  
  # Function to compute model with fitted parameters Mean - sd
  mod_min <- function(Fitted_model,te, type_mod){
    c <- as.numeric(Fitted_model$m$getPars()[1]) - 
      summary(Fitted_model)$coefficients[1,2]
    c1 <- as.numeric(Fitted_model$m$getPars()[2])-
      summary(Fitted_model)$coefficients[2,2]
    if(type_mod == "lin"){
      out <- c*te+c1
    }else if(type_mod == "quadN"){
      c2 <- as.numeric(Fitted_model$m$getPars()[3])-
        summary(Fitted_model)$coefficients[3,2]
      out <- c*te^2+c1*te+c2
    }else if(type_mod == "quad"){
      c2 <- as.numeric(Fitted_model$m$getPars()[3])-
        summary(Fitted_model)$coefficients[3,2]
      out <- -c*(te-c1)*(te-c2)
    }else if(type_mod == "briere"){
      c2 <- as.numeric(Fitted_model$m$getPars()[3])-
        summary(Fitted_model)$coefficients[3,2]
      out <- c*te*(te-c1)*(c2-te)^(1/2)
    }
    return(out)
  }
  
  # Function to compute model with fitted parameters Mean + sd
  mod_max <- function(Fitted_model,te, type_mod){
    c <- as.numeric(Fitted_model$m$getPars()[1]) + 
      summary(Fitted_model)$coefficients[1,2]
    c1 <- as.numeric(Fitted_model$m$getPars()[2])+
      summary(Fitted_model)$coefficients[2,2]
    if(type_mod == "lin"){
      out <- c*te+c1
    }else if(type_mod == "quadN"){
      c2 <- as.numeric(Fitted_model$m$getPars()[3])+
        summary(Fitted_model)$coefficients[3,2]
      out <- c*te^2+c1*te+c2
    }else if(type_mod == "quad"){
      c2 <- as.numeric(Fitted_model$m$getPars()[3])+
        summary(Fitted_model)$coefficients[3,2]
      out <- -c*(te-c1)*(te-c2)
    }else if(type_mod == "briere"){
      c2 <- as.numeric(Fitted_model$m$getPars()[3])+
        summary(Fitted_model)$coefficients[3,2]
      out <- c*te*(te-c1)*(c2-te)^(1/2)
    }
    return(out)
  }
  
  # Run simulation for parameters mean - SD parameters
  vec <- seq(0,45,0.001)
  df_out_min <- data.frame(temp = vec,
                            var = sapply(vec,
                                         function(temp) mod_min(Fitted_model,
                                                                       temp, type_mod)))
  df_out_min$group <- "min"
  
  # Run simulation for parameters mean - SD parameters
  vec <- seq(0,45,0.001)
  df_out_max <- data.frame(temp = vec,
                           var = sapply(vec,
                                        function(temp) mod_max(Fitted_model,
                                                                      temp, type_mod)))
  df_out_max$group <- "max"

  # Run simulation for parameters mean 
  vec <- seq(0,45,0.001)
  df_out_mean <- data.frame(temp = vec,
                           var = sapply(vec,
                                        function(temp) mod(Fitted_model,
                                                               temp, type_mod)))
  df_out_mean$group <- "mean"
  
  # Reshape to plot
  df_out <- rbind(df_out_max,df_out_min,df_out_mean)
  df_out <- reshape(df_out, idvar ="temp" ,
                             timevar = "group", direction = "wide")
  
  # Transform name columns to fit
  colnames(df_realdata) <- c("temp", "var")
  
  # Colors
  line_col <- "#0072B5"
  grey_col <- "#E5E5E5"
  
  # Plot with ribbon
  plot_ribbon <- ggplot(df_out, aes(x=temp,y=var.min)) +
    geom_line(aes(y=var.min), color = grey_col, size = 0.7, alpha=0.5) +
    geom_line(aes(y=var.max), color = grey_col, size = 0.7, alpha=0.5) +
    geom_line(aes(y=var.mean), color = line_col, size = 0.7) +
    geom_ribbon(data = df_out,aes(ymin=var.min,
                                           ymax=var.max),
                fill=grey_col, alpha=0.5) +
    geom_point(data = df_realdata,aes(x = temp,y = var),
               size = 1.1, color = "black") +
    guides( color =FALSE, alpha = FALSE) +
    theme_bw() 
  
  return(plot_ribbon)  
  
}

## Adult mosquito life span --------------------------------------------------
# Linear model
Fitting_lf_lin <- nls(lf ~ cont*temp + cont1,
                      data = lf_gono_df_rand,
                      start = list(cont = 0.001, cont1 = 0.0))
summary(Fitting_lf_lin) # Summary of the fitting

# Plot with model and the results
plot_mod(lf_gono_df_rand[,c("temp", "lf")], Fitting_lf_lin, "lin")[[2]]
  
# Quadratic model
Fitting_lf_quad <- nls(lf ~ -c*(temp-c1)*(temp-c2),
                           data = lf_gono_df_rand,
                           start = list(c = 0.001, c1 = 12, c2 = 30))

summary(Fitting_lf_quad)

# Plot with model and the results
plot_mod(lf_gono_df_rand[,c("temp", "lf")], Fitting_lf_quad, "quad")[[2]] + ylim(c(0,70))

# Quadratic Normal model
Fitting_lf_quadN <- nls(lf ~ cont*temp^2+ cont1*temp+cont2,
                       data = lf_gono_df_rand,
                       start = list(cont = 0.001, cont1 = 12, cont2 = 30))

summary(Fitting_lf_quadN)

# Plot with model and the results
plot_mod(lf_gono_df_rand[,c("temp", "lf")], Fitting_lf_quadN, "quadN")[[2]]

# Briere model
Fitting_lf_briere <- nls(lf ~ c*temp*(temp-c1)*(c2-temp)^(1/2),
                       data = lf_gono_df_rand, algorithm = "port",
                       start = list(c = 0.003, c1 = 14, c2 = 40), 
                       lower=c(0.0001,0,33), upper = c(0.1,18,50))

summary(Fitting_lf_briere)

# Plot with model and the results
plot_mod(lf_gono_df_rand[,c("temp", "lf")], Fitting_lf_briere, "briere")[[2]]

# Compute the AIC value
AIC(Fitting_lf_lin,Fitting_lf_quad,Fitting_lf_quadN,Fitting_lf_briere)

# Optimal
df_opt <- plot_mod(lf_gono_df_rand[,c("temp", "lf")], Fitting_lf_briere, "briere")[[1]]
df_opt[!is.na(df_opt$var) & df_opt$var == max(df_opt$var,na.rm = TRUE),]

# Plot fitting with ribbon# PlotTRUE fitting with ribbon
plot_rib_lf <- Plot_ribbon(lf_gono_df_rand[,c("temp", "lf")], Fitting_lf_briere, "briere") 
plot_rib_lf <- plot_rib_lf + xlim(8,35) + ylim(0,80) + 
  ylab("Adult life span, lf") + xlab("Temperature")

## Length gonotrophic cycle --------------------------------------------------
# Linear model
lf_gono_df_rand$a <- 1/lf_gono_df_rand$gono 
Fitting_a_lin <- nls(a ~ cont*temp + cont1,
                      data = lf_gono_df_rand,
                      start = list(cont = 0.001, cont1 = 0.0))
summary(Fitting_a_lin) # Summary of the fitting

# Plot with model and the results
plot_mod(lf_gono_df_rand[,c("temp", "a")], Fitting_a_lin, "lin")[[2]]

# Quadratic Normal model
Fitting_a_quadN <- nls(a ~ cont*temp^2+ cont1*temp+cont2,
                        data = lf_gono_df_rand,
                        start = list(cont = 0.001, cont1 = 12, cont2 = 30))

summary(Fitting_a_quadN)

# Plot with model and the results
plot_mod(lf_gono_df_rand[,c("temp", "a")], Fitting_a_quadN, "quadN")[[2]]

# Quadratic model
Fitting_a_quad <- nls(a ~ -c*(temp-c1)*(temp-c2),
                      data = lf_gono_df_rand,
                      start = list(c = 0.001, c1 = 12, c2 = 30))

summary(Fitting_a_quad)

# Plot with model and the results
plot_mod(lf_gono_df_rand[,c("temp", "a")], Fitting_a_quad, "quad")[[2]]

# Compute the AIC value
AIC(Fitting_a_lin, Fitting_a_quadN, Fitting_a_quad)

# Optimal
df_opt <- plot_mod(lf_gono_df_rand[,c("temp", "a")], Fitting_a_quad, "quad")[[1]]
df_opt[!is.na(df_opt$var) & df_opt$var == max(df_opt$var,na.rm = TRUE),]

# Plot fitting with ribbon
plot_rib_a <- Plot_ribbon(lf_gono_df_rand[,c("temp", "a")], Fitting_a_quadN, "quadN") 
plot_rib_a <- plot_rib_a + xlim(8,35) + ylim(-0.5,1) + ylab("Biting rate, a") + xlab("Temperature")

## Larva development rate, delta_L --------------------------------------------------
# Linear model
hat_surv_rate_df_r_rand$delta_L <- (100-hat_surv_rate_df_r_rand$L)/100
Fitting_delta_L_lin <- nls(delta_L ~ cont*temp + cont1,
                     data = hat_surv_rate_df_r_rand,
                     start = list(cont = 0.001, cont1 = 0.0))
summary(Fitting_delta_L_lin) # Summary of the fitting

# Plot with model and the results
plot_mod(hat_surv_rate_df_r_rand[,c("temp", "delta_L")], Fitting_delta_L_lin, "lin")[[2]]

# Quadratic model
Fitting_delta_L_quad <- nls(delta_L~ -c*(temp-c1)*(temp-c2),
                      data = hat_surv_rate_df_r_rand,
                      start = list(c = -4.688e-04, c1 = 3, c2 = -7))

summary(Fitting_delta_L_quad)

# Plot with model and the results
plot_mod(hat_surv_rate_df_r_rand[,c("temp", "delta_L")], Fitting_delta_L_quad, "quad")[[2]]

# Quadratic Normal model
Fitting_delta_L_quadN <- nls(delta_L ~ cont*temp^2+ cont1*temp+cont2,
                       data = hat_surv_rate_df_r_rand,
                       start = list(cont = 0.001, cont1 = 12, cont2 = 30))

summary(Fitting_delta_L_quadN)

# Plot with model and the results
plot_mod(hat_surv_rate_df_r_rand[,c("temp", "delta_L")], Fitting_delta_L_quadN, "quadN")[[2]]

# Briere model
Fitting_delta_L_briere <- nls(delta_L ~ c*temp*(temp-c1)*(c2-temp)^(1/2),
                        data = hat_surv_rate_df_r_rand, algorithm = "port",
                        start = list(c = 0.003, c1 = 5, c2 = 35), 
                        lower=c(0.0001,4,33), upper = c(0.1,18,50))

summary(Fitting_delta_L_briere)

# Plot with model and the results
plot_mod(hat_surv_rate_df_r_rand[,c("temp", "delta_L")], Fitting_delta_L_briere, "briere")[[2]]

# Compute the AIC value
AIC(Fitting_delta_L_lin,Fitting_delta_L_quadN,Fitting_delta_L_quad,Fitting_delta_L_briere)

# Optimal
df_opt <- plot_mod(hat_surv_rate_df_r_rand[,c("temp", "delta_L")], Fitting_delta_L_briere, "briere")[[1]]
df_opt[!is.na(df_opt$var) & df_opt$var == min(df_opt$var,na.rm = TRUE),]

# Plot fitting with ribbon
plot_rib_delta_L <- Plot_ribbon(hat_surv_rate_df_r_rand[,c("temp", "delta_L")],
                                Fitting_delta_L_quadN, "quadN") 
plot_rib_delta_L <- plot_rib_delta_L + 
  ylab("Larva development rate, delta_L") + xlab("Temperature")

## Probability from larva to adult, P_LA --------------------------------------------------
# Linear model
hat_surv_rate_df_r_rand$L <- hat_surv_rate_df_r_rand$L/100
Fitting_L_lin <- nls(L ~ cont*temp + cont1,
                      data = hat_surv_rate_df_r_rand,
                      start = list(cont = 0.001, cont1 = 0.0))
summary(Fitting_L_lin) # Summary of the fitting

# Plot with model and the results
plot_mod(hat_surv_rate_df_r_rand[,c("temp", "L")], Fitting_L_lin, "lin")[[2]]

# Quadratic model
Fitting_L_quad <- nls(L ~ -c*(temp-c1)*(temp-c2),
                       data = hat_surv_rate_df_r_rand,
                       start = list(c = 0.001, c1 = 12, c2 = 30))

summary(Fitting_L_quad)

# Plot with model and the results
plot_mod(hat_surv_rate_df_r_rand[,c("temp", "L")], Fitting_L_quad, "quad")[[2]]

# Quadratic Normal model
Fitting_L_quadN <- nls(L ~ cont*temp^2+ cont1*temp+cont2,
                        data = hat_surv_rate_df_r_rand,
                        start = list(cont = 0.001, cont1 = 12, cont2 = 30))

summary(Fitting_L_quadN)

# Plot with model and the results
plot_mod(hat_surv_rate_df_r_rand[,c("temp", "L")], Fitting_L_quadN, "quadN")[[2]]

# Briere model
Fitting_L_briere <- nls(L ~ c*temp*(temp-c1)*(c2-temp)^(1/2),
                         data = hat_surv_rate_df_r_rand, algorithm = "port",
                         start = list(c = 0.003, c1 = 5, c2 = 35), 
                         lower=c(0.0001,4,33), upper = c(0.1,18,50))

summary(Fitting_L_briere)

# Plot with model and the results
plot_mod(hat_surv_rate_df_r_rand[,c("temp", "L")], Fitting_L_briere, "briere")[[2]]

# Compute the AIC value
AIC(Fitting_L_lin,Fitting_L_quad,Fitting_L_quadN,Fitting_L_briere)

# Optimal
df_opt <- plot_mod(hat_surv_rate_df_r_rand[,c("temp", "L")], Fitting_L_quad, "quad")[[1]]
df_opt[df_opt$var == max(df_opt$var,na.rm = TRUE),]

# Plot fitting with ribbon
plot_rib_P_LA <- Plot_ribbon(hat_surv_rate_df_r_rand[,c("temp", "L")], Fitting_L_quad, "quad") 
plot_rib_P_LA <- plot_rib_P_LA + xlim(8,35) + ylim(0,110) + ylab("Probability from Larva to Adult, P_LA") + xlab("Temperature")

## Egg moratality rate, delta_E --------------------------------------------------
# Linear model
hat_surv_rate_df_r_rand$delta_E <- (100 - hat_surv_rate_df_r_rand$E)/100
Fitting_delta_E_lin <- nls(delta_E ~ cont*temp + cont1,
                     data = hat_surv_rate_df_r_rand,
                     start = list(cont = 0.001, cont1 = 0.0))
summary(Fitting_delta_E_lin) # Summary of the fitting

# Plot with model and the results
plot_mod(hat_surv_rate_df_r_rand[,c("temp", "delta_E")], Fitting_delta_E_lin, "lin")[[2]]

# # Quadratic model
# Fitting_delta_E_quad <- nls(delta_E ~ -c*(temp-c1)*(temp-c2),
#                       data = hat_surv_rate_df_r_rand,
#                       start = list(c = 0.000001, c1 = 8, c2 = 30))
# 
# summary(Fitting_delta_E_quad)
# 
# # Plot with model and the results
# plot_mod(hat_surv_rate_df_r_rand[,c("temp", "delta_E")], Fitting_delta_E_quad, "quad")[[2]] 

# Quadratic Normal model
Fitting_delta_E_quadN <- nls(delta_E ~ cont*temp^2+ cont1*temp+cont2,
                       data = hat_surv_rate_df_r_rand,
                       start = list(cont = 0.001, cont1 = 12, cont2 = 30))

summary(Fitting_delta_E_quadN)

# Plot with model and the results
plot_mod(hat_surv_rate_df_r_rand[,c("temp", "delta_E")], Fitting_delta_E_quadN, "quadN")[[2]]

# Briere model
Fitting_delta_E_briere <- nls(delta_E ~ c*temp*(temp-c1)*(c2-temp)^(1/2),
                        data = hat_surv_rate_df_r_rand, algorithm = "port",
                        start = list(c = 0.000003, c1 = 5, c2 = 35), 
                        lower=c(0.0000000001,4,33), upper = c(0.1,18,50))

summary(Fitting_delta_E_briere)

# Plot with model and the results
plot_mod(hat_surv_rate_df_r_rand[,c("temp", "delta_E")], Fitting_delta_E_briere, "briere")[[2]]

# Compute the AIC value
AIC(Fitting_delta_E_lin,Fitting_delta_E_quadN,Fitting_delta_E_briere)

# Optimal
df_opt <- plot_mod(hat_surv_rate_df_r_rand[,c("temp", "delta_E")], Fitting_delta_E_quadN, "quadN")[[1]]
df_opt[df_opt$var == max(df_opt$var,na.rm = TRUE),]

# Plot fitting with ribbon
plot_rib_delta_E <- Plot_ribbon(hat_surv_rate_df_r_rand[,c("temp", "delta_E")],
                                Fitting_delta_E_quadN, "quadN") 
plot_rib_delta_E <- plot_rib_delta_E + xlim(8,35) + ylim(0,1) + 
  ylab("Egg mortality rate, delta_E") + xlab("Temperature")

## Egg moratality rate, delta_E --------------------------------------------------
# Linear model
hat_surv_rate_df_r_rand$delta_E <- (100 - hat_surv_rate_df_r_rand$E)/100
Fitting_delta_E_lin <- nls(delta_E ~ cont*temp + cont1,
                           data = hat_surv_rate_df_r_rand,
                           start = list(cont = 0.001, cont1 = 0.0))
summary(Fitting_delta_E_lin) # Summary of the fitting

# Plot with model and the results
plot_mod(hat_surv_rate_df_r_rand[,c("temp", "delta_E")], Fitting_delta_E_lin, "lin")[[2]]

# Quadratic model
Fitting_delta_E_quad <- nls(delta_E ~ -c*(temp-c1)*(temp-c2),
                            data = hat_surv_rate_df_r_rand,
                            start = list(c = 0.000001, c1 = 8, c2 = 30))

summary(Fitting_delta_E_quad)

# Plot with model and the results
plot_mod(hat_surv_rate_df_r_rand[,c("temp", "delta_E")], Fitting_delta_E_quad, "quad")[[2]] 

# Quadratic Normal model
Fitting_delta_E_quadN <- nls(delta_E ~ cont*temp^2+ cont1*temp+cont2,
                             data = hat_surv_rate_df_r_rand,
                             start = list(cont = 0.001, cont1 = 12, cont2 = 30))

summary(Fitting_delta_E_quadN)

# Plot with model and the results
plot_mod(hat_surv_rate_df_r_rand[,c("temp", "delta_E")], Fitting_delta_E_quadN, "quadN")[[2]]

# Briere model
Fitting_delta_E_briere <- nls(delta_E ~ c*temp*(temp-c1)*(c2-temp)^(1/2),
                              data = hat_surv_rate_df_r_rand, algorithm = "port",
                              start = list(c = 0.000003, c1 = 5, c2 = 35), 
                              lower=c(0.0000000001,4,33), upper = c(0.1,18,50))

summary(Fitting_delta_E_briere)

# Plot with model and the results
plot_mod(hat_surv_rate_df_r_rand[,c("temp", "delta_E")], Fitting_delta_E_briere, "briere")[[2]]

# Compute the AIC value
AIC(Fitting_delta_E_lin,Fitting_delta_E_quadN,Fitting_delta_E_briere)

# Optimal
df_opt <- plot_mod(hat_surv_rate_df_r_rand[,c("temp", "delta_E")], Fitting_delta_E_quadN, "quadN")[[1]]
df_opt[df_opt$var == min(df_opt$var,na.rm = TRUE),]

# Plot fitting with ribbon
plot_rib_delta_E <- Plot_ribbon(hat_surv_rate_df_r_rand[,c("temp", "delta_E")],
                                Fitting_delta_E_quadN, "quadN") 
plot_rib_delta_E <- plot_rib_delta_E + xlim(8,35) + ylim(0,1) + ylab("Egg mortality rate, delta_E") + xlab("Temperature")

## Egg development rate, d_E --------------------------------------------------
# Linear model
develop_time_df_rand$dE <- 1/develop_time_df_rand$E
Fitting_dE_lin <- nls(dE ~ cont*temp + cont1,
                           data = develop_time_df_rand,
                           start = list(cont = 0.001, cont1 = 0.0))
summary(Fitting_dE_lin) # Summary of the fitting

# Plot with model and the results
plot_mod(develop_time_df_rand[,c("temp", "dE")], Fitting_dE_lin, "lin")[[2]]

# Quadratic model
Fitting_dE_quad <- nls(dE ~ -c*(temp-c1)*(temp-c2),
                            data = develop_time_df_rand,
                            start = list(c = 0.001, c1 = 12, c2 = 30))

summary(Fitting_dE_quad)

# Plot with model and the results
plot_mod(develop_time_df_rand[,c("temp", "dE")], Fitting_dE_quad, "quad")[[2]]

# Quadratic Normal model
Fitting_dE_quadN <- nls(dE ~ cont*temp^2+ cont1*temp+cont2,
                             data = develop_time_df_rand,
                             start = list(cont = 0.001, cont1 = 12, cont2 = 30))

summary(Fitting_dE_quadN)

# Plot with model and the results
plot_mod(develop_time_df_rand[,c("temp", "dE")], Fitting_dE_quadN, "quadN")[[2]]

# Briere model
Fitting_dE_briere <- nls(dE ~ c*temp*(temp-c1)*(c2-temp)^(1/2),
                              data = develop_time_df_rand, algorithm = "port",
                              start = list(c = 0.003, c1 = 5, c2 = 35), 
                              lower=c(0.0001,4,33), upper = c(0.1,18,50))

summary(Fitting_dE_briere)

# Plot with model and the results
plot_mod(develop_time_df_rand[,c("temp", "dE")], Fitting_dE_briere, "briere")[[2]]

# Compute the AIC value
AIC(Fitting_dE_lin,Fitting_dE_quad,Fitting_dE_quadN,Fitting_dE_briere)

# Optimal
df_opt <- plot_mod(develop_time_df_rand[,c("temp", "dE")], Fitting_dE_quad, "quad")[[1]]
df_opt[df_opt$var == max(df_opt$var,na.rm = TRUE),]

# Plot fitting with ribbon
plot_rib_dE <- Plot_ribbon(develop_time_df_rand[,c("temp", "dE")], Fitting_dE_quad, "quad") 
plot_rib_dE <- plot_rib_dE + xlim(8,35) + ylim(0,1.1) + ylab("Egg development rate, dE") + xlab("Temperature")

## Larvae development rate, d_L --------------------------------------------------
# Linear model
develop_time_df_rand$dL <- 1/develop_time_df_rand$L

# Remove Inf
develop_time_df_rand <- develop_time_df_rand[!is.infinite(develop_time_df_rand$dL), ] 
Fitting_dL_lin <- nls(dL ~ cont*temp + cont1,
                      data = develop_time_df_rand,
                      start = list(cont = 0.001, cont1 = 0.0))
summary(Fitting_dL_lin) # Summary of the fitting

# Plot with model and the results
plot_mod(develop_time_df_rand[,c("temp", "dL")], Fitting_dL_lin, "lin")[[2]]

# Quadratic model
Fitting_dL_quad <- nls(dL ~ -c*(temp-c1)*(temp-c2),
                       data = develop_time_df_rand,
                       start = list(c = 0.001, c1 = 12, c2 = 30))

summary(Fitting_dL_quad)

# Plot with model and the results
plot_mod(develop_time_df_rand[,c("temp", "dL")], Fitting_dL_quad, "quad")[[2]] 

# Quadratic Normal model
Fitting_dL_quadN <- nls(dL ~ cont*temp^2+ cont1*temp+cont2,
                        data = develop_time_df_rand,
                        start = list(cont = 0.001, cont1 = 12, cont2 = 30))

summary(Fitting_dL_quadN)

# Plot with model and the results
plot_mod(develop_time_df_rand[,c("temp", "dL")], Fitting_dL_quadN, "quadN")[[2]]

# Briere model
Fitting_dL_briere <- nls(dL ~ c*temp*(temp-c1)*(c2-temp)^(1/2),
                         data = develop_time_df_rand, algorithm = "port",
                         start = list(c = 0.00004, c1 = 5, c2 = 35), 
                         lower=c(0.000001,4,33), upper = c(0.1,18,50))

summary(Fitting_dL_briere)

# Plot with model and the results
plot_mod(develop_time_df_rand[,c("temp", "dL")], Fitting_dL_briere, "briere")[[2]]

# Compute the AIC value
AIC(Fitting_dL_lin,Fitting_dL_quad,Fitting_dL_quadN,Fitting_dL_briere)

# Optimal
df_opt <- plot_mod(develop_time_df_rand[,c("temp", "dL")], Fitting_dL_quad, "quad")[[1]]
df_opt[df_opt$var == max(df_opt$var,na.rm = TRUE),]

# Plot fitting with ribbon
plot_rib_dL <- Plot_ribbon(develop_time_df_rand[,c("temp", "dL")], Fitting_dL_quad, "quad") 
plot_rib_dL <- plot_rib_dL + xlim(8,40) + ylim(0,0.1) +
  ylab("Larvae development rate, dL") + xlab("Temperature")

# Join all plots
library(ggpubr)
sizelet = 14
library(latex2exp)
ggarrange(plot_rib_lf + xlim(c(5,35)) +
            theme(text = element_text(size = sizelet)),
          plot_rib_a + xlim(c(5,35)) + ylim(c(-0.4,0.47))+ 
            theme(text = element_text(size = sizelet)),
          plot_rib_dE + xlim(c(5,35)) +
            ylab(TeX("Egg development rate, d$_E$")) +
            theme(text = element_text(size = sizelet)),
          plot_rib_delta_E + xlim(c(5,35)) +
            ylab(TeX("Egg mortality rate, \\delta$_E$")) + ylim(c(0,1)) +
            theme(text = element_text(size = sizelet)),
          plot_rib_dL + xlim(c(5,35)) +
            ylab(TeX("Larva development rate, d$_L$")) + 
            theme(text = element_text(size = sizelet)),
          plot_rib_delta_L + xlim(c(5,35)) + ylim(c(-0.1,1)) +
            ylab(TeX("Larva mortality rate, \\delta$_L$")) +
            theme(text = element_text(size = sizelet)))

# Save the plots
ggsave("/home/marta/Documentos/PHD/2025/Aedes_land/Plots/thermal_responses_kor.pdf",
       width = 11.5, height = 7.5)

ggsave("/home/marta/Documentos/PHD/2025/Aedes_land/Plots/thermal_responses_kor.png",
       width = 11.5, height = 7.5, dpi=300)
