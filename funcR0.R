#------------------------FUNCTIONS---------------------------#
# Main functions 
Briere_func <- function(cte, tmin, tmax, temp){
  outp <- temp*cte*(temp - tmin)*(tmax - temp)^(1/2)
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}

Quad_func <- function(cte, tmin, tmax, temp){
  outp <- -cte*(temp - tmin)*(temp - tmax)
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}

Lin_func <- function(cte, cte1, temp){
  outp <- temp*cte + cte1
  if(outp < 0 | is.na(outp)){
    outp <- 0.00001
  }
  return(outp)
}


Quad <- function(cte, cte1,cte2, temp){
  outp <- cte*temp^2 + cte1*temp + cte2
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}

QuadN_func <- function(cte, c1, c2, temp){
  outp <- cte*temp^2 + c1*temp + c2
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}

### Hatching rate with only rain:
h_f_kor <- function(rain){
  evar= 0.34595
  eopt= 5.51843
  e0=0.00613
  
  # Unimodal function
  return((((1+e0)*exp(-evar*(rain-eopt)^2))/(exp(-evar*(rain - eopt)^2) + e0)))
}

h_f_kor_sig <- function(rain){
  a = 21.03838
  b = 1.33807
  
  # Sigmoidal function
  return(1/(1+exp(-a*(rain-b ))))
  
}

h_f_jap <- function(rain){
  evar= 0.6689132
  eopt= 5.2834025
  e0= 0.0000547
 
  # Unimodal function
  (((1+e0)*exp(-evar*(rain-eopt)^2))/(exp(-evar*(rain - eopt)^2) + e0))
  
  
}

h_f_jap_sig <- function(rain){
  a = 15.74484
  b = 1.35499
  
  # Sigmoidal function
  1/(1+exp(-a*(rain-b )))
  
}

#### -------------------------- Koreicus ------------------------- ####
## Thermal responses Aedes Albopictus from Marini 2019:
a_f_kor <- function(temp){Quad_func(0.0008094,12.19,35.55,temp)} # Biting rate
pLA_f_kor <- function(temp){Quad_func(0.000469,-48.78,44.21,temp)} # Survival probability Egg-Adult
lf_f_kor <- function(temp){Briere_func(0.035,0,33.02,temp)} # Adult life span
dE_f_kor <- function(temp){Quad_func(0.0001397,1.093,53.86,temp)} # Egg development rate
dL_f_kor <- function(temp){Briere_func(0.0000538,6.758,34.55,temp)} # Larva development rate
deltaE_f_kor <- function(temp){QuadN_func(0.001431,-0.0733,1.3469,temp)} # Egg mortality rate
deltaL_f_kor <- function(temp){QuadN_func(4.688e-04, 2.139e-03,-1.099e-02 ,temp)} # Egg mortality rate

f_val <- 60
# R0 function by temperature:
R0_func_kor <- function(Te, rain,lc){
  if(is.na(Te) | is.na(rain)){
    R0 <- NA
  }else{
    a <- a_f_kor(Te)
    f <- f_val
    deltaa <- lf_f_kor(Te)
    dE <- dE_f_kor(Te)
    probla <- pLA_f_kor(Te)
    h <- h_f_kor(rain)#h_f_kor_sig(rain)
    deltaE = deltaE_f_kor(Te)
    deltaL <- deltaL_f_kor(Te)
    dL <- dL_f_kor(Te)
    if((dL+deltaL) < 1.e-9 | (h*dE+deltaE) < 1.e-9){
      R0 <- 0
      # print("divided by 0")
    }else{
      R0 <- (lc*(f*a*deltaa)*(dL/(dL+deltaL))*(h*dE/(h*dE+deltaE)))^(1/3)
    }
  }
  return(R0)
}

# R0 function only temperature:
R0_func_kor_only_temp <- function(Te){
  if(is.na(Te)){
    R0 <- NA
  }else{
    a <- a_f_kor(Te)
    f <- f_val
    
    deltaa <- lf_f_kor(Te)
    dE <- dE_f_kor(Te)
    probla <- pLA_f_kor(Te)
    h <- 1#h_f_jap_kor(rain)
    deltaE = deltaE_f_kor(Te)
    deltaL <- deltaL_f_kor(Te)
    dL <- dL_f_kor(Te)
    if((dL+deltaL) < 1.e-9 | (h*dE+deltaE) < 1.e-9){
      R0 <- 0
    }else{
      R0 <- ((f*a*deltaa)*(dL/(dL+deltaL))*(h*dE/(h*dE+deltaE)))^(1/3)
    }
  }
  return(R0)
}


# R0 function by temperature and rainfall:
R0_func_kor_lc_hat <- function(Te, rain,lc){
  if(is.na(Te) | is.na(rain)){
    R0 <- NA
  }else{
    a <- a_f_kor(Te)
    f <- f_val
    
    deltaa <- lf_f_kor(Te)
    dE <- dE_f_kor(Te)
    probla <- pLA_f_kor(Te)
    h <- lc*h_f_kor(rain)
    deltaE = deltaE_f_kor(Te)
    deltaL <- deltaL_f_kor(Te)
    dL <- dL_f_kor(Te)
    if((dL+deltaL) < 1.e-9 | (h*dE+deltaE) < 1.e-9){
      R0 <- 0
      # print("divided by 0")
    }else{
      R0 <- ((f*a*deltaa)*(dL/(dL+deltaL))*(h*dE/(h*dE+deltaE)))^(1/3)
    }
  }
  return(R0)
}

# R0 function by temperature and rainfall:
R0_func_kor_nolc_sig <- function(Te, rain){
  if(is.na(Te) | is.na(rain)){
    R0 <- NA
  }else{
    a <- a_f_kor(Te)
    f <- f_val
    
    deltaa <- lf_f_kor(Te)
    dE <- dE_f_kor(Te)
    probla <- pLA_f_kor(Te)
    h <- h_f_kor(rain)#h_f_kor_sig(rain)
    deltaE = deltaE_f_kor(Te)
    deltaL <- deltaL_f_kor(Te)
    dL <- dL_f_kor(Te)
    if((dL+deltaL) < 1.e-9 | (h*dE+deltaE) < 1.e-9){
      R0 <- 0
      # print("divided by 0")
    }else{
      R0 <- ((f*a*deltaa)*(dL/(dL+deltaL))*(h*dE/(h*dE+deltaE)))^(1/3)
    }
  }
  return(R0)
}

#####----------------Japonicus-----------------####
dE_f_jap <- function(temp){Briere_func(0.0002859,6.360,35.53 ,temp)} # Mosquito Development Rate
dL_f_jap <- function(temp){Briere_func(7.000e-05,9.705e+00,3.410e+01,temp)} # Survival probability Egg-Adult
lf_f_jap <- function(temp){Lin_func(-2.5045,82.6525,temp)} # Adult life span
deltaL_f_jap <- function(temp){QuadN_func(0.0021476,-0.0806067 ,1.0332455,temp)} # Larvae mortality rate
delta_E_jap <- function(temp){QuadN_func(0.0030957,-0.1341438 ,1.4815436,temp)} # Egg mortality rate

# R0 function by temperature:
R0_func_jap <- function(Te, rain,lc){
  if(is.na(Te) | is.na(rain)){
    R0 <- NA
  }else{
    a <- 1/12 # Paper de Ae. koreicus Marini et al (2019) media de dias
    f <- f_val
    
    lf <- lf_f_jap(Te)
    deltaL <- deltaL_f_jap(Te)
    deltE = delta_E_jap(Te) 
    dE <- dE_f_jap(Te)
    dL <- dL_f_jap(Te)
    h <- h_f_jap(rain)#h_f_jap_sig(rain)
    if(dL == 0 | f == 0 | a == 0 | dE == 0 |  Te<0){
      R0 <- 0
    }else{
      R0 <- (lc*(f*a*lf)*(dL/(dL+(deltaL)))*(h*dE/(h*dE+deltE)))^(1/3)
      
    }
  }
  return(R0)
}


# R0 function by temperature:
R0_func_jap_only_temp <- function(Te){
  if(is.na(Te) ){
    R0 <- NA
  }else{
    a <- 1/12 # Paper de Ae. koreicus Marini et al (2019) media de dias
    f <- f_val
    
    lf <- lf_f_jap(Te)
    deltaL <- deltaL_f_jap(Te)
    deltE = delta_E_jap(Te) 
    dE <- dE_f_jap(Te)
    dL <- dL_f_jap(Te)
    h <- 1# h_f_jap_kor(rain)
    if(dL == 0 | f == 0 | a == 0 | dE == 0 |  Te<0){
      R0 <- 0
    }else{
      R0 <- ((f*a*lf)*(dL/(dL+(deltaL)))*(h*dE/(h*dE+deltE)))^(1/3)
      
    }
  }
  return(R0)
}

# R0 function only with cliamntic variables
R0_func_jap_lc_hat <- function(Te, rain,lc){
  if(is.na(Te) | is.na(rain)){
    R0 <- NA
  }else{
    a <- 1/12 # Paper de Ae. koreicus Marini et al (2019) media de dias
    f <- f_val
    
    lf <- lf_f_jap(Te)
    deltaL <- deltaL_f_jap(Te)
    deltE = delta_E_jap(Te) 
    dE <- dE_f_jap(Te)
    dL <- dL_f_jap(Te)
    h <- lc*h_f_jap(rain)
    if(dL == 0 | f == 0 | a == 0 | dE == 0 |  Te<0){
      R0 <- 0
    }else{
      R0 <- ((f*a*lf)*(dL/(dL+(deltaL)))*(h*dE/(h*dE+deltE)))^(1/3)
      
    }
  }
  return(R0)
}

# R0 function only with cliamntic variables
R0_func_jap_nolc_sig <- function(Te, rain){
  if(is.na(Te) | is.na(rain)){
    R0 <- NA
  }else{
    a <- 1/12 # Paper de Ae. koreicus Marini et al (2019) media de dias
    f <- f_val
    
    lf <- lf_f_jap(Te)
    deltaL <- deltaL_f_jap(Te)
    deltE = delta_E_jap(Te) 
    dE <- dE_f_jap(Te)
    dL <- dL_f_jap(Te)
    h <- h_f_jap_sig(rain)#h_f_jap_sig(rain)
    if(dL == 0 | f == 0 | a == 0 | dE == 0 |  Te<0){
      R0 <- 0
    }else{
      R0 <- ((f*a*lf)*(dL/(dL+(deltaL)))*(h*dE/(h*dE+deltE)))^(1/3)
      
    }
  }
  return(R0)
}

