# MICE model to replace unknown modes (and age and gender) by known modes using the existing distribution between modes and age and gedner


library(dplyr)
library(mice)
library(tidyverse)


cities <- c('antofagasta','arica','cali','coquimbo_laserena','gran_valparaiso','iquique_altohospicio',
            'medellin','kisumu','nairobi','puerto_montt','san_antonio','valdivia')



path <- 'inst/extdata/local/'

for (city in cities){
  
  injury_df <- read.csv(paste0(path,city,'/injuries_',city,'.csv'))
  
  # save copy
  write.csv(injury_df, paste0(path,city,'/injuries_',city,'_with_unknowns.csv'), row.names = F)
  
  # Transforming "unknown" entries and invalid ages to  NA
  age_range <- seq(0,100)
  
  if ('cas_sex' %in% colnames(injury_df)){
    inj <- injury_df %>% 
      mutate(cas_age = ifelse(cas_age %in% age_range, cas_age, NA),
             cas_gender = factor(ifelse(cas_sex %in% c("female", "male", 'Male','Female'), 
                                        cas_sex, NA)),
             cas_mode = factor(ifelse(cas_mode == "unknown", NA, cas_mode)),
             strike_mode = factor(ifelse(strike_mode %in% c("unknown"),
                                         NA, strike_mode)))
  }
  
  if ('cas_gender' %in% colnames(injury_df)){
    inj <- injury_df %>% 
      mutate(cas_age = ifelse(cas_age %in% age_range, cas_age, NA),
             cas_gender = factor(ifelse(cas_gender %in% c("female", "male", 'Male','Female'), 
                                        cas_gender, NA)),
             cas_mode = factor(ifelse(cas_mode == "unknown", NA, cas_mode)),
             strike_mode = factor(ifelse(strike_mode %in% c("unknown"),
                                         NA, strike_mode)))
  }
  
  
  
  # Imputation using mice. 
  if('year' %in% colnames(inj)){
    imp1 <- mice(inj[,c("year","cas_age","cas_gender","cas_mode","strike_mode")], 
                 m = 1, seed = 12345)
  }

  if ('weight' %in% colnames(inj)){
    imp1 <- mice(inj[,c("weight","cas_age","cas_gender","cas_mode","strike_mode")], 
                 m = 1, seed = 12345)
  }
  #imp1
  
  # Adding imputed rows to dataset. The original columns (with missing
  # values) are kept in the dataset with sufix "_original". And the first run of
  # imputation is saved in cas_mode and strike_mode. 
  
  inj2 <- inj %>% 
    rename(cas_age_original = cas_age,
           cas_gender_original = cas_gender, 
           cas_mode_original = cas_mode, 
           strike_mode_original = strike_mode) %>% 
    bind_cols(complete(imp1) %>% 
                dplyr::select(cas_age, cas_gender, cas_mode, strike_mode))

  
  # output imputed file
  write.csv(inj2,paste0(path,city,'/injuries_',city,'.csv'), row.names = F)
  
}


