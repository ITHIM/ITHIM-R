#library(ithimr)
#setwd('ITHIM-R')
library(tidyverse)
library(mice)
##reading WHW file for Belo Horizonte metropolitan area for 2010-2015 period, WHW matric creates by Rahul Goel for WHW paper
whw_file <- '/injuries_whw_2010_2015.csv'
path <- file.path('data/local/belo_horizonte/')
whw <- read.csv(paste0(path,whw_file))
injuries <- data.frame(cas_mode = character(), strike_mode = character(),
                       weight = numeric(), stringsAsFactors = F)
number_of_years <- 6
for (i in 1:nrow(whw))
  for (j in 2:ncol(whw)) {
    count <- whw[i,j]
    if (count > 0) {
      weight <- number_of_years * ceiling(count)/count
      for (k in 1:ceiling(count)) { ## six years of data
        #print(c(k,count))
        injuries[nrow(injuries) + 1,] <- c(as.character(whw[i,1]),
                                           colnames(whw)[j], weight)
      }
    }
  }
## rahul recommends omitting pick up truck and 3wheeled strikers – both almost zero in the matrix.
injuries <- injuries[!injuries$cas_mode %in% c('3Wheeled','Pick-up truck/van'),]
injuries$strike_mode[injuries$strike_mode == 'X2.3.Wheeled'] <- 'motorcycle'
injuries$strike_mode[injuries$strike_mode == 'Car.pick.up.van'] <- 'car'
injuries$strike_mode[injuries$strike_mode == 'Trucks'] <- 'truck'
injuries$strike_mode[injuries$strike_mode == 'Non.collision'] <- 'NOV'
injuries$strike_mode[injuries$strike_mode == 'Pedal.cycle'] <- 'bicycle'
injuries$cas_mode[injuries$cas_mode == 'Pedal Cycle'] <- 'bicycle'
#injuries$cas_mode[injuries$cas_mode=='3Wheeled'] <- 'auto_rickshaw'
#injuries$cas_mode[injuries$cas_mode=='Pick-up truck/van'] <- 'minibus'
injuries$cas_mode[injuries$cas_mode == 'Heavy transport'] <- 'truck'
injuries

# Multiple imputation using MICE
# https://stats.idre.ucla.edu/r/faq/how-do-i-perform-multiple-imputation-using-predictive-mean-matching-in-r/

# Read lookup table
smodes <- read_csv('data/global/modes/standardized_modes.csv')
# Separate rows 
smodes <- smodes %>% separate_rows(original, sep = ';')
# Trim
smodes <- smodes %>% mutate(across(where(is.character), str_trim))

unique(injuries$cas_mode)
unique(injuries$strike_mode)

# Transforming "unknown" and "unspecified" to NA
injuries2 <- injuries %>% 
  mutate(cas_mode = factor(ifelse(cas_mode == "Unknown", NA, cas_mode)),
         strike_mode = factor(ifelse(strike_mode %in% c("Unknown", "unspecified"),
                                     NA, strike_mode)))

# table(injuries$cas_mode, useNA = "always")
# table(injuries2$cas_mode, useNA = "always")
# table(injuries$strike_mode, useNA = "always")
# table(injuries2$strike_mode, useNA = "always")

# There are 4 missing values in cas_mode and 1239 in strike mode.
md.pattern(injuries2)
# 1239/3210 # 38% of missing values in strike_mode (not good)

# In the 4 rows where cas_mode is missing, strike_mode is also missing
md.pairs(injuries2)

# Imputation using mice. I imputed the dataset 5 times (this is why is multiple
# imputation). The idea is to make a sensitivity analysis in the results.
imp1 <- mice(injuries2[,c("cas_mode", "strike_mode")], m = 5, seed = 12345)
imp1

# Adding imputed rows to Belo Horizonte's dataset. The original columns (with
# missing values) are kept in the dataset with sufix "_original". And the first 
# run of imputation is saved in cas_mode and strike_mode. From second to fifth
# imputation are also saved with the sufix "_2nd", to "5th".
injuries3 <- injuries2 %>% 
  rename(cas_mode_original = cas_mode,
         strike_mode_original = strike_mode) %>% 
  bind_cols(complete(imp1) %>% select(cas_mode, strike_mode)) %>%
  bind_cols(complete(imp1, action = 2) %>% select(cas_mode, strike_mode) %>% 
              rename(cas_mode_2nd = cas_mode, strike_mode_2nd = strike_mode)) %>%
  bind_cols(complete(imp1, action = 3) %>% select(cas_mode, strike_mode) %>% 
              rename(cas_mode_3rd = cas_mode, strike_mode_3rd = strike_mode)) %>%  
  bind_cols(complete(imp1, action = 4) %>% select(cas_mode, strike_mode) %>% 
              rename(cas_mode_4th = cas_mode, strike_mode_4th = strike_mode)) %>%
  bind_cols(complete(imp1, action = 5) %>% select(cas_mode, strike_mode) %>% 
              rename(cas_mode_5th = cas_mode, strike_mode_5th = strike_mode))

# Comparing imputations 
table(injuries3$cas_mode_original, injuries3$cas_mode, useNA = "always")
table(injuries3$cas_mode_original, injuries3$cas_mode_2nd, useNA = "always")
table(injuries3$strike_mode_original, injuries3$strike_mode, useNA = "always")
table(injuries3$strike_mode_original, injuries3$strike_mode_2nd, useNA = "always")

# Recode cas_mode and strike_mode
whw <- injuries3 %>% 
  mutate(cas_mode = smodes$exhaustive_list[match(tolower(cas_mode),
                                                 smodes$original)],
         strike_mode = smodes$exhaustive_list[match(tolower(strike_mode),
                                                    smodes$original)],
         cas_mode_2nd = smodes$exhaustive_list[match(tolower(cas_mode_2nd),
                                                     smodes$original)],
         strike_mode_2nd = smodes$exhaustive_list[match(tolower(strike_mode_2nd),
                                                        smodes$original)],
         cas_mode_3rd = smodes$exhaustive_list[match(tolower(cas_mode_3rd),
                                                     smodes$original)],
         strike_mode_3rd = smodes$exhaustive_list[match(tolower(strike_mode_3rd),
                                                        smodes$original)],
         cas_mode_4th = smodes$exhaustive_list[match(tolower(cas_mode_4th),
                                                     smodes$original)],
         strike_mode_4th = smodes$exhaustive_list[match(tolower(strike_mode_4th),
                                                        smodes$original)],
         cas_mode_5th = smodes$exhaustive_list[match(tolower(cas_mode_5th),
                                                     smodes$original)],
         strike_mode_5th = smodes$exhaustive_list[match(tolower(strike_mode_5th),
                                                        smodes$original)])

# Comparing frequencies after recoding
table(injuries3$cas_mode, useNA = "always")
table(whw$cas_mode, useNA = "always")
table(injuries3$strike_mode, useNA = "always")
table(whw$strike_mode, useNA = "always")

# Check if all modes are correctly recoded
unique(whw$cas_mode) %in% smodes$exhaustive_list
unique(whw$cas_mode_2nd) %in% smodes$exhaustive_list
unique(whw$cas_mode_3rd) %in% smodes$exhaustive_list
unique(whw$cas_mode_4th) %in% smodes$exhaustive_list
unique(whw$cas_mode_5th) %in% smodes$exhaustive_list
unique(whw$strike_mode) %in% smodes$exhaustive_list
unique(whw$strike_mode_2nd) %in% smodes$exhaustive_list
unique(whw$strike_mode_3rd) %in% smodes$exhaustive_list
unique(whw$strike_mode_4th) %in% smodes$exhaustive_list
unique(whw$strike_mode_5th) %in% smodes$exhaustive_list

injury_file <- 'injuries_belo_horizonte.csv'
write.csv(whw,paste0('inst/extdata/local/belo_horizonte/',injury_file))



