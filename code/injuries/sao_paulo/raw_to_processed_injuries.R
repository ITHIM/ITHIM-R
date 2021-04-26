# Read library
require(tidyverse)
library(mice)

#Read raw sp injuries
rd <- read_csv("data/local/sao_paulo/sao_paulo_processed_2009_2013.csv")

# Convert to the required structure
# Make sure that we have the all required variables
# Make sure modes are standardized
# Rob can you have a look please?

# 1. extract year
rd$year <- as.numeric(sapply(rd$accref,function(x)paste0('20',substr(x,4,5))))

# 2. rename columns
names(rd)[3:5] <- c('cas_mode','cas_gender','cas_age')

# 3. count NA
## casualty modes not modelled: others, pickup, minibus, cart. If we add this processing to ITHIM-R, we can add on missing fatalities as a constant?
nrow(subset(rd,cas_mode%in%c('others', 'pickup', 'minibus', 'cart')|is.na(cas_mode)|is.na(cas_gender)|is.na(cas_age))) # total missing: 130/5948=2.2%
nrow(subset(rd,cas_mode%in%c('others', 'pickup', 'minibus', 'cart')|is.na(cas_mode))) # total missing: 54/5948=0.9%
## might we want to capture minibus travel within bus travel?
## is pickup van travel?

# 4. rename entries to match input modes
rd$cas_mode[rd$cas_mode=='pedestrian'] <- 'Pedestrian'
rd$cas_mode[rd$cas_mode=='car'] <- 'Car'
rd$cas_mode[rd$cas_mode=='motorcycle'] <- 'Motorcycle'
rd$cas_mode[rd$cas_mode=='bicycle'] <- 'Bicycle'
rd$cas_mode[rd$cas_mode=='truck'] <- 'Truck'
rd$cas_mode[rd$cas_mode=='bus'] <- 'Bus'

rd$strike_mode[rd$strike_mode=='bus'] <- 'Bus_driver'
rd$strike_mode[rd$strike_mode=='motorcycle'] <- 'Motorcycle'
rd$strike_mode[rd$strike_mode=='car/taxi'] <- 'Car'
rd$strike_mode[rd$strike_mode=='heavy goods'] <- 'Truck'
rd$strike_mode[rd$strike_mode=='cyclist'] <- 'Bicycle'

rd$cas_gender[rd$cas_gender=='yes'] <- 'Male'
rd$cas_gender[rd$cas_gender=='no'] <- 'Female'


# 5. sample with replacement to replace NA for casualty age and casualty gender
rd$cas_gender[is.na(rd$cas_gender)] <- sample(rd$cas_gender[!is.na(rd$cas_gender)],sum(is.na(rd$cas_gender)),replace=T)
rd$cas_age[is.na(rd$cas_age)] <- sample(rd$cas_age[!is.na(rd$cas_age)],sum(is.na(rd$cas_age)),replace=T)

# Multiple imputation using MICE
# https://stats.idre.ucla.edu/r/faq/how-do-i-perform-multiple-imputation-using-predictive-mean-matching-in-r/

# Read lookup table
smodes <- read_csv('data/global/modes/standardized_modes.csv')
# Separate rows 
smodes <- smodes %>% separate_rows(original, sep = ';')
# Trim
smodes <- smodes %>% mutate(across(where(is.character), str_trim))

unique(rd$cas_mode)
unique(rd$strike_mode)
# Transforming "unknown" and "unspecified" to NA
rd2 <- rd %>% 
  mutate(cas_mode = factor(cas_mode),
         strike_mode = factor(strike_mode))

# There are 2 missing values in cas_mode and 333 in strike mode.
md.pattern(rd2)

# There are no rows where both cas_mode and strike_mode are missing
md.pairs(rd2)

# Imputation using mice. I imputed the dataset 5 times (this is why is multiple
# imputation). The idea is to make a sensitivity analysis in the results.
imp1 <- mice(rd2[,c("year", "cas_gender", "cas_age", "cas_mode", "strike_mode")],
             m = 5, seed = 12345)
imp1

# Adding imputed rows to SP's dataset. The original columns (with missing
# values) are kept in the dataset with sufix "_original". And the first run of
# imputation is saved in cas_mode and strike_mode. From second to fifth
# imputation are also saved with the sufix "_2nd", to "5th".
rd3 <- rd2 %>% 
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
table(rd3$cas_mode_original, rd3$cas_mode, useNA = "always")
table(rd3$strike_mode_original, rd3$strike_mode, useNA = "always")
table(rd3$strike_mode_original, rd3$strike_mode_2nd, useNA = "always")

# Recode cas_mode and strike_mode
rd4 <- rd3 %>% 
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
                                                        smodes$original)],
         weight = 3) # Weight is 3 because these injuries are from 2012-2014

# Comparing frequencies after recoding
table(rd3$cas_mode, useNA = "always")
table(rd4$cas_mode, useNA = "always")
table(rd3$strike_mode, useNA = "always")
table(rd4$strike_mode, useNA = "always")

# Check if all modes are correctly recoded
unique(rd4$cas_mode) %in% smodes$exhaustive_list
unique(rd4$cas_mode_2nd) %in% smodes$exhaustive_list
unique(rd4$cas_mode_3rd) %in% smodes$exhaustive_list
unique(rd4$cas_mode_4th) %in% smodes$exhaustive_list
unique(rd4$cas_mode_5th) %in% smodes$exhaustive_list
unique(rd4$strike_mode) %in% smodes$exhaustive_list
unique(rd4$strike_mode_2nd) %in% smodes$exhaustive_list
unique(rd4$strike_mode_3rd) %in% smodes$exhaustive_list
unique(rd4$strike_mode_4th) %in% smodes$exhaustive_list
unique(rd4$strike_mode_5th) %in% smodes$exhaustive_list

# 6. save
injury_file <- 'injuries_sao_paulo.csv'
write.csv(rd4,paste0('inst/extdata/local/sao_paulo/',injury_file))

