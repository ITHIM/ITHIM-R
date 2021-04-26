# Load libraries
library(tidyverse)
library(mice)

# Set filepath
file_path <- file.path('data/local/cape_town/injuries_cape_town.csv')

# Read
whw <- read_csv(file_path)
unique(whw$year)
# Remove unused columns
whw$X1 <- whw$year <- NULL

# Rename column
whw <- whw %>% rename(cas_gender = cas_sex)

# Add weight column
#whw$weight <- 4
whw$weight <- 5 # Weight is 5 because there's data from 2012 to 2016

# Read lookup table
smodes <- read_csv('data/global/modes/standardized_modes.csv')
# Separate rows 
smodes <- smodes %>% separate_rows(original, sep = ';')
# Trim
smodes <- smodes %>% 
  mutate(across(where(is.character), str_trim))

# Multiple imputation using MICE
# https://stats.idre.ucla.edu/r/faq/how-do-i-perform-multiple-imputation-using-predictive-mean-matching-in-r/
whw2 <- whw %>% mutate(
  cas_mode = factor(ifelse(cas_mode == "unknown", NA, cas_mode)),
  strike_mode = factor(ifelse(strike_mode == "unknown", NA, strike_mode))
)

table(whw$cas_mode, useNA = "always")
table(whw2$cas_mode, useNA = "always")
table(whw$strike_mode, useNA = "always")
table(whw2$strike_mode, useNA = "always")

# There are 2 age missing, 47 cas_mode and 141 strike mode missing.
md.pattern(whw2)

# Out of the 47 and 151 cas and strike mode, in only 3 rows both modes are missing
md.pairs(whw2)

# imputation
imp1 <- mice(whw2, m = 5, seed = 12345)
imp1
# Adding imputed rows to Cape Town's dataset. The original columns (with missing
# values) are kept in the dataset with sufix "_original". And the first run of
# imputation is saved in cas_mode and strike_mode. From second to fifth
# imputation are also saved with the sufix "_2nd", to "5th".
whw3 <- whw2 %>% 
  rename(cas_mode_original = cas_mode,
         strike_mode_original = strike_mode) %>% 
  bind_cols(complete(imp1) %>% select(cas_mode, strike_mode)) %>%
  bind_cols(complete(imp1, action = 2) %>% select(cas_mode, strike_mode) %>% 
              rename(cas_mode_2nd = cas_mode, strike_mode_2nd = strike_mode)) %>%
  bind_cols(complete(imp1, action = 3) %>% select(cas_mode, strike_mode) %>% 
              rename(cas_mode_3rd = cas_mode, strike_mode_3rd = strike_mode)) %>%   bind_cols(complete(imp1, action = 4) %>% select(cas_mode, strike_mode) %>% 
                                                                                                rename(cas_mode_4th = cas_mode, strike_mode_4th = strike_mode)) %>%
  bind_cols(complete(imp1, action = 5) %>% select(cas_mode, strike_mode) %>% 
              rename(cas_mode_5th = cas_mode, strike_mode_5th = strike_mode))

# Comparing imputations 
table(whw3$cas_mode_original, whw3$cas_mode, useNA = "always")
table(whw3$strike_mode_original, whw3$strike_mode, useNA = "always")
table(whw3$strike_mode_original, whw3$strike_mode_2nd, useNA = "always")

# Recode cas_mode and strike_mode
whw4 <- whw3 %>% 
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
table(whw3$cas_mode, useNA = "always")
table(whw4$cas_mode, useNA = "always")
table(whw3$strike_mode, useNA = "always")
table(whw4$strike_mode, useNA = "always")

# Check if all modes are correctly recoded
unique(whw4$cas_mode) %in% smodes$exhaustive_list
unique(whw4$cas_mode_2nd) %in% smodes$exhaustive_list
unique(whw4$cas_mode_3rd) %in% smodes$exhaustive_list
unique(whw4$cas_mode_4th) %in% smodes$exhaustive_list
unique(whw4$cas_mode_5th) %in% smodes$exhaustive_list
unique(whw4$strike_mode) %in% smodes$exhaustive_list
unique(whw4$strike_mode_2nd) %in% smodes$exhaustive_list
unique(whw4$strike_mode_3rd) %in% smodes$exhaustive_list
unique(whw4$strike_mode_4th) %in% smodes$exhaustive_list
unique(whw4$strike_mode_5th) %in% smodes$exhaustive_list

# Save file
injury_file <- 'injuries_cape_town.csv'
write.csv(whw4,paste0('inst/extdata/local/cape_town/',injury_file))


