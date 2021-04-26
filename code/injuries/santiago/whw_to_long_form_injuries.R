library(tidyverse)
library(mice)


whw_file <- 'santiago_fatality_matrix_4_years_2011_2014.csv'
path <- 'data/local/santiago/'
whw <- read.csv(paste0(path,whw_file))

injuries <- data.frame(cas_mode = character(), strike_mode = character(),
                       weight = numeric(), stringsAsFactors = F)
number_of_years <- 4
for (i in 1:nrow(whw))
  for (j in 2:ncol(whw)) {
    count <- whw[i,j]
    if (!is.na(count) & count > 0) {
      weight <- number_of_years * ceiling(count) / count
      for (k in 1:ceiling(count)) { 
        #print(c(k,count))
        injuries[nrow(injuries) + 1,] <- c(as.character(whw[i, 1]),
                                           colnames(whw)[j], weight)
      }
    }
  }
## rahul recommends omitting pick up truck and 3wheeled strikers – both almost zero in the matrix.
unique(injuries$cas_mode)
unique(injuries$strike_mode)
injuries <- injuries[!injuries$cas_mode %in% c('3Wheeled','Pick-up truck/van'),]

injuries$cas_mode[injuries$cas_mode == 'Cycle'] <- 'bicycle'
injuries$cas_mode[injuries$cas_mode == 'NMV'] <- 'Other'
injuries$cas_mode[injuries$cas_mode == 'MC'] <- 'motorcycle'

injuries$strike_mode[injuries$strike_mode == 'None'] <- 'NOV'
injuries$strike_mode[injuries$strike_mode == 'Cycle'] <- 'bicycle'
injuries$strike_mode[injuries$strike_mode == 'MC'] <- 'motorcycle'
injuries
# head(injuries)
# unique(injuries$cas_mode)
# unique(injuries$strike_mode)

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
# table(injuries$strike_mode, useNA = "always")
# table(injuries2$cas_mode, useNA = "always")
# table(injuries2$strike_mode, useNA = "always")

# There are 148 missing values in cas_mode and 30 in strike mode.
md.pattern(injuries2)

# In 3 rows both types are missing 
md.pairs(injuries2)

# Imputation using mice. I imputed the dataset 5 times (this is why is multiple
# imputation). The idea is to make a sensitivity analysis in the results.
imp1 <- mice(injuries2[,c("cas_mode", "strike_mode")], m = 5, seed = 12345)
imp1

# Adding imputed rows to Santiago's dataset. The original columns (with missing
# values) are kept in the dataset with sufix "_original". And the first run of
# imputation is saved in cas_mode and strike_mode. From second to fifth
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
# table(injuries3$cas_mode_original, injuries3$cas_mode, useNA = "always")
# table(injuries3$strike_mode_original, injuries3$strike_mode, useNA = "always")
# table(injuries3$strike_mode_original, injuries3$strike_mode_2nd, useNA = "always")

# Recode cas_mode and strike_mode
injuries4 <- injuries3 %>% 
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
table(injuries4$cas_mode, useNA = "always")
table(injuries3$strike_mode, useNA = "always")
table(injuries4$strike_mode, useNA = "always")

# Check if all modes are correctly recoded
unique(injuries4$cas_mode) %in% smodes$exhaustive_list
unique(injuries4$cas_mode_2nd) %in% smodes$exhaustive_list
unique(injuries4$cas_mode_3rd) %in% smodes$exhaustive_list
unique(injuries4$cas_mode_4th) %in% smodes$exhaustive_list
unique(injuries4$cas_mode_5th) %in% smodes$exhaustive_list
unique(injuries4$strike_mode) %in% smodes$exhaustive_list
unique(injuries4$strike_mode_2nd) %in% smodes$exhaustive_list
unique(injuries4$strike_mode_3rd) %in% smodes$exhaustive_list
unique(injuries4$strike_mode_4th) %in% smodes$exhaustive_list
unique(injuries4$strike_mode_5th) %in% smodes$exhaustive_list

injury_file <- 'injuries_santiago.csv'
write.csv(injuries4, paste0('inst/extdata/local/santiago/', injury_file))



