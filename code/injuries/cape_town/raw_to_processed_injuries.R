# Load libraries
library(tidyverse)
library(mice)

# Set filepath
file_path <- file.path('data/local/cape_town/injuries_cape_town.csv')

# Read
whw <- read_csv(file_path)

# Remove unused columns
whw$X1 <- whw$year <- NULL

# Rename column
whw <- whw %>% rename(cas_gender = cas_sex)

# Add weight column
whw$weight <- 4

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

# There are 2 age missing, 47 cas_mode and 141 strike mode missing.
md.pattern(whw2)

# Out of the 47 and 151 cas and strike mode, in only 3 rows both modes are missing
md.pairs(whw2)

# imputation
imp1 <- mice(whw2, m = 100, seed = 12345)
imp1
#names(imp1$imp$cas_age)

# Save object with imputation
saveRDS(imp1, file = "code/injuries/cape_town/imputed.RDS") 
# imp1 <- readRDS("code/injuries/cape_town/imputed.RDS")

# Create the function to get the mode, from here https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Impute mode in age
cas_age <- imp1$imp$cas_age %>% rownames_to_column("id") 
cas_age$impute <- apply(cas_age[2:ncol(cas_age)], 1, getmode)

# Impute mode in cas_mode
cas_mode <- imp1$imp$cas_mode %>% rownames_to_column("id") 
cas_mode$impute <- apply(cas_mode[2:ncol(cas_mode)], 1, getmode)

# Impute mode in strike_mode
strike_mode <- imp1$imp$strike_mode %>% rownames_to_column("id") 
strike_mode$impute <- apply(strike_mode[2:ncol(strike_mode)], 1, getmode)

whw3 <- whw2
whw3[cas_age$id, "cas_age"] <- cas_age$impute
whw3[cas_mode$id, "cas_mode"] <- cas_mode$impute
whw3[strike_mode$id, "strike_mode"] <- strike_mode$impute


# Check these values were indeed imputed
# summary(whw2$cas_age); summary(whw3$cas_age)
# table(whw2$cas_mode, useNA = "always");table(whw3$cas_mode, useNA = "always")
# table(whw2$strike_mode, useNA = "always");table(whw3$strike_mode,useNA = "always")

# Recode cas mode
whw <- whw3
whw$cas_mode <- smodes$exhaustive_list[match(tolower(whw$cas_mode), smodes$original)]

# Recode strike mode
whw$strike_mode <- smodes$exhaustive_list[match(tolower(whw$strike_mode), smodes$original)]

# Check if all modes are correctly recoded
unique(whw$cas_mode) %in% smodes$exhaustive_list
unique(whw$strike_mode) %in% smodes$exhaustive_list

# Save file
write_csv(whw, 'inst/extdata/local/cape_town/injuries_cape_town.csv')
