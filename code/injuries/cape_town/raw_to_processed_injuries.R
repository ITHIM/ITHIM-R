# Load libraries
library(tidyverse)

# Set filepath
file_path <- file.path('data/local/cape_town/injuries_cape_town.csv')

# Read
whw <- read_csv(file_path)

# Remove unused columns
whw$X1 <- whw$year <- NULL

# Rename column
whw <- whw %>% rename(cas_gender = cas_sex)

# Read lookup table
smodes <- read_csv('data/global/modes/standardized_modes.csv')
# Separate rows 
smodes <- smodes %>% separate_rows(original, sep = ';')

# Trim
smodes <- smodes %>% 
  mutate(across(where(is.character), str_trim))

# Recode cas mode
whw$cas_mode <- smodes$exhaustive_list[match(tolower(whw$cas_mode), smodes$original)]

# Recode strike mode
whw$strike_mode <- smodes$exhaustive_list[match(tolower(whw$strike_mode), smodes$original)]

# Check if all modes are correctly recoded
unique(whw$cas_mode) %in% smodes$exhaustive_list
unique(whw$strike_mode) %in% smodes$exhaustive_list

# Save file
write_csv(whw, 'inst/extdata/local/cape_town/injuries_cape_town.csv')