# Unify all transport modes

# Read accra's dataset as an example

library(tidyverse)

inj_accra <- read_csv('data/local/accra/pkg_injuries_accra.csv')

# Read lookup table
smodes <- read_csv('data/global/modes/standardized_modes.csv')
# Separate rows 
smodes <- smodes %>% separate_rows(original, sep = ';')

smodes <- smodes %>% 
  mutate(across(where(is.character), str_trim))

inj_accra$strike_mode <- smodes$exhaustive_list[match(tolower(inj_accra$strike_mode), smodes$original)]

inj_accra$cas_mode <- smodes$exhaustive_list[match(tolower(inj_accra$cas_mode), smodes$original)]

write_csv(inj_accra, 'inst/extdata/local/accra/injuries_accra.csv')