# Unify all transport modes

# Read accra's dataset as an example
rm (list = ls())
library(tidyverse)

read_smodes <- function(){
  # Read lookup table
  smodes <- read_csv('data/global/modes/standardized_modes.csv')
  # Separate rows 
  smodes <- smodes %>% separate_rows(original, sep = ';')
  
  # Remove spaces
  smodes <- smodes %>% 
    mutate(across(where(is.character), str_trim))
  
  return(smodes)
  
}

##### Accra

smodes <- read_smodes()

# Read accra injury dataset 
inj_accra <- read_csv('data/local/accra/pkg_injuries_accra.csv')

# Mutate
inj_accra$strike_mode <- smodes$exhaustive_list[match(tolower(inj_accra$strike_mode), smodes$original)]

# Mutate
inj_accra$cas_mode <- smodes$exhaustive_list[match(tolower(inj_accra$cas_mode), smodes$original)]

# Rewrite
write_csv(inj_accra, 'inst/extdata/local/accra/injuries_accra.csv')

#### Bangladesh

# Read bangalore dataset
inj_bang <- read_csv('data/local/bangalore/pkg_injuries_bangalore.csv')

# smodes
smodes <- read_smodes()

# Mutate
inj_bang$cas_mode <- smodes$exhaustive_list[match(tolower(inj_bang$cas_mode), smodes$original)]

# Mutate
inj_bang$strike_mode <- smodes$exhaustive_list[match(tolower(inj_bang$strike_mode), smodes$original)]

# Write

write_csv(inj_bang, 'inst/extdata/local/bangalore/injuries_bangalore.csv')

#### Delhi

# Read bangalore dataset
inj_del <- read_csv('data/local/delhi/pkg_injuries_delhi.csv')

# smodes
smodes <- read_smodes()

# Mutate
inj_del$cas_mode <- smodes$exhaustive_list[match(tolower(inj_del$cas_mode), smodes$original)]

# Mutate
inj_del$strike_mode <- smodes$exhaustive_list[match(tolower(inj_del$strike_mode), smodes$original)]

write_csv(inj_del, 'inst/extdata/local/delhi/injuries_delhi.csv')
