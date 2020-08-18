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

# Accra ####

smodes <- read_smodes()

# Read accra injury dataset 
inj_accra <- read_csv('data/local/accra/pkg_injuries_accra.csv')

# Mutate
inj_accra$strike_mode <- smodes$exhaustive_list[match(tolower(inj_accra$strike_mode), smodes$original)]

# Mutate
inj_accra$cas_mode <- smodes$exhaustive_list[match(tolower(inj_accra$cas_mode), smodes$original)]

# Rewrite
write_csv(inj_accra, 'inst/extdata/local/accra/injuries_accra.csv')

# Bangladesh ####

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

# Belo Horizonte ####

# Read bangalore dataset
inj_bh <- read_csv('data/local/belo_horizonte/pkg_injuries_belo_horizonte.csv')

# smodes
smodes <- read_smodes()

# Mutate
inj_bh$cas_mode <- smodes$exhaustive_list[match(tolower(inj_bh$cas_mode), smodes$original)]

# Mutate
inj_bh$strike_mode <- smodes$exhaustive_list[match(tolower(inj_bh$strike_mode), smodes$original)]

write_csv(inj_bh, 'inst/extdata/local/belo_horizonte/injuries_belo_horizonte.csv')

# Bogota ####

# Read bogota dataset
inj_bg <- read_csv('data/local/bogota/pkg_injuries_bogota.csv')

# smodes
smodes <- read_smodes()

# Mutate
inj_bg$cas_mode <- smodes$exhaustive_list[match(tolower(inj_bg$cas_mode), smodes$original)]

# Mutate
inj_bg$strike_mode <- smodes$exhaustive_list[match(tolower(inj_bg$strike_mode), smodes$original)]

write_csv(inj_bg, 'inst/extdata/local/bogota/injuries_bogota.csv')

# Buenos Aires ####

# Read Buenos Aires dataset
inj_ba <- read_csv('data/local/buenos_aires/pkg_injuries_buenos_aires.csv')

# smodes
smodes <- read_smodes()

# Mutate
inj_ba$cas_mode <- smodes$exhaustive_list[match(tolower(inj_ba$cas_mode), smodes$original)]

# Mutate
inj_ba$strike_mode <- smodes$exhaustive_list[match(tolower(inj_ba$strike_mode), smodes$original)]

write_csv(inj_ba, 'inst/extdata/local/buenos_aires/injuries_buenos_aires.csv')

# Cape town ####

inj_ct <- read_csv('data/local/cape_town/injuries_cape_town.csv')

# Delhi ####

# Read bangalore dataset
inj_del <- read_csv('data/local/delhi/pkg_injuries_delhi.csv')

# smodes
smodes <- read_smodes()

# Mutate
inj_del$cas_mode <- smodes$exhaustive_list[match(tolower(inj_del$cas_mode), smodes$original)]

# Mutate
inj_del$strike_mode <- smodes$exhaustive_list[match(tolower(inj_del$strike_mode), smodes$original)]

write_csv(inj_del, 'inst/extdata/local/delhi/injuries_delhi.csv')


# Mexico City ####
# Read Mexico City dataset
inj_mex <- read_csv('data/local/mexico_city/pkg_injuries_mexico_city.csv')

# smodes
smodes <- read_smodes()

# Mutate
inj_mex$cas_mode <- smodes$exhaustive_list[match(tolower(inj_mex$cas_mode), smodes$original)]

# Mutate
inj_mex$strike_mode <- smodes$exhaustive_list[match(tolower(inj_mex$strike_mode), smodes$original)]

write_csv(inj_mex, 'inst/extdata/local/mexico_city/injuries_mexico_city.csv')

# Santiago ####
# Read Santiago dataset
inj_santiago <- read_csv('data/local/santiago/pkg_injuries_santiago.csv')

# smodes
smodes <- read_smodes()

# Mutate
inj_santiago$cas_mode <- smodes$exhaustive_list[match(tolower(inj_santiago$cas_mode), smodes$original)]

# Mutate
inj_santiago$strike_mode <- smodes$exhaustive_list[match(tolower(inj_santiago$strike_mode), smodes$original)]

write_csv(inj_santiago, 'inst/extdata/local/santiago/injuries_santiago.csv')


# Sao Paulo ####
# Read Sao Paulo dataset
inj_sp <- read_csv('data/local/sao_paulo/pkg_injuries_sao_paulo.csv')

# smodes
smodes <- read_smodes()

# Mutate
inj_sp$cas_mode <- smodes$exhaustive_list[match(tolower(inj_sp$cas_mode), smodes$original)]

# Mutate
inj_sp$strike_mode <- smodes$exhaustive_list[match(tolower(inj_sp$strike_mode), smodes$original)]

write_csv(inj_sp, 'inst/extdata/local/sao_paulo/injuries_sao_paulo.csv')

# Vizag ####
# Read vizag dataset
inj_vg <- read_csv('data/local/vizag/pkg_injuries_vizag.csv')

# smodes
smodes <- read_smodes()

# Mutate
inj_vg$cas_mode <- smodes$exhaustive_list[match(tolower(inj_vg$cas_mode), smodes$original)]

# Mutate
inj_vg$strike_mode <- smodes$exhaustive_list[match(tolower(inj_vg$strike_mode), smodes$original)]

write_csv(inj_vg, 'inst/extdata/local/vizag/injuries_vizag.csv')

