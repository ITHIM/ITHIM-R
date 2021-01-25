# Load libraries
library(tidyverse)
library(reshape2)

# Specify cities
cities <- c('accra', 'bangalore', 'bogota','buenos_aires', 'delhi', 'mexico_city', 'santiago', 'vizag')

# Read lookup table for standardized modes
smodes <- read_csv('data/global/modes/standardized_modes.csv')
# Separate rows 
smodes <- smodes %>% separate_rows(original, sep = ';')

# Trim
smodes <- smodes %>% mutate(across(where(is.character), str_trim))

# Create a list for all whw matrices - used for debugging
temp <- list()

# Loop
for (city in cities){
  
  # Specify path 
  path <- file.path(paste0('data/local/',city, '/', city, '_injuries.csv'))
  
  # Read whw matrix
  whw <- read_csv(path)
  
  # Check if n_years exist
  weight <- 1
  
  if (any(names(whw) == 'n_years')){
    weight <- unique(whw$n_years)
    # remove n_years column
    whw$n_years <- NULL
  }
  
  # Format to long form
  whw_lng <- reshape2::melt(whw)
  
  # Remove all values with NA as value
  whw_lng <- whw_lng %>% filter(!is.na(value))
  
  # Repeat rows with cieled count
  whw_lng <- whw_lng[rep(1:nrow(whw_lng), ceiling(whw_lng$value)),1:2] %>% as.data.frame()
  
  # Rename columns
  names(whw_lng)[1:2] <- c('cas_mode', 'strike_mode')
  
  # Change case to lower
  whw_lng$cas_mode <- tolower(whw_lng$cas_mode)
  
  # Change case to lower
  whw_lng$strike_mode <- tolower(whw_lng$strike_mode)
  
  # Print useful info
  print(city)
  print('cas mode')
  print(unique(whw_lng$cas_mode))
  print(unique(whw_lng$cas_mode) %in% smodes$original)
  print('strike mode')
  print(unique(whw_lng$strike_mode))
  print(unique(whw_lng$strike_mode) %in% smodes$original)
  
  # If any of the cas_mode is not in the standardized original mode, break the loop
  if(!any(unique(whw_lng$cas_mode) %in% smodes$original)){
    print(unique(whw_lng$cas_mode))
    break
  }
  
  # If any of the strike_mode is not in the standardized original mode, break the loop
  if(!any(unique(whw_lng$strike_mode) %in% smodes$original)){
    print(unique(whw_lng$strike_mode))
    break
  }
  
  # Recode cas mode to standard mode
  whw_lng$cas_mode <- smodes$inj_vic_lng[match(tolower(whw_lng$cas_mode), smodes$original)]
  
  # Recode strike mode to standard mode
  whw_lng$strike_mode <- smodes$inj_str_lng[match(tolower(whw_lng$strike_mode), smodes$original)]  
  
  # Add weight column
  whw_lng$weight <- weight
  
  # Save whw_lng in list
  temp[[city]] <- whw_lng
  
  # Specify name of output file
  injury_file <- paste0('injuries_', city, '.csv')
  
  # Write file to the right folder
  write_csv(whw_lng, paste0('inst/extdata/local/',city, '/', injury_file))
}