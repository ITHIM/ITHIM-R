library(tidyverse)
library(reshape2)
cities <- c('bangalore', 'bogota','buenos_aires', 'delhi', 'mexico_city', 'vizag')

# Read lookup table
smodes <- read_csv('data/global/modes/standardized_modes.csv')
# Separate rows 
smodes <- smodes %>% separate_rows(original, sep = ';')

# Trim
smodes <- smodes %>% 
  mutate(across(where(is.character), str_trim))

temp <- list()
for (city in cities){
  path <- file.path(paste0('data/local/',city, '/', city, '_injuries.csv'))
  whw <- read.csv(path)
  whw_lng <- reshape2::melt(whw)
  whw_lng <- whw_lng[rep(1:nrow(whw_lng), ceiling(whw_lng$value)),1:2] %>% as.data.frame()
  names(whw_lng)[1:2] <- c('cas_mode', 'strike_mode')
  
  whw_lng$cas_mode <- tolower(whw_lng$cas_mode)
  
  whw_lng$strike_mode <- tolower(whw_lng$strike_mode)
  print(city)
  print('cas mode')
  print(unique(whw_lng$cas_mode))
  print(unique(whw_lng$cas_mode) %in% smodes$original)
  print('strike mode')
  print(unique(whw_lng$strike_mode))
  print(unique(whw_lng$strike_mode) %in% smodes$original)
  
  if(!any(unique(whw_lng$cas_mode) %in% smodes$original)){
    print(unique(whw_lng$cas_mode))
    break
  }
  
  if(!any(unique(whw_lng$strike_mode) %in% smodes$original)){
    print(unique(whw_lng$strike_mode))
    break
  }
  
  # Recode cas mode
  whw_lng$cas_mode <- smodes$exhaustive_list[match(tolower(whw_lng$cas_mode), smodes$original)]
  
  # Recode strike mode
  whw_lng$strike_mode <- smodes$exhaustive_list[match(tolower(whw_lng$strike_mode), smodes$original)]
  
  
  temp[[city]] <- whw_lng
  
  injury_file <- paste0('injuries_', city, '.csv')
  write_csv(whw_lng, paste0('inst/extdata/local/',city, '/', injury_file))
  
}