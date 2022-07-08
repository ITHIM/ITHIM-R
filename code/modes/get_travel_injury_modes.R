# Load libraries
library(tidyverse)

# Read standard modes tables
smodes <- read_csv('data/global/modes/standardized_modes.csv') |> 
  separate_rows(original, sep = ';') |>  
  mutate(across(where(is.character), str_trim))

# Get names of all cities
dir_path <- "inst/extdata/local/"
cities <- list.dirs(path = dir_path, full.names = F, recursive = F)

# Remove old/legacy cities such as accra_test and bogota_wb
cities[!grepl(c("accra_test|bogota_wb"), cities) ]

trip_travel_modes <- list()
stage_travel_modes <- list()
# Loop through all cities
for(city in cities){
  # city <- cities[1]
  tripset_path <- file.path(paste0(dir_path, city,'/trips_', city, '.csv')) 
  trip_set <- read_csv(tripset_path, col_types = cols())
  trip_set_modes <- trip_set |> 
    filter(!is.na(trip_mode)) |> 
    distinct(trip_id, .keep_all = T) |> 
    count(trip_mode) |> 
    mutate(city = city, 
           p = round(n / sum(n) * 100, 2),
           is_mapped = ifelse(trip_mode %in% smodes$exhaustive_list, 
                              TRUE, FALSE))
  
  if (length(trip_travel_modes) > 0){
    trip_travel_modes <- plyr::rbind.fill(trip_travel_modes, 
                                          trip_set_modes)
  }else{
    trip_travel_modes <- trip_set_modes
  }
  
  if (any(names(trip_set) %in% c('stage_mode'))){
    
    stage_set_modes <- trip_set |>
      filter(!is.na(stage_mode)) |>
      count(stage_mode) |>
      mutate(city = city,
             p = round(n / sum(n) * 100, 2),
             is_mapped = ifelse(stage_mode %in% smodes$exhaustive_list, 
                                TRUE, FALSE))
    
    if (length(stage_travel_modes) > 0){
      stage_travel_modes <- plyr::rbind.fill(stage_travel_modes, 
                                             stage_set_modes)
    }else{
      stage_travel_modes <- stage_set_modes
    }
    
  }
}

