require(tidyverse)

trips <- read_csv("~/Downloads/bogota_synthetic_trips (2).csv")

# Proportion to sample
sample_prop <- 0.2

# Rename trips id
trips$trip_id <- as.integer(as.factor(with(trips, paste(participant_id, trip_id, sep = "_"))))

trips <- trips |> 
  mutate(trip_mode = case_when(
    trip_mode  == "walk" ~ "pedestrian",
    trip_mode  == "bike" ~ "bike",
    trip_mode  == "pt" ~ "bus", 
    TRUE ~ trip_mode))


# Stratified sampling by 'trip_mode'
sampled_trips <- trips %>%
  group_by(trip_mode) %>%
  sample_frac(sample_prop) %>%
  ungroup()

sampled_trips$participant_id <- match(sampled_trips$participant_id, unique(sampled_trips$participant_id))

u_age <- sampled_trips |>
  distinct(participant_id, .keep_all = T) |> 
  rowwise() |> 
  mutate(random_age = ifelse(
    grepl("\\+", age),
    sample(70:100, 1),
    sample(seq(
      as.numeric(strsplit(age, "-")[[1]][1]),
      as.numeric(strsplit(age, "-")[[1]][2])
    ), 1)
  )) |> 
  ungroup() 


sampled_trips <- sampled_trips |> 
  left_join(u_age |> dplyr::select(participant_id, random_age)) |> 
  dplyr::select(-age) |> 
  rename(age = random_age)

# Function to add new rows
add_random_rows <- function(df) {
  # Calculate 10% of existing rows
  n_new <- ceiling(nrow(df) * 0.1)
  
  max_id <- max(df$participant_id)
  
  # Create new data frame with random values
  new_rows <- data.frame(
    participant_id = max_id+1:n_new,
    age = sample(0:100, n_new, replace = TRUE),
    sex = sample(c("Male", "Female"), n_new, replace = TRUE)
  )
  
  # Combine with original data frame
  plyr::rbind.fill(df, new_rows)
}

sampled_trips <- add_random_rows(sampled_trips)

write_csv(sampled_trips, "inst/extdata/local/bogota/trips_wl_bogota.csv")
