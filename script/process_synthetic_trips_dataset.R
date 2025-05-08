require(tidyverse)

trips <- read_csv("~/Downloads/bogota_synthetic_trips (2).csv")

# Rename trips id
trips$trip_id <- as.integer(as.factor(with(trips, paste(participant_id, trip_id, sep = "_"))))

trips <- trips |> 
  mutate(trip_mode = case_when(
    trip_mode  == "walk" ~ "pedestrian",
    trip_mode  == "bike" ~ "bike",
    trip_mode  == "pt" ~ "bus", 
    TRUE ~ trip_mode))


# Proportion to sample
sample_prop <- 0.2

# Stratified sampling by 'trip_mode'
sampled_trips <- trips %>%
  group_by(trip_mode) %>%
  sample_frac(sample_prop) %>%
  ungroup()

sampled_trips <- sampled_trips %>%
  rowwise() %>%
  mutate(random_age = ifelse(
    grepl("\\+", age),
    sample(70:100, 1),
    sample(seq(
      as.numeric(strsplit(age, "-")[[1]][1]),
      as.numeric(strsplit(age, "-")[[1]][2])
    ), 1)
  )) %>%
  ungroup() |> 
  dplyr::select(-c(age)) |> 
  rename(age = random_age)


# Function to add new rows
add_random_rows <- function(df) {
  # Calculate 10% of existing rows
  n_new <- ceiling(nrow(df) * 0.1)
  
  # Create new data frame with random values
  new_rows <- data.frame(
    participant_id = 0,
    age = sample(0:100, n_new, replace = TRUE),
    sex = sample(c("Male", "Female"), n_new, replace = TRUE)
  )
  
  # Combine with original data frame
  plyr::rbind.fill(df, new_rows)
}

sampled_trips <- add_random_rows(sampled_trips)

write_csv(trips, "inst/extdata/local/bogota/trips_wl_bogota.csv")
