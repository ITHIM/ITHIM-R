# Load required libraries
library(dplyr)
library(tidyr)

# Read in the day trip dataset (assuming it's a CSV file)
day_trips <- read.csv("inst/extdata/local/bogota/trips_bogota.csv")

# Create a week-long dataset
week_trips <- day_trips %>%
  # Repeat each row 7 times (one for each day of the week)
  slice(rep(1:dplyr::n(), each = 7)) %>%
  # Add a day of the week column
  mutate(day_of_week = rep(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), dplyr::n()/7)) |> 
  # Re-caculate new trip IDs based on the expanded dataset
  mutate(trip_id = as.integer(as.factor(paste(participant_id, trip_id, day_of_week, sep = "_"))))

# Write the new dataset to a CSV file
write_csv(week_trips, "inst/extdata/local/bogota/trips_wl_bogota.csv")