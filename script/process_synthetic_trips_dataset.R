require(tidyverse)

trips <- read_csv(file.choose())

# Rename trips id
trips$trip_id <- as.integer(as.factor(with(trips, paste(participant_id, trip_id, sep = "_"))))
