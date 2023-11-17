#' Personal motorcycle trips for Chilean cities
#'
#' Function to create personal motorcycle trips for some Chilean cities
#'
#' This function is used to create personal motorcycle trips for those Chilean cities which do not have any
#' motorcycle trips in their travel surveys. It uses as input the existing travel survey data for that city
#' and the proportion of these travel survey trips that are to be added as additional new motorcycle trips.
#'
#' Based on an analysis of the motorcycle trips in the travel surveys of Santiago, San Antonio,
#' Valparaiso and Puerto Montt it makes assumptions about the split between male and female
#' motorcyclists, the number of trips per person and the truncated normal distributions of the
#' distances and ages of the drivers of the personal motorcycle trips to be added. It creates
#' new trips by assuming that each such trip only has one stage, i.e. that the trip duration and distance
#' equals the stage duration and distance.
#'
#' This function contains the following steps:
#'
#' \itemize{
#' \item the characteristics of the motorcycle trips to be added are defined
#'
#' \item set up the parameters of the truncated normal distributions for male and
#'   female trip duration
#'
#' \item set up the parameters of the truncated normal distributions for male and
#'   female age ranges
#'
#' \item find the number of male and female motorcycle trips to be added
#'
#' \item divide the number of new male and female trips by 2 (= number of trips per person)
#'   to calculate the number of new participant ids to be added. If the number of male
#'   or female trips is odd, add an additional trip to get a trip number divisible by 2
#'
#' \item sample from truncated normal distributions to find the duration of new trips (assuming
#'   that each trip only consists of one stage)
#'
#' \item sample from the known age ranges to find the ages of the new male and female
#'   motorcycle trips and create new motorcycle trips
#' }
#'
#'
#' @param raw_trip_set data frame of trips from travel survey
#' @param PROPORTION_MOTORCYCLE_TRIPS proportion of trips in travel survey that are to be added as personal motorcycle trips
#'
#' @return original trip data with additional personal motorcycle trips added
#'
#' @export


add_motorcycle_trips_Chile <- function(raw_trip_set, PROPORTION_MOTORCYCLE_TRIPS) {
  # define the characteristics of the motorcycle trips to be added
  motorbike_proportion <- PROPORTION_MOTORCYCLE_TRIPS
  mbiketrips_per_person <- 2 # number of motorcycle trips per person
  prop_male <- 0.84 # proportion of male motorcyclists
  prop_female <- 1 - prop_male # proportion of female motorcyclist

  # set up parameters for trip duration - truncated normal distribution
  mean_dur_male <- 25 # mean duration (minutes) of the male motorcycle trips
  sd_dur_male <- 24 # standard deviation of the duration of male motorcycle trips
  min_dur_male <- 5 # minimum duration of male motorcycle trips
  max_dur_male <- 100 # maximum duration of male motorcycle trips

  mean_dur_female <- 14 # mean duration (minutes) of the female motorcycle trips
  sd_dur_female <- 13 # standard deviation of the duration of female motorcycle trips
  min_dur_female <- 5 # minimum duration of female motorcycle trips
  max_dur_female <- 80 # maximum duration of female motorcycle trips

  # set up parameters for age of motorbike users - truncated normal distribution
  mean_age_male <- 33 # mean age of male motorcyclists
  sd_age_male <- 15 # standard deviation of age of male motorcyclists
  min_age_male <- 15 # min age of male motorcyclists
  max_age_male <- 65 # maximum age of male motorcyclists

  mean_age_female <- 35 # mean age of male motorcyclists
  sd_age_female <- 10 # standard deviation of age of male motorcyclists
  min_age_female <- 15 # min age of male motorcyclists
  max_age_female <- 65 # maximum age of male motorcyclists


  ind <- raw_trip_set

  # remove trips with NA as trip id
  ind <- ind %>% filter(!is.na(trip_id))

  total_trips <- length(unique(ind$trip_id))

  # find the number of total trips that are to be added as either male or female motorcycle trips
  mbike_trips_male <- round(total_trips * motorbike_proportion * prop_male)
  mbike_trips_female <- round(total_trips * motorbike_proportion * prop_female)

  # divide number of motorbike trips by the number of motorbike trips per person to see how
  # many new person ids need to be added. Add a trip if the number of male or female
  # motorcycle trips is odd to get a number divisible by 2 (= mbiketrips_per_person)
  if (mbike_trips_male %% 2 == 1) {
    mbike_trips_male <- mbike_trips_male + 1
  }

  if (mbike_trips_female %% 2 == 1) {
    mbike_trips_female <- mbike_trips_female + 1
  }

  # calculate the number of new person ids
  new_person_id_male <- mbike_trips_male / mbiketrips_per_person
  new_person_id_female <- mbike_trips_female / mbiketrips_per_person

  mbike_speed <- MODE_SPEEDS %>% filter(stage_mode == "motorcycle") # find the speed of motorcycles
  mbike_speed <- mbike_speed$speed

  # define trip duration and distances of the new motorcycle trips using truncated normal distributions
  trip_duration_male <- round(truncnorm::rtruncnorm(mbike_trips_male, mean = mean_dur_male, sd = sd_dur_male, a = min_dur_male, b = max_dur_male))
  trip_duration_female <- round(truncnorm::rtruncnorm(mbike_trips_female, mean = mean_dur_female, sd = sd_dur_female, a = min_dur_female, b = max_dur_female))
  trip_distance_male <- trip_duration_male / mbike_speed
  trip_distance_female <- trip_duration_female / mbike_speed

  # create new male motorcycle trips - assume the these trips only have one stage,
  # i.e. the stage distance and duration equal the trip distance and duration
  # sample the ages of the newly added motorcyclists by using a truncated normal distribution
  new_trips_male <- data.frame(
    trip_id = c((max(ind$trip_id) + 1):(max(ind$trip_id) + mbike_trips_male)),
    trip_mode = "motorcycle",
    trip_distance = trip_distance_male,
    stage_mode = "motorcycle",
    stage_distance = trip_distance_male,
    stage_duration = trip_duration_male,
    participant_id = rep((max(ind$trip_id) + 1):(max(ind$trip_id) + new_person_id_male), mbiketrips_per_person),
    age = rep(round(truncnorm::rtruncnorm(new_person_id_male,
      mean = mean_age_male, sd = sd_age_male,
      a = min_age_male, b = max_age_male
    )), mbiketrips_per_person),
    sex = "male"
  )

  # create new female motorcycle trips - assume the these trips only have one stage,
  # i.e. the stage distance and duration equal the trip distance and duration
  # sample the ages of the newly added motorcyclists by using a truncated normal distribution
  new_trips_female <- data.frame(
    trip_id = c((max(ind$trip_id) + mbike_trips_male + 1):(max(ind$trip_id) + mbike_trips_male + mbike_trips_female)),
    trip_mode = "motorcycle",
    trip_distance = trip_distance_female,
    stage_mode = "motorcycle",
    stage_distance = trip_distance_female,
    stage_duration = trip_duration_female,
    participant_id = rep((max(ind$trip_id) + new_person_id_male + 1):
    (max(ind$trip_id) + new_person_id_male + new_person_id_female), mbiketrips_per_person),
    age = rep(round(truncnorm::rtruncnorm(new_person_id_female,
      mean = mean_age_female, sd = sd_age_female,
      a = min_age_female, b = max_age_female
    )), mbiketrips_per_person),
    sex = "female"
  )


  raw_trip_set <- rbind(raw_trip_set, new_trips_male, new_trips_female) # add motorcycle trips to existing trip data

  return(raw_trip_set)
}
