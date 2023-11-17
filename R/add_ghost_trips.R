#' Addition of ghost trips
#'
#' Add trips that do not get assigned any physical activity component,
#' can be used as proxy for vehicle distances.
#'
#' This function creates trips based on distance relative to another (reference) mode,
#' without altering the synthetic population. I.e. it adds trips that do not get assigned any physical
#' activity component. This function can be used to add e.g. car_driver and bus_driver trips
#' which are used as proxy for vehicle distances needed for the injury and the CO2 pathways.
#' It can also used to add truck and commercial motorcycle trips that are not included in any travel surveys.
#' As needed for the injury pathway, these newly added trips are assigned as trips made by a males or
#' females based on the proportion of males given as input parameter. The age ranges of males and females
#' taking those newly added trips can also be defined.
#'
#' The new mode distance is equally split by the number of people times the number of trips per people to
#' be added. This is used as the distance for new male trips. As the proportion of female trips tends to be
#' very low, the distance calculated for male trips is split by 10 and using a 10th of the distance
#' for male trips, 10 times as many female trips are added compared with the very low number of female
#' trips that would have been added had the same distance been used as for male trips.
#' E.g. if the number of people to be added is 100 with 1 trip per person and 98% of
#' those trips are made by males, then we add 98 male trips but 10 * 2 = 20 female trips
#' with a 10th of the distance of male trips. This is to ensure a better representation of
#' the demographics of female trips whilst keeping the number of newly added trips as
#' small as possible to reduce the run time of the model.
#'
#' The function performs the following steps:
#'
#' \itemize{
#' \item set up the number of people (with regards to males) and trips per per person to be added
#'
#' \item find the total distance of the reference mode
#'
#' \item find the age ranges for male and female trips
#'
#' \item find the number of male and female participants
#'
#' \item calculate the total distance of the new mode to be added based on the reference distance and
#'   find the speed of the new mode
#'
#' \item  for male trips:
#'    \itemize{
#'    \item define the distance range for each male trip to be added, assume that each trip is of equal length
#'    \item add new male trips sampling from the given age range (add_trips.R)
#'    \item add age and distance categories plus scenario name
#'   }
#' \item repeat for female trips (assuming female trips are a 10th of the distance
#'   of male trips and adding proportionally 10 times as many female trips)
#' }
#'
#'
#'
#' @param raw_trip_set data frame of trips
#' @param trip_mode which mode to add
#' @param distance_ratio fraction of reference distance to use to calculate new mode distance
#' @param reference_mode name of reference mode
#' @param prop_male proportion of newly added mode that are assigned to males
#' @param agerange_male age range of males associated with newly added mode
#' @param agerange_female age range of female drivers associated with newly added mode
#' @param scenario name of scenario for which mode is to be added
#'
#' @return data frame of trips
#'
#' @export


add_ghost_trips <- function(raw_trip_set,
                            trip_mode = "bus_driver",
                            distance_ratio = BUS_TO_PASSENGER_RATIO * DISTANCE_SCALAR_PT,
                            reference_mode = "bus",
                            prop_male = 1,
                            agerange_male = "18, 65",
                            agerange_female = "18, 65",
                            scenario = NA) {
  ## values for new ghost journeys
  nPeople <- 100 # number of people to be added
  nTrips <- 1 # number of trips per person

  # find the sum of distances for all trips made by the reference mode -> reference distance
  total_ref_distance <- sum(raw_trip_set[raw_trip_set$stage_mode == reference_mode, ]$stage_distance, na.rm = T)

  # determine the age ranges for both male and female trips
  agerange_male <- as.numeric(unlist(strsplit(gsub(" ", "", agerange_male, fixed = TRUE), "\\,")))
  agerange_male <- agerange_male[1]:agerange_male[2]
  agerange_female <- as.numeric(unlist(strsplit(gsub(" ", "", agerange_female, fixed = TRUE), "\\,")))
  agerange_female <- agerange_female[1]:agerange_female[2]

  # split number of people into males and females
  nMale_people <- floor(nPeople * prop_male / nTrips)
  nFemale_people <- nPeople - nMale_people

  new_mode <- trip_mode # define mode to be added
  total_new_distance <- total_ref_distance * distance_ratio # calculate total distance of new mode
  speed <- MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode == new_mode] # find mode speed

  # add new male trips
  # define distance range for males (assume range equals the new distance divided by the number of people and trips)
  distance_range_male <- c(total_new_distance / nPeople / nTrips, total_new_distance / nPeople / nTrips)
  for (i in 1:nMale_people) { # add new male trips
    new_trips <- add_trips(
      trip_ids = max(raw_trip_set$trip_id) + 1:nTrips, # add nTrips for each male
      new_mode = new_mode,
      distance = distance_range_male,
      participant_id = 0, # set participant_id to 0 to mark as ghost trip
      age = agerange_male,
      sex = "male",
      nTrips = nTrips,
      speed = speed
    )


    if ("age_cat" %in% names(raw_trip_set)) { # add age information if the existing trip set contains age category information
      age_category <- AGE_CATEGORY
      age_lower_bounds <- AGE_LOWER_BOUNDS
      for (j in 2:length(age_lower_bounds) - 1) {
        new_trips$age_cat[new_trips[["age"]] >= age_lower_bounds[j] & new_trips[["age"]] < age_lower_bounds[j + 1]] <- age_category[j]
      }
      new_trips$age_cat[new_trips[["age"]] >= age_lower_bounds[length(age_lower_bounds)]] <- age_category[length(age_lower_bounds)]
    }

    if ("scenario" %in% names(raw_trip_set)) { # add scenario name
      new_trips$scenario <- scenario
    }

    if ("trip_distance_cat" %in% names(raw_trip_set)) { # trip distance category information

      new_trips$trip_distance_cat[new_trips$trip_distance > 0 & new_trips$trip_distance < DIST_LOWER_BOUNDS[2]] <- DIST_CAT[1]
      new_trips$trip_distance_cat[new_trips$trip_distance >= DIST_LOWER_BOUNDS[2] & new_trips$trip_distance < DIST_LOWER_BOUNDS[3]] <- DIST_CAT[2]
      new_trips$trip_distance_cat[new_trips$trip_distance >= DIST_LOWER_BOUNDS[3]] <- DIST_CAT[3]
    }

    raw_trip_set <- dplyr::bind_rows(raw_trip_set, new_trips) # add male trips to existing trip set
  }


  ## add new female trips
  # as there tend to be far fewer females, the distance of female trips is set to 1/10th of the
  # male trips and 10 times more female trips are added than the number of females
  distance_range_female <- distance_range_male / 10
  if (nFemale_people > 0) {
    for (i in 1:(nFemale_people * 10)) {
      new_trips <- add_trips(
        trip_ids = max(raw_trip_set$trip_id) + 1:nTrips, # add nTrips for each female
        new_mode = new_mode,
        distance = distance_range_female,
        participant_id = 0,
        age = agerange_female,
        sex = "female",
        nTrips = nTrips,
        speed = speed
      )


      if ("age_cat" %in% names(raw_trip_set)) { # add age information if the existing trip set contains age category information
        age_category <- AGE_CATEGORY
        age_lower_bounds <- AGE_LOWER_BOUNDS
        for (j in 2:length(age_lower_bounds) - 1) {
          new_trips$age_cat[new_trips[["age"]] >= age_lower_bounds[j] & new_trips[["age"]] < age_lower_bounds[j + 1]] <- age_category[j]
        }
        new_trips$age_cat[new_trips[["age"]] >= age_lower_bounds[length(age_lower_bounds)]] <- age_category[length(age_lower_bounds)]
      }

      if ("scenario" %in% names(raw_trip_set)) { # add scenario name
        new_trips$scenario <- scenario
      }

      if ("trip_distance_cat" %in% names(raw_trip_set)) { # trip distance category information

        new_trips$trip_distance_cat[new_trips$trip_distance > 0 & new_trips$trip_distance < DIST_LOWER_BOUNDS[2]] <- DIST_CAT[1]
        new_trips$trip_distance_cat[new_trips$trip_distance >= DIST_LOWER_BOUNDS[2] & new_trips$trip_distance < DIST_LOWER_BOUNDS[3]] <- DIST_CAT[2]
        new_trips$trip_distance_cat[new_trips$trip_distance >= DIST_LOWER_BOUNDS[3]] <- DIST_CAT[3]
      }

      raw_trip_set <- dplyr::bind_rows(raw_trip_set, new_trips) # add male trips to existing trip set
    }
  }


  return(raw_trip_set) # return trip date with newly added trips
}
