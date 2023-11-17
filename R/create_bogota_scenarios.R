#' Create scenarios for Bogota
#'
#' Creates three scenarios where in each one, the mode share of a given mode is elevated by a set
#' percentage of the total trips. The scenario modes are cycle, car, and bus.
#'
#' This function creates three scenarios increasing the mode shares of cycling, car
#' and bus by the same pre-defined percentage of the total mode shares.
#' We assume that:
#'
#' \itemize{
#' \item the total number of trips remains the same but trips from other modes
#'   (apart from truck, bus driver, car driver and commercial motorcycle trips
#'   which remain unchanged - at least initially) are converted to the mode in
#'   question; truck and commercial motorcycle trips remain constant across all
#'   scenarios whilst bus driver and car driver trips are updated based on the new
#'   total distance of car and bus trips once the increase in mode share has been
#'   conducted for each scenario
#'
#' \item the percentage share across the three distance bands of the mode that
#'   is increased is preserved, i.e. if 10% of all cycling trips are in distance
#'   band 0-2km then after increasing the cycling mode share by x% of all trips
#'   we still have 10% of all cycling trips in this distance band
#'
#' \item For each scenario we always convert the same % of total trips to the mode
#'   in question, independently of the original overall mode share of that mode.
#'   I.e. if e.g. 1% of all trips are cycle trips and 50% car trips and we apply a
#'   5% increase, then in the cycle scenario 6% of all trips are cycling trips and
#'   in the car scenario 55% of all trips are car trips. We always add a 5% increase
#'   of the total trips.
#'
#' \item we preserve the proportion of trips in each distance band. E.g. if 20% of
#'   all trips are in distance band 0-2km, then in each scenario 20% of all
#'   trips are still in distance band 0-2km.
#' }
#'
#'
#' Example:
#'
#' Assume that there are only two distance bands A and B and that 80% of all
#' cycling trips lie in distance band A and the remaining 20% in distance
#' band B.
#' Assume that 60% of all trips are in distance band A and 40% in distance
#' band B.
#' Assume we want to increase the cycling mode share by 5% of all trips.
#'
#' Then, we need to convert 5% x 80% / 60% = 6.67% of non-cycling trips in distance band A
#' to cycling trips and 5% x 20% / 40% = 2.5% of non-cycling trips in distance band B
#' to cycling trips.
#'
#' Overall, this leads to an increase of
#' (5% x 80% / 60%) x 60% + (5% x 20% / 40%) * 40% = 5%
#' of cycling trips, whilst preserving the cycling mode shares of 80% in distance
#' band A and 20% in distance band B and preserving the total mode shares of 60% in
#' distance band A and 40% in distance band B. The total number of trips is also
#' preserved.
#'
#'
#' The function performs the following steps:
#'
#' \itemize{
#' \item the overall mode shares for each of the cycle, car and bus modes across the three
#'   distance categories is defined
#'
#' \item from the trip data extract the trip information, calculate the total number of trips
#'   and find the proportion of trips in each distance category
#'
#' \item find the proportion of trips to be converted for each mode, scenario and distance category
#'
#' \item define the modes that are not changeable (at least initially) between the scenarios and
#'   divide the trip data into a set with the modes that can be changed and another set with the
#'   trips whose modes cannot be changed
#'
#' \item split the changeable trips and also all trips by distance band
#'
#' \item to create the scenarios loop through the scenarios:
#'   \itemize{
#'   \item loop through each distance band:
#'     \itemize{
#'     \item find the changeable trips that are not of the mode to be increased
#'
#'     \item count the number of trips made by the mode to be increased
#'
#'     \item for the bus scenario we aim to increase all public transport trips,
#'           i.e. we find the changeable trips not made by bus or rail and we count
#'           the number of trips made by bus or rail
#'
#'     \item find the number of total trips that we would like to convert
#'
#'     \item if the number of trips that are changeable equals the number of trips to be
#'           converted, all changeable trips are converted to the mode in question
#'
#'     \item if there are more trips that are changeable than we want to change,
#'           sample the number of trips to change from the changeable trip ids
#'
#'     \item if there are more trips to be converted than there are changeable trips, then
#'           convert as many trips as possible and create a warning message
#'
#'     \item convert the required trips to the new mode in question
#'     }
#'   \item add all trips across the distance bands and add the non-changeable trips
#'
#'   \item update the bus_driver and car_driver trips
#'   }
#' \item create a list containing the trips for each scenario as elements
#' }
#'
#'
#' @param trip_set data frame, baseline scenario trips
#'
#' @return list of baseline scenario and three scenarios
#'
#' @export
#'


create_bogota_scenarios <- function(trip_set) {
  rdr <- trip_set
  trip_set <- NULL

  rd_list <- list()

  # bogota modal split across the three distance categories for each mode
  # cycle, car, bus
  bogota_modeshares <- data.frame(
    c(32.6, 2.7, 0.8), # distance category 0-2km
    c(43.8, 24.9, 17.25), # distance category 2-6km
    c(23.6, 72.4, 81.95)
  ) # distance category >6km
  colnames(bogota_modeshares) <- DIST_CAT
  rownames(bogota_modeshares) <- c("cycle", "car", "bus")

  percentage_change <- SCENARIO_INCREASE # increase of each mode as percentage of total number of trips.

  # only keep necessary columns, i.e. remove any stage information
  rdr_baseline <- rdr %>%
    dplyr::select(c("trip_id", "trip_distance_cat", "scenario", "trip_mode")) %>%
    filter()
  rdr_baseline <- rdr_baseline %>% distinct() # remove any duplicates (for when there are multiple stages)

  no_trips <- nrow(rdr_baseline) # total number of trips

  # proportion of total trips in each distance category
  prop <- list()
  for (i in DIST_CAT) {
    prop[[i]] <- nrow(rdr_baseline %>% filter(trip_distance_cat == i)) / no_trips
  }
  # initialise the proportions to be added in each scenario
  scenario_proportions <- data.frame(
    c(0, 0, 0), # distance category 0-2km
    c(0, 0, 0), # distance category 2-6km
    c(0, 0, 0)
  ) # distance category >6km

  # add row and column names
  colnames(scenario_proportions) <- colnames(bogota_modeshares)
  rownames(scenario_proportions) <- rownames(bogota_modeshares)

  # find the proportion of trips to be converted for each distance category and scenario
  for (c in colnames(scenario_proportions)) {
    for (r in rownames(scenario_proportions)) {
      scenario_proportions[r, c] <- percentage_change * bogota_modeshares[r, c] / prop[[c]]
    }
  }

  SCENARIO_PROPORTIONS <<- scenario_proportions

  # print(scenario_proportions)

  # baseline scenario
  rd_list[["baseline"]] <- rdr
  modes_not_changeable <- c("bus_driver", "truck", "car_driver") # define the modes that can't be changed

  # create data frame containing all the trips that are not going to be changed in a scenario
  # i.e. bus_driver, truck and car_driver trips but also commercial motorcycle trips which have a participant id of 0
  rdr_not_changeable <- rdr %>% filter(trip_mode %in% modes_not_changeable | participant_id == 0)

  # Trips that can be reassigned to another mode
  rdr_changeable <- rdr %>% filter(!trip_mode %in% modes_not_changeable & !participant_id == 0)

  # Split the changeable trips by distance band, save in a new list
  rdr_changeable_by_distance <- list()
  for (j in colnames(SCENARIO_PROPORTIONS)) {
    rdr_changeable_by_distance[[j]] <- rdr_changeable %>%
      filter(trip_distance_cat == j)
  }
  rdr_changeable <- NULL

  # split all trips by distance band
  rdr_all_by_distance <- list()
  for (j in colnames(SCENARIO_PROPORTIONS)) {
    rdr_all_by_distance[[j]] <- rdr %>%
      filter(trip_distance_cat == j)
  }
  rdr <- NULL

  ###############################################################
  # Creation of scenarios
  scen_warning <- c()

  for (i in rownames(SCENARIO_PROPORTIONS)) { # Loop for each scenario
    rdr_copy <- list()

    for (j in colnames(SCENARIO_PROPORTIONS)) { # Loop for each distance band
      rdr_copy[[j]] <- rdr_changeable_by_distance[[j]] # Trips in the distance band

      if (i != "bus") {
        # Identify the trips_id of trips that weren't made by the trip mode
        potential_trip_ids <- unique(rdr_copy[[j]][!rdr_copy[[j]]$trip_mode %in% c(i), ]$trip_id)

        # Count the number of trips that were made by the trip mode
        current_mode_trips <- rdr_copy[[j]] %>%
          filter(trip_mode == i) %>%
          distinct(trip_id) %>%
          nrow()
      } else { # consider bus and rail trips together

        # Identify the trips_id of trips that weren't made by the trip mode
        potential_trip_ids <- unique(rdr_copy[[j]][!rdr_copy[[j]]$trip_mode %in% c(i, "rail"), ]$trip_id)

        # Count the number of trips that were made by the trip mode
        current_mode_trips <- rdr_copy[[j]] %>%
          filter(trip_mode %in% c(i, "rail")) %>%
          distinct(trip_id) %>%
          nrow()
      } # End else

      # These number of trips will be reassigned
      n_trips_to_change <- round(length(unique(rdr_all_by_distance[[j]]$trip_id)) *
        SCENARIO_PROPORTIONS[i, j] / 100)
      # print(n_trips_to_change)

      if (length(potential_trip_ids) > 0 & n_trips_to_change > 0) {
        # if the number of trips that could be changed equals the number of trips that need to be changed
        if (length(potential_trip_ids) == n_trips_to_change) {
          change_trip_ids <- potential_trip_ids

          # if there are less trips to change than should be changed
        } else if (length(potential_trip_ids) < n_trips_to_change) {
          # save name of scenario
          scen_warning <- c(scen_warning, i)

          # convert all trips possible
          change_trip_ids <- potential_trip_ids
        } else { # if there are more trips that can be changed than need to be changed, sample
          change_trip_ids <- base::sample(potential_trip_ids,
            size = n_trips_to_change
          )
        }

        # convert the trips to the new mode
        change_trips <- rdr_copy[[j]][rdr_copy[[j]]$trip_id %in% change_trip_ids, ] # extract trips to be changed
        change_trips$trip_mode <- i # assign a new trip mode name
        change_trips$stage_mode <- i # assign a new stage mode name

        # update the trip duration based on the new mode speeds
        change_trips$stage_duration <- change_trips$stage_distance * 60 /
          MODE_SPEEDS$speed[MODE_SPEEDS$stage_mode == i]

        # Replace trips reassigned in the trip dataset and save all trips in a new list
        rdr_copy[[j]] <-
          rbind(
            rdr_copy[[j]][!rdr_copy[[j]]$trip_id %in% change_trip_ids, ],
            change_trips
          )
      }
    } # End loop for distance bands

    rdr_scen <- do.call(rbind, rdr_copy) # bind across all distance bands
    rdr_scen <- rbind(rdr_scen, rdr_not_changeable) # add trips that could not be changed

    # Remove bus_driver from the dataset, to recalculate them
    if (ADD_BUS_DRIVERS) {
      rdr_scen <- filter(rdr_scen, !trip_mode %in% "bus_driver")
      rdr_scen <- add_ghost_trips(rdr_scen,
        trip_mode = "bus_driver",
        distance_ratio = BUS_TO_PASSENGER_RATIO * DISTANCE_SCALAR_PT,
        reference_mode = "bus",
        agerange_male = BUS_DRIVER_MALE_AGERANGE,
        agerange_female = BUS_DRIVER_FEMALE_AGERANGE,
        scenario = paste0("Scenario ", i)
      )
    }

    # Remove car_driver from the dataset, to recalculate them
    rdr_scen <- filter(rdr_scen, !trip_mode %in% "car_driver")
    if (ADD_CAR_DRIVERS) {
      rdr_scen <- add_ghost_trips(rdr_scen,
        trip_mode = "car_driver",
        distance_ratio = car_driver_scalar * DISTANCE_SCALAR_CAR_TAXI,
        reference_mode = "car",
        scenario = paste0("Scenario ", i)
      )
    }

    rdr_scen$scenario <- paste0("sc_", i) # add scenario name
    rd_list[[i]] <- rdr_scen # create output list by adding trips for each scenario
  } # End loop for scenarios


  # print warning message if there weren't enough trips to be converted for a scenario
  scen_warning <- unique(scen_warning)

  if (length(scen_warning) > 0) {
    for (j in 1:length(scen_warning)) {
      print(paste0(
        "WARNING: There are less trips that can be converted in scenario ", scen_warning[j],
        " than should be converted for ", city
      ))
    }
  }

  return(rd_list)
}
