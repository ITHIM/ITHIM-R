#' Generate synthetic population from trip data
#'
#' Sequence of functions to set up the synthetic population, the synthetic trips, and the scenarios.
#'
#' This function performs the following steps:
#'
#' \itemize{
#' \item The columns from the TRIP_SET are put into the correct order
#'
#' \item multiply the trip distances, stage distances, and durations by the day_to_week scalar
#'   and then divide by 7 to get the distances and durations of an 'average' day of the week
#'
#' \item add bus_driver and truck trips if required (\code{\link{add_ghost_trips()}})
#'
#' \item add personal motorcycle trips if needed (call the appropriate function)
#'
#' \item add commercial motorcycle trips if required (\code{\link{add_ghost_trips()}})
#'
#' \item build the synthetic population by creating a data set that contains the (non-zero) participant
#'   ids and demographic information from the trip data set and adds work and leisure MMET
#'   values by calling \code{\link{create_synth_pop()}} (non travel entries in the trip data set are also removed)
#'
#' \item adds car driver trips if required (\code{\link{add_ghost_trips()}})
#'
#' \item call the \code{\link{ithim_setup_baseline_scenario()}} function to get the baseline data into the correct format
#'   for the creation of the different scenarios
#'
#' \item create the required scenarios by calling the appropriate function
#'
#' \item add walk to pt trips and combine the scenarios into one dataframe by calling the
#'   \code{\link{walk_to_pt_and_combine_scen()}} function
#' }
#'
#'
#' @return data frame of all synthetic trips from all scenarios
#'
#' @export
get_synthetic_from_trips <- function() {
  # to get the right order of trip columns; needed if trips are added
  if (CITY == "bogota_wb") {
    raw_trip_set <- data.frame(
      trip_id = TRIP_SET$trip_id,
      trip_mode = TRIP_SET$trip_mode,
      trip_distance = TRIP_SET$trip_distance,
      stage_mode = TRIP_SET$stage_mode,
      stage_distance = TRIP_SET$stage_distance,
      stage_duration = TRIP_SET$stage_duration,
      participant_id = TRIP_SET$participant_id,
      age = TRIP_SET$age,
      sex = TRIP_SET$sex,
      strata = TRIP_SET$strata,
      limitation = TRIP_SET$limitation,
      trip_motive = TRIP_SET$trip_motive,
      trip_start_time = TRIP_SET$trip_start_time,
      trip_end_time = TRIP_SET$trip_end_time,
      stringsAsFactors = F
    )
  } else {
    raw_trip_set <- data.frame(
      trip_id = TRIP_SET$trip_id,
      trip_mode = TRIP_SET$trip_mode,
      trip_distance = TRIP_SET$trip_distance,
      stage_mode = TRIP_SET$stage_mode,
      stage_distance = TRIP_SET$stage_distance,
      stage_duration = TRIP_SET$stage_duration,
      participant_id = TRIP_SET$participant_id,
      age = TRIP_SET$age,
      sex = TRIP_SET$sex,
      stringsAsFactors = F
    )
  }
  TRIP_SET <- NULL


  # scale distances and durations by the day_to_week_scalar and then divide by 7
  # to get distances travelled on an "average" day
  raw_trip_set$trip_distance <- raw_trip_set$trip_distance * DAY_TO_WEEK_TRAVEL_SCALAR / 7
  raw_trip_set$stage_distance <- raw_trip_set$stage_distance * DAY_TO_WEEK_TRAVEL_SCALAR / 7
  raw_trip_set$trip_distance <- raw_trip_set$trip_distance * DAY_TO_WEEK_TRAVEL_SCALAR / 7



  ## add bus trips
  if (ADD_BUS_DRIVERS) {
    raw_trip_set <- add_ghost_trips(raw_trip_set,
      prop_male = BUS_DRIVER_PROP_MALE,
      agerange_male = BUS_DRIVER_MALE_AGERANGE,
      agerange_female = BUS_DRIVER_FEMALE_AGERANGE
    )
  }
  # add truck trips
  if (ADD_TRUCK_DRIVERS) {
    # if car occupancy ratio is given, convert km of people travelling by car into car vehicle km
    # to get km travelled by truck
    if (ADD_CAR_DRIVERS) {
      raw_trip_set <- add_ghost_trips(raw_trip_set,
        trip_mode = "truck",
        distance_ratio = CAR_OCCUPANCY_RATIO * TRUCK_TO_CAR_RATIO * DISTANCE_SCALAR_CAR_TAXI,
        reference_mode = "car",
        prop_male = TRUCK_DRIVER_PROP_MALE,
        agerange_male = TRUCK_DRIVER_MALE_AGERANGE,
        agerange_female = TRUCK_DRIVER_FEMALE_AGERANGE
      )
    } else { # assume single occupancy of cars such that each km travelled in the travel survey corresponds
      # to the distance travelled by a vehicle
      raw_trip_set <- add_ghost_trips(raw_trip_set,
        trip_mode = "truck",
        distance_ratio = TRUCK_TO_CAR_RATIO * DISTANCE_SCALAR_CAR_TAXI, reference_mode = "car",
        prop_male = TRUCK_DRIVER_PROP_MALE,
        agerange_male = TRUCK_DRIVER_MALE_AGERANGE,
        agerange_female = TRUCK_DRIVER_FEMALE_AGERANGE
      )
    }
  }

  # add personal motorcycle trips if no motorcycle trips exist in travel survey
  if (ADD_PERSONAL_MOTORCYCLE_TRIPS != "no") {
    if (ADD_PERSONAL_MOTORCYCLE_TRIPS == "Chile") { # for Chilean cities call the appropriate function
      raw_trip_set <- add_motorcycle_trips_Chile(raw_trip_set, PROPORTION_MOTORCYCLE_TRIPS)
    }
  }



  if (ADD_MOTORCYCLE_FLEET) {
    raw_trip_set <- add_ghost_trips(raw_trip_set,
      trip_mode = "motorcycle",
      distance_ratio = FLEET_TO_MOTORCYCLE_RATIO / (1 - FLEET_TO_MOTORCYCLE_RATIO), reference_mode = "motorcycle",
      prop_male = COMMERCIAL_MBIKE_PROP_MALE,
      agerange_male = COMMERCIAL_MBIKE_MALE_AGERANGE,
      agerange_female = COMMERCIAL_MBIKE_FEMALE_AGERANGE
    )
  }
  # raw_trip_set <- add_ghost_trips(raw_trip_set,trip_mode='motorcycle',distance_ratio=(1-FLEET_TO_MOTORCYCLE_RATIO)/FLEET_TO_MOTORCYCLE_RATIO,reference_mode='motorcycle')

  # create synthetic population
  synth_pop <- create_synth_pop(raw_trip_set)


  # add car drivers by assuming that only a certain percentage of car trips (population_in_model_ratio) are driven
  # by people in the age range considered by the model.
  if (ADD_CAR_DRIVERS) {
    car_driver_scalar <<- min(1, CAR_OCCUPANCY_RATIO * 1 / population_in_model_ratio)
    # age ranges are not needed as car drivers are only used to calculate total vehicle km travelled for the CO2 model but are not
    # needed for the injury model unlike truck, motorcycle and bus drivers
    synth_pop$trip_set <- add_ghost_trips(synth_pop$trip_set,
      trip_mode = "car_driver",
      distance_ratio = car_driver_scalar * DISTANCE_SCALAR_CAR_TAXI,
      reference_mode = "car"
    )
  }

  raw_trip_set <- NULL

  # extract the population statistics and pa non-travel MMET values
  SYNTHETIC_POPULATION <<- synth_pop$synthetic_population

  trip_set <- synth_pop$trip_set # extract the trip characteristics
  synth_pop <- NULL

  # create scenarios by calling the appropriate function
  trip_set <- ithim_setup_baseline_scenario(trip_set)

  if (SCENARIO_NAME == "TEST_WALK_SCENARIO") {
    SYNTHETIC_TRIPS <- create_walk_scenario(trip_set)
  } else if (SCENARIO_NAME == "TEST_CYCLE_SCENARIO") {
    SYNTHETIC_TRIPS <- create_cycle_scenarios(trip_set)
  } else if (SCENARIO_NAME == "MAX_MODE_SHARE_SCENARIO") {
    SYNTHETIC_TRIPS <- create_max_mode_share_scenarios(trip_set)
  } else if (SCENARIO_NAME == "LATAM") {
    SYNTHETIC_TRIPS <- create_latam_scenarios(trip_set)
  } else if (SCENARIO_NAME == "GLOBAL") {
    SYNTHETIC_TRIPS <- create_global_scenarios(trip_set)
  } else if (SCENARIO_NAME == "AFRICA_INDIA") {
    SYNTHETIC_TRIPS <- create_africa_india_scenarios(trip_set)
  } else if (SCENARIO_NAME == "BOGOTA") {
    SYNTHETIC_TRIPS <- create_bogota_scenarios(trip_set)
  } else {
    SYNTHETIC_TRIPS <- create_all_scenarios(trip_set)
  }


  # some useful variables.
  NSCEN <<- length(SYNTHETIC_TRIPS) - 1 # define number of scenarios
  SCEN <<- sapply(SYNTHETIC_TRIPS, function(x) x$scenario[1])

  # define the scenario names
  # SCEN_SHORT_NAME <<- c("base",paste0("scen", 1:NSCEN) )
  SCEN_SHORT_NAME <<- c("base", paste0("sc_", rownames(SCENARIO_PROPORTIONS)))


  # add walk to pt trips, as appropriate, and combines list of scenarios
  trip_scen_sets <- walk_to_pt_and_combine_scen(SYNTHETIC_TRIPS)

  trip_scen_sets
}
