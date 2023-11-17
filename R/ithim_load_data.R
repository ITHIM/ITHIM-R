#' Load data for model and prepare input data for model
#'
#' Loads and processes data from files using both city specific local data and global data. Processes the
#' input data ready for the ITHIM-Global model run. Writes objects to the global environment.
#'
#'
#' This function performs the following steps to load and process the input data:
#'
#' \itemize{
#' \item find path of ithimr package where global and local data can be found
#'
#' \item check whether drpa package is installed
#'
#' \item read in Global Burden of Disease data for country
#'
#' \item read in city specific trip data:
#'
#'    \itemize{
#'    \item set missing stage or trip information to known stage or trip information,
#'      e.g. if stage duration is missing but trip duration is known then set stage duration to trip duration
#'
#'    \item ensure that stage modes and trip modes only consist of set keywords, replace all other mode names by those key
#'      words, e.g. replace 'train' by 'rail'
#'
#'    \item remove trips where age or sex are 'NA'
#'
#'    \item Rename pedestrian stage modes of non-pedestrian trips from 'pedestrian' to 'walk_to_pt'
#'
#'    \item remove any walk_to_pt stages that do not belong to either a rail or a bus trip modes
#'
#'    \item (call get_scenario_settings() if using the max_mode_share_scenario)
#'   }
#'
#' \item read in the local Global Burden of Disease (GBD) data:
#'    \itemize{
#'    \item combine various head and neck cancers, combine myeloid leukemia diseases, combine respiratory diseases at level 2
#'
#'    \item adjust for rectum cancer in the combined colon and rectum cancer burden
#'    }
#'
#'
#' \item read in local demographic data:
#'    \itemize{
#'    \item find / re-define max and min ages based on the max and min ages in the trip data,
#'      the demographic data and the max and min ages considered in the model
#'
#'    \item remove any population data outside these max and min ages
#'
#'    \item find the proportion of the total population that is considered in the model to the total population
#'
#'    \item get age-category details from population data, after any ages above and below the max and min ages have been removed
#'    }
#'
#'
#' \item extract all diseases plus road injures from GBD data, update format of max and min ages for each entry
#'
#' \item compute proportion of injuries in the age range considered in the model from the GBD data,
#'   this proportion is applied to those injury datasets without age and sex information
#'
#' \item remove ages outside age ranges considered in model from GBD_data
#'
#' \item create burden_of_disease dataframe from the GBD_data by changing the layout of the GBD_data:
#'    \itemize{
#'    \item add city specific population data
#'    \item add the country specific disease rate from the GBD data, ie. the proportion of the number of people
#'      in the country with that disease over the population of the country for each age and sex category
#'    \item using the country disease rate calculate the city population affected by the disease
#'    }
#'
#' \item using the burden_of_disease data (now called DISEASE_BURDEN), calculate the ratio of
#'   YLL to death for each age and sex category for the road_injuries data
#'
#' \item read in the city specific road injury data:
#'    \itemize{
#'    \item Set a 'weight' column to the unique number of years for which injury data exists (if such column does not already exist)
#'
#'    \item where strike mode equals casualty mode, set the strike mode to 'nov' (no other vehicle)
#'
#'    \item call set_injury_contingency.R function to set tables for WHW (who hit whom) and NOV (no other vehicle) fatalities
#'   }
#'
#' }
#'
#' @param speeds named list of mode speeds
#'
#'
#' @export

ithim_load_data <- function(speeds =
                              list(
                                bus = 8.1,
                                bus_driver = 8.1,
                                minibus = 8.1,
                                minibus_driver = 8.1,
                                car = 13.8,
                                car_driver = 13.8,
                                taxi = 13.8,
                                pedestrian = 2.5,
                                walk_to_pt = 2.5,
                                cycle = 7.2,
                                motorcycle = 15.2,
                                truck = 8.1,
                                van = 13.8,
                                subway = 18.1,
                                rail = 21.9,
                                auto_rickshaw = 4,
                                shared_auto = 13.8,
                                shared_taxi = 13.8,
                                cycle_rickshaw = 4,
                                other = 9.1
                              )) {
  ## this function requires path specification, so that it may differ for different case studies

  ## these datasets are all global, saved in global folder
  # find path where ithimr package is saved on computer
  global_path <- paste0(file.path(
    find.package("ithimr", lib.loc = .libPaths()),
    "extdata/global"
  ), "/")

  ## Check if DRPA package is installed
  if (!require("drpa", character.only = TRUE)) {
    stop('Please install "drpa" package and run it again. You can do this by using "remotes::install_github("meta-analyses/drpa")"')
  }

  ### Read in global disease data
  ## DATA FILES FOR MODEL
  DISEASE_INVENTORY <<- read.csv(paste0(global_path, "dose_response/disease_outcomes_lookup.csv"))

  list_of_files <- list.files(
    path = paste0(
      global_path,
      "dose_response/drap/extdata/"
    ),
    recursive = TRUE, pattern = "\\.csv$",
    full.names = TRUE
  )
  for (i in 1:length(list_of_files)) {
    assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
      read.csv(list_of_files[[i]]),
      pos = 1
    )
  }


  ## these datasets are all local, saved in local folder.
  local_path <- PATH_TO_LOCAL_DATA


  ## DATA FILES SPECIFIC FOR EACH CITY

  ## Read in trip data
  # we need columns: trip_id, trip_mode, stage_mode, stage_duration, trip_distance, stage_distance
  # trips can be composed of multiple stages
  # all trip columns are used for scenario generation alone
  # stage columns are used for downstream calculation
  filename <- paste0(local_path, "/trips_", CITY, ".csv")
  trip_set <- read_csv(filename, col_types = cols())

  trip_set$participant_id <- as.numeric(as.factor(trip_set$participant_id))

  ## set missing stage or trip information to known stage or trip information
  mode_cols <- c("trip_mode", "stage_mode")
  if (sum(mode_cols %in% colnames(trip_set)) == 0) {
    stop(paste0('Please include a column labelled "trip_mode" or "stage_mode" in ', filename))
  }
  # set stage_mode to trip_mode if no stage mode exists
  if ("trip_mode" %in% colnames(trip_set) && !"stage_mode" %in% colnames(trip_set)) {
    trip_set$stage_mode <- trip_set$trip_mode
  }
  # set trip_mode to stage_mode if no trip mode exists
  if ("stage_mode" %in% colnames(trip_set) && !"trip_mode" %in% colnames(trip_set)) {
    trip_set$trip_mode <- trip_set$stage_mode
  }
  # set stage_duration to trip duration if no stage_duration exists
  if ("trip_duration" %in% colnames(trip_set) && !"stage_duration" %in% colnames(trip_set)) {
    trip_set$stage_duration <- trip_set$trip_duration
  }
  # set stage distance to trip distance if no stage distance exists
  if ("trip_distance" %in% colnames(trip_set) && !"stage_distance" %in% colnames(trip_set)) {
    trip_set$stage_distance <- trip_set$trip_distance
  }
  # set trip_distance to stage_distance if no trip_distance exists
  if ("stage_distance" %in% colnames(trip_set) && !"trip_distance" %in% colnames(trip_set)) {
    trip_set$trip_distance <- trip_set$stage_distance
  }


  # use specified words for key modes
  # ensure those key words are used for both trip modes and stage modes
  walk_words <- c("walk", "walked", "pedestrian")
  cycle_words <- c("bike", "cycle", "cycling")
  mc_words <- c("motorcycle", "mcycle", "mc", "mtw")
  subway_words <- c("metro", "underground")
  rail_words <- c("train")
  for (i in 1:length(mode_cols)) {
    # lower case mode names
    trip_set[[mode_cols[i]]] <- tolower(trip_set[[mode_cols[i]]])
    # replaces spaces with _
    trip_set[[mode_cols[i]]] <- sapply(trip_set[[mode_cols[i]]], function(x) gsub(" ", "_", as.character(x)))
    trip_set[[mode_cols[i]]][trip_set[[mode_cols[i]]] == "private_car"] <- "car"
    trip_set[[mode_cols[i]]][trip_set[[mode_cols[i]]] %in% walk_words] <- "pedestrian"
    trip_set[[mode_cols[i]]][trip_set[[mode_cols[i]]] %in% cycle_words] <- "cycle"
    trip_set[[mode_cols[i]]][trip_set[[mode_cols[i]]] %in% mc_words] <- "motorcycle"
    trip_set[[mode_cols[i]]][trip_set[[mode_cols[i]]] %in% subway_words] <- "subway"
    trip_set[[mode_cols[i]]][trip_set[[mode_cols[i]]] %in% rail_words] <- "rail"
  }

  # remove trips with unknown age or sex
  trip_set <- subset(trip_set, !is.na(age))
  trip_set <- subset(trip_set, !is.na(sex))
  # set sex to lower letters
  trip_set$sex <- tolower(trip_set$sex)
  # set trip id to 0 if no stage mode exists
  trip_set$trip_id[is.na(trip_set$stage_mode)] <- 0

  # Rename short walk components of non-pedestrian trips from 'pedestrian' to 'walk_to_pt'
  if ("stage_mode" %in% colnames(trip_set) && "trip_mode" %in% colnames(trip_set)) {
    trip_set[!is.na(trip_set$trip_mode) & !is.na(trip_set$stage_mode) & trip_set$trip_mode != "pedestrian" & trip_set$stage_mode == "pedestrian", ]$stage_mode <- "walk_to_pt"

    # Remove walking component in trips that are not PT
    trip_set <- trip_set %>%
      mutate(
        cond = ifelse(stage_mode == "walk_to_pt" &
          !trip_mode %in% c("bus", "rail"), 1, 0)
      ) %>%
      filter(cond == 0 | is.na(cond)) %>%
      dplyr::select(-cond)
  }

  # set to global environment
  TRIP_SET <<- trip_set

  # call get_scenario_settings() if using the max_mode_share_scenario
  if (SCENARIO_NAME == "MAX_MODE_SHARE_SCENARIO" &&
    (!exists("SCENARIO_PROPORTIONS") ||
      exists("SCENARIO_PROPORTIONS") && !isTRUE(base::all.equal(DIST_CAT, colnames(SCENARIO_PROPORTIONS)))
    )) {
    SCENARIO_PROPORTIONS <<- get_scenario_settings(distances = DIST_CAT, speeds = speeds)
  }



  ## Read in city specific global burden of disease data
  # GBD file needs to have the following columns:
  # age (=label, e.g. 15-49)
  # sex (=male or female)
  # measure
  # cause (GBD_DATA$cause matches DISEASE_INVENTORY$GBD_name)
  # metric
  # burden
  # min_age (=number, e.g. 15)
  # max_age (=number, e.g. 49)
  filename <- paste0(local_path, "/gbd_", CITY, ".csv")
  GBD_DATA <- read_csv(filename, col_types = readr::cols())


  # Combine causes related to "Head and neck cancer"
  head_neck_causes <- c(
    "Esophageal cancer", "Larynx cancer",
    "Lip and oral cavity cancer", "Nasopharynx cancer",
    "Other pharynx cancer"
  )
  head_neck <- GBD_DATA %>% filter(cause_name %in% head_neck_causes)
  GBD_DATA <- GBD_DATA %>% filter(!cause_name %in% head_neck_causes)

  # Add head and neck cancer causes by measure, sex and age
  add_causes <- head_neck %>%
    group_by(measure_name.x, sex_name, age_name) %>%
    summarise(val = sum(val)) %>%
    mutate(
      cause_name = "Head and neck cancer",
      location_name = unique(GBD_DATA$location_name)
    ) %>%
    left_join(
      GBD_DATA %>%
        dplyr::select(sex_name, age_name, population) %>% distinct(),
      by = c("sex_name", "age_name")
    ) %>%
    dplyr::select(
      measure_name.x, location_name, sex_name, age_name, cause_name,
      val, population
    )
  # Append head and neck to GBD dataset
  GBD_DATA <- GBD_DATA %>% bind_rows(add_causes)


  # Combine causes related to myeloid leukemia
  myeloid_leukemia_causes <- c("Chronic myeloid leukemia", "Acute myeloid leukemia")
  myeloid_leukemia <- GBD_DATA %>% filter(cause_name %in% myeloid_leukemia_causes)
  GBD_DATA <- GBD_DATA %>% filter(!cause_name %in% myeloid_leukemia_causes)
  # Add causes by measure, sex and age
  add_causes <- myeloid_leukemia %>%
    group_by(measure_name.x, sex_name, age_name) %>%
    summarise(val = sum(val)) %>%
    mutate(
      cause_name = "Myeloid leukemia",
      location_name = unique(GBD_DATA$location_name)
    ) %>%
    left_join(
      GBD_DATA %>%
        dplyr::select(sex_name, age_name, population) %>% distinct(),
      by = c("sex_name", "age_name")
    ) %>%
    dplyr::select(
      measure_name.x, location_name, sex_name, age_name, cause_name,
      val, population
    )
  # Append myeloid leukemia to GBD dataset
  GBD_DATA <- GBD_DATA %>% bind_rows(add_causes)

  # Combine causes related to "Respiratory" causes at level 2
  respiratory_causes <- c(
    "Lower respiratory infections",
    "Upper respiratory infections",
    "Chronic obstructive pulmonary disease",
    "Pneumoconiosis",
    "Asthma",
    "Interstitial lung disease and pulmonary sarcoidosis",
    "Other chronic respiratory diseases"
  )
  respiratory <- GBD_DATA %>% filter(cause_name %in% respiratory_causes)
  # GBD_DATA <- GBD_DATA %>% filter(!cause_name %in% respiratory_causes)
  # Add causes by measure, sex and age
  add_causes <- respiratory %>%
    group_by(measure_name.x, sex_name, age_name) %>%
    summarise(val = sum(val)) %>%
    mutate(
      cause_name = "Respiratory",
      location_name = unique(GBD_DATA$location_name)
    ) %>%
    left_join(
      GBD_DATA %>%
        dplyr::select(sex_name, age_name, population) %>% distinct(),
      by = c("sex_name", "age_name")
    ) %>%
    dplyr::select(
      measure_name.x, location_name, sex_name, age_name, cause_name,
      val, population
    )
  # Append head and neck to GBD dataset
  GBD_DATA <- GBD_DATA %>% bind_rows(add_causes)


  ## Adjust for Rectum cancer in the combined Colon and Rectum cancer burden
  ## Source: https://www.cancer.org/cancer/colon-rectal-cancer/about/key-statistics.html
  ## 106,970 new cases of colon cancer
  ## 46,050 new cases of rectal cancer
  ## So reduce the burden by a 3rd (46,050 / (106,970 + 46,050) = 0.3)
  colon_cancer_causes <- c("Colon and rectum cancer")
  colon_cancer <- GBD_DATA %>% filter(cause_name %in% colon_cancer_causes)
  GBD_DATA <- GBD_DATA %>% filter(!cause_name %in% colon_cancer_causes)
  # Add causes by measure, sex and age
  add_causes <- colon_cancer %>%
    group_by(measure_name.x, sex_name, age_name) %>%
    summarise(val = sum(val) * (2 / 3)) %>%
    mutate(
      cause_name = "Colon cancer",
      location_name = unique(GBD_DATA$location_name)
    ) %>%
    left_join(
      GBD_DATA %>%
        dplyr::select(sex_name, age_name, population) %>% distinct(),
      by = c("sex_name", "age_name")
    ) %>%
    dplyr::select(
      measure_name.x, location_name, sex_name, age_name, cause_name,
      val, population
    )
  # Append colon cancer to GBD dataset
  GBD_DATA <- GBD_DATA %>% bind_rows(add_causes)




  ## Read in local demographic data
  filename <- paste0(local_path, "/population_", CITY, ".csv")
  demographic <- read_csv(filename, col_types = cols())
  demographic <- demographic[!apply(demographic, 1, anyNA), ]
  demographic$age <- gsub("\\s", "", demographic$age)
  demographic$sex <- tolower(demographic$sex)

  # find the minimum and maximum ages based on the min and max ages given in the demographic data, the trip data and
  # the min and max ages given as model input values
  age_category <- demographic$age
  max_age <- max(as.numeric(sapply(age_category, function(x) strsplit(x, "-")[[1]][2])))
  max_age <- min(max_age, max(trip_set$age), AGE_RANGE[2])
  min_age <- min(as.numeric(sapply(age_category, function(x) strsplit(x, "-")[[1]][1])))
  min_age <- max(min_age, min(trip_set$age), AGE_RANGE[1])
  # remove any population outside the newly defined min and max ages
  DEMOGRAPHIC <<- demographic[as.numeric(sapply(age_category, function(x) strsplit(x, "-")[[1]][1])) <= max_age &
    as.numeric(sapply(age_category, function(x) strsplit(x, "-")[[1]][2])) >= min_age, ]

  # find the proportion of the total population that is considered in the model to the total population
  population_in_model_ratio <<- sum(DEMOGRAPHIC$population) / sum(demographic$population)
  population_in_model <<- sum(DEMOGRAPHIC$population)

  # get age-category details from population data, after any ages above and below the max and min ages have been removed
  AGE_CATEGORY <<- unique(DEMOGRAPHIC$age)
  AGE_LOWER_BOUNDS <<- as.numeric(sapply(AGE_CATEGORY, function(x) strsplit(x, "-")[[1]][1]))
  MAX_AGE <<- max(as.numeric(sapply(AGE_CATEGORY, function(x) strsplit(x, "-")[[1]][2])))

  # extract all diseases plus road injures from GBD data, update format of max and min ages for each entry
  disease_names <- c(as.character(DISEASE_INVENTORY$GBD_name), "Road injuries")
  GBD_DATA <- subset(GBD_DATA, cause_name %in% disease_names)
  GBD_DATA$min_age <- as.numeric(sapply(GBD_DATA$age_name, function(x) str_split(x, " to ")[[1]][1]))
  GBD_DATA$max_age <- as.numeric(sapply(GBD_DATA$age_name, function(x) str_split(x, " to ")[[1]][2]))

  # Compute proportion of injuries in the age range using the GBD data, so it can be used
  # when estimating injuries health results.
  # Compute number of deaths in road injuries in all age ranges
  deaths_injuries <- as.numeric(GBD_DATA %>%
    filter(cause_name == "Road injuries" & measure_name.x == "Deaths") %>% summarise(sum(val)))
  # Filter GBD datasets for age-ranges considered (usually 15-69)
  GBD_DATA <- subset(GBD_DATA, max_age >= AGE_LOWER_BOUNDS[1])
  GBD_DATA <- subset(GBD_DATA, min_age <= MAX_AGE)
  # Compute number of deaths in road injuries for age-ranges considered
  # (usually 15-69)
  deaths_injuries_agerange <- as.numeric(GBD_DATA %>%
    filter(cause_name == "Road injuries" &
      measure_name.x == "Deaths") %>%
    summarise(sum(val)))
  # Compute proportion of injuries in the age range. This proportion is
  # going to be applied to the injuries datasets where there is no cas_age or
  # cas_gender
  PROPORTION_INJURIES_AGERANGE <<- deaths_injuries_agerange / deaths_injuries

  # rename GBD_Data columns
  GBD_DATA <- GBD_DATA %>% rename(
    "measure" = "measure_name.x",
    "sex" = "sex_name",
    "age" = "age_name",
    "cause" = "cause_name"
  )
  GBD_DATA$sex <- tolower(GBD_DATA$sex)

  # change layout of GBD_DATA and only keep relevant information
  burden_of_disease <- expand.grid(
    measure = unique(GBD_DATA$measure), sex = unique(DEMOGRAPHIC$sex), age = unique(DEMOGRAPHIC$age),
    cause = disease_names, stringsAsFactors = F
  )
  # add population information, find min and max ages
  burden_of_disease <- dplyr::left_join(burden_of_disease, DEMOGRAPHIC, by = c("age", "sex"))
  burden_of_disease$min_age <- as.numeric(sapply(burden_of_disease$age, function(x) str_split(x, "-")[[1]][1]))
  burden_of_disease$max_age <- as.numeric(sapply(burden_of_disease$age, function(x) str_split(x, "-")[[1]][2]))
  # when we sum ages, we assume that all age boundaries used coincide with the GBD age boundaries.
  # the rate calculated is the proportion of the number of people in the country with that disease over the population of the country
  GBD_DATA$rate <- GBD_DATA$val / GBD_DATA$population
  burden_of_disease <- burden_of_disease %>%
    left_join(GBD_DATA[, c("measure", "sex", "cause", "min_age", "max_age", "rate")],
      by = c("measure", "sex", "cause", "min_age", "max_age")
    )

  # scale disease burden of country to the city using population of city and assuming that the disease rate for the
  # city is the same as for the entire country
  burden_of_disease$burden <- burden_of_disease$population * burden_of_disease$rate
  burden_of_disease$burden[is.na(burden_of_disease$burden)] <- 0

  DISEASE_BURDEN <<- burden_of_disease

  # consider road injuries from DISEASE_BURDEN and calculate the ratio of YLL to deaths for each age and sex group
  gbd_injuries <- DISEASE_BURDEN[which(DISEASE_BURDEN$cause == "Road injuries"), ]
  gbd_injuries$sex_age <- paste0(gbd_injuries$sex, "_", gbd_injuries$age)
  ## calculate the ratio of YLL to deaths for each age and sex group
  gbd_injuries <- arrange(gbd_injuries, measure)
  gbd_inj_yll <- gbd_injuries[which(gbd_injuries$measure == "YLLs (Years of Life Lost)"), ]
  gbd_inj_dth <- gbd_injuries[which(gbd_injuries$measure == "Deaths"), ]
  gbd_inj_yll$yll_dth_ratio <- gbd_inj_yll$burden / gbd_inj_dth$burden
  GBD_INJ_YLL <<- gbd_inj_yll

  ## Read in Physical Activity data
  filename <- paste0(local_path, "/pa_", CITY, ".csv")
  pa_set <- read_csv(filename, col_types = cols())
  pa_set$sex <- tolower(pa_set$sex)
  PA_SET <<- pa_set


  ## injury data
  filename <- paste0(local_path, "/injuries_", CITY, ".csv")
  injuries <- read_csv(filename, col_types = cols())

  # remove injury data outside age range and assign age category, call column age_cat
  if ("cas_age" %in% colnames(injuries)) injuries <- assign_age_groups(injuries, age_label = "cas_age")
  injuries$cas_mode <- tolower(injuries$cas_mode)
  injuries$strike_mode <- tolower(injuries$strike_mode)
  if ("cas_gender" %in% colnames(injuries)) injuries$cas_gender <- tolower(injuries$cas_gender)
  # define strike modes that are considered as no other vehicle collisions
  nov_words <- c("no.other.fixed.or.stationary.object", "no other vehicle", "none")
  injuries$strike_mode[injuries$strike_mode %in% nov_words] <- "nov" # ensure all nov occurrences are labelled as 'nov'

  # add weight column if missing - weight column represents the number of years for which injury data exists
  if (!"weight" %in% colnames(injuries)) {
    injuries$weight <- 1
  }

  # Set weight as the unique number of years for which injury data exists
  if ("year" %in% colnames(injuries)) {
    injuries$weight <- length(unique(injuries$year))
  }


  # Get all injuries where casualty and strike mode are identical for car, bus, motorcycle, cycle and truck
  # Treat bus_driver same as bus for strike mode
  same_cas_str_modes <- injuries %>% filter((cas_mode == "car" & strike_mode == "car") |
    (cas_mode == "bus" & (strike_mode %in% c("bus", "bus_driver"))) |
    (cas_mode == "motorcycle" & strike_mode == "motorcycle") |
    (cas_mode == "cycle" & strike_mode == "cycle") |
    (cas_mode == "truck" & strike_mode == "truck"))

  # Filter all injuries where casualty equals strike mode
  # create dataset where casuality = strike mode fatality counts are removed
  injuries <- injuries %>% filter(!((cas_mode == "car" & strike_mode == "car") |
    (cas_mode == "bus" & (strike_mode %in% c("bus", "bus_driver"))) |
    (cas_mode == "motorcycle" & strike_mode == "motorcycle") |
    (cas_mode == "cycle" & strike_mode == "cycle") |
    (cas_mode == "truck" & strike_mode == "truck")))

  # Where strike mode equals casualty mode, set the strike mode to 'nov'
  same_cas_str_modes <- same_cas_str_modes %>% mutate(strike_mode = "nov")

  # Join all injuries again
  injuries <- dplyr::bind_rows(injuries, same_cas_str_modes)

  # Call function to set tables for WHW and NOV
  set_injury_contingency(injuries)
}
