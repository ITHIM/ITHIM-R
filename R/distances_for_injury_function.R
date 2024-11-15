#' Get distances and parameterise Poisson regression model for injuries
#'
#' Computes exposures (distances) by mode to parameterise the injury regression model,
#' which is computed as a Poisson model and which is used in the ITHIM-Global model
#' to predict injury fatalities at a later stage
#'
#' The function uses distance data and the individual injury fatality data to perform the following steps
#' to parameterise the Poisson injury regression model:
#'
#' \itemize{
#' 
#'  \item taxi, shared_taxi and auto_rickshaw distances are multiplied by 2 to add drivers (note if there are multiple passengers in a vehicle apart
#'    then this will overestimate these vehicle distances but taxis also spend a significant amount of time driving around without any
#'    passenger which is not captured in the travel surveys.)
#' 
#'  \item stage modes are aggregated such that all walk related stages (walk and walk to pt) are of the same mode (walk),
#'    similarly for all car related journeys
#'
#'  \item bus drivers are added to bus journeys (where relevant) to accurately represent all people on a bus
#'
#'  \item Takes Baseline injury tables, split into who-hit whom (whw) and no-other-vehicle (nov) parts,
#'    and adds total population distances for each strike and casualty mode (add_distance_columns.R).
#'    Distances are added by age and gender category if there exists such information for
#'    the injury counts (injuries_for_model dataframe). If there exists a fatality for some casualty and
#'    strike mode and age and sex category but no mode distance for this age and sex category,
#'    then fatalities and distances are aggregated by strike and casualty mode.
#'    If, after the aggregation there still exist fatalities for which either casualty
#'    or strike mode distance are missing, then these fatalities are removed as we cannot
#'    predict injury counts on zero distances. However, this should not happen
#'    as we should have total distances for all modes (possibly inferred
#'    from other modes) that appear in the injury data. - This data is used to parameterise the
#'    Poisson injury model.
#'
#'  \item A new list (injuries_list) is created containing all strike and casualty mode and age and sex combinations
#'    together with strike and casualty mode distances (\code{\link{add_distance_columns()}}) for the baseline and all scenarios. For
#'    the whw model, any strike mode and casualty pairs where strike mode equals casualty mode
#'    are removed as fatalities for these combinations have already been added to the nov matrix.
#'    Combinations which do not have a non-zero strike or casualty mode distance
#'    are also removed. This list will later be used in the \code{\link{injuries_function_2()}} function
#'    to predict fatality counts using the Poisson injury regression model.
#'
#'  \item The casualty and strike mode exponents used to account for the safety in number effect
#'    are added to both the injuries_for_model and injuries_list.
#'
#'  \item	The best possible regression model is being built using Baseline injury counts and distances
#'    (injuries_for_model) such that the standard errors are small wherever possible. Strike
#'    and casualty mode pairs where cas mode = strike mode are removed if they still exist
#'    which they should not as they should have been removed by the ithim_load_data.R function.
#'    Two different forms for whw and nov matrices are defined, taking into account age and sex
#'    information where it exists. The standard errors of the newly built regression models
#'    are checked and if they are too large and if the data has not been aggregated by age
#'    and sex yet, then the data is aggregated and a new Poisson regression model is build.
#'    If the standard errors are still large after this aggregation, then a message is
#'    printed to the screen warning that the standard errors are large.
#' }
#'
#'
#' @param journeys data frame with total distance (by total population) for each age and sex category and for each scenario
#' @param dist table of (total population) distances per mode
#'
#' @return true_distances (mode distances by age and sex with all walking modes and all car modes combined and bus drivers added where relevant),
#' @return injuries_list (list of all strike, casualty, age, sex and mode distance combinations for baseline and all scenarios),
#' @return reg_model (parameterised Poisson regression model),
#' @return injuries_for_model (baseline data containing injury counts for all casualty and strike mode combinations with associated distance data)
#'
#' @export
#'

distances_for_injury_function <- function(journeys, dist) {
  distances <- spread(journeys, stage_mode, tot_dist, fill = 0)

  if ("walk_to_pt" %in% names(distances)) { # add walk to pt to pedestrian distance to have one measure for walking
    distances$pedestrian <- distances$pedestrian + distances$walk_to_pt
  }
  
  # multiply taxi distances by 2 to add taxi driver
  # if there are multiple people in the taxi apart from the driver this will increase
  # taxi distances by too much, however taxis are also spending a significant distance
  # driving around without passengers which is not captured in travel surveys
  if ('taxi' %in% colnames(distances)) distances$taxi <- distances$taxi * 2
  
  if ('shared_taxi' %in% colnames(distances))  distances$shared_taxi <- distances$shared_taxi * 2
  
  # multiply auto_rickshaw by 2 to add driver
  if ('auto_rickshaw' %in% colnames(distances)) distances$auto_rickshaw <- distances$auto_rickshaw * 2
  
  ## add all car related distances to car distance
  distances$car <- rowSums(distances[, colnames(distances) %in% c("car", "taxi", "shared_auto", "shared_taxi")])
  distances <- distances[, -which(names(distances) %in% c("taxi", "shared_auto", "shared_taxi", "walk_to_pt"))]

  true_distances_0 <- distances
  true_distances_0$sex_age <- paste0(true_distances_0$sex, "_", true_distances_0$age_cat) # add one sex and age column

  # add bus driver distances to total bus distances to cover all by people travelling by bus
  if (ADD_BUS_DRIVERS) true_distances_0$bus <- true_distances_0$bus + true_distances_0$bus_driver

  true_distances <- true_distances_0

  # find mode names
  mode_names <- names(true_distances)[!names(true_distances) %in% c("age_cat", "scenario", "sex_age", "sex")]


  injury_table <- INJURY_TABLE

  # add injury_reporting rate
  for (type in INJURY_TABLE_TYPES) {
    INJURY_TABLE[[type]]$injury_reporting_rate <- INJURY_REPORTING_RATE
  }

  INJURY_TABLE <<- INJURY_TABLE


  ## add distance columns for baseline to injury data
  injuries_for_model <- add_distance_columns(injury_table,
    mode_names,
    true_distances_0,
    dist,
    scenarios = SCEN[1]
  )

  # determine whether there are any age and sex combinations for which we don't have any distance information
  zero_dist <- list()
  zero_dist_pos_inj <- list()

  # flag to highlight if some age and gender categories have at least one fatality but zero distance for a strike and casualty mode combination
  zero_dist_flag <- F

  # finds cas and strike mode combinations for which there exist zero distances
  for (type in INJURY_TABLE_TYPES) {
    zero_dist[[type]] <- subset(injuries_for_model$baseline[[type]], strike_distance == 0 | cas_distance == 0)
    zero_dist_pos_inj[[type]] <- subset(zero_dist[[type]], count > 0)
    if (nrow(zero_dist_pos_inj[[type]]) > 0) {
      zero_dist_flag <- T
    }
  }


  # if there exists at least one age and sex category with at least one fatality but with
  # zero distance for a strike and casualty mode combination, aggregate by age and sex
  if (zero_dist_flag == T) {
    for (type in INJURY_TABLE_TYPES) {
      injuries_df <- injuries_for_model$baseline[[type]]
      setDT(injuries_df)
      injuries_for_model$baseline[[type]] <- as.data.frame(injuries_df[, .(
        count = sum(count), weight = mean(weight), strike_distance_sum = mean(strike_distance_sum),
        cas_distance_sum = mean(cas_distance_sum)
      ), by = c("cas_mode", "strike_mode")])
      injuries_for_model$baseline[[type]]$strike_distance <- injuries_for_model$baseline[[type]]$strike_distance_sum
      injuries_for_model$baseline[[type]]$cas_distance <- injuries_for_model$baseline[[type]]$cas_distance_sum
    }
  }

  # remove any injuries for which we don't have either casualty or strike mode distance
  for (type in INJURY_TABLE_TYPES) {
    injuries_for_model$baseline[[type]] <- subset(injuries_for_model$baseline[[type]], strike_distance > 0 & cas_distance > 0)
  }


  # create list where each element contains all age, sex, casualty and strike mode combination for
  # the baseline and all scenarios
  scenario_injury_table <- list()
  for (type in INJURY_TABLE_TYPES) {
    scenario_injury_table[[type]] <- expand.grid(
      age_cat = unique(DEMOGRAPHIC$age),
      cas_gender = unique(DEMOGRAPHIC$sex),
      cas_mode = unique(injuries_for_model[[1]][[type]]$cas_mode),
      strike_mode = unique(injuries_for_model[[1]][[type]]$strike_mode)
    )
  }


  # add distance information for the baseline and all scenarios
  injuries_list <- add_distance_columns(injury_table = scenario_injury_table, mode_names, true_distances_0, dist)


  for (n in 1:(NSCEN + 1)) { # loop through baseline and all scenarios
    for (type in INJURY_TABLE_TYPES) {
      # remove zero distances
      injuries_list[[n]][[type]] <- subset(
        injuries_list[[n]][[type]],
        strike_distance > 0 & cas_distance > 0
      )
      injuries_list[[n]][[type]]$injury_gen_age <- apply(
        cbind(
          as.character(injuries_list[[n]][[type]]$cas_gender),
          as.character(injuries_list[[n]][[type]]$age_cat)
        ), 1,
        function(x) paste(x, collapse = "_")
      ) # create an age sex column
      # remove strike and cas mode pairs where cas mode = strike mode as these are considered nov accidents
      injuries_list[[n]][[type]]$cas_strike_mode <- apply(
        cbind(
          as.character(injuries_list[[n]][[type]]$cas_mode),
          as.character(injuries_list[[n]][[type]]$strike_mode)
        ), 1,
        function(x) paste(x, collapse = "_")
      )
      injuries_list[[n]][[type]] <- injuries_list[[n]][[type]] %>%
        filter(cas_strike_mode != "car_car" & cas_strike_mode != "bus_bus" &
          cas_strike_mode != "motorcycle_motorcycle" &
          cas_strike_mode != "cycle_cycle" &
          cas_strike_mode != "truck_truck")
    }
  }

  ## determine safety in number coefficients and add as columns to injuries_list and injuries_for_model
  if (CALL_INDIVIDUAL_SIN == F) { # if we have the same coefficients for all modes
    CAS_EXPONENT <<- SIN_EXPONENT_SUM * CASUALTY_EXPONENT_FRACTION
    STR_EXPONENT <<- SIN_EXPONENT_SUM - CAS_EXPONENT

    # when running in sampling mode, ensure that cas_exponent and str_exponent are always below 1
    if (CAS_EXPONENT > 1) CAS_EXPONENT <<- 1
    if (STR_EXPONENT > 1) STR_EXPONENT <<- 1

    for (type in INJURY_TABLE_TYPES) { # add the exponents to the the baseline table and also the scenario tables
      injuries_for_model$baseline[[type]]$cas_exponent_col <- CAS_EXPONENT
      injuries_for_model$baseline[[type]]$str_exponent_col <- STR_EXPONENT

      for (n in 1:(NSCEN + 1)) {
        injuries_list[[n]][[type]]$cas_exponent_col <- CAS_EXPONENT
        injuries_list[[n]][[type]]$str_exponent_col <- STR_EXPONENT
      }
    }
  }


  if (CALL_INDIVIDUAL_SIN == T) { # assign coefficients depending on strike and victim modes

    injuries_for_model$baseline$whw$cas_exponent_col <- SIN_EXPONENT_SUM_VEH * CASUALTY_EXPONENT_FRACTION_VEH
    injuries_for_model$baseline$whw$str_exponent_col <- SIN_EXPONENT_SUM_VEH - (SIN_EXPONENT_SUM_VEH * CASUALTY_EXPONENT_FRACTION_VEH)


    injuries_for_model$baseline$whw$cas_exponent_col[injuries_for_model$baseline$whw$cas_mode == "cycle"] <- SIN_EXPONENT_SUM_CYCLE * CASUALTY_EXPONENT_FRACTION_CYCLE
    injuries_for_model$baseline$whw$str_exponent_col[injuries_for_model$baseline$whw$cas_mode == "cycle"] <- SIN_EXPONENT_SUM_CYCLE - (SIN_EXPONENT_SUM_CYCLE * CASUALTY_EXPONENT_FRACTION_CYCLE)

    injuries_for_model$baseline$whw$cas_exponent_col[injuries_for_model$baseline$whw$cas_mode == "pedestrian"] <- SIN_EXPONENT_SUM_PED * CASUALTY_EXPONENT_FRACTION_PED
    injuries_for_model$baseline$whw$str_exponent_col[injuries_for_model$baseline$whw$cas_mode == "pedestrian"] <- SIN_EXPONENT_SUM_PED - (SIN_EXPONENT_SUM_PED * CASUALTY_EXPONENT_FRACTION_PED)

    injuries_for_model$baseline$nov$cas_exponent_col <- SIN_EXPONENT_SUM_NOV
    injuries_for_model$baseline$nov$str_exponent_col <- 1

    # when running in sampling mode, ensure that cas_exponent and str_exponent are always below 1
    injuries_for_model$baseline$whw$cas_exponent_col <- ifelse(injuries_for_model$baseline$whw$cas_exponent_col > 1, 1, injuries_for_model$baseline$whw$cas_exponent_col)
    injuries_for_model$baseline$whw$str_exponent_col <- ifelse(injuries_for_model$baseline$whw$str_exponent_col > 1, 1, injuries_for_model$baseline$whw$str_exponent_col)
    injuries_for_model$baseline$nov$cas_exponent_col <- ifelse(injuries_for_model$baseline$nov$cas_exponent_col > 1, 1, injuries_for_model$baseline$nov$cas_exponent_col)

    for (n in 1:(NSCEN + 1)) { # assign exponents to the scenario tables
      injuries_list[[n]]$whw$cas_exponent_col <- SIN_EXPONENT_SUM_VEH * CASUALTY_EXPONENT_FRACTION_VEH
      injuries_list[[n]]$whw$str_exponent_col <- SIN_EXPONENT_SUM_VEH - (SIN_EXPONENT_SUM_VEH * CASUALTY_EXPONENT_FRACTION_VEH)

      injuries_list[[n]]$whw$cas_exponent_col[injuries_list[[n]]$whw$cas_mode == "cycle"] <- SIN_EXPONENT_SUM_CYCLE * CASUALTY_EXPONENT_FRACTION_CYCLE
      injuries_list[[n]]$whw$str_exponent_col[injuries_list[[n]]$whw$cas_mode == "cycle"] <- SIN_EXPONENT_SUM_CYCLE - (SIN_EXPONENT_SUM_CYCLE * CASUALTY_EXPONENT_FRACTION_CYCLE)

      injuries_list[[n]]$whw$cas_exponent_col[injuries_list[[n]]$whw$cas_mode == "pedestrian"] <- SIN_EXPONENT_SUM_PED * CASUALTY_EXPONENT_FRACTION_PED
      injuries_list[[n]]$whw$str_exponent_col[injuries_list[[n]]$whw$cas_mode == "pedestrian"] <- SIN_EXPONENT_SUM_PED - (SIN_EXPONENT_SUM_PED * CASUALTY_EXPONENT_FRACTION_PED)

      # when running in sampling mode, ensure that cas_exponent and str_exponent are always below 1
      injuries_list[[n]]$whw$cas_exponent_col <- ifelse(injuries_list[[n]]$whw$cas_exponent_col > 1, 1, injuries_list[[n]]$whw$cas_exponent_col)
      injuries_list[[n]]$whw$str_exponent_col <- ifelse(injuries_list[[n]]$whw$str_exponent_col > 1, 1, injuries_list[[n]]$whw$str_exponent_col)
    }

    # when running in sampling mode, ensure that cas_exponent and str_exponent are always below 1
    injuries_for_model$baseline$nov$cas_exponent_col <- min(SIN_EXPONENT_SUM_NOV, 1)
    injuries_for_model$baseline$nov$str_exponent_col <- 1


    for (n in 1:(NSCEN + 1)) {
      # when running in sampling mode, ensure that cas_exponent and str_exponent are always below 1
      injuries_list[[n]]$nov$cas_exponent_col <- min(SIN_EXPONENT_SUM_NOV, 1)
      injuries_list[[n]]$nov$str_exponent_col <- 1
    }
  }


  ########################### Build regression model

  # run regression model on baseline data
  reg_model <- list()

  # define form of regression models for both whw and nov matrices
  forms <- list(
    whw = "count~cas_mode+strike_mode+offset(log(cas_distance)+(cas_exponent_col-1)*log(cas_distance_sum)+log(strike_distance)+
                    (str_exponent_col-1)*log(strike_distance_sum)+log(injury_reporting_rate)+log(weight))",
    nov = "count~cas_mode+offset(log(cas_distance)+(cas_exponent_col-1)*log(cas_distance_sum)+log(injury_reporting_rate)+log(weight))"
  )

  # add age and sex information if this information exists in the injuries_for_model data
  if ("age_cat" %in% names(injuries_for_model[[1]][[1]])) {
    for (type in INJURY_TABLE_TYPES) {
      forms[[type]] <- paste0(c(forms[[type]], "age_cat"), collapse = "+")
    }
  }
  if ("cas_gender" %in% names(injuries_for_model[[1]][[1]])) {
    for (type in INJURY_TABLE_TYPES) {
      forms[[type]] <- paste0(c(forms[[type]], "cas_gender"), collapse = "+")
    }
  }

  max_std <- list() # store largest standard errors
  max_std_def <- 10


  # try most sophisticated model first and then successively simplify model
  for (type in INJURY_TABLE_TYPES) {
    injuries_for_model[[1]][[type]]$injury_reporting_rate <- as.numeric(INJURY_REPORTING_RATE)

    # remove strike and cas mode pairs where cas mode = strike mode
    injuries_for_model[[1]][[type]]$cas_strike_mode <- apply(cbind(
      as.character(injuries_for_model[[1]][[type]]$cas_mode),
      as.character(injuries_for_model[[1]][[type]]$strike_mode)
    ), 1, function(x) paste(x, collapse = "_"))
    injuries_for_model[[1]][[type]] <- injuries_for_model[[1]][[type]] %>%
      filter(cas_strike_mode != "car_car" &
        cas_strike_mode != "bus_bus" &
        cas_strike_mode != "motorcycle_motorcycle" &
        cas_strike_mode != "cycle_cycle" &
        cas_strike_mode != "truck_truck")



    test <- "try-error"


    # try poisson distribution
    test <- try(glm(as.formula(forms[[type]]), data = injuries_for_model[[1]][[type]], family = "poisson"))

    if (length(test) != 1) {
      max_std[[type]] <- max(summary(test)$coefficients[, 2])
    } else {
      max_std[[type]] <- 1000
    }


    # if either standard errors are high or if no model could be built, try building model without age and gender categories if not already done
    if (length(test) == 1 | max_std[[type]] > max_std_def) {
      # remove age and gender if possible and fit model without age and gender categories
      if ("age_cat" %in% names(injuries_for_model[[1]][[1]])) {
        injuries_df <- injuries_for_model$baseline[[type]] # %>% dplyr::select(-c(age_cat, cas_gender))
        setDT(injuries_df)
        injuries_for_model$baseline[[type]] <- as.data.frame(injuries_df[, .(
          count = sum(count), weight = mean(weight), strike_distance_sum = mean(strike_distance_sum),
          cas_distance_sum = mean(cas_distance_sum),
          cas_exponent_col = mean(cas_exponent_col),
          str_exponent_col = mean(str_exponent_col)
        ), by = c("cas_mode", "strike_mode")])
        injuries_for_model$baseline[[type]]$strike_distance <- injuries_for_model$baseline[[type]]$strike_distance_sum
        injuries_for_model$baseline[[type]]$cas_distance <- injuries_for_model$baseline[[type]]$cas_distance_sum
        injuries_for_model[[1]][[type]]$injury_reporting_rate <- as.numeric(INJURY_REPORTING_RATE)

        # try model without age and gender categories
        forms_no_agecat <- list(
          whw = "count~cas_mode+strike_mode+offset(log(cas_distance)+(cas_exponent_col-1)*log(cas_distance_sum)
                                            +log(strike_distance)+(str_exponent_col-1)*log(strike_distance_sum)+
                                            log(injury_reporting_rate)+log(weight))",
          nov = "count~cas_mode+offset(log(cas_distance)+(cas_exponent_col-1)*log(cas_distance_sum)+log(injury_reporting_rate)+log(weight))"
        )

        # re-run regression model
        test <- try(glm(as.formula(forms_no_agecat[[type]]), data = injuries_for_model[[1]][[type]], family = "poisson"))
      }
    }

    # test whether standard error of newly built regression model is above cut-off distance and print
    # warning message if so
    if (length(test) != 1) {
      max_std[[type]] <- max(summary(test)$coefficients[, 2])
    } else {
      max_std[[type]] <- 0
    }

    if (max_std[[type]] > max_std_def) {
      print(paste0("!! The ", type, " injury model for ", city, " has large standard errors!!!"))
    }


    reg_model[[type]] <- test
    test <- NULL
  }

  return(list(true_distances = true_distances, injuries_list = injuries_list, reg_model = reg_model, injuries_for_model = injuries_for_model))
}
