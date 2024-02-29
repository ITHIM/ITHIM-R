#' Calculate total AP exposure per person
#'
#' Calculate total AP exposure per person based on population and personal travel
#'
#' This function performs the following steps:
#'
#' \itemize{
#' \item the exposure factor rates by activity are defined - these parameters are fixed
#'
#' \item calculate pm concentration not related to transport
#'
#' \item calculate PM emission factors for each mode by dividing total emissions by distances travelled
#'
#' \item calculate PM emissions for each mode in each scenario by multiplying the scenario distance
#'   by the emission factors
#'
#' \item for modes without any assigned distance, use the PM emissions from the VEHICLE_INVENTORY instead
#'
#' \item calculate the total PM concentrations for each scenario
#'
#' \item add exposure factors to the trip set by stage mode
#'
#' \item add total scenario PM concentrations to the trip set
#'
#' \item calculate ventilation rate for each stage taking into account demographic characteristics and exposure factors
#'
#' \item calculate the inhaled air and total PM (in micro grams) in the trip set
#'
#' \item calculate the amount of time per day spent in sleep, moderate and vigorous
#'   activities
#'
#' \item add total time spent travelling by each participant to the trip set
#'
#' \item calculate ventilation rate for sleep, moderate and vigorous activities
#'
#' \item for each participant in the synthetic population (with travel component),
#'   calculate the total air inhaled, the total PM inhaled and
#'   the total PM concentration inhaled for each scenario
#'
#' \item assign all participants in the synthetic population without travel component,
#'   the baseline or scenario PM concentrations
#'
#' \item join all people with and without travel in the synthetic population
#' }
#'
#'
#' @param dist total distance travelled by mode by the population for all scenarios
#' @param trip_scen_sets trips data frame of all trips from all scenarios
#'
#' @return background PM concentration for baseline and all scenarios
#' @return total AP exposure per person in the synthetic population (for baseline and scenarios)
#'
#' @export
scenario_pm_calculations <- function(dist, trip_scen_sets) {
  print("Using new implementation of ventilation rates...")

  # Exposure factor rate by activity (the ratio between that mode’s PM2.5 and the background’s PM2.5)
  exp_facs <- data.frame(
    stage_mode = c("car", "taxi", "bus", "rail", "cycle", "pedestrian", "motorcycle"),
    e_rate = c(2.5, 2.5, 1.9, 1.9, 2.0, 1.6, 2.0)
  )

  # concentration contributed by non-transport share (remains constant across the scenarios)
  non_transport_pm_conc <- PM_CONC_BASE * (1 - PM_TRANS_SHARE)

  # adding in travel not covered in the synthetic trip set
  emission_dist <- dist

  ## get emission factor by dividing inventory by baseline distance. (We don't need to scale to a whole year, as we are just scaling the background concentration.)
  ordered_efs <- (VEHICLE_INVENTORY$PM_emission_inventory[match(emission_dist$stage_mode, VEHICLE_INVENTORY$stage_mode)] %>% as.numeric()) / (emission_dist$baseline %>% as.numeric())
  ## get new emission by multiplying emission factor by scenario distance.
  trans_emissions <- emission_dist[, SCEN] * t(repmat(ordered_efs, NSCEN + 1, 1))

  ## augment with travel emission contributions that aren't included in distance calculation
  for (mode_type in which(!VEHICLE_INVENTORY$stage_mode %in% emission_dist$stage_mode)) {
    trans_emissions[nrow(trans_emissions) + 1, ] <- VEHICLE_INVENTORY$PM_emission_inventory[mode_type]
  }

  ## scenario travel pm2.5 calculated as relative to the baseline
  baseline_sum <- sum(trans_emissions[[SCEN[1]]], na.rm = T)
  conc_pm <- c()
  ## in this sum, the non-transport pm is constant; the transport emissions scale the transport contribution (PM_TRANS_SHARE) to the base level (PM_CONC_BASE)
  for (i in 1:length(SCEN)) {
    conc_pm[i] <- non_transport_pm_conc + PM_TRANS_SHARE * PM_CONC_BASE * sum(trans_emissions[[SCEN[i]]], na.rm = T) / baseline_sum
  }

  # Copy trips dataset
  trip_set <- trip_scen_sets

  # Merge baseline PA
  trip_set <- dplyr::left_join(trip_set,
    SYNTHETIC_POPULATION[, c(
      "participant_id",
      "work_ltpa_marg_met"
    )],
    by = "participant_id"
  )

  # Rename short walks as pedestrian
  trip_set$stage_mode[trip_set$stage_mode == "walk_to_pt"] <- "pedestrian"

  # Join trip_set and exponent factors df
  trip_set <- dplyr::left_join(trip_set, exp_facs, "stage_mode")
  
  #----
  # Dan: These new lines of code are for the ventilation rate

  # Dan: Global path to read ventilation rate's distributions
  global_path <- paste0(file.path(
    find.package("ithimr", lib.loc = .libPaths()),
    "extdata/global"
  ), "/")

  # Dan: Read parameter distributions
  ## Dan: Body mass
  body_mass_df <- read.csv(paste0(global_path, "ventilation_rate/BodyMass.csv"))
  colnames(body_mass_df)[1] <- "age"

  ## Dan: Energy Conversion Factor (ECF)
  ecf_df <- read.csv(paste0(global_path, "ventilation_rate/ECF.csv"))
  colnames(ecf_df)[1] <- "age"

  ## Dan: Resting Metabolic Rate (RMR)
  rmr_df <- read.csv(paste0(global_path, "ventilation_rate/RMR.csv"))
  colnames(rmr_df)[1] <- "age"

  ## Dan: Normalized maximum oxygen uptake rate (NVO2max)
  nvo2max_df <- read.csv(paste0(global_path, "ventilation_rate/NVO2max.csv"))
  colnames(nvo2max_df)[1] <- "age"

  ## Dan: Ventilation from Oxygen Uptake (vent_from_oxygen)
  vent_from_oxygen_df <- read.csv(paste0(global_path, "ventilation_rate/VentilationFromOxygenUptake.csv"))
  colnames(vent_from_oxygen_df)[1] <- "age"

  # Dan: MET values for each mode/activity. These values come from the Compendium
  # Dan: Lambed told me that we need v-rates for everyone. I assigned to
  # auto_rickshaw and other the same MET values as bus and cycle.
  # TODO: I don't know if we should add MET values for drivers in ghost trips
  # (bus, car, motorcycle, truck). Right now these are not included because they
  # have participant_id = 0.
  met_df <- data.frame(
    stage_mode = c(
      "car", "taxi", "bus", "rail", "cycle", "pedestrian",
      "sleep", "motorcycle", "auto_rickshaw", "other",
      "moderate", "vigorous", "leisure", "light_activities"
    ),
    met = c(
      CAR_DRIVER_MET, PASSENGER_MET, PASSENGER_MET, PASSENGER_MET,
      CYCLING_MET, WALKING_MET, 0.95, MOTORCYCLIST_MET, PASSENGER_MET,
      PASSENGER_MET, MODERATE_PA_MET, VIGOROUS_PA_MET,
      SEDENTARY_ACTIVITY_MET, LIGHT_ACTIVITY_MET
    ),
    compendium_code = c(
      "16010", "16015", "16016", "16016", "01011",
      "16060", "07030", "16030", "16016", "16016", "GPAQ",
      "GPAQ", "05080", "05080"
    )
  )

  # Dan: Extract people from the synthetic population to calculate their
  # ventilation rates
  people_for_vent_rates <- trip_set %>%
    filter(participant_id != 0) %>%
    distinct(participant_id, .keep_all = T) %>%
    dplyr::select(participant_id, age, sex)

  # Dan: Draw from a log-normal distribution the body mass [kg] of each person
  # in the synthetic population
  people_for_vent_rates <- people_for_vent_rates %>%
    left_join(body_mass_df, by = c("age", "sex")) %>%
    rowwise() %>% # To apply an operation in each row
    mutate(
      # Draw sample from distribution
      sample = rlnorm(1, log(gm), log(gsd)),
      # Check maximum and minimum values
      body_mass = ifelse(sample < lower, lower,
        ifelse(sample > upper, upper, sample)
      )
    ) %>%
    dplyr::select(participant_id, age, sex, body_mass)

  # Dan: Draw from a uniform distribution the Energy Conversion Factor [lt/kcal]
  # of each person in the synthetic population
  people_for_vent_rates <- people_for_vent_rates %>%
    left_join(ecf_df, by = c("age", "sex")) %>%
    rowwise() %>% # To apply an operation in each row
    mutate(
      # Draw sample from distribution
      ecf = runif(1, min = lower, max = upper),
    ) %>%
    dplyr::select(participant_id, age, sex, body_mass, ecf)

  # Dan: For each person in the synthetic population, calculate the Resting
  # Metabolic Rate [kcal/min] from a given regression formula with a normally
  # distributed error
  people_for_vent_rates <- people_for_vent_rates %>%
    left_join(rmr_df, by = c("age", "sex")) %>%
    rowwise() %>% # To apply an operation in each row
    mutate(
      # Draw sample from distribution
      error = rnorm(1, 0, se),
      # Calculate RMR
      rmr = 0.166 * (interc + (slope * body_mass) + error)
    ) %>%
    dplyr::select(participant_id, age, sex, body_mass, ecf, rmr)

  # Dan: Draw from a normalized maximum oxygen uptake rate (NVO2max) [lt/(min*kg)]
  # of each person in the synthetic population
  people_for_vent_rates <- people_for_vent_rates %>%
    left_join(nvo2max_df, by = c("age", "sex")) %>%
    rowwise() %>% # To apply an operation in each row
    mutate(
      # Draw sample from distribution
      sample = rnorm(1, mean, sd),
      # Check maximum and minimum values
      sample_check = ifelse(sample < lower, lower,
        ifelse(sample > upper, upper, sample)
      ),
      # Transform [ml/(min*kg)] to [lt/(min*kg)]
      nvo2max = sample_check / 1000
    ) %>%
    dplyr::select(participant_id, age, sex, body_mass, ecf, rmr, nvo2max)

  # Dan: Calculate the maximum oxygen uptake rate [lt/min] of each person in the
  # synthetic population
  people_for_vent_rates <- people_for_vent_rates %>%
    mutate(vo2max = nvo2max * body_mass)

  # Dan: Get the parameters for the empirical equation to calculate the
  # Ventilation Rate from Oxygen Uptake Rate in each person in the
  # synthetic population
  people_for_vent_rates <- people_for_vent_rates %>%
    left_join(vent_from_oxygen_df, by = c("age", "sex")) %>%
    rowwise() %>% # To apply an operation in each row
    mutate(
      # Draw sample from normal distribution taking into account the variance
      # between persons
      d_k = rnorm(1, 0, sd_person_level)
    ) %>%
    dplyr::select(
      participant_id, body_mass, ecf, rmr, nvo2max, vo2max,
      intercept_a, slope_b, sd_person_level, sd_test_level, d_k
    )

  # Dan: Adding these new columns to the trip set
  trip_set <- trip_set %>%
    left_join(people_for_vent_rates, by = "participant_id")

  # Dan: Adding MET values [dimensionless] for each mode
  trip_set <- trip_set %>% left_join(met_df %>% dplyr::select(stage_mode, met),
    by = "stage_mode"
  )

  # Dan: Calculate new variables for each stage
  #   - vo2 = oxygen uptake [lt/min]
  #   - pct_vo2max = upper limit of VO2max in percentage [%]
  #   - upper_vo2max = permitted upper limit of VO2 [lt/min]
  #   - adj_vo2 = adjusted oxygen uptake [lt/min]
  #   - e_ijk = error to account variance between activities
  #   - log_vent_rate = log of ventilation rate (empirical equation)
  #   - vent_rate = ventilation rate by removing the log in the empirical equation [lt/min]
  #   - v_rate = ventilation rate in different units of measurement [m3/h]
  trip_set <- trip_set %>%
    filter(participant_id != 0) %>%
    rowwise() %>%
    mutate(
      # Ventilation rate for travel modes
      vo2 = ecf * met * rmr,
      pct_vo2max = ifelse(stage_duration < 5, 100,
        ifelse(stage_duration > 540, 33,
          121.2 - (14 * log(stage_duration))
        )
      ),
      upper_vo2max = vo2max * (pct_vo2max / 100),
      adj_vo2 = ifelse(vo2 > upper_vo2max, upper_vo2max, vo2),
      # Draw sample from normal distribution taking into account the variance
      # between activities
      e_ijk = rnorm(1, 0, sd_test_level),
      log_vent_rate = intercept_a + (slope_b * log(adj_vo2 / body_mass)) + d_k + e_ijk,
      vent_rate = exp(log_vent_rate) * body_mass,
      v_rate = vent_rate * 60 / 1000
    ) %>%
    bind_rows(trip_set %>% filter(participant_id == 0))
  #----

  # Create df with scenarios and concentration
  conc_pm_df <- data.frame(
    scenario = SCEN,
    conc_pm = conc_pm
  )

  # Join trip_set with PM concentration df
  trip_set <- left_join(trip_set, conc_pm_df, by = "scenario")

  # cubic meters of air inhaled are the product of the ventilation rate and the
  # time (hours/60) spent travelling by that mode
  trip_set$air_inhaled <- trip_set$stage_duration / 60 * trip_set$v_rate

  # PM inhaled (micro grams) = duration * ventilation rate * exposure rates * concentration
  trip_set$pm_inhaled <- trip_set$stage_duration / 60 * trip_set$v_rate * trip_set$e_rate * trip_set$conc_pm

  # Calculate total_travel_time_hrs of stage_duration
  trip_set <- trip_set %>%
    group_by(participant_id, scenario) %>%
    mutate(total_travel_time_hrs = sum(stage_duration) / 60) %>%
    ungroup()


  # Extract mets for sleep, rest, moderate and vigorous
  sleep_met <- met_df %>%
    filter(stage_mode == "sleep") %>%
    dplyr::select(met) %>%
    as.numeric()
  moderate_met <- met_df %>%
    filter(stage_mode == "moderate") %>%
    dplyr::select(met) %>%
    as.numeric()
  vigorous_met <- met_df %>%
    filter(stage_mode == "vigorous") %>%
    dplyr::select(met) %>%
    as.numeric()
  leisure_met <- met_df %>%
    filter(stage_mode == "leisure") %>%
    dplyr::select(met) %>%
    as.numeric()
  light_met <- met_df %>%
    filter(stage_mode == "light_activities") %>%
    dplyr::select(met) %>%
    as.numeric()

  #----
  # AA: Changed 3.15 to 3.2 (leisure_hours) and 10.75 to 10.8 (light_hours)
  # Dan: We are saying that time spent for any person is:
  # Sleep: 8.3 hours
  # Leisure sedentary screen time: 3.2 hours
  # Light activities: 10.8 hours
  # For a total of 22.3 hours
  # Since it is possible that the unknown time is less than 22.2, I am going
  # to use proportions for now to get values for these activities.
  # There are a couple of rules to consider:
  # - When sleep time is less than 6 hours, we have to assign it to 6 and split
  # proportionally the remaining time in leisure and light activities
  # - When the known time is greater than 18 hours, we assign it to 18 and sleep
  # to 6 hours. Leisure and light activities will be zero in this case

  sleep_hours <- 8.3
  leisure_hours <- 3.2
  light_hours <- 10.8
  
  # Calculate total air and pm inhaled in each person
  synth_pop <- trip_set %>%
    filter(participant_id != 0) %>%
    group_by(participant_id, scenario) %>%
    # reframe instead of summarise in the latest version
    summarise(
      total_travel_time_hrs = max(total_travel_time_hrs, na.rm = T),
      travel_air_inhaled = sum(air_inhaled, na.rm = T),
      travel_pm_inhaled = sum(pm_inhaled, na.rm = T),
      work_ltpa_marg_met = max(work_ltpa_marg_met, na.rm = T)
    ) %>%
    distinct(participant_id, scenario, .keep_all = T) %>%
    # Merge participant information to calculate ventilation rates for sleep and rest
    left_join(people_for_vent_rates, by = "participant_id") %>%
    # Merge scenario concentration
    left_join(conc_pm_df, by = "scenario") %>%
    # Calculate vent_rates and air inhaled in the same way as with travel modes
    rowwise() %>%
    mutate(
      # Transforming work_ltpa_marg_met to a daily value
      daily_work_ltpa_marg_met = work_ltpa_marg_met / 7,
      # Calculate time spent in moderate and vigorous activities from work_ltpa_marg_met
      # I subtract 1 because the initial values are MET not Marginal MET
      time_moderate = (daily_work_ltpa_marg_met * MODERATE_PA_CONTRIBUTION) / (moderate_met - 1),
      time_vigorous = (daily_work_ltpa_marg_met * (1 - MODERATE_PA_CONTRIBUTION)) / (vigorous_met - 1),
      # Calculate known time
      known_time = total_travel_time_hrs + time_moderate + time_vigorous,
      known_time = ifelse(known_time > 18, 18, known_time), # Just to avoid outliers
      # Calculate unknown time
      unknown_time = 24 - known_time,

      # Calculate ventilation rate for each activity (durations are calculated from the unknown time)
      ## Sleep
      sleep_duration = (unknown_time * (sleep_hours / (sleep_hours + leisure_hours + light_hours))) * 60, # In minutes
      ### Conditional to check if sleep duration is less than 6 hours
      sleep_less_6h = ifelse(sleep_duration < (6 * 60), 1, 0),
      ### Assign 6 hours when sleep time is less than 6
      sleep_duration = ifelse(sleep_less_6h == 1, (6 * 60), sleep_duration),
      vo2_sleep = ecf * sleep_met * rmr,
      pct_vo2max_sleep = ifelse(sleep_duration < 5, 100,
        ifelse(sleep_duration > 540, 33,
          121.2 - (14 * log(sleep_duration))
        )
      ),
      upper_vo2max_sleep = vo2max * (pct_vo2max_sleep / 100),
      adj_vo2_sleep = ifelse(vo2_sleep > upper_vo2max_sleep,
        upper_vo2max_sleep, vo2_sleep
      ),
      ### Draw sample from normal distribution taking into account the variance
      ### between activities
      e_ijk_sleep = rnorm(1, 0, sd_test_level),
      log_vent_rate_sleep = intercept_a + (slope_b * log(adj_vo2_sleep / body_mass)) + d_k + e_ijk_sleep,
      vent_rate_sleep = exp(log_vent_rate_sleep) * body_mass,
      v_rate_sleep = vent_rate_sleep * 60 / 1000,
      ### Calculate air and pm inhaled
      sleep_air_inhaled = sleep_duration / 60 * v_rate_sleep,
      sleep_pm_inhaled = sleep_duration / 60 * v_rate_sleep * conc_pm,


      ## Leisure sedentary screen time
      #### Leisure has a correction if sleep time is less than 6 hours
      leisure_duration = ifelse(sleep_less_6h == 1,
        (unknown_time - 6) * (leisure_hours / (leisure_hours + light_hours)), 
        unknown_time * (leisure_hours / (sleep_hours + leisure_hours + light_hours))
      ) * 60, # In minutes for both if and else values
      leisure_duration = ifelse(leisure_duration < 0, 0, leisure_duration), # This happened in 35 rows out of the 132k
      vo2_leisure = ecf * leisure_met * rmr,
      pct_vo2max_leisure = ifelse(leisure_duration < 5, 100,
        ifelse(leisure_duration > 540, 33,
          121.2 - (14 * log(leisure_duration))
        )
      ),
      upper_vo2max_leisure = vo2max * (pct_vo2max_leisure / 100),
      adj_vo2_leisure = ifelse(vo2_leisure > upper_vo2max_leisure,
        upper_vo2max_leisure, vo2_leisure
      ),
      ## Draw sample from normal distribution taking into account the variance
      ## between activities
      e_ijk_leisure = rnorm(1, 0, sd_test_level),
      log_vent_rate_leisure = intercept_a + (slope_b * log(adj_vo2_leisure / body_mass)) + d_k + e_ijk_leisure,
      vent_rate_leisure = exp(log_vent_rate_leisure) * body_mass,
      v_rate_leisure = vent_rate_leisure * 60 / 1000,
      # Calculate air and pm inhaled
      leisure_air_inhaled = leisure_duration / 60 * v_rate_leisure,
      leisure_pm_inhaled = leisure_duration / 60 * v_rate_leisure * conc_pm,


      ## Light activities
      #### Light activities has a correction if sleep time is less than 6 hours
      light_duration = ifelse(sleep_less_6h == 1,
        (unknown_time - 6) * (light_hours / (leisure_hours + light_hours)), 
        unknown_time * (light_hours / (sleep_hours + leisure_hours + light_hours))
      ) * 60, # # In minutes for both if and else values
      light_duration = ifelse(light_duration < 0, 0, light_duration), # This happened in 35 rows out of the 132k
      vo2_light = ecf * light_met * rmr,
      pct_vo2max_light = ifelse(light_duration < 5, 100,
        ifelse(light_duration > 540, 33,
          121.2 - (14 * log(light_duration))
        )
      ),
      upper_vo2max_light = vo2max * (pct_vo2max_light / 100),
      adj_vo2_light = ifelse(vo2_light > upper_vo2max_light,
        upper_vo2max_light, vo2_light
      ),
      ## Draw sample from normal distribution taking into account the variance
      ## between activities
      e_ijk_light = rnorm(1, 0, sd_test_level),
      log_vent_rate_light = intercept_a + (slope_b * log(adj_vo2_light / body_mass)) + d_k + e_ijk_light,
      vent_rate_light = exp(log_vent_rate_light) * body_mass,
      v_rate_light = vent_rate_light * 60 / 1000,
      # Calculate air and pm inhaled
      light_air_inhaled = light_duration / 60 * v_rate_light,
      light_pm_inhaled = light_duration / 60 * v_rate_light * conc_pm,

      # Total air and pm inhaled
      total_air_inhaled = travel_air_inhaled + sleep_air_inhaled + leisure_air_inhaled + light_air_inhaled,
      total_pm_inhaled = travel_pm_inhaled + sleep_pm_inhaled + leisure_pm_inhaled + light_pm_inhaled,

      # Calculate pm / air ratio
      conc_pm_inhaled = total_pm_inhaled / total_air_inhaled
    )
  
  # Change to wide format
  synth_pop <- synth_pop %>%
    dplyr::select(participant_id, scenario, conc_pm_inhaled) %>%
    pivot_wider(names_from = "scenario", values_from = "conc_pm_inhaled") %>%
    setNames(gsub("sc_", "pm_conc_sc_", names(.))) %>% 
    setNames(gsub("baseline", "pm_conc_base", names(.)))
  
  # Get all participants without any travel (in the travel survey)
  id_wo_travel <- SYNTHETIC_POPULATION |>
    filter(!participant_id %in% trip_set$participant_id)

  # Assign all participants without travel baseline + scenario specific base concentration
  id_wo_travel <- cbind(id_wo_travel |>
    dplyr::select(-work_ltpa_marg_met), conc_pm_df |>
    pivot_wider(names_from = "scenario", values_from = "conc_pm"))
  # Rename columns
  id_wo_travel <- id_wo_travel %>%
    setNames(gsub("sc_", "pm_conc_sc_", names(.))) %>% 
    setNames(gsub("baseline", "pm_conc_base", names(.)))

  # Join demographics info from trip_set
  synth_pop <- left_join(
    trip_set |>
      filter(participant_id != 0) |>
      dplyr::select(participant_id, age, sex, age_cat) |>
      distinct(),
    synth_pop,
    by = "participant_id"
  )

  # Combine people with and without trips
  synth_pop <- dplyr::bind_rows(synth_pop, id_wo_travel)

  # Convert data type to integer
  synth_pop$participant_id <- as.integer(synth_pop$participant_id)

  # Return list with concentration and per person PM2.5 exposure (unit: ug/m3)
  list(scenario_pm = conc_pm, pm_conc_pp = as.data.frame(synth_pop))
}
