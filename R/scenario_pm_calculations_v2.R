#' Calculate total AP exposure per person
#' 
#' Calculate total AP exposure per person based on population and personal travel
#' 
#' @param dist distance data frame of population travel from all scenarios
#' @param trip_scen_sets trips data frame of all trips from all scenarios
#' 
#' @return background AP
#' @return total AP exposure per person
#' 
#' @export
scenario_pm_calculations_v2 <- function(dist, trip_scen_sets){
  # Exposure factor rate by activity (the ratio between that mode’s PM2.5 and the background’s PM2.5)
  exp_facs <- data.frame(
    stage_mode = c("car", "taxi", "bus", "rail", "cycle", "pedestrian", "motorcycle"), 
    e_rate = c(2.5, 2.5, 1.9, 1.9, 2.0, 1.6, 2.0)
  )
  
  # concentration contributed by non-transport share (remains constant across the scenarios)
  non_transport_pm_conc <- PM_CONC_BASE*(1 - PM_TRANS_SHARE)  
  
  ## adding in travel not covered in the synthetic trip set, based on distances travelled relative to car, set in VEHICLE_INVENTORY
  emission_dist <- dist
  
  ## get emission factor by dividing inventory by baseline distance. (We don't need to scale to a whole year, as we are just scaling the background concentration.)
  ordered_efs <- (VEHICLE_INVENTORY$PM_emission_inventory[match(emission_dist$stage_mode,VEHICLE_INVENTORY$stage_mode)] %>% as.numeric())/(emission_dist$baseline %>% as.numeric())
  ## get new emission by multiplying emission factor by scenario distance.
  trans_emissions <- emission_dist[,0:NSCEN+2]*t(repmat(ordered_efs,NSCEN+1,1))
  ## augment with travel emission contributions that aren't included in distance calculation
  for(mode_type in which(!VEHICLE_INVENTORY$stage_mode%in%emission_dist$stage_mode))
    trans_emissions[nrow(trans_emissions)+1,] <- VEHICLE_INVENTORY$PM_emission_inventory[mode_type]
  
  ## scenario travel pm2.5 calculated as relative to the baseline
  baseline_sum <- sum(trans_emissions[[SCEN[1]]], na.rm = T)
  conc_pm <- c()
  ## in this sum, the non-transport pm is constant; the transport emissions scale the transport contribution (PM_TRANS_SHARE) to the base level (PM_CONC_BASE)
  for(i in 1:length(SCEN_SHORT_NAME)){
    conc_pm[i] <- non_transport_pm_conc + PM_TRANS_SHARE*PM_CONC_BASE*sum(trans_emissions[[SCEN[i]]], na.rm = T)/baseline_sum
  }
  
  # Copy trips dataset
  trip_set <- trip_scen_sets
  
  # Rename short walks as pedestrian 
  trip_set$stage_mode[trip_set$stage_mode=='walk_to_pt'] <- 'pedestrian'
  
  # Dan: These ventilation rates do not apply anymore
  # trip set is a data.table, vent_rates is a data.frame, returns a data.table
  trip_set <- dplyr::left_join(trip_set, vent_rates, 'stage_mode')
  
  # Join trip_set and exponent factors df
  trip_set <- dplyr::left_join(trip_set, exp_facs, 'stage_mode')
  
  #----
  # Dan: These new lines of code are for the ventilation rate
  
  # Dan: Global path to read ventilation rate's distributions
  global_path <- paste0(file.path(find.package('ithimr',lib.loc = .libPaths()),
                                  'extdata/global'), "/")
  
  # Dan: Read parameter distributions
  ## Dan: Body mass
  body_mass_df <- read.csv(paste0(global_path,"ventilation_rate/BodyMass.csv"))
  ## Dan: Energy Conversion Factor (ECF)
  ecf_df <- read.csv(paste0(global_path,"ventilation_rate/ECF.csv"))
  ## Dan: Resting Metabolic Rate (RMR)
  rmr_df <- read.csv(paste0(global_path,"ventilation_rate/RMR.csv"))
  ## Dan: Normalized maximum oxygen uptake rate (NVO2max)
  nvo2max_df <- read.csv(paste0(global_path,"ventilation_rate/NVO2max.csv"))
  ## Dan: Ventilation from Oxygen Uptake (vent_from_oxygen)
  vent_from_oxygen_df <- read.csv(paste0(global_path,"ventilation_rate/VentilationFromOxygenUptake.csv"))
  
  # Dan: MET values for each mode/activity. These values come from the Compendium
  # TODO: Check these values with the team
  met_df <- data.frame(
    stage_mode = c("rest", "car", "taxi", "bus", "rail", "cycle", "pedestrian", "sleep", "motorcycle"), 
    met = c(1.3, 2.5, 1.3, 1.3, 1.3, 6.8, 4, 0.95, 2.8),
    compendium_code = c('07021', '16010', '16015', '16016', '16016', '01011', 
                   '17270', '07030', '16030')
  )
  
  # Dan: Extract people from the synthetic population to calculate their 
  # ventilation rates
  people_for_vent_rates <- trip_set %>% filter(participant_id != 0) %>% 
    distinct(participant_id, .keep_all=T) %>% 
    dplyr::select(participant_id, age, sex)
  
  # Dan: Draw from a log-normal distribution the body mass [kg] of each person 
  # in the synthetic population
  people_for_vent_rates <- people_for_vent_rates %>% 
    left_join(body_mass_df, by=c('age', 'sex')) %>% 
    rowwise() %>% # To apply an operation in each row
    mutate(
      # Draw sample from distribution
      sample = rlnorm(1, log(gm), log(gsd)),
      # Check maximum and minimum values
      body_mass = ifelse(sample < lower, lower,
                         ifelse(sample > upper, upper, sample))) %>% 
    dplyr::select(participant_id, age, sex, body_mass)
  
  # Dan: Draw from a uniform distribution the Energy Conversion Factor [lt/kcal]
  # of each person in the synthetic population
  people_for_vent_rates <- people_for_vent_rates %>% 
    left_join(ecf_df, by=c('age', 'sex')) %>% 
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
    left_join(rmr_df, by=c('age', 'sex')) %>% 
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
    left_join(nvo2max_df, by=c('age', 'sex')) %>% 
    rowwise() %>% # To apply an operation in each row
    mutate(
      # Draw sample from distribution
      sample = rnorm(1, mean, sd),
      # Check maximum and minimum values
      sample_check = ifelse(sample < lower, lower,
                         ifelse(sample > upper, upper, sample)),
      # Transform [ml/(min*kg)] to [lt/(min*kg)]
      nvo2max = sample_check/1000
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
    left_join(vent_from_oxygen_df, by=c('age', 'sex')) %>% 
    rowwise() %>% # To apply an operation in each row
    mutate(
      # Draw sample from normal distribution taking into account the variance 
      # between persons
      d_k = rnorm(1, 0, sd_person_level)
    ) %>% 
    dplyr::select(participant_id, body_mass, ecf, rmr, nvo2max, vo2max,
                  intercept_a, slope_b, sd_person_level, sd_test_level, d_k)
  
  # Dan: Adding these new columns to the trip set
  trip_set <- trip_set %>% 
    left_join(people_for_vent_rates, by = 'participant_id')

  # Dan: Adding MET values [dimensionless] for each mode
  trip_set <- trip_set %>% left_join(met_df %>% dplyr::select(stage_mode, met), 
                                     by = 'stage_mode')
  
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
    rowwise() %>% 
    mutate(
      vo2 = ecf * met * rmr, 
      pct_vo2max = ifelse(stage_duration < 5, 100,
                    ifelse(stage_duration > 540, 33,
                           121.2 - (14 * log(stage_duration)))),
      upper_vo2max = vo2max * (pct_vo2max / 100),
      adj_vo2 = ifelse(vo2 > upper_vo2max, upper_vo2max, vo2),
      # Draw sample from normal distribution taking into account the variance 
      # between activities
      e_ijk = rnorm(1, 0, sd_test_level),
      log_vent_rate = intercept_a + (slope_b * log(adj_vo2/body_mass)) + d_k + e_ijk,
      vent_rate = exp(log_vent_rate) * body_mass,
      v_rate_2 = vent_rate * 60 / 1000
      )
  
  
  #----
  
  # aux = trip_set %>% filter(stage_mode == 'other')
  # table(aux$trip_mode, useNA = 'always')
  # 
  # aux = trip_set %>% filter(is.na(v_rate))
  # table(aux$stage_mode, useNA = 'always')

  baseline <- trip_set %>% filter(scenario == 'baseline')
  View(baseline %>% group_by(stage_mode) %>% 
    summarise(
      avg_v_rate = mean(v_rate, na.rm = T),
      avg_v_rate_new = mean(v_rate_2, na.rm = T),
      median_v_rate = median(v_rate, na.rm = T),
      median_v_rate_new = median(v_rate_2, na.rm = T),
      ) %>% filter(!is.na(avg_v_rate)))
  
  View(baseline %>% filter(is.na(v_rate_2)))
  ## TODO
  # (OK) 1. Add distribution tables about the ventilation rate in the inst folder, so they are installed and can be accessed
  # (OK) 2. Create new lines of code to add the information about these distributions and calculate BM, RMR, ECF, etc
  # (OK) 3. Bring these new variables to the trip_set
  # 4. Make sure that the code can be used in the same way as in the original function

}