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
  # Dan: Lambed told me that we need v-rates for everyone. I assigned to 
  # auto_rickshaw and other the same MET values as bus and cycle.
  # TODO: I don't know if we should add MET values for drivers in ghost trips
  # (bus, car, motorcycle, truck). Right now these are not included because they
  # have participant_id = 0.
  met_df <- data.frame(
    stage_mode = c("rest", "car", "taxi", "bus", "rail", "cycle", "pedestrian", 
                   "sleep", "motorcycle", "auto_rickshaw", "other"), 
    met = c(1.3, 2.5, 1.3, 1.3, 1.3, MMET_CYCLING + 1, MMET_WALKING + 1, 
            0.95, 2.8, 1.3, 1.3),
    compendium_code = c('07021', '16010', '16015', '16016', '16016', '01011', 
                        '16060', '07030', '16030', '16016', '16016')
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
      # Ventilation rate for travel modes
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
  
  # Create df with scenarios and concentration
  conc_pm_df <- data.frame(scenario = unique(trip_set$scenario),
                           conc_pm = conc_pm)
  # Join trip_set with conc df
  trip_set <- left_join(trip_set, conc_pm_df)
  
  # cubic meters of air inhaled are the product of the ventilation rate and the 
  # time (hours/60) spent travelling by that mode
  trip_set$air_inhaled <- trip_set$stage_duration / 60 * trip_set$v_rate
  trip_set$air_inhaled_2 <- trip_set$stage_duration / 60 * trip_set$v_rate_2
  
  # PM inhaled (micro grams) = duration * ventilation rate * exposure rates * concentration
  trip_set$pm_inhaled <- trip_set$stage_duration / 60 * trip_set$v_rate * trip_set$e_rate * trip_set$conc_pm
  trip_set$pm_inhaled_2 <- trip_set$stage_duration / 60 * trip_set$v_rate_2 * trip_set$e_rate * trip_set$conc_pm
  
  # Sleep (min/day)	499.4 (115.0)
  # Leisure MVPA (min/day)	26.8 (55.3)
  # Leisure sedentary screen time (min/day)	189.5 (155.0)
  # Non-discretionary time (min/day)	482.2 (204.8)
  # Travel (min/day)	79.2 (93.7)
  # Other (min/day)	162.9 (153.1)
  
  lt_sed_time_hrs <- 189.5 / 60
  nd_time_hrs <- 482.2 / 60
  other_time_hrs <- 162.9 / 60
  
  # total_typical_time_rem_hrs <- lt_sed_time_hrs + nd_time_hrs + other_time_hrs
  # 
  # remaining_time_hrs = 24 - total_travel_time_hrs
  # 
  # lt_sed_time_prop_hrs <- lt_sed_time_hrs / total_typical_time_rem_hrs * remaining_time_hrs
  # nd_time_prop_hrs <- nd_time_hrs / total_typical_time_rem_hrs * remaining_time_hrs
  # other_time_prop_hrs <- other_time_hrs / total_typical_time_rem_hrs * remaining_time_hrs
  # 
  # 
  # sedentary_time_hrs = 
  
  # Calculate total_travel_time_hrs of stage_duration
  trip_set <- trip_set %>% 
    group_by(participant_id, scenario) %>% 
    mutate(total_travel_time_hrs = sum(stage_duration) / 60) %>% 
    # total_typical_time_rem_hrs <- lt_sed_time_hrs + nd_time_hrs + other_time_hrs,
    # remaining_time_hrs = 24 - total_travel_time_hrs,
    # lt_sed_time_prop_hrs <- lt_sed_time_hrs / total_typical_time_rem_hrs * remaining_time_hrs,
    # nd_time_prop_hrs <- nd_time_hrs / total_typical_time_rem_hrs * remaining_time_hrs,
    # other_time_prop_hrs <- other_time_hrs / total_typical_time_rem_hrs * remaining_time_hrs) |> 
    ungroup()
  
  
  # Extract mets for sleep and rest
  sleep_met <- met_df %>% filter(stage_mode == 'sleep') %>% 
    dplyr::select(met) %>% as.numeric()
  rest_met <- met_df %>% filter(stage_mode == 'rest') %>% 
    dplyr::select(met) %>% as.numeric()
  
  #----
  # For comparison
  # Get sleep ventilation rate
  sleep_rate <- filter(vent_rates, stage_mode == "sleep") |> dplyr::select(v_rate) |> pull()
  
  # Get rest ventilation rate
  rest_rate <- filter(vent_rates, stage_mode == "rest") |> dplyr::select(v_rate) |> pull()
  
  #----
  # Calculate total air and pm inhaled in each person
  synth_pop <- trip_set  %>%  
    filter(participant_id != 0) %>% 
    group_by(participant_id, scenario) %>% 
    # reframe instead of summarise in the latest version
    reframe(
      total_travel_time_hrs = max(total_travel_time_hrs, na.rm = T),
      total_air_inhaled = sum(air_inhaled, na.rm = T) + 
        8 * sleep_rate + ((16 - total_travel_time_hrs) * rest_rate),
      total_pm_inhaled = sum(pm_inhaled, na.rm = T) +
        8 * (sleep_rate) * conc_pm + ((16 - total_travel_time_hrs) * (rest_rate) * conc_pm),
      travel_air_inhaled_2 = sum(air_inhaled_2, na.rm = T),
      travel_pm_inhaled_2 = sum(pm_inhaled_2, na.rm = T),
    ) %>% 
    distinct(participant_id, scenario, .keep_all = T) %>% 
    # Merge participant information to calculate ventilation rates for sleep and rest
    left_join(people_for_vent_rates, by = 'participant_id') %>% 
    # Merge scenario concentration
    left_join(conc_pm_df, by = 'scenario') %>% 
    # Calculate vent_rates and air inhaled in the same way as with travel modes
    rowwise() %>% 
    mutate(
      # Sleep
      sleep_duration = 8 * 60, # In minutes
      vo2_sleep = ecf * sleep_met * rmr,
      pct_vo2max_sleep = ifelse(sleep_duration < 5, 100,
                                ifelse(sleep_duration > 540, 33,
                                       121.2 - (14 * log(sleep_duration)))),
      upper_vo2max_sleep = vo2max * (pct_vo2max_sleep / 100),
      adj_vo2_sleep = ifelse(vo2_sleep > upper_vo2max_sleep, 
                             upper_vo2max_sleep, vo2_sleep),
      ## Draw sample from normal distribution taking into account the variance 
      ## between activities
      e_ijk_sleep = rnorm(1, 0, sd_test_level),
      log_vent_rate_sleep = intercept_a + (slope_b * log(adj_vo2_sleep/body_mass)) + d_k + e_ijk_sleep,
      vent_rate_sleep = exp(log_vent_rate_sleep) * body_mass,
      v_rate_sleep = vent_rate_sleep * 60 / 1000,
      # Calculate air and pm inhaled
      sleep_air_inhaled = sleep_duration / 60 * v_rate_sleep,
      sleep_pm_inhaled = sleep_duration / 60 * v_rate_sleep * conc_pm,
      
      # Rest
      rest_duration = (16 - total_travel_time_hrs) * 60, # In minutes
      vo2_rest = ecf * rest_met * rmr,
      pct_vo2max_rest = ifelse(rest_duration < 5, 100,
                               ifelse(rest_duration > 540, 33,
                                      121.2 - (14 * log(rest_duration)))),
      upper_vo2max_rest = vo2max * (pct_vo2max_rest / 100),
      adj_vo2_rest = ifelse(vo2_rest > upper_vo2max_rest, 
                            upper_vo2max_rest, vo2_rest),
      ## Draw sample from normal distribution taking into account the variance 
      ## between activities
      e_ijk_rest = rnorm(1, 0, sd_test_level),
      log_vent_rate_rest = intercept_a + (slope_b * log(adj_vo2_rest/body_mass)) + d_k + e_ijk_rest,
      vent_rate_rest = exp(log_vent_rate_rest) * body_mass,
      v_rate_rest = vent_rate_rest * 60 / 1000,
      # Calculate air and pm inhaled
      rest_air_inhaled = rest_duration / 60 * v_rate_rest,
      rest_pm_inhaled = rest_duration / 60 * v_rate_rest * conc_pm,
      
      # Total air and pm inhaled
      total_air_inhaled_2 = travel_air_inhaled_2 + sleep_air_inhaled + rest_air_inhaled,
      total_pm_inhaled_2 = travel_pm_inhaled_2 + sleep_pm_inhaled + rest_pm_inhaled,
      
      # Calculate pm / air ratio
      conc_pm_inhaled = total_pm_inhaled / total_air_inhaled,
      conc_pm_inhaled_2 = total_pm_inhaled_2 / total_air_inhaled_2
    ) 
  
  # Change to wide format
  synth_pop <- synth_pop %>% 
    dplyr::select(participant_id, scenario, conc_pm_inhaled_2) %>% 
    pivot_wider(names_from = 'scenario', values_from = 'conc_pm_inhaled_2') %>% 
    # Rename columns
    rename_at(vars(starts_with(c("base", "sc"))), 
              ~ paste0("pm_conc_", SCEN_SHORT_NAME))
  
  # Get all participants without any travel (in the travel survey)
  id_wo_travel <- SYNTHETIC_POPULATION |> 
    filter(!participant_id %in% trip_set$participant_id)
  
  # Assign all participants without travel baseline + scenario specific base concentration
  id_wo_travel <- cbind(id_wo_travel |> 
                          dplyr::select(-work_ltpa_marg_met), conc_pm_df |> 
                          pivot_wider(names_from = "scenario", values_from = "conc_pm"))
  # Rename columns
  id_wo_travel <- id_wo_travel |> 
    rename_at(vars(starts_with(c("base", "sc"))), 
              ~ paste0("pm_conc_", SCEN_SHORT_NAME))
  
  # Join demographics info from trip_set 
  synth_pop <- left_join(trip_set |> 
                           filter(participant_id != 0) |> 
                           dplyr::select(participant_id, age, sex, age_cat) |> 
                           distinct(), 
                         synth_pop)
  
  # Combine people with and without trips
  synth_pop <- dplyr::bind_rows(synth_pop, id_wo_travel)
  
  # Convert data type to integer
  synth_pop$participant_id <- as.integer(synth_pop$participant_id)
  
  # Return list with concentration and per person PM2.5 exposure (unit: ug/m3)
  list(scenario_pm = conc_pm, pm_conc_pp = as.data.frame(synth_pop))
}

##### 
# Delete this part later
# I used it to compare the previous implementation with the new one
# All variables are there, I didn't remove any variable from the trip set
# because in the end we are using only few variables of it, and we are exporting
# only synth_pop.

# aux = trip_set %>% filter(stage_mode == 'other')
# table(aux$trip_mode, useNA = 'always')
# 
# aux = trip_set %>% filter(is.na(v_rate))
# table(aux$stage_mode, useNA = 'always')
# 
baseline <- trip_set %>% filter(scenario == 'baseline')
View(baseline %>% group_by(stage_mode) %>%
  summarise(
    avg_v_rate = mean(v_rate, na.rm = T),
    avg_v_rate_new = mean(v_rate_2, na.rm = T),
    median_v_rate = median(v_rate, na.rm = T),
    median_v_rate_new = median(v_rate_2, na.rm = T),
    ) #%>%
    #filter(!is.na(avg_v_rate))
  )

# aux <- trip_set %>% filter(is.na(v_rate_2))
# table(aux$stage_mode, useNA = 'always')

# Comparison between both calculations in total air and pm inhaled
# Run this after running until line 333...not after 336
# View(synth_pop %>% group_by(scenario) %>% 
#        summarise(
#          avg_total_air_inhaled = mean(total_air_inhaled, na.rm = T),
#          avg_total_air_inhaled_new = mean(total_air_inhaled_2, na.rm = T),
#          median_total_air_inhaled = median(total_air_inhaled, na.rm = T),
#          median_total_air_inhaled_new = median(total_air_inhaled_2, na.rm = T),
#          avg_total_pm_inhaled = mean(total_pm_inhaled, na.rm = T),
#          avg_total_pm_inhaled_new = mean(total_pm_inhaled_2, na.rm = T),
#          median_total_pm_inhaled = median(total_pm_inhaled, na.rm = T),
#          median_total_pm_inhaled_new = median(total_pm_inhaled_2, na.rm = T),
#          avg_conc_pm_inhaled = mean(conc_pm_inhaled, na.rm = T),
#          avg_conc_pm_inhaled_new = mean(conc_pm_inhaled_2, na.rm = T),
#          median_conc_pm_inhaled = median(conc_pm_inhaled, na.rm = T),
#          median_conc_pm_inhaled_new = median(conc_pm_inhaled_2, na.rm = T),
#        )
# )

# This didn't work, but not sure why
# baseline <- synth_pop %>% filter(scenario == 'baseline')
# aux <- baseline %>% 
#   dplyr::select(scenario, conc_pm_inhaled, conc_pm_inhaled_2) %>% 
#   pivot_longer(
#     cols = conc_pm_inhaled:conc_pm_inhaled_2,
#     names_to = "variable",
#     values_to = "value"
#   )
# 
# p <- ggplot(data=aux, aes(x=value, group=variable, fill=variable)) +
#   geom_density(adjust=1.5, alpha=.4)# +
# #  facet_wrap(~scenario) 
# p



## TODO
# (OK) 1. Add distribution tables about the ventilation rate in the inst folder, so they are installed and can be accessed
# (OK) 2. Create new lines of code to add the information about these distributions and calculate BM, RMR, ECF, etc
# (OK) 3. Bring these new variables to the trip_set
# 4. Make sure that the code can be used in the same way as in the original function
##### 
