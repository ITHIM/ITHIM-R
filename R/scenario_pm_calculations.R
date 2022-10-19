#' Calculate total AP exposure per person
#' 
#' Calculate total AP exposure per person based on population and personal travel
#' 
#' @param dist data frame of population travel from all scenarios
#' @param trip_scen_sets data frame of all trips from all scenarios
#' 
#' @return background AP
#' @return total AP exposure per person
#' 
#' @export
scenario_pm_calculations <- function(dist, trip_scen_sets){
  
  # Ventilation rates by mode unit: m3/hour 
  vent_rates <- data.frame(
    stage_mode = c("rest", "car", "taxi", "bus", "rail", "cycle", "pedestrian", "sleep", "motorcycle"), 
    v_rate = c(0.61, 0.61, 0.61, 0.61, 0.61, 2.55, 1.37, 0.27, 0.61)
  )
  
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
  ordered_efs <- (VEHICLE_INVENTORY$PM_emission_inventory[match(emission_dist$stage_mode,VEHICLE_INVENTORY$stage_mode)] %>% as.numeric())/(emission_dist$Baseline %>% as.numeric())
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
  
  # trip set is a data.table, vent_rates is a data.frame, returns a data.table
  trip_set <- dplyr::left_join(trip_set, vent_rates, 'stage_mode')
  
  # Join trip_set and exponent factords df
  trip_set <- dplyr::left_join(trip_set, exp_facs, 'stage_mode')
  
  # Create df with scenarios and concentration
  conc_pm_df <- data.frame(scenario = unique(trip_set$scenario),
                           conc_pm = conc_pm)
  # Join trip_set with conc df
  trip_set <- left_join(trip_set, conc_pm_df)
  
  
  # litres of air inhaled are the product of the ventilation rate and the time (hours/60) spent travelling by that mode
  trip_set$air_inhaled <- trip_set$stage_duration / 60 * trip_set$v_rate
  
  # PM inhaled (micro grams) = duration * ventilation rate * exposure rates * concentration
  trip_set$pm_inhaled <- trip_set$stage_duration / 60 * trip_set$v_rate * trip_set$e_rate * trip_set$conc_pm
  
  # Calculate total_travel_time_hrs of stage_duration
  trip_set <- trip_set |> 
    group_by(participant_id, scenario) |> 
    mutate(total_travel_time_hrs = sum(stage_duration) / 60) |> 
    ungroup()
  
  # Get sleep ventilation rate
  sleep_rate <- filter(vent_rates, stage_mode == "sleep") |> dplyr::select(v_rate) |> pull()
  
  # Get rest ventilation rate
  rest_rate <- filter(vent_rates, stage_mode == "rest") |> dplyr::select(v_rate) |> pull()
  
  # Calculate total_air_inhaled (unit: m3) = sum(air_inhaled) + 
  #                               8 * sleep_ventilation_rate +
  #                               (16 - total_travel_time_hrs) * rest_ventilation_rate
  # Calculate total_pm_inhaled (unit: µg) = sum(pm_inhaled) + 
  #                               8 * sleep_ventilation_rate * conc_pm +
  #                               (16 - total_travel_time_hrs) * rest_ventilation_rate * conc_pm
  # Calculate conc_pm_inhaled (unit: µg/m3) = total_pm_inhaled / total_air_inhaled 
  synth_pop <- trip_set |> filter(participant_id != 0) |> 
    group_by(participant_id, scenario) |> 
    summarise(total_air_inhaled = sum(air_inhaled, na.rm = T) + 
                8 * sleep_rate + 
                ((16 - total_travel_time_hrs) * rest_rate),
              total_pm_inhaled = sum(pm_inhaled, na.rm = T) +
                8 * (sleep_rate) * conc_pm + 
                ((16 - total_travel_time_hrs) * (rest_rate) * conc_pm)) |> 
    distinct(participant_id, scenario, .keep_all = T) |> 
    mutate(conc_pm_inhaled = total_pm_inhaled / total_air_inhaled)
  
  # Print summary
  synth_pop |> group_by(scenario) |> summarise(as_tibble(rbind(summary(conc_pm_inhaled)))) |> print()
  
  # Change to wide format
  synth_pop <- synth_pop |> 
    dplyr::select(-c(total_air_inhaled, total_pm_inhaled)) |> 
    pivot_wider(names_from = "scenario", values_from = "conc_pm_inhaled")
  
  # Rename columns
  synth_pop <- synth_pop |> 
    rename_at(vars(starts_with(c("Base", "Scen"))), 
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
    rename_at(vars(starts_with(c("Base", "Scen"))), 
              ~ paste0("pm_conc_", SCEN_SHORT_NAME))
  
  # Join demographics info from trip_set 
  synth_pop <- left_join(trip_set |> 
                           filter(participant_id != 0) |> 
                           dplyr::select(participant_id, age, sex, age_cat) |> 
                           distinct(), 
                         synth_pop)
  
  # Combine people with and without trips
  synth_pop <- plyr::rbind.fill(synth_pop, id_wo_travel)
  
  # Convert data type to integer
  synth_pop$participant_id <- as.integer(synth_pop$participant_id)
  
  # Return list with concentration and per person PM2.5 exposure (unit: ug/m3)
  list(scenario_pm = conc_pm, pm_conc_pp = as.data.frame(synth_pop))
  
}
