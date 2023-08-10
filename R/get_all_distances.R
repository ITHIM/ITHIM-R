#' Find population distances by mode for entire population
#' 
#' Function to find the distances travelled by age, sex, scenario and mode for the entire population 
#' rather than just the synthetic population.
#' 
#' This function performs the following steps:
#' 
#' - generate distance and duration matrices by age, sex, mode and scenario from the ithim_object$trip_scen_sets
#'   for the synthetic population by calling the dist_dur_tbls.R function
#'   
#' - find the total mode distances for each scenario and scale this up to the distance travelled by the entire
#'   population by using the demographic information for the city
#' 
#' - in order to scale the distances by age, sex, mode and scenario to the entire population, the proportion of 
#'   the distances travelled by each age, sex, mode and scenario combination in the synthetic population to total 
#'   distances by mode and scenario in the synthetic population is found. The total population distances by mode 
#'   are then multiplied by these proportions to find the total population distances travelled by each mode 
#'   and age and sex category.
#' 
#' - the distances_for_injury_function.R function is called which creates a list inj_distances that is added
#'   to ithim_object containing the following matrices:
#'   - true_distances (population mode distances by age and sex with all walking modes and all car modes combined and 
#'      bus drivers added where relevant)
#'   - injuries_list (list of all strike, casualty, age, sex and mode distance combinations for baseline 
#'      and all scenarios, used to predict fatalities later in the model run)
#'   - reg_model (parameterised Poisson injury regression model)
#'   - injuries_for_model (baseline data containing injury counts for all casualty and strike mode 
#'      combinations with associated distance data)
#' 
#' 
#' 
#' 
#' @param ithim_object list containing city specific information including the synthetic trip set
#' 
#' @return ithim_object again, with additional total population distance for each mode and scenario, distances for injury pathway plus parameterised Poisson injury regression model
#' 
#' @export
#' 

get_all_distances <- function(ithim_object){
  
 
  # Generate distance and duration matrices taken from trip_scen_sets list as part of ithim_object
  # by calling the dist_dur_tbls.R function
  dist_and_dir <- dist_dur_tbls(ithim_object$trip_scen_sets)
  
  ithim_object$dist <- dist_and_dir$dist # create a distance matrix
  ithim_object$dur <- dist_and_dir$dur # create a duration matrix
  
  trip_scen_sets <- ithim_object$trip_scen_sets
  
 

  # Use demographic information
  pop <- DEMOGRAPHIC
  
  # Rename column
  pop <- pop %>% dplyr::rename(age_cat = age)
  
  
  total_synth_pop <- nrow(SYNTHETIC_POPULATION) # find the size of the total synthetic population
  
  # Recalculate distance by scaling to the total population (rather than the synthetic population) 
  dist <- trip_scen_sets %>% group_by(stage_mode, scenario) %>% 
    summarise(ave_dist = sum(stage_distance) / total_synth_pop * sum(pop$population)) %>% spread(scenario, ave_dist)

  # add 'walk_to_pt' stage distances to 'pedestrian' stage distances
  if ('pedestrian' %in% dist$stage_mode && 'walk_to_pt' %in% dist$stage_mode){
    dist[dist$stage_mode == "pedestrian",][2:ncol(dist)] <- dist[dist$stage_mode == "pedestrian",][2:ncol(dist)] +
      dist[dist$stage_mode == "walk_to_pt",][2:ncol(dist)]

    dist <- dist %>% filter(stage_mode != 'walk_to_pt')
  }
 
  ## Find population distances for each age and gender category by
  # - determining the proportion of age and sex specific mode distances
  #   to total mode distances in the synthetic population
  # - multiplying the total population distances by these proportions
  
  # find individual age / gender distances in synthetic population:
  trips_age_gender <- trip_scen_sets %>%
    group_by (age_cat,sex,stage_mode, scenario) %>%
    summarise(dist_age = sum(stage_distance))
  
  # find total trip distances by mode and scenario in the synthetic population
  trips_scen_mode <- trip_scen_sets %>%
    group_by (stage_mode, scenario) %>%
    summarise(dist_synth = sum(stage_distance))

  # create data frame for each age, sex, mode and scenario combination containing the specific 
  # distance travelled for this combination in the synthetic population and the total
  # distance travelled by the synthetic population for this specific mode and scenario
  trips_age_gender <- left_join(trips_age_gender, trips_scen_mode, by = c('stage_mode', 'scenario'))
  
  # find proportion of total trip distance for each age and gender category distance in the synthetic population
  trips_age_gender$prop <- trips_age_gender$dist_age / trips_age_gender$dist_synth
  
  # find total distance across entire population (and not just synthetic population)
  trips_age_gender$tot_dist <- trips_age_gender$dist_synth / total_synth_pop * sum(pop$population)
  
  # scale total distance by trip proportion for each age and gender
  trips_age_gender$tot_dist <- trips_age_gender$tot_dist * trips_age_gender$prop
 
  
  journeys <- trips_age_gender %>% dplyr::select(-c(dist_age, dist_synth, prop))
  
  
  # Add true_dist to the ithim_object
  ithim_object$true_dist <- dist
  
  
  # distances for injuries calculation but also parameterisation of Poisson injury regression model
  ithim_object$inj_distances <- distances_for_injury_function(journeys = journeys, dist = dist)
  
  return(ithim_object)
}
