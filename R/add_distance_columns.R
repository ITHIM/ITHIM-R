#' Add strike and casualty distances to injury tables
#'
#' This function adds strike and casualty distance information for both
#' individual age and sex categories and aggregated by mode to both the who-hit-whom (whw)
#' and no-other-vehicle (nov) injury fatality matrices.
#'
#'
#' The function performs the following steps:
#'
#' \itemize{
#' \item set up a dataframe containing all the age and sex categories found in the
#'   injury table input data
#'
#' \item add age and sex index to mode distances
#'
#' \item define a list with two elements for whw and nov with the same indices used in the
#'   true_distances dataframe for all age and sex categories
#'
#' \item define a list (with two elements for whw and nov) matching the indices of the mode_names
#'   with the casualty modes in the injury table
#'
#' \item for the whw matrix create a strike mode indices vector matching the strike modes
#'   in the injury table with the strike_modes vector
#'
#' \item find the strike and casualty distances for all modes in the injury tables both as sum
#'   across all age and sex categories and for each individual age and sex category. For modes
#'   that exist in the injury tables for which we have no known distances, the mean mode distances
#'   of the known modes is used
#'
#' \item add the strike and casualty modes to the whw and nov matrices in the injuries_list output list
#' }
#'
#' @param injury_table (list of) data frame(s) to be edited, contains aggregated fatality counts split into whw and nov matrices
#' @param mode_names which modes to take distances for, taken from the aggregated modes in the trip data
#' @param true_distances_0 population distances to add to injury table
#' @param dist table used to access bus distance - not currently used
#' @param scenarios which scenarios to process
#'
#' @return injury tables with strike and casualty distance information for both the whw and nov matrices
#'
#' @export

add_distance_columns <- function(injury_table, mode_names, true_distances_0, dist, scenarios = SCEN) {
  injury_temp <- injury_table

  by_age <- "age_cat" %in% names(injury_table[[1]]) # gives FALSE if no age categories given
  by_gender <- "cas_gender" %in% names(injury_table[[1]])
  for (type in INJURY_TABLE_TYPES) { # type is either whw or nov
    if (!by_age) injury_temp[[type]]$age_cat <- 1 # add 'dummy' age_cat and cas_gender columns if they don't exist
    if (!by_gender) injury_temp[[type]]$cas_gender <- 1
  }
  u_gen <- unique(injury_temp[[1]]$cas_gender)
  u_age <- unique(injury_temp[[1]]$age_cat)
  dem_index_table <- expand.grid(cas_gender = u_gen, age_cat = u_age) # df with all age and gender category combinations in injury table

  if (!by_age) true_distances_0$age_cat <- 1
  if (!by_gender) true_distances_0$sex <- 1
  # assign same index as in dem_index_table to true_distances
  true_distances_0$dem_index <- length(u_gen) * (match(true_distances_0$age_cat, u_age) - 1) + match(true_distances_0$sex, u_gen)

  cas_mode_indices <- list()
  dem_index <- list()
  # initialise tables and store indices
  for (type in INJURY_TABLE_TYPES) {
    # define a list (with two elements for whw and nov) with the same indices used for the true_distances
    # dataframe for all age and gender categories
    # define a list (with two elements for whw and nov) matching the indices of the mode_names
    # with the cas modes in the injury table
    gen_index <- match(injury_temp[[type]]$cas_gen, u_gen)
    age_index <- match(injury_temp[[type]]$age_cat, u_age)
    dem_index[[type]] <- length(u_gen) * (age_index - 1) + gen_index
    cas_mode_indices[[type]] <- match(injury_table[[type]]$cas_mode, mode_names)
  }

  # for whw matrices create a strike mode indices vector matching the strike mode in the injury table with the strike_modes vector
  if ("whw" %in% INJURY_TABLE_TYPES) {
    strike_distances <- true_distances_0
    strike_modes <- unique(as.character(injury_table$whw$strike_mode))
    strike_mode_indices <- match(injury_table$whw$strike_mode, unique(c(mode_names, strike_modes)))
  }

  ## Calculate distances
  injuries_list <- list()

  for (i in 1:length(scenarios)) { # loop through scenarios
    scen <- scenarios[i]
    injuries_list[[scen]] <- list()
    true_scen_dist <- subset(true_distances_0, scenario == scen) # filter distance for specific scenario
    # aggregate distances by age and gender categories if such info exists in injury table
    dist_summary <- as.data.frame(t(sapply(sort(unique(true_scen_dist$dem_index)), function(x) {
      colSums(subset(true_scen_dist, dem_index == x)[, !colnames(true_scen_dist) %in% c("age_cat", "sex", "scenario", "sex_age", "dem_index")])
    })))
    # apply casualty distance sums
    distance_sums <- sapply(mode_names, function(x) sum(dist_summary[[x]])) # total distance for each mode
    if ("whw" %in% INJURY_TABLE_TYPES) { # find the strike distances
      strike_true_scen_dist <- subset(strike_distances, scenario == scen)
      strike_dist_summary <- as.data.frame(t(sapply(unique(strike_true_scen_dist$dem_index), function(x) { # aggregate distances by age and gender categories if such info exists in injury table
        colSums(subset(strike_true_scen_dist, dem_index == x)[, !colnames(strike_true_scen_dist) %in% c("age_cat", "sex", "scenario", "sex_age", "dem_index")])
      })))
      # find strike distance sums
      strike_distance_sums <- sapply(mode_names, function(x) sum(strike_dist_summary[[x]]))

      # Add mean mode distance to the missing strike modes
      # These are modes that exist as strike_mode in the injuries dataset but which do not appear
      # in the trip dataset such as e.g. 'unknown' modes
      missing_strike_dist_modes <- strike_modes[!strike_modes %in% names(strike_distance_sums)]
      old_length <- length(strike_distance_sums)
      if (length(missing_strike_dist_modes) > 0) {
        for (i in 1:length(missing_strike_dist_modes)) {
          str_mode <- missing_strike_dist_modes[i]
          strike_distance_sums <- c(strike_distance_sums, mean(strike_distance_sums))
          names(strike_distance_sums)[(old_length + 1):length(strike_distance_sums)] <- str_mode # strike_modes[!strike_modes%in%names(strike_distance_sums)][1]

          old_length <- length(strike_distance_sums)
        }
      }
    }


    for (type in INJURY_TABLE_TYPES) {
      injuries_list[[scen]][[type]] <- injury_table[[type]]

      # initialise all strike distances as 1
      injuries_list[[scen]][[type]]$strike_distance <- 1
      injuries_list[[scen]][[type]]$strike_distance_sum <- 1

      # add the casualty distance sums, i.e. the mode distances summed across all age and sex categories
      injuries_list[[scen]][[type]]$cas_distance_sum <- distance_sums[cas_mode_indices[[type]]]

      # add casualty distances by age and sex category
      injuries_list[[scen]][[type]]$cas_distance <- as.numeric(as.data.frame(dist_summary)[cbind(dem_index[[type]], cas_mode_indices[[type]])])

      # add strike distances to the 'whw' matrix
      if (type == "whw") {
        injuries_list[[scen]][[type]]$strike_distance <- strike_distance_sums[strike_mode_indices]
        injuries_list[[scen]][[type]]$strike_distance_sum <- injuries_list[[scen]][[type]]$strike_distance
      }
      # Following not needed as nov matrices do not contain any strike mode information plus there is no information for bus drivers anyway
      # }else{
      #   if(!'bus_driver'%in%mode_names){
      #
      #     if (length(bus_base) > 0){
      #       injuries_list[[scen]][[type]]$strike_distance[injuries_list[[scen]][[type]]$strike_mode=='bus_driver'] <- dist[which(dist$stage_mode=='bus_driver'),i+1]/bus_base
      #       injuries_list[[scen]][[type]]$strike_distance_sum[injuries_list[[scen]][[type]]$strike_mode=='bus_driver'] <- dist[which(dist$stage_mode=='bus_driver'),i+1]/bus_base
      #     }
      #   }
      # }
    }
  } # end of scenario loop

  return(injuries_list)
}
