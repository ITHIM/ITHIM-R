#' Calculate total CO2 exposure per mode and scenario
#'
#' Calculate total CO2 exposure for each mode and for each scenario based on the CO2 emissions inventory
#'
#' This function performs the following steps:
#'
#' \itemize{
#' \item calculate emission factors for each mode by dividing total emissions by distances travelled
#'
#' \item calculate CO2 emissions for each mode in each scenario by multiplying the scenario distance times the emission factors
#'
#' \item for modes without any assigned distance, use the CO2 emissions from the VEHICLE_INVENTORY instead
#' }
#'
#' @param dist data frame of population travel from all scenarios
#'
#' @return total CO2 exposure per mode
#'
#' @export
#'


scenario_co2_calculations <- function(dist) {
  # total population distances travelled by all modes
  emission_dist <- dist

  # get emission factor by dividing inventory emissions by baseline distance.
  # (We don't need to scale to a whole year, as we are just scaling the background concentration.)
  ordered_efs <- (VEHICLE_INVENTORY$CO2_emission_inventory[match(emission_dist$stage_mode, VEHICLE_INVENTORY$stage_mode)] %>% as.numeric()) / (emission_dist$baseline %>% as.numeric())

  # get new emissions by multiplying emission factor by scenario distance.
  trans_emissions <- emission_dist[, SCEN] * t(repmat(ordered_efs, NSCEN + 1, 1))

  # augment with travel emission contributions that aren't included in the distance calculation
  # loop through modes without an assigned distance
  for (mode_type in which(!VEHICLE_INVENTORY$stage_mode %in% emission_dist$stage_mode)) {
    # add emissions from vehicle inventory
    trans_emissions[nrow(trans_emissions) + 1, ] <- VEHICLE_INVENTORY$CO2_emission_inventory[mode_type]
  }


  return(trans_emissions)
}
