#' Injury summary statistics
#'
#' Creates summarised injury tables for 'who hit who' and 'no other vehicle' fatality counts
#' for each casualty and strike mode combination (and age and sex combination where this information exists)
#'
#'
#' The function performs the following steps using the individual fatality injury input file:
#'
#' \itemize{
#' \item The data is split into a WHW (who hit who) matrix where both casualty and strike mode are given
#'   and a NOV (no other vehicle) matrix where strike mode was set to NOV in \code{\link{ithim_load_data()}} or
#'   no other vehicle was involved in the accident.
#'
#' \item If no age and gender information is given, then the counts are multiplied by the
#'   proportion of injuries relevant to the proportion of the population considered
#'   in the model (e.g. 15 - 65 year olds) based on the GBD data
#'
#' \item Data is aggregated by casualty and strike mode, and age and sex where such information exists
#'
#' \item Complete whw and nov matrices containing all casualty and strike (and age and sex)
#'   combinations are created with zero counts for those combinations with no fatalities
#'
#' \item a list of aggregated whw and nov matrices is set to the Global environment
#' }
#'
#' @param injuries data frame of individual injury events
#'
#'
#' @export


set_injury_contingency <- function(injuries) {
  # create list containing all modes in trip data set with speeds
  mode_names <- c(intersect(unique(TRIP_SET$stage_mode), MODE_SPEEDS$stage_mode))


  # add bus drivers, car drivers and motorcyclists if the corresponding flags are set to true
  if (ADD_BUS_DRIVERS) mode_names <- c(mode_names, "bus_driver")
  if (ADD_TRUCK_DRIVERS) mode_names <- c(mode_names, "truck")
  if (ADD_MOTORCYCLE_FLEET | ADD_PERSONAL_MOTORCYCLE_TRIPS != "no") mode_names <- c(mode_names, "motorcycle")

  injury_list <- list()
  injury_table_types <- c()

  # check whether there are any whw injuries given and create a whw matrix
  if (length(unique(injuries$strike_mode)) == 1 && !"nov" %in% injuries$strike_mode || length(unique(injuries$strike_mode)) > 1) {
    injury_list$whw <- subset(injuries, cas_mode %in% mode_names & strike_mode != "nov")
    injury_table_types <- c(injury_table_types, "whw")
  }
  # check if there are any nov injuries and create a nov matrix
  if ("nov" %in% injuries$strike_mode) {
    injury_list$nov <- subset(injuries, cas_mode %in% mode_names & strike_mode == "nov")
    injury_table_types <- c(injury_table_types, "nov")
  }

  injury_table <- list()

  # define column names to keep
  for (type in c(injury_table_types)) { # loop through 'whw' and 'nov'
    # Temporal injuries dataset
    temp_df <- injury_list[[type]]
    # Columns of interest
    group_columns <- c("cas_mode", "strike_mode", "age_cat", "cas_gender")
    # Filter columns available in injuries dataset
    keep_names <- group_columns[group_columns %in% names(temp_df)]

    # summarise list of injuries by cas_mode, strike_mode, age_cat and cas_gender where this information exists
    # setDT(injury_list[[type]])
    injury_summary <- temp_df %>%
      group_by(across(keep_names)) %>%
      summarise(
        count = dplyr::n(),
        weight = mean(weight)
      ) %>%
      as.data.frame()

    # Conditional to restrict the number of injuries in dataset that don't have
    # cas_age nor cas_gender
    if (sum(names(injury_summary) %in% c("age_cat", "cas_gender")) == 0) {
      # If no age nor gender exists, then each count is multiplied by the
      # proportion of deaths found in the GBD dataset
      injury_summary$count <- injury_summary$count * PROPORTION_INJURIES_AGERANGE
    }
    #

    ## create matrices for all cas and strike mode combinations (and all age and gender combinations) with 0 counts
    temp_table <- expand.grid(lapply(as.data.frame(temp_df)[, keep_names], unique))

    # Merge counts and weights to created matrix
    temp_table <- temp_table %>%
      left_join(injury_summary, by = keep_names)
    # Fill NAs because of no injuries in some cas and strike mode combinations
    temp_table$count[is.na(temp_table$count)] <- 0
    temp_table$weight[is.na(temp_table$weight)] <- mean(temp_df$weight)

    injury_table[[type]] <- temp_table
    rm(temp_df, temp_table)
  }
  INJURY_TABLE <<- injury_table
  INJURY_TABLE_TYPES <<- injury_table_types
}
