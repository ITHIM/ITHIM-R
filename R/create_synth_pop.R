#' Creates baseline population
#'
#' Creates a baseline population by matching individuals in the trip set
#' to individuals in the physical activity (PA) dataset
#'
#' The function performs the following steps:
#'
#' \itemize{
#' \item adds age category to trip and physical activity datasets by calling assign_age_groups.R
#'
#' \item To match people in trip data with people in the physical activity dataset:
#'   \itemize{
#'   \item create a baseline population by taking the unique participant ids together with age and gender
#'     information from the trip data (not including bus driver, truck, car driver and commercial
#'     motorcycle trips)
#'
#'   \item to assign non-occupational physical activity MMET values to this
#'     baseline population, the following steps are performed:
#'
#'    \itemize{
#'     \item for each sex and age category, find the proportion of people with zero non-occupational MMET values
#'       and also find the list of people with non-zero MMET values
#'
#'     \item if BACKGROUND_PA_CONFIDENCE < 1 when calling the value of information script, i.e. when
#'       input values are sampled from distributions, a beta distribution is built from which the
#'       proportion of people with zero work and leisure MMET values is sampled using the
#'       known proportion as mean of this distribution
#'
#'     \item sample with replacement from a vector with 0 MMET values and the vector non-zero MMET values (from
#'       the people having non-zero work and leisure MMET values) using the proportion of people with
#'       zero work and leisure MMET values and assign those sampled MMET values to the baseline population
#'
#'      }
#'   }
#' \item remove participants with trip or stage modes that are not in Vehicle inventory
#' }
#'
#'
#'
#' @param raw_trip_set data frame of raw trips taken, bus_driver, new motorcycle and truck trips have already been added
#'
#' @return the baseline population and the trip set which has been pruned
#'
#' @export


create_base_pop <- function(raw_trip_set) {
  # Notes:
  ## duration: units are minutes per day.
  ## work_ltpa_marg_met: units are marginal MET-h/week.

  # Add age category for trip_set dataset.
  trip_set <- assign_age_groups(raw_trip_set, age_category = AGE_CATEGORY, age_lower_bounds = AGE_LOWER_BOUNDS, max_age = MAX_AGE)
  ## !! assuming more than one age category
  
  # match only for "real" people (i.e. not `ghost drivers', whose participant id is 0)
  # extract unique participant ids including age, sex and age category information from the trip data
  baseline_population <- filter(trip_set, !duplicated(participant_id) & participant_id > 0)[, names(trip_set) %in% c("participant_id", "age", "sex", "age_cat")]
  
  n <- nrow(baseline_population)
  
  # Example usage
  n <- nrow(baseline_population) #length(unique(baseline_population$participant_id)) # Total number of samples
  prob_zero <- 0.5 # Probability of getting a zero
  mean <- 10  # Mean of the normal distribution
  sd <- 1  # Standard deviation of the normal distribution
  
  # baseline_population <- baseline_population |> 
  #   mutate(broader_age_cat = case_when(age <= 30 ~ "15-30",
  #                                      age > 30 & age < 50 ~ "31-50",
  #                                      age > 50 ~ "50")) |> 
  #   rowwise() |> # n, prob_zero, age_group, sex, base_mean = 10, sd = 2
  #   mutate(work_ltpa_marg_met = create_bimodal_distribution(n, prob_zero, age_group = broader_age_cat, sex, mean, sd))
  
  
  baseline_population <- baseline_population |> 
    mutate(
      broader_age_cat = case_when(
        age <= 25 ~ "15-25",
        age > 25 & age <= 40 ~ "26-40",
        age > 40 & age <= 55 ~ "40-55",
        age > 55 ~ "56+"
      )
    ) |> 
    rowwise() |> 
    mutate(
      work_ltpa_marg_met = create_bimodal_distribution(
        n = 1, 
        prob_zero = if_else(age < 30, 0.3, 0.7),   # Default probability
        age_group = broader_age_cat, 
        sex = sex,
        base_mean = mean,
        sd = sd
      )
    ) |> 
    ungroup() |> 
    dplyr::select(-broader_age_cat)
  
  # Convert all integer columns to numeric
  baseline_population <- baseline_population %>%
    mutate(across(where(is.integer), as.numeric))

  # remove participants with trip or stage modes that are not in Vehicle inventory
  trip_set <- filter(trip_set, trip_mode %in% VEHICLE_INVENTORY$stage_mode & stage_mode %in% VEHICLE_INVENTORY$stage_mode)

  trip_set <- drop_na(trip_set)

  return(list(trip_set = trip_set, baseline_population = baseline_population))
}


# create_bimodal_distribution <- function(n, prob_zero, mean, sd) {
#   # Generate zeros
#   zeros <- rep(0, rbinom(1, n, prob_zero))
#   
#   # Generate non-zero values from normal distribution
#   non_zeros <- rnorm(n - length(zeros), mean, sd)
#   
#   # Combine and shuffle the results
#   result <- sample(c(zeros, non_zeros))
#   
#   return(result)
# }



create_bimodal_distribution <- function(n, prob_zero, age_group, sex, base_mean = 10, sd = 2) {
  # Set mean adjustments for age and sex
  age_adjustment <- switch(
    age_group,
    "15-25" = 6,    # Highest for young
    "26-40" = 4,    # Higher then
    "40-55" = 2,    # Lower for middle-aged
    "56+"   = 0,    # Lowest for older
    stop("Invalid age_group")
  )

  sex_adjustment <- switch(
    sex,
    "male"   = 3,   # Higher for males
    "female" = 0,   # Lower for females
    stop("Invalid sex")
  )

  # Calculate the final mean
  mean <- base_mean + age_adjustment + sex_adjustment
  
  
  # Decide if this value is zero or not
  is_zero <- rbinom(1, 1, prob_zero) == 1
  if (is_zero) {
    return(0)
  } else {
    return(rnorm(1, mean, sd))
  }

  # # Generate zeros
  # zeros <- rep(0, rbinom(1, n, prob_zero))
  # 
  # # Generate non-zero values from normal distribution
  # non_zeros <- rnorm(n - length(zeros), mean, sd)
  # 
  # # Combine and shuffle the results
  # result <- sample(c(zeros, non_zeros))
  # 
  # return(result)
}
