#' Set parameters for ITHIM-Global run
#'
#' Function to set parameters by either using the constant value or sampling from a pre-defined function
#'
#'
#' For each input parameters there are two options: to be set to a constant, or to be sampled
#' from a specified distribution. Each parameter is given as an argument of length 1 or 2.
#' If length 1, it's constant, and set to the global environment.
#' If length 2, a distribution is defined and sampled from NSAMPLE times.
#' There are some exceptions, listed below.
#'
#' The function performs the following steps:
#'
#' \itemize{
#' \item set all input parameters to the global environment (if sampling function is called, they are overwritten)
#'
#' \item loop through all potential variables with a lognormal distribution and sample from this distribution
#'   if required
#'
#' \item loop through all potential variables with a beta distribution and sample from this distribution
#'   if required
#'
#' \item if BACKGROUND_PA_CONFIDENCE<1 then add BACKGROUND_PA_ZEROS parameters
#'
#' \item if PM_EMISSION_INVENTORY_CONFIDENCE<1, then sample those PM inventory values by
#'   using a Dirichlet distribution which is parameterised by gamma random variables
#'
#' \item if CO2_EMISSION_INVENTORY_CONFIDENCE<1, then sample those CO2 inventory values by
#'   using a Dirichlet distribution which is parameterised by gamma random variables
#'
#' \item if PA_DOSE_RESPONSE_QUANTILE == T, find all diseases that are related to physical activity
#'   levels and assign a quantile to them by sampling from a uniform distribution between 0 and 1
#'
#' \item if AP_DOSE_RESPONSE_QUANTILE == T, find all diseases that are related to air pollution levels
#'   and assign a quantile to them by sampling from a uniform distribution between 0 and 1
#' }
#'
#' At the bottom of this function, the \code{\link{dirichlet_pointiness()}} function is defined which
#' parameterises the Dirichlet distributions for the PM and CO2 emission inventories.
#'
#'
#'
#' @param NSAMPLES constant integer: number of samples to take
#' @param BUS_WALK_TIME lognormal parameter: duration of walk to bus stage
#' @param RAIL_WALK_TIME lognormal parameter: duration of walk to rail stage
#' @param CYCLING_MMET lognormal parameter: MMETs when cycling
#' @param WALKING_MMET lognormal parameter: MMETs when walking
#' @param PASSENGER_MMET lognormal parameter: MMET value associated with being a passenger on public transport
#' @param CAR_DRIVER_MMET lognormal parameter: MMET value associated with being a car driver
#' @param MOTORCYCLIST_MMET lognormal parameter: MMET value associated with being a motorcyclist
#' @param SEDENTARY_ACTIVITY_MMET lognormal parameter: MMET value associated with sedentary activity
#' @param LIGHT_ACTIVITY_MMET lognormal parameter: MMET value associated with light activity
#' @param MODERATE_PA_MMET lognormal parameter: MMET value associated with moderate activity
#' @param VIGOROUS_PA_MMET lognormal parameter: MMET value associated with vigorous activity
#' @param PM_CONC_BASE lognormal parameter: background PM2.5 concentration
#' @param PM_TRANS_SHARE beta parameter: fraction of background PM2.5 attributable to transport
#' @param PA_DOSE_RESPONSE_QUANTILE logic: whether or not to sample from physical activity relative risk dose response functions
#' @param AP_DOSE_RESPONSE_QUANTILE logic: whether or not to sample from air pollution relative risk dose response functions
#' @param BACKGROUND_PA_SCALAR lognormal parameter: reporting scalar for physical activity to correct bias in data
#' @param BACKGROUND_PA_CONFIDENCE beta parameter: confidence in accuracy of zero non-travel physical activity levels
#' @param INJURY_REPORTING_RATE lognormal parameter: rate of injury fatality reporting
#' @param CHRONIC_DISEASE_SCALAR lognormal parameter: scalar for background disease rates to adjust for bias in GBD data
#' @param DAY_TO_WEEK_TRAVEL_SCALAR beta parameter: rate of scaling travel from one day to one week - CURRENTLY used as constant only (using as beta parameter would need some further considerations)
#' @param SIN_EXPONENT_SUM lognormal parameter: linearity of injuries with respect to two modes. SIN_EXPONENT_SUM=2 means no safety in numbers
#' @param CASUALTY_EXPONENT_FRACTION beta parameter: casualty exponent contribution to SIN_EXPONENT_SUM
#' @param SIN_EXPONENT_SUM_NOV lognormal parameter: linearity of injuries with respect to two modes where strike mode = NOV. SIN_EXPONENT_SUM=2 means no safety in numbers
#' @param SIN_EXPONENT_SUM_CYCLE lognormal parameter: linearity of injuries with respect to two modes where victim mode = cycle. SIN_EXPONENT_SUM=2 means no safety in numbers
#' @param CASUALTY_EXPONENT_FRACTION_CYCLE beta parameter: casualty exponent contribution to SIN_EXPONENT_SUM_CYCLE  where victim mode = cycle
#' @param SIN_EXPONENT_SUM_PED lognormal parameter: linearity of injuries with respect to two modes  where victim mode = pedestrian. SIN_EXPONENT_SUM=2 means no safety in numbers
#' @param CASUALTY_EXPONENT_FRACTION_PED beta parameter: casualty exponent contribution to SIN_EXPONENT_SUM_PED where victim mode = pedestrian
#' @param SIN_EXPONENT_SUM_VEH lognormal parameter: linearity of injuries with respect to two modes where victim mode = a vehicle. SIN_EXPONENT_SUM=2 means no safety in numbers
#' @param CASUALTY_EXPONENT_FRACTION_VEH beta parameter: casualty exponent contribution to SIN_EXPONENT_SUM_VEH where victim mode = a vehicle
#' @param CALL_INDIVIDUAL_SIN logic: whether or not to call the safety in number coefficients for individual vehicles or use the same coefficients for all modes
#' @param BUS_TO_PASSENGER_RATIO beta parameter: number of buses per passenger
#' @param CAR_OCCUPANCY_RATIO beta parameter: number of people per car (including driver)
#' @param TRUCK_TO_CAR_RATIO beta parameter: proportion of truck to car vehicle km travelled
#' @param FLEET_TO_MOTORCYCLE_RATIO beta parameter: amount of motorcycle trips that are to be added as commercial trips
#' @param PM_EMISSION_INVENTORY_CONFIDENCE beta parameter: confidence in accuracy of PM emission inventory
#' @param PROPORTION_MOTORCYCLE_TRIPS beta parameter: proportion of trips that are to be added as personal motorcycle trips
#' @param CO2_EMISSION_INVENTORY_CONFIDENCE beta parameter: confidence in accuracy of CO2 emission inventory
#' @param DISTANCE_SCALAR_CAR_TAXI lognormal parameter: scalar to adjust for bias in car distance travelled
#' @param DISTANCE_SCALAR_WALKING lognormal parameter: scalar to adjust for bias in walking distance travelled
#' @param DISTANCE_SCALAR_PT lognormal parameter: scalar to adjust for bias in PT distance travelled
#' @param DISTANCE_SCALAR_CYCLING lognormal parameter: scalar to adjust for bias in cycling distance travelled
#' @param DISTANCE_SCALAR_MOTORCYCLE lognormal parameter: scalar to adjust for biase in motorcycle distance travelled
#' @param BUS_DRIVER_PROP_MALE scalar: proportion of bus drivers that are male
#' @param BUS_DRIVER_MALE_AGERANGE character: age range of male bus drivers
#' @param BUS_DRIVER_FEMALE_AGERANGE character: age range of female bus drivers
#' @param TRUCK_DRIVER_PROP_MALE scalar: proportion of truck drivers that are male
#' @param TRUCK_DRIVER_MALE_AGERANGE character: age range of male truck drivers
#' @param TRUCK_DRIVER_FEMALE_AGERANGE character: age range of female truck drivers
#' @param COMMERCIAL_MBIKE_PROP_MALE scalar: proportion of commercial motorcycle drivers that are male
#' @param COMMERCIAL_MBIKE_MALE_AGERANGE character: age range of male commercial motorcycle drivers
#' @param COMMERCIAL_MBIKE_FEMALE_AGERANGE character: age range of female commercial motorcycle drivers
#' @param MINIMUM_PT_TIME scalar: minimum time that person spends on public transport
#' @param MODERATE_PA_CONTRIBUTION scalar: proportion contribution of moderate PA in Leisure MVPA
#'
#' @return list of samples of uncertain parameters
#'
#' @export
ithim_setup_parameters <- function(NSAMPLES = 1,
                                   BUS_WALK_TIME = 16,
                                   RAIL_WALK_TIME = 12.5,
                                   CYCLING_MMET = 5.8,
                                   WALKING_MMET = 2.5,
                                   PASSENGER_MMET = 0.3,
                                   CAR_DRIVER_MMET = 1.5,
                                   MOTORCYCLIST_MMET = 1.8,
                                   SEDENTARY_ACTIVITY_MMET = 0.3,
                                   LIGHT_ACTIVITY_MMET = 2,
                                   MODERATE_PA_MMET = 3,
                                   VIGOROUS_PA_MMET = 7,
                                   PM_CONC_BASE = 12.69,
                                   PM_TRANS_SHARE = 0.42,
                                   PA_DOSE_RESPONSE_QUANTILE = F,
                                   AP_DOSE_RESPONSE_QUANTILE = F,
                                   BACKGROUND_PA_SCALAR = 1,
                                   BACKGROUND_PA_CONFIDENCE = 1,
                                   INJURY_REPORTING_RATE = 1,
                                   CHRONIC_DISEASE_SCALAR = 1,
                                   DAY_TO_WEEK_TRAVEL_SCALAR = 7,
                                   SIN_EXPONENT_SUM = 2,
                                   CASUALTY_EXPONENT_FRACTION = 0.5,
                                   SIN_EXPONENT_SUM_NOV = 1,
                                   SIN_EXPONENT_SUM_CYCLE = 2,
                                   CASUALTY_EXPONENT_FRACTION_CYCLE = 0.5,
                                   SIN_EXPONENT_SUM_PED = 2,
                                   CASUALTY_EXPONENT_FRACTION_PED = 0.5,
                                   SIN_EXPONENT_SUM_VEH = 2,
                                   CASUALTY_EXPONENT_FRACTION_VEH = 0.5,
                                   BUS_TO_PASSENGER_RATIO = 0.0389,
                                   CAR_OCCUPANCY_RATIO = 0.625,
                                   TRUCK_TO_CAR_RATIO = 0.3,
                                   FLEET_TO_MOTORCYCLE_RATIO = 0.441,
                                   PROPORTION_MOTORCYCLE_TRIPS = 0,
                                   PM_EMISSION_INVENTORY_CONFIDENCE = 1,
                                   CO2_EMISSION_INVENTORY_CONFIDENCE = 1,
                                   DISTANCE_SCALAR_CAR_TAXI = 1,
                                   DISTANCE_SCALAR_WALKING = 1,
                                   DISTANCE_SCALAR_PT = 1,
                                   DISTANCE_SCALAR_CYCLING = 1,
                                   DISTANCE_SCALAR_MOTORCYCLE = 1,
                                   BUS_DRIVER_PROP_MALE = 0.98,
                                   BUS_DRIVER_MALE_AGERANGE = "19, 65",
                                   BUS_DRIVER_FEMALE_AGERANGE = "19, 65",
                                   TRUCK_DRIVER_PROP_MALE = 0.98,
                                   TRUCK_DRIVER_MALE_AGERANGE = "18, 65",
                                   TRUCK_DRIVER_FEMALE_AGERANGE = "18, 65",
                                   COMMERCIAL_MBIKE_PROP_MALE = 0.99,
                                   COMMERCIAL_MBIKE_MALE_AGERANGE = "18, 65",
                                   COMMERCIAL_MBIKE_FEMALE_AGERANGE = "18, 65",
                                   MINIMUM_PT_TIME = 3,
                                   MODERATE_PA_CONTRIBUTION = 0.5,
                                   CALL_INDIVIDUAL_SIN = F,
                                   SCENARIO_NAME = "GLOBAL",
                                   SCENARIO_INCREASE = 0.05) {
  # Check if default values are set in which case a warning message appears to change the default values
  # for PM_CONC_BASE and PM_TRANS_SHAREs
  # if ((length(PM_CONC_BASE==1)&&PM_CONC_BASE == 50) |
  #     (length(PM_TRANS_SHARE==1)&&PM_TRANS_SHARE == 0.225))
  # error_handling(1, "ithim_setup_parameters", "PM_CONC_BASE, PM_TRANS_SHARE")

  ## Set PARAMETERS
  # Parameters are assigned to the global environment and so are set for every function
  # They are over-written when sample_parameters is called.
  BUS_WALK_TIME <<- BUS_WALK_TIME
  RAIL_WALK_TIME <<- RAIL_WALK_TIME
  CYCLING_MET <<- CYCLING_MMET + 1
  WALKING_MET <<- WALKING_MMET + 1
  PASSENGER_MET <<- PASSENGER_MMET + 1
  CAR_DRIVER_MET <<- CAR_DRIVER_MMET + 1
  MOTORCYCLIST_MET <<- MOTORCYCLIST_MMET + 1
  SEDENTARY_ACTIVITY_MET <<- SEDENTARY_ACTIVITY_MMET + 1
  LIGHT_ACTIVITY_MET <<- LIGHT_ACTIVITY_MMET + 1
  MODERATE_PA_MET <<- MODERATE_PA_MMET + 1
  VIGOROUS_PA_MET <<- VIGOROUS_PA_MMET + 1
  PM_CONC_BASE <<- PM_CONC_BASE
  PM_TRANS_SHARE <<- PM_TRANS_SHARE
  PA_DOSE_RESPONSE_QUANTILE <<- PA_DOSE_RESPONSE_QUANTILE
  BACKGROUND_PA_SCALAR <<- BACKGROUND_PA_SCALAR
  BACKGROUND_PA_CONFIDENCE <<- BACKGROUND_PA_CONFIDENCE
  INJURY_REPORTING_RATE <<- INJURY_REPORTING_RATE
  CHRONIC_DISEASE_SCALAR <<- CHRONIC_DISEASE_SCALAR
  SIN_EXPONENT_SUM <<- SIN_EXPONENT_SUM
  CASUALTY_EXPONENT_FRACTION <<- CASUALTY_EXPONENT_FRACTION
  SIN_EXPONENT_SUM_NOV <<- SIN_EXPONENT_SUM_NOV
  SIN_EXPONENT_SUM_CYCLE <<- SIN_EXPONENT_SUM_CYCLE
  SIN_EXPONENT_SUM_PED <<- SIN_EXPONENT_SUM_PED
  CASUALTY_EXPONENT_FRACTION_PED <<- CASUALTY_EXPONENT_FRACTION_PED
  SIN_EXPONENT_SUM_VEH <<- SIN_EXPONENT_SUM_VEH
  CASUALTY_EXPONENT_FRACTION_VEH <<- CASUALTY_EXPONENT_FRACTION_VEH
  BUS_TO_PASSENGER_RATIO <<- BUS_TO_PASSENGER_RATIO
  CAR_OCCUPANCY_RATIO <<- CAR_OCCUPANCY_RATIO
  TRUCK_TO_CAR_RATIO <<- TRUCK_TO_CAR_RATIO
  FLEET_TO_MOTORCYCLE_RATIO <<- FLEET_TO_MOTORCYCLE_RATIO
  PROPORTION_MOTORCYCLE_TRIPS <<- PROPORTION_MOTORCYCLE_TRIPS
  DISTANCE_SCALAR_CAR_TAXI <<- DISTANCE_SCALAR_CAR_TAXI
  DISTANCE_SCALAR_WALKING <<- DISTANCE_SCALAR_WALKING
  DISTANCE_SCALAR_PT <<- DISTANCE_SCALAR_PT
  DISTANCE_SCALAR_CYCLING <<- DISTANCE_SCALAR_CYCLING
  DISTANCE_SCALAR_MOTORCYCLE <<- DISTANCE_SCALAR_MOTORCYCLE
  BUS_DRIVER_PROP_MALE <<- BUS_DRIVER_PROP_MALE
  BUS_DRIVER_MALE_AGERANGE <<- BUS_DRIVER_MALE_AGERANGE
  BUS_DRIVER_FEMALE_AGERANGE <<- BUS_DRIVER_FEMALE_AGERANGE
  TRUCK_DRIVER_PROP_MALE <<- TRUCK_DRIVER_PROP_MALE
  TRUCK_DRIVER_MALE_AGERANGE <<- TRUCK_DRIVER_MALE_AGERANGE
  TRUCK_DRIVER_FEMALE_AGERANGE <<- TRUCK_DRIVER_FEMALE_AGERANGE
  COMMERCIAL_MBIKE_PROP_MALE <<- COMMERCIAL_MBIKE_PROP_MALE
  COMMERCIAL_MBIKE_MALE_AGERANGE <<- COMMERCIAL_MBIKE_MALE_AGERANGE
  COMMERCIAL_MBIKE_FEMALE_AGERANGE <<- COMMERCIAL_MBIKE_FEMALE_AGERANGE
  MINIMUM_PT_TIME <<- MINIMUM_PT_TIME
  MODERATE_PA_CONTRIBUTION <<- MODERATE_PA_CONTRIBUTION
  parameters <- list()

  # Variables with lognormal distribution (no MMET values and no sin_exponent_sum_veh)
  # Define those variables and loop through them, sampling
  # from a pre-defined lognormal distribution if needed
  normVariables <- c(
    "BUS_WALK_TIME",
    "RAIL_WALK_TIME",
    "PM_CONC_BASE",
    "BACKGROUND_PA_SCALAR",
    "SIN_EXPONENT_SUM",
    "SIN_EXPONENT_SUM_NOV",
    "SIN_EXPONENT_SUM_CYCLE",
    "SIN_EXPONENT_SUM_PED",
    "DISTANCE_SCALAR_WALKING",
    "CHRONIC_DISEASE_SCALAR",
    "DISTANCE_SCALAR_PT",
    "DISTANCE_SCALAR_CAR_TAXI",
    "DISTANCE_SCALAR_CYCLING",
    "DISTANCE_SCALAR_MOTORCYCLE"
  )
  for (i in 1:length(normVariables)) {
    name <- normVariables[i]
    val <- get(normVariables[i])
    if (length(val) == 1) { # if variable length is 1, do not sample
      assign(name, val, envir = .GlobalEnv)
    } else {
      # Use mean and sd values in log form, sample from a lognormal distribution
      parameters[[name]] <-
        rlnorm(NSAMPLES, log(val[1]), log(val[2]))
    }
  }

  
  
  # MMET values with lognormal distribution
  # Define those variables and loop through them, sampling
  # from a pre-defined lognormal distribution if needed and add 1
  normVariablesMMET <- c(
    "CYCLING_MMET",
    "WALKING_MMET",
    "PASSENGER_MMET",
    "CAR_DRIVER_MMET",
    "MOTORCYCLIST_MMET",
    "SEDENTARY_ACTIVITY_MMET",
    "LIGHT_ACTIVITY_MMET",
    "MODERATE_PA_MMET",
    "VIGOROUS_PA_MMET"
  )
  for (i in 1:length(normVariablesMMET)) {
    name <- gsub('MMET', 'MET',normVariablesMMET[i])
    val <- get(normVariablesMMET[i])
    if (length(val) == 1) { # if variable length is 1, do not sample
      assign(name, val + 1, envir = .GlobalEnv)
    } else {
      # Use mean and sd values in log form, sample from a lognormal distribution
      parameters[[name]] <-
        (rlnorm(NSAMPLES, log(val[1]), log(val[2])) + 1)
    }
  }
  
  
  
  # SIN_EXPONENT_SUM_VEH with lognormal distribution
  # Define those variables and loop through them, sampling
  # from a pre-defined lognormal distribution with mean -1 of the original mean
  # and if needed and add 1
  normVariablesSIN_VEH <- c(
    "SIN_EXPONENT_SUM_VEH"
  )
  
  name <- normVariablesSIN_VEH[1]
  val <- get(normVariablesSIN_VEH[1])
  if (length(val) == 1) { # if variable length is 1, do not sample
    assign(name, val , envir = .GlobalEnv)
  } else {
    # calculate original mean and standard deviation from log_mean and log_std
    log_mean <- val[1]
    log_std <- val[2]
    mean_orig <- exp(log(log_mean)+(log(log_std))^2/2)
    std_orig <- sqrt((exp((log(log_std))^2)-1)*exp(2*log(log_mean)+(log(log_std))^2))
    
    # subtract 1 from original mean but keep std
    mean_up <- mean_orig - 1
    std_up <- std_orig
    
    # recalculate log_mean_up and log_std_up
    log_mean_up <- mean_up^2/(sqrt(mean_up^2 + std_up^2))
    log_std_up <- exp( sqrt(log(1 + (std_up^2/mean_up^2))))
    
    # Use mean and sd values in log form, sample from a lognormal distribution
    parameters[[name]] <-
      (rlnorm(NSAMPLES, log(log_mean_up), log(log_std_up)) + 1)
  }
  
  
  
  
  # Variables with beta distribution
  # Define those variables and loop through them, sampling
  # from a pre-defined beta distribution if needed
  betaVariables <- c(
    "PM_TRANS_SHARE",
    "INJURY_REPORTING_RATE",
    "CASUALTY_EXPONENT_FRACTION",
    "BUS_TO_PASSENGER_RATIO",
    "CAR_OCCUPANCY_RATIO",
    "TRUCK_TO_CAR_RATIO",
    "FLEET_TO_MOTORCYCLE_RATIO",
    "PROPORTION_MOTORCYCLE_TRIPS",
    "CASUALTY_EXPONENT_FRACTION",
    "CASUALTY_EXPONENT_FRACTION_CYCLE",
    "CASUALTY_EXPONENT_FRACTION_PED",
    "CASUALTY_EXPONENT_FRACTION_VEH"
  )
  for (i in 1:length(betaVariables)) {
    name <- betaVariables[i]
    val <- get(betaVariables[i])
    if (length(val) == 1) { # if variable length is 1, do not sample
      assign(name, val, envir = .GlobalEnv)
    } else {
      parameters[[name]] <-
        rbeta(NSAMPLES, val[1], val[2]) # sample from beta distribution
    }
  }

  # option to sample from DAY_TO_WEEK_TRAVEL_SCALAR, however input parameter spreadsheet
  # currently only set-up to read in this parameter as a scalar
  # sampling from a beta distribution also requires further consideration as scalar
  # should have the option to be larger than 1
  # if(length(DAY_TO_WEEK_TRAVEL_SCALAR) > 1 ){
  #   parameters$DAY_TO_WEEK_TRAVEL_SCALAR <- 7*rbeta(NSAMPLES,DAY_TO_WEEK_TRAVEL_SCALAR[1],DAY_TO_WEEK_TRAVEL_SCALAR[2])
  # }else{
  #   DAY_TO_WEEK_TRAVEL_SCALAR <<- DAY_TO_WEEK_TRAVEL_SCALAR
  # }

  # if BACKGROUND_PA_CONFIDENCE<1 then add BACKGROUND_PA_ZEROS parameters
  if (BACKGROUND_PA_CONFIDENCE < 1) {
    parameters$BACKGROUND_PA_ZEROS <- runif(NSAMPLES, 0, 1)
  }

  # if PM_EMISSION_INVENTORY_CONFIDENCE<1, then sample those PM inventory values by
  # using a Dirichlet distribution which is parameterised by gamma random variables
  # if (PM_EMISSION_INVENTORY_CONFIDENCE < 1) {
  #   total <- sum(unlist(PM_EMISSION_INVENTORY))
  #   parameters$PM_EMISSION_INVENTORY <- list()
  #   for (n in 1:NSAMPLES) {
  #     samples <- lapply(PM_EMISSION_INVENTORY, function(x) rgamma(1, shape = x / total * dirichlet_pointiness(PM_EMISSION_INVENTORY_CONFIDENCE), scale = 1))
  #     new_total <- sum(unlist(samples))
  #     parameters$PM_EMISSION_INVENTORY[[n]] <- lapply(samples, function(x) x / new_total)
  #   }
  # }

  
  
  # if PM_EMISSION_INVENTORY_CONFIDENCE<1, then sample those PM inventory values by
  # using Beta distributions 
  if (PM_EMISSION_INVENTORY_CONFIDENCE < 1) {
    pm_inv <- sort(unlist(PM_EMISSION_INVENTORY))
    
    total <- sum(pm_inv)
    
    # ensure working with proportions that add up to 1
    if (total != 1){
      pm_inv <- pm_inv / total
      total <- sum(pm_inv)
    }
    
    # initialise output values
    estimate <- 0
    estimate_list <- list()
    estimate_list[['sum']] <- c(rep(0, NSAMPLES))
    
    conf <- PM_EMISSION_INVENTORY_CONFIDENCE
    # define standard deviation of beta distributions (sd also depends on emission proportion, see below)
    std <- (1-conf)^1.2/16
    
    # loop through proportions in the inventory
    for (i in 1:(length(pm_inv)-1)){
      if (pm_inv[i]==0){
        estimate <- 0
        estimate_list[[i+1]] <- c(rep(0, NSAMPLES))
      } else {
        total <- total - estimate #calculate new total by subtracting the newest estimate
        new_prop <- pm_inv[[i]]/total
        mean <- new_prop  #set mean to the be this new proportion
        sd <- std * mean^0.25  #define standard deviation
        # calculate alpha and beta values of Beta distribution with above mean and std
        alpha <- (mean*(1-mean)/sd^2-1)*mean
        beta <- (mean*(1-mean)/sd^2-1)*(1-mean)
        sample <- rbeta(NSAMPLES,alpha, beta)  #sample from beta distribution
        estimate <- sample * total # re-scale to original proportion
        estimate_list[[i+1]] <- estimate  #save new estimate
        estimate_list[['sum']] <- estimate_list[['sum']] + estimate  #sum across all estimates
      }
    }
    
    # for final proportion
    estimate_list[[length(PM_EMISSION_INVENTORY)+1]] <- 1 - estimate_list[['sum']]
    
    # get into correct format 
    for (j in 1:NSAMPLES){
      pm_inv_updated <- pm_inv
      for (i in 2:length(estimate_list)){
        pm_inv_updated[i-1] <- estimate_list[[i]][[j]]  
      }
      # get into correct order again
      PM_inventory_updated <- PM_EMISSION_INVENTORY
      for (k in names(PM_inventory_updated)){
        PM_inventory_updated[k] <- pm_inv_updated[k]
      }
      parameters$PM_EMISSION_INVENTORY[[j]] <- PM_inventory_updated
    }
    
  }


  # if CO2_EMISSION_INVENTORY_CONFIDENCE<1, then sample those CO2 inventory values by
  # using a Dirichlet distribution which is parameterised by gamma random variables
  # if (CO2_EMISSION_INVENTORY_CONFIDENCE < 1) {
  #   total <- sum(unlist(CO2_EMISSION_INVENTORY))
  #   parameters$CO2_EMISSION_INVENTORY <- list()
  #   for (n in 1:NSAMPLES) {
  #     samples <- lapply(CO2_EMISSION_INVENTORY, function(x) rgamma(1, shape = x / total * dirichlet_pointiness(CO2_EMISSION_INVENTORY_CONFIDENCE), scale = 1))
  #     new_total <- sum(unlist(samples))
  #     parameters$CO2_EMISSION_INVENTORY[[n]] <- lapply(samples, function(x) x / new_total * total) # assuming total CO2 emissions stay the same
  #   }
  # }
  
  # if CO2_EMISSION_INVENTORY_CONFIDENCE<1, then sample those CO2 inventory values by
  # using Beta distributions 
  if (CO2_EMISSION_INVENTORY_CONFIDENCE < 1) {
    CO2_inv <- sort(unlist(CO2_EMISSION_INVENTORY))
    
    total <- sum(CO2_inv)
    
    # ensure working with proportions that add up to 1
    if (total != 1){
      CO2_inv <- CO2_inv / total
      total <- sum(CO2_inv)
    }
    
    # initialise output values
    estimate <- 0
    estimate_list <- list()
    estimate_list[['sum']] <- c(rep(0, NSAMPLES))
    
    conf <- CO2_EMISSION_INVENTORY_CONFIDENCE
    # define standard deviation of beta distributions (sd also depends on emission proportion, see below)
    std <- (1-conf)^1.2/16
    
    # loop through proportions in the inventory
    for (i in 1:(length(CO2_inv)-1)){
      if (CO2_inv[i]==0){
        estimate <- 0
        estimate_list[[i+1]] <- c(rep(0, NSAMPLES))
      } else {
        total <- total - estimate #calculate new total by subtracting the newest estimate
        new_prop <- CO2_inv[[i]] /total # re-calculate proportions of ith value in inventory
        mean <- new_prop  #set mean to the be this new proportion
        sd <- std * mean^0.25  #define standard deviation
        # calculate alpha and beta values of Beta distribution with above mean and std
        alpha <- (mean*(1-mean)/sd^2-1)*mean
        beta <- (mean*(1-mean)/sd^2-1)*(1-mean)
        sample <- rbeta(NSAMPLES,alpha, beta)  #sample from beta distribution
        estimate <- sample * total # re-calculate the proportion of this sample from 1
        estimate_list[[i+1]] <- estimate  #save new estimate
        estimate_list[['sum']] <- estimate_list[['sum']] + estimate  #sum across all estimates
      }
    }
    
    # for final proportion
    estimate_list[[length(CO2_EMISSION_INVENTORY)+1]] <- 1 - estimate_list[['sum']]
    
    # get into correct format
    for (j in 1:NSAMPLES){
      co2_inv_updated <- CO2_inv
      for (i in 2:length(estimate_list)){
        co2_inv_updated[i-1] <- estimate_list[[i]][[j]]  
      }
      # get into correct order again
      CO2_inventory_updated <- CO2_EMISSION_INVENTORY
      for (k in names(CO2_inventory_updated)){
        CO2_inventory_updated[k] <- co2_inv_updated[k]
      }
      parameters$CO2_EMISSION_INVENTORY[[j]] <- CO2_inventory_updated
    }
    

  }

  
  

  # PA DOSE RESPONSE
  # if PA_DOSE_RESPONSE_QUANTILE == T, find all diseases that are related to
  # physical activity levels and assign a quantile to them by sampling from a uniform
  # distribution between 0 and 1
  if (PA_DOSE_RESPONSE_QUANTILE == T) {
    pa_diseases <- subset(DISEASE_INVENTORY, physical_activity == 1)
    dr_pa_list <- list()
    for (disease in pa_diseases$pa_acronym) {
      parameters[[paste0("PA_DOSE_RESPONSE_QUANTILE_", disease)]] <- runif(NSAMPLES, 0, 1)
    }
  }


  # AP DOSE RESPONSE
  # if AP_DOSE_RESPONSE_QUANTILE == T, find all diseases that are related to
  # air pollution levels and assign a quantile to them by sampling from a uniform
  # distribution between 0 and 1
  if (AP_DOSE_RESPONSE_QUANTILE == T) {
    ap_diseases <- subset(DISEASE_INVENTORY, air_pollution == 1)
    dr_ap_list <- list()
    for (disease in ap_diseases$ap_acronym) {
      parameters[[paste0("AP_DOSE_RESPONSE_QUANTILE_", disease)]] <- runif(NSAMPLES, 0, 1)
    }
  }

  parameters
}


#' Function for Dirichlet parameters
#'
#' Function to map a confidence value to a parameterisation of a Dirichlet distribution.
#'
#' Note that the parameterisation is somewhat arbitrary but seems to work on visual inspection.
#'
#' @param confidence value between 0 and 1
#'
#' @return parameterisation
#'
#' @export
dirichlet_pointiness <- function(confidence) {
  exp((2.25 * confidence + 1)^2)
}
