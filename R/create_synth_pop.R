#' Creates synthetic population
#' 
#' Creates a synthetic population by matching individuals in the trip set to individuals in the PA set
#' 
#' The function performs the following steps:
#' 
#' - adds age category to trip and physical activity datasets
#' - To match people in trip data with people in the physical activity dataset:
#'   - create a synthetic population by taking the unique participant ids together with age and gender
#'     information from the trip data (not including bus driver, truck, car driver and commercial 
#'     motorcycle trips)
#'   - to assign non - travel (i.e. work and leisure) physical activity MMET values to this 
#'     synthetic population, the following steps are performed:
#'     - for each sex and age category, find the proportion of people with zero work and leisure MMET values
#'       and also find the list of people with non-zero MMET values
#'     - if BACKGROUND_PA_CONFIDENCE < 1 when calling the value of information script, i.e. when 
#'       input values are sampled from distributions, a beta distribution is built from which the 
#'       proportion of people with zero work and leisure MMET values is sample using the  
#'       known proportion as mean of this distribution
#'     - sample with replacement from a vector with 0 MMET values and the vector non-zero MMET values (from
#'       the people having non-zero work and leisure MMET values) using the proportion of people with
#'       zero work and leisure MMET values and assign those sampled MMET values to the synthetic population
#' - remove any non-travel trips from the trip data
#'
#' 
#' 
#' 
#' @param raw_trip_set data frame of raw trips taken, bus_driver, new motorcycle and truck trips have already been added
#' 
#' @return the synthetic population and the trip set which has been pruned
#' 
#' @export


create_synth_pop <- function(raw_trip_set){

  #Notes:
  ##duration: units are minutes per day.
  ##work_ltpa_marg_met: units are marginal MET-h/week.
  
  # Add age category for trip_set dataset.
  trip_set <- assign_age_groups(raw_trip_set,age_category=AGE_CATEGORY,age_lower_bounds=AGE_LOWER_BOUNDS,max_age=MAX_AGE)
  ##!! assuming more than one age category
  
  # assign age categories to the physical activity dataset
  pa <- PA_SET
  #Make age category for pa dataset.
  if(CITY=='accra'){
    age_category <- c("15-55", "56-69","70+")
    age_lower_bounds <- c(15,56,70)
  }else{
    age_category <- AGE_CATEGORY
    age_lower_bounds <- AGE_LOWER_BOUNDS
  }
  pa <- assign_age_groups(pa,age_category=age_category,age_lower_bounds)
 
  
   
  ###  Match people in the trip_set with people in the physical activity datasets
 
  column_to_keep <- which(colnames(pa)%in%c('work_ltpa_marg_met')) # define which column to keep from pa data
  unique_ages <- unique(trip_set$age_cat) # find unqiue age categories in trip set
  unique_genders <- unique(trip_set$sex) # find unique sexes in trip set
  
  # match only for "real" people (i.e. not `ghost drivers', whose participant id is 0)
  # extract unique participant ids including age, sex and age category information from the trip data
  synthetic_population <- subset(trip_set,!duplicated(participant_id)&participant_id>0)[,names(trip_set)%in%c("participant_id","age","sex","age_cat")]
  ## initialise zeros and densities
  zeros <- densities <- list()
  for(age_group in unique_ages){ # loop through age categories
    zeros[[age_group]] <- densities[[age_group]] <- list() # initialise lists
    pa_age_category <- age_category[which(AGE_CATEGORY==age_group)]
    for(gender in unique_genders){ # loop through sexes
      # find people in pa data with given age category and sex
      matching_people <- as.data.frame(filter(pa, age_cat == pa_age_category & sex == gender)[,column_to_keep])
      raw_zero <- 1 # raw_zero is the proportion of people with given sex and age_category who have zero non-travel met values
      if(nrow(matching_people)>0){ # if there are people with the right sex and age category in the pa dataset
        # find proportion of those people whose work and leisure physical activity met is equal to 0
        raw_zero <- sum(matching_people$work_ltpa_marg_met==0)/length(matching_people$work_ltpa_marg_met)
      } 
      if(BACKGROUND_PA_CONFIDENCE < 1){ # option to sample the raw_zero proportion from a beta distribution
        mean <- raw_zero # define mean of beta distribution
        if (raw_zero == 0) mean <- 0.001 # can't build beta distribution with a mean of 0 or 1
        if (raw_zero == 1) mean <- 0.999
        std <- (1-BACKGROUND_PA_CONFIDENCE)/5  # define standard deviation of beta distribution
        #std <- 1/(BACKGROUND_PA_CONFIDENCE^2 + 0.07) / 100 
        
        # define alpha and beta values and sample from the corresponding distribution
        alpha <- abs((mean*(1-mean)/std^2-1)*mean)
        beta <- abs((mean*(1-mean)/std^2-1)*(1-mean))
        raw_zero <- rbeta(1,alpha,beta)
      }
      zeros[[age_group]][[gender]] <- raw_zero # proportion of people with given sex and age_category who have zero non-travel met values
      densities[[age_group]][[gender]] <- matching_people$work_ltpa_marg_met[matching_people$work_ltpa_marg_met>0] # people with non-zero work and leisure pa
    }
  }
  
  # assign all participants 0 leisure/work mmets
  synthetic_population$work_ltpa_marg_met <- 0
  
  # match population to PA dataset via demographic information
  for(age_group in unique_ages){ # loop through age groups
    pa_age_category <- age_category[which(AGE_CATEGORY==age_group)]
    for(gender in unique_genders){ # loop through sexes
      i <- which(synthetic_population$age_cat==age_group&synthetic_population$sex==gender)
      raw_density <- densities[[age_group]][[gender]] # people with non zero work and leisure met
      prob_zero <- zeros[[age_group]][[gender]] # proportion of people with zero work and leisure met
      # sample with from 0 and pa of people with non-zero pa with replacement 
      v <- sample(c(0,raw_density),length(i),replace=T,prob=c(prob_zero,(rep(1,length(raw_density))-prob_zero)/length(raw_density)))
      if (length(v) > 0)
        # assign new mmet to synthetic population (from trip data) with given age category and sex
        synthetic_population$work_ltpa_marg_met[i] <- c(v)
    }
  }
  
  # Convert all integer columns to numeric
  synthetic_population[, sapply(synthetic_population,class)=='integer'] <- lapply(synthetic_population[, sapply(synthetic_population,class)=='integer'], as.numeric)
  
  # remove non-travelling participants
  trip_set <- subset(trip_set,trip_mode%in%VEHICLE_INVENTORY$stage_mode&stage_mode%in%VEHICLE_INVENTORY$stage_mode)

  trip_set <- drop_na(trip_set)
  
  return(list(trip_set=trip_set,synthetic_population=synthetic_population))
  
}


#' Parameterise confidence in PA data - CURRENTLY NOT IN USE
#' 
#' Takes a confidence value between 0 and 1 and returns a parameter for a beta distribution
#' 
#' @param confidence value between 0 and 1 representing how confident we are about the PA dataset
#' 
#' @return a value to parametrise a beta distribution
#' 
#' @export
beta_pointiness <- function(confidence){
  500^(confidence+0.2)
}