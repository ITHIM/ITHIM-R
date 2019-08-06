#' Creates synthetic population
#' 
#' Creates a synthetic population by matching individuals in the trip set to individuals in the PA set
#' 
#' @param raw_trip_set data frame of raw trips taken
#' 
#' @return the synthetic population and the trip set which has been pruned
#' 
#' @export
create_synth_pop <- function(raw_trip_set){
  #Add physical activity variables to trip dataset.
  #Leandro Garcia & Ali Abbas.
  #5 July 2018.
  
  # Last Updated by Ali Abbas
  # Added 32 new motorcyle trips 
  # Multiplied baseline dataset by 4
  
  #Notes:
  ##trip_mode = '99': persons who did not travel.
  ##duration: units are minutes per day.
  ##work_ltpa_marg_met: units are marginal MET-h/week.
  
  # Make age category for trip_set dataset.
  trip_set <- assign_age_groups(raw_trip_set,age_category=AGE_CATEGORY,age_lower_bounds=AGE_LOWER_BOUNDS,max_age=MAX_AGE)
  ##!! assuming more than one age category
  
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
  
  #Match persons in the trip (trip_set) e physical activity datasets.
  column_to_keep <- which(colnames(pa)%in%c('work_ltpa_marg_met'))
  unique_ages <- unique(trip_set$age_cat)
  unique_genders <- unique(trip_set$sex)
  
  if(BACKGROUND_PA_CONFIDENCE < 1){
    pointiness <- beta_pointiness(BACKGROUND_PA_CONFIDENCE)
  }
  
  # get population from trip_set: all the unique ids, and their demographic information
  # match only for "real" people (i.e. not `ghost drivers', whose id is 0)
  synthetic_population <- subset(trip_set,!duplicated(participant_id)&participant_id>0)[,names(trip_set)%in%c("participant_id","age","sex","age_cat")]
  ## get zeros and densities
  zeros <- densities <- list()
  for(age_group in unique_ages){
    zeros[[age_group]] <- densities[[age_group]] <- list()
    pa_age_category <- age_category[which(AGE_CATEGORY==age_group)]
    for(gender in unique_genders){
      matching_people <- as.data.frame(filter(pa, age_cat == pa_age_category & sex == gender)[,column_to_keep])
      raw_zero <- 1
      if(nrow(matching_people)>0) raw_zero <- sum(matching_people$work_ltpa_marg_met==0)/length(matching_people$work_ltpa_marg_met)
      if(BACKGROUND_PA_CONFIDENCE < 1){
        beta <- ifelse(raw_zero==0,0,(1/raw_zero - 1)*pointiness*raw_zero)
        alpha <- pointiness - beta
        raw_zero <- qbeta(BACKGROUND_PA_ZEROS,alpha,beta)
      }
      zeros[[age_group]][[gender]] <- raw_zero
      densities[[age_group]][[gender]] <- matching_people$work_ltpa_marg_met[matching_people$work_ltpa_marg_met>0]
    }
  }
  
  # assign all participants 0 leisure/work mmets
  synthetic_population$work_ltpa_marg_met <- 0
  # match population to PA dataset via demographic information
  for(age_group in unique_ages){
    pa_age_category <- age_category[which(AGE_CATEGORY==age_group)]
    for(gender in unique_genders){
      i <- which(synthetic_population$age_cat==age_group&synthetic_population$sex==gender)
      raw_density <- densities[[age_group]][[gender]]
      prob_zero <- zeros[[age_group]][[gender]]
      v <- sample(c(0,raw_density),length(i),replace=T,prob=c(prob_zero,(rep(1,length(raw_density))-prob_zero)/length(raw_density)))
      if (length(v) > 0)
        synthetic_population$work_ltpa_marg_met[i] <- c(v)
    }
  }
  
  # Convert all int columns to numeric
  synthetic_population[, sapply(synthetic_population,class)=='integer'] <- lapply(synthetic_population[, sapply(synthetic_population,class)=='integer'], as.numeric)
  
  # remove non-travelling participants
  ##!! RJ trip_modes given in command line defines all trips that we count
  trip_set <- subset(trip_set,trip_mode%in%VEHICLE_INVENTORY$stage_mode&stage_mode%in%VEHICLE_INVENTORY$stage_mode)
  trip_set <- subset(trip_set,trip_mode!='other')
  trip_set <- drop_na(trip_set)
  
  return(list(trip_set=trip_set,synthetic_population=synthetic_population))
  
}

#' Parametrise confidence in PA data
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