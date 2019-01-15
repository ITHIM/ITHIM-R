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
  
  trip_set <- subset(raw_trip_set,!trip_mode%in%c("Train", "Other", "Unspecified"))
  # Make age category for trip_set dataset.
  trip_set <- assign_age_groups(trip_set,age_category=AGE_CATEGORY,age_lower_bounds=AGE_LOWER_BOUNDS,max_age=MAX_AGE)
  ##!! assuming more than one age category
  
  pa <- PA_SET
  ##!! RJ question for AA/LG: why the different age categories?
  #Make age category for pa dataset.
  age_category <- c("15-55", "56-69","70+")
  pa <- assign_age_groups(pa,age_category=age_category,age_lower_bounds=c(15,56,70))
  
  #Match persons in the trip (trip_set) e physical activity datasets.
  column_to_keep <- which(colnames(pa)%in%c('work_ltpa_marg_met'))
  unique_ages <- unique(trip_set$age_cat)
  unique_genders <- unique(trip_set$sex)
  
  # get population from trip_set: all the unique ids, and their demographic information
  # match only for "real" people (i.e. not `ghost drivers', whose id is 0)
  synthetic_population <- subset(trip_set,!duplicated(participant_id)&participant_id>0)[,names(trip_set)%in%c("participant_id","age","sex","age_cat")]
  # assign all participants 0 leisure/work mmets
  synthetic_population$work_ltpa_marg_met <- 0
  # match population to PA dataset via demographic information
  for(age_group in unique_ages){
    pa_age_category <- age_category[which(AGE_CATEGORY==age_group)]
    for(gender in unique_genders){
      matching_people <- as.data.frame(filter(pa, age_cat == pa_age_category & sex == gender)[,column_to_keep])
      i <- which(synthetic_population$age_cat==age_group&synthetic_population$sex==gender)
      v <- (matching_people[sample(nrow(matching_people),length(i),replace=T),])
      synthetic_population$work_ltpa_marg_met[i] <- c(v)
    }
  }
  
  # Convert all int columns to numeric
  synthetic_population[, sapply(synthetic_population,class)=='integer'] <- lapply(synthetic_population[, sapply(synthetic_population,class)=='integer'], as.numeric)
  # remove non-travelling participants
  trip_set <- subset(trip_set,trip_mode!=99)
  
  return(list(trip_set=trip_set,synthetic_population=synthetic_population))
  
}
