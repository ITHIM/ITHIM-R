create_synth_pop <- function(raw_trip_set){
  #Add physical activity variables to trip dataset.
  #Leandro Garcia & Ali Abbas.
  #5 July 2018.
  
  # Last Updated by Ali Abbas
  # Added 32 new motorcyle trips 
  # Multiplied baseline dataset by 4
  
  #Notes:
  ##trip_mode = '99': persons who did not travel.
  ##work: job-related physical activity.
  ##ltpa: leisure-time physical activity.
  ##mpa: moderate physical activity (3 MET; 2 marginal MET).
  ##vpa: vigorous physical activity (6 MET; 5 marginal MET).
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
  
  synthetic_population <- subset(trip_set,!duplicated(participant_id)&participant_id>0)[,names(trip_set)%in%c("participant_id","age","sex","age_cat")]
  synthetic_population$work_ltpa_marg_met <- 0
  ##synth match only for "real" people 
  temp <- c()
  for(age_group in unique_ages){
    for(gender in unique_genders){
      i <- unique(subset(trip_set,age_cat==age_group&sex==gender&participant_id>0)$participant_id)
      pa_age_category <- age_category[which(AGE_CATEGORY==age_group)]
      matching_people <- as.data.frame(filter(pa, age_cat == pa_age_category & sex == gender)[,column_to_keep])
      v <- (matching_people[sample(nrow(matching_people),length(i),replace=T),])
      temp <- rbind( temp, cbind(v,i) )
      i <- which(synthetic_population$age_cat==age_group&synthetic_population$sex==gender)
      synthetic_population$work_ltpa_marg_met[i] <- c(v)
    }
  }
  
  namevector <- c(colnames(pa)[column_to_keep], "participant_id")
  colnames(temp) <- namevector
  temp <- as.data.frame (temp)
  
  #trip_and_pa_set <- left_join(trip_set, temp, "participant_id")
  
  # Convert all int columns to numeric
  #trip_and_pa_set[, sapply(trip_and_pa_set,class)=='integer'] <- lapply(trip_and_pa_set[, sapply(trip_and_pa_set,class)=='integer'], as.numeric)
  synthetic_population[, sapply(synthetic_population,class)=='integer'] <- lapply(synthetic_population[, sapply(synthetic_population,class)=='integer'], as.numeric)
  trip_set <- subset(trip_set,trip_mode!=99)
  
  #trip_and_pa_set$trip_id[trip_and_pa_set$trip_mode == '99'] <- 0
  
  list(trip_set=trip_set,synthetic_population=synthetic_population)
  
}