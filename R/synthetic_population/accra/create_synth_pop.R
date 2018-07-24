#Add physical activity variables to trip dataset.
#Leandro Garcia.
#5 July 2018.

#Notes:
##trip_mode = '99': persons who did not travel.
##work: job-related physical activity.
##ltpa: leisure-time physical activity.
##mpa: moderate physical activity (3 MET; 2 marginal MET).
##vpa: vigorous physical activity (6 MET; 5 marginal MET).
##duration: units are minutes per day.
##work_ltpa_marg_met: units are marginal MET-h/week.

#Load package.
library(tidyverse)

#Read datasets.
ind <- read_csv("data/synth_pop_data/accra/raw_data/trips/trips_Accra.csv")
pa <- read_csv("data/synth_pop_data/accra/raw_data/PA/pa_Accra.csv")

# Convert character to int
ind$participant_id <- as.integer(as.factor(ind$participant_id))

# Multiply ind by 4 to have a bigger number of trips (and ind)
ind1 <- ind
ind1$participant_id <- ind1$participant_id + max(ind$participant_id)

ind2 <- ind1
ind2$participant_id <- ind2$participant_id + max(ind1$participant_id)

ind3 <- ind2
ind3$participant_id <- ind3$participant_id + max(ind2$participant_id)

ind <- rbind(ind, ind1)
ind <- rbind(ind, ind2)
ind <- rbind(ind, ind3)


#Set seed.
set.seed(1)

#Make age category for ind dataset.
ind <- filter(ind, age < 70)
age_category <- c("15-49", "50-69")
ind$age_cat[ind$age >= 15 & ind$age < 50] <- age_category[1]
ind$age_cat[ind$age >= 50 & ind$age < 70] <- age_category[2]

#Make participant ID numeric.
ind$participant_id <- as.numeric(ind$participant_id)

#Make age category for pa dataset.
pa <- filter(pa, age < 70)
age_category <- c("15-55", "56-69")
pa$age_cat[pa$age >= 15 & pa$age <= 55] <- age_category[1]
pa$age_cat[pa$age > 55 & pa$age < 70] <- age_category[2]

#Match persons in the trip (ind) e physical activity datasets.
temp <- matrix(nrow = 0, ncol = 10, byrow = T)

for (i in unique(ind$participant_id)){
  rage <- ind %>% filter(participant_id == i) %>% summarise(first(age_cat))
  rage <- rage[[1]]
  
  rsex <- ind %>% filter(participant_id == i) %>% summarise(first(sex))
  rsex <- rsex[[1]]
  
  v <- NA
  
  if (rage == "15-49") {
    v <- filter(pa, age_cat == "15-55" & sex == rsex) %>%
      select(work_mpa_duration : ltpa_vpa_days, work_ltpa_marg_met) %>%
      sample_n(1) %>% as.double()
  }
  else {
    v <- filter(pa, age_cat == "56-69" & sex == rsex) %>%
      select(work_mpa_duration : ltpa_vpa_days, work_ltpa_marg_met) %>%
      sample_n(1) %>% as.double()
  }

  v <- matrix(c(v, i), ncol = 10, byrow = T)
  temp <- rbind(temp, v)
}

namevector <- c(colnames(pa[, c(4:11, 20)]), "participant_id")
colnames(temp) <- namevector
temp <- as.data.frame (temp)

ind <- left_join(ind, temp, "participant_id")

#Save csv.
write_csv(ind, "data/synth_pop_data/accra/travel_survey/synthetic_population_with_trips.csv")

##END OF CODE##