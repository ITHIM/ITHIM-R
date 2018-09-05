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

#Load package.
library(tidyverse)

#Read datasets.
ind <- read_csv("data/synth_pop_data/accra/raw_data/trips/trips_Accra.csv")

# Convert character to int
ind$participant_id <- as.numeric(as.factor(ind$participant_id))

# Create new motorbike trips
# Add 4 new people with 3 trips each
# Age: 15-59 and gender: male

n <- 4
nt <- 3

new_trips <- data.frame(trip_id = c( (max(ind$trip_id) + 1):(max(ind$trip_id) + (n * nt) )), 
                        trip_mode = 'Motorcycle', 
                        trip_duration = round(runif( (n * nt), 15, 100)), 
                        participant_id = rep((max(ind$trip_id)+1):(max(ind$trip_id) + n), nt),
                        age = rep(round(runif(n, 15, 49)), nt),
                        sex = 'Male')

# Add new motorbikes trips to baseline
ind <- rbind(ind, new_trips)


# Redefine motorcycle mode for a select 14 rows
td <- ind %>% filter(trip_mode == 'Other' & trip_duration < 60) %>% 
  sample_n(14) %>% mutate(trip_mode = 'Motorcycle') %>% 
  select(trip_id, trip_mode)

ind <- ind %>% mutate(trip_mode = ifelse(trip_id %in% td$trip_id, td$trip_mode, trip_mode))
                      
pa <- read_csv("data/synth_pop_data/accra/raw_data/PA/pa_Accra.csv")


# Multiply ind by 4 to have a bigger number of trips (and ind)
ind1 <- ind
ind1$participant_id <- ind1$participant_id + max(ind$participant_id)
ind1$trip_id <- (max(ind$trip_id) + 1): (max(ind$trip_id) + nrow(ind1))

ind2 <- ind
ind2$participant_id <- ind2$participant_id + max(ind1$participant_id)
ind2$trip_id <- (max(ind1$trip_id) + 1): (max(ind1$trip_id) + nrow(ind2))

ind3 <- ind
ind3$participant_id <- ind3$participant_id + max(ind2$participant_id)
ind3$trip_id <- (max(ind2$trip_id) + 1): (max(ind2$trip_id)  + nrow(ind3))

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

# Convert all int columns to numeric
ind[, c(1, 3, 5)] <- lapply(ind[, c(1, 3, 5)], as.numeric)

ind[ind$trip_mode == '99',]$trip_id <- 0

#Save csv.
write.csv(ind, "data/synth_pop_data/accra/travel_survey/synthetic_population_with_trips.csv", row.names = F)

##END OF CODE##