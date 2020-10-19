#####A Notes -------------------------------------------------------------------
## These codes were initiated by Rahul. Lambed is modifying the codes to generate trip datasets for the selected TIGTHAT cities. Please change work directory from J to V if you are on medschool network. 

## trip data set is processed to stage level where possible

#####Ghana####

rm(list =ls())

source("code/producing_trips_rd/used_functions.R")

# read data
time_use_0 <- read.spss("V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Accra/Accra data and microdata/Time Use Survey/Data/GTUS 2009 24 Hours Individual Diary.sav", to.data.frame = T)

#lookup
trip_mode <- data.frame(distinct(time_use_0, ActLoc2), 
                        trip_mode = c(NA, "walk","bus","taxi",  "bicycle",  "car", 
                                      "other","train","other","other","other" ))


dat <- 
    time_use_0 %>% 
    filter(region == "Greater Accra", URBRUR == "Urban") %>% 
    rename(sex = B102,
           age = B105,
           cluster_id = EANum,
           household_id = HHNum,
           participant_id = MemID,
           participant_wt = Adj_hh_wt) %>% 
    mutate(#separate start and end time,
        start = substr(Diary_hour, 1,2),
        end = substr(Diary_hour, 6,7),
        Duration = ActDur,
        Duplicate = "notDuplicate",
        Same = "notSame")

dat$ActCode1 <- ifelse(grepl("Work", dat$ActCode1), "work", ifelse(grepl("Learning",dat$ActCode1), "school", "other"))



levels(dat$ActLoc2) <- c(levels(dat$ActLoc2), "missing")
dat$ActLoc2[which(is.na(dat$ActLoc2))] <- "missing"

#for loop sums same activity
for(i in 2:nrow(dat)){
    if(dat$cluster_id[i]== dat$cluster_id[i-1] &
       dat$household_id[i] == dat$household_id[i-1] &
       dat$participant_id[i] == dat$participant_id[i-1] &
       dat$ActCode[i] == dat$ActCode[i-1] & 
       dat$ActLoc2[i] == dat$ActLoc2[i-1] &
       dat$start[i] == dat$end[i-1]){
        dat$Duration[i] <- `+`(dat$Duration[i], dat$Duration[i-1])
        dat$Duplicate[i] <- "Duplicate"
        dat$Same[i-1] <- "Same"		
    }
}

dat$ActLoc2[which(dat$ActLoc2 =="missing")] <- NA

dat <- dat %>% 
    #drop sub-activities that have been summed into one activity
    filter(!(Same == "Same")) %>% 
    #add modes
    left_join(trip_mode) 

    

trip <- 
    dat %>% 
    #filter trips only
    filter(ActLoc1 == "Travelling / Moving") %>% 
    mutate(#identify trip by row number
        trip_id = row_number(),
        trip_duration = Duration,
        trip_purpose = ActCode1)

no_trip <- dat %>% 
    anti_join(trip, by= c("cluster_id", "household_id", "participant_id")) %>% 
    mutate(trip_id = NA,
           trip_duration = NA,
           trip_purpose = NA)
#join datasets
trip <- bind_rows(trip, no_trip) %>% 
    select(cluster_id, household_id, participant_id, participant_wt, age, sex, 
           trip_id, trip_mode, trip_duration, trip_purpose) %>% 
           {.[!duplicated(.),]}

trip$trip_mode[which(!is.na(trip$trip_id) & is.na(trip$trip_mode))] <- "other"


trip$year <- 2009
trip$gdppc2014 <- 4000
trip$population2014 <- 2242000
  
write.csv(trip, "data/local/accra/accra_trip.csv")

# Reread already stored trip data
trip <- read_csv("data/local/accra/accra_trip.csv")

# Save it in a local var
rd <- trip
# Expand by household IDs

# Round participant weight
rd <- rd %>% mutate(w = if_else(is.na(participant_wt), 0, round(participant_wt)))

# Subtract 1 from non-zero entries
rd <- rd %>% mutate(w = if_else(w > 0, w - 1, w))

# Expand it according to weights, and assign IDs to the newly expanded rows
exp <- rd %>% filter(w > 0) %>% uncount(w, .id = "pid")

# Arrange df
rd <- exp %>% arrange(cluster_id, household_id, participant_id, trip_id)

# Create participant_id as a combination of cluster_id, household_id, participant_id, and pid (the newly expanded id)
rd$participant_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, pid, sep = "_"))))

# Create trip_id as a combination of cluster_id, household_id, participant_id, pid (the newly expanded id) and trip_id
rd$trip_id <- as.integer(as.factor(with(rd, paste(cluster_id, household_id, participant_id, pid, trip_id, sep = "_"))))

# Reorder and select columns
rd1 <- rd %>% dplyr::select(participant_id, age, sex, trip_id, trip_mode, trip_duration)

# Write as accra trip dataset
write_csv(rd1, 'inst/extdata/local/accra/trips_accra.csv')

#quality_check(trip)