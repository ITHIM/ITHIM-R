# Cities with problems:
#  Cape Town
# Bogota
# Bangalore
# Accra

# Accra
trips <- read_csv("inst/extdata/local/accra/trips_accra.csv")
names(trips)
length(unique(trips$trip_id))
# Checking the number of missing values
sapply(trips, function(x) sum(is.na(x)))
# ASk lambed where accra_trip_with_mbike.csv came from and why it has rows with trip information but not trip id

# Bangalore
trips <- read_csv("inst/extdata/local/bangalore/trips_bangalore.csv")
names(trips)
length(unique(trips$trip_id))
length(unique(trips$stage_id))
# Checking the number of missing values
sapply(trips, function(x) sum(is.na(x))) 
trips <- trips %>% mutate(stage_id_concat = paste(trip_id, stage_id, sep = "-"))
length(unique(trips$stage_id_concat))
nrow(trips)
View(trips[duplicated(trips$stage_id_concat),])
View(trips[trips$stage_id_concat == "84-1",]) # there's clearly something wrong
# stage id is not unique, although trip id is right

# Belo horizonte 
trips <- read_csv("inst/extdata/local/belo_horizonte/trips_belo_horizonte.csv")
names(trips)
length(unique(trips$trip_id))
nrow(trips)
length(unique(trips$trip_id)) == nrow(trips) #ok

# Bogota
trips <- read_csv("inst/extdata/local/bogota/trips_bogota.csv")
names(trips)
length(unique(trips$trip_id))
length(unique(trips$stage_id))
trips <- trips %>% mutate(stage_id_concat = paste(trip_id, stage_id, sep = "-"))
length(unique(trips$stage_id_concat))
nrow(trips)
View(trips[duplicated(trips$stage_id_concat),])
# Checking the number of missing values
sapply(trips, function(x) sum(is.na(x)))
# View(trips[trips$stage_id_concat == "89-1",]) # there's clearly something wrong 
# View(trips[trips$stage_id_concat == "97-2",]) # there's clearly something wrong 
# length(unique(trips$trip_id)) == nrow(trips) #ok

# Buenos aires: ok
trips <- read_csv("inst/extdata/local/buenos_aires/trips_buenos_aires.csv")
names(trips)
length(unique(trips$trip_id))
nrow(trips)
trips <- trips %>% mutate(stage_id_concat = paste(trip_id, stage_id, sep = "-"))
length(unique(trips$stage_id_concat))
nrow(trips)
length(unique(trips$stage_id_concat)) == nrow(trips) #ok

# cape town: there's one stage with duplicated id
trips <- read_csv("inst/extdata/local/cape_town/trips_cape_town.csv")
names(trips)
length(unique(trips$trip_id))
nrow(trips)
View(trips[duplicated(trips$trip_id),])
#View(trips[trips$trip_id == "7194",]) # there's clearly something wrong
length(unique(trips$trip_id)) == nrow(trips) #ok

# delhi: OK
trips <- read_csv("inst/extdata/local/delhi/trips_delhi.csv")
names(trips)
length(unique(trips$trip_id))
nrow(trips)
trips <- trips %>% mutate(stage_id_concat = paste(trip_id, stage_id, sep = "-"))
length(unique(trips$stage_id_concat))
nrow(trips)
length(unique(trips$stage_id_concat)) == nrow(trips)

# Mexico city: OK
trips <- read_csv("inst/extdata/local/mexico_city/trips_mexico_city.csv")
names(trips)
length(unique(trips$trip_id))
nrow(trips)
trips <- trips %>% mutate(stage_id_concat = paste(trip_id, stage_id, sep = "-"))
length(unique(trips$stage_id_concat))
nrow(trips)
length(unique(trips$stage_id_concat)) == nrow(trips)

# Santiago: OK
trips <- read_csv("inst/extdata/local/santiago/trips_santiago.csv")
names(trips)
length(unique(trips$trip_id))
nrow(trips)
trips <- trips %>% mutate(stage_id_concat = paste(trip_id, stage_id, sep = "-"))
length(unique(trips$stage_id_concat))
nrow(trips)
length(unique(trips$stage_id_concat)) == nrow(trips)

# Sao Paulo: OK
trips <- read_csv("inst/extdata/local/sao_paulo/trips_sao_paulo.csv")
names(trips)
length(unique(trips$trip_id))
nrow(trips)
trips <- trips %>% mutate(stage_id_concat = paste(trip_id, stage_id, sep = "-"))
length(unique(trips$stage_id_concat))
nrow(trips)
length(unique(trips$stage_id_concat)) == nrow(trips)

# Vizag: OK
trips <- read_csv("inst/extdata/local/vizag/trips_vizag.csv")
names(trips)
length(unique(trips$trip_id))
nrow(trips)
length(unique(trips$trip_id)) == nrow(trips)

