# Remove everything
rm (list = ls())
# Load packages
library(tidyverse)
library(haven)
library(plotly)


#Set seed
set.seed(1)

# Load all functions
source("code/PA/code/functions.R")

# Sample size for scenario
sample_size <- 110

# Constants
# Initialize  energy expenditure constants - taken from ICT
METCycling <- 5.63
METWalking <- 3.53
METEbikes <- 4.50

# Read combined individual travel survey and Physical Activity data
raw_data <- haven::read_dta("data/synth_pop_data/england/SPtrip_CensusNTSAPS_E06000001.dta")

# Create id columns for all trips (and people without any trips)
raw_data$id <- 1:nrow(raw_data)

# Create a lookup table for selected columns - labelled columns from stata
lt <- create.lookups(raw_data, c("female", "agecat", "agecat_det", "trip_mainmode"))

# Sort it
lt <- arrange(lt, names, id)


# Sample 10k unique IDs
# Select trips for the 10k people
baseline <- raw_data %>% filter(census_id %in% sample(unique(census_id), 1000)) 

lbls <- as.character(unique(as_factor(baseline$trip_mainmode, labels = "values")))

ggplot(baseline, aes (x = as_factor(trip_mainmode, labels = "values"))) + geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Remove labels
baseline <- clear.labels(baseline)
# Replace NAs with 0
baseline$trip_cycletime_min <- ifelse(is.na(baseline$trip_cycletime_min), 0, baseline$trip_cycletime_min)
# Replace NAs with 0
baseline$trip_walktime_min <- ifelse(is.na(baseline$trip_walktime_min), 0, baseline$trip_walktime_min)
# Calculate trip_cycletime_hr by dividing trip_cycletime_min by 60
baseline$trip_cycletime_hr <- baseline$trip_cycletime_min / 60
# Calculate trip_walktime_hr by dividing trip_walktime_min by 60
baseline$trip_walktime_hr <- baseline$trip_walktime_min / 60

# Get total individual level walking and cycling and sport mmets 
individual_mmet <- baseline %>% group_by(census_id) %>% summarise (female = first(female), 
                                                                   agecat_det = first(agecat_det), 
                                                                   cycleNTS_wkhr = sum(trip_cycletime_hr),
                                                                   walkNTS_wkhr = sum(trip_walktime_hr),
                                                                   sport_wkmmets = first(sport_wkmmets))


# Sum all mmets for individuals
individual_mmet$total_mmet <- ((METCycling - 1) * individual_mmet$cycleNTS_wkhr) + 
                          ((METWalking - 1) * individual_mmet$walkNTS_wkhr) + individual_mmet$sport_wkmmets

# Replace NAs with 0
individual_mmet$total_mmet <- ifelse(is.na(individual_mmet$total_mmet), 0, individual_mmet$total_mmet)

# Create a summary of mmets by gender and age
b_mmet <- individual_mmet  %>% group_by(female, agecat_det) %>% summarise(mean = mean(total_mmet))

# Define a scenario
# Switch modes
# Create cycling specifics distances

# Sample 100 unique individual IDs
# Select non-cycling trips for the 10k people
sc <- baseline %>% filter(trip_mainmode != (filter(lt, names == 'trip_mainmode' & val == 'Bicycle') %>% distinct(id) %>% as.integer()) & census_id %in% sample(unique(census_id), sample_size))
# Convert those trips to cycling
sc$trip_mainmode <- filter(lt, names == 'trip_mainmode' & val == 'Bicycle') %>% distinct(id) %>% as.integer()
# Create trip_cycletime_hr by dividing trip_cycletime_min by 60
sc$trip_cycletime_hr <- sc$trip_durationraw_min / 60
# Create a copy of baseline var
nbaseline <- baseline
# Update selected rows by sc
nbaseline[nbaseline$id %in% sc$id, ] <- sc

# Sum all mmets for individuals
individual_mmet_sc <- nbaseline %>% group_by(census_id) %>% summarise (female = first(female), 
                                                                   agecat_det = first(agecat_det), 
                                                                   cycleNTS_wkhr = sum(trip_cycletime_hr),
                                                                   walkNTS_wkhr = sum(trip_walktime_hr),
                                                                   sport_wkmmets = first(sport_wkmmets))


ggplot(nbaseline, aes (x = trip_mainmode)) + geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Calculate individual sum of mmets for the scenario
individual_mmet_sc$total_mmet <- ((METCycling - 1) * individual_mmet_sc$cycleNTS_wkhr) + 
  ((METWalking - 1) * individual_mmet_sc$walkNTS_wkhr) + individual_mmet_sc$sport_wkmmets

# Replace NAs with 0
individual_mmet_sc$total_mmet <- ifelse(is.na(individual_mmet_sc$total_mmet), 0, individual_mmet_sc$total_mmet)
# Create a summary of mmets by gender and age
sc_mmet <- individual_mmet_sc  %>% group_by(female, agecat_det) %>% summarise(mean = mean(total_mmet))

# Read DR data
mmet2RR_mat <- read.csv("data/dose_response/PA/all_cause/MA-all-cause-mortality-RRs.csv", header = T, as.is = T)

# Append scenario's mmet to baseline's mmet
individual_mmet <- dplyr::left_join(individual_mmet, by = "census_id", individual_mmet_sc %>% mutate (total_mmet_sc = total_mmet) %>% select(census_id, total_mmet_sc))

# Convert mmet into rr for all mmets columns
rr <- mmet2RR(individual_mmet, c('total_mmet', 'total_mmet_sc'))

# Calculate pif calculations (pif between baseline and scenario's RR)
pif <- PAF(pop = rr, attr = c('female', 'agecat_det'), cn = c('total_mmet', 'total_mmet_sc'))
pif <- data.frame(pif)
pif <- arrange(pif, age.band, gender)

# Create a agecat_det lookup table
lt_age <- filter(lt, names == "agecat_det")

# Convert factors of age and sex into full form
pif$age.band <- lt_age$val[match(pif$age.band, lt_age$id)]

# Create string gender column
pif$gender <- ifelse(pif$gender == 1, "Female", "Male")

#pif$age.band[pif$age.band == "10 to 15"] <- "15 to 19"
pif$age.band[pif$age.band == "16 to 19"] <- "15 to 19"

# Read gbd data
gbd_data <- read_csv("data/demographics/gbd/england/IHME_GBD_2016_DATA.csv")

# No need to arrange
# gbd_data <- arrange(gbd_data, measure, age, sex)

# Get both ylls and yll_red together
yll_dfs <- combine_health_and_pif(pop = pif, hc = gbd_data, hm = "YLLs (Years of Life Lost)", cn = c("total_mmet", "total_mmet_sc"))
# Subset to get yll
yll <- as.data.frame(yll_dfs[1])
# Subset to get yll_reductions
yll_red <- as.data.frame(yll_dfs[2])

# Plot yll_red
ggsave(filename = paste0("sc", sample_size, ".png"), 
       
       
       ggplot(yll_red, aes(x = age.band, y = total_mmet_sc, fill = gender)) + 
           geom_col(position = "dodge") + 
           xlab("\nAge Groups\n") +
           ylab("\nYLL Reduction (%) \n") +
           labs(title = paste("YLL Reductions for scenario - ", sample_size)) +
           scale_y_continuous(limits = c(0, 0.03)),
       width = 10, height = 4, dpi = 300, units = "in", device='png'
)


# Get both deaths and in reduction in deaths
death_dfs <- combine_health_and_pif(pop = pif, hc = gbd_data, hm = "Deaths", cn = c("total_mmet", "total_mmet_sc"))
# Subset to get deaths
death <- as.data.frame(death_dfs[1])
# Subset to get reduciton in deaths
death_red <- as.data.frame(death_dfs[2])