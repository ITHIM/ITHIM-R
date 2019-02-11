# Read library
require(tidyverse)

#Read raw sp injuries
rd <- read_csv("inst/extdata/local/sao_paulo/sao_paulo_processed_2009_2013.csv")

# Convert to the required structure
# Make sure that we have the all required variables
# Make sure modes are standardized
# Rob can you have a look please?

# 1. extract year
rd$year <- as.numeric(sapply(rd$accref,function(x)paste0('20',substr(x,4,5))))

# 2. rename columns
names(rd)[3:5] <- c('cas_mode','cas_gender','cas_age')

# 3. count NA
## casualty modes not modelled: others, pickup, minibus, cart. If we add this processing to ITHIM-R, we can add on missing fatalities as a constant?
nrow(subset(rd,cas_mode%in%c('others', 'pickup', 'minibus', 'cart')|is.na(cas_mode)|is.na(cas_gender)|is.na(cas_age))) # total missing: 130/5948=2.2%
nrow(subset(rd,cas_mode%in%c('others', 'pickup', 'minibus', 'cart')|is.na(cas_mode))) # total missing: 54/5948=0.9%
## might we want to capture minibus travel within bus travel?
## is pickup van travel?

# 4. rename entries to match input modes
rd$cas_mode[rd$cas_mode=='pedestrian'] <- 'Pedestrian'
rd$cas_mode[rd$cas_mode=='car'] <- 'Car'
rd$cas_mode[rd$cas_mode=='motorcycle'] <- 'Motorcycle'
rd$cas_mode[rd$cas_mode=='bicycle'] <- 'Bicycle'
rd$cas_mode[rd$cas_mode=='truck'] <- 'Truck'
rd$cas_mode[rd$cas_mode=='bus'] <- 'Bus'

rd$strike_mode[rd$strike_mode=='bus'] <- 'Bus_driver'
rd$strike_mode[rd$strike_mode=='motorcycle'] <- 'Motorcycle'
rd$strike_mode[rd$strike_mode=='car/taxi'] <- 'Car'
rd$strike_mode[rd$strike_mode=='heavy goods'] <- 'Truck'
rd$strike_mode[rd$strike_mode=='cyclist'] <- 'Bicycle'

rd$cas_gender[rd$cas_gender=='yes'] <- 'Male'
rd$cas_gender[rd$cas_gender=='no'] <- 'Female'


# 5. sample with replacement to replace NA for casualty age and casualty gender
rd$cas_gender[is.na(rd$cas_gender)] <- sample(rd$cas_gender[!is.na(rd$cas_gender)],sum(is.na(rd$cas_gender)),replace=T)
rd$cas_age[is.na(rd$cas_age)] <- sample(rd$cas_age[!is.na(rd$cas_age)],sum(is.na(rd$cas_age)),replace=T)

# 6. save
write.csv(rd,"inst/extdata/local/sao_paulo/injuries_sao_paulo.csv")
