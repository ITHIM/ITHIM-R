# Clear workspace
rm (list = ls())


require(ithimr)
#require(tidyverse)

io <- run_ithim_setup(CITY = 'bangalore',
                      TEST_WALK_SCENARIO = T,
                      ADD_WALK_TO_BUS_TRIPS = F,
                      ADD_TRUCK_DRIVERS = F,
                      ADD_BUS_DRIVERS = F,
                      PM_TRANS_SHARE = 0.281,
                      PM_CONC_BASE = 47.4,
                      speeds = list(subway = 32,
                                    bicycle = 15),
                      emission_inventory = list(motorcycle = 1409,
                                                auto_rickshaw = 133,
                                                car = 2214,
                                                bus_driver = 644,
                                                big_truck = 4624,
                                                truck = 3337,
                                                van = 0,
                                                other = 0,
                                                taxi = 0))

io$outcomes <- run_ithim(io, seed = 1)


## plot results for YLLs
result_df <- data.frame(variable = names(io$outcome$hb$ylls[,3:ncol(io$outcome$hb$ylls)]),
                              value = colSums(io$outcome$hb$ylls[,3:ncol(io$outcome$hb$ylls)]))

result_df$variable <- sapply(strsplit(as.character(result_df$variable), "_"), tail, 1)

ggplot(data = result_df) +
  aes(x = variable, y = value) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()


plotly::ggplotly(ggplot(data = reshape2::melt(io$outcomes$mmets %>% dplyr::select(base_mmet, scen1_mmet) %>% rename(baseline = base_mmet, scenario = scen1_mmet) )) +
  aes(x = variable, y = value) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
)



################################################################
ithim_object <- run_ithim_setup(CITY = 'bangalore',
                      NSAMPLES=1024,
                      seed=4,
                      MAX_MODE_SHARE_SCENARIO = T,
                      PROPENSITY_TO_TRAVEL = T,
                      ADD_WALK_TO_BUS_TRIPS = T,
                      MMET_CYCLING = c(log(4.63),log(1.2)), 
                      MMET_WALKING = c(log(2.53),log(1.2)), 
                      DAY_TO_WEEK_TRAVEL_SCALAR = 7,
                      
                      INJURY_LINEARITY= c(log(1),log(1.05)),
                      CASUALTY_EXPONENT_FRACTION =  c(15,15),
                      
                      PA_DOSE_RESPONSE_QUANTILE = T,  
                      AP_DOSE_RESPONSE_QUANTILE = T,
                      
                      INJURY_REPORTING_RATE = c(50,3),  
                      CHRONIC_DISEASE_SCALAR = c(0,log(1.2)),  
                      PM_CONC_BASE = c(log(47),log(1.17)),  
                      PM_TRANS_SHARE = c(6.5,17),  
                      BACKGROUND_PA_SCALAR = c(0,log(1.2)),
                      BACKGROUND_PA_CONFIDENCE = 0.3,
                      BUS_WALK_TIME = c(log(5),log(1.2)),
                      MOTORCYCLE_TO_CAR_RATIO = 0,
                      BUS_TO_PASSENGER_RATIO = c(20,600),
                      TRUCK_TO_CAR_RATIO = c(3,10),
                      EMISSION_INVENTORY_CONFIDENCE = 0.9,
                      DISTANCE_SCALAR_CAR_TAXI = c(0,log(1.2)),
                      DISTANCE_SCALAR_WALKING = c(0,log(1.2)),
                      DISTANCE_SCALAR_PT = c(0,log(1.2)),
                      DISTANCE_SCALAR_CYCLING = c(0,log(1.2)),
                      DISTANCE_SCALAR_MOTORCYCLE = c(0,log(1.2)),
                      speeds = list(subway = 32,
                                    bicycle = 15),
                      emission_inventory = list(motorcycle = 1409,
                                                auto_rickshaw = 133,
                                                car = 2214,
                                                bus_driver = 644,
                                                big_truck = 4624,
                                                truck = 3337,
                                                van = 0,
                                                other = 0,
                                                taxi = 0))
numcores <- detectCores()
ithim_object$outcomes <- NULL
ithim_object$outcomes <- mclapply(1:NSAMPLES,FUN=run_ithim,ithim_object=ithim_object,mc.cores = numcores)
for(i in 1:length(ithim_object$outcomes))if(length(ithim_object$outcomes[[i]])<2) print(i)
outcomes <- list()
for(i in 1:NSAMPLES) outcomes[[i]] <- run_ithim(seed=i,ithim_object)
