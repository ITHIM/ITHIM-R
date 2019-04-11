library(ithimr)
rm(list=ls())
cities <- c('accra','sao_paulo','delhi','bangalore')
emission_inventories = list(accra=NULL,
                            sao_paulo=list(motorcycle=4,
                                           car=4,
                                           bus_driver=32,
                                           big_truck=56,
                                           truck=4,
                                           van=0,
                                           other=0,
                                           taxi=0),
                            delhi=list(motorcycle=1409,
                                       auto_rickshaw=133,
                                       car=2214,
                                       bus_driver=644,
                                       big_truck=4624,
                                       truck=3337,
                                       van=0,
                                       other=0,
                                       taxi=0),
                            bangalore=list(motorcycle=1757,
                                           auto_rickshaw=220,
                                           car=4173,
                                           bus_driver=1255,
                                           big_truck=4455,
                                           truck=703,
                                           van=0,
                                           other=0,
                                           taxi=0))
#################################################################
## run diagnostics
for(city in cities){
  ithim_object <- run_ithim_setup(ADD_TRUCK_DRIVERS = F,ADD_BUS_DRIVERS = F,CITY=city,MAX_MODE_SHARE_SCENARIO=T,DIST_CAT = c("0-1 km", "2-5 km", "6+ km"),emission_inventory = emission_inventories[[city]])
  summarise_ithim_inputs(ithim_object)
}


##################################################################
speeds <- list(accra=NULL,
               sao_paulo=NULL,
               delhi=list(subway=32,
                          bicycle=15),
               bangalore=list(subway=32,
                          bicycle=15))

# beta parameters for INJURY_REPORTING_RATE
injury_report_rate <- list(accra=1,
                           sao_paulo=1,
                           delhi=1,
                           bangalore=1)
# lnorm parameters for CHRONIC_DISEASE_SCALAR
chronic_disease_scalar <- list(accra=1,
                               sao_paulo=1,
                               delhi=1,
                               bangalore=1)
# lnorm parameters for PM_CONC_BASE
pm_concentration <- list(accra=50,
                         sao_paulo=18,
                         delhi=122,
                         bangalore=47.4)
# beta parameters for PM_TRANS_SHARE
pm_trans_share <- list(accra=0.225,
                       sao_paulo=0.4,
                       delhi=0.225,
                       bangalore=0.281)
# lnorm parameters for BACKGROUND_PA_SCALAR
background_pa_scalar <- list(accra=1,
                             sao_paulo=1,
                             delhi=1,
                             bangalore=1)
# lnorm parameters for BUS_WALK_TIME
bus_walk_time <- list(accra=5,
                      sao_paulo=5,
                      delhi=5,
                      bangalore=5)
# lnorm parameters for MMET_CYCLING
mmet_cycling <- 4.63
# lnorm parameters for MMET_WALKING
mmet_walking <- 2.53
# lnorm parameters for MOTORCYCLE_TO_CAR_RATIO
mc_car_ratio <- list(accra=0.2,
                     sao_paulo=0,
                     delhi=0,
                     bangalore=0)
# beta parameters for DAY_TO_WEEK_TRAVEL_SCALAR
day_to_week_scalar <- 7
# lnorm parameters for INJURY_LINEARITY
injury_linearity <- 1
# beta parameters for CASUALTY_EXPONENT_FRACTION
cas_exponent <- 0.5
# beta parameters for DAY_TO_WEEK_TRAVEL_SCALAR
day_to_week_scalar <- 7

#################################################
## without uncertainty
toplot <- matrix(0,nrow=5,ncol=length(cities)) #5 scenarios, 4 cities
ithim_objects <- list()
min_age <- 15
max_age <- 69
for(city in cities){
  ithim_objects[[city]] <- run_ithim_setup(DIST_CAT = c("0-1 km", "2-5 km", "6+ km"),
                                  ADD_WALK_TO_BUS_TRIPS=F,
                                  CITY=city,ADD_TRUCK_DRIVERS = F,
                                  MAX_MODE_SHARE_SCENARIO = T,
                                  ADD_BUS_DRIVERS = F,
                                  emission_inventory = emission_inventories[[city]],
                                  speeds = speeds[[city]],
                                  
                                  MMET_CYCLING = mmet_cycling, 
                                  MMET_WALKING = mmet_walking, 
                                  DAY_TO_WEEK_TRAVEL_SCALAR = day_to_week_scalar,
                                  INJURY_LINEARITY= injury_linearity,
                                  CASUALTY_EXPONENT_FRACTION = cas_exponent,
                                  PA_DOSE_RESPONSE_QUANTILE = F,  
                                  AP_DOSE_RESPONSE_QUANTILE = F,
                                  INJURY_REPORTING_RATE = injury_report_rate[[city]],  
                                  CHRONIC_DISEASE_SCALAR = chronic_disease_scalar[[city]],  
                                  PM_CONC_BASE = pm_concentration[[city]],  
                                  PM_TRANS_SHARE = pm_trans_share[[city]],  
                                  BACKGROUND_PA_SCALAR = background_pa_scalar[[city]],
                                  BUS_WALK_TIME = bus_walk_time[[city]],
                                  MOTORCYCLE_TO_CAR_RATIO = mc_car_ratio[[city]])
  ithim_objects$scen_prop <- SCENARIO_PROPORTIONS
  ithim_objects[[city]]$outcomes <- run_ithim(ithim_objects[[city]], seed = 1)
  ithim_objects[[city]]$synth_pop <- SYNTHETIC_POPULATION
  ithim_objects[[city]]$demographic <- DEMOGRAPHIC
  ithim_objects[[city]]$disease_burden <- DISEASE_BURDEN
  ithim_objects[[city]]$emission_inventory <- EMISSION_INVENTORY
  ithim_objects[[city]]$injury_table <- INJURY_TABLE
  
  ## store results to plot
  min_ages <- sapply(ithim_objects[[city]]$outcome$hb$ylls$age_cat,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
  max_ages <- sapply(ithim_objects[[city]]$outcome$hb$ylls$age_cat,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
  sub_outcome <- subset(ithim_objects[[city]]$outcome$hb$ylls,min_ages>=min_age&max_ages<=max_age)
  result_mat <- colSums(sub_outcome[,3:ncol(sub_outcome)])
  columns <- length(result_mat)
  nDiseases <- columns/NSCEN
  ylim <- range(result_mat)
  if(city==cities[1]){
    disease_list <- list()
    for(i in 1:nDiseases) disease_list[[i]] <- toplot
  }
  min_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
  max_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
  for(i in 1:nDiseases)
    disease_list[[i]][,which(cities==city)] <- result_mat[1:NSCEN + (i - 1) * NSCEN]/sum(subset(DEMOGRAPHIC,min_pop_ages>=min_age&max_pop_ages<=max_age)$population)
}

{x11(width = 10, height = 5);
  layout.matrix <- matrix(c(2:6,1,7:12), nrow =2, ncol =6,byrow=T)
  graphics::layout(mat = layout.matrix,heights = c(2,3),widths = c(2.8,2,2,2,2,2.5))
  cols <- c('navyblue','hotpink','grey','darkorange')
  mar1 <- rep(7,nDiseases); mar1[1:6] <- 1
  mar2 <- rep(1,nDiseases); mar2[c(2,7)] <- 6; mar2[c(1,12)] <- 3
  for(i in 1:nDiseases){
    ylim <- if(i==12) c(-0.25,0.02)*1 else if(i==1) c(-1.7,2)*1e-3 else c(-11,4)*1e-4
    par(mar = c(mar1[i], mar2[i], 4, 1))
      barplot(t(disease_list[[i]]), ylim = ylim, las = 2,beside=T,col=cols, names.arg=if(i<7) NULL else  rownames(SCENARIO_PROPORTIONS), 
              main = paste0(last(strsplit(names(result_mat)[i * NSCEN], '_')[[1]])),yaxt='n')
    if(i%in%c(2,1,7,12)) {axis(2,cex.axis=1.5); if(i%in%c(2,7)) mtext(side=2,'YLL gain per person',line=3)}
    if(i==nDiseases-1) legend(legend=cities,fill=cols,bty='n',y=-1e-5,x=5)
  }
}

## Save the ithim_object in the results folder
##########

saveRDS(ithim_objects, "C:/RStudio Projects/ITHIM-R/results/multi_city/io.rds")

#################################################
## with uncertainty
## comparison across cities
numcores <- detectCores()
nsamples <- 1024
setting_parameters <- c("BUS_WALK_TIME","PM_CONC_BASE","MOTORCYCLE_TO_CAR_RATIO","BACKGROUND_PA_SCALAR","BACKGROUND_PA_ZEROS","EMISSION_INVENTORY",                        
                        "CHRONIC_DISEASE_SCALAR","PM_TRANS_SHARE","INJURY_REPORTING_RATE","BUS_TO_PASSENGER_RATIO","TRUCK_TO_CAR_RATIO",
                        "DISTANCE_SCALAR_CAR_TAXI",
                        "DISTANCE_SCALAR_WALKING",
                        "DISTANCE_SCALAR_PT",
                        "DISTANCE_SCALAR_CYCLING",
                        "DISTANCE_SCALAR_MOTORCYCLE")


# beta parameters for INJURY_REPORTING_RATE
injury_reporting_rate <- list(accra=c(8,3),
                           sao_paulo=c(50,3),
                           delhi=c(50,3),
                           bangalore=c(50,3))
# lnorm parameters for CHRONIC_DISEASE_SCALAR
chronic_disease_scalar <- list(accra=c(0,log(1.2)),
                               sao_paulo=c(0,log(1.2)),
                               delhi=c(0,log(1.2)),
                               bangalore=c(0,log(1.2)))
# lnorm parameters for PM_CONC_BASE
pm_conc_base <- list(accra=c(log(50),log(1.3)),
                               sao_paulo=c(log(20),log(1.3)),
                         delhi=c(log(122),log(1.3)),
                         bangalore=c(log(47),log(1.17))) ## mean=47.4, sd=7.5
# beta parameters for PM_TRANS_SHARE
pm_trans_share <- list(accra=c(5,20),
                           sao_paulo=c(8,8),
                       delhi=c(4,4),
                       bangalore=c(6.5,17)) ## mean 0.281, sd 0.089
# lnorm parameters for BACKGROUND_PA_SCALAR
background_pa_scalar <- list(accra=c(0,log(1.2)),
                               sao_paulo=c(0,log(1.2)),
                             delhi=c(0,log(1.2)),
                             bangalore=c(0,log(1.2)))
# values between 0 and 1 for BACKGROUND_PA_CONFIDENCE
background_pa_confidence <- list(accra=0.5,
                                 sao_paulo=0.7,
                                 delhi=0.3,
                                 bangalore=0.3)
# lnorm parameters for BUS_WALK_TIME
bus_walk_time <- list(accra=c(log(5),log(1.2)),
                     sao_paulo=0,
                     delhi=0,
                     bangalore=c(log(5),log(1.2)))
# lnorm parameters for MMET_CYCLING
mmet_cycling <- c(log(4.63),log(1.2))
# lnorm parameters for MMET_WALKING
mmet_walking <- c(log(2.53),log(1.2))
# lnorm parameters for MOTORCYCLE_TO_CAR_RATIO
motorcycle_to_car_ratio <- list(accra=c(-1.4,0.4),
                       sao_paulo=0,
                     delhi=0,
                     bangalore=0)
# lnorm parameters for INJURY_LINEARITY
injury_linearity <- c(log(1),log(1.05))
# beta parameters for CASUALTY_EXPONENT_FRACTION
casualty_exponent_fraction <- c(15,15)
# logical for PA dose response: set T for city 1, and reuse values in 2 and 3; no need to recompute
pa_dr_quantile <- c(T,F,F,F)
# logical for AP dose response: set T for city 1, and reuse values in 2 and 3; no need to recompute
ap_dr_quantile <- c(T,F,F,F)
# logical for walk scenario
test_walk_scenario <- F
# logical for cycle scenario
test_cycle_scenario <- F
# if walk scenario, choose Baseline as reference scenario
ref_scenarios <- list(accra='Baseline',
                      sao_paulo='Baseline',
                      delhi='Baseline',
                      bangalore='Baseline')
# whether or not to add walk trips to bus trips
add_walk_to_bus_trips <- c(T,F,F,T)
# bus occupancy beta distribution
bus_to_passenger_ratio  <- list(accra=c(20,600),
                                sao_paulo=c(20,600),
                                delhi=c(20,600),
                                bangalore=c(20,600))
# truck beta distribution
truck_to_car_ratio  <- list(accra=c(3,10),
                            sao_paulo=c(3,10),
                            delhi=c(3,10),
                            bangalore=c(3,10))
# emission confidences
emission_confidence  <- list(accra=0.5,
                            sao_paulo=0.7,
                            delhi=0.9,
                            bangalore=0.9)
# lnorm parameters for DISTANCE_SCALAR_CAR_TAXI
distance_scalar_car_taxi <- list(accra=c(0,log(1.2)),
                                 sao_paulo=c(0,log(1.2)),
                                 delhi=c(0,log(1.2)),
                                 bangalore=c(0,log(1.2)))
# lnorm parameters for DISTANCE_SCALAR_MOTORCYCLE
distance_scalar_motorcycle <- list(accra=c(0,log(1.2)),
                                 sao_paulo=c(0,log(1.2)),
                                 delhi=c(0,log(1.2)),
                                 bangalore=c(0,log(1.2)))
# lnorm parameters for DISTANCE_SCALAR_PT
distance_scalar_pt <- list(accra=c(0,log(1.2)),
                                 sao_paulo=c(0,log(1.2)),
                                 delhi=c(0,log(1.2)),
                                 bangalore=c(0,log(1.2)))
# lnorm parameters for DISTANCE_SCALAR_WALKING
distance_scalar_walking <- list(accra=c(0,log(1.2)),
                                 sao_paulo=c(0,log(1.2)),
                                 delhi=c(0,log(1.2)),
                                 bangalore=c(0,log(1.2)))
# lnorm parameters for DISTANCE_SCALAR_CYCLING
distance_scalar_cycling <- list(accra=c(0,log(1.2)),
                                 sao_paulo=c(0,log(1.2)),
                                 delhi=c(0,log(1.2)),
                                 bangalore=c(0,log(1.2)))

betaVariables <- c("PM_TRANS_SHARE",
                   "INJURY_REPORTING_RATE",
                   "CASUALTY_EXPONENT_FRACTION",
                   "BUS_TO_PASSENGER_RATIO",
                   "TRUCK_TO_CAR_RATIO")
normVariables <- c("BUS_WALK_TIME",
                   "MMET_CYCLING",
                   "MMET_WALKING",
                   "PM_CONC_BASE",
                   "MOTORCYCLE_TO_CAR_RATIO",
                   "BACKGROUND_PA_SCALAR",
                   "CHRONIC_DISEASE_SCALAR",
                   "INJURY_LINEARITY",
                   "DISTANCE_SCALAR_CAR_TAXI",
                   "DISTANCE_SCALAR_WALKING",
                   "DISTANCE_SCALAR_PT",
                   "DISTANCE_SCALAR_CYCLING",
                   "DISTANCE_SCALAR_MOTORCYCLE")

save(cities,setting_parameters,injury_reporting_rate,chronic_disease_scalar,pm_conc_base,pm_trans_share,
          background_pa_scalar,background_pa_confidence,bus_walk_time,mmet_cycling,mmet_walking,emission_inventories,
          motorcycle_to_car_ratio,injury_linearity,casualty_exponent_fraction,pa_dr_quantile,ap_dr_quantile,
          bus_to_passenger_ratio,truck_to_car_ratio,emission_confidence,distance_scalar_car_taxi,distance_scalar_motorcycle,
          distance_scalar_pt,distance_scalar_walking,distance_scalar_cycling,betaVariables,normVariables,file='diagnostic/parameter_settings.Rdata')

parameters_only <- T
multi_city_ithim <- outcome <- outcome_pp <- list()
for(ci in 1:length(cities)){
  city <- cities[ci]

  multi_city_ithim[[ci]] <- run_ithim_setup(CITY=city,  
                                            NSAMPLES = nsamples,
                                            seed=ci,
                                            
                                            DIST_CAT = c('0-1 km','2-5 km','6+ km'),
                                            TEST_WALK_SCENARIO = test_walk_scenario,
                                            TEST_CYCLE_SCENARIO = test_cycle_scenario,
                                            REFERENCE_SCENARIO=ref_scenarios[[city]],
                                            MAX_MODE_SHARE_SCENARIO=T,
                                            ADD_WALK_TO_BUS_TRIPS = add_walk_to_bus_trips[ci],
                                            
                                            speeds = speeds[[city]],
                                            emission_inventory = emission_inventories[[city]],
                                            
                                            MMET_CYCLING = mmet_cycling, 
                                            MMET_WALKING = mmet_walking, 
                                            DAY_TO_WEEK_TRAVEL_SCALAR = day_to_week_scalar,
                                            INJURY_LINEARITY= injury_linearity,
                                            CASUALTY_EXPONENT_FRACTION = casualty_exponent_fraction,
                                            
                                            PA_DOSE_RESPONSE_QUANTILE = pa_dr_quantile[ci],  
                                            AP_DOSE_RESPONSE_QUANTILE = ap_dr_quantile[ci],
                                            
                                            INJURY_REPORTING_RATE = injury_reporting_rate[[city]],  
                                            CHRONIC_DISEASE_SCALAR = chronic_disease_scalar[[city]],  
                                            PM_CONC_BASE = pm_conc_base[[city]],  
                                            PM_TRANS_SHARE = pm_trans_share[[city]],  
                                            BACKGROUND_PA_SCALAR = background_pa_scalar[[city]],
                                            BACKGROUND_PA_CONFIDENCE = background_pa_confidence[[city]],
                                            BUS_WALK_TIME = bus_walk_time[[city]],
                                            MOTORCYCLE_TO_CAR_RATIO = motorcycle_to_car_ratio[[city]],
                                            BUS_TO_PASSENGER_RATIO = bus_to_passenger_ratio[[city]],
                                            TRUCK_TO_CAR_RATIO = truck_to_car_ratio[[city]],
                                            EMISSION_INVENTORY_CONFIDENCE = emission_confidence[[city]],
                                            DISTANCE_SCALAR_CAR_TAXI = distance_scalar_car_taxi[[city]],
                                            DISTANCE_SCALAR_WALKING = distance_scalar_walking[[city]],
                                            DISTANCE_SCALAR_PT = distance_scalar_pt[[city]],
                                            DISTANCE_SCALAR_CYCLING = distance_scalar_cycling[[city]],
                                            DISTANCE_SCALAR_MOTORCYCLE = distance_scalar_motorcycle[[city]])
  
  # for first city, store model parameters. For subsequent cities, copy parameters over.
  if(ci==1){
    model_parameters <- names(multi_city_ithim[[ci]]$parameters)[!names(multi_city_ithim[[ci]]$parameters)%in%setting_parameters]
    parameter_names <- model_parameters[model_parameters!="DR_AP_LIST"]
    parameter_samples <- sapply(parameter_names,function(x)multi_city_ithim[[ci]]$parameters[[x]])
  }else{
    for(param in model_parameters) multi_city_ithim[[ci]]$parameters[[param]] <- multi_city_ithim[[1]]$parameters[[param]]
  }
  
  if(!parameters_only){
    if(Sys.info()[['sysname']] == "Windows"){
      multi_city_ithim[[ci]]$outcomes <- list()
      for(i in 1:nsamples) multi_city_ithim[[ci]]$outcomes[[i]] <- run_ithim(ithim_object = multi_city_ithim[[ci]])
    }else{
      multi_city_ithim[[ci]]$outcomes <- mclapply(1:nsamples, FUN = run_ithim, ithim_object = multi_city_ithim[[ci]],mc.cores = numcores)
    }
    
    ## get outcomes
    min_ages <- sapply(multi_city_ithim[[ci]]$outcomes[[1]]$hb$ylls$age_cat,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
    max_ages <- sapply(multi_city_ithim[[ci]]$outcomes[[1]]$hb$ylls$age_cat,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
    keep_rows <- which(min_ages>=min_age&max_ages<=max_age)
    keep_cols <- which(!sapply(names(multi_city_ithim[[ci]]$outcomes[[1]]$hb$ylls),function(x)grepl('ac|neo|age|sex',as.character(x))))
    
    outcome_pp[[city]] <- t(sapply(multi_city_ithim[[ci]]$outcomes, function(x) colSums(x$hb$ylls[keep_rows,keep_cols],na.rm=T)))
    min_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
    max_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
    outcome_pp[[city]] <- outcome_pp[[city]]/sum(subset(DEMOGRAPHIC,min_pop_ages>=min_age&max_pop_ages<=max_age)$population)
    colnames(outcome_pp[[city]]) <- paste0(colnames(outcome_pp[[city]]),'_',city)
    
    ## omit ac (all cause) and neoplasms (neo) and age and gender columns
    outcome[[city]] <- t(sapply(multi_city_ithim[[ci]]$outcomes, function(x) colSums(x$hb$ylls[keep_rows,keep_cols],na.rm=T)))
    colnames(outcome[[city]]) <- paste0(colnames(outcome[[city]]),'_',city)
  }
  
  ## rename city-specific parameters according to city
  for(i in 1:length(multi_city_ithim[[ci]]$parameters$EMISSION_INVENTORY[[1]])){
    extract_vals <- sapply(multi_city_ithim[[ci]]$parameters$EMISSION_INVENTORY,function(x)x[[i]])
    if(sum(extract_vals)!=0)
      multi_city_ithim[[ci]]$parameters[[paste0('EMISSION_INVENTORY_',names(multi_city_ithim[[ci]]$parameters$EMISSION_INVENTORY[[1]])[i],'_',city)]] <- extract_vals
  }
  for(param in setting_parameters) names(multi_city_ithim[[ci]]$parameters)[which(names(multi_city_ithim[[ci]]$parameters)==param)] <- paste0(param,'_',city)
  multi_city_ithim[[ci]]$parameters <- multi_city_ithim[[ci]]$parameters[-which(names(multi_city_ithim[[ci]]$parameters)==paste0('EMISSION_INVENTORY_',city))]
  parameter_names_city <- names(multi_city_ithim[[ci]]$parameters)[sapply(names(multi_city_ithim[[ci]]$parameters),function(x)grepl(x,pattern=city))]
  ## add to parameter names
  parameter_names <- c(parameter_names,parameter_names_city)
  ## get parameter samples and add to array of parameter samples
  parameter_samples <- cbind(parameter_samples,sapply(parameter_names_city,function(x)multi_city_ithim[[ci]]$parameters[[x]]))
  
}


saveRDS(parameter_samples,'diagnostic/parameter_samples.Rds')

#################################################


## calculate EVPPI
outcomes_pp <- do.call(cbind,outcome_pp)
outcome$combined <- outcomes_pp
##!! find way to set!!
NSCEN <- ncol(outcome[[1]])/sum(sapply(colnames(outcome[[1]]),function(x)grepl('scen1',x)))
evppi <- matrix(0, ncol = NSCEN*(length(cities)+1), nrow = ncol(parameter_samples))
for(j in 1:length(outcome)){
  case <- outcome[[j]]
  for(k in 1:NSCEN){
    scen_case <- case[,seq(k,ncol(case),by=NSCEN)]
    y <- rowSums(scen_case)
    vary <- var(y)
    for(i in 1:ncol(parameter_samples)){
      x <- parameter_samples[, i];
      model <- gam(y ~ s(x))
      evppi[i, (j-1)*NSCEN + k] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100
    }
  }
}
colnames(evppi) <- apply(expand.grid(SCEN_SHORT_NAME[2:6],names(outcome)),1,function(x)paste0(x,collapse='_'))
rownames(evppi) <- colnames(parameter_samples)
## add four-dimensional EVPPI if AP_DOSE_RESPONSE is uncertain.

multi_city_parallel_evppi_for_AP <- function(disease,parameter_samples,outcome){
  voi <- c()
  x1 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_ALPHA_',disease))];
  x2 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_BETA_',disease))];
  x3 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_GAMMA_',disease))];
  x4 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_TMREL_',disease))];
  for(j in 1:length(outcome)){
    case <- outcome[[j]]
    for(k in 1:NSCEN){
      scen_case <- case[,seq(k,ncol(case),by=NSCEN)]
      y <- rowSums(scen_case)
      vary <- var(y)
      model <- gam(y ~ te(x1,x2,x3,x4))
      voi[(j-1)*NSCEN + k] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100
    }
  }
  voi
}

if("DR_AP_LIST"%in%names(multi_city_ithim[[1]]$parameters)&&NSAMPLES>=1024){
  AP_names <- sapply(names(multi_city_ithim[[1]]$parameters),function(x)length(strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA')[[1]])>1)
  diseases <- sapply(names(multi_city_ithim[[1]]$parameters)[AP_names],function(x)strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA_')[[1]][2])
  evppi_for_AP <- mclapply(diseases, 
                           FUN = multi_city_parallel_evppi_for_AP,
                           parameter_samples,
                           outcome, 
                           mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
  names(evppi_for_AP) <- paste0('AP_DOSE_RESPONSE_QUANTILE_',diseases)
  evppi <- rbind(evppi,do.call(rbind,evppi_for_AP))
  ## get rows to remove
  keep_names <- sapply(rownames(evppi),function(x)!any(c('ALPHA','BETA','GAMMA','TMREL')%in%strsplit(x,'_')[[1]]))
  evppi <- evppi[keep_names,]
}

multi_city_parallel_evppi_for_emissions <- function(sources,outcome){
  voi <- c()
  averages <- colMeans(sources)
  x1 <- sources[,order(averages,decreasing=T)[1]];
  x2 <- sources[,order(averages,decreasing=T)[2]];
  x3 <- sources[,order(averages,decreasing=T)[3]];
  x4 <- sources[,order(averages,decreasing=T)[4]];
  for(j in 1:length(outcome)){
    case <- outcome[[j]]
    for(k in 1:NSCEN){
      scen_case <- case[,seq(k,ncol(case),by=NSCEN)]
      y <- rowSums(scen_case)
      vary <- var(y)
      model <- gam(y ~ te(x1,x2,x3,x4))
      voi[(j-1)*NSCEN + k] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100
    }
  }
  voi
}

if("EMISSION_INVENTORY_car_accra"%in%names(multi_city_ithim[[1]]$parameters)&&NSAMPLES>=1024){
  sources <- list()
  for(ci in 1:length(cities)){
    city <- cities[ci]
    emission_names <- sapply(colnames(parameter_samples),function(x)grepl('EMISSION_INVENTORY_',x)&grepl(city,x))
    sources[[ci]] <- parameter_samples[,emission_names]
  }
  evppi_for_emissions <- mclapply(sources, 
                                  FUN = multi_city_parallel_evppi_for_emissions,
                                  outcome, 
                                  mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
  
  names(evppi_for_emissions) <- paste0('EMISSION_INVENTORY_',cities)
  ## get rows to remove
  keep_names <- sapply(rownames(evppi),function(x)!grepl('EMISSION_INVENTORY_',x))
  evppi <- evppi[keep_names,]
  
  evppi <- rbind(evppi,do.call(rbind,evppi_for_emissions))
}
print(evppi)



library(RColorBrewer)
library(plotrix)


#parameter_names <- c('walk-to-bus time','cycling MMETs','walking MMETs','background PM2.5','motorcycle distance','non-travel PA','non-communicable disease burden',
#                     'injury linearity','traffic PM2.5 share','injury reporting rate','casualty exponent fraction','day-to-week scalar',
#                     'all-cause mortality (PA)','IHD (PA)','cancer (PA)','lung cancer (PA)','stroke (PA)','diabetes (PA)','IHD (AP)','lung cancer (AP)',
#                     'COPD (AP)','stroke (AP)')
evppi <- apply(evppi,2,function(x){x[is.na(x)]<-0;x})
{x11(width=8); par(mar=c(6,15,3.5,5.5))
labs <- rownames(evppi)
get.pal=colorRampPalette(brewer.pal(9,"Reds"))
redCol=rev(get.pal(12))
bkT <- seq(max(evppi)+1e-10, 0,length=13)
cex.lab <- 1.5
maxval <- round(bkT[1],digits=1)
col.labels<- c(0,maxval/2,maxval)
cellcolors <- vector()
for(ii in 1:length(unlist(evppi)))
  cellcolors[ii] <- redCol[tail(which(unlist(evppi[ii])<bkT),n=1)]
color2D.matplot(evppi,cellcolors=cellcolors,main="",xlab="",ylab="",cex.lab=2,axes=F)
fullaxis(side=1,las=2,at=NSCEN*0:(length(outcome)-1)+NSCEN/2,labels=names(outcome),line=NA,pos=NA,outer=FALSE,font=NA,lwd=0,cex.axis=1)
fullaxis(side=2,las=1,at=(length(labs)-1):0+0.5,labels=labs,line=NA,pos=NA,outer=FALSE,font=NA,lwd=0,cex.axis=0.8)
mtext(3,text='By how much (%) could we reduce uncertainty in\n the outcome if we knew this parameter perfectly?',line=1)
color.legend(NSCEN*length(outcome)+0.5,0,NSCEN*length(outcome)+0.8,length(labs),col.labels,rev(redCol),gradient="y",cex=1,align="rb")}


scen_out <- sapply(outcome,function(x)rowSums(x))
ninefive <- apply(scen_out,2,quantile,c(0.025,0.975))
{x11(height=3,width=6); par(mar=c(5,5,1,1))
  plot(apply(scen_out,2,mean),1:length(outcome),pch=16,cex=1,frame=F,ylab='',xlab='Change in YLL relative to baseline',col='navyblue',yaxt='n',xlim=range(ninefive))
  axis(2,las=2,at=1:length(outcome),labels=names(outcome))
  for(i in 1:length(outcome)) lines(ninefive[,i],c(i,i),lwd=2,col='navyblue')
  abline(v=0,col='grey',lty=2,lwd=2)
  text(y=3,x=ninefive[1,3],'95%',col='navyblue',adj=c(-0,-0.7))
}

