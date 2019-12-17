library(ithimr)
library(earth)
library(RColorBrewer)
library(plotrix)
rm(list=ls())
cities <- c('accra','sao_paulo','delhi','bangalore','bogota','belo_horizonte','santiago','mexico_city','buenos_aires')
min_age <- 15
max_age <- 69

all_inputs <- read.csv('all_city_parameter_inputs.csv',stringsAsFactors = F)

parameter_names <- all_inputs$parameter
parameter_starts <- which(parameter_names!='')
parameter_stops <- c(parameter_starts[-1] - 1, nrow(all_inputs))
parameter_names <- parameter_names[parameter_names!='']
parameter_list <- list()
compute_mode <- 'sample'
for(i in 1:length(parameter_names)){
  parameter_list[[parameter_names[i]]] <- list()
  parameter_index <- which(all_inputs$parameter==parameter_names[i])
  if(all_inputs[parameter_index,2]=='')  {
    parameter_list[[parameter_names[i]]] <- lapply(cities,function(x) {
      city_index <- which(colnames(all_inputs)==x)
      val <- all_inputs[parameter_index,city_index]
      ifelse(val%in%c('T','F'),val,as.numeric(val))
    })
    names(parameter_list[[parameter_names[i]]]) <- cities
  }else if(all_inputs[parameter_index,2]=='constant'){
    indices <- 0
    if(compute_mode=='sample') indices <- 1:2
    parameter_list[[parameter_names[i]]] <- lapply(cities,function(x) {
      city_index <- which(colnames(all_inputs)==x)
      val <- all_inputs[parameter_index+indices,city_index]
      ifelse(val=='',0,as.numeric(val))
    })
    names(parameter_list[[parameter_names[i]]]) <- cities
  }else{
    parameter_list[[parameter_names[i]]] <- lapply(cities,function(x) {
      city_index <- which(colnames(all_inputs)==x)
      if(any(all_inputs[parameter_starts[i]:parameter_stops[i],city_index]!='')){
        sublist_indices <- which(all_inputs[parameter_starts[i]:parameter_stops[i],city_index]!='')
        thing <- as.list(as.numeric(c(all_inputs[parameter_starts[i]:parameter_stops[i],city_index])[sublist_indices]))
        names(thing) <- c(all_inputs[parameter_starts[i]:parameter_stops[i],2])[sublist_indices]
        thing
      }
    }
    )
    names(parameter_list[[parameter_names[i]]]) <- cities
  }
}

for(i in 1:length(parameter_list)) assign(names(parameter_list)[i],parameter_list[[i]])

###changed the bangalore transport emissions-- MC emissions from 1757 to 817 and car emissions from 4173 to 1107
##this is done based on ratio of car/MC ownership in bangalore to that of delhi from Census data (0.50 and 0.58 respectively)==
###1757=0.58*1409 and 1107=  0.50*2214
##################################################################

# constant parameters for DAY_TO_WEEK_TRAVEL_SCALAR
day_to_week_scalar <- 7



#################################################
## with uncertainty
## comparison across cities
setting_parameters <- c("BUS_WALK_TIME","PM_CONC_BASE","BACKGROUND_PA_SCALAR","BACKGROUND_PA_ZEROS","EMISSION_INVENTORY",                        
                        "CHRONIC_DISEASE_SCALAR","PM_TRANS_SHARE","INJURY_REPORTING_RATE","BUS_TO_PASSENGER_RATIO","TRUCK_TO_CAR_RATIO",
                        "FLEET_TO_MOTORCYCLE_RATIO","DISTANCE_SCALAR_CAR_TAXI",
                        "DISTANCE_SCALAR_WALKING",
                        "DISTANCE_SCALAR_PT",
                        "DISTANCE_SCALAR_CYCLING",
                        "DISTANCE_SCALAR_MOTORCYCLE")


# lnorm parameters for MMET_CYCLING
mmet_cycling <- c(log(4.63),log(1.2))
# lnorm parameters for MMET_WALKING
mmet_walking <- c(log(2.53),log(1.1))
# lnorm parameters for SIN_EXPONENT_SUM
sin_exponent_sum <- c(log(1.9),log(1.03))
# beta parameters for CASUALTY_EXPONENT_FRACTION
casualty_exponent_fraction <- c(15,15)
# logical for PA dose response: set T for city 1, and reuse values in 2 and 3; no need to recompute
pa_dr_quantile <- c(T,rep(F,length(cities)-1))
# logical for AP dose response: set T for city 1, and reuse values in 2 and 3; no need to recompute
ap_dr_quantile <- c(T,rep(F,length(cities)-1))
# logical for walk scenario
test_walk_scenario <- F
# logical for cycle scenario
test_cycle_scenario <- F

betaVariables <- c("PM_TRANS_SHARE",
                   "INJURY_REPORTING_RATE",
                   "CASUALTY_EXPONENT_FRACTION",
                   "BUS_TO_PASSENGER_RATIO",
                   "TRUCK_TO_CAR_RATIO",
                   "FLEET_TO_MOTORCYCLE_RATIO")
normVariables <- c("BUS_WALK_TIME",
                   "MMET_CYCLING",
                   "MMET_WALKING",
                   "PM_CONC_BASE",
                   "BACKGROUND_PA_SCALAR",
                   "CHRONIC_DISEASE_SCALAR",
                   "SIN_EXPONENT_SUM",
                   "DISTANCE_SCALAR_CAR_TAXI",
                   "DISTANCE_SCALAR_WALKING",
                   "DISTANCE_SCALAR_PT",
                   "DISTANCE_SCALAR_CYCLING",
                   "DISTANCE_SCALAR_MOTORCYCLE")

save(cities,setting_parameters,injury_reporting_rate,chronic_disease_scalar,pm_conc_base,pm_trans_share,
     background_pa_scalar,background_pa_confidence,bus_walk_time,mmet_cycling,mmet_walking,emission_inventories,
     sin_exponent_sum,casualty_exponent_fraction,pa_dr_quantile,ap_dr_quantile,
     bus_to_passenger_ratio,truck_to_car_ratio,emission_confidence,distance_scalar_car_taxi,distance_scalar_motorcycle,
     distance_scalar_pt,distance_scalar_walking,distance_scalar_cycling,add_motorcycle_fleet,fleet_to_motorcycle_ratio,
     betaVariables,normVariables,file='diagnostic/parameter_settings.Rdata')


parameters_only <- F
multi_city_ithim <- outcome <- outcome_pp <- yll_per_hundred_thousand <- list()
numcores <- 16
nsamples <- 1024
print(system.time(
  for(ci in 1:length(cities)){
    city <- cities[ci]
    
    multi_city_ithim[[ci]] <- run_ithim_setup(CITY=city,  
                                              NSAMPLES = nsamples,
                                              seed=ci,
                                              
                                              DIST_CAT = c('0-1 km','2-5 km','6+ km'),
                                              AGE_RANGE = c(min_age,max_age),
                                              TEST_WALK_SCENARIO = test_walk_scenario,
                                              TEST_CYCLE_SCENARIO = test_cycle_scenario,
                                              REFERENCE_SCENARIO='Baseline',
                                              MAX_MODE_SHARE_SCENARIO=T,
                                              ADD_BUS_DRIVERS = F,
                                              ADD_TRUCK_DRIVERS = F,
                                              ADD_MOTORCYCLE_FLEET = add_motorcycle_fleet[[city]],
                                              ADD_WALK_TO_BUS_TRIPS = F,#add_walk_to_bus_trips[[city]],
                                              
                                              speeds = speeds[[city]],
                                              emission_inventory = emission_inventories[[city]],
                                              
                                              MMET_CYCLING = mmet_cycling, 
                                              MMET_WALKING = mmet_walking, 
                                              DAY_TO_WEEK_TRAVEL_SCALAR = day_to_week_scalar,
                                              SIN_EXPONENT_SUM= sin_exponent_sum,
                                              CASUALTY_EXPONENT_FRACTION = casualty_exponent_fraction,
                                              
                                              PA_DOSE_RESPONSE_QUANTILE = pa_dr_quantile[ci],  
                                              AP_DOSE_RESPONSE_QUANTILE = ap_dr_quantile[ci],
                                              
                                              INJURY_REPORTING_RATE = injury_reporting_rate[[city]],  
                                              CHRONIC_DISEASE_SCALAR = chronic_disease_scalar[[city]],  
                                              PM_CONC_BASE = pm_conc_base[[city]],  
                                              PM_TRANS_SHARE = pm_trans_share[[city]],  
                                              BACKGROUND_PA_SCALAR = background_pa_scalar[[city]],
                                              BACKGROUND_PA_CONFIDENCE = background_pa_confidence[[city]],
                                              BUS_WALK_TIME = bus_walk_time[[city]],## not random; use mean
                                              BUS_TO_PASSENGER_RATIO = bus_to_passenger_ratio[[city]],
                                              TRUCK_TO_CAR_RATIO = truck_to_car_ratio[[city]],
                                              FLEET_TO_MOTORCYCLE_RATIO = fleet_to_motorcycle_ratio[[city]],
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
      background_quantile <- plnorm(multi_city_ithim[[1]]$parameters$PM_CONC_BASE,pm_conc_base[[1]][1],pm_conc_base[[1]][2])
      multi_city_ithim[[ci]]$parameters$PM_CONC_BASE <- qlnorm(background_quantile,pm_conc_base[[city]][1],pm_conc_base[[city]][2])
      proportion_quantile <- pbeta(multi_city_ithim[[1]]$parameters$PM_TRANS_SHARE,pm_trans_share[[1]][1],pm_trans_share[[1]][2])
      multi_city_ithim[[ci]]$parameters$PM_TRANS_SHARE <- qbeta(background_quantile,pm_trans_share[[city]][1],pm_trans_share[[city]][2])
    }
    
    if(!parameters_only){
      if(Sys.info()[['sysname']] == "Windows"){
        multi_city_ithim[[ci]]$outcomes <- list()
        for(i in 1:nsamples) multi_city_ithim[[ci]]$outcomes[[i]] <- run_ithim(ithim_object = multi_city_ithim[[ci]],seed=i)
      }else{
        multi_city_ithim[[ci]]$outcomes <- mclapply(1:nsamples, FUN = run_ithim, ithim_object = multi_city_ithim[[ci]],mc.cores = numcores)
      }
      multi_city_ithim[[ci]]$DEMOGRAPHIC <- DEMOGRAPHIC
      
      ## get outcomes
      min_ages <- sapply(multi_city_ithim[[ci]]$outcomes[[1]]$hb$ylls$age_cat,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
      max_ages <- sapply(multi_city_ithim[[ci]]$outcomes[[1]]$hb$ylls$age_cat,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
      keep_rows <- which(min_ages>=min_age&max_ages<=max_age)
      keep_cols <- which(!sapply(names(multi_city_ithim[[ci]]$outcomes[[1]]$hb$ylls),function(x)grepl('ac|neo|age|sex',as.character(x))))
      print(city)
      #for(i in 1:length(multi_city_ithim[[ci]]$outcomes)) print(length(multi_city_ithim[[ci]]$outcomes[[i]]))
      outcome_pp[[city]] <- t(sapply(multi_city_ithim[[ci]]$outcomes, function(x) colSums(x$hb$ylls[keep_rows,keep_cols],na.rm=T)))
      min_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
      max_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
      outcome_pp[[city]] <- outcome_pp[[city]]/sum(subset(DEMOGRAPHIC,min_pop_ages>=min_age&max_pop_ages<=max_age)$population)
      colnames(outcome_pp[[city]]) <- paste0(colnames(outcome_pp[[city]]),'_',city)
      ## get yll per 100,000 by age
      outcome_age_min <- c(15,50)
      outcome_age_max <- c(49,69)
      outcome_age_groups <- c('15-49','50-69')
      yll_per_hundred_thousand[[city]] <- list()
      for(aa in 1:length(outcome_age_groups)){
        age <- outcome_age_groups[aa]
        keep_rows2 <- which(min_ages>=outcome_age_min[aa]&max_ages<=outcome_age_max[aa])
        tmp <- t(sapply(multi_city_ithim[[ci]]$outcomes, function(x) colSums(x$hb$ylls[keep_rows2,keep_cols],na.rm=T)))
        tmp <- tmp/sum(subset(DEMOGRAPHIC,min_pop_ages>=outcome_age_min[aa]&max_pop_ages<=outcome_age_max[aa])$population)*100000
        yll_per_hundred_thousand[[city]][[age]] <- tmp
      }
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
    
    saveRDS(multi_city_ithim[[ci]],paste0('results/multi_city/',city,'.Rds'))
    if(ci>1) multi_city_ithim[[ci]] <- 0
  }
))

for(ci in 1:length(cities)) multi_city_ithim[[ci]] <- readRDS(paste0('results/multi_city/',cities[ci],'.Rds'))

saveRDS(parameter_samples,'diagnostic/parameter_samples.Rds',version=2)

## gather results ###############################################
NSCEN <- ncol(outcome[[1]])/sum(sapply(colnames(outcome[[1]]),function(x)grepl('scen1',x)))
SCEN_SHORT_NAME <- c('baseline',rownames(SCENARIO_PROPORTIONS))
## compute and save yll per hundred thousand by age
saveRDS(yll_per_hundred_thousand,'results/multi_city/yll_per_hundred_thousand.Rds',version=2)
yll_per_hundred_thousand_results <- list()
populations <- rep(0,length(outcome_age_groups))
combined_yll <- combined_yll_quantile <- list()
for(aa in 1:length(outcome_age_groups)){
  age <- outcome_age_groups[aa]
  for(ci in 1:length(cities)){
    min_pop_ages <- sapply(multi_city_ithim[[ci]]$DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
    max_pop_ages <- sapply(multi_city_ithim[[ci]]$DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
    populations[aa] <- populations[aa] + sum(subset(multi_city_ithim[[ci]]$DEMOGRAPHIC,min_pop_ages>=outcome_age_min[aa]&max_pop_ages<=outcome_age_max[aa])$population)
  }
  combined_yll[[age]] <- matrix(0,ncol=NSCEN,nrow=NSAMPLES)
  combined_yll_quantile[[age]] <- matrix(0,nrow=NSCEN,ncol=3)#(median=numeric(),'5%'=numeric(),'95%'=numeric())
  colnames(combined_yll_quantile[[age]]) <- c('median','5%','95%')
  rownames(combined_yll_quantile[[age]]) <- rownames(SCENARIO_PROPORTIONS)
}
for(ci in 1:length(cities)){
  city <- cities[ci]
  case <- yll_per_hundred_thousand[[city]]
  yll_per_hundred_thousand_results[[city]] <- list()
  for(aa in 1:length(outcome_age_groups)){
    age <- outcome_age_groups[aa]
    min_pop_ages <- sapply(multi_city_ithim[[ci]]$DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
    max_pop_ages <- sapply(multi_city_ithim[[ci]]$DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
    population <- sum(subset(multi_city_ithim[[ci]]$DEMOGRAPHIC,min_pop_ages>=outcome_age_min[aa]&max_pop_ages<=outcome_age_max[aa])$population)
    yll_per_hundred_thousand_results[[city]][[age]] <- matrix(0,nrow=NSCEN,ncol=3)#(median=numeric(),'5%'=numeric(),'95%'=numeric())
    colnames(yll_per_hundred_thousand_results[[city]][[age]]) <- c('median','5%','95%')
    rownames(yll_per_hundred_thousand_results[[city]][[age]]) <- rownames(SCENARIO_PROPORTIONS)
    case_age <- case[[age]]
    for(k in 1:NSCEN){
      scen_case <- case_age[,seq(k,ncol(case_age),by=NSCEN)]
      y <- rowSums(scen_case)
      yll_per_hundred_thousand_results[[city]][[age]][k,] <- quantile(y,c(0.5,0.05,0.95))
      combined_yll[[age]][,k] <- combined_yll[[age]][,k] + y*population/100000
    }
  }
}
yll_per_hundred_thousand_results$combined <- list()
for(aa in 1:length(outcome_age_groups)){
  age <- outcome_age_groups[aa]
  yll_per_hundred_thousand_results$combined[[age]] <- t(apply(combined_yll[[age]]/populations[aa]*100000,2,quantile,c(0.5,0.05,0.95)))
  colnames(yll_per_hundred_thousand_results$combined[[age]]) <- c('median','5%','95%')
  rownames(yll_per_hundred_thousand_results$combined[[age]]) <- rownames(SCENARIO_PROPORTIONS)
}

saveRDS(yll_per_hundred_thousand_results,'results/multi_city/yll_per_hundred_thousand_quantiles.Rds',version=2)
for(i in 1:length(yll_per_hundred_thousand_results))
  for(j in 1:length(yll_per_hundred_thousand_results[[i]]))
    write.csv(yll_per_hundred_thousand_results[[i]][[j]],
              paste0('results/multi_city/yll_per_hundred_thousand/',names(yll_per_hundred_thousand_results)[i],names(yll_per_hundred_thousand_results[[i]])[j],'.csv'))

## plot results #####################################################################

scen_out <- lapply(outcome[-length(outcome)],function(x)sapply(1:NSCEN,function(y)rowSums(x[,seq(y,ncol(x),by=NSCEN)])))
ninefive <- lapply(scen_out,function(x) apply(x,2,quantile,c(0.05,0.95)))
means <- sapply(scen_out,function(x)apply(x,2,mean))
yvals <- rep(1:length(scen_out),each=NSCEN)/10 + rep(1:NSCEN,times=length(scen_out))
cols <- rainbow(length(outcome)-1)
{pdf('results/multi_city/city_yll.pdf',height=6,width=6); par(mar=c(5,5,1,1))
  plot(as.vector(means),yvals,pch=16,cex=1,frame=F,ylab='',xlab='Change in YLL relative to baseline',col=rep(cols,each=NSCEN),yaxt='n',xlim=range(unlist(ninefive)))
  axis(2,las=2,at=1:NSCEN+0.25,labels=SCEN_SHORT_NAME[2:6])
  for(i in 1:length(outcome[-length(outcome)])) for(j in 1:NSCEN) lines(ninefive[[i]][,j],rep(yvals[j+(i-1)*NSCEN],2),lwd=2,col=cols[i])
  abline(v=0,col='grey',lty=2,lwd=2)
  text(y=4.2,x=ninefive[[2]][1,4],'90%',col='navyblue',adj=c(-0,-0.7))
  legend(col=rev(cols),lty=1,bty='n',x=ninefive[[2]][1,4],legend=rev(names(outcome)[-length(outcome)]),y=4,lwd=2)
  dev.off()
}

comb_out <- sapply(1:NSCEN,function(y)rowSums(outcome[[length(outcome)]][,seq(y,ncol(outcome[[length(outcome)]]),by=NSCEN)]))
ninefive <- apply(comb_out,2,quantile,c(0.05,0.95))
means <- apply(comb_out,2,mean)
{pdf('results/multi_city/combined_yll_pp.pdf',height=3,width=6); par(mar=c(5,5,1,1))
  plot(as.vector(means),1:NSCEN,pch=16,cex=1,frame=F,ylab='',xlab='Change in YLL pp relative to baseline',col='navyblue',yaxt='n',xlim=range(ninefive))
  axis(2,las=2,at=1:NSCEN,labels=SCEN_SHORT_NAME[2:6])
  for(j in 1:NSCEN) lines(ninefive[,j],c(j,j),lwd=2,col='navyblue')
  abline(v=0,col='grey',lty=2,lwd=2)
  text(y=4,x=ninefive[1,4],'90%',col='navyblue',adj=c(-0,-0.7))
  dev.off()
}



## calculate EVPPI ##################################################################
for(i in 1:length(outcome_pp)){
  outcome_pp_quantile <- matrix(0,nrow=NSCEN,ncol=3)#(median=numeric(),'5%'=numeric(),'95%'=numeric())
  colnames(outcome_pp_quantile) <- c('median','5%','95%')
  rownames(outcome_pp_quantile) <- rownames(SCENARIO_PROPORTIONS)
  for(k in 1:NSCEN){
    scen_case <- outcome_pp[[i]][,seq(k,ncol(outcome_pp[[i]]),by=NSCEN)]
    y <- rowSums(scen_case)*100000
    outcome_pp_quantile[k,] <- quantile(y,c(0.5,0.05,0.95))
  }
  write.csv(outcome_pp_quantile,paste0('results/multi_city/yll_per_hundred_thousand/',cities[i],'.csv'))
}
outcomes_pp <- do.call(cbind,outcome_pp)
outcome$combined <- outcomes_pp
saveRDS(outcome,'results/multi_city/outcome.Rds',version=2)


evppi <- matrix(0, ncol = NSCEN*(length(cities)+1), nrow = ncol(parameter_samples))
for(j in 1:length(outcome)){
  case <- outcome[[j]]
  for(k in 1:NSCEN){
    scen_case <- case[,seq(k,ncol(case),by=NSCEN)]
    y <- rowSums(scen_case)
    vary <- var(y)
    for(i in 1:ncol(parameter_samples)){
      x <- parameter_samples[, i];
      model <- earth(y ~ x)
      evppi[i, (j-1)*NSCEN + k] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100
    }
  }
}
colnames(evppi) <- apply(expand.grid(SCEN_SHORT_NAME[2:6],names(outcome)),1,function(x)paste0(x,collapse='_'))
rownames(evppi) <- colnames(parameter_samples)

multi_city_parallel_evppi <- function(jj,sources,outcome,all=F,multi_city_outcome=T){
  voi <- rep(0,length(outcome)*NSCEN)
  sourcesj <- sources[[jj]]
  ncities <- length(outcome) - as.numeric(multi_city_outcome)
  if(all==T) jj <- 1:ncities
  if(multi_city_outcome==T) jj <- c(jj,length(outcome))
  for(j in jj){
    case <- outcome[[j]]
    for(k in 1:NSCEN){
      scen_case <- case[,seq(k,ncol(case),by=NSCEN)]
      y <- rowSums(scen_case)
      vary <- var(y)
      model <- earth(y ~ sourcesj, degree=min(4,ncol(sourcesj)))
      voi[(j-1)*NSCEN + k] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100
    }
  }
  voi
}

## add four-dimensional EVPPI if AP_DOSE_RESPONSE is uncertain.

numcores <- 8
if("DR_AP_LIST"%in%names(multi_city_ithim[[1]]$parameters)&&NSAMPLES>=1024){
  AP_names <- sapply(names(multi_city_ithim[[1]]$parameters),function(x)length(strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA')[[1]])>1)
  diseases <- sapply(names(multi_city_ithim[[1]]$parameters)[AP_names],function(x)strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA_')[[1]][2])
  sources <- list()
  for(di in diseases){
    col_names <- sapply(colnames(parameter_samples),function(x)grepl('AP_DOSE_RESPONSE_QUANTILE',x)&grepl(di,x))
    sources[[di]] <- parameter_samples[,col_names]
  }
  evppi_for_AP <- mclapply(1:length(sources), 
                           FUN = multi_city_parallel_evppi,
                           sources,
                           outcome, 
                           all=T,
                           mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
  names(evppi_for_AP) <- paste0('AP_DOSE_RESPONSE_QUANTILE_',diseases)
  evppi <- rbind(evppi,do.call(rbind,evppi_for_AP))
  ## get rows to remove
  keep_names <- sapply(rownames(evppi),function(x)!any(c('ALPHA','BETA','GAMMA','TMREL')%in%strsplit(x,'_')[[1]]))
  evppi <- evppi[keep_names,]
}

# x2 <- evppi(parameter=c(38:40),input=inp$mat,he=m,method="GP")
#fit <- fit.gp(parameter = parameter, inputs = inputs, x = x, n.sim = n.sim)
if("EMISSION_INVENTORY_car_accra"%in%names(multi_city_ithim[[1]]$parameters)&&NSAMPLES>=1024){
 sources <- list()
 for(ci in 1:length(cities)){
   city <- cities[ci]
   emission_names <- sapply(colnames(parameter_samples),function(x)grepl('EMISSION_INVENTORY_',x)&grepl(city,x))
   sources[[ci]] <- parameter_samples[,emission_names]
 }
 evppi_for_emissions <- mclapply(1:length(sources),
                                 FUN = multi_city_parallel_evppi,
                                 sources,
                                 outcome,
                                 mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))

 names(evppi_for_emissions) <- paste0('EMISSION_INVENTORY_',cities)
 ## get rows to remove
 keep_names <- sapply(rownames(evppi),function(x)!grepl('EMISSION_INVENTORY_',x))
 evppi <- evppi[keep_names,]

 evppi <- rbind(evppi,do.call(rbind,evppi_for_emissions))
}

if(sum(c("BACKGROUND_PA_SCALAR_accra","BACKGROUND_PA_ZEROS_accra")%in%names(multi_city_ithim[[1]]$parameters))==2&&NSAMPLES>=1024){
  sources <- list()
  for(ci in 1:length(cities)){
    city <- cities[ci]
    pa_names <- sapply(colnames(parameter_samples),function(x)(grepl('BACKGROUND_PA_SCALAR_',x)||grepl('BACKGROUND_PA_ZEROS_',x))&grepl(city,x))
    sources[[ci]] <- parameter_samples[,pa_names]
  }
  evppi_for_pa <- mclapply(1:length(sources), 
                           FUN = multi_city_parallel_evppi,
                           sources, 
                           outcome, 
                           mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
  
  names(evppi_for_pa) <- paste0('BACKGROUND_PA_',cities)
  ## get rows to remove
  keep_names <- sapply(rownames(evppi),function(x)!grepl('BACKGROUND_PA_',x))
  evppi <- evppi[keep_names,]
  
  evppi <- rbind(evppi,do.call(rbind,evppi_for_pa))
}

saveRDS(evppi,'results/multi_city/evppi.Rds',version=2)
write.csv(evppi,'results/multi_city/evppi.csv')



#parameter_names <- c('walk-to-bus time','cycling MMETs','walking MMETs','background PM2.5','motorcycle distance','non-travel PA','non-communicable disease burden',
#                     'injury linearity','traffic PM2.5 share','injury reporting rate','casualty exponent fraction','day-to-week scalar',
#                     'all-cause mortality (PA)','IHD (PA)','cancer (PA)','lung cancer (PA)','stroke (PA)','diabetes (PA)','IHD (AP)','lung cancer (AP)',
#                     'COPD (AP)','stroke (AP)')
evppi <- apply(evppi,2,function(x){x[is.na(x)]<-0;x})
{pdf('results/multi_city/evppi.pdf',height=15,width=4+length(outcome)); par(mar=c(10,22,3.5,5.5))
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
  color2D.matplot(evppi,cellcolors=cellcolors,main="",xlab="",ylab="",cex.lab=2,axes=F,border='white')
  fullaxis(side=1,las=2,at=NSCEN*0:(length(outcome)-1)+NSCEN/2,labels=names(outcome),line=NA,pos=NA,outer=FALSE,font=NA,lwd=0,cex.axis=1)
  fullaxis(side=2,las=1,at=(length(labs)-1):0+0.5,labels=labs,line=NA,pos=NA,outer=FALSE,font=NA,lwd=0,cex.axis=0.8)
  mtext(3,text='By how much (%) could we reduce uncertainty in\n the outcome if we knew this parameter perfectly?',line=1)
  color.legend(NSCEN*length(outcome)+0.5,0,NSCEN*length(outcome)+0.8,length(labs),col.labels,rev(redCol),gradient="y",cex=1,align="rb")
  for(i in seq(0,NSCEN*length(outcome),by=NSCEN)) abline(v=i)
  for(i in seq(0,length(labs),by=NSCEN)) abline(h=i)
  dev.off()}

