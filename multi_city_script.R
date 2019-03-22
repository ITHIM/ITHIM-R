rm(list=ls())
cities <- c('accra','sao_paulo','delhi')
speeds <- list(accra=NULL,
               sao_paulo=NULL,
               delhi=list(subway=32,
                          bicycle=15))
emission_inventories = list(accra=NULL,
                            sao_paulo=NULL,
                            delhi=list(motorcycle=1409,
                                       auto_rickshaw=133,
                                       car=2214,
                                       bus_driver=644,
                                       big_truck=4624,
                                       truck=3337,
                                       van=0,
                                       other=0,
                                       taxi=0))


#################################################
## without uncertainty
for(city in cities){
  ithim_object <- run_ithim_setup(DIST_CAT = c("0-1 km", "2-5 km", "6+ km"),
                                  ADD_WALK_TO_BUS_TRIPS=F,
                                  CITY=city,ADD_TRUCK_DRIVERS = F,
                                  ADD_BUS_DRIVERS = F,
                                  emission_inventory = emission_inventories[[city]],
                                  speeds = speeds[[city]])
  #ithim_object <- run_ithim_setup(TEST_WALK_SCENARIO=T,ADD_WALK_TO_BUS_TRIPS=F)
  ithim_object$outcomes <- run_ithim(ithim_object, seed = 1)
  ##
  
  ## plot results
  result_mat <- colSums(ithim_object$outcome$hb$ylls[,3:ncol(ithim_object$outcome$hb$ylls)])
  columns <- length(result_mat)
  nDiseases <- columns/NSCEN
  ylim <- range(result_mat)
  x11(width = 8, height = 8); par(mfrow = c(3, 4))
  for(i in 1:nDiseases){
    if(i<5) {
      par(mar = c(1, 4, 4, 1))
      barplot(result_mat[1:NSCEN + (i - 1) * NSCEN], names.arg = '', ylim = ylim, las = 2, 
              main = paste0(last(strsplit(names(result_mat)[i * NSCEN], '_')[[1]])))
    }else{
      par(mar = c(5, 4, 4, 1))
      barplot(result_mat[1:NSCEN + (i - 1) * NSCEN], names.arg = SCEN_SHORT_NAME[2:6], ylim = ylim, las = 2, 
              main = paste0( last(strsplit(names(result_mat)[i * NSCEN], '_')[[1]])))
    }
  }
}
#################################################
## with uncertainty
## comparison across cities
numcores <- detectCores()
nsamples <- 16
setting_parameters <- c("BUS_WALK_TIME","PM_CONC_BASE","MOTORCYCLE_TO_CAR_RATIO","BACKGROUND_PA_SCALAR",                          
                        "CHRONIC_DISEASE_SCALAR","PM_TRANS_SHARE","INJURY_REPORTING_RATE")


# beta parameters for INJURY_REPORTING_RATE
injury_report_rate <- list(accra=c(8,3),
                           sao_paulo=c(8,3),
                           delhi=c(8,3))
# lnorm parameters for CHRONIC_DISEASE_SCALAR
chronic_disease_scalar <- list(accra=c(0,log(1.2)),
                               sao_paulo=c(0,log(1.2)),
                               delhi=c(0,log(1.2)))
# lnorm parameters for PM_CONC_BASE
pm_concentration <- list(accra=c(50,log(1.2)),
                               sao_paulo=c(50,log(1.2)),
                               delhi=c(50,log(1.2)))
# beta parameters for PM_TRANS_SHARE
pm_trans_share <- list(accra=c(8,3),
                           sao_paulo=c(8,3),
                           delhi=c(8,3))
# lnorm parameters for BACKGROUND_PA_SCALAR
background_pa_scalar <- list(accra=c(0,log(1.2)),
                               sao_paulo=c(0,log(1.2)),
                               delhi=c(0,log(1.2)))
# lnorm parameters for BUS_WALK_TIME
bus_walk_time <- list(accra=c(5,log(1.2)),
                      sao_paulo=c(5,log(1.2)),
                      delhi=c(5,log(1.2)))
# lnorm parameters for MMET_CYCLING
mmet_cycling <- c(5,log(1.2))
# lnorm parameters for MMET_WALKING
mmet_walking <- c(2,log(1.2))
# lnorm parameters for MOTORCYCLE_TO_CAR_RATIO
mc_car_ratio <- list(accra=c(-1.4,0.4),
                       sao_paulo=c(-1.4,0.4),
                       delhi=c(-1.4,0.4))
# beta parameters for DAY_TO_WEEK_TRAVEL_SCALAR
day_to_week_scalar <- c(20,3)
# lnorm parameters for INJURY_LINEARITY
injury_linearity <- c(log(1),log(1.2))
# beta parameters for CASUALTY_EXPONENT_FRACTION
cas_exponent <- c(8,8)
# logical for PA dose response: set T for city 1, and reuse values in 2 and 3; no need to recompute
pa_dr_quantile <- c(T,F,F)
# logical for AP dose response: set T for city 1, and reuse values in 2 and 3; no need to recompute
ap_dr_quantile <- c(T,F,F)
# logical for walk scenario
test_walk_scenario <- c(F,F,F)
# logical for cycle scenario
test_cycle_scenario <- c(F,F,F)
# if walk scenario, choose Baseline as reference scenario
ref_scenarios <- list(accra='Baseline',
                      sao_paulo='Baseline',
                      delhi='Baseline')


multi_city_ithim <- outcome <- outcome_pp <- list()
for(ci in 1:length(cities)){
  city <- cities[ci]

  multi_city_ithim[[ci]] <- run_ithim_setup(CITY=city,  
                                            NSAMPLES = nsamples,
                                            seed=ci,
                                            
                                            DIST_CAT = c('0-1 km','2-5 km','6+ km'),
                                            TEST_WALK_SCENARIO = test_walk_scenario[ci],
                                            TEST_CYCLE_SCENARIO = test_cycle_scenario[ci],
                                            REFERENCE_SCENARIO=ref_scenarios[[city]],
                                            
                                            speeds = speeds[[city]],
                                            emission_inventory = emission_inventories[[city]],
                                            
                                            MMET_CYCLING = mmet_cycling, 
                                            MMET_WALKING = mmet_walking, 
                                            DAY_TO_WEEK_TRAVEL_SCALAR = day_to_week_scalar,
                                            INJURY_LINEARITY= injury_linearity,
                                            CASUALTY_EXPONENT_FRACTION = cas_exponent,
                                            
                                            PA_DOSE_RESPONSE_QUANTILE = pa_dr_quantile[ci],  
                                            AP_DOSE_RESPONSE_QUANTILE = ap_dr_quantile[ci],
                                            
                                            INJURY_REPORTING_RATE = injury_report_rate[[city]],  
                                            CHRONIC_DISEASE_SCALAR = chronic_disease_scalar[[city]],  
                                            PM_CONC_BASE = pm_concentration[[city]],  
                                            PM_TRANS_SHARE = pm_trans_share[[city]],  
                                            BACKGROUND_PA_SCALAR = background_pa_scalar[[city]],
                                            BUS_WALK_TIME = bus_walk_time[[city]],
                                            MOTORCYCLE_TO_CAR_RATIO = mc_car_ratio[[city]])
  
  # for first city, store model parameters. For subsequent cities, copy parameters over.
  if(ci==1){
    model_parameters <- names(multi_city_ithim[[ci]]$parameters)[!names(multi_city_ithim[[ci]]$parameters)%in%setting_parameters]
    parameter_names <- model_parameters[model_parameters!="DR_AP_LIST"]
    parameter_samples <- sapply(parameter_names,function(x)multi_city_ithim[[ci]]$parameters[[x]])
  }else{
    for(param in model_parameters) multi_city_ithim[[ci]]$parameters[[param]] <- multi_city_ithim[[1]]$parameters[[param]]
  }
  
  if(Sys.info()[['sysname']] == "Windows"){
    multi_city_ithim[[ci]]$outcomes <- list()
    for(i in 1:nsamples) multi_city_ithim[[ci]]$outcomes[[i]] <- ithim_uncertainty(ithim_object = multi_city_ithim[[ci]])
  }else{
    multi_city_ithim[[ci]]$outcomes <- mclapply(1:nsamples, FUN = ithim_uncertainty, ithim_object = multi_city_ithim[[ci]],mc.cores = numcores)
  }
  
  ## rename city-specific parameters according to city
  for(param in setting_parameters) names(multi_city_ithim[[ci]]$parameters)[which(names(multi_city_ithim[[ci]]$parameters)==param)] <- paste0(param,'_',city)
  parameter_names_city <- paste0(setting_parameters,'_',city)
  ## add to parameter names
  parameter_names <- c(parameter_names,parameter_names_city)
  ## get parameter samples and add to array of parameter samples
  parameter_samples <- cbind(parameter_samples,sapply(parameter_names_city,function(x)multi_city_ithim[[ci]]$parameters[[x]]))
  
  ## get outcomes
  keep_cols <- which(!sapply(names(multi_city_ithim[[ci]]$outcomes[[1]]$hb$ylls),function(x)grepl('ac|neo|age|sex',as.character(x))))
  outcome_pp[[city]] <- t(sapply(multi_city_ithim[[ci]]$outcomes, function(x) colSums(x$hb$ylls[,keep_cols],na.rm=T)))
  outcome_pp[[city]] <- outcome_pp[[city]]/sum(DEMOGRAPHIC$population)
  colnames(outcome_pp[[city]]) <- paste0(colnames(outcome_pp[[city]]),'_',city)
  
  ## omit ac (all cause) and neoplasms (neo) and age and gender columns
  outcome[[city]] <- t(sapply(multi_city_ithim[[ci]]$outcomes, function(x) colSums(x$hb$ylls[,keep_cols],na.rm=T)))
  colnames(outcome[[city]]) <- paste0(colnames(outcome[[city]]),'_',city)
}





####################


## calculate EVPPI
outcomes_pp <- do.call(cbind,outcome_pp)
outcome$combined <- outcomes_pp
##!! find way to set!!
NSCEN <- 1
evppi <- matrix(0, ncol = length(cities)+1, nrow = ncol(parameter_samples))
for(j in 1:length(outcome)){
  case <- outcome[[j]]
  y <- rowSums(case)
  vary <- var(y)
  for(i in 1:ncol(parameter_samples)){
    x <- parameter_samples[, i];
    model <- gam(y ~ s(x))
    evppi[i, j] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100
    
  }
}
colnames(evppi) <- names(outcome)
rownames(evppi) <- colnames(parameter_samples)
## add four-dimensional EVPPI if AP_DOSE_RESPONSE is uncertain.

multi_city_parallel_evppi_for_AP <- function(disease,parameter_samples,outcome){
  AP_DOSE_RESPONSE_QUANTILE <- c()
  x1 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_ALPHA_',disease))];
  x2 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_BETA_',disease))];
  x3 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_GAMMA_',disease))];
  x4 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_TMREL_',disease))];
  for(j in 1:length(outcome)){
    case <- outcome[[j]]
    y <- rowSums(case)
    vary <- var(y)
    model <- gam(y ~ te(x1,x2,x3,x4))
    AP_DOSE_RESPONSE_QUANTILE[j] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100 
  }
  AP_DOSE_RESPONSE_QUANTILE
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
print(evppi)



library(RColorBrewer)
library(plotrix)


#parameter_names <- c('walk-to-bus time','cycling MMETs','walking MMETs','background PM2.5','motorcycle distance','non-travel PA','non-communicable disease burden',
#                     'injury linearity','traffic PM2.5 share','injury reporting rate','casualty exponent fraction','day-to-week scalar',
#                     'all-cause mortality (PA)','IHD (PA)','cancer (PA)','lung cancer (PA)','stroke (PA)','diabetes (PA)','IHD (AP)','lung cancer (AP)',
#                     'COPD (AP)','stroke (AP)')
{x11(width=5); par(mar=c(6,12,3.5,5.5))
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
fullaxis(side=1,las=2,at=1:length(outcome)-0.5,labels=names(outcome),line=NA,pos=NA,outer=FALSE,font=NA,lwd=0,cex.axis=1)
fullaxis(side=2,las=1,at=(length(labs)-1):0+0.5,labels=labs,line=NA,pos=NA,outer=FALSE,font=NA,lwd=0,cex.axis=0.8)
mtext(3,text='By how much (%) could we reduce uncertainty in\n the outcome if we knew this parameter perfectly?',line=1)
color.legend(5.5,0,5.5+0.3,length(labs),col.labels,rev(redCol),gradient="y",cex=1,align="rb")}


scen_out <- sapply(outcome,function(x)rowSums(x))
ninefive <- apply(scen_out,2,quantile,c(0.025,0.975))
{x11(height=3,width=6); par(mar=c(5,5,1,1))
  plot(apply(scen_out,2,mean),1:length(outcome),pch=16,cex=1,frame=F,ylab='',xlab='Change in YLL relative to baseline',col='navyblue',yaxt='n',xlim=range(ninefive))
  axis(2,las=2,at=1:length(outcome),labels=names(outcome))
  for(i in 1:length(outcome)) lines(ninefive[,i],c(i,i),lwd=2,col='navyblue')
  abline(v=0,col='grey',lty=2,lwd=2)
  text(y=3,x=ninefive[1,3],'95%',col='navyblue',adj=c(-0,-0.7))
}

