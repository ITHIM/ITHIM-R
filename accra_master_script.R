rm (list = ls())
library(ithimr)
#################################################
## Use case 0: basic ITHIM, walk scenario:

## 
ithim_object <- run_ithim_setup(TEST_WALK_SCENARIO=T,ADD_WALK_TO_BUS_TRIPS=F,CITY='accra_test',ADD_TRUCK_DRIVERS = F,ADD_BUS_DRIVERS = F)
#ithim_object <- run_ithim_setup(TEST_WALK_SCENARIO=T,ADD_WALK_TO_BUS_TRIPS=F)
ithim_object$outcomes <- run_ithim(ithim_object, seed = 1)
print(names(ithim_object$outcome))
##

## plot results
result_mat <- colSums(ithim_object$outcome$hb$ylls[,3:ncol(ithim_object$outcome$hb$ylls)])
columns <- length(result_mat)
nDiseases <- columns/NSCEN
ylim <- range(result_mat)
#x11(width = 8, height = 5); par(mfrow = c(2, 4))
barplot(result_mat, names.arg = sapply(names(result_mat) ,function(x)paste0( last(strsplit(x, '_')[[1]]))), ylim = ylim, las = 2)


## 
ithim_object <- run_ithim_setup(DIST_CAT = c("0-1 km", "2-5 km", "6+ km"),ADD_WALK_TO_BUS_TRIPS=F,CITY='accra_test',ADD_TRUCK_DRIVERS = F,ADD_BUS_DRIVERS = F,MAX_MODE_SHARE_SCENARIO = T)
#ithim_object <- run_ithim_setup(TEST_WALK_SCENARIO=T,ADD_WALK_TO_BUS_TRIPS=F)
ithim_object$outcomes <- run_ithim(ithim_object, seed = 1)
##

## plot results
result_mat <- colSums(ithim_object$outcome$hb$ylls[,3:ncol(ithim_object$outcome$hb$ylls)])
columns <- length(result_mat)
nDiseases <- columns/NSCEN
ylim <- range(result_mat)
{x11(width = 8, height = 8); par(mfrow = c(3, 4))
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
  }}


#################################################
## Use case 1: basic ITHIM:

## 
ithim_object <- run_ithim_setup(REFERENCE_SCENARIO='Scenario 1')
ithim_object$outcomes <- run_ithim(ithim_object, seed = 1)
print(names(ithim_object$outcome))
##

## plot results
result_mat <- colSums(ithim_object$outcome$hb$deaths[,3:ncol(ithim_object$outcome$hb$deaths)])
columns <- length(result_mat)
nDiseases <- columns/NSCEN
ylim <- range(result_mat)
{x11(width = 8, height = 8); par(mfrow = c(3, 4))
for(i in 1:nDiseases){
  if(i<5) {
    par(mar = c(1, 4, 4, 1))
    barplot(result_mat[1:NSCEN + (i - 1) * NSCEN], names.arg = '', ylim = ylim, las = 2, 
            main = paste0(last(strsplit(names(result_mat)[i * NSCEN], '_')[[1]])))
  }else{
    par(mar = c(5, 4, 4, 1))
    barplot(result_mat[1:NSCEN + (i - 1) * NSCEN], names.arg = SCEN_SHORT_NAME[c(1, 3:6)], ylim = ylim, las = 2, 
            main = paste0( last(strsplit(names(result_mat)[i * NSCEN], '_')[[1]])))
  }
}}

#################################################
## Use case 2: environmental scenarios:

## assume already run:
# ithim_object <- run_ithim_setup(REFERENCE_SCENARIO='Scenario 1')
ithim_object$outcome <- list()

## what if: cleaner fleet !! not yet implemented
ithim_object$parameters <- ithim_setup_parameters()
ithim_object$outcome$cleaner_fleet <- run_ithim(ithim_object, seed = 1)

## what if: the roads are safer
ithim_object$parameters <- ithim_setup_parameters(INJURY_REPORTING_RATE = 2)
ithim_object$outcome$safety <- run_ithim(ithim_object, seed = 1)

## what if: the rate of chronic disease doubles
ithim_object$parameters <- ithim_setup_parameters(CHRONIC_DISEASE_SCALAR = 2)
ithim_object$outcome$chronic_disease <- run_ithim(ithim_object, seed = 1)

## what if: non-transport air pollution is half
ithim_object$parameters <- ithim_setup_parameters(PM_CONC_BASE = 30.625, PM_TRANS_SHARE = 0.3673469)
ithim_object$outcome$background_ap <- run_ithim(ithim_object, seed = 1)

## what if: non-transport physical activity is half
ithim_object$parameters <- ithim_setup_parameters(BACKGROUND_PA_SCALAR = 0.5)
ithim_object$outcome$background_pa <- run_ithim(ithim_object, seed = 1)

## plot results
result_list <- lapply(ithim_object$outcome,function(x)colSums(x$hb$ylls[,3:ncol(x$hb$ylls)]))
result_mat <- do.call(rbind, result_list)
columns <- ncol(result_mat)
nDiseases <- columns/NSCEN
for(i in 1:nDiseases){
  x11(width = 8, height = 5); par(mfrow = c(2, 3))
  ylim <- range(result_mat[, 1:NSCEN+(i-1)*NSCEN])
  for(j in 1:nrow(result_mat)){
    if(j<4) {
      par(mar = c(1, 4, 4, 1))
      barplot(result_mat[j, 1:NSCEN + (i - 1) * NSCEN], names.arg = '', ylim = ylim, las = 2, 
              main = paste0(rownames(result_mat)[j], ', ', last(strsplit(colnames(result_mat)[i * NSCEN], '_')[[1]])))
    }else{
      par(mar = c(5, 4, 4, 1))
      barplot(result_mat[j, 1:NSCEN + (i - 1) * NSCEN], names.arg = SCEN_SHORT_NAME[c(1, 3:6)], ylim = ylim, las = 2, 
              main = paste0(rownames(result_mat)[j], ',  ', last(strsplit(colnames(result_mat)[i * NSCEN], '_')[[1]])))
    }
  }
}

#################################################
## Use case 3: sampling:
## sample size, travel patterns, emissions (cleaner fleet)

ithim_object <- run_ithim_setup(NSAMPLES = 1024,
                                TEST_WALK_SCENARIO=T,
                                ADD_WALK_TO_BUS_TRIPS = F,# = c(log(5), log(1.2)),
                                MMET_WALKING = c(log(2.53),log(1.2)), 
                                MMET_CYCLING = c(log(4.63),log(1.2)), 
                                PM_CONC_BASE = c(log(50), log(1.3)),  
                                PM_TRANS_SHARE = c(5,20), 
                                INJURY_REPORTING_RATE = c(8,3), 
                                CHRONIC_DISEASE_SCALAR = c(log(1), log(1.2)),  
                                BACKGROUND_PA_SCALAR = c(log(1), log(1.2)),   
                                BUS_TO_PASSENGER_RATIO = c(20,600),
                                TRUCK_TO_CAR_RATIO = c(3,10),
                                DISTANCE_SCALAR_CAR_TAXI = c(0,log(1.2)),
                                DISTANCE_SCALAR_MOTORCYCLE = c(0,log(1.2)),
                                DISTANCE_SCALAR_WALKING = c(0,log(1.2)),
                                DISTANCE_SCALAR_CYCLING = c(0,log(1.2)),
                                DISTANCE_SCALAR_PT = c(0,log(1.2)),
                                PA_DOSE_RESPONSE_QUANTILE = T,  
                                AP_DOSE_RESPONSE_QUANTILE = T,
                                DAY_TO_WEEK_TRAVEL_SCALAR = 7,#c(20,3),
                                SIN_EXPONENT_SUM= c(log(2),log(1.1)),
                                CASUALTY_EXPONENT_FRACTION = c(15,15),
                                EMISSION_INVENTORY_CONFIDENCE = 0.5,
                                BACKGROUND_PA_CONFIDENCE = 0.5)

numcores <- 4#detectCores()
run_ithim(ithim_object,1)
ithim_object$outcomes <- mclapply(1:NSAMPLES, FUN = run_ithim, ithim_object = ithim_object, mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
for(i in 1:NSAMPLES) print(length(ithim_object$outcomes[[i]]))

plot(ithim_object$parameters$MOTORCYCLE_TO_CAR_RATIO,sapply(ithim_object$outcomes,function(x)sum(x$hb$deaths[,40])))
plot(ithim_object$parameters$INJURY_REPORTING_RATE,sapply(ithim_object$outcomes,function(x)sum(x$hb$deaths[,40])))
plot(ithim_object$parameters$AP_DOSE_RESPONSE_QUANTILE_GAMMA_cvd_ihd,sapply(ithim_object$outcomes,function(x)sum(x$hb$deaths[,10])))

## calculate EVPPI
parameter_names <- names(ithim_object$parameters)[!names(ithim_object$parameters)%in%c('PROPENSITY_TO_TRAVEL','EMISSION_INVENTORY',"DR_AP_LIST")]
parameter_samples <- sapply(parameter_names,function(x)ithim_object$parameters[[x]])
outcome <- t(sapply(ithim_object$outcomes, function(x) colSums(x$hb$ylls[,c(5,7:ncol(x$hb$ylls))])))

upper <- apply(outcome,2,quantile,.95)
lower <- apply(outcome,2,quantile,.05)
{
  pdf('accra_walk_yll.pdf'); par(mar=c(5,5,2,2))
  plot(c(lower[1],upper[1]),c(1,1),lty=1,typ='l',col='navyblue',lwd=2,xlim=1.1*range(c(lower,upper)),ylim=c(0,length(lower)),yaxt='n',frame=F,ylab='',xlab='YLL gain',cex.axis=1.5,cex.lab=1.5)
  abline(v=0,col='grey',lwd=2,lty=2)
  for(i in 2:length(lower)) lines(c(lower[i],upper[i]),c(i,i),lty=1,col='navyblue',lwd=2)
  axis(2,at=1:length(lower),labels=sapply(names(lower),function(x)last(strsplit(x,'_')[[1]])),las=2)
  text(x=max(upper),y=which.max(upper),'90%',cex=1.5,pos=3,col='navyblue')
  dev.off()
}

evppi <- matrix(0, ncol = NSCEN, nrow = ncol(parameter_samples))
for(j in 1:(NSCEN)){
  y <- rowSums(outcome)#[, j+5] ## +5 means we choose ihd outcome for each scenario
  vary <- var(y)
  for(i in 1:ncol(parameter_samples)){
    x <- parameter_samples[, i];
    model <- gam(y ~ s(x))
    evppi[i, j] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100
    
  }
}
#colnames(evppi) <- SCEN_SHORT_NAME[c(1,3:6)]
rownames(evppi) <- colnames(parameter_samples)
parallel_evppi_for_AP <- function(disease,parameter_samples,outcome,NSCEN){
  AP_DOSE_RESPONSE_QUANTILE <- c()
  x1 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_ALPHA_',disease))];
  x2 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_BETA_',disease))];
  x3 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_GAMMA_',disease))];
  x4 <- parameter_samples[,which(colnames(parameter_samples)==paste0('AP_DOSE_RESPONSE_QUANTILE_TMREL_',disease))];
  for(j in 1:(NSCEN)){
    y <- rowSums(outcome[,seq(j,ncol(outcome),by=NSCEN)])
    vary <- var(y)
    model <- gam(y ~ te(x1,x2,x3,x4))
    AP_DOSE_RESPONSE_QUANTILE[j] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100 
  }
  AP_DOSE_RESPONSE_QUANTILE
}

## add four-dimensional EVPPI if AP_DOSE_RESPONSE is uncertain.
if("DR_AP_LIST"%in%names(ithim_object$parameters)&&NSAMPLES>=1024){
  AP_names <- sapply(names(ithim_object$parameters),function(x)length(strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA')[[1]])>1)
  diseases <- sapply(names(ithim_object$parameters)[AP_names],function(x)strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA_')[[1]][2])
  evppi_for_AP <- mclapply(diseases, FUN = parallel_evppi_for_AP,parameter_samples,outcome,NSCEN, mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
  names(evppi_for_AP) <- paste0('AP_DOSE_RESPONSE_QUANTILE_',diseases)
  evppi <- evppi[!sapply(rownames(evppi),function(x)length(strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_')[[1]])>1),,drop=F]
  evppi <- rbind(evppi,do.call(rbind,evppi_for_AP))
}
print(evppi)

multi_city_parallel_evppi_for_emissions <- function(sources,outcome){
  voi <- c()
  averages <- colMeans(sources)
  x1 <- sources[,order(averages,decreasing=T)[1]];
  x2 <- sources[,order(averages,decreasing=T)[2]];
  x3 <- sources[,order(averages,decreasing=T)[3]];
  x4 <- sources[,order(averages,decreasing=T)[4]];
  #for(j in 1:length(outcome)){
  case <- outcome#[[j]]
  for(k in 1:NSCEN){
    scen_case <- case[,seq(k,ncol(case),by=NSCEN)]
    y <- rowSums(scen_case)
    vary <- var(y)
    model <- gam(y ~ te(x1,x2,x3,x4))
    voi[(j-1)*NSCEN + k] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100
  }
  #}
  voi
}

if("EMISSION_INVENTORY"%in%names(ithim_object$parameters)&&NSAMPLES>=1024){
  sources <- as.data.frame(t(sapply(ithim_object$parameters$EMISSION_INVENTORY,function(x) unlist(x))))
  evppi_for_emissions <- multi_city_parallel_evppi_for_emissions(sources, outcome)
  
  names(evppi_for_emissions) <- NULL
  
  evppi <- rbind(evppi,EMISSION_INVENTORY=evppi_for_emissions)
}
print(evppi)
{
  pdf('accra_walk_evppi.pdf',height=10)#x11(height=10); 
  par(mar=c(5,25,2,2)); 
  barplot(evppi,horiz = T,xlab='EVPPI, %',cex.axis=1.5,cex.lab=1.5,beside = T,names.arg=rownames(evppi),las=2)
  dev.off()
}

#################################################
## Use case 4: Application: six behavioural scenarios and five environmental scenarios.
## sample size, travel patterns, emissions (cleaner fleet)

environmental_scenarios <- c('now','safer','more_chronic_disease','less_background_AP','less_background_PA')
certainty_parameters <- list(uncertain=list(
  safey_scalar          = list(now=c(8,3),             safer=2,                  more_chronic_disease=c(8,3),             less_background_AP=c(8,3),       less_background_PA=c(8,3)),
  disease_scalar        = list(now=c(0,log(1.2)),      safer=c(0,log(1.2)),      more_chronic_disease=2.0,                less_background_AP=c(0,log(1.2)),less_background_PA=c(0,log(1.2))),
  background_pm         = list(now=c(log(50),log(1.2)),safer=c(log(50),log(1.2)),more_chronic_disease=c(log(50),log(1.2)),less_background_AP=30.625,       less_background_PA=c(log(50),log(1.2))),
  transport_pm          = list(now=c(5,20),            safer=c(5,20),            more_chronic_disease=c(5,20),            less_background_AP=0.3673469,    less_background_PA=c(5,20)),
  background_pa_scalar  = list(now=c(0,log(1.2)),      safer=c(0,log(1.2)),      more_chronic_disease=c(0,log(1.2)),      less_background_AP=c(0,log(1.2)),less_background_PA=0.5),
  NSAMPLES = 1024,
  BUS_WALK_TIME = c(log(5), log(1.2)),
  MMET_CYCLING = c(log(5), log(1.2)), 
  MMET_WALKING = c(log(2.5), log(1.2)), 
  MOTORCYCLE_TO_CAR_RATIO = c(-1.4,0.4),
  DAY_TO_WEEK_TRAVEL_SCALAR = 7,
  SIN_EXPONENT_SUM= c(log(2),log(1.2)),
  CASUALTY_EXPONENT_FRACTION = c(8,8),
  PA_DOSE_RESPONSE_QUANTILE = T,  
  AP_DOSE_RESPONSE_QUANTILE = T,
  EMISSION_INVENTORY_CONFIDENCE = 0.5,
  BUS_TO_PASSENGER_RATIO = c(20,600),
  TRUCK_TO_CAR_RATIO = c(3,10)
), not_uncertain=list(
  safey_scalar          = list(now=1,    safer=2,    more_chronic_disease=1,    less_background_AP=1,        less_background_PA=1),
  disease_scalar        = list(now=1,    safer=1,    more_chronic_disease=2.0,  less_background_AP=1,        less_background_PA=1),
  background_pm         = list(now=50,   safer=50,   more_chronic_disease=50,   less_background_AP=30.625,   less_background_PA=50),
  transport_pm          = list(now=0.225,safer=0.225,more_chronic_disease=0.225,less_background_AP=0.3673469,less_background_PA=0.225),
  background_pa_scalar  = list(now=1,    safer=1,    more_chronic_disease=1,    less_background_AP=1,        less_background_PA=0.5),
  NSAMPLES = 1,
  BUS_WALK_TIME = 5,
  MMET_CYCLING = 4.63, 
  MMET_WALKING = 2.53, 
  MOTORCYCLE_TO_CAR_RATIO = 0.2,
  DAY_TO_WEEK_TRAVEL_SCALAR = 7,
  SIN_EXPONENT_SUM= 2,
  CASUALTY_EXPONENT_FRACTION = 0.5,
  PA_DOSE_RESPONSE_QUANTILE = F,  
  AP_DOSE_RESPONSE_QUANTILE = F,
  EMISSION_INVENTORY_CONFIDENCE = 1
))

file_name <- paste0('six_by_one_scenarios_',certainty_parameters$uncertain$NSAMPLES,'.Rds')
if(file.exists(file_name)){
  ithim_object_list <- readRDS(file_name)
}else{
  ithim_object_list <- list()
  for(certainty in c('not_uncertain','uncertain')){
    ithim_object_list[[certainty]] <- list()
    for(environmental_scenario in environmental_scenarios)
      if(certainty=='uncertain'&&environmental_scenario=='now'||certainty=='not_uncertain'){
        ithim_object <- run_ithim_setup(NSAMPLES = certainty_parameters[[certainty]]$NSAMPLES,
                                        REFERENCE_SCENARIO='Scenario 1',
                                      BUS_WALK_TIME = certainty_parameters[[certainty]]$BUS_WALK_TIME,
                                      MMET_CYCLING = certainty_parameters[[certainty]]$MMET_CYCLING, 
                                      MMET_WALKING = certainty_parameters[[certainty]]$MMET_WALKING, 
                                      INJURY_REPORTING_RATE = certainty_parameters[[certainty]]$safey_scalar[[environmental_scenario]],  
                                      CHRONIC_DISEASE_SCALAR = certainty_parameters[[certainty]]$disease_scalar[[environmental_scenario]],  
                                      PM_CONC_BASE = certainty_parameters[[certainty]]$background_pm[[environmental_scenario]],  
                                      PM_TRANS_SHARE = certainty_parameters[[certainty]]$transport_pm[[environmental_scenario]],  
                                      BACKGROUND_PA_SCALAR = certainty_parameters[[certainty]]$background_pa_scalar[[environmental_scenario]],  
                                      MOTORCYCLE_TO_CAR_RATIO = certainty_parameters[[certainty]]$MOTORCYCLE_TO_CAR_RATIO,  
                                      DAY_TO_WEEK_TRAVEL_SCALAR = certainty_parameters[[certainty]]$DAY_TO_WEEK_TRAVEL_SCALAR,
                                      CASUALTY_EXPONENT_FRACTION = certainty_parameters[[certainty]]$CASUALTY_EXPONENT_FRACTION,  
                                      SIN_EXPONENT_SUM = certainty_parameters[[certainty]]$SIN_EXPONENT_SUM,
                                      PA_DOSE_RESPONSE_QUANTILE = certainty_parameters[[certainty]]$PA_DOSE_RESPONSE_QUANTILE,  
                                      AP_DOSE_RESPONSE_QUANTILE = certainty_parameters[[certainty]]$AP_DOSE_RESPONSE_QUANTILE)
        print(c(certainty,environmental_scenario))
        numcores <- detectCores()
        if(certainty=='not_uncertain'){
          ithim_object$outcomes <- run_ithim(ithim_object, seed = 1)
        }else if(certainty=='uncertain'&&environmental_scenario=='now'){
          print(1)
          ithim_object$outcomes <- mclapply(1:NSAMPLES, FUN = ithim_uncertainty, ithim_object = ithim_object,mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
          ## calculate EVPPI
          parameter_names <- names(ithim_object$parameters)[names(ithim_object$parameters)!="DR_AP_LIST"]
          parameter_samples <- sapply(parameter_names,function(x)ithim_object$parameters[[x]])
          ## omit all-cause mortality
          keep_indices <- which(sapply(colnames(ithim_object$outcomes[[1]]$hb$deaths),function(x)!grepl('neo',x)&!grepl('ac',x)))
          outcome <- t(sapply(ithim_object$outcomes, function(x) colSums(x$hb$deaths[,keep_indices[-c(1:2)]])))
          evppi <- matrix(0, ncol = NSCEN, nrow = ncol(parameter_samples))
          for(j in 1:(NSCEN)){
            y <- rowSums(outcome[,seq(j,ncol(outcome),by=NSCEN)])
            vary <- var(y)
            for(i in 1:ncol(parameter_samples)){
              x <- parameter_samples[, i];
              model <- gam(y ~ s(x))
              evppi[i, j] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100
              
            }
          }
          colnames(evppi) <- SCEN_SHORT_NAME[c(1,3:6)]
          rownames(evppi) <- colnames(parameter_samples)
          ## add four-dimensional EVPPI if AP_DOSE_RESPONSE is uncertain.
          if("DR_AP_LIST"%in%names(ithim_object$parameters)&&NSAMPLES>=1024){
            AP_names <- sapply(names(ithim_object$parameters),function(x)length(strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA')[[1]])>1)
            diseases <- sapply(names(ithim_object$parameters)[AP_names],function(x)strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA_')[[1]][2])
            NSCEN <- 5
            evppi_for_AP <- mclapply(diseases, FUN = parallel_evppi_for_AP,parameter_samples,outcome,NSCEN, mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
            names(evppi_for_AP) <- paste0('AP_DOSE_RESPONSE_QUANTILE_',diseases)
            evppi <- rbind(evppi,do.call(rbind,evppi_for_AP))
            ## get rows to remove
            keep_names <- sapply(rownames(evppi),function(x)!any(c('ALPHA','BETA','GAMMA','TMREL')%in%strsplit(x,'_')[[1]]))
            evppi <- evppi[keep_names,]
          }
          print(evppi)
          ithim_object$evppi <- evppi
        }
        
        ithim_object_list[[certainty]][[environmental_scenario]] <- ithim_object
      }
  }
  saveRDS(ithim_object_list,file_name,version=2)

  }

library(RColorBrewer)
library(plotrix)

#file_name <- paste0('six_by_one_scenarios_4096.Rds')
#ithim_object_list <- readRDS(file_name)
evppi <- ithim_object_list$uncertain$now$evppi


parameter_names <- c('walk-to-bus time','cycling MMETs','walking MMETs','background PM2.5','motorcycle distance','non-travel PA','non-communicable disease burden',
                     'injury linearity','traffic PM2.5 share','injury reporting rate','casualty exponent fraction','day-to-week scalar',
                     'all-cause mortality (PA)','IHD (PA)','cancer (PA)','lung cancer (PA)','stroke (PA)','diabetes (PA)','breast cancer (PA)','colon cancer (PA)',
                     'endometrial cancer (PA)','IHD (AP)','lung cancer (AP)','COPD (AP)','stroke (AP)','LRI (AP)')
x11(width=5); par(mar=c(6,12,3.5,5.5))
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
fullaxis(side=1,las=2,at=0:4+0.5,labels=SCEN_SHORT_NAME[-2],line=NA,pos=NA,outer=FALSE,font=NA,lwd=0,cex.axis=1)
fullaxis(side=2,las=1,at=(length(labs)-1):0+0.5,labels=parameter_names,line=NA,pos=NA,outer=FALSE,font=NA,lwd=0,cex.axis=0.8)
mtext(3,text='By how much (%) could we reduce uncertainty in\n the outcome if we knew this parameter perfectly?',line=1)
color.legend(5.5,0,5.5+0.3,length(labs),col.labels,rev(redCol),gradient="y",cex=1,align="rb")

parameter_names <- c('walk-to-bus time','cycling MMETs','walking MMETs','background PM2.5','motorcycle distance','non-travel PA','NCD burden',
                     'injury linearity','traffic PM2.5 share','injury reporting rate','casualty exponent fraction','day-to-week scalar',
                     'all-cause mortality (PA)','IHD (PA)','cancer (PA)','lung cancer (PA)','stroke (PA)','diabetes (PA)','IHD (AP)','lung cancer (AP)',
                     'COPD (AP)','stroke (AP)')
x11(width=9,height=6); par(mfrow=c(3,4),mar=c(5,3,1,1)); 
for(i in 1:12)  plot(density(ithim_object_list$uncertain$now$parameters[[i]]),cex.lab=1.5,cex.axis=1.5,col='navyblue',xlab=parameter_names[i],ylab='',frame=F,main='',lwd=2)

outcome <- t(sapply(ithim_object_list$uncertain$now$outcomes, function(x) colSums(x$hb$deaths[,(NSCEN+3):ncol(x$hb$deaths)])))

ithim_object <- ithim_object_list$uncertain$now
parameter_samples <- sapply(labs,function(x)ithim_object$parameters[[x]])
outcome <- t(sapply(ithim_object$outcomes, function(x) colSums(x$hb$deaths[,3:ncol(x$hb$deaths)])))
y <- rowSums(outcome[,seq(3,ncol(outcome),by=NSCEN)])
x <- parameter_samples[, 7];
plot(x,y,xlab='Street safety',ylab='Outcome')
    
x11(); boxplot(sapply(1:5,function(x)rowSums(outcome[,seq(x,ncol(outcome),by=NSCEN)])),frame=F,names=SCEN_SHORT_NAME[c(1,3:6)],ylab='Deaths: difference from Scenario 2',col='navyblue')
#points(1:5,sapply(1:5,function(x)sum(ithim_object_list$not_uncertain$now$outcomes$hb$deaths[,seq(2+x,ncol(ithim_object_list$not_uncertain$now$outcomes$hb$deaths),by=5)])),col='blue')

ithim_object <- ithim_object_list$uncertain$now
diff_outcome <- t(sapply(ithim_object$outcomes, function(x) colSums(x$hb$deaths[,ncol(ithim_object$outcomes[[1]]$hb$deaths)-4:0])))
x11(width=10); par(mfrow=c(1,2)); boxplot(diff_outcome,frame=F,names=SCEN_SHORT_NAME[c(1,3:6)],main='Injury death difference')

outcome <- t(sapply(ithim_object_list$uncertain$now$outcomes, function(x) sum(x$inj$deaths)))
total_outcome <- diff_outcome#t(repmat(outcome,5,1)) - diff_outcome
boxplot(total_outcome,frame=F,names=SCEN_SHORT_NAME[c(1,3:6)],main='Injury death total')

outcome <- t(sapply(ithim_object_list$uncertain$now$outcomes, function(x) colSums(x$hb$deaths[,(NSCEN+3):ncol(x$hb$deaths)])))
scen_out <- sapply(1:NSCEN,function(x)rowSums(outcome[,seq(x,ncol(outcome),by=NSCEN)]))
ninefive <- apply(scen_out,2,quantile,c(0.025,0.975))
{x11(height=3,width=6); par(mar=c(5,5,1,1))
plot(apply(scen_out,2,mean),1:NSCEN,pch=16,cex=1,frame=F,ylab='',xlab='Number of deaths averted relative to scenario 1',col='navyblue',yaxt='n',xlim=range(ninefive))
axis(2,las=2,at=1:NSCEN,labels=SCEN_SHORT_NAME[c(1,3:6)])
for(i in 1:NSCEN) lines(ninefive[,i],c(i,i),lwd=2,col='navyblue')
abline(v=0,col='grey',lty=2,lwd=2)
text(y=3,x=ninefive[1,3],'95%',col='navyblue',adj=c(-0,-0.7))
}


#########################################################################



######################################
# plot emission inventory
modes <- names(ithim_object$parameters$EMISSION_INVENTORY[[1]])
{x11(height=4,width=8); par(mar=c(5,5,2,2),mfrow=c(2,4))
for(m in 1:length(modes)){
  extract_dist <- sapply(ithim_object$parameters$EMISSION_INVENTORY,function(x)x[[m]])
  if(sum(extract_dist)!=0)
  plot(density(extract_dist,from=0,to=1),lwd=2,col='navyblue',xlim=c(0,1),main='',ylab='Density',xlab=modes[m],frame=F)
}
}


## plot confidences
confidences <- c(0.5,0.7,0.9)
parameters <- c(100,600,10000)
dists <- list()
emis <- list(car=4,motorbike=4,bus=20,hgv=60)
total <- sum(unlist(emis))
for(i in 1:length(confidences)){
  confidence <- confidences[i]
  samples <- sapply(emis,function(x) rgamma(1000,shape=x/total*exp((2.25*confidences+1)^2)[i],scale=1))
  new_total <- rowSums(samples)
  dists[[i]] <- apply(samples,2,function(x)x/new_total)
}
{x11(); par(mar=c(5,5,1,1),mfrow=c(2,2))
for(j in 1:ncol(dists[[1]])){
  for(i in 1:length(confidences)){
    if(i==1) {
      plot(density(dists[[i]][,j],from=0,to=1),lwd=2,typ='l',col=cols[i],frame=F,main='',xlab=names(emis)[j],ylim=c(0,20),cex.axis=1.5,cex.lab=1.5)
      if(j==1) legend(title='Confidence',legend=confidences,col=cols,bty='n',x=0.6,y=21,lty=1,lwd=2,cex=1.25)
    }
    else lines(density(dists[[i]][,j],from=0,to=1),lwd=2,typ='l',col=cols[i])
  }
}
}   
    
2=(sqrt(log(parameters))-1)/confidences
exp((2*confidences+1)^2)=sqrt(log(parameters))