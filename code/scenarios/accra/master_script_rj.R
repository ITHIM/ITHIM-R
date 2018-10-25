# setwd('~/overflow_dropbox/ITHIM-R/')
rm (list = ls())
source('ithim_r_functions.R')

#################################################
## Use case 1: basic ITHIM:

## 
ithim_object <- run_ithim_setup()
ithim_object$outcomes <- run_ithim(ithim_object, seed = 1)
print(names(ithim_object$outcome))
##

## plot results
result_mat <- colSums(ithim_object$outcome$hb$ylls[,3:ncol(ithim_object$outcome$hb$ylls)])
columns <- length(result_mat)
nDiseases <- columns/NSCEN
ylim <- range(result_mat)
x11(width = 8, height = 5); par(mfrow = c(2, 4))
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
}

#################################################
## Use case 2: environmental scenarios:

## assume already run:
# ithim_object <- run_ithim_setup()
ithim_object$outcome <- list()

## what if: cleaner fleet !! not yet implemented
ithim_object$parameters <- ithim_setup_parameters()
ithim_object$outcome$cleaner_fleet <- run_ithim(ithim_object, seed = 1)

## what if: the roads are safer
ithim_object$parameters <- ithim_setup_parameters(SAFETY_SCALAR = 0.5)
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
ithim_object <- run_ithim_setup(NSAMPLES = 16,
                                MEAN_BUS_WALK_TIME = c(log(5), log(1.2)),
                                MMET_CYCLING = c(log(5), log(1.2)), 
                                PM_CONC_BASE = c(log(50), log(1.2)),  
                                PM_TRANS_SHARE = c(5, 5),  
                                PA_DOSE_RESPONSE_QUANTILE = T,  
                                AP_DOSE_RESPONSE_QUANTILE = T)

numcores <- detectCores()
ithim_object$outcomes <- mclapply(1:NSAMPLES, FUN = ithim_uncertainty, ithim_obj = ithim_object, mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))

## calculate EVPPI
parameter_names <- names(ithim_object$parameters)[names(ithim_object$parameters)!="DR_AP_LIST"]
parameter_samples <- sapply(parameter_names,function(x)ithim_object$parameters[[x]])
outcome <- t(sapply(ithim_object$outcomes, function(x) colSums(x$hb$ylls[,3:ncol(x$hb$ylls)])))
evppi <- matrix(0, ncol = NSCEN, nrow = ncol(parameter_samples))
for(j in 1:(NSCEN)){
  y <- outcome[, j+5] ## +5 means we choose ihd outcome for each scenario
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
  evppi_for_AP <- mclapply(diseases, FUN = parallel_evppi_for_AP,parameter_samples,outcome, mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
  names(evppi_for_AP) <- paste0('AP_DOSE_RESPONSE_QUANTILE_',diseases)
  evppi <- rbind(evppi,do.call(rbind,evppi_for_AP))
}
print(evppi)

#################################################
## Use case 4: Application: six behavioural scenarios and five environmental scenarios.
## sample size, travel patterns, emissions (cleaner fleet)

environmental_scenarios <- c('now','safer','more_chronic_disease','less_background_AP','less_background_PA')
certainty_parameters <- list(uncertain=list(
  safey_scalar          = list(now=c(0,log(1.2)),      safer=0.5,                more_chronic_disease=c(0,log(1.2)),      less_background_AP=c(0,log(1.2)),less_background_PA=c(0,log(1.2))),
  disease_scalar        = list(now=c(0,log(1.2)),      safer=c(0,log(1.2)),      more_chronic_disease=2.0,                less_background_AP=c(0,log(1.2)),less_background_PA=c(0,log(1.2))),
  background_pm         = list(now=c(log(50),log(1.2)),safer=c(log(50),log(1.2)),more_chronic_disease=c(log(50),log(1.2)),less_background_AP=30.625,       less_background_PA=c(log(50),log(1.2))),
  transport_pm          = list(now=c(5,5),             safer=c(5,5),             more_chronic_disease=c(5,5),             less_background_AP=0.3673469,    less_background_PA=c(5,5)),
  background_pa_scalar  = list(now=c(0,log(1.2)),      safer=c(0,log(1.2)),      more_chronic_disease=c(0,log(1.2)),      less_background_AP=c(0,log(1.2)),less_background_PA=0.5),
  NSAMPLES = 1024,
  MEAN_BUS_WALK_TIME = c(log(5), log(1.2)),
  MMET_CYCLING = c(log(5), log(1.2)), 
  MMET_WALKING = c(log(2.5), log(1.2)), 
  PA_DOSE_RESPONSE_QUANTILE = T,  
  AP_DOSE_RESPONSE_QUANTILE = T
), not_uncertain=list(
  safey_scalar          = list(now=1,    safer=0.5,  more_chronic_disease=1,    less_background_AP=1,        less_background_PA=1),
  disease_scalar        = list(now=1,    safer=1,    more_chronic_disease=2.0,  less_background_AP=1,        less_background_PA=1),
  background_pm         = list(now=50,   safer=50,   more_chronic_disease=50,   less_background_AP=30.625,   less_background_PA=50),
  transport_pm          = list(now=0.225,safer=0.225,more_chronic_disease=0.225,less_background_AP=0.3673469,less_background_PA=0.225),
  background_pa_scalar  = list(now=1,    safer=1,    more_chronic_disease=1,    less_background_AP=1,        less_background_PA=0.5),
  NSAMPLES = 1,
  MEAN_BUS_WALK_TIME = 5,
  MMET_CYCLING = 4.63, 
  MMET_WALKING = 2.53, 
  PA_DOSE_RESPONSE_QUANTILE = F,  
  AP_DOSE_RESPONSE_QUANTILE = F
))

file_name <- paste0('six_by_five_scenarios_',certainty_parameters$uncertain$NSAMPLES,'.Rds')
if(file.exists(file_name)){
  ithim_object_list <- readRDS(file_name)
}else{
  numcores <- detectCores()
  ithim_object_list <- list()
  for(certainty in c('not_uncertain','uncertain')){
    ithim_object_list[[certainty]] <- list()
    for(environmental_scenario in environmental_scenarios){
      
      ithim_object <- run_ithim_setup(NSAMPLES = certainty_parameters[[certainty]]$NSAMPLES,
                                      MEAN_BUS_WALK_TIME = certainty_parameters[[certainty]]$MEAN_BUS_WALK_TIME,
                                      MMET_CYCLING = certainty_parameters[[certainty]]$MMET_CYCLING, 
                                      MMET_WALKING = certainty_parameters[[certainty]]$MMET_WALKING, 
                                      SAFETY_SCALAR = certainty_parameters[[certainty]]$safey_scalar[[environmental_scenario]],  
                                      CHRONIC_DISEASE_SCALAR = certainty_parameters[[certainty]]$disease_scalar[[environmental_scenario]],  
                                      PM_CONC_BASE = certainty_parameters[[certainty]]$background_pm[[environmental_scenario]],  
                                      PM_TRANS_SHARE = certainty_parameters[[certainty]]$transport_pm[[environmental_scenario]],  
                                      BACKGROUND_PA_SCALAR = certainty_parameters[[certainty]]$background_pa_scalar[[environmental_scenario]],  
                                      PA_DOSE_RESPONSE_QUANTILE = certainty_parameters[[certainty]]$PA_DOSE_RESPONSE_QUANTILE,  
                                      AP_DOSE_RESPONSE_QUANTILE = certainty_parameters[[certainty]]$AP_DOSE_RESPONSE_QUANTILE)
      
      if(certainty=='not_uncertain'){
        ithim_object$outcomes <- run_ithim(ithim_object, seed = 1)
      }else if(certainty=='uncertain'){
        ithim_object$outcomes <- mclapply(1:NSAMPLES, FUN = ithim_uncertainty, ithim_obj = ithim_object,mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
        ## calculate EVPPI
        parameter_names <- names(ithim_object$parameters)[names(ithim_object$parameters)!="DR_AP_LIST"]
        parameter_samples <- sapply(parameter_names,function(x)ithim_object$parameters[[x]])
        outcome <- t(sapply(ithim_object$outcomes, function(x) colSums(x$hb$deaths[,3:ncol(x$hb$deaths)])))
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
          evppi_for_AP <- mclapply(diseases, FUN = parallel_evppi_for_AP,parameter_samples,outcome, mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
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
  saveRDS(ithim_object_list,file_name)
}

library(RColorBrewer)
library(plotrix)

file_name <- paste0('six_by_five_scenarios_1024.Rds')
ithim_object_list <- readRDS(file_name)
evppi <- ithim_object_list$uncertain$now$evppi


x11(width=5); par(mar=c(6,11.5,3.5,5))
parameter_names <- c('walk-to-bus time','cycling mMETs','walking mMETs','background PM2.5','traffic PM2.5 share',
                     'non-travel PA','street safety','non-communicable disease burden','all-cause mortality (PA)','IHD (PA)',
                     'cancer (PA)','lung cancer (PA)','stroke (PA)','diabetes (PA)','IHD (AP)','lung cancer (AP)',
                     'COPD (AP)','stroke (AP)')
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

x11(width=8,height=5); par(mfrow=c(2,4),mar=c(5,1,1,1)); 
for(i in 1:8)  plot(density(ithim_object_list$uncertain$now$parameters[[i]]),xlab=names(ithim_object_list$uncertain$now$parameters)[i],ylab='',frame=F,main='',lwd=2)


x11(); boxplot(sapply(1:5,function(x)rowSums(outcome[,seq(x,ncol(outcome),by=NSCEN)])))
points(1:5,sapply(1:5,function(x)sum(ithim_object_list$not_uncertain$now$outcomes$hb$deaths[,seq(2+x,ncol(ithim_object_list$not_uncertain$now$outcomes$hb$deaths),by=5)])),col='blue')

