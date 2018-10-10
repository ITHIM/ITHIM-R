# setwd('~/overflow_dropbox/ITHIM-R/')
rm (list = ls())
source('ithim_r_functions.R')

#################################################
## Use case 1: basic ITHIM:

## 
ithim_object <- run_ithim_setup()
ithim_object$outcome <- run_ithim(ithim_object, seed = 1)
##

## plot results
result_mat <- colSums(ithim_object$outcome$ylls[,3:ncol(ithim_object$outcome$ylls)])
columns <- length(result_mat)
nDiseases <- (columns-1)/NSCEN
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
## Use case 2: what-if scenarios:

## assume already run:
# ithim_object <- run_ithim_setup()
ithim_object$outcome <- list()

## what if: cleaner fleet
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
result_list <- lapply(ithim_object$outcome,function(x)colSums(x$ylls[,3:ncol(x$ylls)]))
result_mat <- do.call(rbind, result_list)
columns <- ncol(result_mat)
nDiseases <- (columns-1)/NSCEN
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
                                MMET_CYCLING = c(log(5), 1), 
                                PM_CONC_BASE = c(log(50), 1),  
                                PM_TRANS_SHARE = c(5, 5),  
                                PA_DOSE_RESPONSE_QUANTILE = c(0,1),  
                                AP_DOSE_RESPONSE_QUANTILE = c(0,1))

numcores <- detectCores()
results <- mclapply(1:NSAMPLES, FUN = ithim_uncertainty, ithim_object = ithim_object, mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))

## get EVPPI
parameter_samples <- t(sapply(results, function(x) x$parameter_samples))
outcome <- t(sapply(results, function(x) colSums(x$outcome$ylls[,3:ncol(x$outcome$ylls)])))
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
  AP_DOSE_RESPONSE_QUANTILE <- c()
  for(j in 1:(NSCEN)){
    y <- outcome[, j+5] ## +5 means we choose ihd outcome for each scenario
    vary <- var(y)
    x1 <- parameter_samples[,which(colnames(parameter_samples)=='AP_DOSE_RESPONSE_QUANTILE_ALPHA')];
    x2 <- parameter_samples[,which(colnames(parameter_samples)=='AP_DOSE_RESPONSE_QUANTILE_BETA')];
    x3 <- parameter_samples[,which(colnames(parameter_samples)=='AP_DOSE_RESPONSE_QUANTILE_GAMMA')];
    x4 <- parameter_samples[,which(colnames(parameter_samples)=='AP_DOSE_RESPONSE_QUANTILE_TMREL')];
    model <- gam(y ~ te(x1,x2,x3,x4))
    AP_DOSE_RESPONSE_QUANTILE[j] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100
  }
  evppi <- rbind(evppi,AP_DOSE_RESPONSE_QUANTILE)
}
print(evppi)

