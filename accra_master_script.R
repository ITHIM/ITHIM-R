rm (list = ls())
library(ithimr)
library(earth)
#################################################
## Use case 0: basic ITHIM, walk scenario:

## 
ithim_object <- run_ithim_setup(TEST_WALK_SCENARIO=T,ADD_WALK_TO_BUS_TRIPS=F,CITY='accra',ADD_TRUCK_DRIVERS = F,ADD_BUS_DRIVERS = F)
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
ithim_object <- run_ithim_setup(DIST_CAT = c("0-1 km", "2-5 km", "6+ km"),ADD_WALK_TO_BUS_TRIPS=F,CITY='accra',ADD_TRUCK_DRIVERS = F,ADD_BUS_DRIVERS = F,MAX_MODE_SHARE_SCENARIO = T)
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
                                MMET_WALKING = c((2.53),(1.2)), 
                                MMET_CYCLING = c((4.63),(1.2)), 
                                PM_CONC_BASE = c((50), (1.3)),  
                                PM_TRANS_SHARE = c(5,20), 
                                INJURY_REPORTING_RATE = c(8,3), 
                                CHRONIC_DISEASE_SCALAR = c((1), (1.2)),  
                                BACKGROUND_PA_SCALAR = c((1), (1.2)),   
                                BUS_TO_PASSENGER_RATIO = c(20,600),
                                TRUCK_TO_CAR_RATIO = c(3,10),
                                DISTANCE_SCALAR_CAR_TAXI = c(1,(1.2)),
                                DISTANCE_SCALAR_MOTORCYCLE = c(1,(1.2)),
                                DISTANCE_SCALAR_WALKING = c(1,(1.2)),
                                DISTANCE_SCALAR_CYCLING = c(1,(1.2)),
                                DISTANCE_SCALAR_PT = c(1,(1.2)),
                                PA_DOSE_RESPONSE_QUANTILE = T,  
                                AP_DOSE_RESPONSE_QUANTILE = T,
                                DAY_TO_WEEK_TRAVEL_SCALAR = 7,#c(20,3),
                                SIN_EXPONENT_SUM= c((1.5),(1.1)),
                                CASUALTY_EXPONENT_FRACTION = c(15,15),
                                EMISSION_INVENTORY_CONFIDENCE = 0.5,
                                BACKGROUND_PA_CONFIDENCE = 0.5)

numcores <- 4#detectCores()
run_ithim(ithim_object,1)
ithim_object$outcomes <- mclapply(1:NSAMPLES, FUN = run_ithim, ithim_object = ithim_object, mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
for(i in 1:NSAMPLES) print(length(ithim_object$outcomes[[i]]))

plot(ithim_object$parameters$AP_DOSE_RESPONSE_QUANTILE_GAMMA_cvd_ihd,sapply(ithim_object$outcomes,function(x)sum(x$hb$deaths[,10])))

## gather outcome
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


## calculate EVPPI

compute_single_evppi <- function(sourcesj,case,nscen=1){
  # initialise vector
  voi <- rep(0,nscen)
  max_degree <- ifelse(is.vector(sourcesj),1,ncol(sourcesj))
  indices <- 1
  # loop over all scenarios
  for(k in 1:nscen){
    # extract scenario values and sum
    scen_case <- case[,seq(k,ncol(case),by=nscen)]
    y <- rowSums(scen_case)
    # compute outcome variance
    vary <- var(y)
    # model outcome as a function of input(s)
    model <- earth(y ~ sourcesj, degree=min(4,max_degree))
    # compute evppi as percentage
    voi[k] <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100
  }
  voi
}


##!! assume there is more than one outcome
outcomes <- list(outcome,outcome)

evppi <- matrix(0,nrow=ncol(parameter_samples),ncol=length(outcomes)*NSCEN)
colnames(evppi) <- apply(expand.grid(SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)],names(outcome)),1,function(x)paste0(x,collapse='_'))
rownames(evppi) <- colnames(parameter_samples)

for(i in 1:length(outcomes)){
  outcome <- outcomes[[i]]
  for(j in 1:ncol(parameter_samples)){
    sourcesj <- parameter_samples[,j]
    evppi[j,(i-1)*NSCEN + 1:NSCEN] <- compute_single_evppi(sourcesj=sourcesj,case=outcome,nscen=NSCEN)
  }
}



#evppi <- mclapply(1:ncol(parameter_samples), 
#         FUN = ithimr:::compute_evppi,
#         as.data.frame(parameter_samples),
#         list(outcome), 
#         nscen=NSCEN,
#         all=T,
#         multi_city_outcome=F,
#         mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
#evppi <- do.call(rbind,evppi)


## add four-dimensional EVPPI if AP_DOSE_RESPONSE is uncertain.

if(any(grepl('AP_DOSE_RESPONSE_QUANTILE_ALPHA',parameter_names))&&NSAMPLES>=1024){
  AP_names <- sapply(colnames(parameter_samples),function(x)length(strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA')[[1]])>1)
  diseases <- sapply(colnames(parameter_samples)[AP_names],function(x)strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA_')[[1]][2])
  sources <- list()
  for(di in diseases){
    col_names <- sapply(colnames(parameter_samples),function(x)grepl('AP_DOSE_RESPONSE_QUANTILE',x)&grepl(di,x))
    sources[[di]] <- parameter_samples[,col_names]
  }
  evppi_for_AP <- matrix(0,nrow=length(sources),ncol=length(outcomes)*NSCEN)
  rownames(evppi_for_AP) <- paste0('AP_DOSE_RESPONSE_QUANTILE_',diseases)
  for(i in 1:length(outcomes)){
    outcome <- outcomes[[i]]
    for(j in 1:length(sources)){
      sourcesj <- sources[[j]]
      evppi_for_AP[j,(i-1)*NSCEN + 1:NSCEN] <- compute_single_evppi(sourcesj=sourcesj,case=outcome,nscen=NSCEN)
    }
  }
  
  #evppi_for_AP <- mclapply(1:length(sources), 
  #                         FUN = ithimr:::compute_evppi,
  #                         sources,
  #                         list(outcome), 
  #                         nscen=NSCEN,
  #                         all=T,
  #                         multi_city_outcome=F,
  #                         mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
  #names(evppi_for_AP) <- paste0('AP_DOSE_RESPONSE_QUANTILE_',diseases)
  #evppi <- rbind(evppi,do.call(rbind,evppi_for_AP))
  
  
  evppi <- rbind(evppi,evppi_for_AP)
  ## get rows to remove
  keep_names <- sapply(rownames(evppi),function(x)!any(c('ALPHA','BETA','GAMMA','TMREL')%in%strsplit(x,'_')[[1]]))
  evppi <- evppi[keep_names,]
}

##!! not tested
if("EMISSION_INVENTORY"%in%colnames(parameter_samples)&&NSAMPLES>=1024){
  emission_names <- sapply(colnames(parameter_samples),function(x)grepl('EMISSION_INVENTORY_',x))
  sources <- parameter_samples[,emission_names]
  
  evppi_for_emissions <- matrix(0,nrow=1,ncol=length(outcomes)*NSCEN)
  rownames(evppi_for_emissions) <- 'EMISSION_INVENTORY'
  for(i in 1:length(outcomes)){
    outcome <- outcomes[[i]]
    sourcesj <- sources
    evppi_for_emissions[1,(i-1)*NSCEN + 1:NSCEN] <- compute_single_evppi(sourcesj=sourcesj,case=outcome,nscen=NSCEN)
  }
  #evppi_for_emissions <- mclapply(1:length(sources),
  #                                FUN = ithimr:::compute_evppi,
  #                                sources,
  #                                list(outcome),
  #                                nscen=NSCEN,
  #                                multi_city_outcome=F,
  #                                mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
  #names(evppi_for_emissions) <- paste0('EMISSION_INVENTORY')
  ## get rows to remove
  keep_names <- sapply(rownames(evppi),function(x)!grepl('EMISSION_INVENTORY_',x))
  evppi <- evppi[keep_names,]
  
  #evppi <- rbind(evppi,do.call(rbind,evppi_for_emissions))
  evppi <- rbind(evppi,evppi_for_emissions)
}

if(sum(c("BACKGROUND_PA_SCALAR","BACKGROUND_PA_ZEROS")%in%colnames(parameter_samples))==2&&NSAMPLES>=1024){
    pa_names <- sapply(colnames(parameter_samples),function(x)(grepl('BACKGROUND_PA_SCALAR',x)||grepl('BACKGROUND_PA_ZEROS',x)))
    sources <- parameter_samples[,pa_names]
    
    evppi_for_pa <- matrix(0,nrow=1,ncol=length(outcomes)*NSCEN)
    rownames(evppi_for_pa) <- 'BACKGROUND_PA'
    for(i in 1:length(outcomes)){
      outcome <- outcomes[[i]]
      sourcesj <- sources
      evppi_for_pa[1,(i-1)*NSCEN + 1:NSCEN] <- compute_single_evppi(sourcesj=sourcesj,case=outcome,nscen=NSCEN)
    }
  #evppi_for_pa <- compute_evppi(1,list(sources), 
  #                         list(outcome), 
  #                         nscen=NSCEN,
  #                         multi_city_outcome=F)
  
  #names(evppi_for_pa) <- 'BACKGROUND_PA'
  ## get rows to remove
  keep_names <- sapply(rownames(evppi),function(x)!grepl('BACKGROUND_PA_',x))
  evppi <- evppi[keep_names,]
  
  #evppi <- rbind(evppi,do.call(rbind,evppi_for_pa))
  evppi <- rbind(evppi,evppi_for_pa)
}





print(evppi)
{
  pdf('accra_walk_evppi.pdf',height=10)#x11(height=10); 
  par(mar=c(5,25,2,2)); 
  barplot(evppi,horiz = T,xlab='EVPPI, %',cex.axis=1.5,cex.lab=1.5,beside = T,names.arg=rownames(evppi),las=2)
  dev.off()
}

#################################################
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
emis <- list(car=4,motorcycle=4,bus=20,hgv=60)
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