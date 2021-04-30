#' Routine to sample or set parameters for ITHIM
#' 
#' Parameters have two options: to be set to a constant, and to be sampled from a prespecified distribution.
#' Each parameter is given as an argument of length 1 or 2. 
#' If length 1, it's constant, and set to the global environment. 
#' If length 2, a distribution is defined and sampled from NSAMPLE times.
#' There are some exceptions, listed below.
#' 
#' @param NSAMPLES constant integer: number of samples to take
#' @param BUS_WALK_TIME lognormal parameter: duration of walk to PT
#' @param MMET_CYCLING lognormal parameter: mMETs when cycling
#' @param MMET_WALKING lognormal parameter: mMETs when walking
#' @param PM_CONC_BASE lognormal parameter: background PM2.5 concentration
#' @param PM_TRANS_SHARE beta parameter: fraction of background PM2.5 attributable to transport
#' @param PA_DOSE_RESPONSE_QUANTILE logic: whether or not to sample from PA RR DR functions
#' @param AP_DOSE_RESPONSE_QUANTILE logic: whether or not to sample from AP RR DR functions
#' @param BACKGROUND_PA_SCALAR lognormal parameter: reporting scalar for PA
#' @param BACKGROUND_PA_CONFIDENCE beta parameter: confidence in accuracy of PA survey
#' @param INJURY_REPORTING_RATE lognormal parameter: rate of injury reporting
#' @param CHRONIC_DISEASE_SCALAR lognormal parameter: scalar for background disease rates
#' @param DAY_TO_WEEK_TRAVEL_SCALAR beta parameter: rate of scaling travel from one day to one week
#' @param SIN_EXPONENT_SUM lognormal parameter: linearity of injuries with respect to two modes. SIN_EXPONENT_SUM=2 means no safety in numbers.
#' @param CASUALTY_EXPONENT_FRACTION beta parameter: casualty contribution to SIN_EXPONENT_SUM
#' @param BUS_TO_PASSENGER_RATIO beta parameter: number of buses per passenger
#' @param TRUCK_TO_CAR_RATIO beta parameter: number of trucks per car
#' @param FLEET_TO_MOTORCYCLE_RATIO beta parameter: amount of motorcycle that's fleet
#' @param PM_EMISSION_INVENTORY_CONFIDENCE beta parameter: confidence in accuracy of emission inventory
#' @param DISTANCE_SCALAR_CAR_TAXI lognormal parameter: scalar for car distance travelled
#' @param DISTANCE_SCALAR_WALKING lognormal parameter: scalar for walking distance travelled
#' @param DISTANCE_SCALAR_PT lognormal parameter: scalar for PT distance travelled
#' @param DISTANCE_SCALAR_CYCLING lognormal parameter: scalar for cycling distance travelled
#' @param DISTANCE_SCALAR_MOTORCYCLE lognormal parameter: scalar for motorcycle distance travelled
#' 
#' @return list of samples of uncertain parameters
#' 
#' @export
ithim_setup_parameters <- function(NSAMPLES = 1,
                                   BUS_WALK_TIME= 5,
                                   MMET_CYCLING = 4.63,
                                   MMET_WALKING = 2.53,
                                   PM_CONC_BASE = 50,  
                                   PM_TRANS_SHARE = 0.225,
                                   PA_DOSE_RESPONSE_QUANTILE = F,
                                   AP_DOSE_RESPONSE_QUANTILE = F,
                                   BACKGROUND_PA_SCALAR = 1,
                                   BACKGROUND_PA_CONFIDENCE = 1,
                                   INJURY_REPORTING_RATE = 1,
                                   CHRONIC_DISEASE_SCALAR = 1,
                                   DAY_TO_WEEK_TRAVEL_SCALAR = 7,
                                   SIN_EXPONENT_SUM= 2,
                                   CASUALTY_EXPONENT_FRACTION = 0.5,
                                   BUS_TO_PASSENGER_RATIO = 0.022,
                                   TRUCK_TO_CAR_RATIO = 0.21,
                                   FLEET_TO_MOTORCYCLE_RATIO = 0,
                                   PM_EMISSION_INVENTORY_CONFIDENCE = 1,
                                   DISTANCE_SCALAR_CAR_TAXI = 1,
                                   DISTANCE_SCALAR_WALKING = 1,
                                   DISTANCE_SCALAR_PT = 1,
                                   DISTANCE_SCALAR_CYCLING = 1,
                                   DISTANCE_SCALAR_MOTORCYCLE = 1){
  
  if ((length(PM_CONC_BASE==1)&&PM_CONC_BASE == 50) |
      (length(PM_TRANS_SHARE==1)&&PM_TRANS_SHARE == 0.225))
  error_handling(1, "ithim_setup_parameters", "PM_CONC_BASE, PM_TRANS_SHARE")
  
  ## PARAMETERS
  ##RJ parameters are assigned to the environment and so are set for every function. They are over-written when sample_parameters is called.
  BUS_WALK_TIME <<- BUS_WALK_TIME
  MMET_CYCLING <<- MMET_CYCLING
  MMET_WALKING <<- MMET_WALKING
  PM_CONC_BASE <<- PM_CONC_BASE
  PM_TRANS_SHARE <<- PM_TRANS_SHARE
  PA_DOSE_RESPONSE_QUANTILE <<- PA_DOSE_RESPONSE_QUANTILE
  BACKGROUND_PA_SCALAR <<- BACKGROUND_PA_SCALAR
  BACKGROUND_PA_CONFIDENCE <<- BACKGROUND_PA_CONFIDENCE
  INJURY_REPORTING_RATE <<- INJURY_REPORTING_RATE
  CHRONIC_DISEASE_SCALAR <<- CHRONIC_DISEASE_SCALAR
  SIN_EXPONENT_SUM <<- SIN_EXPONENT_SUM
  CASUALTY_EXPONENT_FRACTION <<- CASUALTY_EXPONENT_FRACTION
  BUS_TO_PASSENGER_RATIO <<- BUS_TO_PASSENGER_RATIO
  TRUCK_TO_CAR_RATIO <<- TRUCK_TO_CAR_RATIO
  FLEET_TO_MOTORCYCLE_RATIO <<- FLEET_TO_MOTORCYCLE_RATIO
  DISTANCE_SCALAR_CAR_TAXI <<- DISTANCE_SCALAR_CAR_TAXI
  DISTANCE_SCALAR_WALKING <<- DISTANCE_SCALAR_WALKING
  DISTANCE_SCALAR_PT <<- DISTANCE_SCALAR_PT
  DISTANCE_SCALAR_CYCLING <<-  DISTANCE_SCALAR_CYCLING
  DISTANCE_SCALAR_MOTORCYCLE <<- DISTANCE_SCALAR_MOTORCYCLE
  parameters <- list()
  
  ##Variables with normal distribution
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
  for (i in 1:length(normVariables)) {
    name <- normVariables[i]
    val <- get(normVariables[i])
    if (length(val) == 1) {
      assign(name, val, envir = .GlobalEnv)
    } else {
      # Use mean and sd values in log form
      parameters[[name]] <-
        rlnorm(NSAMPLES, log(val[1]), log(val[2]))
    }
  }
  
  ##Variables with beta distribution
  betaVariables <- c("PM_TRANS_SHARE",
                     "INJURY_REPORTING_RATE",
                     "CASUALTY_EXPONENT_FRACTION",
                     "BUS_TO_PASSENGER_RATIO",
                     "TRUCK_TO_CAR_RATIO",
                     "FLEET_TO_MOTORCYCLE_RATIO")
  for (i in 1:length(betaVariables)) {
    name <- betaVariables[i]
    val <- get(betaVariables[i])
    if (length(val) == 1) {
      assign(name, val, envir = .GlobalEnv)
    } else {
      parameters[[name]] <-
        rbeta(NSAMPLES, val[1], val[2])
    }
  }
  
  if(length(DAY_TO_WEEK_TRAVEL_SCALAR) > 1 ){
    parameters$DAY_TO_WEEK_TRAVEL_SCALAR <- 7*rbeta(NSAMPLES,DAY_TO_WEEK_TRAVEL_SCALAR[1],DAY_TO_WEEK_TRAVEL_SCALAR[2])
  }else{
    DAY_TO_WEEK_TRAVEL_SCALAR <<- DAY_TO_WEEK_TRAVEL_SCALAR
  }
  
  if(BACKGROUND_PA_CONFIDENCE<1){
    parameters$BACKGROUND_PA_ZEROS <- runif(NSAMPLES,0,1)
  }
  
  if(PM_EMISSION_INVENTORY_CONFIDENCE<1){
    total <- sum(unlist(PM_EMISSION_INVENTORY))
    parameters$PM_EMISSION_INVENTORY <- list()
    for(n in 1:NSAMPLES){
      samples <- lapply(PM_EMISSION_INVENTORY,function(x) rgamma(1,shape=x/total*dirichlet_pointiness(PM_EMISSION_INVENTORY_CONFIDENCE),scale=1))
      new_total <- sum(unlist(samples))
      parameters$PM_EMISSION_INVENTORY[[n]] <- lapply(samples,function(x)x/new_total)
    }
  }
  
  ## PA DOSE RESPONSE
  if(PA_DOSE_RESPONSE_QUANTILE == T ) {
    pa_diseases <- subset(DISEASE_INVENTORY,physical_activity==1)
    dr_pa_list <- list()
    for(disease in pa_diseases$pa_acronym)
      parameters[[paste0('PA_DOSE_RESPONSE_QUANTILE_',disease)]] <- runif(NSAMPLES,0,1)
  }
  
  #### AP DOSE RESPONSE
  AP_DOSE_RESPONSE_QUANTILE <<- AP_DOSE_RESPONSE_QUANTILE
  ## shortcut: use saved median values
  if(!AP_DOSE_RESPONSE_QUANTILE){
    global_path <- file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/global/')
    global_path <- paste0(global_path, "/")
    DR_AP_LIST <<- readRDS(paste0(global_path,"dose_response/drap/dr_ap_list.Rds"))
  }else{
    dr_ap_list <- list()
    ap_diseases <- subset(DISEASE_INVENTORY,air_pollution==1)
    ap_parameters <- list()
    for(disease in ap_diseases$ap_acronym){ 
      for(letter in c('ALPHA_','BETA_','GAMMA_','TMREL_')){
        if(AP_DOSE_RESPONSE_QUANTILE){
          ap_parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_',letter,disease)]] <- runif(NSAMPLES,0,1)
          parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_',letter,disease)]] <- ap_parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_',letter,disease)]]
        } else {
          ap_parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_',letter,disease)]] <- 0.5
        }
      }
      dr_ap <- subset(DR_AP,cause_code==disease)
      dr_ap_list[[disease]] <- list()
      quant1 <- ap_parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_GAMMA_',disease)]]
      quant2 <- ap_parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_BETA_',disease)]]
      quant3 <- ap_parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_ALPHA_',disease)]]
      quant4 <- ap_parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_TMREL_',disease)]]
      for(age in unique(dr_ap$age_code)){
        dr_ap_age <- subset(dr_ap,age_code==age)
        #######################################
        lbeta <- log(dr_ap_age$beta)
        lgamma <- log(dr_ap_age$gamma)
        gamma_val <- quantile(density(lgamma),quant1)
        beta_val <- c()
        for(i in 1:ifelse(AP_DOSE_RESPONSE_QUANTILE,NSAMPLES,1)){
          den <- kde2d(lgamma,lbeta,n=c(1,100),h=0.2,lims=c(gamma_val[i],gamma_val[i],min(lbeta)-1,max(lbeta)+1))
          beta_val[i] <- approx(x=cumsum(den$z)/sum(den$z),y=den$y,xout=quant2[i])$y
        }
        mod <- gam(log(alpha)~te(log(gamma),log(beta)),data=dr_ap_age)
        pred_val <- predict(mod, newdata=data.frame(beta=exp(beta_val),gamma=exp(gamma_val)),se.fit=T)
        alpha_val <- qnorm(quant3,pred_val$fit,sqrt(mod$sig2))
        # generate a value for tmrel given alpha, beta and gamma
        mod <- gam(log(tmrel)~ns(log(gamma),df=8)+ns(log(beta),df=8)+ns(log(alpha),df=8),data=dr_ap_age)
        pred_val <- predict(mod, newdata=data.frame(alpha=exp(alpha_val),beta=exp(beta_val),gamma=exp(gamma_val)),se.fit=T)
        tmrel_val <- qnorm(quant4,pred_val$fit,sqrt(mod$sig2))
        dr_ap_list[[disease]][[as.character(age)]] <- data.frame(alpha=exp(alpha_val),beta=exp(beta_val),gamma=exp(gamma_val),tmrel=exp(tmrel_val))
      }
      if(AP_DOSE_RESPONSE_QUANTILE){ 
        # turn list inside out, so it's indexed first by sample
        parameters$DR_AP_LIST <- lapply(1:NSAMPLES,function(x)lapply(dr_ap_list,function(y) lapply(y,function(z)z[x,])))
      }else{
        DR_AP_LIST <<- dr_ap_list
      }
    }
    
  }
  parameters
}


#' Function for Dirichlet parameters
#' 
#' Function to map a confidence value to a parametrisation of a Dirichlet distribution
#' 
#' @param confidence value between 0 and 1
#' 
#' @return parametrisation
#' 
#' @export
dirichlet_pointiness <- function(confidence){
  exp((2.25*confidence+1)^2)
}