#' @export
ithim_setup_parameters <- function(NSAMPLES = 1,
                                   BUS_WALK_TIME= 5,
                                   MMET_CYCLING = 4.63,
                                   MMET_WALKING = 2.53,
                                   PM_CONC_BASE = 50,  
                                   PM_TRANS_SHARE = 0.225,
                                   MC_TO_CAR_RATIO = 0.2,
                                   PA_DOSE_RESPONSE_QUANTILE = F,
                                   AP_DOSE_RESPONSE_QUANTILE = F,
                                   BACKGROUND_PA_SCALAR = 1,
                                   INJURY_REPORTING_RATE = 1,
                                   CHRONIC_DISEASE_SCALAR = 1 ){
  ## PARAMETERS
  ##RJ parameters are assigned to the environment and so are set for every function. They are over-written when sample_parameters is called.
  BUS_WALK_TIME <<- BUS_WALK_TIME
  MMET_CYCLING <<- MMET_CYCLING
  MMET_WALKING <<- MMET_WALKING
  PM_CONC_BASE <<- PM_CONC_BASE
  BACKGROUND_PA_SCALAR <<- BACKGROUND_PA_SCALAR
  PM_TRANS_SHARE <<- PM_TRANS_SHARE
  MC_TO_CAR_RATIO <<- MC_TO_CAR_RATIO
  INJURY_REPORTING_RATE <<- INJURY_REPORTING_RATE
  CHRONIC_DISEASE_SCALAR <<- CHRONIC_DISEASE_SCALAR
  PA_DOSE_RESPONSE_QUANTILE <<- PA_DOSE_RESPONSE_QUANTILE
  parameters <- list()
  if(length(BUS_WALK_TIME) > 1 )    parameters$BUS_WALK_TIME <- rlnorm(NSAMPLES,BUS_WALK_TIME[1], BUS_WALK_TIME[2])
  if(length(MMET_CYCLING) > 1 )     parameters$MMET_CYCLING <- rlnorm(NSAMPLES,MMET_CYCLING[1], MMET_CYCLING[2])
  if(length(MMET_WALKING) > 1 )     parameters$MMET_WALKING <- rlnorm(NSAMPLES,MMET_WALKING[1], MMET_WALKING[2])
  if(length(PM_CONC_BASE) > 1 )     parameters$PM_CONC_BASE <- rlnorm(NSAMPLES,PM_CONC_BASE[1],PM_CONC_BASE[2])
  if(length(PM_TRANS_SHARE) > 1 )   parameters$PM_TRANS_SHARE <- rbeta(NSAMPLES,PM_TRANS_SHARE[1],PM_TRANS_SHARE[2])
  if(length(MC_TO_CAR_RATIO) > 1 )  parameters$MC_TO_CAR_RATIO <- rlnorm(NSAMPLES,MC_TO_CAR_RATIO[1],MC_TO_CAR_RATIO[2])
  if(length(BACKGROUND_PA_SCALAR) > 1 )     parameters$BACKGROUND_PA_SCALAR <- rlnorm(NSAMPLES,BACKGROUND_PA_SCALAR[1],BACKGROUND_PA_SCALAR[2])
  if(length(INJURY_REPORTING_RATE) > 1 )    parameters$INJURY_REPORTING_RATE <- rbeta(NSAMPLES,INJURY_REPORTING_RATE[1],INJURY_REPORTING_RATE[2])
  if(length(CHRONIC_DISEASE_SCALAR) > 1 )   parameters$CHRONIC_DISEASE_SCALAR <- rlnorm(NSAMPLES,CHRONIC_DISEASE_SCALAR[1],CHRONIC_DISEASE_SCALAR[2])
  if(PA_DOSE_RESPONSE_QUANTILE == T ) {
    pa_diseases <- subset(DISEASE_INVENTORY,physical_activity==1)
    dr_pa_list <- list()
    for(disease in pa_diseases$pa_acronym)
      parameters[[paste0('PA_DOSE_RESPONSE_QUANTILE_',disease)]] <- runif(NSAMPLES,0,1)
  }
  if(AP_DOSE_RESPONSE_QUANTILE == F ) {
    AP_DOSE_RESPONSE_QUANTILE <<- AP_DOSE_RESPONSE_QUANTILE
    dr_ap_list <- list()
    for ( j in 1:nrow(DISEASE_INVENTORY)) if (DISEASE_INVENTORY$air_pollution[j] == 1){ 
      cause <- as.character(DISEASE_INVENTORY$ap_acronym[j])
      dr_ap <- subset(DR_AP,cause_code==cause)
      dr_ap_list[[cause]] <- list()
      for(age in unique(dr_ap$age_code)){
        dr_ap_age <- subset(dr_ap,age_code==age)
        dr_ap_list[[cause]][[as.character(age)]] <- data.frame(alpha=mean(dr_ap_age$alpha),beta=mean(dr_ap_age$beta),gamma=mean(dr_ap_age$gamma),tmrel=mean(dr_ap_age$tmrel))
      }
    }
    DR_AP_LIST <<- dr_ap_list
  }else{
    ap_diseases <- subset(DISEASE_INVENTORY,air_pollution==1)
    for(disease in ap_diseases$ap_acronym)
      for(letter in c('ALPHA_','BETA_','GAMMA_','TMREL_'))
        parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_',letter,disease)]] <- runif(NSAMPLES,0,1)
      dr_ap_list <- list()
      for(disease in ap_diseases$ap_acronym){ 
        dr_ap <- subset(DR_AP,cause_code==disease)
        dr_ap_list[[disease]] <- list()
        quant1 <- parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_GAMMA_',disease)]]
        quant2 <- parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_BETA_',disease)]]
        quant3 <- parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_ALPHA_',disease)]]
        quant4 <- parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_TMREL_',disease)]]
        for(age in unique(dr_ap$age_code)){
          dr_ap_age <- subset(dr_ap,age_code==age)
          #######################################
          lbeta <- log(dr_ap_age$beta)
          lgamma <- log(dr_ap_age$gamma)
          gamma_val <- quantile(density(lgamma),quant1)
          beta_val <- c()
          for(i in 1:NSAMPLES){
            den <- kde2d(lgamma,lbeta,n=c(1,100),h=0.2,lims=c(gamma_val[i],gamma_val[i],min(lbeta)-1,max(lbeta)+1))
            beta_val[i] <- approx(x=cumsum(den$z)/sum(den$z),y=den$y,xout=quant2[i])$y
          }
          mod <- gam(log(alpha)~te(log(gamma),log(beta)),data=dr_ap_age)
          pred_val <- predict(mod, newdata=data.frame(beta=exp(beta_val),gamma=exp(gamma_val)),se.fit=T)
          alpha_val <- qnorm(quant3,pred_val$fit,sqrt(mod$sig2))
          #######################################
          
          # generate a value for alpha
          #alpha_val <- quantile(log(dr_ap_age$alpha),parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_ALPHA_',disease)]])
          # generate a value for beta given alpha
          #mod <- gam(log(beta)~ns(log(alpha),df=8),data=dr_ap_age)
          #pred_val <- predict(mod, newdata=data.frame(alpha=exp(alpha_val)),se.fit=T)
          #beta_val <- qnorm(parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_BETA_',disease)]],pred_val$fit,sqrt(mod$sig2))
          # generate a value for gamma given beta and alpha
          #mod <- gam(log(gamma)~ns(log(beta),df=8)+ns(log(alpha),df=8),data=dr_ap_age)
          #pred_val <- predict(mod, newdata=data.frame(alpha=exp(alpha_val),beta=exp(beta_val)),se.fit=T)
          #gamma_val <- qnorm(parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_GAMMA_',disease)]],pred_val$fit,sqrt(mod$sig2))
          
          # generate a value for tmrel given alpha, beta and gamma
          mod <- gam(log(tmrel)~ns(log(gamma),df=8)+ns(log(beta),df=8)+ns(log(alpha),df=8),data=dr_ap_age)
          pred_val <- predict(mod, newdata=data.frame(alpha=exp(alpha_val),beta=exp(beta_val),gamma=exp(gamma_val)),se.fit=T)
          tmrel_val <- qnorm(parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_TMREL_',disease)]],pred_val$fit,sqrt(mod$sig2))
          dr_ap_list[[disease]][[as.character(age)]] <- data.frame(alpha=exp(alpha_val),beta=exp(beta_val),gamma=exp(gamma_val),tmrel=exp(tmrel_val))
        }
      }
      # turn list inside out, so it's indexed first by sample
      parameters$DR_AP_LIST <- lapply(1:NSAMPLES,function(x)lapply(dr_ap_list,function(y) lapply(y,function(z)z[x,])))
  }
  parameters
}
