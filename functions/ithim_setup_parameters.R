# Modified by Ali Abbas
# Research Associate
# UKCRC Centre for Diet and Activity Research (CEDAR)
# MRC Epidemiology Unit
# University of Cambridge School of Clinical Medicine
# aa797 [AT] medschl [DOT] cam [DOT] ac [DOT] uk | http://www.cedar.iph.cam.ac.uk/people/cdfs/ali-abbas/

# Modified by Daniel Fuller
# Canada Research Chair in Population Physical Activity
# School of Human Kinetics and Recreation
# Memorial University of Newfoundland
# dfuller [AT] mun [DOT] ca | www.walkabilly.ca/home/

# Modified by Javad Rahimipour Anaraki on 08/11/18
# Ph.D. Candidate
# Department of Computer Science
# Memorial University of Newfoundland
# jra066 [AT] mun [DOT] ca | www.cs.mun.ca/~jra066

#   input:
#  output:

ithim_setup_parameters <- function(NSAMPLES = 1,
                                   MEAN_BUS_WALK_TIME = 5,
                                   MMET_CYCLING = 4.63,
                                   MMET_WALKING = 2.53,
                                   PM_CONC_BASE = 50,
                                   PM_TRANS_SHARE = 0.225,
                                   PA_DOSE_RESPONSE_QUANTILE = F,
                                   AP_DOSE_RESPONSE_QUANTILE = F,
                                   BACKGROUND_PA_SCALAR = 1,
                                   SAFETY_SCALAR = 1,
                                   CHRONIC_DISEASE_SCALAR = 1) {
  #=======================Function=========================
  source("error_handling.R")
  
  
  #=======================Warnings=========================
  if (PM_CONC_BASE == 50 |
      PM_TRANS_SHARE == 0.225)
    error_handling(1, "ithim_setup_parameters", "PM_CONC_BASE, PM_TRANS_SHARE")
  
  
  #=======================Variables========================
  ## PARAMETERS
  ##RJ parameters are assigned to the environment and so are set for every function. They are over-written when sample_parameters is called.
  parameters <- list()
  variables <- c("MEAN_BUS_WALK_TIME",
                 "MMET_CYCLING",
                 "MMET_WALKING",
                 "PM_CONC_BASE",
                 "PM_TRANS_SHARE",
                 "BACKGROUND_PA_SCALAR",
                 "SAFETY_SCALAR",
                 "CHRONIC_DISEASE_SCALAR")
  
  #==========================Main==========================
  for (i in 1:length(variables)) {
    name <- variables[i]
    val <- get(variables[i])
    if (length(val) == 1) {
      assign(name, val, envir = .GlobalEnv)
    } else {
      parameters[[name]] <-
        rlnorm(NSAMPLES, val[1], val[2])
    }
  }
  
  # if (length(MEAN_BUS_WALK_TIME) == 1) {
  #   MEAN_BUS_WALK_TIME <<- MEAN_BUS_WALK_TIME
  # }
  # else{
  #   parameters$MEAN_BUS_WALK_TIME <-
  #     rlnorm(NSAMPLES, MEAN_BUS_WALK_TIME[1], MEAN_BUS_WALK_TIME[2])
  # }
  # 
  # 
  # if (length(MMET_CYCLING) == 1) {
  #   MMET_CYCLING <<- MMET_CYCLING
  # } else{
  #   parameters$MMET_CYCLING <-
  #     rlnorm(NSAMPLES, MMET_CYCLING[1], MMET_CYCLING[2])
  # }
  # 
  # if (length(MMET_WALKING) == 1) {
  #   MMET_WALKING <<- MMET_WALKING
  # } else{
  #   parameters$MMET_WALKING <-
  #     rlnorm(NSAMPLES, MMET_WALKING[1], MMET_WALKING[2])
  # }
  # 
  # if (length(PM_CONC_BASE) == 1) {
  #   PM_CONC_BASE <<- PM_CONC_BASE
  # } else{
  #   parameters$PM_CONC_BASE <-
  #     rlnorm(NSAMPLES, PM_CONC_BASE[1], PM_CONC_BASE[2])
  # }
  # 
  # if (length(PM_TRANS_SHARE) == 1) {
  #   PM_TRANS_SHARE <<- PM_TRANS_SHARE
  # } else{
  #   parameters$PM_TRANS_SHARE <-
  #     rbeta(NSAMPLES, PM_TRANS_SHARE[1], PM_TRANS_SHARE[2])
  # }
  # 
  # if (length(BACKGROUND_PA_SCALAR) == 1) {
  #   BACKGROUND_PA_SCALAR <<- BACKGROUND_PA_SCALAR
  # } else{
  #   parameters$BACKGROUND_PA_SCALAR <-
  #     rlnorm(NSAMPLES, BACKGROUND_PA_SCALAR[1], BACKGROUND_PA_SCALAR[2])
  # }
  # 
  # if (length(SAFETY_SCALAR) == 1) {
  #   SAFETY_SCALAR <<- SAFETY_SCALAR
  # } else{
  #   parameters$SAFETY_SCALAR <-
  #     rlnorm(NSAMPLES, SAFETY_SCALAR[1], SAFETY_SCALAR[2])
  # }
  # 
  # if (length(CHRONIC_DISEASE_SCALAR) == 1) {
  #   CHRONIC_DISEASE_SCALAR <<- CHRONIC_DISEASE_SCALAR
  # } else{
  #   parameters$CHRONIC_DISEASE_SCALAR <-
  #     rlnorm(NSAMPLES,
  #            CHRONIC_DISEASE_SCALAR[1],
  #            CHRONIC_DISEASE_SCALAR[2])
  # }
  
  if (PA_DOSE_RESPONSE_QUANTILE == F) {
    PA_DOSE_RESPONSE_QUANTILE <<- PA_DOSE_RESPONSE_QUANTILE
  } else{
    pa_diseases <- subset(DISEASE_OUTCOMES, physical_activity == 1)
    dr_pa_list <- list()
    for (disease in pa_diseases$pa_acronym)
      parameters[[paste0('PA_DOSE_RESPONSE_QUANTILE_', disease)]] <-
      runif(NSAMPLES, 0, 1)
  }
  
  if (AP_DOSE_RESPONSE_QUANTILE == F) {
    AP_DOSE_RESPONSE_QUANTILE <<- AP_DOSE_RESPONSE_QUANTILE
    dr_ap_list <- list()
    for (j in 1:nrow(DISEASE_OUTCOMES))
      if (DISEASE_OUTCOMES$air_pollution[j] == 1) {
        cause <- as.character(DISEASE_OUTCOMES$ap_acronym[j])
        dr_ap <- subset(DR_AP, cause_code == cause)
        dr_ap_list[[cause]] <- list()
        for (age in unique(dr_ap$age_code)) {
          dr_ap_age <- subset(dr_ap, age_code == age)
          dr_ap_list[[cause]][[as.character(age)]] <-
            data.frame(
              alpha = mean(dr_ap_age$alpha),
              beta = mean(dr_ap_age$beta),
              gamma = mean(dr_ap_age$gamma),
              tmrel = mean(dr_ap_age$tmrel)
            )
        }
      }
    DR_AP_LIST <<- dr_ap_list
  } else{
    ap_diseases <- subset(DISEASE_OUTCOMES, air_pollution == 1)
    for (disease in ap_diseases$ap_acronym)
      for (letter in c('ALPHA_', 'BETA_', 'GAMMA_', 'TMREL_'))
        parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_', letter, disease)]] <-
          runif(NSAMPLES, 0, 1)
      dr_ap_list <- list()
      for (disease in ap_diseases$ap_acronym) {
        dr_ap <- subset(DR_AP, cause_code == disease)
        dr_ap_list[[disease]] <- list()
        for (age in unique(dr_ap$age_code)) {
          dr_ap_age <- subset(dr_ap, age_code == age)
          # generate a value for alpha
          alpha_val <-
            quantile(log(dr_ap_age$alpha), parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_ALPHA_', disease)]])
          # generate a value for beta given alpha
          mod <- gam(log(beta) ~ ns(log(alpha), df = 3), data = dr_ap_age)
          pred_val <-
            predict(mod,
                    newdata = data.frame(alpha = exp(alpha_val)),
                    se.fit = T)
          beta_val <-
            qnorm(parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_BETA_', disease)]], pred_val$fit, sqrt(mod$sig2))
          # generate a value for gamma given beta and alpha
          mod <-
            gam(log(gamma) ~ ns(log(beta), df = 3) + ns(log(alpha), df = 3), data =
                  dr_ap_age)
          pred_val <-
            predict(mod,
                    newdata = data.frame(
                      alpha = exp(alpha_val),
                      beta = exp(beta_val)
                    ),
                    se.fit = T)
          gamma_val <-
            qnorm(parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_GAMMA_', disease)]], pred_val$fit, sqrt(mod$sig2))
          # generate a value for tmrel given alpha, beta and gamma
          mod <-
            gam(log(tmrel) ~ ns(log(gamma), df = 3) + ns(log(beta), df = 3) + ns(log(alpha), df =
                                                                                   3),
                data = dr_ap_age)
          pred_val <-
            predict(
              mod,
              newdata = data.frame(
                alpha = exp(alpha_val),
                beta = exp(beta_val),
                gamma = exp(gamma_val)
              ),
              se.fit = T
            )
          tmrel_val <-
            qnorm(parameters[[paste0('AP_DOSE_RESPONSE_QUANTILE_TMREL_', disease)]], pred_val$fit, sqrt(mod$sig2))
          dr_ap_list[[disease]][[as.character(age)]] <-
            data.frame(
              alpha = exp(alpha_val),
              beta = exp(beta_val),
              gamma = exp(gamma_val),
              tmrel = exp(tmrel_val)
            )
        }
      }
      # turn list inside out, so it's indexed first by sample
      parameters$DR_AP_LIST <-
        lapply(1:NSAMPLES, function(x)
          lapply(dr_ap_list, function(y)
            lapply(y, function(z)
              z[x, ])))
  }
  parameters
}
