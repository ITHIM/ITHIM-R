context("test-ithimr")

test_that("accra basic", {
  # load saved result
  accra_results <- readRDS('accra_results.Rds')
  # generate new baseline accra results
  ithim_object <- run_ithim_setup(REFERENCE_SCENARIO='Scenario 1')
  ithim_object$outcomes <- run_ithim(ithim_object, seed = 1)
  result_mat <- colSums(ithim_object$outcome$hb$ylls[,3:ncol(ithim_object$outcome$hb$ylls)])
  # organise results to compare to 0. retain vector names.
  test_vals_1 <- abs(accra_results - result_mat)
  names(test_vals_1) <- names(accra_results)
  test_vals_1[accra_results>0] <- test_vals_1[accra_results>0]/accra_results[accra_results>0]
  test_vals_2 <- rep(0,length(result_mat))
  names(test_vals_2) <- names(result_mat)
  # test
  expect_equal(test_vals_1,test_vals_2,tolerance=0.05)
  
})

test_that("accra_test, walk scenario", {
  # load saved result
  accra_results <- readRDS('accra_walk_results.Rds')
  # generate new baseline accra results
  ithim_object <- run_ithim_setup( CITY='accra_test',
                                   TEST_WALK_SCENARIO=T,
                                   ADD_WALK_TO_BUS_TRIPS=F,
                                   ADD_TRUCK_DRIVERS = F,
                                   ADD_BUS_DRIVERS = F)
  ithim_object$outcomes <- run_ithim(ithim_object, seed = 1)
  result_mat <- colSums(ithim_object$outcome$hb$ylls[,3:ncol(ithim_object$outcome$hb$ylls)])
  # organise results to compare to 0. retain vector names.
  test_vals_1 <- abs(accra_results - result_mat)
  names(test_vals_1) <- names(accra_results)
  test_vals_1[accra_results>0] <- test_vals_1[accra_results>0]/accra_results[accra_results>0]
  test_vals_2 <- rep(0,length(result_mat))
  names(test_vals_2) <- names(result_mat)
  # test
  expect_equal(test_vals_1,test_vals_2,tolerance=0.05)
  
})

test_that("accra evppi", {
  ## this takes a long time: only test if you really want to
  test_evppi <- F
  if(test_evppi){
    # load saved result
    #accra_evppi <- readRDS('accra_evppi_test.Rds')
    # generate new accra evppi results
    environmental_scenario <- c('now')
    certainty <- 'uncertain'
    certainty_parameters <- list(uncertain=list(
      safey_scalar          = list(now=c(8,3)),
      disease_scalar        = list(now=c(0,log(1.2))),
      background_pm         = list(now=c(log(50),log(1.2))),
      transport_pm          = list(now=c(5,20)),
      background_pa_scalar  = list(now=c(0,log(1.2))),
      NSAMPLES = 1024,
      BUS_WALK_TIME = c(log(5), log(1.2)),
      MMET_CYCLING = c(log(5), log(1.2)), 
      MMET_WALKING = c(log(2.5), log(1.2)), 
      MC_TO_CAR_RATIO = c(-1.4,0.4),
      PA_DOSE_RESPONSE_QUANTILE = T,  
      AP_DOSE_RESPONSE_QUANTILE = T
    ))
    
    ithim_object <- run_ithim_setup(NSAMPLES = certainty_parameters[[certainty]]$NSAMPLES,
                                    BUS_WALK_TIME = certainty_parameters[[certainty]]$BUS_WALK_TIME,
                                    MMET_CYCLING = certainty_parameters[[certainty]]$MMET_CYCLING, 
                                    MMET_WALKING = certainty_parameters[[certainty]]$MMET_WALKING, 
                                    INJURY_REPORTING_RATE = certainty_parameters[[certainty]]$safey_scalar[[environmental_scenario]],  
                                    CHRONIC_DISEASE_SCALAR = certainty_parameters[[certainty]]$disease_scalar[[environmental_scenario]],  
                                    PM_CONC_BASE = certainty_parameters[[certainty]]$background_pm[[environmental_scenario]],  
                                    PM_TRANS_SHARE = certainty_parameters[[certainty]]$transport_pm[[environmental_scenario]],  
                                    BACKGROUND_PA_SCALAR = certainty_parameters[[certainty]]$background_pa_scalar[[environmental_scenario]],  
                                    MC_TO_CAR_RATIO = certainty_parameters[[certainty]]$MC_TO_CAR_RATIO,  
                                    PA_DOSE_RESPONSE_QUANTILE = certainty_parameters[[certainty]]$PA_DOSE_RESPONSE_QUANTILE,  
                                    AP_DOSE_RESPONSE_QUANTILE = certainty_parameters[[certainty]]$AP_DOSE_RESPONSE_QUANTILE)
    print(c(certainty,environmental_scenario))
    numcores <- detectCores()
    ithim_object$outcomes <- mclapply(1:NSAMPLES, FUN = ithim_uncertainty, ithim_object = ithim_object,mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
    ## calculate EVPPI
    parameter_names <- names(ithim_object$parameters)[names(ithim_object$parameters)!="DR_AP_LIST"]
    parameter_samples <- sapply(parameter_names,function(x)ithim_object$parameters[[x]])
    ## omit all-cause mortality
    outcome <- t(sapply(ithim_object$outcomes, function(x) colSums(x$hb$deaths[,3:ncol(x$hb$deaths)])))
    evppi <- matrix(0, ncol = NSCEN, nrow = ncol(parameter_samples))
    for(j in 1:(NSCEN)){
      y <- rowSums(outcome[,seq(NSCEN+j,ncol(outcome),by=NSCEN)])
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
    # test
    expect_equal(sum(evppi>0),95)
  }else{
    expect_equal(0,0)
  }
})
