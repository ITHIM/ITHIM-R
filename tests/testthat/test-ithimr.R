context("test-ithimr")

test_that("accra basic", {
  # load saved result
  accra_results <- readRDS('accra_results.Rds')
  # generate new baseline accra results
  ithim_object <- run_ithim_setup(REFERENCE_SCENARIO='Scenario 1')
  ithim_object$outcomes <- run_ithim(ithim_object, seed = 1)
  result_mat <- colSums(ithim_object$outcome$hb$ylls[,3:ncol(ithim_object$outcome$hb$ylls)])
  # test
  expect_equal(result_mat,accra_results,tolerance=0.1)
  
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
  # test
  expect_equal(accra_results,result_mat,tolerance=0.1)
  
})

test_that("accra uncertain parallel", {
  # load saved result
  #accra_evppi <- readRDS('accra_evppi_test.Rds')
  # generate new accra evppi results
  
  ithim_object <- run_ithim_setup(REFERENCE_SCENARIO='Scenario 1',
                                  NSAMPLES = 16,
                                  BUS_WALK_TIME = c(log(5), log(1.2)),
                                  MMET_CYCLING = c(log(5), log(1.2)), 
                                  MMET_WALKING = c(log(2.5), log(1.2)), 
                                  INJURY_REPORTING_RATE = c(8,3),  
                                  CHRONIC_DISEASE_SCALAR = c(0,log(1.2)),  
                                  PM_CONC_BASE = c(log(50),log(1.2)),  
                                  PM_TRANS_SHARE = c(5,20),  
                                  BACKGROUND_PA_SCALAR = c(0,log(1.2)),  
                                  MOTORCYCLE_TO_CAR_RATIO = c(-1.4,0.4),  
                                  DAY_TO_WEEK_TRAVEL_SCALAR = c(20,3),  
                                  INJURY_LINEARITY= c(log(1),log(1.2)),
                                  CASUALTY_EXPONENT_FRACTION = c(8,8),
                                  PA_DOSE_RESPONSE_QUANTILE = F,  
                                  AP_DOSE_RESPONSE_QUANTILE = F)
  numcores <- detectCores()
  ithim_object$outcomes <- mclapply(1:NSAMPLES, FUN = ithim_uncertainty, ithim_object = ithim_object,mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
  ## calculate EVPPI
  parameter_names <- names(ithim_object$parameters)[names(ithim_object$parameters)!="DR_AP_LIST"]
  parameter_samples <- sapply(parameter_names,function(x)ithim_object$parameters[[x]])
  ## omit all-cause mortality
  outcome <- t(sapply(ithim_object$outcomes, function(x) colSums(x$hb$deaths[,3:ncol(x$hb$deaths)])))
  NSCEN <- 5
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
  colnames(evppi) <- c("base",paste0("scen", 1:NSCEN) )[c(1,3:6)]
  rownames(evppi) <- colnames(parameter_samples)
  # test
  expect_equal(sum(evppi>0),60)
})

test_that("accra evppi", {
  ## this takes a long time: only test if you really want to
  test_evppi <- F
  if(test_evppi){
    # load saved result
    #accra_evppi <- readRDS('accra_evppi_test.Rds')
    
    # generate new accra evppi results
    ithim_object <- run_ithim_setup(REFERENCE_SCENARIO        = 'Scenario 1',
                                    NSAMPLES                  = 1024,
                                    BUS_WALK_TIME             = c(log(5), log(1.2)),
                                    MMET_CYCLING              = c(log(5), log(1.2)), 
                                    MMET_WALKING              = c(log(2.5), log(1.2)), 
                                    INJURY_REPORTING_RATE     = c(8,3),  
                                    CHRONIC_DISEASE_SCALAR    = c(0,log(1.2)),  
                                    PM_CONC_BASE              = c(log(50),log(1.2)),  
                                    PM_TRANS_SHARE            = c(5,20),  
                                    BACKGROUND_PA_SCALAR      = c(0,log(1.2)),  
                                    MOTORCYCLE_TO_CAR_RATIO           = c(-1.4,0.4),  
                                    PA_DOSE_RESPONSE_QUANTILE = T,  
                                    AP_DOSE_RESPONSE_QUANTILE = T)
    numcores <- detectCores()
    ithim_object$outcomes <- mclapply(1:NSAMPLES, FUN = ithim_uncertainty, ithim_object = ithim_object,mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
    ## calculate EVPPI
    parameter_names <- names(ithim_object$parameters)[names(ithim_object$parameters)!="DR_AP_LIST"]
    parameter_samples <- sapply(parameter_names,function(x)ithim_object$parameters[[x]])
    ## omit all-cause mortality
    outcome <- t(sapply(ithim_object$outcomes, function(x) colSums(x$hb$deaths[,3:ncol(x$hb$deaths)])))
    NSCEN <- 5
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
    colnames(evppi) <- c("base",paste0("scen", 1:NSCEN) )[c(1,3:6)]
    rownames(evppi) <- colnames(parameter_samples)
    ## add four-dimensional EVPPI if AP_DOSE_RESPONSE is uncertain.
    if("DR_AP_LIST"%in%names(ithim_object$parameters)&&NSAMPLES>=1024){
      AP_names <- sapply(names(ithim_object$parameters),function(x)length(strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA')[[1]])>1)
      diseases <- sapply(names(ithim_object$parameters)[AP_names],function(x)strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA_')[[1]][2])
      evppi_for_AP <- mclapply(diseases, FUN = parallel_evppi_for_AP,parameter_samples,outcome,NSCEN, mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))
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
