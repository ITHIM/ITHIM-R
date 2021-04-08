#Code to run the model and generate results for web interface related to the paper:
#Health impacts of changes in travel patterns in Greater Accra Metropolitan Area, Ghana
#Results in https://shiny.mrc-epid.cam.ac.uk/ithim/.

#Code by Leandro Garcia (l.garcia@qub.ac.uk).
#March 2021.
#R version 4.0.2

#Call packages####
packages <- c("tidyverse", "earth", "ggpubr", "ithimr")
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}
)

#Set up sensitivity analysis scenarios####
scenarios <- c("safer", "chronic_disease", "background_ap", "background_pa")
parameters <- list(seed = 1,
                   CITY = 'accra',
                   REFERENCE_SCENARIO = 'Scenario 1',
                   NSAMPLES = 1024,
                   BUS_WALK_TIME = c(5, 1.2),
                   MMET_CYCLING = c(4.63, 1.2),
                   MMET_WALKING = c(2.53, 1.1),
                   PM_CONC_BASE = list(safer = c(50, 1.3), chronic_disease = c(50, 1.3), background_ap = 30.63, background_pa = c(50, 1.3)),
                   PM_TRANS_SHARE = list(safer = c(1.5, 5), chronic_disease = c(1.5, 5), background_ap = 0.37, background_pa = c(1.5, 5)),
                   PA_DOSE_RESPONSE_QUANTILE = TRUE,
                   AP_DOSE_RESPONSE_QUANTILE = TRUE,
                   BACKGROUND_PA_SCALAR = list(safer = c(1, 1.2), chronic_disease = c(1, 1.2), background_ap = c(1, 1.2), background_pa = 0.5),
                   INJURY_REPORTING_RATE = list(safer = 2, chronic_disease = c(8, 3), background_ap = c(8, 3), background_pa = c(8, 3)),
                   CHRONIC_DISEASE_SCALAR = list(safer = c(1, 1.2), chronic_disease = 2, background_ap = c(1, 1.2), background_pa = c(1, 1.2)),
                   SIN_EXPONENT_SUM = c(1.7, 1.03),
                   CASUALTY_EXPONENT_FRACTION = c(15, 15))

#Run sensitivity analysis scenarios####
for (scenario in scenarios) {
  ithim_object_temp <- run_ithim_setup(seed = parameters$seed,
                                       CITY = parameters$CITY,
                                       REFERENCE_SCENARIO = parameters$REFERENCE_SCENARIO,
                                       NSAMPLES = parameters$NSAMPLES,
                                       BUS_WALK_TIME = parameters$BUS_WALK_TIME,
                                       MMET_CYCLING = parameters$MMET_CYCLING,
                                       MMET_WALKING = parameters$MMET_WALKING,
                                       PM_CONC_BASE = parameters$PM_CONC_BASE[[scenario]],
                                       PM_TRANS_SHARE = parameters$PM_TRANS_SHARE[[scenario]],
                                       PA_DOSE_RESPONSE_QUANTILE = parameters$PA_DOSE_RESPONSE_QUANTILE,
                                       AP_DOSE_RESPONSE_QUANTILE = parameters$AP_DOSE_RESPONSE_QUANTILE,
                                       BACKGROUND_PA_SCALAR = parameters$BACKGROUND_PA_SCALAR[[scenario]],
                                       INJURY_REPORTING_RATE = parameters$INJURY_REPORTING_RATE[[scenario]],
                                       CHRONIC_DISEASE_SCALAR = parameters$CHRONIC_DISEASE_SCALAR[[scenario]],
                                       SIN_EXPONENT_SUM = parameters$SIN_EXPONENT_SUM,
                                       CASUALTY_EXPONENT_FRACTION = parameters$CASUALTY_EXPONENT_FRACTION)

ithim_object_temp$outcomes <- lapply(1:NSAMPLES, run_ithim, ithim_object = ithim_object_temp)
ithim_object_temp$outcomes <- lapply(ithim_object_temp$outcomes, function(x) x$hb)
ithim_object_temp$parameters <- NULL
assign(paste0("ithim_object_", scenario), ithim_object_temp)
rm(ithim_object_temp)
}

#Call the main analysis results####
ithim_object_now <- list()
ithim_object_now$outcomes <- lapply(ithim_object$outcomes, function(x) x$hb)


#Combine results in a list####
sensitivity_analysis_results <- list(now = ithim_object_now,
                                     safer = ithim_object_safer,
                                     more_chronic_disease = ithim_object_chronic_disease,
                                     less_background_AP = ithim_object_background_ap,
                                     less_background_PA = ithim_object_background_pa)

saveRDS(sensitivity_analysis_results, "sensitivity_analysis_results.Rds")

###END OF CODE###