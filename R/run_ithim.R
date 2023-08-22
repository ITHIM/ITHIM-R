#' Wrapper for running ITHIM
#' 
#' Switch to either calculate the health burden using the constant input parameter values or to sample from distributions first
#' 
#' This function works by creating a switch to run the computation by calling 
#' ithim_calculation_sequence() directly, or to divert to the sampling case
#' ithim_uncertainty.R which first extracts the sampled parameters and then calls
#' the ithim_calculation_sequence().
#' 
#' 
#' @param ithim_object list of input data needed to calculate the health burden
#' @param seed 
#' 
#' @return ithim_object list of items giving the input data and output results
#' 
#' @export


run_ithim <- function(ithim_object,seed=1){ 
  if(length(ithim_object$parameters)>0){ # if running in sampling modes, call ithim_uncertainty()
    ithim_object <- ithim_uncertainty(ithim_object,seed)
  }else{
    ithim_object <- ithim_calculation_sequence(ithim_object,seed)
  }
  return(ithim_object)
  
}






#' Cascade of computations that form ITHIM-Global
#' 
#' Ordered set of computations as part of ITHIM-Global that calculates the required output parameters
#' 
#' This function performs the following steps:
#' 
#' \enumerate{
#' 
#' \item extract all lists and variables from the ithim_object list
#' 
#' \item  air pollution pathway:
#'    \itemize{
#'    \item calculate the PM2.5 exposure for each person in the synthetic population 
#'      and PM2.5 emissions for each mode and scenario (scenario_pm_calculations.R)
#'    \item calculate the CO2 emissions for each mode and scenario (scenario_co2_calculations.R)
#'    \item assign relative risk to each person in the synthetic population for each disease
#'      related to PM pollution and each scenario based on the individual PM exposure 
#'      levels (gen_ap_rr.R)
#'      }
#'      
#' \item  physical activity pathway:
#'    \itemize{
#'    \item calculate total mMETs for each person in the synthetic population (total_mmet.R)
#'    \item assign relative risk to each person in the synthetic population for each disease
#'      related to physical activity levels and each scenario based on the individual mMET
#'      values (gen_pa_rr.R)
#'    }
#'  
#' \item  physical activity and air pollution combined:
#'    \itemize{
#'    \item combine the PA and AP datasets by joining the two datasets. For disease affected by 
#'      both PA and AP calculate the joined relative risk by multiplying the PA and AP
#'      relative risks (combined_rr_ap_pa.R)  
#'    \item calculate the health burden (Yll and deaths) for each disease and age and sex
#'      category (health_burden.R):
#'      \itemize{
#'      \item calculate the health burden (Yll and deaths) for each disease and age 
#'        and sex category. Combine the AP and PA pathways for diseases affected
#'        by both AP and PA
#'      \item if running in constant mode also calculate the health burden for both the
#'        AP and PA pathways separately
#'        }
#'     }
#'     
#' \item  injury pathway:
#'    \itemize{
#'    \item estimate the injury deaths for the baseline and each scenario by age and sex category, 
#'       also estimate the total injury deaths counts for the who-hit-whom and no-other-vehicle matrices
#'       by casualty (and strike) mode again for the baseline and each scenario (injuries_function2.R)
#'       \itemize{
#'      \itemif running in constant mode include upper and lower confidence intervals  
#'      }
#'    \item calculate the years of life lost from the injury deaths (injury_death_to_yll.R)
#'   }
#'   
#' \item  combine all pathways using the outputs from (3) and (4):
#'    \itemize{
#'    \item combine the AP, PA and injury health burden data for ylls and deaths (join_hb_and_injury.R)
#'      for all diseases, injuries and scenarios
#'      }
#' } 
#'  
#'  
#' @param ithim_object name of disease
#' @param seed
#' 
#' @return ithim_object - list of items making up the ithim result
#' 
#' @export


ithim_calculation_sequence <- function(ithim_object,seed=1){ 
  
  
  ############################
  ## (0) SET UP
  set.seed(seed)
  for(i in 1:length(ithim_object))
    assign(names(ithim_object)[i],ithim_object[[i]])
  constant_mode <- length(parameters)==0
  ithim_object <- NULL
  
  ############################
  ## (1) AP PATHWAY
  # Calculate PM2.5 emissions for each mode and scenario and calculate PM2.5 
  # exposure for each person in the synthetic population 
  pm_conc <- scenario_pm_calculations(dist = (true_dist %>% dplyr::filter(stage_mode != 'unknown')
                                      %>% dplyr::mutate_at(-c(1), as.integer)), 
                                      trip_scen_sets=trip_scen_sets)#3
  
  # Calculate the CO2 emissions for each mode and scenario
  co2_conc <- scenario_co2_calculations(dist = (true_dist %>% dplyr::filter(stage_mode != 'unknown')
                                              %>% dplyr::mutate_at(-c(1), as.integer)))
  
  
  
  scenario_pm <- pm_conc$scenario_pm
  pm_conc_pp <- pm_conc$pm_conc_pp
  pm_conc <- NULL
  
  # Assign relative risks to each person in the synthetic population for each disease
  # related to air pollution and each scenario based on the individual PM exposure levels
  RR_AP_calculations <- gen_ap_rr(pm_conc_pp)
  if(!constant_mode) pm_conc_pp <- NULL
  
  
  ############################
  ## (2) PA PATHWAY
  # Physical activity calculations
  
  # calculate total mMETs for each person in the synthetic population
  mmets_pp <- total_mmet(trip_scen_sets)
  trip_scen_sets <- NULL

  # assign a relative risk to each person in the synthetic population for each disease
  # related to physical activity levels and each scenario based on the individual mMET values
  RR_PA_calculations <- gen_pa_rr(mmets_pp, 
                                  conf_int = ifelse(constant_mode, TRUE, FALSE))
  if(!constant_mode) mmets_pp <- NULL
  
  
  ############################
  ## (3) COMBINE (1) AND (2)
  # Physical activity and air pollution combined
  
  # create one dataframe containing both the PA, the AP and the combined PA and AP relative risks
  # (for those diseases affected by both PA and AP) for all people in the synthetic population and all scenarios
  RR_PA_AP_calculations <- combined_rr_ap_pa(ind_pa = RR_PA_calculations, ind_ap = RR_AP_calculations, 
                                             conf_int = ifelse(constant_mode, TRUE, FALSE))
  
  RR_PA_calculations <- NULL
  RR_AP_calculations <- NULL
  
  # calculate the health burden (Yll and deaths) for each disease and age and sex category
  # by combining the AP and PA pathways for diseases affected by both AP and PA
  hb_AP_PA <- health_burden(ind_ap_pa=RR_PA_AP_calculations, conf_int = ifelse(constant_mode, TRUE, FALSE))
  
  # if running in constant mode calculate the health burden (Yll and deaths) for each disease and age and sex category
  # for each pathway (AP and PA) independently
  if(constant_mode) { 
    pathway_hb_AP_PA <- health_burden(RR_PA_AP_calculations, conf_int = 
                                        ifelse(constant_mode, TRUE, FALSE), combined_AP_PA = FALSE)
  }
  RR_PA_AP_calculations <- NULL
  
  ############################
  ## (4) INJURIES
  # Injuries calculation
  
  # extract the data used to run the injury pathway
  for(i in 1:length(inj_distances))
    assign(names(inj_distances)[i],inj_distances[[i]])
  #inj_distances <- NULL
  
  # estimate the injury deaths for the baseline and each scenario by age and sex category 
  # if running in constant mode include upper and lower confidence intervals
  injuries0 <- injuries_function_2(true_distances,injuries_list,reg_model,constant_mode)
  injuries_list <- NULL
  reg_model <- NULL
  true_distances <- NULL
  
  # extract the injury deaths for the baseline and each scenario by age and sex category 
  # (contains upper and lower confidence interval boundaries if running in constant mode)
  injuries <- injuries0[[1]]
  
  # extract the total injury deaths for the baseline and each scenario split into 
  # who-hit-whom and no-other-vehicle matrices by casualty (and strike) mode
  whw <- injuries0[[2]]
  injuries0 <- NULL
  
  # calculate the years of life lost from the injury deaths.
  # function returns the injury and yll values of the reference scenario and also
  # a dataframe giving the changes in yll and deaths for all non-reference scenarios
  # compared with the reference scenario
  deaths_yll_injuries <- injury_death_to_yll(injuries)
  injuries <- NULL
  
  # extract the reference injury and yll values
  ref_injuries <- deaths_yll_injuries$ref_injuries
  

  ############################
  ## (5) COMBINE (3) AND (4)
  # Combine health burden from disease and injury
  hb <- join_hb_and_injury(hb_AP_PA,deaths_yll_injuries$deaths_yll_injuries)
  
  # return lists
  if(constant_mode) {
    pathway_hb <- join_hb_and_injury(pathway_hb_AP_PA,deaths_yll_injuries$deaths_yll_injuries)
    return(list(mmets=mmets_pp,scenario_pm=scenario_pm,pm_conc_pp=pm_conc_pp, co2_conc = co2_conc,
                injuries=injuries,ref_injuries=ref_injuries,hb=hb,pathway_hb=pathway_hb,whw=whw))
  }else{
    return(list(hb=hb,ref_injuries=ref_injuries))
  }
}
      
