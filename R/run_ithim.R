#' Wrapper for running ITHIM
#' 
#' Switch to run the computation directly, or divert to the sampling case
#' 
#' @param ithim_object list of items making up the ithim set up
#' @param seed 
#' 
#' @return ithim_object list of items making up the ithim result
#' 
#' @export
run_ithim <- function(ithim_object,seed=1){ 
  if(length(ithim_object$parameters)>0){
    ithim_object <- ithim_uncertainty(ithim_object,seed)
  }else{
    ithim_object <- ithim_calculation_sequence(ithim_object,seed)
  }
  return(ithim_object)
  
}

#' Cascade of computations that form the ITHIM
#' 
#' Ordered set of computations that form the ITHIM, from travel information to health burden.
#' 
#' @param ithim_object name of disease
#' @param seed
#' 
#' @return list of items making up the ithim result
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
  # Calculated PM2.5 concentrations
  pm_conc <- scenario_pm_calculations(dist = (true_dist %>% dplyr::filter(stage_mode != 'unknown')
                                      %>% mutate_at(-c(1), as.integer)), 
                                      trip_scen_sets=trip_scen_sets)#3
  
  # Calculated PM2.5 concentrations
  co2_conc <- scenario_co2_calculations(dist = (true_dist %>% dplyr::filter(stage_mode != 'unknown')
                                              %>% mutate_at(-c(1), as.integer)))
  
  ############################
  ## (2) PA PATHWAY
  # Calculate total mMETs
  mmets_pp <- total_mmet(trip_scen_sets)
  trip_scen_sets <- NULL
  
  ############################
  ## (1) AP PATHWAY
  scenario_pm <- pm_conc$scenario_pm
  pm_conc_pp <- pm_conc$pm_conc_pp
  pm_conc <- NULL
  
  # Air pollution calculation
  RR_AP_calculations <- gen_ap_rr(pm_conc_pp)
  if(!constant_mode) pm_conc_pp <- NULL
  
  ############################
  ## (2) PA PATHWAY
  # Physical activity calculation
  RR_PA_calculations <- gen_pa_rr(mmets_pp)
  if(!constant_mode) mmets_pp <- NULL
  
  ############################
  ## (3) COMBINE (1) AND (2)
  # Physical activity and air pollution combined
  RR_PA_AP_calculations <- combined_rr_ap_pa(ind_pa=RR_PA_calculations,ind_ap=RR_AP_calculations)
  RR_PA_calculations <- NULL
  RR_AP_calculations <- NULL
  hb_AP_PA <- health_burden(ind_ap_pa=RR_PA_AP_calculations)
  if(constant_mode) {
    pathway_hb_AP_PA <- health_burden(RR_PA_AP_calculations,combined_AP_PA=F)
  }
  RR_PA_AP_calculations <- NULL
  
  ############################
  ## (4) INJURIES
  # Injuries calculation
  for(i in 1:length(inj_distances))
    assign(names(inj_distances)[i],inj_distances[[i]])
  inj_distances <- NULL
  
  #(injuries <- injuries_function(relative_distances,scen_dist))
  injuries0 <- injuries_function_2(true_distances,injuries_list,reg_model,constant_mode)
  injuries_list <- NULL
  reg_model <- NULL
  true_distances <- NULL
  
  injuries <- injuries0[[1]]
  whw <- injuries0[[2]]
  injuries0 <- NULL
  deaths_yll_injuries <- injury_death_to_yll(injuries)
  #injuries <- NULL
  ref_injuries <- deaths_yll_injuries$ref_injuries
  
  #print(sort(sapply(ls(),function(x)object.size(get(x)))))
  ############################
  ## (5) COMBINE (3) AND (4)
  # Combine health burden from disease and injury
  hb <- join_hb_and_injury(hb_AP_PA,deaths_yll_injuries$deaths_yll_injuries)
  if(constant_mode) {
    pathway_hb <- join_hb_and_injury(pathway_hb_AP_PA,deaths_yll_injuries$deaths_yll_injuries)
    return(list(mmets=mmets_pp,scenario_pm=scenario_pm,pm_conc_pp=pm_conc_pp, co2_conc = co2_conc,
                injuries=injuries,ref_injuries=ref_injuries,hb=hb,pathway_hb=pathway_hb,whw=whw))
  }else{
    return(list(hb=hb,ref_injuries=ref_injuries))
  }
}
      
