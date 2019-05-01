#' @export
run_ithim <- function(ithim_object,seed=1){ 
  if(length(ithim_object$parameters)>0){
    ithim_object <- ithim_uncertainty(ithim_object,seed)
  }else{
    ithim_object <- ithim_calculation_sequence(ithim_object,seed)
  }
  return(ithim_object)
  
}

#' @export
ithim_calculation_sequence <- function(ithim_object,seed=1){ 
  ############################
  ## (0) SET UP
  set.seed(seed)
  for(i in 1:length(ithim_object))
    assign(names(ithim_object)[i],ithim_object[[i]])
  ############################
  ## (1) AP PATHWAY
  # Calculated PM2.5 concentrations
  (pm_conc <- scenario_pm_calculations(dist,pp_summary))
  scenario_pm <- pm_conc$scenario_pm
  pm_conc_pp <- pm_conc$pm_conc_pp
  # Air pollution calculation
  (RR_AP_calculations <- gen_ap_rr(pm_conc_pp))
  ############################
  ## (2) PA PATHWAY
  # Calculate total mMETs
  (mmets_pp <- total_mmet(pp_summary))
  # Physical activity calculation
  (RR_PA_calculations <- gen_pa_rr(mmets_pp))
  ############################
  ## (3) COMBINE (1) AND (2)
  # Physical activity and air pollution combined
  (RR_PA_AP_calculations <- combined_rr_ap_pa(RR_PA_calculations,RR_AP_calculations))
  ############################
  ## (4) INJURIES
  # Injuries calculation
  for(i in 1:length(inj_distances))
    assign(names(inj_distances)[i],inj_distances[[i]])
  #(injuries <- injuries_function(relative_distances,scen_dist))
  constant_mode <- length(names(parameters))==0
  (injuries0 <- injuries_function_2(true_distances,injuries_list,reg_model,constant_mode))
  injuries <- injuries0[[1]]
  whw <- injuries0[[2]]
  (deaths_yll_injuries <- injury_death_to_yll(injuries))
  ref_injuries <- deaths_yll_injuries$ref_injuries
  ############################
  ## (5) COMBINE (3) AND (4)
  # Combine health burden from disease and injury
  (hb <- health_burden(RR_PA_AP_calculations,deaths_yll_injuries$deaths_yll_injuries))
  pathway_hb <- NULL
  if(constant_mode) pathway_hb <- health_burden(RR_PA_AP_calculations,deaths_yll_injuries$deaths_yll_injuries,combined_AP_PA=F)
  return(list(mmets=mmets_pp,scenario_pm=scenario_pm,pm_conc_pp=pm_conc_pp,injuries=injuries,ref_injuries=ref_injuries,hb=hb,pathway_hb=pathway_hb,whw=whw))
}
