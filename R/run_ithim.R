#' @export
run_ithim <- function(ithim_object,seed=1){ 
  ############################
  ## (0) SET UP
  set.seed(seed)
  for(i in 1:length(ithim_object))
    assign(names(ithim_object)[i],ithim_object[[i]])
  ############################
  ## (1) AP PATHWAY
  # Calculated PM2.5 concentrations
  (pm_conc <- scenario_pm_calculations(dist,trip_scen_sets))
  scenario_pm <- pm_conc$scenario_pm
  pm_conc_pp <- pm_conc$pm_conc_pp
  # Air pollution calculation
  (RR_AP_calculations <- gen_ap_rr(pm_conc_pp))
  ############################
  ## (2) PA PATHWAY
  # Calculate total mMETs
  (mmets_pp <- total_mmet(trip_scen_sets))
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
  (injuries <- injuries_function_2(true_distances,injuries_list,reg_model))
  (deaths_yll_injuries <- injury_death_to_yll(injuries))
  ref_injuries <- deaths_yll_injuries$ref_injuries
  ############################
  ## (5) COMBINE (3) AND (4)
  # Combine health burden from disease and injury
  (hb <- health_burden(RR_PA_AP_calculations,deaths_yll_injuries$deaths_yll_injuries))
  return(list(mmets=mmets_pp,scenario_pm=scenario_pm,pm_conc_pp=pm_conc_pp,injuries=injuries,ref_injuries=ref_injuries,hb=hb))
}