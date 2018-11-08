run_ithim <- function(ithim_object,seed=1){ 
  ############################
  ## (0) SET UP
  set.seed(seed)
  for(i in 1:length(ithim_object))
    assign(names(ithim_object)[i],ithim_object[[i]])
  ############################
  ## (1) AP PATHWAY
  # Calculated PM2.5 concentrations
  (pm_conc <- scenario_pm_calculations(dist,bs))
  scenario_pm <- pm_conc$scenario_pm
  pm_conc_pp <- pm_conc$pm_conc_pp
  # Air pollution calculation
  (RR_AP_calculations <- gen_ap_rr(bs,pm_conc_pp))
  ############################
  ## (2) PA PATHWAY
  # Calculate total mMETs
  (mmets <- total_mmet(bs))
  # Physical activity calculation
  (RR_PA_calculations <- gen_pa_rr(mmets))
  ############################
  ## (3) COMBINE (1) AND (2)
  # Physical activity and air pollution combined
  (RR_PA_AP_calculations <- combined_rr_pa_pa(RR_PA_calculations,RR_AP_calculations))
  ############################
  ## (4) INJURIES
  # Injuries calculation
  (deaths_yll_injuries <- injuries_function(inj_distances[[1]],inj_distances[[2]]))
  injuries <- deaths_yll_injuries$injuries
  scen1_injuries <- deaths_yll_injuries$scen1_injuries
  ############################
  ## (5) COMBINE (3) AND (4)
  # Combine health burden from disease and injury
  (hb <- health_burden(RR_PA_AP_calculations,deaths_yll_injuries$deaths_yll_injuries))
  return(list(mmets=mmets,scenario_pm=scenario_pm,pm_conc_pp=pm_conc_pp,injuries=injuries,scen1_injuries=scen1_injuries,hb=hb))
}
