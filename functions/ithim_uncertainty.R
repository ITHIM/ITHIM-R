ithim_uncertainty <- function(ithim_obj,seed=1){ 
  ############################
  ## (0) SET UP
  for(i in 1:length(ithim_obj))
    assign(names(ithim_obj)[i],ithim_obj[[i]])
  # Get parameters
  for(i in 1:length(parameters))
    assign(names(parameters)[i],parameters[[i]][[seed]],pos=1)
  ## Re-do set up if MEAN_BUS_WALK_TIME has changed
  if('MEAN_BUS_WALK_TIME'%in%names(parameters)){
    rd <- ithim_setup_baseline_scenario()
    ithim_obj$bs <- create_all_scenarios(rd)
    ######################
    # Generate distance and duration matrices
    dist_and_dur <- dist_dur_tbls(ithim_obj$bs)
    ithim_obj$dist <- dist_and_dur$dist
    ithim_obj$dur <- dist_and_dur$dur
    # distances for injuries calculation
    ithim_obj$inj_distances <- distances_for_injury_function(ithim_obj$bs)
  }
  ############################
  # Run ITHIM cascade of functions
  run_results <- run_ithim(ithim_obj,seed)
  run_results$dist <- ithim_obj$dist
  run_results$dur <- ithim_obj$dur
  #return(run_results)
  ##!! RJ for now return only hb from uncertain simulations
  return(list(hb=run_results$hb))
}
