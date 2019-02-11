#' @export
injuries_function <- function(relative_distances,scen_dist){
  ### injury code
  ### This is the script for distance-based injury model for Accra using safety-in-numbers
  
  ##RJ match exponents and distances to multiply matrices
  ## TO DO: regression model with reporting rate 1/3 -- 1. Therefore, fix safety scalar to be either 0.5 or 1.
  whw_mat2 <- list()
  whw_mat <- data.frame(WHW_MAT)
  vic_order <- whw_mat[,1]
  strike_order <- colnames(whw_mat)[2:ncol(whw_mat)]
  sin_vic <- INJ_DIST_EXP[1:6,]
  sin_vic_ordered <- sin_vic[match(vic_order,data.frame(sin_vic)[,1]),match(strike_order,colnames(sin_vic))]
  sin_str <- INJ_DIST_EXP[7:12,]
  sin_str_ordered <- sin_str[match(vic_order,data.frame(sin_str)[,1]),match(strike_order,colnames(sin_str))]
  whw_mat_adjusted <- whw_mat[,2:8]/INJURY_REPORTING_RATE
  for (k in 1:(length(SCEN_SHORT_NAME))) {
    victim_dist <- scen_dist[match(vic_order,rownames(scen_dist)),k]
    strk_dist <- scen_dist[match(strike_order,rownames(scen_dist)),k]
    victim_dist_mat <- t(repmat(victim_dist,length(strk_dist),1))
    strk_dist_mat <- repmat(strk_dist,length(victim_dist),1)
    whw_mat2[[k]] <- whw_mat_adjusted*(victim_dist_mat^sin_vic_ordered)*(strk_dist_mat^sin_str_ordered) 
  }
  
  ## get total injuries
  # names of victim types
  victim_deaths <- as.data.frame(WHW_MAT[,1])  
  # number of deaths in baseline by victim type
  victim_deaths <- cbind(victim_deaths, rowSums(whw_mat_adjusted))
  for (k in 2:(length(SCEN_SHORT_NAME))) victim_deaths <- cbind(victim_deaths, as.data.frame(rowSums(whw_mat2[[k]],na.rm=T))) 
  names(victim_deaths)[1] <- c("victim_type")
  names(victim_deaths)[2:(length(SCEN_SHORT_NAME)+1)] <- SCEN_SHORT_NAME
  
  ## distribute injuries to ages
  ##RJ match distances and injuries to multiply matrices
  dist_scen_indices <- match(relative_distances$scenario,SCEN)
  vic_scen_indices <- match(SCEN_SHORT_NAME[dist_scen_indices],colnames(victim_deaths))
  vic_mode_indices <- match(names(relative_distances)[2+1:nrow(victim_deaths)],victim_deaths[,1])
  injuries <- relative_distances
  ##!! hard-coding of indices
  injuries[,2+1:nrow(victim_deaths)] <- injuries[,2+1:nrow(victim_deaths)]*t(victim_deaths[vic_mode_indices,vic_scen_indices])
  
  injuries[,ncol(injuries)+1] <- rowSums(injuries[,2+1:nrow(victim_deaths)],na.rm=T)
  names(injuries)[ncol(injuries)] <-"Deaths"
  
  injuries
}
