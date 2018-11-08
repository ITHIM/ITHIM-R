injuries_function <- function(relative_distances,scen_dist){
  ### injury code
  ### This is the script for distance-based injury model for Accra using safety-in-numbers
  
  ##RJ match exponents and distances to multiply matrices
  whw_mat2 <- list()
  whw_mat <- data.frame(WHW_MAT)
  vic_order <- whw_mat[,1]
  strike_order <- colnames(whw_mat)[2:ncol(whw_mat)]
  sin_vic <- S.I.N[1:6,]
  sin_vic_ordered <- sin_vic[match(vic_order,data.frame(sin_vic)[,1]),match(strike_order,colnames(sin_vic))]
  sin_str <- S.I.N[7:12,]
  sin_str_ordered <- sin_str[match(vic_order,data.frame(sin_str)[,1]),match(strike_order,colnames(sin_str))]
  whw_mat_adjusted <- whw_mat[,2:8]*SAFETY_SCALAR
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
  for (k in 2:(length(SCEN_SHORT_NAME))) victim_deaths <- cbind(victim_deaths, as.data.frame(rowSums(whw_mat2[[k]][,2:7],na.rm=T))) 
  names(victim_deaths)[1] <- c("victim_type")
  names(victim_deaths)[2:(length(SCEN_SHORT_NAME)+1)] <- SCEN_SHORT_NAME
  
  ## distribute injuries to ages
  ##RJ match distances and injuries to multiply matrices
  dist_scen_indices <- match(relative_distances$scenario,SCEN)
  vic_scen_indices <- match(SCEN_SHORT_NAME[dist_scen_indices],colnames(victim_deaths))
  vic_mode_indices <- match(names(relative_distances)[3:7],victim_deaths[,1])
  injuries <- relative_distances
  injuries[,3:7] <- injuries[,3:7]*t(victim_deaths[vic_mode_indices,vic_scen_indices])
  
  injuries[,9] <- rowSums(injuries[,3:7],na.rm=T)
  names(injuries)[9] <-"Deaths"
  
  joined_injury <- left_join(injuries, GBD_INJ_YLL[,c('sex_age','sex','yll_dth_ratio')], by="sex_age")
  
  joined_injury$YLL <- joined_injury$Deaths*joined_injury$yll_dth_ratio
  death_and_yll <- dplyr::select(joined_injury, c('age_cat','sex','scenario','Deaths','YLL'))
  
  x_deaths <- dplyr::select(death_and_yll, -YLL)
  x_deaths <- spread(x_deaths,scenario, Deaths)
  x_yll <- dplyr::select(death_and_yll, -Deaths)
  x_yll <- spread(x_yll,scenario, YLL)
  
  scen1_injuries <- list(deaths=x_deaths[,4],ylls=x_yll[,4])
  deaths <- x_deaths[,-4]
  deaths[,3:7] <- - deaths[,3:7] + t(repmat(unlist(scen1_injuries$deaths),NSCEN,1))
  ylls <- x_yll[,-4]
  ylls[,3:7] <- - ylls[,3:7] + t(repmat(unlist(scen1_injuries$ylls),NSCEN,1))
  
  deaths_yll_injuries <- as.data.frame(cbind(deaths, ylls[,-c(1:2)]))
  names(deaths_yll_injuries)[1:2]<- c("age_cat", "sex")
  
  metric <- c("deaths", "yll")
  k <- 1
  for  (i in 1: 2)
    for (j in c(1:(NSCEN+1))[-2]){
      names(deaths_yll_injuries)[2+k] <- paste0(SCEN_SHORT_NAME[j],"_",metric[i],"_inj")
      k<-k+1
    }
  
  list(injuries=injuries,deaths_yll_injuries=deaths_yll_injuries,scen1_injuries=scen1_injuries)
}
