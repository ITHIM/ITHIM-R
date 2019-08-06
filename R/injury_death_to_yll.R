#' Map injury death burden to YLL burden
#' 
#' Calculated the YLL burden from the death burden of injury based on the ratio in the GBD data.
#' 
#' @param injuries data frame of injury deaths
#' 
#' @return list of injury deaths and YLLs (which are differences from reference scenario) plus the values in the reference scenario.
#' 
#' @export
injury_death_to_yll <- function(injuries){
  
  joined_injury <- left_join(injuries, GBD_INJ_YLL[,c('sex_age','sex','yll_dth_ratio')], by=c("sex_age",'sex'))
  
  joined_injury$YLL <- joined_injury$Deaths*joined_injury$yll_dth_ratio
  death_and_yll <- dplyr::select(joined_injury, c('age_cat','sex','scenario','Deaths','YLL'))
  
  x_deaths <- dplyr::select(death_and_yll, -YLL)
  x_deaths <- spread(x_deaths,scenario, Deaths)
  x_yll <- dplyr::select(death_and_yll, -Deaths)
  x_yll <- spread(x_yll,scenario, YLL)
  
  ref_scen <- REFERENCE_SCENARIO
  ref_scen_index <- which(SCEN==ref_scen)
  calc_scen <- SCEN[SCEN!=ref_scen]
  calc_scen_index <- which(colnames(x_deaths)%in%calc_scen)
  
  ref_injuries <- as.data.frame(cbind(x_deaths[,1:2],deaths=x_deaths[[ref_scen]],ylls=x_yll[[ref_scen]]))
  deaths <- t(repmat(unlist(ref_injuries$deaths),NSCEN,1)) - x_deaths[,calc_scen_index,drop=F]
  ylls <- t(repmat(unlist(ref_injuries$ylls),NSCEN,1)) - x_yll[,calc_scen_index,drop=F]
  deaths_yll_injuries <- as.data.frame(cbind(as.data.frame(x_deaths[,1:2]),deaths, ylls))
  
  metric <- c("deaths", "yll")
  k <- 1
  for  (i in 1: 2)
    for (j in c(1:(NSCEN+1))[-ref_scen_index]){
      names(deaths_yll_injuries)[2+k] <- paste0(SCEN_SHORT_NAME[j],"_",metric[i],"_inj")
      k<-k+1
    }
  
  list(deaths_yll_injuries=deaths_yll_injuries,ref_injuries=ref_injuries)
}
