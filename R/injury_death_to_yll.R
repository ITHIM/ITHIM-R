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
  
  # injuries is a tibble, GBD_INJ_YLL is a data.frame, returns a tibble
  joined_injury <- dplyr::left_join(injuries, GBD_INJ_YLL[,c('sex_age','sex','yll_dth_ratio')], by=c("sex_age",'sex'))
  
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
  
  # Repeat the above logic for lower and upper interval
  
  if (any(colnames(injuries) %in% c('Deaths_lb', 'Deaths_ub')))
  {
    
    # injuries is a tibble, GBD_INJ_YLL is a data.frame, returns a tibble
    joined_injury_lb <- dplyr::left_join(injuries, GBD_INJ_YLL[,c('sex_age','sex','yll_dth_ratio')], by=c("sex_age",'sex'))
    
    joined_injury_lb$YLL_lb <- joined_injury_lb$Deaths_lb * joined_injury_lb$yll_dth_ratio
    death_and_yll_lb <- dplyr::select(joined_injury_lb, c('age_cat','sex','scenario','Deaths_lb','YLL_lb'))
    
    x_deaths_lb <- dplyr::select(death_and_yll_lb, -YLL_lb)
    x_deaths_lb <- spread(x_deaths_lb,scenario, Deaths_lb)
    x_yll_lb <- dplyr::select(death_and_yll_lb, -Deaths_lb)
    x_yll_lb <- spread(x_yll_lb,scenario, YLL_lb)
    
    ref_scen_lb <- REFERENCE_SCENARIO
    ref_scen_index_lb <- which(SCEN==ref_scen_lb)
    calc_scen <- SCEN[SCEN!=ref_scen_lb]
    calc_scen_index <- which(colnames(x_deaths_lb)%in%calc_scen)
    
    ref_injuries_lb <- as.data.frame(cbind(x_deaths_lb[,1:2],deaths_lb=x_deaths_lb[[ref_scen_lb]],ylls_lb=x_yll_lb[[ref_scen_lb]]))
    deaths_lb <- t(repmat(unlist(ref_injuries_lb$deaths_lb),NSCEN,1)) - x_deaths_lb[,calc_scen_index,drop=F]
    ylls_lb <- t(repmat(unlist(ref_injuries_lb$ylls_lb),NSCEN,1)) - x_yll_lb[,calc_scen_index,drop=F]
    deaths_yll_injuries_lb <- as.data.frame(cbind(as.data.frame(x_deaths_lb[,1:2]),deaths_lb, ylls_lb))
    
    metric <- c("deaths", "yll")
    k <- 1
    for  (i in 1: 2)
      for (j in c(1:(NSCEN+1))[-ref_scen_index_lb]){
        
        names(deaths_yll_injuries_lb)[2+k] <- paste0(SCEN_SHORT_NAME[j], "_", metric[i], "_inj_lb")
        k<-k+1
      }
    
    # injuries is a tibble, GBD_INJ_YLL is a data.frame, returns a tibble
    joined_injury_ub <- dplyr::left_join(injuries, GBD_INJ_YLL[,c('sex_age','sex','yll_dth_ratio')], by=c("sex_age",'sex'))
    
    joined_injury_ub$YLL_ub <- joined_injury_ub$Deaths_ub * joined_injury_ub$yll_dth_ratio
    death_and_yll_ub <- dplyr::select(joined_injury_ub, c('age_cat','sex','scenario','Deaths_ub','YLL_ub'))
    
    x_deaths_ub <- dplyr::select(death_and_yll_ub, -YLL_ub)
    x_deaths_ub <- spread(x_deaths_ub,scenario, Deaths_ub)
    x_yll_ub <- dplyr::select(death_and_yll_ub, -Deaths_ub)
    x_yll_ub <- spread(x_yll_ub,scenario, YLL_ub)
    
    ref_scen_ub <- REFERENCE_SCENARIO
    ref_scen_index_ub <- which(SCEN==ref_scen_ub)
    calc_scen <- SCEN[SCEN!=ref_scen_ub]
    calc_scen_index <- which(colnames(x_deaths_ub)%in%calc_scen)
    
    ref_injuries_ub <- as.data.frame(cbind(x_deaths_ub[,1:2],deaths_ub=x_deaths_ub[[ref_scen_ub]],ylls_ub=x_yll_ub[[ref_scen_ub]]))
    deaths_ub <- t(repmat(unlist(ref_injuries_ub$deaths_ub),NSCEN,1)) - x_deaths_ub[,calc_scen_index,drop=F]
    ylls_ub <- t(repmat(unlist(ref_injuries_ub$ylls_ub),NSCEN,1)) - x_yll_ub[,calc_scen_index,drop=F]
    deaths_yll_injuries_ub <- as.data.frame(cbind(as.data.frame(x_deaths_ub[,1:2]),deaths_ub, ylls_ub))
    
    metric <- c("deaths", "yll")
    k <- 1
    for  (i in 1: 2)
      for (j in c(1:(NSCEN+1))[-ref_scen_index_ub]){
        
        names(deaths_yll_injuries_ub)[2+k] <- paste0(SCEN_SHORT_NAME[j], "_", metric[i], "_inj_ub")
        k<-k+1
      }
    
    # Combine lower and upper datasets
    
    deaths_yll_injuries <- left_join(deaths_yll_injuries, deaths_yll_injuries_ub)
    deaths_yll_injuries <- left_join(deaths_yll_injuries, deaths_yll_injuries_lb)
    
    ref_injuries <- left_join(ref_injuries, ref_injuries_ub)
    ref_injuries <- left_join(ref_injuries, ref_injuries_lb)
  }
  
  list(deaths_yll_injuries=deaths_yll_injuries,ref_injuries=ref_injuries)
}
