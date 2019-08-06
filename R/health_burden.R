#' Compute health burden
#' 
#' Compute health burden for populations in scenarios given relative risks
#' 
#' @param ind_ap_pa data.frame of all individuals' relative risks for diseases
#' @param combined_AP_PA=T logic: whether to combine the two exposure pathways (AP and PA) or to compute independently
#' 
#' @return list of data.frames: one for deaths per disease per demographic group, and likewise for YLLs
#' 
#' @export
health_burden <- function(ind_ap_pa,combined_AP_PA=T){
  
  demographic <- DEMOGRAPHIC
  demographic$dem_index <- 1:nrow(demographic)
  demographic <- demographic[,-3]
  names(demographic)[which(colnames(demographic)=='age')] <- 'age_cat'
  
  # subset gbd data for outcome types
  gbd_data_scaled <- DISEASE_BURDEN
  ## chronic disease scalar scales all diseases
  names(gbd_data_scaled)[which(names(gbd_data_scaled)=='age')] <- 'age_cat'
  gbd_data_scaled <- left_join(gbd_data_scaled,demographic, by=c('sex','age_cat'))
  gbd_data_scaled$burden <- gbd_data_scaled$burden*CHRONIC_DISEASE_SCALAR
  gbd_deaths <- subset(gbd_data_scaled,measure=='Deaths')
  gbd_ylls <- subset(gbd_data_scaled,measure=='YLLs (Years of Life Lost)')
  
  ind_ap_pa <- left_join(ind_ap_pa, demographic, by=c('sex','age_cat'))
  pop_details <- deaths <- ylls <- demographic
  # set up reference (scen1)
  reference_scenario <- SCEN_SHORT_NAME[which(SCEN==REFERENCE_SCENARIO)]
  scen_names <- SCEN_SHORT_NAME[SCEN_SHORT_NAME!=reference_scenario]
  ### iterating over all all disease outcomes
  for ( j in 1:nrow(DISEASE_INVENTORY)){
    # Disease acronym and full name
    ac <- as.character(DISEASE_INVENTORY$acronym[j])
    gbd_dn <- as.character(DISEASE_INVENTORY$GBD_name[j])
    # calculating health outcome, or independent pathways?
    pathways_to_calculate <- ifelse(combined_AP_PA,1,DISEASE_INVENTORY$physical_activity[j]+DISEASE_INVENTORY$air_pollution[j])
    for(path in 1:pathways_to_calculate){
      # set up column names
      if(combined_AP_PA){
        middle_bit <-
          paste0(
            ifelse(DISEASE_INVENTORY$physical_activity[j] == 1, 'pa_', ''),
            ifelse(DISEASE_INVENTORY$air_pollution[j] == 1, 'ap_', '')
          )
      }else{
        # if independent, choose which one
        middle_bit <- c('pa_','ap_')[which(c(DISEASE_INVENTORY$physical_activity[j],DISEASE_INVENTORY$air_pollution[j])==1)[path]]
      }
      base_var <- paste0('RR_', middle_bit, reference_scenario, '_', ac)
      scen_vars <- paste0('RR_', middle_bit, scen_names, '_', ac)
      # subset gbd data
      gbd_deaths_disease <- subset(gbd_deaths,cause==gbd_dn)
      gbd_ylls_disease <- subset(gbd_ylls,cause==gbd_dn)
      # set up pif tables
      pif_table <- setDT(ind_ap_pa[,colnames(ind_ap_pa)%in%c(base_var,'dem_index')])
      setnames(pif_table,base_var,'outcome')
      pif_ref <- pif_table[,.(sum(outcome)),by='dem_index']
      ## sort pif_ref
      setorder(pif_ref,dem_index)
      for (index in 1:length(scen_vars)){
        # set up naming conventions
        scen <- scen_names[index]
        scen_var <- scen_vars[index]
        yll_name <- paste0(scen, '_ylls_',middle_bit,ac)
        deaths_name <- paste0(scen, '_deaths_',middle_bit,ac)
        # Calculate PIFs for selected scenario
        pif_table <- setDT(ind_ap_pa[,colnames(ind_ap_pa)%in%c(scen_var,'dem_index')])
        setnames(pif_table,scen_var,'outcome')
        pif_temp <- pif_table[,.(sum(outcome)),by='dem_index']
        ## sort pif_temp
        setorder(pif_temp,dem_index)
        pif_scen <- (pif_ref[,2] - pif_temp[,2]) / pif_ref[,2]
        # Calculate ylls 
        yll_dfs <- combine_health_and_pif(pif_values=pif_scen, hc = gbd_ylls_disease)
        ylls[[yll_name]] <- yll_dfs[,V1]
        # Calculate deaths 
        death_dfs <- combine_health_and_pif(pif_values=pif_scen,hc=gbd_deaths_disease)
        deaths[[deaths_name]] <- death_dfs[,V1]
      }
    }
  }
  deaths <- deaths[,-which(colnames(deaths)=='dem_index')]
  ylls <- ylls[,-which(colnames(ylls)=='dem_index')]
  list(deaths=deaths,ylls=ylls)
}

#' Join disease health burden and injury
#' 
#' Join the two data frames for health burden: that from disease, and that from road-traffic injury
#' 
#' @param ind_ap_pa list (deaths, YLLs) of data frames of all demographic groups' burdens for diseases
#' @param inj list (deaths, YLLs) of data frames of all demographic groups' burdens for road-traffic injury
#' 
#' @return list of data.frames: one for deaths per cause per demographic group, and likewise for YLLs
#' 
#' @export
join_hb_and_injury <- function(ind_ap_pa,inj){
  
  deaths <- ind_ap_pa$deaths
  ylls <- ind_ap_pa$ylls
  # Select deaths columns
  inj_deaths <- dplyr::select(inj, c(age_cat, sex, contains("deaths")))
  # Select yll columns
  inj_ylls <- dplyr::select(inj, c(age_cat, sex, contains("yll")))
  # Join injuries data to global datasets
  deaths <- left_join(deaths, inj_deaths, by = c("age_cat", "sex"))
  ylls <- left_join(ylls, inj_ylls, by = c("age_cat", "sex"))
  list(deaths=deaths,ylls=ylls)
}
