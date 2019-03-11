#' @export
health_burden <- function(ind_ap_pa,inj){
  # subset gbd data for outcome types
  gbd_data_scaled <- DISEASE_BURDEN
  gbd_data_scaled$burden[gbd_data_scaled$cause%in%c("Neoplasms","Ischemic heart disease","Tracheal, bronchus, and lung cancer")] <- 
    gbd_data_scaled$burden[gbd_data_scaled$cause%in%c("Neoplasms","Ischemic heart disease","Tracheal, bronchus, and lung cancer")]*CHRONIC_DISEASE_SCALAR
  gbd_deaths <- subset(gbd_data_scaled,measure=='Deaths')
  gbd_ylls <- subset(gbd_data_scaled,measure=='YLLs (Years of Life Lost)')
  ##!! Hard-coded column names to initialise tables.
  sex_index <- which(colnames(ind_ap_pa)=='sex')
  age_index <- which(colnames(ind_ap_pa)=='age_cat')
  unique_category1 <- unique(ind_ap_pa[[sex_index]])
  unique_category2 <- unique(ind_ap_pa[[age_index]])
  pop_details <- expand.grid(unique_category1, unique_category2,stringsAsFactors = F)
  colnames(pop_details) <- colnames(ind_ap_pa)[c(sex_index,age_index)]
  deaths <- deaths_red <- ylls <- ylls_red <- pop_details
  # set up reference (scen1)
  reference_scenario <- SCEN_SHORT_NAME[which(SCEN==REFERENCE_SCENARIO)]
  scen_names <- SCEN_SHORT_NAME[SCEN_SHORT_NAME!=reference_scenario]
  ### iterating over all all disease outcomes
  for ( j in 1:nrow(DISEASE_INVENTORY)){
    # Disease acronym and full name
    ac <- as.character(DISEASE_INVENTORY$acronym[j])
    gbd_dn <- as.character(DISEASE_INVENTORY$GBD_name[j])
    # set up column names
    middle_bit <-
      paste0(
        ifelse(DISEASE_INVENTORY$physical_activity[j] == 1, 'pa_', ''),
        ifelse(DISEASE_INVENTORY$air_pollution[j] == 1, 'ap_', '')
      )
    base_var <- paste0('RR_', middle_bit, reference_scenario, '_', ac)
    scen_vars <- paste0('RR_', middle_bit, scen_names, '_', ac)
    # subset gbd data
    gbd_deaths_disease <- subset(gbd_deaths,cause==gbd_dn)
    gbd_ylls_disease <- subset(gbd_ylls,cause==gbd_dn)
    # set up pif tables
    pif_ref <-
      population_attributable_fraction(pop = ind_ap_pa[, colnames(ind_ap_pa) %in% c(base_var, 'sex', 'age_cat')], cn = base_var, mat =
                                         pop_details)
    #yll_ref <-
    #  combine_health_and_pif(pop = pop_details,
    #                         pif_values = pif_ref,
    #                         hc = gbd_ylls_disease)
    #death_ref <-
    #  combine_health_and_pif(pop = pop_details,
    #                         pif_values = pif_ref,
    #                         hc = gbd_deaths_disease)
    for (index in 1:length(scen_vars)){
      # set up naming conventions
      scen <- scen_names[index]
      scen_var <- scen_vars[index]
      yll_name <- paste0(scen, '_ylls_',middle_bit,ac)
      deaths_name <- paste0(scen, '_deaths_',middle_bit,ac)
      # Calculate PIFs for selected scenario
      pif_temp <- population_attributable_fraction(pop = ind_ap_pa[,colnames(ind_ap_pa)%in%c(scen_var,'sex', 'age_cat')], cn = scen_var, mat=pop_details)
      pif_scen <- (pif_ref - pif_temp) / pif_ref
      # Calculate ylls 
      yll_dfs <- combine_health_and_pif(pop=pop_details,pif_values=pif_scen, hc = gbd_ylls_disease)
      ylls[[yll_name]] <- yll_dfs
      # Calculate deaths 
      death_dfs <- combine_health_and_pif(pop=pop_details,pif_values=pif_scen,hc=gbd_deaths_disease)
      deaths[[deaths_name]] <- death_dfs
    }
  }
  # Select deaths columns
  inj_deaths <- dplyr::select(inj, c(age_cat, sex, contains("deaths")))
  # Select yll columns
  inj_ylls <- dplyr::select(inj, c(age_cat, sex, contains("yll")))
  # Join injuries data to global datasets
  deaths <- left_join(deaths, inj_deaths, by = c("age_cat", "sex"))
  ylls <- left_join(ylls, inj_ylls, by = c("age_cat", "sex"))
  list(deaths=deaths,ylls=ylls)
}
