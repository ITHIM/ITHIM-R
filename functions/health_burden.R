health_burden <- function(ind,inj){
  # subset gbd data for outcome types
  gbd_data_scaled <- GBD_DATA
  gbd_data_scaled$value_gama[gbd_data_scaled$cause%in%c("Neoplasms","Ischemic heart disease","Tracheal, bronchus, and lung cancer")] <- 
    gbd_data_scaled$value_gama[gbd_data_scaled$cause%in%c("Neoplasms","Ischemic heart disease","Tracheal, bronchus, and lung cancer")]*CHRONIC_DISEASE_SCALAR
  gbd_deaths <- subset(gbd_data_scaled,measure=='Deaths' & metric == "Number")
  gbd_ylls <- subset(gbd_data_scaled,measure=='YLLs (Years of Life Lost)' & metric == "Number")
  ##!! Hard-coded column names to initialise tables.
  unique_category1 <- unique(ind[[2]])
  unique_category2 <- unique(ind[[4]])
  pop_details <- expand.grid(unique_category1, unique_category2,stringsAsFactors = F)
  colnames(pop_details) <- colnames(ind)[c(2,4)]
  deaths <- deaths_red <- ylls <- ylls_red <- pop_details
  # set up reference (scen1)
  reference_scenario <- 'scen1'
  scen_names <- SCEN_SHORT_NAME[SCEN_SHORT_NAME!=reference_scenario]
  ### iterating over all all disease outcomes
  for ( j in 1:nrow(DISEASE_OUTCOMES)){
    # Disease acronym and full name
    ac <- as.character(DISEASE_OUTCOMES$acronym[j])
    gbd_dn <- as.character(DISEASE_OUTCOMES$GBD_name[j])
    # set up column names
    middle_bit <-
      paste0(
        ifelse(DISEASE_OUTCOMES$physical_activity[j] == 1, 'pa_', ''),
        ifelse(DISEASE_OUTCOMES$air_pollution[j] == 1, 'ap_', '')
      )
    base_var <- paste0('RR_', middle_bit, reference_scenario, '_', ac)
    scen_vars <- paste0('RR_', middle_bit, scen_names, '_', ac)
    # subset gbd data
    gbd_deaths_disease <- subset(gbd_deaths,cause==gbd_dn)
    gbd_ylls_disease <- subset(gbd_ylls,cause==gbd_dn)
    # set up pif tables
    pif_ref <-
      PAF(pop = ind[, colnames(ind) %in% c(base_var, 'sex', 'age_cat')], cn = base_var, mat =
            pop_details)
    yll_ref <-
      combine_health_and_pif(pop = pop_details,
                             pif_values = pif_ref,
                             hc = gbd_ylls_disease)
    death_ref <-
      combine_health_and_pif(pop = pop_details,
                             pif_values = pif_ref,
                             hc = gbd_ylls_disease)
    for (index in 1:length(scen_vars)){
      # set up naming conventions
      scen <- scen_names[index]
      scen_var <- scen_vars[index]
      yll_name <- paste0(scen, '_ylls_',middle_bit,ac)
      #yll_red_name <- paste0(scen, '_ylls_red_',middle_bit,ac)
      deaths_name <- paste0(scen, '_deaths_',middle_bit,ac)
      #deaths_red_name <- paste0(scen, '_deaths_red_',middle_bit,ac)
      # Calculate PIFs for selected scenario
      pif_temp <- PAF(pop = ind[,colnames(ind)%in%c(scen_var,'sex', 'age_cat')], cn = scen_var, mat=pop_details)
      pif_scen <- (pif_ref - pif_temp) / pif_ref
      # Calculate ylls (total and red)
      yll_dfs <- combine_health_and_pif(pop=pop_details,pif_values=pif_scen, hc = gbd_ylls_disease)
      ylls[[yll_name]] <- yll_dfs
      #ylls_red[[yll_red_name]] <- yll_dfs / yll_ref
      # Calculate deaths (total and red)
      death_dfs <- combine_health_and_pif(pop=pop_details,pif_values=pif_scen,hc=gbd_deaths_disease)
      deaths[[deaths_name]] <- death_dfs
      #deaths_red[[deaths_red_name]] <- death_dfs / death_ref
    }
  }
  # Select deaths columns
  inj_deaths <- dplyr::select(inj, c(age_cat, sex, contains("deaths")))
  # Select yll columns
  inj_ylls <- dplyr::select(inj, c(age_cat, sex, contains("yll")))
  # Join injuries data to global datasets
  deaths <- left_join(deaths, inj_deaths, by = c("age_cat", "sex"))
  ylls <- left_join(ylls, inj_ylls, by = c("age_cat", "sex"))
  #list(deaths=deaths,deaths_red=deaths_red,ylls=ylls,ylls_red=ylls_red)
  list(deaths=deaths,ylls=ylls)
}
