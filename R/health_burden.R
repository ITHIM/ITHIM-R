#' Compute health burden
#'
#' Compute health burden for population in scenarios given relative risks for diseases
#'
#' This function performs the following steps:
#'
#' \itemize{
#'  \item get the demographic and disease burden (subset of Global Burden of Disease dataset) data
#'    into the correct formats and join the two datasets
#'
#'  \item scale the burden data by the CHRONIC_DISEASE_SCALAR to account for bias in the data
#'
#'  \item split the above dataframe into two dataframes, one for deaths and one
#'    for years of life lost (YLLs)
#'
#'  \item add a demographic index (by age and sex category) to the dataframe containing the
#'    individual relative risk for different diseases
#'
#'  \item set the reference and the other scenarios
#'
#'  \item iterate over all disease outcomes:
#'    \itemize{
#'    \item define column names
#'
#'    \item loop either over 1 or 2 pathways depending on whether both PA and AP are affecting
#'      the disease and whether the AP and PA pathways are combined or not:
#'      \itemize{
#'      \item extract the relevant burden of disease for the specific scenario both for YLLs and deaths
#'
#'      \item find the sum of the relative risks (RR) for the specific disease for each age
#'        and sex category for the reference scenario
#'
#'      \item loop through the non-reference scenarios:
#'        \itemize{
#'        \item define column names
#'
#'        \item find the sum of the relative risks (RR) for the specific disease for each age
#'          and sex category for the non-reference scenario
#'
#'        \item calculate the PIF (potential impact fraction), i.e the proportional change
#'          in the sum of relative risks between the reference and the non-reference
#'          scenario for each age and sex category
#'
#'        \item calculate the health burden (deaths and ylls) for the non-reference scenario
#'          compared to the reference scenario by multiplying the current burden of disease
#'          by the PIF (combine_health_and_pif.R)
#'          }
#'
#'      \item if confidence intervals are required, loop through the upper and lower confidence
#'        interval limits and calculate the health burden for deaths and YLLs using
#'        the upper and lower confidence relative risks. If no upper and lower relative risk
#'        values exist, use the median value instead
#'        }
#'    }
#' }
#'
#'
#' @param ind_ap_pa dataframe of all individuals' relative risks for diseases
#' @param conf_int=F logic: whether to include confidence interval from dose response relationships or not
#' @param combined_AP_PA=T logic: whether to combine the two exposure pathways (AP and PA) or to compute them independently
#'
#' @return list of dataframes: one for deaths per disease per demographic group and scenario, and likewise for YLLs
#'
#' @export


health_burden <- function(ind_ap_pa, conf_int = F, combined_AP_PA = T) {
  # use demographic data and get it into the required format
  demographic <- DEMOGRAPHIC
  demographic$dem_index <- 1:nrow(demographic)
  demographic <- demographic[, !names(demographic) %in% "population"]
  names(demographic)[which(colnames(demographic) == "age")] <- "age_cat"

  # use disease burden data (created from Global Burden of Disease dataset)
  gbd_data_scaled <- DISEASE_BURDEN
  names(gbd_data_scaled)[which(names(gbd_data_scaled) == "age")] <- "age_cat"

  # join the demographic and the disease burden data
  gbd_data_scaled <- dplyr::left_join(gbd_data_scaled, demographic, by = c("sex", "age_cat"))

  # use the CHRONIC_DISEASE_SCALAR to scale all diseases to account for bias in the GBD data
  gbd_data_scaled$burden <- gbd_data_scaled$burden * CHRONIC_DISEASE_SCALAR

  # create two dataframes for deaths and years of life lost (YLL)
  gbd_deaths <- subset(gbd_data_scaled, measure == "Deaths")
  gbd_ylls <- subset(gbd_data_scaled, measure == "YLLs (Years of Life Lost)")

  # join the dataframe containing the relative risks for all diseases with the demographic
  # information, adding the demographic index
  ind_ap_pa <- dplyr::left_join(ind_ap_pa, demographic, by = c("sex", "age_cat"))

  # initialise dataframes
  deaths <- ylls <- demographic

  # set up a reference scenario which needs to correspond to the scenario to which the city
  # level data corresponds
  if (REFERENCE_SCENARIO == "Baseline") REFERENCE_SCENARIO <- "baseline"
  reference_scenario <- SCEN_SHORT_NAME[which(SCEN == REFERENCE_SCENARIO)]
  # set up non-reference scenarios
  scen_names <- SCEN_SHORT_NAME[SCEN_SHORT_NAME != reference_scenario]

  ### iterate over all disease outcomes
  for (j in 1:nrow(DISEASE_INVENTORY)) {
    # Disease acronym and full name
    ac <- as.character(DISEASE_INVENTORY$acronym[j])
    gbd_dn <- as.character(DISEASE_INVENTORY$GBD_name[j])

    # calculate health outcome, or independent pathways?
    pathways_to_calculate <- ifelse(combined_AP_PA, 1,
      DISEASE_INVENTORY$physical_activity[j] + DISEASE_INVENTORY$air_pollution[j]
    )

    # loop through relevant pathways depending on whether disease affects AP or PA and
    # whether AP and PA are combined or not
    for (path in 1:pathways_to_calculate) { # pathways_to_calculate is either 1 or 2
      # set up column names
      if (combined_AP_PA) { # if combined, find whether only pa or ap or both
        middle_bit <-
          paste0(
            ifelse(DISEASE_INVENTORY$physical_activity[j] == 1, "pa_", ""),
            ifelse(DISEASE_INVENTORY$air_pollution[j] == 1, "ap_", "")
          )
      } else {
        # if independent, choose which one
        middle_bit <- c("pa_", "ap_")[which(c(DISEASE_INVENTORY$physical_activity[j], DISEASE_INVENTORY$air_pollution[j]) == 1)[path]]
      }

      # set column names
      base_var <- paste0("RR_", middle_bit, reference_scenario, "_", ac)
      scen_vars <- paste0("RR_", middle_bit, scen_names, "_", ac)

      # subset gbd data for deaths and ylls by the given disease
      gbd_deaths_disease <- subset(gbd_deaths, cause == gbd_dn)
      gbd_ylls_disease <- subset(gbd_ylls, cause == gbd_dn)

      ## set up pif tables
      # extract the relative baseline risk and demographic index
      pif_table <- setDT(ind_ap_pa[, colnames(ind_ap_pa) %in% c(base_var, "dem_index")])
      # set the relative risk column name to 'outcome'
      setnames(pif_table, base_var, "outcome")
      # sum the outcomes for the synthetic population by age and sex category
      pif_ref <- pif_table[, .(sum(outcome)), by = "dem_index"]
      ## sort pif_ref
      setorder(pif_ref, dem_index) # order by age and sex category, i.e dem_index


      for (index in 1:length(scen_vars)) { # loop through scenarios

        # set up naming conventions
        scen <- scen_names[index]
        scen_var <- scen_vars[index]
        yll_name <- paste0(scen, "_ylls_", middle_bit, ac)
        deaths_name <- paste0(scen, "_deaths_", middle_bit, ac)

        # extract the relative scenario risk and demographic index
        pif_table <- setDT(ind_ap_pa[, colnames(ind_ap_pa) %in% c(scen_var, "dem_index")])
        # set the relative risk column name to 'outcome'
        setnames(pif_table, scen_var, "outcome")
        # sum the outcomes for the synthetic population by age and sex category
        pif_temp <- pif_table[, .(sum(outcome)), by = "dem_index"]

        setorder(pif_temp, dem_index) # sort pif_temp
        # calculate PIF (i.e. change in RR compared to reference scenario) for this scenario and convert to vector
        pif_scen <- ((pif_ref[, 2] - pif_temp[, 2]) / pif_ref[, 2]) %>% pull()

        # Calculate the difference in ylls between the non-reference and the reference scenario
        # by multiplying the current burden of disease for particular disease by the PIF value,
        # i.e the expected change between the reference scenario and the given scenario
        # for each age and sex category
        yll_dfs <- combine_health_and_pif(pif_values = pif_scen, hc = gbd_ylls_disease)
        ylls[[yll_name]] <- yll_dfs

        # Calculate the difference in deaths between the non-reference and the reference scenario
        # by multiplying current burden of disease for particular disease by the PIF value,
        # i.e the expected change between the reference scenario and the given scenario
        # for each age and sex category
        death_dfs <- combine_health_and_pif(pif_values = pif_scen, hc = gbd_deaths_disease)
        deaths[[deaths_name]] <- death_dfs
      }

      # repeat the above steps if a confidence interval is required
      if (conf_int) {
        for (conf_cols in c("lb", "ub")) {
          # set up variable names
          base_var <- paste0("RR_", middle_bit, reference_scenario, "_", ac, "_", conf_cols)
          scen_vars <- paste0("RR_", middle_bit, scen_names, "_", ac, "_", conf_cols)

          # subset gbd data for given disease and split into deaths and ylls
          gbd_deaths_disease <- subset(gbd_deaths, cause == gbd_dn)
          gbd_ylls_disease <- subset(gbd_ylls, cause == gbd_dn)

          # if no relative risk confidence intervals exist, use the median value instead
          if (!base_var %in% colnames(ind_ap_pa)) {
            # set the baseline
            ind_ap_pa[[base_var]] <- ind_ap_pa[[paste0("RR_", middle_bit, reference_scenario, "_", ac)]]

            for (index in 1:length(scen_vars)) { # repeat for the scenarios
              ind_ap_pa[[scen_vars[[index]]]] <-
                ind_ap_pa[[paste0("RR_", middle_bit, scen_names[index], "_", ac)]]
            }
          }

          # set up pif tables
          pif_table <- setDT(ind_ap_pa[, colnames(ind_ap_pa) %in% c(base_var, "dem_index")])
          setnames(pif_table, base_var, "outcome")
          pif_ref <- pif_table[, .(sum(outcome)), by = "dem_index"]
          setorder(pif_ref, dem_index) # sort pif_ref

          for (index in 1:length(scen_vars)) { # loop through scenarios
            # set up naming conventions
            scen <- scen_names[index]
            scen_var <- scen_vars[index]
            yll_name <- paste0(scen, "_ylls_", middle_bit, ac, "_", conf_cols)
            deaths_name <- paste0(scen, "_deaths_", middle_bit, ac, "_", conf_cols)
            # find sum of relative risks for scenario by age and sex category
            pif_table <- setDT(ind_ap_pa[, colnames(ind_ap_pa) %in% c(scen_var, "dem_index")])
            setnames(pif_table, scen_var, "outcome")
            pif_temp <- pif_table[, .(sum(outcome)), by = "dem_index"]
            # sort pif_temp
            setorder(pif_temp, dem_index)
            pif_scen <- ((pif_ref[, 2] - pif_temp[, 2]) / pif_ref[, 2]) %>% pull() # Calculate PIFs for selected scenario
            # Calculate ylls
            yll_dfs <- combine_health_and_pif(pif_values = pif_scen, hc = gbd_ylls_disease)
            ylls[[yll_name]] <- yll_dfs
            # Calculate deaths
            death_dfs <- combine_health_and_pif(pif_values = pif_scen, hc = gbd_deaths_disease)
            deaths[[deaths_name]] <- death_dfs
          }
        } # end of confidence interval loop
      } # end of confidence interval 'if' structure
    } # end of pathways loop
  } # end of disease loop

  # remove columns
  deaths <- deaths[, -which(colnames(deaths) == "dem_index")]
  ylls <- ylls[, -which(colnames(ylls) == "dem_index")]

  list(deaths = deaths, ylls = ylls)
}



#' Join disease health burden and injury data
#'
#' Join the two data frames for health burden: that from disease, and that from road-traffic injury
#'
#' This function performs the following steps:
#'
#' \itemize{
#'  \item extract the yll and deaths data from the AP and PA pathways
#'  \item extract the yll and deaths data from the injury data
#'  \item create one dataframe for yll and one for deaths containing all the AP, PA and injury data
#' }
#'
#' @param ind_ap_pa list (deaths, YLLs) of data frames of all demographic groups' burdens of diseases
#' @param inj list (deaths, YLLs) of data frames of all demographic groups' burdens for road-traffic injury
#'
#' @return list of dataframes: one for deaths per cause per demographic group, and likewise for YLLs
#'
#' @export


join_hb_and_injury <- function(ind_ap_pa, inj) {
  deaths <- ind_ap_pa$deaths
  ylls <- ind_ap_pa$ylls

  # Select deaths columns from injury data
  inj_deaths <- dplyr::select(inj, c(age_cat, sex, contains("deaths")))

  # Select yll columns from injury data
  inj_ylls <- dplyr::select(inj, c(age_cat, sex, contains("yll")))

  # Join injuries data to global deaths and yll datasets
  deaths <- dplyr::left_join(deaths, inj_deaths, by = c("age_cat", "sex"))
  ylls <- dplyr::left_join(ylls, inj_ylls, by = c("age_cat", "sex"))
  list(deaths = deaths, ylls = ylls)
}
