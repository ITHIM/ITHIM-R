#' Map injury death burden to YLL (years of life lost) burden
#'
#' Calculate the YLL burden from the death burden of injury based on the ratio in the GBD data.
#'
#' The function performs the following steps:
#'
#' \itemize{
#'  \item join the estimated injury deaths with the global burden of disease (GBD) injury data by age and sex
#'
#'  \item multiply the estimated injury deaths by the yll to injury death ratio in the GBD data to predict YLL
#'    from the estimated injury deaths
#'
#'  \item extract and create matrices for deaths and ylls with one column for each scenario
#'
#'  \item create dataframe A with ylls and deaths of reference scenario
#'
#'  \item create dataframe B showing the differences in deaths and yll for each non-reference
#'    scenario to the reference scenario
#'
#'  \item if confidence intervals are required:
#'
#'  \itemize{
#'    \item create dataframe with ylls and deaths of reference scenario using both the lower and upper
#'      relative risk boundary values
#'
#'    \item create dataframe showing the differences in deaths and yll for each non-reference
#'      scenario to the reference scenario using both the lower and upper relative
#'      risk boundary values
#'
#'    \item add the confidence upper and lower interval boundary values to the two output dataframes A and B
#'    }
#' }
#'
#' @param injuries data frame of injury deaths by age and sex category for each scenario incl. baseline
#'
#' @return list of injury deaths and YLLs (given as differences to reference scenario) plus the values in reference scenario
#'
#' @export


injury_death_to_yll <- function(injuries) {
  # join the injury deaths with the YLLs lost due to injuries in the global burden of disease data
  joined_injury <- dplyr::left_join(injuries, GBD_INJ_YLL[, c("sex_age", "sex", "yll_dth_ratio")], by = c("sex_age", "sex"))

  # multiply the injury deaths by the yll to injury death ratio in the global burden of disease injury data
  joined_injury$YLL <- joined_injury$Deaths * joined_injury$yll_dth_ratio
  # extract columns of interest
  death_and_yll <- dplyr::select(joined_injury, c("age_cat", "sex", "scenario", "Deaths", "YLL"))

  # extract and create matrices for deaths and ylls with one column for each scenario
  x_deaths <- dplyr::select(death_and_yll, -YLL)
  # create one column for age_cat, sex and each scenario (incl baseline) for deaths
  x_deaths <- spread(x_deaths, scenario, Deaths)
  x_yll <- dplyr::select(death_and_yll, -Deaths)
  # create one column for age_cat, sex and each scenario (incl baseline) for ylls
  x_yll <- spread(x_yll, scenario, YLL)

  # set reference and other scenarios
  ref_scen <- REFERENCE_SCENARIO
  if (REFERENCE_SCENARIO == "Baseline") {
    ref_scen <- "baseline"
  }
  ref_scen_index <- which(SCEN == ref_scen)
  calc_scen <- SCEN[SCEN != ref_scen]
  # find indexes of columns in x_deaths (and therefore also x_yll) data that belong to the non-reference scenarios
  calc_scen_index <- which(colnames(x_deaths) %in% calc_scen)

  # create dataframe with ylls and deaths of reference scenario
  ref_injuries <- as.data.frame(cbind(x_deaths[, 1:2], deaths = x_deaths[[ref_scen]], ylls = x_yll[[ref_scen]]))

  # calculate the differences in injury deaths between the non-reference and the reference scenario
  deaths <- t(repmat(unlist(ref_injuries$deaths), NSCEN, 1)) - x_deaths[, calc_scen_index, drop = F]
  names(deaths) <- paste0(names(deaths), "_deaths_inj")
  # Reordering columns to avoid mistakes later on
  deaths <- deaths[, paste0(SCEN_SHORT_NAME[-1], "_deaths_inj")]

  # calculate the differences in injury ylls between the non-reference and the reference scenario
  ylls <- t(repmat(unlist(ref_injuries$ylls), NSCEN, 1)) - x_yll[, calc_scen_index, drop = F]
  names(ylls) <- paste0(names(ylls), "_yll_inj")
  # Reordering columns to avoid mistakes later on
  ylls <- ylls[, paste0(SCEN_SHORT_NAME[-1], "_yll_inj")]

  # create one dataframe showing the differences in deaths and yll for each non-reference
  # scenario to the reference scenario
  deaths_yll_injuries <- as.data.frame(cbind(as.data.frame(x_deaths[, 1:2]), deaths, ylls))

  # update columns names of deaths_yll_injuries to state whether the column shows injury deaths or ylls
  # Dan: I am commenting these lines because we were making the mistake of calling
  # these scenarios as cycle, car, bus (see SCEN_SHORT_NAME), but the real
  # order was bus, car, cycle (see names(deaths)).
  # metric <- c("deaths", "yll")
  # k <- 1
  # for  (i in 1: 2)
  #   for (j in c(1:(NSCEN+1))[-ref_scen_index]){
  #     names(deaths_yll_injuries)[2+k] <- paste0(SCEN_SHORT_NAME[j],"_",metric[i],"_inj")
  #     k<-k+1
  #   }

  # Repeat the above logic for lower and upper confidence interval values if they exist
  if (any(colnames(injuries) %in% c("Deaths_lb", "Deaths_ub"))) {
    ## lower interval boundary
    # join the injury deaths with the YLLs lost due to injuries in the global burden of disease data
    joined_injury_lb <- dplyr::left_join(injuries, GBD_INJ_YLL[, c("sex_age", "sex", "yll_dth_ratio")], by = c("sex_age", "sex"))

    # multiply the injury deaths by the yll to injury death ratio in the global burden of disease injury data
    joined_injury_lb$YLL_lb <- joined_injury_lb$Deaths_lb * joined_injury_lb$yll_dth_ratio
    death_and_yll_lb <- dplyr::select(joined_injury_lb, c("age_cat", "sex", "scenario", "Deaths_lb", "YLL_lb"))

    # extract and create matrices for deaths and ylls with one column for each scenario
    x_deaths_lb <- dplyr::select(death_and_yll_lb, -YLL_lb)
    x_deaths_lb <- spread(x_deaths_lb, scenario, Deaths_lb)
    x_yll_lb <- dplyr::select(death_and_yll_lb, -Deaths_lb)
    x_yll_lb <- spread(x_yll_lb, scenario, YLL_lb)

    # set reference and other scenarios
    ref_scen_lb <- REFERENCE_SCENARIO
    ref_scen_index_lb <- which(SCEN == ref_scen_lb)
    calc_scen <- SCEN[SCEN != ref_scen_lb]
    calc_scen_index <- which(colnames(x_deaths_lb) %in% calc_scen)

    # create one dataframe showing the differences in deaths and yll for each non-reference scenario to the
    # reference scenario
    ref_injuries_lb <- as.data.frame(cbind(x_deaths_lb[, 1:2], deaths_lb = x_deaths_lb[[ref_scen_lb]], ylls_lb = x_yll_lb[[ref_scen_lb]]))

    deaths_lb <- t(repmat(unlist(ref_injuries_lb$deaths_lb), NSCEN, 1)) - x_deaths_lb[, calc_scen_index, drop = F]
    names(deaths_lb) <- paste0(names(deaths_lb), "_deaths_inj_lb")
    # Reordering columns to avoid mistakes later on
    deaths_lb <- deaths_lb[, paste0(SCEN_SHORT_NAME[-1], "_deaths_inj_lb")]

    ylls_lb <- t(repmat(unlist(ref_injuries_lb$ylls_lb), NSCEN, 1)) - x_yll_lb[, calc_scen_index, drop = F]
    names(ylls_lb) <- paste0(names(ylls_lb), "_yll_inj_lb")
    # Reordering columns to avoid mistakes later on
    ylls_lb <- ylls_lb[, paste0(SCEN_SHORT_NAME[-1], "_yll_inj_lb")]

    deaths_yll_injuries_lb <- as.data.frame(cbind(as.data.frame(x_deaths_lb[, 1:2]), deaths_lb, ylls_lb))

    # update columns names of deaths_yll_injuries to state whether the column shows injury deaths or ylls
    # Dan: I am commenting these lines because we were making the mistake of calling
    # these scenarios as cycle, car, bus (see SCEN_SHORT_NAME), but the real
    # order was bus, car, cycle (see names(deaths_lb)).
    # metric <- c("deaths", "yll")
    # k <- 1
    # for  (i in 1: 2)
    #   for (j in c(1:(NSCEN+1))[-ref_scen_index_lb]){
    #
    #     names(deaths_yll_injuries_lb)[2+k] <- paste0(SCEN_SHORT_NAME[j], "_", metric[i], "_inj_lb")
    #     k<-k+1
    #   }



    ## upper confidence interval boundary
    # join the injury deaths with the YLLs lost due to injuries in the global burden of disease data
    joined_injury_ub <- dplyr::left_join(injuries, GBD_INJ_YLL[, c("sex_age", "sex", "yll_dth_ratio")], by = c("sex_age", "sex"))

    # multiply the injury deaths by the yll to injury death ratio in the global burden of disease injury data
    joined_injury_ub$YLL_ub <- joined_injury_ub$Deaths_ub * joined_injury_ub$yll_dth_ratio
    death_and_yll_ub <- dplyr::select(joined_injury_ub, c("age_cat", "sex", "scenario", "Deaths_ub", "YLL_ub"))

    # extract and create matrices for deaths and ylls with one column for each scenario
    x_deaths_ub <- dplyr::select(death_and_yll_ub, -YLL_ub)
    x_deaths_ub <- spread(x_deaths_ub, scenario, Deaths_ub)
    x_yll_ub <- dplyr::select(death_and_yll_ub, -Deaths_ub)
    x_yll_ub <- spread(x_yll_ub, scenario, YLL_ub)

    # set reference and other scenarios
    ref_scen_ub <- REFERENCE_SCENARIO
    ref_scen_index_ub <- which(SCEN == ref_scen_ub)
    calc_scen <- SCEN[SCEN != ref_scen_ub]
    calc_scen_index <- which(colnames(x_deaths_ub) %in% calc_scen)

    # create one dataframe showing the differences in deaths and yll for each non-reference scenario to the
    # reference scenario
    ref_injuries_ub <- as.data.frame(cbind(x_deaths_ub[, 1:2], deaths_ub = x_deaths_ub[[ref_scen_ub]], ylls_ub = x_yll_ub[[ref_scen_ub]]))
    deaths_ub <- t(repmat(unlist(ref_injuries_ub$deaths_ub), NSCEN, 1)) - x_deaths_ub[, calc_scen_index, drop = F]
    names(deaths_ub) <- paste0(names(deaths_ub), "_deaths_inj_ub")
    # Reordering columns to avoid mistakes later on
    deaths_ub <- deaths_ub[, paste0(SCEN_SHORT_NAME[-1], "_deaths_inj_ub")]

    ylls_ub <- t(repmat(unlist(ref_injuries_ub$ylls_ub), NSCEN, 1)) - x_yll_ub[, calc_scen_index, drop = F]
    names(ylls_ub) <- paste0(names(ylls_ub), "_yll_inj_ub")
    # Reordering columns to avoid mistakes later on
    ylls_ub <- ylls_ub[, paste0(SCEN_SHORT_NAME[-1], "_yll_inj_ub")]

    deaths_yll_injuries_ub <- as.data.frame(cbind(as.data.frame(x_deaths_ub[, 1:2]), deaths_ub, ylls_ub))

    # update columns names of deaths_yll_injuries to state whether the column shows injury deaths or ylls
    # Dan: I am commenting these lines because we were making the mistake of calling
    # these scenarios as cycle, car, bus (see SCEN_SHORT_NAME), but the real
    # order was bus, car, cycle (see names(deaths_lb)).
    # metric <- c("deaths", "yll")
    # k <- 1
    # for  (i in 1: 2)
    #   for (j in c(1:(NSCEN+1))[-ref_scen_index_ub]){
    #
    #     names(deaths_yll_injuries_ub)[2+k] <- paste0(SCEN_SHORT_NAME[j], "_", metric[i], "_inj_ub")
    #     k<-k+1
    #   }

    # add lower and upper boundary values to the reference and the yll and deaths injury differences datasets
    deaths_yll_injuries <- left_join(deaths_yll_injuries, deaths_yll_injuries_ub, by = c("age_cat", "sex"))
    deaths_yll_injuries <- left_join(deaths_yll_injuries, deaths_yll_injuries_lb, by = c("age_cat", "sex"))

    ref_injuries <- left_join(ref_injuries, ref_injuries_ub, by = c("age_cat", "sex"))
    ref_injuries <- left_join(ref_injuries, ref_injuries_lb, by = c("age_cat", "sex"))
  }

  list(deaths_yll_injuries = deaths_yll_injuries, ref_injuries = ref_injuries)
}
