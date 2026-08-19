#' Wrapper for running ITHIM
#'
#' Switch to either calculate the health burden using the constant input parameter values or to sample from distributions first
#'
#' This function works by creating a switch to run the computation by calling
#' \code{\link{ithim_calculation_sequence()}} directly, or to divert to the sampling case
#' \code{\link{ithim_uncertainty()}} which first extracts the sampled parameters and then calls
#' the \code{\link{ithim_calculation_sequence()}}.
#'
#'
#' @param ithim_object list of input data needed to calculate the health burden
#' @param seed random seed
#'
#' @return ithim_object list of items giving the input data and output results
#'
#' @export


run_ithim <- function(ithim_object, seed = 1) {
  if (length(ithim_object$parameters) > 0) { # if running in sampling modes, call ithim_uncertainty()
    ithim_object <- ithim_uncertainty(ithim_object, seed)
  } else {
    ithim_object <- ithim_calculation_sequence(ithim_object, seed)
  }
  return(ithim_object)
}






#' Cascade of computations that form ITHIM-Global
#'
#' Ordered set of computations as part of ITHIM-Global that calculates the required output parameters
#'
#' This function performs the following steps:
#'
#' \enumerate{
#'
#' \item extract all lists and variables from the ithim_object list
#'
#' \item  air pollution pathway:
#'    \itemize{
#'    \item calculate the PM2.5 exposure for each person in the baseline population
#'      and PM2.5 emissions for each mode and scenario (\code{\link{scenario_pm_calculations()}})
#'
#'    \item calculate the CO2 emissions for each mode and scenario (\code{\link{scenario_co2_calculations()}})
#'
#'    \item assign relative risk to each person in the baseline population for each disease
#'      related to PM pollution and each scenario based on the individual PM exposure
#'      levels (\code{\link{gen_ap_rr()}})
#'      }
#'
#' \item  physical activity pathway:
#'    \itemize{
#'    \item calculate total mMETs for each person in the baseline population (\code{\link{total_mmet()}})
#'
#'    \item assign relative risk to each person in the baseline population for each disease
#'      related to physical activity levels and each scenario based on the individual mMET
#'      values (\code{\link{gen_pa_rr()}})
#'    }
#'
#' \item  physical activity and air pollution combined:
#'    \itemize{
#'    \item combine the PA and AP datasets by joining the two datasets. For disease affected by
#'      both PA and AP calculate the joined relative risk by multiplying the PA and AP
#'      relative risks (\code{\link{combined_rr_ap_pa()}})
#'
#'    \item calculate the health burden (Yll and deaths) for each disease and age and sex
#'      category (\code{\link{health_burden()}}):
#'      \itemize{
#'      \item calculate the health burden (Yll and deaths) for each disease and age
#'        and sex category. Combine the AP and PA pathways for diseases affected
#'        by both AP and PA
#'
#'      \item if running in constant mode also calculate the health burden for both the
#'        AP and PA pathways separately
#'        }
#'     }
#'
#'
#' \item  combine all pathways using the outputs from 3. and 4.:
#'    \itemize{
#'    \item combine the AP, PA and health burden data for ylls and deaths
#'      for all diseases and scenarios
#'      }
#' }
#'
#'
#' @param ithim_object name of disease
#' @param seed random seed
#'
#' @return ithim_object - list of items making up the ithim result
#'
#' @export


ithim_calculation_sequence <- function(ithim_object, seed = 1) {
  ############################
  ## (0) SET UP
  set.seed(seed)
  for (i in 1:length(ithim_object)) {
    assign(names(ithim_object)[i], ithim_object[[i]])
  }
  constant_mode <- length(parameters) == 0
  ithim_object <- NULL

  ############################
  ## (1) AP PATHWAY
  # Calculate PM2.5 emissions for each mode and scenario and calculate PM2.5
  # exposure for each person in the baseline population
  
  pm_conc <- scenario_pm_calculations(
    dist = (true_dist |>  
              dplyr::filter(stage_mode != "unknown") |> 
              mutate_if(is.numeric, ~ (.x/DAY_TO_WEEK_TRAVEL_SCALAR)) |> 
              dplyr::mutate_at(-c(1), as.integer)),
    trip_scen_sets = trip_scen_sets
  ) # 3

  # Calculate the CO2 emissions for each mode and scenario
  co2_emission_inventory <- scenario_co2_calculations(dist = (true_dist %>% dplyr::filter(stage_mode != "unknown")
    %>% dplyr::mutate_at(-c(1), as.integer)))



  scenario_pm <- pm_conc$scenario_pm
  pm_conc_pp <- pm_conc$pm_conc_pp
  pm_conc <- NULL

  # Assign relative risks to each person in the baseline population for each disease
  # related to air pollution and each scenario based on the individual PM exposure levels
  RR_AP_calculations <- gen_ap_rr(pm_conc_pp)
  if (!constant_mode) pm_conc_pp <- NULL


  ############################
  ## (2) PA PATHWAY
  # Physical activity calculations

  # calculate total mMETs for each person in the baseline population
  mmets_pp <- total_mmet(trip_scen_sets)
  trip_scen_sets <- NULL

  # assign a relative risk to each person in the baseline population for each disease
  # related to physical activity levels and each scenario based on the individual mMET values
  RR_PA_calculations <- gen_pa_rr(mmets_pp,
    conf_int = ifelse(constant_mode, TRUE, FALSE)
  )
  if (!constant_mode) mmets_pp <- NULL


  ############################
  ## (3) COMBINE (1) AND (2)
  # Physical activity and air pollution combined

  # create one dataframe containing both the PA, the AP and the combined PA and AP relative risks
  # (for those diseases affected by both PA and AP) for all people in the baseline population and all scenarios
  RR_PA_AP_calculations <- combined_rr_ap_pa(
    ind_pa = RR_PA_calculations, ind_ap = RR_AP_calculations,
    conf_int = ifelse(constant_mode, TRUE, FALSE)
  )

  RR_PA_calculations <- NULL
  RR_AP_calculations <- NULL

  # calculate the health burden (Yll and deaths) for each disease and age and sex category
  # by combining the AP and PA pathways for diseases affected by both AP and PA
  hb_AP_PA <- health_burden(ind_ap_pa = RR_PA_AP_calculations, conf_int = ifelse(constant_mode, TRUE, FALSE))

  # if running in constant mode calculate the health burden (Yll and deaths) for each disease and age and sex category
  # for each pathway (AP and PA) independently
  if (constant_mode) {
    pathway_hb_AP_PA <- health_burden(RR_PA_AP_calculations,
      conf_int =
        ifelse(constant_mode, TRUE, FALSE), combined_AP_PA = FALSE
    )
  }
  RR_PA_AP_calculations <- NULL

  # return lists
  if (constant_mode) {
    #pathway_hb <- join_hb_and_injury(pathway_hb_AP_PA, deaths_yll_injuries$deaths_yll_injuries)
    return(list(
      mmets = mmets_pp, scenario_pm = scenario_pm, pm_conc_pp = pm_conc_pp, co2_emission_inventory = co2_emission_inventory,
      #ref_injuries = ref_injuries, 
      hb = hb_AP_PA, pathway_hb = pathway_hb_AP_PA #, whw = whw
    ))
  } else {
    return(list(hb = hb_AP_PA)) #, ref_injuries = ref_injuries))
  }
}
