#' Combine health and potential impact fraction (PIF)
#'
#' Applies PIF calculated from relative risks (RRs) to the current observed health burden from the
#' Global Burden of Disease data to generate the scenario health burdens
#'
#' This function performs the following steps:
#'
#' \itemize{
#' \item the current observed health burden for a particular disease is multiplied by the PIF, i.e
#'   the change in fraction in disease expected for the current scenario compared with the
#'   reference scenario
#' }
#'
#' @param pif_values vector of values of PIFs for all age and sex categories
#' @param hc data frame of current burden of disease
#'
#' @return estimated scenario burden of disease for all age and sex categories
#'
#' @export
combine_health_and_pif <- function(pif_values, hc = DISEASE_BURDEN) {
  setorder(hc, dem_index)
  hm_cn_values <- hc$burden
  return_values <- hm_cn_values * pif_values # multiply burden of disease times PIF
  round(as.vector(return_values), 5)
}
