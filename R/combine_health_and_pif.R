#' Combine health and PIF
#' 
#' Applies PIF calculated from RRs to the current observed health burden from GBD to generate scenario health burden
#' 
#' @param pif_values vector of values of PIFs
#' @param hc data frame of current burden of disease
#' 
#' @return estimated scenario burden of disease
#' 
#' @export
combine_health_and_pif <- function(pif_values, hc=DISEASE_BURDEN){
#combine_health_and_pif <- function(pop, pif_values, hc=DISEASE_BURDEN, hm_cn = 'burden'){
  # pif_values are already ordered as in pop; reorder hc values to match.
  # hm_cn_values <- hc[[hm_cn]]
  # return_values <- c()
  # for (new_row in 1:nrow(pop))
  #   return_values[new_row] <- hm_cn_values[hc$sex == pop$sex[new_row] & hc$age ==  pop$age_cat[new_row] ]
  # return_values <- return_values * pif_values
  # round(return_values,5)
  # pif_values are already ordered as in pop; reorder hc values to match.
  setorder(hc,dem_index)
  hm_cn_values <- hc$burden
  return_values <- hm_cn_values * pif_values
  round(as.vector(return_values),5)
}
