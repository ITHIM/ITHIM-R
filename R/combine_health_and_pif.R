#' @export
combine_health_and_pif <- function(pif_values, hc=DISEASE_BURDEN){
#combine_health_and_pif <- function(pop, pif_values, hc=DISEASE_BURDEN, hm_cn = 'burden'){
  # pif_values are already ordered as in pop; reorder hc values to match.
  setorder(hc,dem_index)
  hm_cn_values <- hc$burden
  return_values <- hm_cn_values * pif_values
  round(as.vector(return_values),5)
}
