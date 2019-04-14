#' @export
population_attributable_fraction <- function(pop, cn, mat){
  ##!! would be faster as data.table
  paf <- sapply(mat$dem_index,function(x)sum(pop[[cn]][pop$dem_index==x]))
  paf
}
