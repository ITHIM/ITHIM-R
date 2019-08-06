#' Calculate population attributable fraction
#' 
#' 
#' 
#' @param pop 
#' @param cn 
#' @param mat 
#' 
#' @return population attributable fractions by demographic group
#' 
#' @export
population_attributable_fraction <- function(pop, cn, mat){
  ##!! hard coding of indices: 1=sex, 2=age or age_cat
  paf <- apply(mat,1,function(x)sum(pop[[cn]][pop[[1]]==x[1]&pop[[2]]==x[2]]))
  paf
}
