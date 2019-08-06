#' Reduce size of glm object
#' 
#' Delete some attributes of glm object in order to save space
#' 
#' @param obj glm object
#' 
#' @return glm object
#' 
#' @export
trim_glm_object <- function(obj){
  obj$y <- c()
  obj$model <- c()
  obj$R <- c()
  obj$qr$qr <- c()
  obj$residuals <- c()
  obj$fitted.values <- c()
  obj$effects <- c()
  obj$linear.predictors <- c()
  obj$weights <- c()
  obj$prior.weights <- c()
  obj$data <- c()
  obj$family$variance = c()
  obj$family$dev.resids = c()
  obj$family$aic = c()
  obj$family$validmu = c()
  obj$family$simulate = c()
  #attr(obj$terms,".Environment") = c()
  attr(obj$formula,".Environment") = c()
  obj
}
