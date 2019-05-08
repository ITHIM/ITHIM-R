#' @export
saveRDS <- function(x,filename,version=2,...){
  base::saveRDS(x,file=filename,version=version,...)
}