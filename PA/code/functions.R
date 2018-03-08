# Function to clear columns with labelled class
# taken from: https://stackoverflow.com/a/24070958
clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
    for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}


create.lookups <- function(df, cols){
  lt <- data.frame(name = NULL, id = NULL, val = NULL)
  for (i in 1:length(cols)){
    val <- as.character(unique(as_factor(df[[cols[i]]], labels = "values")))
    id <- unique(df[[cols[i]]])
    m <- matrix(c(rep(cols[i], length(id)), id, val), nrow = length(id), ncol = 3)
    lt <- rbind(lt, as.data.frame(m))
  }
  lt
}