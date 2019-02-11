#' @export
error_handling <- function(code, function_name, variable_name) {
  output <-
    switch(
      code,
      paste0(
        "In ",
        function_name,
        "(",
        variable_name,
        "): Default values should be changed!"
      )
    )
  
  print(output)
  
}