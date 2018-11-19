# Programmed by Javad Rahimipour Anaraki on 09/11/18
# Ph.D. Candidate
# Department of Computer Science
# Memorial University of Newfoundland
# jra066 [AT] mun [DOT] ca | www.cs.mun.ca/~jra066

#   input: Error/Warning code, function name, and valiable name
#  output: Error/Warning message

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