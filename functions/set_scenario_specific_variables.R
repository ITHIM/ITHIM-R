set_scenario_specific_variables <- function(rd){
  NSCEN <<- length(unique(rd$scenario)) - 1
  SCEN <<- unique(rd$scenario)
  SCEN_SHORT_NAME <<- c("base",paste0("scen", 1:NSCEN) )
}
