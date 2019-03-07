# Clear workspace
rm (list = ls())

{
  require(ithimr)
  #require(tidyverse)
  
  ithim_object <- run_ithim_setup(CITY = 'delhi',
                                  TEST_WALK_SCENARIO = T,
                                  ADD_WALK_TO_BUS_TRIPS = F,
                                  ADD_TRUCK_DRIVERS = F,
                                  ADD_BUS_DRIVERS = F)
  

  ithim_object$outcomes <- run_ithim(ithim_object, seed = 1)
  
}