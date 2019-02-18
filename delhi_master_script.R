# Clear workspace
rm (list = ls())

{
  require(ithimr)
  #require(tidyverse)
  
  ithim_object <- run_ithim_setup(CITY = 'delhi',
                                  ADD_WALK_TO_BUS_TRIPS = F,
                                  ADD_BUS_DRIVERS = F,
                                  ADD_TRUCK_DRIVERS = F,
                                  TEST_WALK_SCENARIO = F,
                                  BUS_WALK_TIME= 5,
                                  PM_TRANS_SHARE = 0.225
  )
  
  ithim_object$outcomes <- run_ithim(ithim_object, seed = 1)
  
}