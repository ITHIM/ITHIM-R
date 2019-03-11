# Clear workspace
rm (list = ls())

{
  require(ithimr)
  #require(tidyverse)
  
  ithim_object <- run_ithim_setup(CITY = 'delhi',
                                  TEST_WALK_SCENARIO = T,
                                  ADD_WALK_TO_BUS_TRIPS = F,
                                  ADD_TRUCK_DRIVERS = F,
                                  ADD_BUS_DRIVERS = F,
                                  speeds = list(subway=32,
                                                bicycle=15),
                                  emission_inventory = list(motorcycle=1409,
                                                            auto_rickshaw=133,
                                                            car=2214,
                                                            bus_driver=644,
                                                            big_truck=4624,
                                                            truck=3337,
                                                            van=0,
                                                            other=0,
                                                            taxi=0))
  

  ithim_object$outcomes <- run_ithim(ithim_object, seed = 1)
  
}