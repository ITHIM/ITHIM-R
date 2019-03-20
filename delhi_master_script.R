# Clear workspace
rm (list = ls())


require(ithimr)
#require(tidyverse)

io <- run_ithim_setup(CITY = 'delhi',
                                TEST_WALK_SCENARIO = T,
                                ADD_WALK_TO_BUS_TRIPS = F,
                                ADD_TRUCK_DRIVERS = F,
                                ADD_BUS_DRIVERS = F,
                                speeds = list(subway = 32,
                                              bicycle = 15),
                                emission_inventory = list(motorcycle = 1409,
                                                          auto_rickshaw = 133,
                                                          car = 2214,
                                                          bus_driver = 644,
                                                          big_truck = 4624,
                                                          truck = 3337,
                                                          van = 0,
                                                          other = 0,
                                                          taxi = 0))

io$outcomes <- run_ithim(io, seed = 1)


## plot results for YLLs
result_mat <- colSums(io$outcome$hb$ylls[,3:ncol(io$outcome$hb$ylls)])
columns <- length(result_mat)
nDiseases <- columns/NSCEN
ylim <- range(result_mat)
{x11(width = 8, height = 8); par(mfrow = c(3, 4))
  for(i in 1:nDiseases){
    if(i<5) {
      par(mar = c(1, 4, 4, 1))
      barplot(result_mat[1:NSCEN + (i - 1) * NSCEN], names.arg = '', ylim = ylim, las = 2, 
              main = paste0(last(strsplit(names(result_mat)[i * NSCEN], '_')[[1]])))
    }else{
      par(mar = c(5, 4, 4, 1))
      barplot(result_mat[1:NSCEN + (i - 1) * NSCEN], names.arg = SCEN_SHORT_NAME[c(1, 3:6)], ylim = ylim, las = 2, 
              main = paste0( last(strsplit(names(result_mat)[i * NSCEN], '_')[[1]])))
    }
  }}

