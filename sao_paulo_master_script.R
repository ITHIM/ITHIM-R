# read sd
{
  require(ithimr)
  #require(tidyverse)
  
  ithim_object <- run_ithim_setup(CITY = 'sao_paulo',
                                  ADD_WALK_TO_BUS_TRIPS = F,
                                  ADD_BUS_DRIVERS = F,
                                  ADD_TRUCK_DRIVERS = F,
                                  TEST_WALK_SCENARIO = F,
                                  BUS_WALK_TIME= 5,
                                  PM_TRANS_SHARE = 0.4,
                                  PM_emission_inventory = list(motorcycle=4,
                                                            car=4,
                                                            bus_driver=32,
                                                            big_truck=56,
                                                            truck=4,
                                                            van=0,
                                                            other=0,
                                                            taxi=0),
                                  PM_CONC_BASE = 20
                                  )
  
  ithim_object$outcomes <- run_ithim(ithim_object, seed = 1)
  
}

## plot results
{
  ## plot results
  result_mat <- colSums(ithim_object$outcome$hb$ylls[,3:ncol(ithim_object$outcome$hb$ylls)])
  columns <- length(result_mat)
  nDiseases <- columns/NSCEN
  ylim <- range(result_mat)
  x11(width = 8, height = 5); par(mfrow = c(2, 4))
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
  }
}

ithim_object$outcome <- list()

## what if: cleaner fleet !! not yet implemented
ithim_object$parameters <- ithim_setup_parameters()
ithim_object$outcome$cleaner_fleet <- run_ithim(ithim_object, seed = 1)

## what if: the roads are safer
ithim_object$parameters <- ithim_setup_parameters(INJURY_REPORTING_RATE = 2)
ithim_object$outcome$safety <- run_ithim(ithim_object, seed = 1)

## what if: the rate of chronic disease doubles
ithim_object$parameters <- ithim_setup_parameters(CHRONIC_DISEASE_SCALAR = 2)
ithim_object$outcome$chronic_disease <- run_ithim(ithim_object, seed = 1)

## what if: non-transport air pollution is half
ithim_object$parameters <- ithim_setup_parameters(PM_CONC_BASE = 30.625, PM_TRANS_SHARE = 0.3673469)
ithim_object$outcome$background_ap <- run_ithim(ithim_object, seed = 1)

## what if: non-transport physical activity is half
ithim_object$parameters <- ithim_setup_parameters(BACKGROUND_PA_SCALAR = 0.5)
ithim_object$outcome$background_pa <- run_ithim(ithim_object, seed = 1)


## plot results
result_list <- lapply(ithim_object$outcome,function(x)colSums(x$hb$ylls[,3:ncol(x$hb$ylls)]))
result_mat <- do.call(rbind, result_list)
columns <- ncol(result_mat)
nDiseases <- columns/NSCEN
for(i in 1:nDiseases){
  x11(width = 8, height = 5); par(mfrow = c(2, 3))
  ylim <- range(result_mat[, 1:NSCEN+(i-1)*NSCEN])
  for(j in 1:nrow(result_mat)){
    if(j<4) {
      par(mar = c(1, 4, 4, 1))
      barplot(result_mat[j, 1:NSCEN + (i - 1) * NSCEN], names.arg = '', ylim = ylim, las = 2, 
              main = paste0(rownames(result_mat)[j], ', ', last(strsplit(colnames(result_mat)[i * NSCEN], '_')[[1]])))
    }else{
      par(mar = c(5, 4, 4, 1))
      barplot(result_mat[j, 1:NSCEN + (i - 1) * NSCEN], names.arg = SCEN_SHORT_NAME[c(1, 3:6)], ylim = ylim, las = 2, 
              main = paste0(rownames(result_mat)[j], ',  ', last(strsplit(colnames(result_mat)[i * NSCEN], '_')[[1]])))
    }
  }
}


td <- reshape2::melt(result_mat)
td <- td %>% dplyr::mutate(scen = stringr::str_extract(Var2, "[^_]+"))
td <- td %>% dplyr::mutate(var = stringr::str_extract(Var2, "[^_]+$"))
ggplot(data = td, aes(x = var, y = value, fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge2", colour = "black", alpha = 0.5) + theme_minimal() + facet_wrap(~Var1)


cols <- c("cleaner_fleet" = "#e41a1c", 
                "safety" = "#377eb8", 
                "chronic_disease" = "#4daf4a", 
                "background_ap" = "#984ea3",
                "background_pa" = "#80b1d3")#, 
#"Scenario 5" = "#cc4c02")

require(ggplot2)
require(ggthemes)
plotly::ggplotly(
  ggplot(data = td) +
    aes(x = var, fill = Var1, weight = value) +
    geom_bar(position = "dodge2", color = 'black', alpha = 0.5) +
    scale_fill_manual(values = cols) +
    labs(title = paste0("Reduction in Years of Life Lost (YLL)"), y = paste('<- Harms     Benefits ->'), x = "") +
    theme_solarized_2() +
    theme(legend.position = 'top') +
    guides(fill = guide_legend(title = "Secular Trends"))
)
