library(ithimr)
rm(list=ls())
# options(warn=2)  # treat warnings as errors
cities <- c('bangalore', 'belo_horizonte', 'bogota', 'buenos_aires', 'cape_town',
             'delhi', 'mexico_city', 'santiago', 'sao_paulo', 'vizag')
# cities <- c('bogota')
min_age <- 15
max_age <- 69

all_inputs <- read.csv('all_city_parameter_inputs.csv',stringsAsFactors = F)

# all_inputs$cape_town_lng <- all_inputs$cape_town

parameter_names <- all_inputs$parameter
parameter_starts <- which(parameter_names!='')
parameter_stops <- c(parameter_starts[-1] - 1, nrow(all_inputs))
parameter_names <- parameter_names[parameter_names!='']
parameter_list <- list()
compute_mode <- 'constant'
for(i in 1:length(parameter_names)){
  parameter_list[[parameter_names[i]]] <- list()
  parameter_index <- which(all_inputs$parameter==parameter_names[i])
  if(all_inputs[parameter_index,2]=='')  {
    parameter_list[[parameter_names[i]]] <- lapply(cities,function(x) {
      city_index <- which(colnames(all_inputs)==x)
      val <- all_inputs[parameter_index,city_index]
      ifelse(val%in%c('T','F'),val,as.numeric(val))
    })
    names(parameter_list[[parameter_names[i]]]) <- cities
  }else if(all_inputs[parameter_index,2]=='constant'){
    indices <- 0
    if(compute_mode=='sample') indices <- 1:2
    parameter_list[[parameter_names[i]]] <- lapply(cities,function(x) {
      city_index <- which(colnames(all_inputs)==x)
      val <- all_inputs[parameter_index+indices,city_index]
      ifelse(val=='',0,as.numeric(val))
    })
    names(parameter_list[[parameter_names[i]]]) <- cities
  }else{
    parameter_list[[parameter_names[i]]] <- lapply(cities,function(x) {
      city_index <- which(colnames(all_inputs)==x)
      if(any(all_inputs[parameter_starts[i]:parameter_stops[i],city_index]!='')){
        sublist_indices <- which(all_inputs[parameter_starts[i]:parameter_stops[i],city_index]!='')
        thing <- as.list(as.numeric(c(all_inputs[parameter_starts[i]:parameter_stops[i],city_index])[sublist_indices]))
        names(thing) <- c(all_inputs[parameter_starts[i]:parameter_stops[i],2])[sublist_indices]
        thing
      }
    }
    )
    names(parameter_list[[parameter_names[i]]]) <- cities
  }
}

for(i in 1:length(parameter_list)) assign(names(parameter_list)[i],parameter_list[[i]])


###changed the bangalore transport emissions-- MC emissions from 1757 to 817 and car emissions from 4173 to 1107
##this is done based on ratio of car/MC ownership in bangalore to that of delhi from Census data (0.50 and 0.58 respectively)==
###1757=0.58*1409 and 1107=  0.50*2214
#################################################################
## run diagnostics
#for(city in cities){
#  ithim_object <- run_ithim_setup(ADD_TRUCK_DRIVERS = F,ADD_BUS_DRIVERS = F,CITY=city,MAX_MODE_SHARE_SCENARIO=T,DIST_CAT = c("0-1 km", "2-5 km", "6+ km"),PM_emission_inventory = PM_emission_inventories[[city]])
#  summarise_ithim_inputs(ithim_object)
#}


##################################################################

# constant parameters for DAY_TO_WEEK_TRAVEL_SCALAR
day_to_week_scalar <- 7

# constant parameters for MMET_CYCLING
mmet_cycling <- 4.63
# constant parameters for MMET_WALKING
mmet_walking <- 2.53
# constant parameters for SIN_EXPONENT_SUM
sin_exponent_sum <- 1.7
# constant parameters for CASUALTY_EXPONENT_FRACTION
cas_exponent <- 0.5
# add mc fleet to sp

#################################################
## without uncertainty
toplot <- matrix(0,nrow=5,ncol=length(cities)) #5 scenarios, 4 cities
ithim_objects <- list()
for(city in cities){
  print(city)
  ithim_objects[[city]] <- run_ithim_setup(DIST_CAT = c("0-1 km", "2-5 km", "6+ km"),
                                  ADD_WALK_TO_BUS_TRIPS = as.logical(add_walk_to_bus_trips[[city]]),
                                  CITY = city,
                                  AGE_RANGE = c(min_age,max_age),
                                  ADD_TRUCK_DRIVERS = F,
                                  MAX_MODE_SHARE_SCENARIO = T,
                                  ADD_BUS_DRIVERS = F,
                                  ADD_MOTORCYCLE_FLEET = as.logical(add_motorcycle_fleet[[city]]),
                                  PM_emission_inventory = PM_emission_inventories[[city]],
                                  CO2_emission_inventory = CO2_emission_inventories[[city]],
                                  speeds = speeds[[city]],
                                  
                                  FLEET_TO_MOTORCYCLE_RATIO = fleet_to_motorcycle_ratio[[city]],
                                  MMET_CYCLING = mmet_cycling, 
                                  MMET_WALKING = mmet_walking, 
                                  DAY_TO_WEEK_TRAVEL_SCALAR = day_to_week_scalar,
                                  SIN_EXPONENT_SUM= sin_exponent_sum,
                                  CASUALTY_EXPONENT_FRACTION = cas_exponent,
                                  PA_DOSE_RESPONSE_QUANTILE = F,  
                                  AP_DOSE_RESPONSE_QUANTILE = F,
                                  INJURY_REPORTING_RATE = injury_reporting_rate[[city]],  
                                  CHRONIC_DISEASE_SCALAR = chronic_disease_scalar[[city]],  
                                  PM_CONC_BASE = pm_conc_base[[city]],  
                                  PM_TRANS_SHARE = pm_trans_share[[city]],  
                                  BACKGROUND_PA_SCALAR = background_pa_scalar[[city]],
                                  BUS_WALK_TIME = bus_walk_time[[city]])
  ithim_objects$scen_prop <- SCENARIO_PROPORTIONS
  ithim_objects[[city]]$demographic <- DEMOGRAPHIC
  ithim_objects[[city]]$synth_pop <- SYNTHETIC_POPULATION
  ithim_objects[[city]]$outcomes <- run_ithim(ithim_object=ithim_objects[[city]], seed = 1)
  ithim_objects[[city]]$disease_burden <- DISEASE_BURDEN
  ithim_objects[[city]]$PM_emission_inventory <- PM_EMISSION_INVENTORY
  ithim_objects[[city]]$injury_table <- INJURY_TABLE
  
  ## store results to plot
  min_ages <- sapply(ithim_objects[[city]]$outcome$hb$ylls$age_cat,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
  max_ages <- sapply(ithim_objects[[city]]$outcome$hb$ylls$age_cat,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
  sub_outcome <- subset(ithim_objects[[city]]$outcome$hb$ylls,min_ages>=min_age&max_ages<=max_age)
  result_mat <- colSums(sub_outcome[,3:ncol(sub_outcome)])
  columns <- length(result_mat)
  nDiseases <- columns/NSCEN
  if(city==cities[1]){
    disease_list <- list()
    for(i in 1:nDiseases) disease_list[[i]] <- toplot
  }
  min_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
  max_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
  for(i in 1:nDiseases)
    disease_list[[i]][,which(cities==city)] <- result_mat[1:NSCEN + (i - 1) * NSCEN]/sum(subset(DEMOGRAPHIC,min_pop_ages>=min_age&max_pop_ages<=max_age)$population)
}

{x11(width = 10, height = 5);
  layout.matrix <- matrix(c(2:6,1,7:12), nrow =2, ncol =6,byrow=T)
  graphics::layout(mat = layout.matrix,heights = c(2,3),widths = c(2.8,2,2,2,2,2.5))
  ylim <- range(result_mat)
  cols <- rainbow(length(cities))
  mar1 <- rep(7,nDiseases); mar1[1:6] <- 1
  mar2 <- rep(1,nDiseases); mar2[c(2,7)] <- 6; mar2[c(1,12)] <- 3
  for(i in 1:nDiseases){
    ylim <- if(i%in%c(1,12)) range(disease_list[[i]]) else c(-11,4)*1e-4
    par(mar = c(mar1[i], mar2[i], 4, 1))
      barplot(t(disease_list[[i]]), ylim = ylim, las = 2,beside=T,col=cols, names.arg=if(i<7) NULL else  rownames(SCENARIO_PROPORTIONS), 
              main = paste0(last(strsplit(names(result_mat)[i * NSCEN], '_')[[1]])),yaxt='n')
    if(i%in%c(2,1,7,12)) {axis(2,cex.axis=1.5); if(i%in%c(2,7)) mtext(side=2,'YLL gain per person',line=3)}
    if(i==nDiseases-1) legend(legend=cities,fill=cols,bty='n',y=-1e-5,x=5,cex=0.9)
  }
}

## Save the ithim_object in the results folder
##########

# saveRDS(ithim_objects, paste("results/multi_city/io", CAS_EXPONENT, STR_EXPONENT, ".rds", sep = "_"),version = 2)
saveRDS(ithim_objects, "results/multi_city/io.rds", version = 2)
