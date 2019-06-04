library(ithimr)

parameter_samples <- readRDS('diagnostic/parameter_samples.Rds')
outcome <- readRDS('results/multi_city/outcome.Rds')

cities <- names(outcome)[1:4]
sources <- list()
for(ci in 1:length(cities)){
  city <- cities[ci]
  emission_names <- sapply(colnames(parameter_samples),function(x)grepl('EMISSION_INVENTORY_',x)&grepl(city,x))
  sources[[ci]] <- parameter_samples[,emission_names]
}
evppi_for_emissions <- mclapply(sources, 
                                FUN = multi_city_parallel_evppi_for_emissions,
                                outcome, 
                                mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))

names(evppi_for_emissions) <- paste0('EMISSION_INVENTORY_',cities)

print(do.call(rbind,evppi_for_emissions)))




