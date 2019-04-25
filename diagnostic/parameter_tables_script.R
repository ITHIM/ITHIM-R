# load samples and settings
library(ithimr)
setwd('diagnostic')
parameter_samples <- readRDS('parameter_samples.Rds')
load('parameter_settings.Rdata')
source('dfSummaryrj.R')

# values between 0 and 1 for BACKGROUND_PA_CONFIDENCE
background_pa_zeros <- list()
parameter_samples_to_plot <- parameter_samples
raw_zero <- 0.5; 
for(city in cities){
  pointiness <- beta_pointiness(background_pa_confidence[[city]]);
  beta <- (1/raw_zero - 1)*pointiness*raw_zero; 
  alpha <- pointiness - beta;
  background_pa_zeros[[city]] <- c(alpha,beta)
  parameter_samples_to_plot[,which(colnames(parameter_samples_to_plot)==paste0('BACKGROUND_PA_ZEROS_',city))] <- 
    qbeta(parameter_samples[,which(colnames(parameter_samples)==paste0('BACKGROUND_PA_ZEROS_',city))],alpha,beta)
}

# values between 0 and 1 for PROPENSITY_TO_TRAVEL
pointiness <- 200
modes <- c('walking','pt','car','motorcycle','bicycle')
propensity_to_travel <- c(0.5,0.4,0.3,0.2,0.1)
names(propensity_to_travel) <- modes
# go through modes
for(i in 1:length(modes)){
  raw_zero <- propensity_to_travel[i]; 
  mod <- modes[i]
  for(city in cities){
    beta <- (1/raw_zero - 1)*pointiness*raw_zero; 
    alpha <- pointiness - beta;
    parameter_samples_to_plot[,which(colnames(parameter_samples_to_plot)==paste0('PROPENSITY_TO_TRAVEL_',mod,'_',city))] <- 
      qbeta(parameter_samples[,which(colnames(parameter_samples)==paste0('PROPENSITY_TO_TRAVEL_',mod,'_',city))],alpha,beta)
  }
}

# remove zeros from emission inventory
emission_inventory1 <- emission_inventories
tmp <- run_ithim_setup()
emission_inventory1$accra <- EMISSION_INVENTORY
for(i in 1:length(emission_inventory1))
  for(j in length(emission_inventory1[[i]]):1)
    if(emission_inventory1[[i]][[j]] == 0) emission_inventory1[[i]][[j]] <- NULL

emission_inventory2 <- lapply(emission_inventory1,function(x)sapply(x,function(y)y/sum(unlist(x))))
emission_inventory <- lapply(cities,function(x)formatC(signif(emission_inventory2[[x]]*dirichlet_pointiness(emission_confidence[[x]]),digits=2), digits=2,format="fg"))
names(emission_inventory) <- cities

# get distribution descriptions
distributions <- sapply(colnames(parameter_samples_to_plot),
                        function(x){
                          if(grepl('EMISSION_INVENTORY',x)){ city <- cities[sapply(cities,function(y)grepl(y,x))]; 
                          paste0('Dir(',paste(emission_inventory[[city]],collapse=', '),');\\\nConfidence=',emission_confidence[[city]])}
                          else if(grepl('PROPENSITY_TO_TRAVEL',x)){ city <- cities[sapply(cities,function(y)grepl(y,x))]; 
                          mod <- which(sapply(modes,function(z) grepl(z,x)))
                          paste0('E.g. p=',propensity_to_travel[mod],';\\\nPointiness=200')}
                          else if(grepl('DOSE_RESPONSE',x)) 'Uniform(0,1)' 
                          else if(x%in%normVariables) paste0('Lnorm(',sprintf('%.1f',get(tolower(x))[1]),', ',
                                                             sprintf('%.2f',get(tolower(x))[2]),')') 
                          else if(x%in%betaVariables) paste0('Beta(',sprintf('%.1f',get(tolower(x))[1]),', ',
                                                             sprintf('%.1f',get(tolower(x))[2]),')') 
                          else {city <- cities[sapply(cities,function(y)grepl(y,x))]; 
                          param <- strsplit(x,paste0('_',city))[[1]][1]; 
                          dists <- get(tolower(param))[[city]]
                          if(param=='BACKGROUND_PA_ZEROS') paste0('Confidence=',background_pa_confidence[[city]],
                                                                  ';\\\ne.g. Beta(',sprintf('%.1f',dists[1]),', ',
                                                                  sprintf('%.1f',dists[2]),')\\\nfor zeros=50%') 
                          else if(param%in%normVariables) paste0('Lnorm(',sprintf('%.1f',dists[1]),', ',
                                                                 sprintf('%.2f',dists[2]),')') 
                          else if(param%in%betaVariables) paste0('Beta(',sprintf('%.1f',dists[1]),', ',
                                                                 sprintf('%.1f',dists[2]),')')}})

# save table for all
x <- dfSummaryrj(parameter_samples_to_plot,style='grid',na.col=F,valid.col=F,distributions=distributions)
summarytools::view(x,file='parameter_table_all.html')

# city allocations
city_allocations <- sapply(colnames(parameter_samples_to_plot), function(x) sapply(cities,function(y)grepl(y,x)))
global_columns <- apply(city_allocations,2,function(x)all(x==F))

# save table for global
x <- dfSummaryrj(parameter_samples_to_plot[,global_columns],style='grid',na.col=F,valid.col=F,distributions=distributions)
summarytools::view(x,file='parameter_table_global.html')

# save tables for cities
city_allocations2 <- city_allocations[,!global_columns]
city_columns <- apply(city_allocations2,2,which)
city_parameters <- parameter_samples_to_plot[,!global_columns]
for(i in 1:length(cities)){
  x <- dfSummaryrj(city_parameters[,city_columns==i],style='grid',na.col=F,valid.col=F,distributions=distributions)
  summarytools::view(x,file=paste0('parameter_table_',cities[i],'.html'),title = city)
}

setwd('..')
