# load samples and settings
setwd('diagnostic')
parameter_samples <- readRDS('parameter_samples.Rds')
load('parameter_settings.Rdata')
source('dfSummaryrj.R')
library(summarytools)
library(ithimr)
align_numbers_dfs <<- summarytools:::align_numbers_dfs
txtbarplot <<- summarytools:::txtbarplot

# values between 0 and 1 for BACKGROUND_PA_CONFIDENCE
background_pa_zeros <- list()
parameter_samples_to_plot <- parameter_samples
parameter_samples_to_plot <- parameter_samples_to_plot[,which(colnames(parameter_samples_to_plot)!=paste0('BUS_WALK_TIME'))]
raw_zero <- 0.5; 
for(city in cities){
  pointiness <- beta_pointiness(background_pa_confidence[[city]]);
  beta <- (1/raw_zero - 1)*pointiness*raw_zero; 
  alpha <- pointiness - beta;
  background_pa_zeros[[city]] <- c(alpha,beta)
  parameter_samples_to_plot[,which(colnames(parameter_samples_to_plot)==paste0('BACKGROUND_PA_ZEROS_',city))] <- 
    qbeta(parameter_samples[,which(colnames(parameter_samples)==paste0('BACKGROUND_PA_ZEROS_',city))],alpha,beta)
}

# remove zeros from emission inventory
PM_emission_inventory1 <- PM_emission_inventories
tmp <- run_ithim_setup()
PM_emission_inventory1$accra <- EMISSION_INVENTORY
for(i in 1:length(PM_emission_inventory1))
  for(j in length(PM_emission_inventory1[[i]]):1)
    if(PM_emission_inventory1[[i]][[j]] == 0) PM_emission_inventory1[[i]][[j]] <- NULL

PM_emission_inventory2 <- lapply(PM_emission_inventory1,function(x)sapply(x,function(y)y/sum(unlist(x))))
PM_emission_inventory <- lapply(cities,function(x)formatC(signif(PM_emission_inventory2[[x]]*dirichlet_pointiness(emission_confidence[[x]]),digits=2), digits=2,format="fg"))
names(PM_emission_inventory) <- cities

## remove DOSE_RESPOSE parameters
parameter_samples_to_plot <- parameter_samples_to_plot[,!sapply(colnames(parameter_samples_to_plot),function(x)grepl('DOSE_RESPONSE',x))]

# get distribution descriptions
distributions <- sapply(colnames(parameter_samples_to_plot),
                        function(x){
                          if(grepl('EMISSION_INVENTORY',x)){ city <- cities[sapply(cities,function(y)grepl(y,x))]; 
                          paste0('Dir(',paste(PM_emission_inventory[[city]],collapse=', '),');\\\nConfidence=',emission_confidence[[city]])}
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
x <- dfSummaryrj(parameter_samples_to_plot,style='grid',na.col=F,valid.col=F,distributions=distributions,col.widths=c(5,100,100,20000))
#x <- dfSummary(parameter_samples_to_plot,plain.ascii = FALSE, style = "grid",graph.magnif = 0.75, valid.col = FALSE)
summarytools::view(x,file='parameter_table_all.html')
if(file.exists('dr_curves.html')){
  file.copy('dr_curves.html', 'dr_curves_plus_all_parameters.html')
  summarytools::view(x,file='dr_curves_plus_all_parameters.html',append=T)
}

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
city_distributions <- distributions[!global_columns]
for(i in 1:length(cities)){
  x <- dfSummaryrj(city_parameters[,city_columns==i],style='grid',na.col=F,valid.col=F,distributions=city_distributions[city_columns==i])
  summarytools::view(x,file=paste0('parameter_table_',cities[i],'.html'),title = city)
}

setwd('..')




## to append:
## iconv -s -t utf-8 parameter_table_all.html dr_curves.html | pandoc -s -f html -t html -o parameter_table_all_plus_dr.html
