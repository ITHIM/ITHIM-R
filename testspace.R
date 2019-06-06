library(ithimr)

NSCEN <- 5

parameter_samples <- readRDS('diagnostic/parameter_samples.Rds')
outcome <- readRDS('results/multi_city/outcome.Rds')
evppi <- readRDS('results/multi_city/evppi.Rds')
numcores <- 8
cities <- names(outcome)[1:4]
sources <- list()
for(ci in 1:length(cities)){
  city <- cities[ci]
  emission_names <- sapply(colnames(parameter_samples),function(x)grepl('EMISSION_INVENTORY_',x)&grepl(city,x))
  sources[[ci]] <- parameter_samples[,emission_names]
}

evppi_for_emissions <- matrix(0,nrow=length(sources),ncol=ncol(evppi))
rownames(evppi_for_emissions) <- paste0('EMISSION_INVENTORY_',cities)
colnames(evppi_for_emissions) <- colnames(evppi)
calcflag <- sapply(colnames(evppi_for_emissions),function(y)
  sapply(rownames(evppi_for_emissions),function(x){city <- strsplit(x,'EMISSION_INVENTORY_')[[1]][2]; grepl(city,y)|grepl('combined',y)})
)

parallel_evppi_for_emissions <- function(index,outcome,sources){
  voi <- c()
  cases_scen <- NSCEN*length(sources)
  outcome_index <- floor((index-1)/cases_scen) + 1
  scen_index <- floor((index - (outcome_index-1)*cases_scen - 1)/length(sources)) + 1
  source_index <- (index-1)%%length(sources)+1
  
  inputs <- sources[[source_index]]
  nSources <- ncol(inputs)
  for(i in 1:nSources)
    assign(paste0('x',i),inputs[,i])
  form <- 'y ~ '
  for(m in 3:nSources)
    for(i in 2:(m-1))
      for(l in 1:(i-1))
        form <- paste0(form,ifelse(form=='y ~ ','','+'),paste0('ti(',paste0('x',m),',',paste0('x',l),',',paste0('x',i),')'))
  for(m in 2:nSources)
    for(i in 1:(m-1))
      form <- paste0(form,ifelse(form=='y ~ ','','+'),paste0('ti(',paste0('x',m),',',paste0('x',i),')'))
  for(m in 1:nSources)
    form <- paste0(form,ifelse(form=='y ~ ','','+'),paste0('ti(',paste0('x',m),')'))
  print(form)
  case <- outcome[[outcome_index]]
  scen_case <- case[,seq(scen_index,ncol(case),by=NSCEN)]
  y <- rowSums(scen_case)
  vary <- var(y)
  model <- gam(as.formula(form))
  voi <- (vary - mean((y - model$fitted) ^ 2)) / vary * 100
  voi
}

voi_out <- mclapply(which(calcflag), 
                    FUN = parallel_evppi_for_emissions,
                    outcome, 
                    sources,
                    mc.cores = ifelse(Sys.info()[['sysname']] == "Windows",  1,  numcores))

evppi_for_emissions[which(calcflag)] <- unlist(voi_out)
print(evppi_for_emissions)




