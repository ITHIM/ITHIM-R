rm(list=ls())
library(ithimr)

global_path <- file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/global/')
global_path <- paste0(global_path, "/")

##reading GBD 2017 IER functions that were provided by Rick Burnett: this include Diabetes in addition to previous five disease end-points
DR_AP <- read.csv(paste0(global_path,"dose_response/drap/dose_response_2017.csv"))

DISEASE_INVENTORY <- read.csv(paste0(global_path,"dose_response/disease_outcomes_lookup.csv"))
nsamples <- 100
parameters <- ithim_setup_parameters(NSAMPLES=nsamples,
                                     AP_DOSE_RESPONSE_QUANTILE=T)
for(i in 1:length(parameters))
  assign(names(parameters)[i],parameters[[i]],pos=1)
causes <- unique(DR_AP$cause_code)

############## plot parameter samples
dr_ap_list <- list()
for ( j in c(1:nrow(DISEASE_INVENTORY))[DISEASE_INVENTORY$air_pollution == 1]){ 
  cause <- as.character(DISEASE_INVENTORY$ap_acronym[j])
  dr_ap <- subset(DR_AP,cause_code==cause)
  ages <- unique(dr_ap$age_code)
  dr_ap_list[[cause]] <- list()
  print(cause)
  print(ages)
  for(age in ages){
    dr_ap_list[[cause]][[age]] <- list()
    dr_ap_age <- subset(dr_ap,age_code==age)
    alpha <- sapply(DR_AP_LIST,function(x)x[[cause]][[as.character(age)]]$alpha)
    beta <- sapply(DR_AP_LIST,function(x)x[[cause]][[as.character(age)]]$beta)
    gamma <- sapply(DR_AP_LIST,function(x)x[[cause]][[as.character(age)]]$gamma)
    tmrel <- sapply(DR_AP_LIST,function(x)x[[cause]][[as.character(age)]]$tmrel)
    dr_ap_list[[cause]][[age]]$alpha <- alpha
    dr_ap_list[[cause]][[age]]$beta <- beta
    dr_ap_list[[cause]][[age]]$gamma <- gamma
    dr_ap_list[[cause]][[age]]$tmrel <- tmrel
    #pdf(paste0('results/dose_response/AP/',cause,age,'.pdf'))
    #x11(width=8); 
    par(mfrow=c(2,3),mar=c(4,4,1,1))
    plot(log(dr_ap_age$alpha),log(dr_ap_age$beta),main=paste0(cause,age),col='grey'); points(log(alpha),log(beta),col='hotpink',pch=18)
    plot(log(dr_ap_age$alpha),(log(dr_ap_age$gamma)),main=paste0(cause,age),col='grey'); points(log(alpha),log(gamma),col='hotpink',pch=18)
    plot(log(dr_ap_age$alpha),log(dr_ap_age$tmrel),main=paste0(cause,age),col='grey'); points(log(alpha),log(tmrel),col='hotpink',pch=18)
    plot(log(dr_ap_age$beta),log(dr_ap_age$gamma),main=paste0(cause,age),col='grey'); points(log(beta),log(gamma),col='hotpink',pch=18)
    plot(log(dr_ap_age$beta),log(dr_ap_age$tmrel),main=paste0(cause,age),col='grey'); points(log(beta),log(tmrel),col='hotpink',pch=18)
    plot(log(dr_ap_age$gamma),log(dr_ap_age$tmrel),main=paste0(cause,age),col='grey'); points(log(gamma),log(tmrel),col='hotpink',pch=18)
    #dev.off()
  }
}

# Save dr AP
write_rds(dr_ap_list, "inst/extdata/global/dose_response/drap/dr_ap_list.Rds")