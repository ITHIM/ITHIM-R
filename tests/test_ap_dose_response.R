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


######## plot curves
pm <- 1:300
for ( j in 1:nrow(DISEASE_INVENTORY)) if (DISEASE_INVENTORY$air_pollution[j] == 1){ 
  cause <- as.character(DISEASE_INVENTORY$ap_acronym[j])
  dr_ap <- subset(DR_AP,cause_code==cause)
  ages <- unique(dr_ap$age_code)
  print(cause)
  print(ages)
  if(length(ages)>1){ 
    pdf(paste0('results/dose_response/AP/',cause,'DR-sim.pdf'),width=6,height=8); par(mfrow=c(1,1))
    layout.matrix <- matrix(1:length(ages),nrow=5,ncol=3,byrow=T)
    graphics::layout(mat=layout.matrix,heights=c(4,3,3,3,4),widths=c(3.8,3,3))
  }else{
    pdf(paste0('results/dose_response/AP/',cause,'DR-sim.pdf'))
    par(mfrow=c(1,1))
  }
  for(age in ages){
    par(mar=c(ifelse(age>80,5,1),ifelse(age%in%c(99,25,40,55,70,85),5,1),ifelse(age%in%c(25:35,99),5,1),1))
    dr_ap_age <- subset(dr_ap,age_code==age)
    alpha <- dr_ap_list[[cause]][[age]]$alpha
    beta <- dr_ap_list[[cause]][[age]]$beta
    gamma <- dr_ap_list[[cause]][[age]]$gamma
    tmrel <- dr_ap_list[[cause]][[age]]$tmrel
    for(i in 1:100){#length(alpha)){
      pmcurve <- ap_dose_response_curve(pm,alpha[i],beta[i],gamma[i],tmrel[i])
      pmcurve[is.na(pmcurve)] <- 1
      if(i==1) {plot(pm,pmcurve,typ='l',col=rgb((nsamples-i)/nsamples,0.2,i/nsamples,0.1),ylim=c(1,5),main=ifelse(age%in%c(30,99),as.character(DISEASE_INVENTORY$GBD_name)[j],''),cex.lab=1.5,
                     frame=F,xlab=ifelse(age>70,'Dose',''),ylab=ifelse(age%in%c(99,25,40,55,70,85),'Relative risk',''),xaxt='n',yaxt='n',cex.main=1.5)
        axis(1,labels=ifelse(age>80,T,F),cex.axis=1.5)
        axis(2,labels=ifelse(age%in%c(99,25,40,55,70,85),T,F),cex.axis=1.5)
        if(length(ages)>1) mtext(side=3,text=paste0('Age ',age),line=0)
      }
      else lines(pm,pmcurve,col=rgb((nsamples-i)/nsamples,0.2,i/nsamples,0.1),cex.axis=1.5)
    }
  }
  dev.off()
  if(length(ages)>1){ 
    pdf(paste0('results/dose_response/AP/',cause,'DR.pdf'),width=6,height=8); par(mfrow=c(1,1))
    layout.matrix <- matrix(1:length(ages),nrow=5,ncol=3,byrow=T)
    graphics::layout(mat=layout.matrix,heights=c(4,3,3,3,4),widths=c(3.8,3,3))
  }else{
    pdf(paste0('results/dose_response/AP/',cause,'DR.pdf'))
    par(mfrow=c(1,1))
  }
  for(age in ages){
    par(mar=c(ifelse(age>80,5,1),ifelse(age%in%c(99,25,40,55,70,85),5,1),ifelse(age%in%c(25:35,99),5,1),1))
    dr_ap_age <- subset(dr_ap,age_code==age)
    alpha <- dr_ap_age$alpha
    beta <- dr_ap_age$beta
    gamma <- dr_ap_age$gamma
    tmrel <- dr_ap_age$tmrel
    for(i in 1:100){#length(alpha)){
      pmcurve <- ap_dose_response_curve(pm,alpha[i],beta[i],gamma[i],tmrel[i])
      pmcurve[is.na(pmcurve)] <- 1
      if(i==1) {plot(pm,pmcurve,typ='l',col=rgb((nsamples-i)/nsamples,0.2,i/nsamples,0.1),ylim=c(1,5),main=ifelse(age%in%c(30,99),as.character(DISEASE_INVENTORY$GBD_name)[j],''),cex.lab=1.5,
                     frame=F,xlab=ifelse(age>70,'Dose',''),ylab=ifelse(age%in%c(99,25,40,55,70,85),'Relative risk',''),xaxt='n',yaxt='n',cex.main=1.5)
        axis(1,labels=ifelse(age>80,T,F),cex.axis=1.5)
        axis(2,labels=ifelse(age%in%c(99,25,40,55,70,85),T,F),cex.axis=1.5)
        if(length(ages)>1) mtext(side=3,text=paste0('Age ',age),line=0)
      }
      else lines(pm,pmcurve,col=rgb((nsamples-i)/nsamples,0.2,i/nsamples,0.1),cex.axis=1.5)
    }
  }
  dev.off()
}
    
    

