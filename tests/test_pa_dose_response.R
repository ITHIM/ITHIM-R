rm(list=ls())

global_path <- file.path(find.package('ithimr',lib.loc=.libPaths()), 'extdata/global/')
global_path <- paste0(global_path, "/")

DISEASE_INVENTORY <- read.csv(paste0(global_path,"dose_response/disease_outcomes_lookup.csv"))

list_of_files <- list.files(path = paste0(global_path,"dose_response/drpa/extdata/"), recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)
for (i in 1:length(list_of_files)){
  assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
         readr::read_csv(list_of_files[[i]],col_types = cols()),
         pos = 1)
}

doses_vector <- as.numeric(0:50)
cols <- rainbow(sum(DISEASE_INVENTORY$physical_activity == 1))
cause_indices <- which(DISEASE_INVENTORY$physical_activity == 1)

############# median dose-response curves
PA_DOSE_RESPONSE_QUANTILE <<- 0.5
#pdf('results/dose_response/PA_dose_response.pdf',width=11,height=11);par(mar=c(5,5,2,1))
for ( j in 1:length(cause_indices)){
  cause <- as.character(DISEASE_INVENTORY$pa_acronym[cause_indices[j]])
  # get name of disease
  print(cause)
  return_vector <- PA_dose_response(cause = cause,dose = doses_vector)
  
  if(j==1){
    plot(doses_vector,return_vector$rr,type='l',ylim=0:1,main='',ylab='Relative risk',xlab='Dose',
         frame=F,col=cols[j],xlim=range(doses_vector),cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lwd=2) 
  }else{
    lines(doses_vector,return_vector$rr,col=cols[j],lwd=2)
  }
}
legend(legend=DISEASE_INVENTORY$pa_acronym[DISEASE_INVENTORY$physical_activity==1],col=cols,bty='n',x=0,y=0.5,lwd=2)
#dev.off()

############# sample dose-response curves
nsamples <- 10
parameters <- ithim_setup_parameters(NSAMPLES=nsamples,
                                     PA_DOSE_RESPONSE_QUANTILE=T)
#pdf('results/dose_response/PA_dose_response_sample.pdf',width=11,height=11);
par(mar=c(5,5,2,1),mfrow=c(3,3))
for ( j in 1:length(cause_indices)){
  cause <- as.character(DISEASE_INVENTORY$pa_acronym[cause_indices[j]])
  # get name of disease
  print(cause)
  for(seed in 1:nsamples){
    for(i in 1:length(parameters))
      assign(names(parameters)[i],parameters[[i]][[seed]],pos=1)
    return_vector <- PA_dose_response(cause = cause,dose = doses_vector)
    
    if(seed==1){
      plot(doses_vector,return_vector$rr,type='l',ylim=0:1,main=cause,ylab='Relative risk',xlab='Dose',
           frame=F,col=cols[j],xlim=range(doses_vector),cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lwd=1) 
    }else{
      lines(doses_vector,return_vector$rr,col=cols[j],lwd=1)
    }
  }
}
#dev.off()

################ explanatory graph
library(diagram)
j <- 3
ylow <- 0.6
{x11(); par(mfrow=c(2,2),mar=c(2,5,1,1))
pa_dn <- as.character(DISEASE_INVENTORY$pa_acronym[j])
pa_n <- as.character(DISEASE_INVENTORY$acronym[j])
outcome_type <- ifelse(pa_dn%in%c('lung_cancer','stroke'), 'incidence' , 'mortality')
dose <- doses_clean
if(pa_dn %in% c('total_cancer','coronary_heart_disease')) dose[dose>35] <- 35
cause<-pa_dn
fname <- paste(cause, outcome_type, sep = "_")
lookup_table <- get(paste0(fname))
lookup_df <- as.data.frame(lookup_table)
rr <- approx(x=lookup_df$dose,y=lookup_df$RR,xout=dose,yleft=1,yright=min(lookup_df$RR))$y
lb <- approx(x = lookup_df$dose,y = lookup_df$lb,xout = dose,yleft = 1,yright = min(lookup_df$lb))$y
ub <-approx(x = lookup_df$dose,y = lookup_df$ub,xout = dose,yleft = 1,yright = min(lookup_df$ub))$y
plot(doses_clean,rr,lwd=2,type='l',col=rgb(0.6,0.2,0.2),
     xlim=range(doses_clean),ylim=c(ylow,1),ylab='Response',frame=F,cex.lab=1.5,cex.axis=1.5,xlab='')
lines(doses_clean,lb,lwd=2,type='l',lty=2,col=rgb(0.6,0.2,0.2,1))
lines(doses_clean,ub,lwd=2,type='l',lty=2,col=rgb(0.6,0.2,0.2,1))
text('Mean',x=40,y=0.8,col=rgb(0.6,0.2,0.2),cex=1.5)
text('Lower bound',x=35,y=0.66,col=rgb(0.6,0.2,0.2),cex=1.5)
text('Upper bound',x=35,y=0.93,col=rgb(0.6,0.2,0.2),cex=1.5)
par(mar=c(2,2,1,1))
for(i in 1:500){
  x<-runif(1)
  rr0 <- truncnorm::qtruncnorm(p=x, mean=rr, sd=(rr-lb)/1.96,a=0)#a=lb,b=ub)#
  if(i==1){plot(doses_clean,rr0,type='l',col=rgb(0.6,0.2,0.2,0.1),
               xlim=range(doses_clean),ylim=c(ylow,1),ylab='',frame=F,cex.lab=1.5,cex.axis=1.5,xlab='') 
    text('Samples',x=35,y=0.97,col=rgb(0.6,0.2,0.2),cex=1.5)
  }else lines(doses_clean,rr0,col=rgb(0.6,0.2,0.2,0.1))
}
quant <- 0.5
par(mar=c(5,5,1,1))
rr0 <- truncnorm::qtruncnorm(p=quant, mean=rr, sd=(rr-lb)/1.96,a=0)
plot(doses_clean,rr0,lwd=2,type='l',col='grey',
     xlim=range(doses_clean),ylim=c(ylow,1),ylab='Response',frame=F,cex.lab=1.5,cex.axis=1.5,xlab='Dose')
doses <- c(5,25,45)
text('Median',x=10,y=0.97,col='grey',cex=1.5)
abline(h=ylow,lwd=2,lty=2,col='grey')
points(doses,rep(ylow,length(doses)),col=rgb(0.6,0.2,0.2),pch=18,cex=1.5)
points(doses,rr0[which(doses_clean%in%doses)],col=rgb(0.6,0.2,0.2),pch=15,cex=1.5)
for(dose in doses) arrows(x0=dose,y0=ylow,y1=rr0[which(doses_clean==dose)]-0.02,col=rgb(0.6,0.2,0.2),length=0.1,lty=1,lwd=2)
par(mar=c(5,2,1,1))
cols <- c('hotpink','navyblue','darkorange2')
quants <- c(0.1,0.6,0.9)
for(i in 1:length(quants)){
  quant <- quants[i]
  rr0 <- truncnorm::qtruncnorm(p=quant, mean=rr, sd=(rr-lb)/1.96,a=0)
  if(i==1) {plot(doses_clean,rr0,lwd=2,type='l',col=cols[i],
       xlim=range(doses_clean),ylim=c(ylow,1),ylab='',frame=F,cex.lab=1.5,cex.axis=1.5,xlab='Dose')
    for(dose in doses) arrows(x0=dose,y0=ylow,y1=rr0[which(doses_clean==dose)]-0.02,col=cols[i],length=0.1,lty=1,lwd=2)
  }  else{
    lines(doses_clean,rr0,lwd=2,type='l',col=cols[i])
    for(dose in doses) 
      curvedarrow(from=c(dose,ylow),to=c(dose,rr0[which(doses_clean==dose)]-0.01),lty=1,lwd=2,lcol=cols[i],
                  arr.col=cols[i],arr.pos=0.65+0.05*i,curve=-70+30*i,arr.type='curved',arr.adj=0,endhead=T)
  }
  abline(h=ylow,lwd=2,lty=2,col='grey')
  points(doses,rr0[which(doses_clean%in%doses)],col=cols[i],pch=15,cex=1.5)
  
  points(doses,rep(ylow,length(doses)),col=rgb(0.6,0.2,0.2),pch=18,cex=1.5)
}
}


