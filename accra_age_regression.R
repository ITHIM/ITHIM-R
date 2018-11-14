setwd('~/overflow_dropbox/ITHIM-R/')
rm (list = ls())
source('ithim_r_functions.R')

ithim_object <- run_ithim_setup()
mode_names <- c("Bicycle","Bus","Motorcycle","Pedestrian","Car")

journeys <- ithim_object$trip_scen_sets %>% 
  group_by (age,sex,trip_mode,scenario) %>% 
  summarise(tot_dist = sum(trip_distance))
journeys$trip_mode[journeys$trip_mode%in%c('Walking','Short Walking')] <- 'Pedestrian'
journeys$trip_mode[journeys$trip_mode%in%c('Private Car','Taxi')] <- 'Car'
distances <- expand.grid(age=min(journeys$age):max(journeys$age),gender=unique(journeys$sex),mode=mode_names)
distances$raw_distance <- apply(distances,1,function(x)sum(subset(journeys,scenario=='Baseline'&age==x[1]&sex==x[2]&trip_mode==x[3])$tot_dist))*
  365*1600000/length(unique(SYNTHETIC_POPULATION$participant_id))
cols <- c('navyblue','darkorange2')
gens <- c('Female','Male')
true_distances <- distances
true_distances$sex_age <-  paste0(true_distances$gender,"_",true_distances$age)
# removed addition of bus_driver to bus
modp <- glm((raw_distance>0)~ns(age,knots=c(25,50))+(gender+mode)^2,
            data=true_distances,family=binomial(link=logit),control=glm.control(maxit=1000,epsilon=1e-10))
pos_distances <- subset(true_distances,raw_distance>0)
modd <- glm(log(raw_distance)~ns(age,knots=c(25,50))+(gender+mode)^2+ns(age,knots=c(25,50)):(mode=='Car')+ns(age,knots=c(25,50)):(mode=='Bus')+ns(age,knots=c(25,50)):(mode=='Pedestrian'),
            data=pos_distances,control=glm.control(maxit=100,epsilon=1e-7))

probability <- predict(modp,type='response')#sigmoid(rnorm(length(probs[[1]]),probs[[1]],probs[[2]]))
if_travel <- predict(modd,newdata = true_distances,type='response',se.fit=T)
true_distances$pred_distance <- exp(if_travel[[1]])*probability*1e-9
true_distances$low_distance <- exp(if_travel[[1]]-if_travel[[2]])*probability*1e-9
true_distances$high_distance <- exp(if_travel[[1]]+if_travel[[2]])*probability*1e-9
{x11(width=11,height=6); par(mfrow=c(2,3),mar=c(5,5,2,1))
for(mi in 1:(length(mode_names)+1))
  for(j in 1:2){
    m <- 'Bicycle'
    if(mi<6) m <- mode_names[mi]
    gen <- gens[j]
    bl <- subset(true_distances,gender==gen&mode==m)
    maxy <- max(subset(true_distances,mode==m)$raw_distance)*1e-9
    if(mi==6) maxy <- 2*max(bl$raw_distance)*1e-9
    if(gen==gens[1])
      plot(bl$age,bl$pred_distance,col=cols[j],frame=F,ylim=c(0,maxy),typ='l',lwd=2,main=m,
           cex.main=1.5,cex.lab=1.5,cex.axis=1.5,ylab='Total distance, bn km',xlab='Age')
    else lines(bl$age,bl$pred_distance,col=cols[j],lwd=2)
    lines(bl$age,bl$low_distance,col=cols[j],lty=2)
    lines(bl$age,bl$high_distance,col=cols[j],lty=2)
    points(bl$age,bl$raw_distance*1e-9,col=cols[j],pch=16)
    if(mi==1) legend(x=50,y=0.018,bty='n',legend=c('Mean','SE','Male','Female'),lty=c(1:2,NA,NA),col=cols[c(rep(2,3),1)],lwd=c(rep(2,2),NA,NA),pch=c(rep(NA,2),16,16),cex=1.4)

  }
} 

injuries <- readRDS('code/injuries/data/accra_injuries_long.Rds')
injuries <- subset(injuries,!is.na(cas_age)&cas_age>14&cas_age<70)

# divide injuries into those for which we can write a WHW matrix, i.e. we know distances of both striker and casualty, 
## and those for which we don't know striker distance: no or other vehicle (noov)
## we can only model casualties for which we know distance travelled (i.e. no Truck casualties for Accra)
injury_list <- list()
injury_list$whw <- subset(injuries,cas_mode%in%mode_names&strike_mode%in%mode_names)
injury_list$noov <- subset(injuries,cas_mode%in%mode_names&!strike_mode%in%mode_names)
injury_table <- list()
for(type in c('whw','noov')){
  ##TODO make contingency table without prior knowledge of column names
  injury_table[[type]] <- expand.grid(year=unique(injury_list[[type]]$year),cas_mode=unique(injury_list[[type]]$cas_mode),
                                      strike_mode=unique(injury_list[[type]]$strike_mode),cas_age=min(injuries$cas_age):
                                        max(injuries$cas_age),cas_gender=unique(injury_list[[type]]$cas_gender),stringsAsFactors = F)
  
  injury_table[[type]]$count <- apply(injury_table[[type]],1,
                                      function(x)nrow(subset(injury_list[[type]],year==as.numeric(x[1])&cas_mode==as.character(x[2])&
                                                               strike_mode==as.character(x[3])&cas_gender==as.character(x[5])&cas_age==as.numeric(x[4])))) 
}
u_gen <- unique(injury_table[[1]]$cas_gender)
u_age <- unique(injury_table[[1]]$cas_age)
age_gen_labels <- apply(expand.grid(u_gen,u_age),1,function(x)paste(x,collapse='_'))
cas_mode_indices <- list()
injury_gen_age <- list()
for(type in c('whw','noov')){
  ##TODO make contingency table without prior knowledge of column names
  gen_index <- match(injury_table[[type]]$cas_gen,u_gen)
  age_index <- match(injury_table[[type]]$cas_age,u_age)
  injury_gen_age[[type]] <- age_gen_labels[length(u_gen)*(age_index-1)+gen_index]
  injury_table[[type]]$injury_gen_age <- injury_gen_age[[type]]
  cas_mode_indices[[type]] <- match(injury_table[[type]]$cas_mode,mode_names)
}
strike_mode_indices <- match(injury_table$whw$strike_mode,mode_names)
injury_table$whw$cas_age <- as.numeric(injury_table$whw$cas_age)

nSamples <- 1000
mode_samples <- list()
map_modes <- list()
keep_modes <- c('Pedestrian','Bicycle','Car')
for(m in keep_modes) mode_samples[[m]] <- expand.grid(age=sort(unique(journeys$age)),gender=unique(journeys$sex))
samples <- expand.grid(age=sort(unique(journeys$age)),gender=unique(journeys$sex))
expected <- expand.grid(age=sort(unique(journeys$age)),gender=unique(journeys$sex))
injury_map <- list()
age_knots <- c(25,45)
for(i in 1:nSamples){
  set.seed(i)
  #probs <- predict(modp,type='link',se.fit=T)
  true_distances$probability <- predict(modp,type='response')#sigmoid(rnorm(length(probs[[1]]),probs[[1]],probs[[2]]))
  travel_dist <- predict(modd,newdata = true_distances,type='link',se.fit=T)
  true_distances$if_travel <- exp(rnorm(length(travel_dist[[1]]),travel_dist[[1]],travel_dist[[2]]))
  true_distances$pred_distance <- true_distances$if_travel*true_distances$probability
  
  injuries_list <- list()
  true_scen_dist <- true_distances#subset(true_distances,scenario==scen)
  reg_model <- list()
  for(type in c('whw','noov')){
    injuries_list[[type]] <- injury_table[[type]]
    injuries_list[[type]]$strike_distance <- 1
    injuries_list[[type]]$strike_distance_sum <- 1
    distance_sums <- sapply(mode_names,function(x)sum(subset(true_scen_dist,mode==x)$pred_distance))
    injuries_list[[type]]$cas_distance_sum <- distance_sums[cas_mode_indices[[type]]]
    cas_demo_indices <- match(injury_gen_age[[type]],true_scen_dist$sex_age)
    if(i==1) injury_map[[type]] <- apply(injuries_list[[type]],1,function(x)which(true_distances$mode==x[2]&true_distances$sex_age==x[7]))
    injuries_list[[type]]$cas_distance <- true_distances$pred_distance[injury_map[[type]]]
    if(type=='whw'){
      injuries_list[[type]]$strike_distance <- distance_sums[strike_mode_indices]
      injuries_list[[type]]$strike_distance_sum <- injuries_list[[type]]$strike_distance
    }
    injuries_list[[type]] <- subset(injuries_list[[type]],strike_distance>0&cas_distance>0)
    reg_model[[type]] <- glm(count~strike_mode+ns(cas_age,knots=age_knots,Boundary.knots=c(20,68))+cas_mode+ns(cas_age,knots=age_knots,Boundary.knots=c(20,68)):cas_mode+cas_gender,
                             data=injuries_list[[type]],family=poisson(link=log),
                             offset=0.5*log(cas_distance)+0.5*log(strike_distance),control=glm.control(maxit=500,epsilon=1e-12))
    injuries_list[[type]] <- subset(injuries_list[[type]],year==2016)
    pred <- predict(reg_model[[type]],newdata = injuries_list[[type]],type='link',se.fit=F)#T)
    injuries_list[[type]]$lambda <- exp(pred)#exp(rnorm(length(pred[[1]]),pred[[1]],pred[[2]]))
    injuries_list[[type]]$pred <- rpois(length(injuries_list[[type]]$lambda),injuries_list[[type]]$lambda)
  }
  bl <- rbind(injuries_list$whw,injuries_list$noov)
  if(i==1) map_samples <- apply(samples,1,function(x)which(bl$cas_age==x[1]&bl$cas_gender==x[2]))
  if(i==1) for(m in keep_modes) map_modes[[m]] <- apply(samples,1,function(x)which(bl$cas_age==x[1]&bl$cas_gender==x[2]&bl$cas_mode==m))
  samples[[i+2]] <- sapply(1:ncol(map_samples),function(x)sum(bl$pred[map_samples[,x]]))
  expected[[i+2]] <- sapply(1:ncol(map_samples),function(x)sum(bl$lambda[map_samples[,x]])/sum(unique(bl$cas_distance[map_samples[,x]])))*1e9
  for(m in keep_modes) mode_samples[[m]][[i+2]] <- sapply(1:ncol(map_modes[[m]]),function(x)sum(bl$lambda[map_modes[[m]][,x]]/bl$cas_distance[map_modes[[m]][,x]]))*1e9
}
colSums(samples[,3:ncol(samples)])

par(mfrow=c(1,1))
for(j in 1:2){
  gen <- gens[j]
  bl <- subset(expected,gender==gen)
  bl$lower025 <- apply(bl[,3:ncol(bl)],1,function(x)quantile(x,0.025,type=5))
  bl$lower25 <- apply(bl[,3:ncol(bl)],1,function(x)quantile(x,0.25,type=5))
  bl$med <- apply(bl[,3:ncol(bl)],1,function(x)quantile(x,0.5,type=8))
  bl$upper75 <- apply(bl[,3:ncol(bl)],1,function(x)quantile(x,0.75,type=9))
  bl$upper975 <- apply(bl[,3:ncol(bl)],1,function(x)quantile(x,0.975,type=9))
  if(gen=='Female')
    plot(bl$age,bl$med,col=cols[j],frame=F,ylim=c(0,11),typ='l',lwd=2)
  else lines(bl$age,bl$med,col=cols[j],lwd=2)
  lines(bl$age,bl$lower25,col=cols[j],lty=2)
  lines(bl$age,bl$upper75,col=cols[j],lty=2)
  #lines(bl$age,bl$lower025,col=cols[j],lty=3)
  #lines(bl$age,bl$upper975,col=cols[j],lty=3)
  bl <- rbind(subset(injuries_list$whw,cas_gender==gen),subset(injuries_list$noov,cas_gender==gen))
  fatalities <- sapply(unique(bl$cas_age),function(x)sum(subset(bl,cas_age==x)$count))
  distance <- sapply(unique(bl$cas_age),function(x) sum(subset(true_distances,age==x&gender==gen)$pred_distance)/1e9)
  points(unique(bl$cas_age),fatalities/distance,col=cols[j],pch=16)
  print(max(fatalities/distance))
}

{x11(width=11,height=4); par(mfrow=c(1,3),mar=c(5,5,2,1))
for(mi in 1:length(keep_modes)){
  bl <- list()
  raw_rate <- list()
  for(j in 1:2){
    m <- keep_modes[mi]
    gen <- gens[j]
    raw <- rbind(subset(injuries_list$whw,cas_gender==gen&cas_mode==m),subset(injuries_list$noov,cas_gender==gen&cas_mode==m))
    fatalities <- sapply(unique(raw$cas_age),function(x)sum(subset(raw,cas_age==x)$count))
    distance <- sapply(unique(raw$cas_age),function(x) sum(subset(true_distances,age==x&gender==gen&mode==m)$pred_distance)/1e9)
    raw_rate[[j]] <- fatalities/distance
    bl[[j]] <- subset(mode_samples[[m]],gender==gen)
    bl[[j]]$lower025 <- apply(bl[[j]][,3:ncol(bl[[j]])],1,function(x)quantile(x,0.025,type=5))
    bl[[j]]$med <- apply(bl[[j]][,3:ncol(bl[[j]])],1,function(x)quantile(x,0.5,type=8))
    bl[[j]]$upper975 <- apply(bl[[j]][,3:ncol(bl[[j]])],1,function(x)quantile(x,0.975,type=9))
  }
  maxy <- max(unlist(raw_rate),na.rm=T)
  for(j in 1:2){
    gen <- gens[j]
    if(gen==gens[1])
      plot(bl[[j]]$age,bl[[j]]$med,col=cols[1],frame=F,ylim=c(0,maxy),typ='l',lwd=2,main=m,
           cex.lab=1.5,cex.axis=1.5,ylab='Fatalities per bn km',xlab='Age',cex.main=1.5)
    else lines(bl[[j]]$age,bl[[j]]$med,col=cols[j],lwd=2)
    lines(bl[[j]]$age,bl[[j]]$lower025,col=cols[j],lty=2)
    lines(bl[[j]]$age,bl[[j]]$upper975,col=cols[j],lty=2)
    points(unique(raw$cas_age),raw_rate[[j]],col=cols[j],pch=16)
    if(mi==1) legend(x=15,y=180,bty='n',legend=c('Expectation','SE','Male','Female'),lty=c(1:2,NA,NA),col=cols[c(rep(2,3),1)],lwd=c(rep(2,2),NA,NA),pch=c(rep(NA,2),16,16),cex=1.5)
  }
}
}

{x11(width=8); par(mfrow=c(1,1),mar=c(5,5,1,1))
for(j in 2:1){
  gen <- gens[j]
  bl <- subset(samples,gender==gen)
  bl$lower025 <- apply(bl[,3:ncol(bl)],1,function(x)quantile(x,0.025,na.rm=T))
  bl$lower25 <- apply(bl[,3:ncol(bl)],1,function(x)quantile(x,0.25,na.rm=T))
  bl$med <- apply(bl[,3:ncol(bl)],1,function(x)quantile(x,0.5,na.rm=T))
  bl$upper75 <- apply(bl[,3:ncol(bl)],1,function(x)quantile(x,0.75,na.rm=T))
  bl$upper975 <- apply(bl[,3:ncol(bl)],1,function(x)quantile(x,0.975,na.rm=T))
  if(gen==gens[2])
    plot(bl$age,bl$med,col=cols[j],frame=F,ylim=c(0,8),typ='l',lwd=2,cex.axis=1.5,cex.lab=1.5,xlab='Age',ylab='Number of fatalities')
  else lines(bl$age,bl$med,col=cols[j],lwd=2)
  lines(bl$age,bl$lower25,col=cols[j],lty=2,lwd=2)
  lines(bl$age,bl$upper75,col=cols[j],lty=2,lwd=2)
  lines(bl$age,bl$lower025,col=cols[j],lty=3,lwd=2)
  lines(bl$age,bl$upper975,col=cols[j],lty=3,lwd=2)
  bl <- rbind(subset(injuries_list$whw,cas_gender==gen),subset(injuries_list$noov,cas_gender==gen))
  fatalities <- sapply(unique(bl$cas_age),function(x)sum(subset(bl,cas_age==x)$count))
  points(unique(bl$cas_age),fatalities,col=cols[j],pch=16)
}
legend(x=55,y=8,bty='n',legend=c('Median','50%','95%','Male','Female'),lty=c(1:3,NA,NA),col=cols[c(rep(2,4),1)],lwd=c(rep(2,3),NA,NA),pch=c(rep(NA,3),16,16),cex=1.5)}




par(mfrow=c(1,2))
for(out in c('count','pred'))
  for(gen in c('Male','Female')){
    bl <- rbind(subset(injuries_list$whw,cas_gender==gen),subset(injuries_list$noov,cas_gender==gen))
    print(sum(bl[[out]]))
    fatalities <- sapply(unique(bl$cas_age),function(x)sum(subset(bl,cas_age==x)[[out]]))
    print(max(fatalities))
    if(gen=='Male')
      plot(unique(bl$cas_age),fatalities,col='darkorange2',frame=F,ylim=c(0,7))
    else points(unique(bl$cas_age),fatalities,col='navyblue')
  }

plot(injuries_list$whw$count,injuries_list$whw$pred)
par(mfrow=c(2,3))
for(plot_mode in mode_names[-3]){
  baseline <- subset(injuries_list$whw,cas_mode==plot_mode)
  for(gen in c('Male','Female')){
    bl <- subset(baseline,cas_gender==gen)
    fatalities <- sapply(unique(bl$cas_age),function(x)sum(subset(bl,cas_age==x)$pred))
    if(gen=='Male')
      plot(unique(bl$cas_age),fatalities,col='darkorange2',main=plot_mode,ylim=c(0,1.4))
    else points(unique(bl$cas_age),fatalities,col='navyblue')
  }
}



##############################################################################
## age grouped

mode_samples <- list()
keep_modes <- c('Pedestrian','Bicycle','Car')
for(m in keep_modes) mode_samples[[m]] <- expand.grid(age=sort(unique(dataset[[1]]$cas_age)),gender=unique(dataset[[1]]$cas_gender))
samples <- expand.grid(age=sort(unique(dataset[[1]]$cas_age)),gender=unique(dataset[[1]]$cas_gender))
nSamples <- 1000
map_modes <- list()
reg_model <- list()
dataset <- ithim_object$inj_distances$injuries_list$Baseline
for(type in c('whw','noov')){
  dataset[[type]]$cas_distance <- dataset[[type]]$cas_distance*365*1600000/length(unique(SYNTHETIC_POPULATION$participant_id))*1e-9
  dataset[[type]]$cas_distance_sum <- dataset[[type]]$cas_distance_sum*365*1600000/length(unique(SYNTHETIC_POPULATION$participant_id))*1e-9
  
  reg_model[[type]] <- glm(count~strike_mode+cas_age*cas_gender*cas_mode,control=glm.control(maxit=1000,epsilon=1e-10),
                           data=dataset[[type]],family='poisson',offset=0.5*log(cas_distance)+0.5*log(strike_distance))
  dataset[[type]] <- subset(dataset[[type]],year==2016)
}
for(i in 1:nSamples){
  for(type in c('whw','noov')){
    pred <- predict(reg_model[[type]],newdata = dataset[[type]],type='link',se.fit=F)#T)
    dataset[[type]]$lambda <- exp(pred)#exp(rnorm(length(pred[[1]]),pred[[1]],pred[[2]]))
    dataset[[type]]$pred <- rpois(length(dataset[[type]]$lambda),dataset[[type]]$lambda)
  }
  bl <- rbind(dataset$whw,dataset$noov)
  if(i==1) map_samples <- apply(samples,1,function(x)which(bl$cas_age==x[1]&bl$cas_gender==x[2]))
  if(i==1) for(m in keep_modes) map_modes[[m]] <- apply(samples,1,function(x)which(bl$cas_age==x[1]&bl$cas_gender==x[2]&bl$cas_mode==m))
  
  samples[[i+2]] <- sapply(1:length(map_samples),function(x)sum(bl$pred[map_samples[[x]]]))
  for(m in keep_modes){
    if(class(map_modes[[m]])=='matrix')
      mode_samples[[m]][[i+2]] <- sapply(1:ncol(map_modes[[m]]),function(x)sum(bl$pred[map_modes[[m]][,x]]/bl$cas_distance[map_modes[[m]][,x]]))
    else 
      mode_samples[[m]][[i+2]] <- sapply(1:length(map_modes[[m]]),function(x)sum(bl$pred[map_modes[[m]][[x]]]/bl$cas_distance[map_modes[[m]][[x]]]))
    
  }
}

{x11(width=11,height=4); par(mfrow=c(1,3),mar=c(5,5,2,1))
  for(mi in 1:length(keep_modes)){
    bl <- list()
    raw_rate <- list()
    for(j in 1:2){
      m <- keep_modes[mi]
      gen <- gens[j]
      raw <- rbind(subset(ithim_object$inj_distances$injuries_list$Baseline$whw,cas_gender==gen&cas_mode==m),subset(ithim_object$inj_distances$injuries_list$Baseline$noov,cas_gender==gen&cas_mode==m))
      fatalities <- sapply(unique(raw$cas_age),function(x)sum(subset(raw,cas_age==x)$count))/10
      distance <- sapply(unique(raw$cas_age),function(x) subset(raw,cas_age==x)$cas_distance[1])*365*1600000/length(unique(SYNTHETIC_POPULATION$participant_id))*1e-9
      raw_rate[[j]] <- fatalities/distance
      bl[[j]] <- subset(mode_samples[[m]],gender==gen)
      bl[[j]]$lower025 <- apply(bl[[j]][,3:ncol(bl[[j]])],1,function(x)quantile(x,0.025,type=5))
      bl[[j]]$lower25 <- apply(bl[[j]][,3:ncol(bl[[j]])],1,function(x)quantile(x,0.25,type=5))
      bl[[j]]$med <- apply(bl[[j]][,3:ncol(bl[[j]])],1,function(x)quantile(x,0.5,type=8))
      bl[[j]]$upper975 <- apply(bl[[j]][,3:ncol(bl[[j]])],1,function(x)quantile(x,0.975,type=9))
      bl[[j]]$upper75 <- apply(bl[[j]][,3:ncol(bl[[j]])],1,function(x)quantile(x,0.75,type=9))
    }
    maxy <- max(unlist(raw_rate),sapply(bl,function(x)x$upper975),na.rm=T)
    ages <- levels(bl[[j]]$age)
    x <- 1:length(ages)-0.1
    for(j in 1:2){
      gen <- gens[j]
      if(gen==gens[1]){
        plot(x,bl[[j]]$med,col=cols[j],frame=F,ylim=c(0,maxy),main=m,pch=16,cex=2,xaxt='n',xlim=c(0,length(ages)+1),
             cex.lab=1.5,cex.axis=1.5,ylab='Fatalities per bn km',xlab='Age',cex.main=1.5)
        axis(1,at=x+0.1,labels = ages,cex.axis=1.5)
      }
      else points(x+0.2*(j-1),bl[[j]]$med,col=cols[j],pch=16,cex=2)
      for(i in 1:2) arrows(x0=x[i]+0.2*(j-1),y0=bl[[j]]$lower025[i],y1=bl[[j]]$upper975[i],col=cols[j],angle=90,length=0.1,code=3,lty=2)
      for(i in 1:2) arrows(x0=x[i]+0.2*(j-1),y0=bl[[j]]$lower25[i],y1=bl[[j]]$upper75[i],col=cols[j],angle=90,length=0.1,code=3,lwd=2)
      #points(x+0.1*(j-1),raw_rate[[j]][1:length(x)],col=cols[j],pch=16,cex=2)
      if(mi==1) legend(x=0,y=180,bty='n',legend=c('50%','95%','Male','Female'),lty=c(1:2,NA,NA),col=cols[c(rep(2,3),1)],lwd=c(rep(2,2),NA,NA),pch=c(rep(NA,2),16,16),cex=1.5)
    }
  }
}
{x11(width=11,height=4); par(mfrow=c(1,3),mar=c(5,5,2,1))
  for(mi in 1:length(keep_modes)){
    distance <- matrix(0,nrow=2,ncol=2)
    for(j in 1:2){
      m <- keep_modes[mi]
      gen <- gens[j]
      for(k in 1:length(ages)){
        raw <- subset(ithim_object$inj_distances$injuries_list$Baseline$whw,cas_gender==gen&cas_mode==m&cas_age==ages[k])
        if(nrow(raw)>0)
          distance[j,k] <- raw$cas_distance[1]*365*1600000/length(unique(SYNTHETIC_POPULATION$participant_id))*1e-9
      }
    }
    x <- 1:length(ages)-0.1
      gen <- gens[j]
        barplot(distance,col=cols,beside=T,main=m,pch=16,cex=1.5,names.arg=ages,ylim=c(0,1.6),
             cex.lab=1.5,cex.axis=1.5,ylab='Total distance, bn km',xlab='Age',cex.main=1.5)
      if(mi==1) legend(x=1,y=1.5,bty='n',legend=c('Female','Male'),fill=cols,cex=1.5)
    }
  }

{x11(); par(mfrow=c(1,1),mar=c(5,5,2,1))
    bl <- list()
    raw_rate <- list()
    for(j in 1:2){
      gen <- gens[j]
      raw <- rbind(subset(ithim_object$inj_distances$injuries_list$Baseline$whw,cas_gender==gen),subset(ithim_object$inj_distances$injuries_list$Baseline$noov,cas_gender==gen))
      fatalities <- sapply(unique(raw$cas_age),function(x)sum(subset(raw,cas_age==x)$count))/10
      raw_rate[[j]] <- fatalities
      bl[[j]] <- subset(samples,gender==gen)
      bl[[j]]$lower025 <- apply(bl[[j]][,3:ncol(bl[[j]])],1,function(x)quantile(x,0.025,type=5))
      bl[[j]]$lower25 <- apply(bl[[j]][,3:ncol(bl[[j]])],1,function(x)quantile(x,0.25,type=5))
      bl[[j]]$med <- apply(bl[[j]][,3:ncol(bl[[j]])],1,function(x)quantile(x,0.5,type=8))
      bl[[j]]$upper975 <- apply(bl[[j]][,3:ncol(bl[[j]])],1,function(x)quantile(x,0.975,type=9))
      bl[[j]]$upper75 <- apply(bl[[j]][,3:ncol(bl[[j]])],1,function(x)quantile(x,0.75,type=9))
    }
    maxy <- max(unlist(raw_rate),sapply(bl,function(x)x$upper975),na.rm=T)
    ages <- levels(bl[[j]]$age)
    x <- 1:length(ages)-0.1
    for(j in 1:2){
      gen <- gens[j]
      if(gen==gens[1]){
        plot(x,bl[[j]]$med,col=cols[j],frame=F,ylim=c(0,maxy),pch=16,cex=2,xaxt='n',xlim=c(0,length(ages)+1),
             cex.lab=1.5,cex.axis=1.5,ylab='Fatalities',xlab='Age',cex.main=1.5)
        axis(1,at=x+0.1,labels = ages,cex.axis=1.5)
      }
      else points(x+0.2*(j-1),bl[[j]]$med,col=cols[j],pch=16,cex=2)
      for(i in 1:2) arrows(x0=x[i]+0.2*(j-1),y0=bl[[j]]$lower025[i],y1=bl[[j]]$upper975[i],col=cols[j],angle=90,length=0.1,code=3,lty=2)
      for(i in 1:2) arrows(x0=x[i]+0.2*(j-1),y0=bl[[j]]$lower25[i],y1=bl[[j]]$upper75[i],col=cols[j],angle=90,length=0.1,code=3,lwd=2)
      #points(x+0.1*(j-1),raw_rate[[j]][1:length(x)],col=cols[j],pch=16,cex=2)
      legend(x=0,y=90,bty='n',legend=c('50%','95%','Male','Female'),lty=c(1:2,NA,NA),col=cols[c(rep(2,3),1)],lwd=c(rep(2,2),NA,NA),pch=c(rep(NA,2),16,16),cex=1.5)
    }
}
