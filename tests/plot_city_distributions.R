lnorm_parameters <- list(
  # lnorm parameters for CHRONIC_DISEASE_SCALAR
  chronic_disease_scalar = list(accra=c(0,log(1.2)),
                                sao_paulo=c(0,log(1.2)),
                                delhi=c(0,log(1.2)),
                                bangalore=c(0,log(1.2))),
  # lnorm parameters for PM_CONC_BASE
  pm_concentration = list(accra=c(log(50),log(1.3)),
                          sao_paulo=c(log(20),log(1.3)),
                          delhi=c(log(122),log(1.3)),
                          bangalore=c(log(63),log(1.3))),
  # lnorm parameters for BACKGROUND_PA_SCALAR
  background_pa_scalar = list(accra=c(0,log(1.2)),
                              sao_paulo=c(0,log(1.2)),
                              delhi=c(0,log(1.2)),
                              bangalore=c(0,log(1.2))),
  # lnorm parameters for BUS_WALK_TIME
  bus_walk_time = list(accra=c(log(5),log(1.2)),
                       sao_paulo=c(log(5),log(1.2)),
                       delhi=c(log(5),log(1.2)),
                       bangalore=c(log(5),log(1.2))),
  # lnorm parameters for MOTORCYCLE_TO_CAR_RATIO
  mc_car_ratio = list(accra=c(-1.4,0.4),
                      sao_paulo=c(-1.4,0.4),
                      delhi=c(-1.4,0.4),
                      bangalore=c(-1.4,0.4)),
  # lnorm parameters for TRUCK_TO_CAR_RATIO
  truck_car_ratio = list(accra=c(-1.4,0.4),
                         sao_paulo=c(-1.4,0.4),
                         delhi=c(-1.4,0.4),
                         bangalore=c(-1.4,0.4))
)


beta_parameters <- list(
  # beta parameters for INJURY_REPORTING_RATE
  injury_report_rate = list(accra=c(8,3),
                            sao_paulo=c(50,3),
                            delhi=c(50,3),
                            bangalore=c(50,3)),
  # beta parameters for PM_TRANS_SHARE
  pm_trans_share = list(accra=c(5,20),
                        sao_paulo=c(8,8),
                        delhi=c(8,8),
                        bangalore=c(8,8)),
  # bus occupancy beta distribution
  bus_to_passenger_ratio = list(accra=c(20,600),
                                  sao_paulo=c(20,600),
                                  delhi=c(20,600),
                                  bangalore=c(20,600)),
  # truck beta distribution
  truck_to_car_ratio  <- list(accra=c(3,10),
                              sao_paulo=c(3,10),
                              delhi=c(3,10),
                              bangalore=c(3,10))
)


cols <- c('navyblue','hotpink','grey','darkorange')
{x11(height=6,width=6); par(mfrow=c(2,2),mar=c(5,5,2,2))
  x <- seq(0,1,length=100)
  for(i in 1:length(beta_parameters)) {
    toplot <- list()
    for(j in 1:length(beta_parameters[[1]]))
      toplot[[j]] <- dbeta(x,beta_parameters[[i]][[j]][1],beta_parameters[[i]][[j]][2])
    ymax <- max(unlist(toplot))
    for(j in 1:length(beta_parameters[[1]]))
      if(j==1){
        plot(x,toplot[[j]],typ='l',lwd=2,col=cols[j],frame=F,xlab=names(beta_parameters)[i],ylab='Density',ylim=c(0,ymax))
      }else{
        lines(x,toplot[[j]],beta_parameters[[i]][[j]][2],typ='l',lwd=2,col=cols[j])
      }
    legend(legend=names(beta_parameters[[1]]),x=0,y=ymax,col=cols,lwd=2,bty='n')
  }
}
xmaxes <- c(3,200,3,10,1,1)
{x11(height=6,width=9); par(mfrow=c(2,3),mar=c(5,5,2,2))
  for(i in 1:length(lnorm_parameters)) {
    x <- seq(0,xmaxes[i],length=100)
    toplot <- list()
    for(j in 1:length(lnorm_parameters[[1]]))
      toplot[[j]] <- dlnorm(x,lnorm_parameters[[i]][[j]][1],lnorm_parameters[[i]][[j]][2])
    ymax <- max(unlist(toplot))
    for(j in 1:length(lnorm_parameters[[1]]))
      if(j==1){
        plot(x,toplot[[j]],typ='l',lwd=2,col=cols[j],frame=F,xlab=names(lnorm_parameters)[i],ylab='Density',ylim=c(0,ymax),xlim=c(0,xmaxes[i]))
      }else{
        lines(x,toplot[[j]],lnorm_parameters[[i]][[j]][2],typ='l',lwd=2,col=cols[j])
      }
    legend(legend=names(lnorm_parameters[[1]]),x=0.5*xmaxes[i],y=ymax,col=cols,lwd=2,bty='n')
  }
}


emission_parameters <- list(accra=list(
  bus_driver=0.82,
  car=0.228,
  taxi=0.011,
  motorcycle=0.011,
  truck=0.859,
  big_truck=0.711,
  other=0.082
),
sao_paulo=list(motorcycle=4,
               car=4,
               bus_driver=32,
               big_truck=56,
               truck=4),
delhi=list(motorcycle=1409,
           auto_rickshaw=133,
           car=2214,
           bus_driver=644,
           big_truck=4624,
           truck=3337),
bangalore=list(motorcycle=1757,
               auto_rickshaw=220,
               car=4173,
               bus_driver=1255,
               big_truck=4455,
               truck=703))

library(distr)
confidences <- c(0.9,0.5,0.2)
confidences <- (confidences - 0.6)*5 + confidences
n <- 1000
for(i in 1:length(emission_parameters)){
  total <- sum(unlist(emission_parameters[[i]]))
  d1 <- list()
  for(k in 1:length(confidences)){
    confidence <- confidences[k]
    distributions <- lapply(emission_parameters[[i]],function(x) Gammad(shape=x/total*100*(3^confidence),scale=1))
    samples <- sapply(distributions,function(x) r(x)(n))
    d1[[k]] <- apply(samples,2,function(x)density(x/rowSums(samples),from=0,to=1))
  }
  x11(height=4,width=8); par(mfrow=c(2,4),mar=c(5,5,2,2))
  for(j in 1:length(d1[[1]]))
    for(k in 1:length(d1))
      if(k==1)
        plot(d1[[k]][[j]],frame=F,xlab=names(emission_parameters[[i]])[j],main='',ylab='Density',lwd=2,col=cols[k],ylim=c(0,max(d1[[k]][[j]]$y)/2))
      else lines(d1[[k]][[j]],col=cols[k],lwd=2)
}
  
