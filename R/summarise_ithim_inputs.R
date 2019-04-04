#' @export
summarise_ithim_inputs <- function(ithim_object){
  modenames <- unlist(ithim_object$dist[,1])
  
  distances <- as.matrix(ithim_object$dist[,-1])
  distances_pc <- apply(distances,2,function(x)x/sum(x))
  x11(); par(mar=c(5,5,2,8)); barplot(distances_pc,col=rainbow(length(modenames)),legend.text=modenames,args.legend = c(x=length(SCEN)+4),ylab='Mode share by distance',main=CITY)
  
  trips <- sapply(SCEN,function(y)sapply(modenames,function(x)nrow(subset(subset(ithim_object$trip_scen_sets,trip_mode==x&scenario==y),!duplicated(trip_id)))))
  trips <- apply(trips,2,function(x)x/sum(x))
  x11(); par(mar=c(5,5,2,8)); barplot(trips,col=rainbow(length(modenames)),legend.text=modenames,args.legend = c(x=length(SCEN)+4),ylab='Mode share by trip mode',main=CITY)
  
  injuries <- sapply(unique(INJURY_TABLE$whw$cas_mode),function(x)sum(subset(INJURY_TABLE$whw,cas_mode==x)$count)+sum(subset(INJURY_TABLE$noov,cas_mode==x)$count))
  names(injuries) <- unique(INJURY_TABLE$whw$cas_mode)
  injury_modes <- c('pedestrian','bicycle','car','motorcycle')
  injury_rates <- sapply(injury_modes,function(x)sum(subset(INJURY_TABLE$whw,cas_mode==x)$count)+sum(subset(INJURY_TABLE$noov,cas_mode==x)$count))/
    distances[match(c('walking','bicycle','car','motorcycle'),modenames),1]
  
  print(injuries)
  x11();  barplot(injury_rates,col=rainbow(length(injury_rates)),ylab='Injury rates',main=CITY)
  
  emissions <- unlist(EMISSION_INVENTORY)
  emissions <- emissions[emissions>0]
  x11(); pie(emissions,main=paste0(CITY,' emissions'))

}
