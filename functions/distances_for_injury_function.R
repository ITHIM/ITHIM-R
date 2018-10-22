distances_for_injury_function <- function(rd){
  
  journeys <- filter(rd,!is.na(trip_id), !trip_mode%in%c(99,"Train","Unspecified","Other")) %>% 
    group_by (age_cat,sex,trip_mode, scenario) %>% 
    summarise(tot_dist = sum(trip_distance))
  distances <- spread(journeys,trip_mode, tot_dist,fill=0) 
  distances$Pedestrian <- distances$Walking + distances$`Short Walking`
  distances <- distances[, -which(names(distances) ==  "Walking")]
  distances <- distances[, -which(names(distances) ==  "Short Walking")]
  distances$Car <- distances$Taxi + distances$`Private Car`
  distances <- distances[, -which(names(distances) ==  "Private Car")]
  distances <- distances[, -which(names(distances) ==  "Taxi")]
  scen_dist <- sapply(1:(NSCEN+1),function(x)c(colSums(subset(distances,scenario == SCEN[x])[,4:8])))
  colnames(scen_dist) <- SCEN_SHORT_NAME
  for(i in 2:6) scen_dist[,i] <- scen_dist[,i]/scen_dist[,1] 
  scen_dist <- rbind(scen_dist,Truck=1,Tuktuk=1)
  
  mode_names <- names(distances)[4:8]
  for (i in 1: length(mode_names))
    for (n in 1:(NSCEN+1))
      distances[[mode_names[i]]][which(distances$scenario == unique(rd$scenario)[n])] <- 
    distances[[mode_names[i]]][which(distances$scenario == unique(rd$scenario)[n])]/ sum(distances[[mode_names[i]]][which(distances$scenario == unique(rd$scenario)[n])],na.rm=T)
  relative_distances <- distances
  relative_distances$sex_age <-  paste0(relative_distances$sex,"_",relative_distances$age_cat)
  relative_distances <- relative_distances[,-c(which(names(relative_distances) == 'sex'))]
  
  list(relative_distances=relative_distances,scen_dist=scen_dist)
}
