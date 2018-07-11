### This is the script for distance-based injury model for Accra using safety-in-numbers

scen_dist<-as.data.frame(read_csv( "data/scenarios/accra/dist_by_mode_all_scenarios_all_ages.csv")) ## total distance travelled by all population by different modes and for different scenarios
names(scen_dist)<- c("mode", "base", "scen1", "scen2", "scen3")
scen_dist[nrow(scen_dist)+1, 1]<- "Car"
scen_dist[nrow(scen_dist),2:5]<- colSums(scen_dist[4:5,2:5])
scen_dist<- scen_dist[-c(4,5),]
scen_dist[,1]<-c("Bicycle", "Bus", "Motorcycle", "Pedestrian", "Car")
scen_dist[nrow(scen_dist)+1, 1]<-"Truck"
scen_dist[nrow(scen_dist)+1, 1]<-"Tuktuk"
scen_dist[nrow(scen_dist)-1,2:5]<-1
scen_dist[nrow(scen_dist),2:5]<-1

whw_mat<-read.csv('R/injuries/accra/who_hit_who_accra.csv')
whw_mat

## calculating the ratio of distances for each mode in each scenario
scen_dist[,3]<-scen_dist[,3]/scen_dist[,2]
scen_dist[,4]<-scen_dist[,4]/scen_dist[,2]
scen_dist[,5]<-scen_dist[,5]/scen_dist[,2]

victim_deaths_scenarios<- as.data.frame(names(whw_mat2)[2:7])
names(victim_deaths_scenarios)[1]<-"Victim_type"

for (k in 3:5) ## iterating over the three scenarios as indexed in scen_dist matrix
{
for (i in 1: nrow(whw_mat))
{
  for (j in 2: ncol(whw_mat))
  {
    nrow_vic_dist<- which(scen_dist[,1]== whw_mat[i,1])
    victim_dist<-scen_dist[nrow_vic_dist,k] ### 3== scenario1, 4== scenario 2, in the scen_dist matrix
    nrow_strk_dist<- which(scen_dist[,1]== names(whw_mat)[j])
    strk_dist<- scen_dist[nrow_strk_dist,k]
    whw_mat2[i, j]<- whw_mat[i, j]*(victim_dist^0.5)*(strk_dist^0.7)   
  }
  
}
write.csv(whw_mat2,paste0('R/injuries/accra/whw_mat_scen',k-2,'.csv'))  
print(rowSums(whw_mat2[,2:8]))
scen_deaths<-sum(rowSums(whw_mat2[,2:8]))
difference<- scen_deaths- sum(rowSums(whw_mat[,2:8]))
print(difference)
names(whw_mat2[2:7])

}


rd <- read_csv("data/scenarios/accra/baseline_and_three_scenarios.csv")
View(rd)

x<-rd %>% filter(!is.na(trip_id), trip_mode!=99, !is.na(trip_mode), trip_mode!= "Train", trip_mode!= "Unspecified",trip_mode!= "Other"  ) %>% group_by (age_cat,sex,trip_mode, scenario) %>% summarise(tot_dist=sum(trip_distance))
x<-spread(x,trip_mode, tot_dist) 
x$Pedestrian<- x$Walking+x$`Short Walking`
x<- x[,-which(names(x)== "Walking")]
x<- x[,-which(names(x)== "Short Walking")]

x$Car<- x$Taxi+x$`Private Car`
x<- x[,-which(names(x)== "Private Car")]
x<- x[,-which(names(x)== "Taxi")]
list<-names(x)[4:8]
for (i in 1: length(list))
{
  

x[[list[i]]][which(x$scenario=="Baseline")]<- x[[list[i]]][which(x$scenario=="Baseline")]/ sum(x[[list[i]]][which(x$scenario=="Baseline")],na.rm=T)
x[[list[i]]][which(x$scenario=="Scenario 1")]<- x[[list[i]]][which(x$scenario=="Scenario 1")]/ sum(x[[list[i]]][which(x$scenario=="Scenario 1")],na.rm=T)
x[[list[i]]][which(x$scenario=="Scenario 2")]<- x[[list[i]]][which(x$scenario=="Scenario 2")]/ sum(x[[list[i]]][which(x$scenario=="Scenario 2")],na.rm=T)
x[[list[i]]][which(x$scenario=="Scenario 3")]<- x[[list[i]]][which(x$scenario=="Scenario 3")]/ sum(x[[list[i]]][which(x$scenario=="Scenario 3")],na.rm=T)
}
