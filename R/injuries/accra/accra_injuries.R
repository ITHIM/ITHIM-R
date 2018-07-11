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

for (i in 1: 4)
{
  row<- which(scen_dist[,1]== "Bicycle")

}
scen_dist
scen_dist[,3]<-scen_dist[,3]/scen_dist[,2]
scen_dist[,4]<-scen_dist[,4]/scen_dist[,2]
scen_dist[,5]<-scen_dist[,5]/scen_dist[,2]

whw_mat2<- whw_mat
for (i in 1: nrow(whw_mat))
{
  for (j in 2: ncol(whw_mat))
  {
    nrow<- which(whw_mat[,1]==whw_mat[i,1] )
    
    whw_mat2[i, j]<- whw_mat
  }
  
}