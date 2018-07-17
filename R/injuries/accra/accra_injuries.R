### This is the script for distance-based injury model for Accra using safety-in-numbers

scen_dist<-as.data.frame(read_csv("data/scenarios/accra/dist_by_mode_all_scenarios_all_ages.csv")) ## total distance travelled by all population by different modes and for different scenarios
names(scen_dist)<- c("mode", "base", "scen1", "scen2", "scen3")
scen_dist[nrow(scen_dist)+1, 1]<- "Car"
scen_dist[nrow(scen_dist),2:5]<- colSums(scen_dist[4:5,2:5]) ## summing Private Car and Taxi as Car
scen_dist<- scen_dist[-c(4,5),]  ## removing Private Car and Taxi rows
scen_dist[,1]<-c("Bicycle", "Bus", "Motorcycle", "Pedestrian", "Car")
scen_dist[nrow(scen_dist)+1, 1]<-"Truck"  ## adding truck as one of the vehicel types
scen_dist[nrow(scen_dist)+1, 1]<-"Tuktuk" ## adding tuktuk as one of the vehicel types
scen_dist[nrow(scen_dist)-1,2:5]<-1 ## allocating dummy distance of 1 as this will not be changed across the scenarios
scen_dist[nrow(scen_dist),2:5]<-1 ## allocating dummy distance of 1 as this will not be changed 

whw_mat<-read.csv('R/injuries/accra/who_hit_who_accra.csv')
whw_mat ## columns as striking and rows as victim

## calculating the ratio of distances for each mode in each scenario
scen_dist[,3]<-scen_dist[,3]/scen_dist[,2]
scen_dist[,4]<-scen_dist[,4]/scen_dist[,2]
scen_dist[,5]<-scen_dist[,5]/scen_dist[,2]

victim_deaths<- as.data.frame(whw_mat[,1])  ## names of victims
victim_deaths<- cbind(victim_deaths, scen=as.data.frame(rowSums(whw_mat[,3:8])))  ## number of deaths in baseline by victim type
whw_mat2<-whw_mat
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
    whw_mat2[i, j]<- whw_mat[i, j]*(victim_dist^0.5)*(strk_dist^0.7)   ### safety in numbers coefficients: 0.5 for victim and 0.7 for striking
  }
  
}
write.csv(whw_mat2,paste0('R/injuries/accra/whw_mat_scen',k-2,'.csv'))  
victim_deaths<- cbind(victim_deaths, as.data.frame(rowSums(whw_mat2[,3:8])))
}
names(victim_deaths)<- c("victim_type","base", "scen1", "scen2", "scen3")
victim_deaths ### number of road deaths in the baseline and scenarios by victim type


rd <- read_csv("data/scenarios/accra/baseline_and_three_scenarios.csv")

# Redefine age_cat to match with GBD's
# Make age category
age_category <- c("15-49", "50-69", ">70")
rd$age_cat[rd$age >= 15 & rd$age < 50] <- age_category[1]
rd$age_cat[rd$age >= 50 & rd$age < 70] <- age_category[2]
#View(rd)

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


names<- c("Baseline", "Scenario 1", "Scenario 2", "Scenario 3")
for (i in 1: 4)
{
  for (j in 1: nrow(x))
  {
    if (x[j,3]==names[i]) ## checking baseline or the 3 scenarios
        {
           for ( k in 1: 5) ## iterating ovet the 5 victim type names in X
           {
             row<-which (victim_deaths$victim_type==names(x)[k+3])
             if( !is.na(x[j,k+3]))
             {
               x[j,k+3] <- x[j,k+3]* victim_deaths[row,i+1]
             }
             }
        }
    
  }
  
}

x[,9] <- rowSums(x[,4:8],na.rm=T)
names(x)[9]<-"Deaths"
x$sex_age<-  paste0(x$sex,"_",x$age_cat)

write_csv(x, "data/synth_pop_data/accra/injuries/deaths_by_mode.csv")

gbd_data<- read_csv('data/demographics/gbd/accra/GBD Accra.csv')
gbd_injuries<- gbd_data[which(gbd_data$cause=="Road injuries"),]

a<-unique(gbd_injuries$age)
gbd_injuries$sex_age <- paste0(gbd_injuries$sex,"_",gbd_injuries$age)
for ( i in 7: 12) ## calculating the ratio of YLL to deaths for each age and sex group
{
  gbd_injuries[i,ncol(gbd_injuries)-1] <-gbd_injuries[i,ncol(gbd_injuries)-1]/ gbd_injuries[i-6,ncol(gbd_injuries)-1]
  
}
##Removing deaths data from GBD
gbd_injuries<- gbd_injuries[which(gbd_injuries$measure=="YLLs (Years of Life Lost)"),]
gbd_injuries<- gbd_injuries[,-c(1: (ncol(gbd_injuries)-2))]
names(gbd_injuries)[1]<- as.character("YLL_death_ratio")

x<- x%>% left_join(gbd_injuries, by="sex_age")
x$YLL<- x$Deaths*x$YLL_death_ratio
View(x)  ### 

x<- x[,c(1,2,3,9,12)]
x  ### Death burden from injuries
x_deaths<- x[,-5]
x_deaths<-spread(x_deaths,scenario, Deaths)
x_deaths[,4]<-x_deaths[,4] - x_deaths[,3] 
x_deaths[,5]<-x_deaths[,5] - x_deaths[,3] 
x_deaths[,6]<-x_deaths[,6] - x_deaths[,3] 

x_yll<- x[,-4]
x_yll<-spread(x_yll,scenario, YLL)
x_yll[,4]<-x_yll[,4] - x_yll[,3] 
x_yll[,5]<-x_yll[,5] - x_yll[,3] 
x_yll[,6]<-x_yll[,6] - x_yll[,3] 

deaths_yll_injuries<- cbind(x_deaths, x_yll)
deaths_yll_injuries<-deaths_yll_injuries[,-c(7,8)]
deaths_yll_injuries<- as.data.frame(deaths_yll_injuries)
names(deaths_yll_injuries)<- c("age_cat", "sex", "base_deaths_inj", "scen1_deaths_inj", "scen2_deaths_inj", "scen3_deaths_inj", 
                               "base_yll_inj", "scen1_yll_inj", "scen2_yll_inj", "scen3_yll_inj")

deaths_yll_injuries[,3:ncol(deaths_yll_injuries)] <- -1 * deaths_yll_injuries[,3:ncol(deaths_yll_injuries)] 

write_csv(deaths_yll_injuries, "R/injuries/accra/deaths_yll_injuries.csv")
  