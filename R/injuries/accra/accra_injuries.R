
### injury code
### This is the script for distance-based injury model for Accra using safety-in-numbers
## number of scenarios
rd <- read_csv("data/scenarios/accra/baseline_and_scenarios.csv")
dataset <- filter(rd, ! trip_mode %in% c('Short Walking', "99", "Train", "Other", "Unspecified"))
nscen<- length(unique(dataset$scenario)) -1

## short names of the scenarios
scen <- unique(rd$scenario)
scen_shortened_name<-c("base")
for (i in 2: (nscen+1))
{
  scen_shortened_name[i]<- paste0("scen", i-1) 
}

sin<- read.csv('R/injuries/data/sin_coefficients_pairs.csv')
scen_dist<-as.data.frame(read_csv("data/scenarios/accra/dist_by_mode_all_scenarios_all_ages.csv")) ## total distance travelled by all population by different modes and for different scenarios
names(scen_dist)[1]<- c("mode")
names(scen_dist)[2:(length(scen_shortened_name)+1)]<-scen_shortened_name
scen_dist[nrow(scen_dist)+1, 1]<- "Car"
scen_dist[nrow(scen_dist),2:(length(unique(dataset$scenario))+1)]<- colSums(scen_dist[4:5,2:(length(unique(dataset$scenario))+1)]) ## summing Private Car and Taxi as Car
scen_dist<- scen_dist[-c(4,5),]  ## removing Private Car and Taxi rows
scen_dist[,1]<-c("Bicycle", "Bus", "Motorcycle", "Pedestrian", "Car")
## adding truck as one of the vehicle types
scen_dist[nrow(scen_dist)+1, 1]<-"Truck"  
## adding tuktuk as one of the vehicle types
scen_dist[nrow(scen_dist)+1, 1]<-"Tuktuk"
## allocating dummy distance of 1 for trucks as these will not be changed across the scenarios
scen_dist[nrow(scen_dist)-1,2:(length(unique(dataset$scenario))+1)]<-1   
## allocating dummy distance of 1 for tuk-tuks as these will not be changed 
scen_dist[nrow(scen_dist),2:(length(unique(dataset$scenario))+1)]<-1  

whw_mat<-read.csv('R/injuries/accra/who_hit_who_accra.csv')
## columns as striking and rows as victim
whw_mat 

## calculating the ratio of distances for each mode in each scenario

for (i in 1:nscen )
{
  scen_dist[,(2+i)]<-scen_dist[,(2+i)]/scen_dist[,2]
}

## names of victim types
victim_deaths<- as.data.frame(whw_mat[,1])  
## number of deaths in baseline by victim type
victim_deaths<- cbind(victim_deaths, scen=as.data.frame(rowSums(whw_mat[,3:8])))  

whw_mat2<-whw_mat
for (k in 3:(1+length(scen_shortened_name))) ## iterating over the scenarios as indexed in scen_dist matrix
{
  for (i in 1: nrow(whw_mat))
  {
    for (j in 2: ncol(whw_mat))
    {
      nrow_vic_dist<- which(scen_dist[,1]== whw_mat[i,1])
      victim_dist<-scen_dist[nrow_vic_dist,k] ### 3== scenario1, 4== scenario 2, in the scen_dist matrix
      nrow_strk_dist<- which(scen_dist[,1]== names(whw_mat)[j])
      strk_dist<- scen_dist[nrow_strk_dist,k]
      nrow_sin<-  which(sin[,1]==whw_mat[i,1]) 
      ncol_sin<- which(names(sin)==names(whw_mat)[j])
      
      whw_mat2[i, j]<- whw_mat[i, j]*(victim_dist^sin[nrow_sin[1],ncol_sin])*(strk_dist^sin[nrow_sin[1]+6,ncol_sin])   ### safety in numbers coefficients: 0.5 for victim and 0.7 for striking
      print(nrow_strk_dist)
      
    }
    
  }
  write_csv(whw_mat2,paste0('R/injuries/accra/whw_mat_scen',k-2,'.csv'))  
  victim_deaths<- cbind(victim_deaths, as.data.frame(rowSums(whw_mat2[,3:8])))
}
names(victim_deaths)[1]<- c("victim_type")
names(victim_deaths)[2:(length(scen_shortened_name)+1)]<-scen_shortened_name
victim_deaths ### number of road deaths in the baseline and scenarios by victim type


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
  for (n in 1: length(unique(dataset$scenario)))
  {
    x[[list[i]]][which(x$scenario==unique(dataset$scenario)[n])]<- x[[list[i]]][which(x$scenario==unique(dataset$scenario)[n])]/ sum(x[[list[i]]][which(x$scenario==unique(dataset$scenario)[n])],na.rm=T)
    
  }
}


#names<- c("Baseline", "Scenario 1", "Scenario 2", "Scenario 3")
for (i in 1: length(scen_shortened_name))
{
  for (j in 1: nrow(x))
  {
    if (x[j,3]==unique(dataset$scenario)[i]) ## checking baseline or the 3 scenarios
    {
      for (k in 1: 5) ## iterating ovet the 5 victim type names in X
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

#write_csv(x, "data/synth_pop_data/accra/injuries/deaths_by_mode.csv")

gbd_data<- read_csv('data/demographics/gbd/accra/GBD Accra.csv')
gbd_injuries<- gbd_data[which(gbd_data$cause=="Road injuries"),]

a<-unique(gbd_injuries$age)
gbd_injuries$sex_age <- paste0(gbd_injuries$sex,"_",gbd_injuries$age)
## calculating the ratio of YLL to deaths for each age and sex group
gbd_injuries<- arrange(gbd_injuries, measure)
gbd_inj_yll<- gbd_injuries[which(gbd_injuries$measure=="YLLs (Years of Life Lost)"),]
gbd_inj_dth<- gbd_injuries[which(gbd_injuries$measure=="Deaths"),]
gbd_inj_yll$yll_dth_ratio<- gbd_inj_yll$value_gama/gbd_inj_dth$value_gama 
str(x)
x<- x[,-c(which(names(x)=='sex'))]

x<- x%>% left_join(gbd_inj_yll, by="sex_age")

x$YLL<- x$Deaths*x$yll_dth_ratio
x<-select(x, c('age_cat','sex','scenario','Deaths','YLL'))
#sort(names(x))

x  ### Death burden from injuries
x_deaths<- select(x, -YLL)
x_deaths<-spread(x_deaths,scenario, Deaths)

for (n in 1: nscen)
{
  
  x_deaths[,n+3]<-x_deaths[,n+3] - x_deaths[,3] 
  
}

x_yll<- select(x, -Deaths)
x_yll<-spread(x_yll,scenario, YLL)
for (n in 1: nscen)
{
  
  x_yll[,n+3]<-x_yll[,n+3] - x_yll[,3] 
  
}


View(x_deaths)

deaths_yll_injuries<- cbind(x_deaths, x_yll)
names(deaths_yll_injuries)
deaths_yll_injuries<-deaths_yll_injuries[,-c((2+length(unique(dataset$scenario))+1),(2+length(unique(dataset$scenario))+2))]
deaths_yll_injuries<- as.data.frame(deaths_yll_injuries)
names(deaths_yll_injuries)[1:2]<- c("age_cat", "sex")


metric<-c("deaths", "yll")
k=1
for  (i in 1: 2)
{
  
  for (j in 1: length(unique(dataset$scenario)))
  {
    names(deaths_yll_injuries)[2+k] <- paste0(scen_shortened_name[j],"_",metric[i],"_inj")
    k<-k+1
    
  }
}



View(deaths_yll_injuries)

deaths_yll_injuries[,3:ncol(deaths_yll_injuries)] <- -1 * deaths_yll_injuries[,3:ncol(deaths_yll_injuries)] 

write_csv(deaths_yll_injuries, "R/injuries/accra/deaths_yll_injuries.csv")

