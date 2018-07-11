library(dplyr)
library(tidyverse)
### this code is to calculate daily PM concentrations for the baseline and the scenarios
ind <- read_csv("data/synth_pop_data/accra/processed_data/scenarios/baseline_and_three_scenarios.csv")
ind<-as.data.frame(ind)
head(ind)
#ind  %>% group_by(trip_mode) %>% 
## calculating in-vehicle exposure ratio based on baseline concentration (using Goel et al., 2015)
## all modes categorised as open except car which is assumed to be half as closed (air-conditioned closed windows) and other half as open windows

pm_conc_base <- (30+70)/2   ## average of the range of annual PM2.5 concentration reported for residential areas in Dionisio et al. (2010) for year 2006-2010
pm_trans_share<-22.5/100 ## Share of transport in total PM2.5 concentration http://www.who.int/quantifying_ehimpacts/global/source_apport/en/

##Thiago sent file for updated PM2.5 concentrations from years 2015-2017, however, our model is based on 2009, therefore, the new values have not
##been used
ratios<- read.csv("data/synth_pop_data/accra/pollution/pm_exposure_ratio_look_up.csv")
ratios<- subset(ratios, select=c("Mode", "ratio", "vent_rate"))
for (i in 3: 7)  ## all open modes-- walking, motorcycle, bicycle, bus and short walking
{
    ratios$ratio[i]<- 3.216-0.379 *log(pm_conc_base)
    
}
for (i in 1:2 ) ## car and taxi
  
{
  ratios$ratio[i]<- (0.5+ratios$ratio[3])/2  ## Average of 0.5 (air-conditioned closed door; Goel et al.2015) and ratio for open modes
  
}
write.csv(ratios,'data/synth_pop_data/accra/pollution/pm_exposure_ratio_look_up.csv')

##Calculation of background PM2.5 concentration based on emissions in the scenarios

scen_dist<-as.data.frame(read_csv( "data/scenarios/accra/dist_by_mode_all_scenarios_all_ages.csv")) ## total distance travelled by all population by different modes and for different scenarios
nscen=3 ### Number of scenarios
###emission inventory file
trans_emissions<-read.csv("data/emission calculations accra/transport_emission_inventory_accra.csv")
names(trans_emissions)<- c("vehicle_type", "delhi_fleet_2011", "delhi_fleet_perHH", "accra_fleet_2010", "PM2_5_emiss_fact", "base_emissions")
n=2 ## the column where baseline distances are in the scenario distance file
p= ncol(trans_emissions)
for ( i in 1:nscen)
{
trans_emissions[1,p+i]<- trans_emissions$base_emissions[1]*scen_dist[4,n+i]/scen_dist[4,n] ## scenario emissions of 4W1
trans_emissions[2,p+i]<- trans_emissions$base_emissions[2]*scen_dist[4,n+i]/scen_dist[4,n] ## scenario emissions of 4W2
trans_emissions[3,p+i]<- trans_emissions$base_emissions[3]*scen_dist[3,n+i]/scen_dist[3,n] ## scenario emissions of 2W
trans_emissions[4,p+i]<- trans_emissions$base_emissions[4]*scen_dist[5,n+i]/scen_dist[5,n] ## scenario emissions of Taxi
trans_emissions[5,p+i]<- trans_emissions$base_emissions[5]*scen_dist[2,n+i]/scen_dist[2,n] ## scenario emissions of bus
trans_emissions[6,p+i]<- trans_emissions$base_emissions[6]*1 ## scenario emissions of trucks
trans_emissions[7,p+i]<- trans_emissions$base_emissions[7]*1 ## scenario emissions of trucks
trans_emissions[8,p+i]<- trans_emissions$base_emissions[8]*1 ## scenario emissions of trucks
names(trans_emissions)[p+i]<-(paste("scen",i,"_emissions", sep=""))
}


trans_emissions[nrow(trans_emissions)+1,2:ncol(trans_emissions)]<- colSums(trans_emissions[,2:ncol(trans_emissions)],na.rm=T)
trans_emissions

trans_emissions[,1]<-as.character(trans_emissions[,1])
trans_emissions[nrow(trans_emissions),1]<-"Total"  
total_row<- nrow(trans_emissions)

trans_emissions[nrow(trans_emissions)+1,1]<-"pm_conc_total"
trans_emissions[nrow(trans_emissions),ncol(trans_emissions)-3]<- pm_conc_base
trans_emissions[nrow(trans_emissions)+1,1]<-"pm_conc_transport"
trans_emissions[nrow(trans_emissions), ncol(trans_emissions)-3]<- pm_trans_share*pm_conc_base
trans_emissions[nrow(trans_emissions), ncol(trans_emissions)-2]<- pm_trans_share*pm_conc_base*trans_emissions[total_row, ncol(trans_emissions)-2]/trans_emissions[total_row, ncol(trans_emissions)-3]
trans_emissions[nrow(trans_emissions), ncol(trans_emissions)-1]<- pm_trans_share*pm_conc_base*trans_emissions[total_row, ncol(trans_emissions)-1]/trans_emissions[total_row, ncol(trans_emissions)-3]
trans_emissions[nrow(trans_emissions), ncol(trans_emissions)]<-pm_trans_share*pm_conc_base*trans_emissions[total_row, ncol(trans_emissions)]/trans_emissions[total_row, ncol(trans_emissions)-3]
non_transport_pm_conc<- pm_conc_base -  (pm_trans_share*pm_conc_base)  ### this is the concentration contributed by non-transport share (remains constant across the scenarios)
trans_emissions[nrow(trans_emissions)-1,ncol(trans_emissions)-2]<-  non_transport_pm_conc + trans_emissions[nrow(trans_emissions),ncol(trans_emissions)-2]
trans_emissions[nrow(trans_emissions)-1,ncol(trans_emissions)-1]<-  non_transport_pm_conc + trans_emissions[nrow(trans_emissions),ncol(trans_emissions)-1]
trans_emissions[nrow(trans_emissions)-1,ncol(trans_emissions)]<-  non_transport_pm_conc + trans_emissions[nrow(trans_emissions),ncol(trans_emissions)]

conc_pm<- trans_emissions[nrow(trans_emissions)-1, 6:9]


### following code generates final_data
for (i in 1: 4)
{
 rd <- read_csv("data/scenarios/accra/baseline_and_three_scenarios.csv")
 str(rd)
 names(rd)[2]<- "base_mode"
 names(rd)[3]<- "base_duration"
  
 lookup_ratio_pm<-  read.csv('data/synth_pop_data/accra/pollution/pm_exposure_ratio_look_up.csv')
 a<-c("base", "scen1", "scen2", "scen3")
 name<-paste(a[i],'_mode', sep="")   ## for base and scenario modes, the name of that column is changed to "Mode" 
 col= which(names(rd)==name)  
 names(rd)[col]<- "Mode"
 rd<-rd %>% left_join(lookup_ratio_pm, "Mode")  ## attaching the file with in-vehicle ratio and ventilation rate
 col= which(names(rd)==paste(a[i],"_duration", sep=""))
 for (j in 1: nrow(rd))  ## calculating on-road transport PM dose
 {
   rd$pm_dose[j]<-  as.numeric(rd[j,col])*rd$ratio[j]*as.numeric(conc_pm[i])*rd$vent_rate[j]/60
   rd$on_road_air[j]<- as.numeric(rd[j,col])*rd$vent_rate[j]/60
 }
 rd<-as.data.frame(rd)
 rd$participant_id<-as.factor(rd$participant_id)
 
 if (i ==1) ### baseline
 {
   individual_data<- rd %>% group_by(participant_id) %>% summarise(on_road_dur=sum(base_duration,na.rm=TRUE), on_road_pm= sum(pm_dose,na.rm=TRUE), on_road_pm= sum(pm_dose,na.rm=TRUE), air_inhaled=sum(on_road_air,na.rm=TRUE))
   
 }
 if (i ==2) ## scenario 1
 {
   individual_data<- rd %>% group_by(participant_id) %>% summarise(on_road_dur=sum(scen1_duration,na.rm=TRUE), on_road_pm= sum(pm_dose,na.rm=TRUE), on_road_pm= sum(pm_dose,na.rm=TRUE), air_inhaled=sum(on_road_air,na.rm=TRUE))
   
 }
 if (i ==3) ## scenario 1
 {
   individual_data<- rd %>% group_by(participant_id) %>% summarise(on_road_dur=sum(scen2_duration,na.rm=TRUE), on_road_pm= sum(pm_dose,na.rm=TRUE), on_road_pm= sum(pm_dose,na.rm=TRUE), air_inhaled=sum(on_road_air,na.rm=TRUE))
   
 }
 if (i ==4) ## scenario 1
 {
   individual_data<- rd %>% group_by(participant_id) %>% summarise(on_road_dur=sum(scen3_duration,na.rm=TRUE), on_road_pm= sum(pm_dose,na.rm=TRUE), on_road_pm= sum(pm_dose,na.rm=TRUE), air_inhaled=sum(on_road_air,na.rm=TRUE))
   
 }
 head(individual_data)
 for (j in 1: nrow(individual_data)) ## getting the concentrations
      {
        individual_data$pm_conc[j]<-(((24- ((individual_data$on_road_dur[j])/60))*as.numeric(conc_pm[i])*10) + individual_data$on_road_pm[j])/(((24- ((individual_data$on_road_dur[j])/60))*10)+individual_data$air_inhaled[j])
      }
 
 individual_data<-subset(individual_data, select=c("participant_id", "pm_conc"))
 names(individual_data)[2]<- paste('pm_conc_',a[i], sep="")
 
 if (i == 1 )
 {
   
  final_data<-individual_data 
 }  else
 {
 final_data<- final_data %>%  left_join(individual_data,by="participant_id")
 }

}
final_data<- final_data[]
write.csv(as.data.frame(final_data), 'data/synth_pop_data/accra/pollution/individual_level_pm_conc_scenarios.csv')

length(unique(final_data$participant_id))
str(final_data)

