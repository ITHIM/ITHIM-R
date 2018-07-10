library(dplyr)
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
trans_emissions[1,p+i]<- trans_emissions$base_emissions[1]*scen_dist[6,n+i]/scen_dist[6,n] ## scenario emissions of 2W
trans_emissions[2,p+i]<- trans_emissions$base_emissions[2]*scen_dist[6,n+i]/scen_dist[6,n] ## scenario emissions of 2W
trans_emissions[3,p+i]<- trans_emissions$base_emissions[3]*scen_dist[4,n+i]/scen_dist[4,n] ## scenario emissions of 2W
trans_emissions[4,p+i]<- trans_emissions$base_emissions[4]*scen_dist[8,n+i]/scen_dist[8,n] ## scenario emissions of Taxi
trans_emissions[5,p+i]<- trans_emissions$base_emissions[5]*scen_dist[3,n+i]/scen_dist[3,n] ## scenario emissions of bus
trans_emissions[6,p+i]<- trans_emissions$base_emissions[6]*1 ## scenario emissions of trucks
trans_emissions[7,p+i]<- trans_emissions$base_emissions[7]*1 ## scenario emissions of trucks
trans_emissions[8,p+i]<- trans_emissions$base_emissions[8]*1 ## scenario emissions of trucks
names(trans_emissions)[p+i]<-(paste("scen",i,"_emissions", sep=""))
}


trans_emissions[nrow(trans_emissions)+1,2:ncol(trans_emissions)]<- colSums(trans_emissions[,2:ncol(trans_emissions)],na.rm=T)
trans_emissions

trans_emissions[,1]<-as.character(trans_emissions[,1])
trans_emissions[nrow(trans_emissions),1]<-"Total"                                                                

trans_emissions[nrow(trans_emissions)+1,1]<-"pm_conc_total"
trans_emissions[nrow(trans_emissions),ncol(trans_emissions)-4]<- pm_conc_base
trans_emissions[nrow(trans_emissions)+1,1]<-"pm_conc_transport"
trans_emissions[nrow(trans_emissions), ncol(trans_emissions)-4]<- 