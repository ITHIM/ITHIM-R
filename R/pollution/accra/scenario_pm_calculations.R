library(dplyr)
library(tidyverse)
### this code is to calculate daily PM concentrations for the baseline and the scenarios
# ind <- read_csv("data/synth_pop_data/accra/processed_data/scenarios/baseline_and_three_scenarios.csv")
# ind <- as.data.frame(ind)
# head(ind)
#ind  %>% group_by(trip_mode) %>% 
## calculating in-vehicle exposure ratio based on baseline concentration (using Goel et al., 2015)
## all modes categorised as open except car which is assumed to be half as closed (air-conditioned closed windows) and other half as open windows

pm_conc_base <- (30 + 70) / 2   ## average of the range of annual PM2.5 concentration reported for residential areas in Dionisio et al. (2010) for year 2006-2010
pm_trans_share <- 22.5 / 100 ## Share of transport in total PM2.5 concentration http://www.who.int/quantifying_ehimpacts/global/source_apport/en/

##Thiago sent file for updated PM2.5 concentrations from years 2015-2017, however, our model is based on 2009, therefore, the new values have not
##been used
ratios <- read_csv("data/synth_pop_data/accra/pollution/pm_exposure_ratio_look_up.csv")
ratios <- subset(ratios, select=c("Mode", "ratio", "vent_rate"))
for (i in 3: 7)  ## all open modes-- walking, motorcycle, bicycle, bus and short walking
{
    ratios$ratio[i] <- 3.216-0.379 * log(pm_conc_base)
    
}

for (i in 1:2 ) ## car and taxi
{
  ratios$ratio[i] <- (0.5 + ratios$ratio[3])/2  ## Average of 0.5 (air-conditioned closed door; Goel et al.2015) and ratio for open modes
  
}

write_csv(ratios,'data/synth_pop_data/accra/pollution/pm_exposure_ratio_look_up.csv')

##Calculation of background PM2.5 concentration based on emissions in the scenarios

scen_dist <- as.data.frame(read_csv( "data/scenarios/accra/dist_by_mode_all_scenarios_all_ages.csv")) ## total distance travelled by all population by different modes and for different scenarios
### Calculating number of scenarios besides the baseline
rd <- read_csv("data/scenarios/accra/baseline_and_scenarios.csv")
dataset <- filter(rd, ! trip_mode %in% c('Short Walking', "99", "Train", "Other", "Unspecified"))
nscen<- length(unique(dataset$scenario)) -1

###emission inventory file
trans_emissions <- read_csv("data/emission calculations accra/transport_emission_inventory_accra.csv")
names(trans_emissions)<- c("vehicle_type", "delhi_fleet_2011", "delhi_fleet_perHH", "accra_fleet_2010", "PM2_5_emiss_fact", "base_emissions")
n= which(names(scen_dist)=="Baseline") ## the column where baseline distances are in the scenario distance file
p= ncol(trans_emissions)
for (i in 1:nscen)
{
trans_emissions[1,p+i]<- trans_emissions$base_emissions[1]*scen_dist[4,n+i]/scen_dist[4,n] ## scenario emissions of 4W1
trans_emissions[2,p+i]<- trans_emissions$base_emissions[2]*scen_dist[4,n+i]/scen_dist[4,n] ## scenario emissions of 4W2 (>2000cc engine size)
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

#trans_emissions[,1]<-as.character(trans_emissions[,1])
trans_emissions[nrow(trans_emissions),1]<-"Total"  
total_row<- nrow(trans_emissions)

trans_emissions[nrow(trans_emissions)+1,1]<-"pm_conc_total"
trans_emissions
trans_emissions[nrow(trans_emissions),ncol(trans_emissions)-nscen]<- pm_conc_base
trans_emissions[nrow(trans_emissions)+1,1]<-"pm_conc_transport"
trans_emissions[nrow(trans_emissions), ncol(trans_emissions)-nscen]<- pm_trans_share*pm_conc_base

repeat{
  trans_emissions[nrow(trans_emissions), ncol(trans_emissions)-(nscen-1)]<- pm_trans_share*pm_conc_base*trans_emissions[total_row, ncol(trans_emissions)-(nscen-1)]/trans_emissions[total_row, ncol(trans_emissions)-nscen]
  nscen<- nscen -1
  if(nscen==0){
    break
  }
}
nscen<-length(unique(dataset$scenario)) -1  

non_transport_pm_conc<- pm_conc_base -  (pm_trans_share*pm_conc_base)  ### this is the concentration contributed by non-transport share (remains constant across the scenarios)

repeat{
  trans_emissions[nrow(trans_emissions)-1,ncol(trans_emissions)-(nscen-1)]<-  non_transport_pm_conc + trans_emissions[nrow(trans_emissions),ncol(trans_emissions)-(nscen-1)]
  nscen<- nscen -1
  if(nscen==0){
    break
  }
}
nscen<-length(unique(dataset$scenario)) -1



conc_pm <- trans_emissions[nrow(trans_emissions)-1, 6:(6+nscen)] ## background concentrations for baseline and all scenarios

rd <- read_csv("data/scenarios/accra/baseline_and_scenarios.csv")


lookup_ratio_pm <-  read_csv('data/synth_pop_data/accra/pollution/pm_exposure_ratio_look_up.csv')

lookup_ratio_pm <- rename(lookup_ratio_pm, trip_mode = Mode)

# "Baseline"   "Scenario 1" "Scenario 2" "Scenario 3"
scen <- unique(rd$scenario)

scen_shortened_name<-c("base")
for ( i in 2: nscen+1)
{
  
  scen_shortened_name[i]<- paste0("scen", i-1) 
}

i <- 1 ## used in the loop late
### following code generates final_data
for (scen_index in scen){
  
  #scen_index <- "Baseline"
  
  rd_scen <- filter(rd, scenario == scen_index)
  
  rd_scen <- rd_scen %>% left_join(lookup_ratio_pm, "trip_mode")  ## attaching the file with in-vehicle ratio and ventilation rate
  
  for (j in 1: nrow(rd_scen))  ## calculating on-road transport PM dose
  {
    rd_scen$pm_dose[j] <-  as.numeric(rd_scen[j, 'trip_duration']) * rd_scen$ratio[j] * as.numeric(conc_pm[i])*rd_scen$vent_rate[j] / 60
    rd_scen$on_road_air[j] <- as.numeric(rd_scen[j, 'trip_duration']) * rd_scen$vent_rate[j] / 60
  }
  
  rd_scen$participant_id <- as.factor(rd_scen$participant_id)
  
  individual_data <- rd_scen %>% group_by(participant_id) %>% summarise(on_road_dur = sum(trip_duration,na.rm=TRUE), 
                                                                        on_road_pm = sum(pm_dose,na.rm=TRUE), 
                                                                        on_road_pm= sum(pm_dose,na.rm=TRUE), 
                                                                        air_inhaled = sum(on_road_air,na.rm=TRUE))
    
  for (j in 1: nrow(individual_data)) ## getting the concentrations
  {
    individual_data$pm_conc[j] <- (((24- ((individual_data$on_road_dur[j]) / 60)) * as.numeric(conc_pm[i])*10) + individual_data$on_road_pm[j])/(((24- ((individual_data$on_road_dur[j])/60))*10)+individual_data$air_inhaled[j])
  }
  
  individual_data <- subset(individual_data, select=c("participant_id", "pm_conc"))
  names(individual_data)[2] <- paste0('pm_conc_',scen_shortened_name[i])
  
  if (i == 1 )
  {
    final_data<-individual_data 
  }else{
    final_data<- final_data %>%  left_join(individual_data,by="participant_id")
  }
  
  i <- i + 1
}

final_data<- final_data[]

length(unique(final_data$participant_id))
str(final_data)
#means<-final_data %>% summarise (mean_base=mean(pm_conc_base),mean_scen1=mean(pm_conc_scen1),mean_scen2=mean(pm_conc_scen2),mean_scen3=mean(pm_conc_scen3))
#means<-as.data.frame(means)  ### mean of PM2.5 concentrations for all individuals in baseline and scenarios
###Lines which are normalising the concentrations
#final_data[,2]<- final_data[,2]* as.numeric(conc_pm[1])/as.numeric(means[1])  ## multiplying by the ratio of baseline background concentration and background concentration of scenarios population
#final_data[,3]<- final_data[,3]* as.numeric(conc_pm[1])/as.numeric(means[2])
#final_data[,4]<- final_data[,4]* as.numeric(conc_pm[1])/as.numeric(means[3])
#final_data[,5]<- final_data[,5]* as.numeric(conc_pm[1])/as.numeric(means[4])
write_csv(as.data.frame(final_data), 'data/synth_pop_data/accra/pollution/individual_level_pm_conc_scenarios.csv')
