#####A Notes -------------------------------------------------------------------
## These codes were initiated by Raul. Lambed is modifying the codes to generate trip datasets for the selected TIGTHAT cities. Please change work directory from J to V if you are on medschool network. 

#####A Required Packages and functions -------------------------------------------------------
library(tidyverse)
library(readxl)
library(haven)
library(nnet)

quality_check <- function(trip){
    
    trip_per_capita <- trip %>% distinct(trip_id) %>% nrow / trip %>% distinct(participant_id) %>% nrow
    mode_share <- trip %>% filter(!is.na(trip_id)) %>% group_by(trip_mode) %>% summarise(mode_share = n()*100/nrow(.))
    avg_mode_time <- trip %>% filter(!is.na(trip_id)) %>% group_by(trip_mode) %>% summarise(avg_mode_time = mean(trip_duration, na.rm = T))
    
    
    print(trip_per_capita)
    print(mode_share)
    print(avg_mode_time)
    
   # par(mfrow=c(2,2))
   # plot(trip$trip_duration)
   # boxplot(trip_duration ~ trip_mode, trip)
   # hist(trip$trip_duration[which(trip$trip_mode == "walk")], breaks = 50)
   # plot(density(trip$trip_duration[which(trip$trip_mode == "walk")], bw = 100))
}

#####Argentina Buenos Aires############
setwd('J:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Argentina/WP1-TS/Buenos Aires/')

person_0 <- read_sav('ENMODO_PERSONAS_pub_20121115.sav')
trip_0 <- read_sav("ENMODO_VIAJES_pub_20121115.sav")
stage_0 <- read_sav('ENMODO_ETAPAS_pub_20121115.sav')
lookup_trip_purpose <- read_excel("lookup_trip_purpose.xlsx",sheet = 1, range = cell_cols("A:B"))
lookup_stage_mode <- read_excel("lookup_stage_mode.xlsx",sheet = 1, range = cell_cols("A:C"))# also ranks the modes 

#keep relevant variables
person <- person_0[,c("IDP", "EDAD", "SEXO")]
trip <- trip_0[, c("IDP", "IDV", "ACTIDEST","HORAFIN", "HORAINI", "HORASALI", "MINSALID", "HORALLEG", "MINLLEGA")] %>% 
    mutate(trip_duration_1 = ((HORALLEG - HORASALI)%%24)*60 + (MINLLEGA - MINSALID), 
           trip_duration_2 = difftime(HORAFIN,HORAINI,tz="GMT",units="mins"),
           trip_duration = ifelse(trip_duration_1 > 540, abs(trip_duration_2), trip_duration_1)) %>% 
    left_join(lookup_trip_purpose) %>%
    {.[,c(1,2,12,13)]}
stage <- stage_0[,c("IDP","IDV","IDE","MODOTRAN", "DURAHORA", "DURAMINU")] %>% # select relevant data
    left_join(lookup_stage_mode) %>%  #add mode names in english and ranks
    left_join(count(.,IDV)) %>%  # add number of stages for each trip
    left_join(trip[,c("IDV", "trip_duration")]) %>% 
    mutate(stage_duration = ifelse(DURAMINU == 99 & n == 1, trip_duration,
                                   ifelse(DURAMINU == 99 & n > 1, NA, 60*DURAHORA + DURAMINU))) %>% 
    
    left_join(group_by(., stage_mode) %>% summarise(average_mode_time = mean(stage_duration, na.rm=T))) %>% 
    
    group_by(IDV) %>% 
    mutate(trip_mode = stage_mode[which.is.max(rank)], # add trip main mode
           average_mode_time = ifelse(DURAMINU == 99 & n > 1 & "walk" %in% levels(as.factor(stage_mode)) & stage_mode == "walk", 1,
                                      ifelse(DURAMINU == 99 & n > 1 & "walk" %in% levels(as.factor(stage_mode)) & stage_mode != "walk",6,average_mode_time))) %>% 
    
    left_join(group_by(.,IDV) %>% summarise(sum_average = sum(average_mode_time))) %>% 
    mutate(stage_duration = ifelse(is.na(stage_duration), round(trip_duration*average_mode_time/sum_average), stage_duration)) %>% 
    
    {.[,c("IDP", "IDV", "IDE", "stage_mode", "stage_duration", "trip_mode")]}
       

#Join the three datasets and rename variables
trip <- person %>% left_join(trip) %>% left_join(stage)
names(trip) <- c("participant_id", "age","sex", "trip_id","trip_duration", "trip_purpose", "stage_id", "stage_mode","stage_duration", "trip_mode")

quality_check(trip)
#write.csv(trip, "trips_buenas_aires.csv")

#####Argentina Cordoba###########
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Argentina/WP1-TS/')
cba_hh<-read.csv('Cordoba/cba-hh.csv')
cba_pp<-read.csv('Cordoba/cba-pp.csv')
cba_trip<-read.csv('Cordoba/cba-trip.csv')
cba_stg<-read.csv('Cordoba/cba-stages.csv')

cba_pp$female<-0
cba_pp$female[which(cba_pp$sexo=="Fem")]<-1
pp_sel<-subset(cba_pp, select=c("id_hogar","id_pers","edad", "female" )) ## selecting the individual level variables

cba_trip$trip_duration_min<-(cba_trip$Llegada-cba_trip$Salida)/60 ## calculating the travel time in minutes
for (i in 1:nrow(cba_trip))
{
  if (cba_trip$trip_duration_min[i]<0)
  {
    cba_trip$trip_duration_min[i]<-((cba_trip$Llegada[i]+(24*3600))-cba_trip$Salida)/60
  }
}

trip_sel<-subset(cba_trip, select=c("id_pers","id_viaje","distancia","motivo_viaje","medio_viaje","trip_duration_min", "factores" )) ## selecting the trip-level variables

trip_sel<-trip_sel %>% left_join(pp_sel, by="id_pers")


#assigning trip purpose using ACTIDEST in bsas_trip
trip_sel$trip_purpose<-0
trip_sel$trip_purpose[trip_sel$motivo_viaje=="Trabajo (lugar de)"]<-1  ###commuting as 1

##making binary variable for travel mode (1= bicycle)
trip_sel$trip_main_mode[trip_sel$medio_viaje!="Bicicleta"]<- 0
trip_sel$trip_main_mode[trip_sel$medio_viaje=="Bicicleta"]<- 1

trip_sel$city<- "Cordoba"
trip_sel$country<-"Argentina"
trip_sel$age_cat<-'NA'
trip_sel$urban<-1
trip_sel$year<-2009

trip_sel<- subset(trip_sel, select=c("country", "city","urban","id_hogar","id_pers","female", "edad", "id_viaje", "distancia","trip_duration_min", "trip_purpose", "trip_main_mode", "age_cat", "factores","year"))
colnames(trip_sel)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip","year")
a<-rbind(a, trip_sel)

rm("cba_hh", "cba_pp", "cba_stg", "cba_trip","trip_sel")

#####Argentina Mendoza##########
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Argentina/WP1-TS/')
mza_hh<-read.csv('Mendoza/mza-hh.csv')
mza_pp<-read.csv('Mendoza/mza-pp.csv')
mza_trip<-read.csv('Mendoza/mza-trip.csv')
mza_stg<-read.csv('Mendoza/mza-stages.csv')

mza_pp$female<-0
mza_pp$female[which(mza_pp$sexo=="Femenino")]<-1


pp_sel<-subset(mza_pp, select=c("Formulario","nro_pers","edad_max", "female" )) ## selecting the individual level variables
colnames(pp_sel)[2]<-"Nro_Pers"   ## the variable starts with capital letter in trips

pp_sel$hh_id[nchar(pp_sel$Formulario)==4] <-  paste(pp_sel$Formulario[nchar(pp_sel$Formulario)==4], sep="")
pp_sel$hh_id[nchar(pp_sel$Formulario)==3] <-  paste("0",pp_sel$Formulario[nchar(pp_sel$Formulario)==3], sep="")
pp_sel$hh_id[nchar(pp_sel$Formulario)==2] <-  paste("00",pp_sel$Formulario[nchar(pp_sel$Formulario)==2], sep="")
pp_sel$hh_id[nchar(pp_sel$Formulario)==1] <-  paste("000",pp_sel$Formulario[nchar(pp_sel$Formulario)==1], sep="")

pp_sel$pers_id[nchar(pp_sel$Nro_Pers)==1] <-  paste("0",pp_sel$Nro_Pers[nchar(pp_sel$Nro_Pers)==1], sep="")
pp_sel$pers_id[nchar(pp_sel$Nro_Pers)==2] <-  paste(pp_sel$Nro_Pers[nchar(pp_sel$Nro_Pers)==2], sep="")

pp_sel$ind_id<- paste(pp_sel$hh_id,pp_sel$pers_id,sep="")
pp_sel$ind_id<-as.numeric(pp_sel$ind_id)


mza_trip$hh_id[nchar(mza_trip$Formulario)==4] <-  paste(mza_trip$Formulario[nchar(mza_trip$Formulario)==4], sep="")
mza_trip$hh_id[nchar(mza_trip$Formulario)==3] <-  paste("0",mza_trip$Formulario[nchar(mza_trip$Formulario)==3], sep="")
mza_trip$hh_id[nchar(mza_trip$Formulario)==2] <-  paste("00",mza_trip$Formulario[nchar(mza_trip$Formulario)==2], sep="")
mza_trip$hh_id[nchar(mza_trip$Formulario)==1] <-  paste("000",mza_trip$Formulario[nchar(mza_trip$Formulario)==1], sep="")

mza_trip$pers_id[nchar(mza_trip$Nro_Pers)==1] <-  paste("0",mza_trip$Nro_Pers[nchar(mza_trip$Nro_Pers)==1], sep="")
mza_trip$pers_id[nchar(mza_trip$Nro_Pers)==2] <-  paste(mza_trip$Nro_Pers[nchar(mza_trip$Nro_Pers)==2], sep="")

mza_trip$ind_id<- paste(mza_trip$hh_id,mza_trip$pers_id,sep="")
mza_trip$ind_id<-as.numeric(mza_trip$ind_id)



trip_sel<-subset(mza_trip, select=c("ind_id","Nro_Viaje","activ_dest","mododefinitivo2_max","durac_viaje", "FEX" )) ## selecting the trip-level variables
trip_sel<- left_join(trip_sel,pp_sel, by="ind_id")

#assigning trip purpose using ACTIDEST in bsas_trip
trip_sel$trip_purpose<- 0
trip_sel$trip_purpose[trip_sel$activ_dest=="Trabajo (lugar de)"]<-1  ###commuting as 1

###making binary variable for travel mode (1= bicycle)
trip_sel$trip_main_mode[trip_sel$mododefinitivo2_max!="Bicicleta"]<- 0
trip_sel$trip_main_mode[trip_sel$mododefinitivo2_max=="Bicicleta"]<- 1

trip_sel$trip_distance<-NA


trip_sel$city<- "Mendoza"
trip_sel$country<-"Argentina"
trip_sel$age_cat<-'NA'
trip_sel$urban<-1
trip_sel$year<-2010
trip_sel<- subset(trip_sel, select=c("country", "city","urban","Formulario","ind_id","female", "edad_max", "Nro_Viaje", "trip_distance","durac_viaje", "trip_purpose", "trip_main_mode", "age_cat", "FEX","year"))
colnames(trip_sel)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip","year")

a<-rbind(a, trip_sel)
rm("mza_hh", "mza_pp", "mza_stg", "mza_trip","trip_sel")

#####Argentina Neuqu?n y Cipolletti#############
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Argentina/WP1-TS/')
nqn_cipp_hh<-read.csv('Neuqu?n y Cipolletti/nqn_cipp-hh.csv')
nqn_cipp_pp<-read.csv('Neuqu?n y Cipolletti/nqn_cipp-pp.csv')
nqn_cipp_trip<-read.csv('Neuqu?n y Cipolletti/nqn_cipp-trip.csv')
nqn_cipp_stg<-read.csv('Neuqu?n y Cipolletti/nqn_cipp-stages.csv')
nqn_cipp_pp$female<-0
nqn_cipp_pp$female[which(nqn_cipp_pp$Sexo=="Mujer")]<-1
pp_sel<-subset(nqn_cipp_pp, select=c("ID_HOGAR","ID_CP","Edad", "female" )) ## selecting the individual level variables
trip_sel<-subset(nqn_cipp_trip, select=c("ID_CP","ID_VIAJE","Act_Destino","Tiempo_Modo" ,"FEX")) ## selecting the trip-level variables

###making binary variable for travel mode (1= bicycle)
nqn_cipp_stg$trip_main_mode[nqn_cipp_stg$Tipo_Modo!="Bicicleta"]<- 0
nqn_cipp_stg$trip_main_mode[nqn_cipp_stg$Tipo_Modo=="Bicicleta"]<- 1

#assigning trip purpose using ACTIDEST in bsas_trip
trip_sel$trip_purpose<-0
trip_sel$trip_purpose[trip_sel$Act_Destino=="Trabajo (lugar de)"]<-1  ###commuting as 1

stg_sel<-subset(nqn_cipp_stg, select=c( "ID_VIAJE","trip_main_mode")) 

trip_sel<-trip_sel %>% left_join(pp_sel, by="ID_CP")
trip_sel<-trip_sel %>% left_join(stg_sel, by="ID_VIAJE")


trip_sel$city<- "Neuqu?n y Cipolletti"
trip_sel$country<-"Argentina"

trip_sel$trip_distance<-NA
trip_sel$age_cat<-'NA'
trip_sel$urban<-1
trip_sel$year<-2012

trip_sel<- subset(trip_sel, select=c("country", "city","urban","ID_HOGAR","ID_CP","female", "Edad", "ID_VIAJE", "trip_distance","Tiempo_Modo", "trip_purpose", "trip_main_mode", "age_cat", "FEX", "year"))
colnames(trip_sel)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip","year")
a<-rbind(a, trip_sel)
rm("nqn_cipp_hh", "nqn_cipp_pp", "nqn_cipp_stg", "nqn_cipp_trip","trip_sel")

#####Argentina Posadas###############
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Argentina/WP1-TS/')
posadas_hh<-read.csv('Posadas/posadas-hh.csv')
posadas_pp<-read.csv('Posadas/posadas-pp.csv')
posadas_trip<-read.csv('Posadas/posadas-trip.csv')
posadas_stg<-read.csv('Posadas/posadas-stages.csv')

posadas_pp$female<-0
posadas_pp$female[which(posadas_pp$Sexo=="Mujer")]<-1

pp_sel<-subset(posadas_pp, select=c("FORMULARIO","PersID","Edad", "female" )) ## selecting the individual level variables
trip_sel<-subset(posadas_trip, select=c("PersID","ViajeID","ActividadDestino","Medio_Transporte","TiempoViaje","Distancia", "FEX" )) ## selecting the trip-level variables
trip_sel<-trip_sel %>% left_join(pp_sel, by="PersID")

###making binary variable for travel mode (1= bicycle)
trip_sel$trip_main_mode[trip_sel$Medio_Transporte!="Bicicleta"]<- 0
trip_sel$trip_main_mode[trip_sel$Medio_Transporte=="Bicicleta"]<- 1

#assigning trip purpose using ACTIDEST in bsas_trip
trip_sel$trip_purpose<-0
trip_sel$trip_purpose[trip_sel$ActividadDestino=="Trabajo (lugar de)"]<-1  ###commuting as 1

trip_sel$city<- "Posadas"
trip_sel$country<-"Argentina"
trip_sel$age_cat<-'NA'
trip_sel$urban<-1
trip_sel$year<-2010

trip_sel<- subset(trip_sel, select=c("country", "city","urban","FORMULARIO","PersID","female", "Edad", "ViajeID", "Distancia","TiempoViaje", "trip_purpose", "trip_main_mode", "age_cat", "FEX", "year"))
colnames(trip_sel)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")

a<-rbind(a, trip_sel)
rm("posadas_hh", "posadas_pp", "posadas_stg", "posadas_trip","trip_sel")


#####Argentina Resistencia-Corrientes#############
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Argentina/WP1-TS/')
res_corr_hh<-read.csv('Resistencia-Corrientes/res_corr-hh.csv')
res_corr_pp<-read.csv('Resistencia-Corrientes/res_corr-pp.csv')
res_corr_trip<-read.csv('Resistencia-Corrientes/res_corr-trip.csv')
res_corr_stg<-read.csv('Resistencia-Corrientes/res_corr-stages.csv')

res_corr_pp$female<-0
res_corr_pp$female[which(res_corr_pp$Sexo=="Mujer")]<-1

pp_sel<-subset(res_corr_pp, select=c("ID_HOGAR","ID_CP","Edad", "female" )) ## selecting the individual level variables
trip_sel<-subset(res_corr_trip, select=c("ID_CP","ID_VIAJE","VI_actividad_destino", "FEX" )) ## selecting the trip-level variables
trip_sel<-trip_sel %>% left_join(pp_sel, by="ID_CP")

x<-as.data.frame(res_corr_stg %>% group_by(ID_VIAJE) %>% summarise(n())) ###number of stages per trip

res_corr_stg<-res_corr_stg %>% left_join(x, by="ID_VIAJE")  ## joining number of stages in the dataset

res_corr_stg[which(res_corr_stg$`n()`>1 & res_corr_stg$VII_tipo_medio =="Bicicleta"),] ## only two cases with cycle as a mode and more than one stage


# summing up travel time for all stages in each trip and attaching with the main stages dataset
res_corr_stg <- res_corr_stg %>% left_join(as.data.frame(res_corr_stg %>% group_by(ID_VIAJE) %>% summarise(trip_duration_min=sum(VII_tiempo_transporte))),by="ID_VIAJE")

###making binary variable for travel mode (1= bicycle)
res_corr_stg$trip_main_mode[res_corr_stg$VII_tipo_medio!="Bicicleta"]<- 0
res_corr_stg$trip_main_mode[res_corr_stg$VII_tipo_medio=="Bicicleta"]<- 1

stg_sel<- subset(res_corr_stg, select=c("ID_VIAJE","trip_main_mode","trip_duration_min"))

trip_sel<-trip_sel %>% left_join(stg_sel, by="ID_VIAJE")

#assigning trip purpose using ACTIDEST in bsas_trip
trip_sel$trip_purpose<-0
trip_sel$trip_purpose[trip_sel$VI_actividad_destino=="Trabajo (lugar de)"]<-1  ###commuting as 1

trip_sel$trip_distance<-NA


trip_sel$city<- "Resistencia-Corrientes"
trip_sel$country<-"Argentina"
trip_sel$age_cat<-'NA'
trip_sel$urban<-1
trip_sel$year<-2013

trip_sel<-subset(trip_sel, select=c("country", "city","urban","ID_HOGAR","ID_CP","female","Edad","ID_VIAJE","trip_distance","trip_duration_min","trip_purpose","trip_main_mode", "age_cat", "FEX" , "year")) ## selecting the trip-level variables

colnames(trip_sel)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trip_sel)
rm("res_corr_hh", "res_corr_pp", "res_corr_stg", "res_corr_trip","trip_sel")

#####Argentina Rosario##############
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Argentina/WP1-TS/')
rosario_hh<-read.csv('Rosario/rosario-hh.csv')
rosario_pp<-read.csv('Rosario/rosario-pp.csv')
rosario_trip<-read.csv('Rosario/rosario-trip.csv')

## trip file in Rosario consists of stages
## assigning lowest hierarchy to walk (1), middle to cycle (2) and 3 to all others: maximum of these in a given trip is assigned as the main_mode
rosario_trip$mode_hier<- 3
rosario_trip$mode_hier[which(rosario_trip$p08_modo=="A pie")]<-1
rosario_trip$mode_hier[which(rosario_trip$p08_modo=="Bicicleta")]<-2
x<-as.data.frame(rosario_trip %>% group_by(Id_viaje) %>% summarise(max(mode_hier))) ###number of stages per trip
rosario_trip<-rosario_trip %>% left_join(x, by="Id_viaje")  ## joining number of stages in the dataset
#making binary variable for travel mode 
rosario_trip$trip_main_mode[rosario_trip$mode_hier==2]<-1 ## assigning bicycle value of 1
rosario_trip$trip_main_mode[rosario_trip$mode_hier!=2]<- 0

rosario_pp$female<-0
rosario_pp$female[which(rosario_pp$p03_sexo=="Femenino")]<-1

pp_sel<-subset(rosario_pp, select=c("Id_hogar","Id_pers","female", "p04_edad" )) ## selecting the individual level variables
trip_sel<-subset(rosario_trip, select=c("Id_pers","Id_viaje","p032_actD" ,"trip_main_mode", "p051_durac", "ponder_v")) ## selecting the trip-level variables
trip_sel$trip_main_mode<- 0
trip_sel$trip_main_mode[which(trip_sel$p08_modo=="Bicicleta")]<- 1
trip_sel$trip_purpose<- 0
trip_sel$trip_purpose[which(trip_sel$p032_actD=="Trabajo (lugar de)")]<- 1

trip_sel<-trip_sel %>% left_join(pp_sel, by="Id_pers")

trip_sel$city<- "Rosario"
trip_sel$country<-"Argentina"
trip_sel$age_cat<-'NA'
trip_sel$urban<-1
trip_sel$year<-2008

trip_sel$trip_distance<-NA
trip_sel<-subset(trip_sel, select=c("country", "city","urban","Id_hogar","Id_pers","female", "p04_edad","Id_viaje","trip_distance", "p051_durac","trip_purpose" ,"trip_main_mode", "age_cat", "ponder_v", "year"))
colnames(trip_sel)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trip_sel)
rm("rosario_hh", "rosario_pp", "rosario_trip","trip_sel")

#####Argentina Salta###########
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Argentina/WP1-TS/')
salta_hh<-read.csv('Salta/salta-hh.csv')
salta_pp<-read.csv('Salta/salta-pp.csv')
salta_trip<-read.csv('Salta/salta-trip.csv')
salta_stages<-read.csv('Salta/salta-stages.csv')
pp_sel<-subset(salta_pp, select=c("ID_HOGAR","ID_CP","Sexo", "Edad_pers" )) ## selecting the individual level variables

pp_sel$female<-0
pp_sel$female[which(pp_sel$Sexo=="Mujer")]<-1

salta_trip$trip_purpose<-0
salta_trip$trip_purpose[salta_trip$Motivos=="Trabajo (lugar de)"]<-1

trip_sel<-subset(salta_trip, select=c("ID_CP","ID_VIAJE","trip_purpose" ,"FEX"))
trip_sel<-trip_sel %>% left_join(pp_sel, by="ID_CP")

x<-as.data.frame(salta_stages %>% group_by(ID_VIAJE) %>% summarise(n())) ###number of stages per trip

salta_stages<-salta_stages %>% left_join(x, by="ID_VIAJE")  ## joining number of stages in the dataset

trips_cycle<-salta_stages$ID_VIAJE[which(salta_stages$`n()`>1 & salta_stages$Modo =="Bicicleta")] ## only 4 cases with cycle as a mode and more than one stage
# in all cases duration was same for all the stages, therefore, they were allocated cycle as their main mode

salta_stages$trip_main_mode<-0
salta_stages$trip_main_mode[which(salta_stages$Modo =="Bicicleta")]<-1
salta_stages$trip_main_mode[which(salta_stages$ID_VIAJE  %in% trips_cycle)]<-1  ## for the cases with two stages and one stage with cycle

# summing up travel time for all stages in each trip and attaching with the main stages dataset
salta_stages <- salta_stages %>% left_join(as.data.frame(salta_stages %>% group_by(ID_VIAJE) %>% summarise(trip_duration_min=sum(Tiempo_transporte))),by="ID_VIAJE")

stg_sel<-subset(salta_stages, select=c("ID_VIAJE","trip_main_mode", "trip_duration_min"))

trip_sel<-trip_sel %>% left_join(stg_sel, by="ID_VIAJE")

trip_sel$city<- "Salta"
trip_sel$country<-"Argentina"
trip_sel$age_cat<-'NA'
trip_sel$urban<-1
trip_sel$trip_distance<-NA
trip_sel$year<-2012
trip_sel<-subset(trip_sel, select=c("country", "city","urban","ID_HOGAR","ID_CP","female", "Edad_pers","ID_VIAJE","trip_distance", "trip_duration_min","trip_purpose" ,"trip_main_mode", "age_cat", "FEX", "year"))
colnames(trip_sel)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trip_sel)
rm("salta_hh", "salta_pp", "salta_trip","salta_stages","trip_sel")

#####Argentina Tucuman#############
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Argentina/WP1-TS/')
tucuman_hh<-read.csv('Tucuman/tucuman-hh.csv')
tucuman_pp<-read.csv('Tucuman/tucuman-pp.csv')
tucuman_trip<-read.csv('Tucuman/tucuman-trip.csv')
tucuman_stages<-read.csv('Tucuman/tucuman-stages.csv')

pp_sel<-subset(tucuman_pp, select=c("ID_PERS", "FORMULARIO","SEXO", "EDAD"))
pp_sel$female<-0
pp_sel$female[which(pp_sel$Sexo=="Mujeres")]<-1

tucuman_trip$trip_main_mode<-0 
tucuman_trip$trip_main_mode[which(tucuman_trip$Medio_Transporte=="Bicicleta")]<-1
tucuman_trip$trip_purpose<-0
tucuman_trip$trip_purpose[which(tucuman_trip$ACT_DEST_VIAJE=="Trabajo (lugar de)")]<-1
tucuman_trip$trip_duration_min=tucuman_trip$DURACION_VIAJE/60

trip_sel<-subset(tucuman_trip, select=c("ID_PERS","ID_VIAJE","trip_purpose","trip_main_mode", "trip_duration_min", "FEX"))
trip_sel<-trip_sel %>% left_join(pp_sel, by="ID_PERS")

trip_sel$city<- "Tucuman"
trip_sel$country<-"Argentina"
trip_sel$age_cat<-'NA'
trip_sel$urban<-1
trip_sel$year<-2011

trip_sel$trip_distance<-NA
trip_sel<-subset(trip_sel, select=c("country", "city","urban","FORMULARIO","ID_PERS","female", "EDAD","ID_VIAJE","trip_distance", "trip_duration_min","trip_purpose" ,"trip_main_mode","age_cat", "FEX", "year"))
colnames(trip_sel)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trip_sel)
rm("tucuman_hh", "tucuman_pp", "tucuman_trip","tucuman_stages","trip_sel", "pp_sel")

#####Australia South East Queenslandout#####
## Defining a work trip
## If DESTPURP1 is 7 (work) it is a work trip
## If DESTPURP1 is 8 (home) and ORIGPURP1 is 7 is is a work trip
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Australia/Australian travel survey/Queensland/Original data')
person<-read.csv('SEQ_HTS_PERSONS.csv')
trips<- read.csv('SEQ_HTS_TRIPS.csv')
HHS<- read.csv('SEQ_HTS_HOUSEHOLDS.csv')

HHS<-subset(HHS, select=c( "HHID", "AREA"))

HHS$geog[HHS$AREA==1]<-'Brisbane'
HHS$geog[HHS$AREA==2]<-'Gold Coast'
HHS$geog[HHS$AREA==3]<-'Sunshine Coast'
for (i in 1: nrow(trips))
{
    
    if (trips$ORIGPURP1[i] == 7 & trips$DESTPURP1[i]==8)
    {
        trips$purpose[i]<- 1
    }
    else if (trips$DESTPURP1[i]==7)
    {
        trips$purpose[i]<-1
    }
    else 
    {
        trips$purpose[i]<-0
    }
}

person<-subset(person, select=c("PERSID", "HHID", "AGE","SEX"))
trips<-trips %>% left_join(person, by="PERSID")
trips<-trips %>% left_join(HHS, by="HHID")

trips$urban<-1
trips$country<-"Australia"
trips$mode<-0
trips$mode[trips$MODE1 ==5]<-1  ###cycling as 1
trips$gender[trips$SEX==1]<-0
trips$gender[trips$SEX==2]<-1
trips$age_cat<-'NA'
trips$distance<-trips$NETWORK_DIST
trips$year<-2015

trips<-subset(trips, select=c("country", "geog", "urban","HHID", "PERSID", "gender", "AGE",  "TRIPID","distance", "TOTTRIPTIME", "purpose", "mode", "age_cat", "NRTWGT", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips", "HHS")

#####Australia Goldstone#####
## Defining a work trip
## If DESTPURP1 is 7 (work) it is a work trip
## If DESTPURP1 is 8 (home) and ORIGPURP1 is 7 is is a work trip
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Australia/Australian travel survey/Queensland/Original data')
person<-read.csv('GLD_PERSONS.csv')
trips<- read.csv('GLD_TRIPS.csv')
for (i in 1: nrow(trips))
{
    
    if (trips$ORIGPURP1[i] == 7 & trips$DESTPURP1[i]==8)
    {
        trips$purpose[i]<- 1
    }
    else if (trips$DESTPURP1[i]==7)
    {
        trips$purpose[i]<-1
    }
    else 
    {
        trips$purpose[i]<-0
    }
}

person<-subset(person, select=c("PERSID", "HHID", "AGE","SEX"))
trips<-trips %>% left_join(person, by="PERSID")
#trips<-trips %>% left_join(HHS, by="HHID")

trips$urban<-1
trips$country<-"Australia"
trips$geog<-"Goldstone"
trips$mode<-0
trips$mode[trips$MODE1 ==5]<-1  ###cycling as 1
trips$gender[trips$SEX==1]<-0
trips$gender[trips$SEX==2]<-1
trips$age_cat<-'NA'
trips$distance<-trips$NETWORK_DIST
trips$year<-2010

trips<-subset(trips, select=c("country", "geog", "urban","HHID", "PERSID", "gender", "AGE",  "TRIPID","distance", "TOTTRIPTIME", "purpose", "mode", "age_cat", "NRTWGT", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips")

#####Australia Wide Bay Burnett#####
## Defining a work trip
## If DESTPURP1 is 7 (work) it is a work trip
## If DESTPURP1 is 8 (home) and ORIGPURP1 is 7 is is a work trip
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Australia/Australian travel survey/Queensland/Original data')
person<-read.csv('WBB_PERSONS.csv')
trips<- read.csv('WBB_TRIPS.csv')
HHS<- read.csv('WBB_HHS.csv')

HHS<-subset(HHS, select=c( "HHID", "AREA"))

HHS$geog[HHS$AREA==1]<-'Bundaberg'
HHS$geog[HHS$AREA==2]<-'Burnett'
HHS$geog[HHS$AREA==3]<-'Hervey Bay'
HHS$geog[HHS$AREA==4]<-'Maryborough'

for (i in 1: nrow(trips))
{
    
    if (trips$ORIGPURP1[i] == 7 & trips$DESTPURP1[i]==8)
    {
        trips$purpose[i]<- 1
    }
    else if (trips$DESTPURP1[i]==7)
    {
        trips$purpose[i]<-1
    }
    else 
    {
        trips$purpose[i]<-0
    }
}

person<-subset(person, select=c("PERSID", "HHID", "AGE","SEX"))
trips<-trips %>% left_join(person, by="PERSID")
trips<-trips %>% left_join(HHS, by="HHID")

trips$urban<-1
trips$country<-"Australia"

trips$mode<-0
trips$mode[trips$MODE1 ==5]<-1  ###cycling as 1
trips$gender[trips$SEX==1]<-0
trips$gender[trips$SEX==2]<-1
trips$age_cat<-'NA'
trips$distance<-trips$NETWORK_DIST
trips$year<-2010

trips<-subset(trips, select=c("country", "geog", "urban","HHID", "PERSID", "gender", "AGE",  "TRIPID","distance", "TOTTRIPTIME", "purpose", "mode", "age_cat", "NRTWGT", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips", "HHS")


#####Australia Mackay#####
## Defining a work trip
## If DESTPURP1 is 7 (work) it is a work trip
## If DESTPURP1 is 8 (home) and ORIGPURP1 is 7 is is a work trip
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Australia/Australian travel survey/Queensland/Original data')
person<-read.csv('MKY_PERSONS.csv')
trips<- read.csv('MKY_TRIPS.csv')
for (i in 1: nrow(trips))
{
    
    if (trips$ORIGPURP1[i] == 7 & trips$DESTPURP1[i]==8)
    {
        trips$purpose[i]<- 1
    }
    else if (trips$DESTPURP1[i]==7)
    {
        trips$purpose[i]<-1
    }
    else 
    {
        trips$purpose[i]<-0
    }
}

person<-subset(person, select=c("PERSID", "HHID", "AGE","SEX"))
trips<-trips %>% left_join(person, by="PERSID")
#trips<-trips %>% left_join(HHS, by="HHID")

trips$urban<-1
trips$country<-"Australia"
trips$geog<-"Mackay"
trips$mode<-0
trips$mode[trips$MODE1 ==5]<-1  ###cycling as 1
trips$gender[trips$SEX==1]<-0
trips$gender[trips$SEX==2]<-1
trips$age_cat<-'NA'
trips$distance<-trips$NETWORK_DIST
trips$year<-2011

trips<-subset(trips, select=c("country", "geog", "urban","HHID", "PERSID", "gender", "AGE",  "TRIPID","distance", "TOTTRIPTIME", "purpose", "mode", "age_cat", "NRTWGT", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips")



#####Australia Townsville#####
## Defining a work trip
## If DESTPURP1 is 7 (work) it is a work trip
## If DESTPURP1 is 8 (home) and ORIGPURP1 is 7 is is a work trip
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Australia/Australian travel survey/Queensland/Original data')
person<-read.csv('TVL_PERSONS.csv')
trips<- read.csv('TVL_TRIPS.csv')
for (i in 1: nrow(trips))
{
    
    if (trips$ORIGPURP1[i] == 7 & trips$DESTPURP1[i]==8)
    {
        trips$purpose[i]<- 1
    }
    else if (trips$DESTPURP1[i]==7)
    {
        trips$purpose[i]<-1
    }
    else 
    {
        trips$purpose[i]<-0
    }
}

person<-subset(person, select=c("PERSID", "HHID", "AGE","SEX"))
trips<-trips %>% left_join(person, by="PERSID")
#trips<-trips %>% left_join(HHS, by="HHID")

trips$urban<-1
trips$country<-"Australia"
trips$geog<-"Townsville"
trips$mode<-0
trips$mode[trips$MODE1 ==5]<-1  ###cycling as 1
trips$gender[trips$SEX==1]<-0
trips$gender[trips$SEX==2]<-1
trips$age_cat<-'NA'
trips$distance<-trips$NETWORK_DIST
trips$year<-2011

trips<-subset(trips, select=c("country", "geog", "urban","HHID", "PERSID", "gender", "AGE",  "TRIPID","distance", "TOTTRIPTIME", "purpose", "mode", "age_cat", "NRTWGT", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips")


#####Australia Darling Downs and Lockyer#####
## Defining a work trip
## If DESTPURP1 is 7 (work) it is a work trip
## If DESTPURP1 is 8 (home) and ORIGPURP1 is 7 is is a work trip
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Australia/Australian travel survey/Queensland/Original data')
person<-read.csv('DD_PERSONS.csv')
trips<- read.csv('DD_TRIPS.csv')
HHS<- read.csv('DD_HOUSEHOLDS.csv')
areas<- read.csv('DDL_AREA.csv')

HHS<-subset(HHS, select=c( "HHID", "AREA"))
HHS<-HHS %>% left_join(areas, by="AREA")

for (i in 1: nrow(trips))
{
    
    if (trips$ORIGPURP1[i] == 7 & trips$DESTPURP1[i]==8)
    {
        trips$purpose[i]<- 1
    }
    else if (trips$DESTPURP1[i]==7)
    {
        trips$purpose[i]<-1
    }
    else 
    {
        trips$purpose[i]<-0
    }
}

person<-subset(person, select=c("PERSID", "HHID", "AGE","SEX"))
trips<-trips %>% left_join(person, by="PERSID")
trips<-trips %>% left_join(HHS, by="HHID")

trips$urban<-1
trips$country<-"Australia"

trips$mode<-0
trips$mode[trips$MAINMODE ==5]<-1  ###cycling as 1
trips$gender[trips$SEX==1]<-0
trips$gender[trips$SEX==2]<-1
trips$age_cat<-'NA'
trips$distance<-trips$NETWORK_DIST
trips$year<-2012

trips<-subset(trips, select=c("country", "geog", "urban","HHID", "PERSID", "gender", "AGE",  "TRIPID","distance", "TRIPTIME", "purpose", "mode", "age_cat", "NRTWGT", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips", "HHS", "areas")


#####Australia Toowoomba Region#####
## Defining a work trip
## If DESTPURP1 is 7 (work) it is a work trip
## If DESTPURP1 is 8 (home) and ORIGPURP1 is 7 is is a work trip
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Australia/Australian travel survey/Queensland/Original data')
person<-read.csv('TWB_PERSONS.csv')
trips<- read.csv('TWB_TRIPS.csv')
for (i in 1: nrow(trips))
{
    
    if (trips$ORIGPURP1[i] == 7 & trips$DESTPURP1[i]==8)
    {
        trips$purpose[i]<- 1
    }
    else if (trips$DESTPURP1[i]==7)
    {
        trips$purpose[i]<-1
    }
    else 
    {
        trips$purpose[i]<-0
    }
}

person<-subset(person, select=c("PERSID", "HHID", "AGE","SEX"))
trips<-trips %>% left_join(person, by="PERSID")
#trips<-trips %>% left_join(HHS, by="HHID")

trips$urban<-1
trips$country<-"Australia"
trips$geog<-"Toowoomba"
trips$mode<-0
trips$mode[trips$MODE1 ==5]<-1  ###cycling as 1
trips$gender[trips$SEX==1]<-0
trips$gender[trips$SEX==2]<-1
trips$age_cat<-'NA'
trips$distance<-trips$NETWORK_DIST
trips$year<-2012

trips<-subset(trips, select=c("country", "geog", "urban","HHID", "PERSID", "gender", "AGE",  "TRIPID","distance", "TOTTRIPTIME", "purpose", "mode", "age_cat", "NRTWGT", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips")

#####Australia Cairns Region#####
## Defining a work trip
## If DESTPURP1 is 7 (work) it is a work trip
## If DESTPURP1 is 8 (home) and ORIGPURP1 is 7 is is a work trip
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Australia/Australian travel survey/Queensland/Original data')
person<-read.csv('CNS_PERSONS.csv')
trips<- read.csv('CNS_TRIPS.csv')
for (i in 1: nrow(trips))
{
    
    if (trips$ORIGPURP1[i] == 7 & trips$DESTPURP1[i]==8)
    {
        trips$purpose[i]<- 1
    }
    else if (trips$DESTPURP1[i]==7)
    {
        trips$purpose[i]<-1
    }
    else 
    {
        trips$purpose[i]<-0
    }
}

person<-subset(person, select=c("PERSID", "HHID", "AGE","SEX"))
trips<-trips %>% left_join(person, by="PERSID")
#trips<-trips %>% left_join(HHS, by="HHID")

trips$urban<-1
trips$country<-"Australia"
trips$geog<-"Cairns"
trips$mode<-0
trips$mode[trips$MODE1 ==5]<-1  ###cycling as 1
trips$gender[trips$SEX==1]<-0
trips$gender[trips$SEX==2]<-1
trips$age_cat<-'NA'
trips$distance<-trips$NETWORK_DIST
trips$year<-2014

trips<-subset(trips, select=c("country", "geog", "urban","HHID", "PERSID", "gender", "AGE",  "TRIPID","distance", "TOTTRIPTIME", "purpose", "mode", "age_cat", "NRTWGT", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips")


#####Australia Rockhampton Region#####
## Defining a work trip
## If DESTPURP1 is 7 (work) it is a work trip
## If DESTPURP1 is 8 (home) and ORIGPURP1 is 7 is is a work trip
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Australia/Australian travel survey/Queensland/Original data')
person<-read.csv('RKH_PERSONS.csv')
trips<- read.csv('RKH_TRIPS.csv')
for (i in 1: nrow(trips))
{
    
    if (trips$ORIGPURP1[i] == 7 & trips$DESTPURP1[i]==8)
    {
        trips$purpose[i]<- 1
    }
    else if (trips$DESTPURP1[i]==7)
    {
        trips$purpose[i]<-1
    }
    else 
    {
        trips$purpose[i]<-0
    }
}

person<-subset(person, select=c("PERSID", "HHID", "AGE","SEX"))
trips<-trips %>% left_join(person, by="PERSID")
#trips<-trips %>% left_join(HHS, by="HHID")

trips$urban<-1
trips$country<-"Australia"
trips$geog<-"Rockhampton"
trips$mode<-0
trips$mode[trips$MODE1 ==5]<-1  ###cycling as 1
trips$gender[trips$SEX==1]<-0
trips$gender[trips$SEX==2]<-1
trips$age_cat<-'NA'
trips$distance<-trips$NETWORK_DIST
trips$year<-2014

trips<-subset(trips, select=c("country", "geog", "urban","HHID", "PERSID", "gender", "AGE",  "TRIPID","distance", "TOTTRIPTIME", "purpose", "mode", "age_cat", "NRTWGT", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips")


#####Australia Gympie, Gayndah, Kingaroy and Tin Can Bay/Cooloola#####
## Defining a work trip
## If DESTPURP1 is 7 (work) it is a work trip
## If DESTPURP1 is 8 (home) and ORIGPURP1 is 7 is is a work trip
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Australia/Australian travel survey/Queensland/Original data')
person<-read.csv('GC_PERSONS.csv')
trips<- read.csv('GC_TRIPS.csv')
for (i in 1: nrow(trips))
{
    
    if (trips$ORIGPURP1[i] == 7 & trips$DESTPURP1[i]==8)
    {
        trips$purpose[i]<- 1
    }
    else if (trips$DESTPURP1[i]==7)
    {
        trips$purpose[i]<-1
    }
    else 
    {
        trips$purpose[i]<-0
    }
}

person<-subset(person, select=c("PERSID", "HHID", "AGE","SEX"))
trips<-trips %>% left_join(person, by="PERSID")
#trips<-trips %>% left_join(HHS, by="HHID")

trips$urban<-1
trips$country<-"Australia"
trips$geog<-"Gympie, Gayndah, Kingaroy and Tin Can Bay/Cooloola"
trips$mode<-0
trips$mode[trips$MODE1 ==5]<-1  ###cycling as 1
trips$gender[trips$SEX==1]<-0
trips$gender[trips$SEX==2]<-1
trips$age_cat<-'NA'
trips$distance<-trips$NETWORK_DIST
trips$year<-2010

trips<-subset(trips, select=c("country", "geog", "urban","HHID", "PERSID", "gender", "AGE",  "TRIPID","distance", "TOTTRIPTIME", "purpose", "mode", "age_cat", "TRIPWGT14", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips")


#####Australia Victoria#####
## Defining a work trip
## If DESTPURP1 is 7 (work) it is a work trip
## If DESTPURP1 is 8 (home) and ORIGPURP1 is 7 is is a work trip
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Australia/Australian travel survey/Victoria/Original data')
person<-read.csv('P_VISTA12_16_SA1_V1.csv')
trips<- read.csv('T_VISTA12_16_SA1_V1.csv')
for (i in 1: nrow(trips))
{
    
    if (trips$ORIGPURP1[i] == "Workplace"  & trips$DESTPURP1[i]=="At or Go Home")
    {
        trips$purpose[i]<- 1
    }
    else if (trips$DESTPURP1[i]=="Workplace")
    {
        trips$purpose[i]<-1
    }
    else 
    {
        trips$purpose[i]<-0
    }
}

person<-subset(person, select=c("PERSID", "HHID", "AGE","SEX"))
trips<-trips %>% left_join(person, by="PERSID")
#trips<-trips %>% left_join(HHS, by="HHID")

trips$urban<-1
trips$country<-"Australia"
trips$geog<-"Victoria"
trips$mode<-0
trips$mode[as.character(trips$Mode1) =="Bicycle"]<-1  ###cycling as 1
trips$gender[trips$SEX=="Male"]<-0
trips$gender[trips$SEX=="Female"]<-1
trips$age_cat<-'NA'
trips$distance<-trips$CUMDIST
trips$year<-2015

trips<-subset(trips, select=c("country", "geog", "urban","HHID.x", "PERSID", "gender", "AGE",  "TRIPNO","distance", "TRAVTIME", "purpose", "mode", "age_cat", "RP_ADTRIPWGT_LGA", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips")

#####Brazil Sao Paulo####
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Brazil/Sao Paulo/Pesquisa Origem Destino 2012')
trips<-read.csv('Mobilidade_2012_v0.csv')
str(trips)
trips<-subset(trips, select=c("ID_PESS", "ID_DOM", "N_VIAG", "DURACAO", "DISTANCIA","MODOPRIN", "SEXO", "IDADE", "FE_VIA" , "MOTIVO_D" , "MOTIVO_O"))
trips$female<-0
trips$female[which(trips$SEXO==2)]<-1
trips$city<- "Sao Paulo"
trips$urban<-1  
trips$country<-"Brazil"
trips$year<-2012
unique(trips$MODOPRIN)

for (i in 1: nrow(trips))
{
    
    
    if (!(is.na(trips$MOTIVO_D[i])) & trips$MOTIVO_D[i] == 8  & trips$MOTIVO_O[i]<4 & ! (is.na(trips$MOTIVO_O[i])) )
    {
        trips$purpose[i]<- 1
    }
    else if (! (is.na(trips$MOTIVO_D[i])) & trips$MOTIVO_D[i]<4)
    {
        trips$purpose[i]<-1
    }
    else 
    {
        trips$purpose[i]<-0
    }
}

trips$mode<-'NA'
trips$mode[which(trips$MODOPRIN==1)]<-"Omnibus" 
trips$mode[which(trips$MODOPRIN==2)]<-"Omnibus" 
trips$mode[which(trips$MODOPRIN==3)]<-"Omnibus" 
trips$mode[which(trips$MODOPRIN==4)]<-"Omnibus" 
trips$mode[which(trips$MODOPRIN==5)]<-"Bus" 
trips$mode[which(trips$MODOPRIN==6)]<-"Car" 
trips$mode[which(trips$MODOPRIN==7)]<-"Car" 
trips$mode[which(trips$MODOPRIN==8)]<-"Taxi" 
trips$mode[which(trips$MODOPRIN==9)]<-"Minibus/van" 
trips$mode[which(trips$MODOPRIN==10)]<-"Minibus/van" 
trips$mode[which(trips$MODOPRIN==11)]<-"Minibus/van" 
trips$mode[which(trips$MODOPRIN==12)]<-"Metro" 
trips$mode[which(trips$MODOPRIN==13)]<-"Tram" 
trips$mode[which(trips$MODOPRIN==14)]<-"Motorcycle" 
trips$mode[which(trips$MODOPRIN==15)]<-"Bicycle" 
trips$mode[which(trips$MODOPRIN==16)]<-"Walk" 
trips$mode[which(trips$MODOPRIN==17)]<-"Other" 
trips<- trips[which(!is.na(trips$N_VIAG)),]
trips<-subset(trips, select=c("ID_DOM", "ID_PESS", "female", "IDADE",  "N_VIAG","DISTANCIA", "DURACAO", "mode", "FE_VIA" , "purpose"))
colnames(trips)<-c("hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_duration","trip_time", "trip_main_mode", "weight_trip", "trip_purpose")


#####Brazil Belo Horizonte######
setwd('J:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Brazil/Belo Horizonte/Travel survey')
trip<-read.table('dbo_TB_VIAGENS_INTERNAS_RMBH.txt', header = TRUE, sep=",")
person<- read.table('dbo_TB_DOMICILIO_PESSOA_ENTREGA.txt', header = TRUE, sep=",")
#hh_weights<- read.table('dbo_TB_FATOR_EXPANSÂO_DOMICÍLIO.txt', header = TRUE, sep=",")

###there are no stages, therefore, walking to and from public transport is not included
### we have a process coded in the ITHIM-R which adds this walking in such cases (Ali does it once we provide him the data)

##create unique person id
person$participant_id<- paste0(person$ID_DOMICILIO, "_", person$ID_PESSOA)
##selecting the variables needed for individuals (personid, sex, age)
person<- subset(person, select=c("participant_id","DS_SEXO","IDADE"))
person$sex<-"Male"
person$sex[which(person$Sexo=="Feminino")]<-"Female"
#removing the sexo variables
person<- person[,-2]

##create trip and person id
trip$trip_id<- paste0(trip$Domicilio, "_", trip$Pessoa, "_", trip$Identificação)
trip$participant_id<- paste0(trip$Domicilio, "_", trip$Pessoa)

##calculate trip duration
trip$trip_duration <- NA
trip$trip_duration <- (as.numeric(substr(trip$TEMPO.DE.DESLOCAMENTO, 12,13)))*60 + as.numeric(substr(trip$TEMPO.DE.DESLOCAMENTO, 15,16))

##allocating mode names in english
lookup<- as.data.frame((unique(trip$DS_SH_MEIO_TRANSPORTE)))
lookup<- cbind(lookup, c("bus", "car", "walk", "metro", "bus", "car", "motorcycle", "other", "truck", "bicycle", "taxi"))
names(lookup)<- c("DS_SH_MEIO_TRANSPORTE", "trip_mode")
trip <- trip %>% left_join(lookup, by="DS_SH_MEIO_TRANSPORTE")

##renaming the sex
trip$sex<-"Male"
trip$sex[which(trip$Sexo=="Feminino")]<-"Female"


trip %>% group_by(trip_mode) %>% summarise(n())

###checking for data quality
##trip rate
nrow(trip)/nrow(person)
##trip rate for older than 15 (it goes up from 1.39 to 1.42)
nrow(trip[which(trip$Idade>15),])/nrow(person[which(person$IDADE>15),])
##duration per capita (seems lower value than expected)
sum(trip$trip_duration)/nrow(person)
##average trip duration per mode
trip %>% group_by(trip_mode) %>% summarise(mean(trip_duration))

##number of unique people who made a trip
length(unique(trip$participant_id))
##dividing this by the total number of people-- 59% of total sample made a trip, which brings down the average trip rate per capita
length(unique(trip$participant_id))/nrow(person)

##selecting the variables needed for trips (this one unusually has sex and age (idade) included in the trips file)
trip<- subset(trip, select=c("participant_id", "trip_id","trip_duration","trip_mode", "Idade", "sex"))

##it seems that persons file has no weight variable, in which case we may have to get rid of the weight variable in the trip file also (Fator.expansão)
##selecting persons which did not make any trip, to add to the trip file
person<-person[which(!(person$participant_id %in% unique(trip$participant_id))),]
#renaming age variable same as trip
names(person)[2]<-"Idade"

##adding all the other columns

person$trip_id<- NA
person$trip_duration<- NA
person$trip_mode<- NA
#person$Idade<- NA

##settings the sequence same as trips
person<- subset(person, select=c("participant_id", "trip_id","trip_duration","trip_mode", "Idade", "sex"))

trip<- rbind(trip, person)
trip <- trip %>% rename(age = Idade)

quality_check(trip)

write.csv(trip,'V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Brazil/Belo Horizonte/ITHIM R data/trips.csv')


#####Brazil Salvador######
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Brazil/Salvador')
trips<-read.csv('trips.csv')
persons<- read.csv('individual.csv')
hh<- read.csv('household.csv')
lookup<-read.csv('mode_names_lookup.csv')

persons$id_person<- paste0(persons$IDENTIFICADOR, "_", persons$SEQ)
persons<- subset(persons, select=c("id_person", "SEXO", "IDADE"))

trips$id_person<- paste0(trips$IDENTIFICADOR, "_", trips$SEQ)
trips$id_trip<- paste0(trips$id_person, "_", trips$VIAGEM)

trips$trip_duration <- ((as.numeric(substr(trips$HORA_D, 1,2)))*60 + as.numeric(substr(trips$HORA_D, 4,5)))-( (as.numeric(substr(trips$HORA_O, 1,2)))*60 + as.numeric(substr(trips$HORA_O, 4,5)))
#trips$purpose<- 0
#trips$purpose[which(trips$MOTIVO_D<4)]<-1
#trips$purpose[which(trips$MOTIVO_O<4 & trips$MOTIVO_D==8) ]<-1

trips <- trips %>% left_join(persons, by="id_person")
trips$female<-0
trips$female[which(trips$SEXO==2)]<-1
trips$country<- "Brazil"
trips$city<- "Salvador"
trips$urban<-1
trips$year<-2012
trips$age_cat<- 'NA'
trips$distance<- NA
##joining the lookup table for mode names
trips <- trips %>% left_join(lookup, by="MODO_TRANSP")

##checking results
sum(trips$trip_duration)/nrow(persons) ## has 64 minutes as average time per capita, very high
##average trip duration per mode
x<- trips  %>% group_by(mode_eng) %>% summarise(mean (trip_duration, na.rm=T))



#####Chile Antofagasta#####
###added duration in the raw excel file as the column 'duration'
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Chile/Travel Surveys/Antofagasta')
person<-read.csv('Persona.csv')
trips<- read.csv('Viaje.csv')
age<-read.csv('Edad de personas.csv')
str(person)
###making a unique ID for each individual
person$IDPersona2<-'NA'
person$IDPersona2[which(person$IDPersona<10)]<- paste(person$IDFolio[which(person$IDPersona<10)],'0',person$IDPersona[which(person$IDPersona<10)], sep = "") 
person$IDPersona2[which(person$IDPersona>9)]<- paste(person$IDFolio[which(person$IDPersona>9)],person$IDPersona[which(person$IDPersona>9)], sep = "")  

trips$IDPersona<-'NA'
trips$IDPersona2[which(trips$IdPersona<10)]<- paste(trips$IDFolio[which(trips$IdPersona<10)],'0',trips$IdPersona[which(trips$IdPersona<10)], sep = "") 
trips$IDPersona2[which(trips$IdPersona>9)]<- paste(trips$IDFolio[which(trips$IdPersona>9)],trips$IdPersona[which(trips$IdPersona>9)], sep = "") 

age$IDPersona2<-'NA'
age$IDPersona2[which(age$IDPERSONA<10)]<- paste(age$IDFolio[which(age$IDPERSONA<10)],'0',age$IDPERSONA[which(age$IDPERSONA<10)], sep = "") 
age$IDPersona2[which(age$IDPERSONA>9)]<- paste(age$IDFolio[which(age$IDPERSONA>9)],age$IDPERSONA[which(age$IDPERSONA>9)], sep = "")  

trips<-trips %>% left_join(person, by="IDPersona2")
trips<-trips %>% left_join(age, by="IDPersona2")
str(trips)

trips$urban<-1
trips$country<-"Chile"
trips$geog<- "Antofagasta"
trips$age_cat<- 'NA'
trips$gender[trips$IDSexo==2]<-1
trips$gender[trips$IDSexo==1]<-0
trips$distance<-NA
trips$purpose<-0
trips$purpose[trips$PropositoEstraus==0]<-1  ###commuting as 1
trips$mode<-0
trips$mode[trips$Modo_Desagregado==8]<-1  ###commuting as 1
trips$weights<- trips$Factor
trips$IDPersona2<-as.numeric(trips$IDPersona2)
trips$year<-2010

trips<-subset(trips, select=c("country", "geog", "urban","IDFolio", "IDPersona2", "gender", "Edad",  "IdViaje","distance", "duration", "purpose", "mode", "age_cat", "weights", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips","age")



#####Chile Santiago#####
###added duration in the raw excel file(Viaje) as the column 'duration'
setwd("J:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Chile/Travel Surveys/Santiago")

person <- read.csv("Persona.csv")
trip <- read.csv("Viaje.csv")
stage <- read.csv("stages.csv")
age<-read.csv('EdadPersonas.csv')
lookup_person_sex <- read.csv("lookup_person_sex.csv")
lookup_trip_purpose <- read.csv("lookup_trip_purpose.csv")
lookup_trip_mode <- read.csv("lookup_trip_mode.csv")
lookup_stage_mode <- read.csv("lookup_stage_mode.csv")

#add age and sex to person
person <- person %>% select(Hogar, Persona, Sexo) %>% left_join(age) %>% left_join(lookup_person_sex) %>% {.[,-3]}

#extract relevant variable in tripset
trip <- trip %>% select(Hogar, Persona, Viaje, PropositoAgregado,TiempoViaje , ModoAgregado) %>% left_join(lookup_trip_mode) %>% left_join(lookup_trip_purpose)%>% {.[,-c(4,6,8)]}

#extract relevant variable in stage
stage <- stage %>% select(Hogar, Persona, Viaje, Etapa, Modo) %>% left_join(lookup_stage_mode) %>% {.[,-c(5,7)]}

#add trip and stages to person
trip <- person %>% left_join(trip) %>% left_join(stage) %>% select(Persona, Edad, sex, Viaje, Etapa, stage_mode, trip_mode, TiempoViaje, trip_purpose)
names(trip) <- c("participant_id", "age", "sex", "trip_id", "stage_id", "stage_mode", "trip_mode", "trip_duration", "trip_purpose")

quality_check(trip)

#write.csv(trip, "trips_santiago.csv")



#####Chile Arica####
###added duration in the raw excel file (Viaje) as the column 'duration'
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Chile/Travel Surveys/Arica')
person<-read.csv('Persona.csv')
trips<- read.csv('Viaje.csv')
age<-read.csv('Edad de personas.csv')

person$IDPersona2<-'NA'
person$IDPersona2[which(person$IDPersona<10)]<- paste(person$IDFolio[which(person$IDPersona<10)],'0',person$IDPersona[which(person$IDPersona<10)], sep = "") 
person$IDPersona2[which(person$IDPersona>9)]<- paste(person$IDFolio[which(person$IDPersona>9)],person$IDPersona[which(person$IDPersona>9)], sep = "")  

trips$IDPersona2<-'NA'
trips$IDPersona2[which(trips$IDPersona<10)]<- paste(trips$IDFolio[which(trips$IDPersona<10)],'0',trips$IDPersona[which(trips$IDPersona<10)], sep = "") 
trips$IDPersona2[which(trips$IDPersona>9)]<- paste(trips$IDFolio[which(trips$IDPersona>9)],trips$IDPersona[which(trips$IDPersona>9)], sep = "") 

age$IDPersona2<-'NA'
age$IDPersona2[which(age$IDPersona<10)]<- paste(age$IDFolio[which(age$IDPersona<10)],'0',age$IDPersona[which(age$IDPersona<10)], sep = "") 
age$IDPersona2[which(age$IDPersona>9)]<- paste(age$IDFolio[which(age$IDPersona>9)],age$IDPersona[which(age$IDPersona>9)], sep = "")  

person<-person %>% left_join(age, by="IDPersona2")

person<-subset(person, select=c("IDFolio.x", "IDPersona2", "IDSexo","Edad"))

trips<-trips %>% left_join(person, by="IDPersona2")

trips$urban<-1
trips$country<-"Chile"
trips$geog<-"Arica"
trips$purpose<-0
trips$purpose[trips$PropositoEstraus==0]<-1  ###commuting as 1
trips$mode<-0
trips$mode[trips$IDModo ==8]<-1  ###commuting as 1
trips$gender[trips$IDSexo==2]<-1
trips$gender[trips$IDSexo==1]<-0
trips$age_cat<-'NA'
trips$distance<-NA
trips$year<-2010

trips<-subset(trips, select=c("country", "geog", "urban","IDFolio", "IDPersona2", "gender", "Edad",  "IDviaje","distance", "duration", "purpose", "mode", "age_cat", "Factor", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips","age")

#####Chile Copiapo####
###added duration in the raw excel file (Viaje) as the column 'duration'
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Chile/Travel Surveys/Copiapo')
person<-read.csv('Persona.csv')
trips<- read.csv('Viaje.csv')
age<-read.csv('Edad de personas.csv')

person$IDPersona2<-'NA'
person$IDPersona2[which(person$IDPersona<10)]<- paste(person$IDFolio[which(person$IDPersona<10)],'0',person$IDPersona[which(person$IDPersona<10)], sep = "") 
person$IDPersona2[which(person$IDPersona>9)]<- paste(person$IDFolio[which(person$IDPersona>9)],person$IDPersona[which(person$IDPersona>9)], sep = "")  

trips$IDPersona2<-'NA'
trips$IDPersona2[which(trips$IdPersona<10)]<- paste(trips$IDFolio[which(trips$IdPersona<10)],'0',trips$IdPersona[which(trips$IdPersona<10)], sep = "") 
trips$IDPersona2[which(trips$IdPersona>9)]<- paste(trips$IDFolio[which(trips$IdPersona>9)],trips$IdPersona[which(trips$IdPersona>9)], sep = "") 

age$IDPersona2<-'NA'
age$IDPersona2[which(age$IDPersona<10)]<- paste(age$IDFolio[which(age$IDPersona<10)],'0',age$IDPersona[which(age$IDPersona<10)], sep = "") 
age$IDPersona2[which(age$IDPersona>9)]<- paste(age$IDFolio[which(age$IDPersona>9)],age$IDPersona[which(age$IDPersona>9)], sep = "")  


person<-person %>% left_join(age, by="IDPersona2")

person<-subset(person, select=c("IDFolio.x", "IDPersona2", "IDSexo","Edad"))

trips<-trips %>% left_join(person, by="IDPersona2")

trips$urban<-1
trips$country<-"Chile"
trips$geog<-"Copiapo"
trips$purpose<-0
trips$purpose[trips$PropositoEstraus==0]<-1  ###commuting as 1
trips$mode<-0
trips$mode[trips$IDModo ==8]<-1  ###commuting as 1
trips$gender[trips$IDSexo==2]<-1
trips$gender[trips$IDSexo==1]<-0
trips$age_cat<-'NA'
trips$distance<-NA
trips$year<-2010

trips<-subset(trips, select=c("country", "geog", "urban","IDFolio", "IDPersona2", "gender", "Edad",  "IdViaje","distance", "duration", "purpose", "mode", "age_cat", "Factor", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips","age")


#####Chile Coquimbo-La Serena####
###added duration in the raw excel file (Viaje) as the column 'duration'
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Chile/Travel Surveys/Coquimbo-La Serena')
person<-read.csv('Persona.csv')
trips<- read.csv('Viaje.csv')
age<-read.csv('Edad de personas.csv')

person$IDPersona2<-'NA'
person$IDPersona2[which(person$IDPersona<10)]<- paste(person$IDFolio[which(person$IDPersona<10)],'0',person$IDPersona[which(person$IDPersona<10)], sep = "") 
person$IDPersona2[which(person$IDPersona>9)]<- paste(person$IDFolio[which(person$IDPersona>9)],person$IDPersona[which(person$IDPersona>9)], sep = "")  

trips$IDPersona2<-'NA'
trips$IDPersona2[which(trips$IDPersona<10)]<- paste(trips$IDFolio[which(trips$IDPersona<10)],'0',trips$IDPersona[which(trips$IDPersona<10)], sep = "") 
trips$IDPersona2[which(trips$IDPersona>9)]<- paste(trips$IDFolio[which(trips$IDPersona>9)],trips$IDPersona[which(trips$IDPersona>9)], sep = "") 

age$IDPersona2<-'NA'
age$IDPersona2[which(age$IDPersona<10)]<- paste(age$IDFolio[which(age$IDPersona<10)],'0',age$IDPersona[which(age$IDPersona<10)], sep = "") 
age$IDPersona2[which(age$IDPersona>9)]<- paste(age$IDFolio[which(age$IDPersona>9)],age$IDPersona[which(age$IDPersona>9)], sep = "")  

person<-person %>% left_join(age, by="IDPersona2")

person<-subset(person, select=c("IDFolio.x", "IDPersona2", "IDSexo","Edad", "Factor"))

trips<-trips %>% left_join(person, by="IDPersona2")

trips$urban<-1
trips$country<-"Chile"
trips$geog<-"Coquimbo-LaSerena"
trips$purpose<-0
trips$purpose[trips$PropositoEstraus==0]<-1  ###commuting as 1
trips$mode<-0
trips$mode[trips$IDModo ==8]<-1  ###commuting as 1
trips$gender[trips$IDSexo==2]<-1
trips$gender[trips$IDSexo==1]<-0
trips$age_cat<-'NA'
trips$distance<-NA
trips$year<-2010

trips<-subset(trips, select=c("country", "geog", "urban","IDFolio", "IDPersona2", "gender", "Edad",  "IDViaje","distance", "duration", "purpose", "mode", "age_cat", "Factor", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips","age")

#####Chile Gran Valparaiso#### DOES NOT HAVE AGE!######
###added duration in the raw excel file (Viaje) as the column 'duration'
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Chile/Travel Surveys/Gran Valparaiso')
person<-read.csv('Persona.csv')
trips<- read.csv('Viaje.csv')
str(person)
str(trips)


person$IDPersona2<-'NA'
person$IDPersona2[which(person$Persona<10)]<- paste(person$Hogar[which(person$Persona<10)],'0',person$Persona[which(person$Persona<10)], sep = "") 
person$IDPersona2[which(person$Persona>9)]<- paste(person$Hogar[which(person$Persona>9)],person$Persona[which(person$Persona>9)], sep = "")  

trips$IDPersona2<-'NA'
trips$IDPersona2[which(trips$Persona<10)]<- paste(trips$Hogar[which(trips$Persona<10)],'0',trips$Persona[which(trips$Persona<10)], sep = "") 
trips$IDPersona2[which(trips$Persona>9)]<- paste(trips$Hogar[which(trips$Persona>9)],trips$Persona[which(trips$Persona>9)], sep = "") 

person<-subset(person, select=c("Hogar", "IDPersona2", "Sexo", "Factor"))

trips<-trips %>% left_join(person, by="IDPersona2")

trips$urban<-1
trips$country<-"Chile"
trips$geog<-"Gran Valparaiso"
trips$purpose<-0
trips$purpose[trips$PropositoEstraus==1]<-1  ###commuting as 1
trips$mode<-0
trips$mode[trips$IDModo ==7]<-1  ###commuting as 1
trips$gender[trips$Sexo==2]<-1
trips$gender[trips$Sexo==1]<-0
trips$age_cat<-'NA'
trips$distance<-NA
trips$Edad<-NA
trips$year<-2014

trips<-subset(trips, select=c("country", "geog", "urban","Hogar.x", "IDPersona2", "gender", "Edad",  "Viaje","distance", "duration", "purpose", "mode", "age_cat", "Factor", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips")

#####Chile Iquique####
###added duration in the raw excel file (Viaje) as the column 'duration'
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Chile/Travel Surveys/Iquique')
person<-read.csv('Persona.csv')
trips<- read.csv('Viaje.csv')
age<-read.csv('Edad de personas.csv')

person<-person %>% left_join(age, by="IDPersona")
str(person)
person<-subset(person, select=c("IDFolio.x", "IDPersona", "IDSexo","Edad", "Factor"))
trips<-trips %>% left_join(person, by="IDPersona")
str(trips)

trips$urban<-1
trips$country<-"Chile"
trips$geog<-"Iquique"
trips$purpose<-0
trips$purpose[trips$PropositoEstraus==0]<-1  ###commuting as 1
trips$mode<-0
trips$mode[trips$IDModo ==8]<-1  ###cycling as 1
trips$gender[trips$IDSexo==2]<-1
trips$gender[trips$IDSexo==1]<-0
trips$age_cat<-'NA'
trips$distance<-NA
trips$year<-2010

trips<-subset(trips, select=c("country", "geog", "urban","IDFolio", "IDPersona", "gender", "Edad",  "IDViaje","distance", "duration", "purpose", "mode", "age_cat", "Factor.x", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips","age")


#####Chile Puerto Montt####
###added duration in the raw excel file (Viaje) as the column 'duration'
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Chile/Travel Surveys/Puerto Montt')
person<-read.csv('Persona.csv')
trips<- read.csv('Viaje.csv')
age<-read.csv('Edad de personas.csv')

person$IDPersona2<-'NA'
person$IDPersona2[which(person$IDPersona<10)]<- paste(person$IDFolio[which(person$IDPersona<10)],'0',person$IDPersona[which(person$IDPersona<10)], sep = "") 
person$IDPersona2[which(person$IDPersona>9)]<- paste(person$IDFolio[which(person$IDPersona>9)],person$IDPersona[which(person$IDPersona>9)], sep = "")  

trips$IDPersona2<-'NA'
trips$IDPersona2[which(trips$IdPersona<10)]<- paste(trips$IDFolio[which(trips$IdPersona<10)],'0',trips$IdPersona[which(trips$IdPersona<10)], sep = "") 
trips$IDPersona2[which(trips$IdPersona>9)]<- paste(trips$IDFolio[which(trips$IdPersona>9)],trips$IdPersona[which(trips$IdPersona>9)], sep = "") 

age$IDPersona2<-'NA'
age$IDPersona2[which(age$IDPERSONA<10)]<- paste(age$IDFolio[which(age$IDPERSONA<10)],'0',age$IDPERSONA[which(age$IDPERSONA<10)], sep = "") 
age$IDPersona2[which(age$IDPERSONA>9)]<- paste(age$IDFolio[which(age$IDPERSONA>9)],age$IDPERSONA[which(age$IDPERSONA>9)], sep = "")  

person<-person %>% left_join(age, by="IDPersona2")

person<-subset(person, select=c("IDFolio.x", "IDPersona2", "IDSexo","Edad"))

trips<-trips %>% left_join(person, by="IDPersona2")

trips$urban<-1
trips$country<-"Chile"
trips$geog<-"Puerto Montt"
trips$purpose<-0
trips$purpose[trips$PropositoEstraus==1]<-1  ###commuting as 1
trips$mode<-0
trips$mode[trips$IDModo ==8]<-1  ###commuting as 1
trips$gender[trips$IDSexo==2]<-1
trips$gender[trips$IDSexo==1]<-0
trips$age_cat<-'NA'
trips$distance<-NA
trips$year<-2014

trips<-subset(trips, select=c("country", "geog", "urban","IDFolio", "IDPersona2", "gender", "Edad",  "IdViaje","distance", "duration", "purpose", "mode", "age_cat", "Factor", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips","age")


#####Chile Temuco-Padre Lascasas####
###added duration in the raw excel file (Viaje) as the column 'duration'
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Chile/Travel Surveys/Temuco-Padre Lascasas')
person<-read.csv('Persona.csv')
trips<- read.csv('Viaje.csv')
age<-read.csv('Edad de personas.csv')

person$IDPersona2<-'NA'
person$IDPersona2[which(person$IDPersona<10)]<- paste(person$IDFolio[which(person$IDPersona<10)],'0',person$IDPersona[which(person$IDPersona<10)], sep = "") 
person$IDPersona2[which(person$IDPersona>9)]<- paste(person$IDFolio[which(person$IDPersona>9)],person$IDPersona[which(person$IDPersona>9)], sep = "")  

trips$IDPersona2<-'NA'
trips$IDPersona2[which(trips$IdPersona<10)]<- paste(trips$IDFolio[which(trips$IdPersona<10)],'0',trips$IdPersona[which(trips$IdPersona<10)], sep = "") 
trips$IDPersona2[which(trips$IdPersona>9)]<- paste(trips$IDFolio[which(trips$IdPersona>9)],trips$IdPersona[which(trips$IdPersona>9)], sep = "") 

age$IDPersona2<-'NA'
age$IDPersona2[which(age$IDPERSONA<10)]<- paste(age$IDFolio[which(age$IDPERSONA<10)],'0',age$IDPERSONA[which(age$IDPERSONA<10)], sep = "") 
age$IDPersona2[which(age$IDPERSONA>9)]<- paste(age$IDFolio[which(age$IDPERSONA>9)],age$IDPERSONA[which(age$IDPERSONA>9)], sep = "")  

person<-person %>% left_join(age, by="IDPersona2")

person<-subset(person, select=c("IDFolio.x", "IDPersona2", "IDSexo","Edad"))

trips<-trips %>% left_join(person, by="IDPersona2")

trips$urban<-1
trips$country<-"Chile"
trips$geog<-"Temuco-Padre Lascasas"
trips$purpose<-0
trips$purpose[trips$PropositoEstraus==0]<-1  ###commuting as 1
trips$mode<-0
trips$mode[trips$IDModo ==8]<-1  ###commuting as 1
trips$gender[trips$IDSexo==2]<-1
trips$gender[trips$IDSexo==1]<-0
trips$age_cat<-'NA'
trips$distance<-NA
trips$year<-2013

trips<-subset(trips, select=c("country", "geog", "urban","IDFolio", "IDPersona2", "gender", "Edad",  "IdViaje","distance", "duration", "purpose", "mode", "age_cat", "Factor", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips","age")


#####Chile Valdivia####
###added duration in the raw excel file (Viaje) as the column 'duration'
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Chile/Travel Surveys/Valdivia')
person<-read.csv('Persona.csv')
trips<- read.csv('Viaje.csv')
age<-read.csv('Edad de personas.csv')

person$IDPersona2<-'NA'
person$IDPersona2[which(person$IDPersona<10)]<- paste(person$IDFolio[which(person$IDPersona<10)],'0',person$IDPersona[which(person$IDPersona<10)], sep = "") 
person$IDPersona2[which(person$IDPersona>9)]<- paste(person$IDFolio[which(person$IDPersona>9)],person$IDPersona[which(person$IDPersona>9)], sep = "")  

trips$IDPersona2<-'NA'
trips$IDPersona2[which(trips$IdPersona<10)]<- paste(trips$IDFolio[which(trips$IdPersona<10)],'0',trips$IdPersona[which(trips$IdPersona<10)], sep = "") 
trips$IDPersona2[which(trips$IdPersona>9)]<- paste(trips$IDFolio[which(trips$IdPersona>9)],trips$IdPersona[which(trips$IdPersona>9)], sep = "") 

age$IDPersona2<-'NA'
age$IDPersona2[which(age$IDPERSONA<10)]<- paste(age$IDFolio[which(age$IDPERSONA<10)],'0',age$IDPERSONA[which(age$IDPERSONA<10)], sep = "") 
age$IDPersona2[which(age$IDPERSONA>9)]<- paste(age$IDFolio[which(age$IDPERSONA>9)],age$IDPERSONA[which(age$IDPERSONA>9)], sep = "")  

person<-person %>% left_join(age, by="IDPersona2")

person<-subset(person, select=c("IDFolio.x", "IDPersona2", "IDSexo","Edad"))

trips<-trips %>% left_join(person, by="IDPersona2")

trips$urban<-1
trips$country<-"Chile"
trips$geog<-"Valdivia"
trips$purpose<-0
trips$purpose[trips$PropositoEstraus==0]<-1  ###commuting as 1
trips$mode<-0
trips$mode[trips$IDModo ==8]<-1  ###commuting as 1
trips$gender[trips$IDSexo==2]<-1
trips$gender[trips$IDSexo==1]<-0
trips$age_cat<-'NA'
trips$distance<-NA
trips$year<-2013

trips<-subset(trips, select=c("country", "geog", "urban","IDFolio", "IDPersona2", "gender", "Edad",  "IdViaje","distance", "duration", "purpose", "mode", "age_cat", "Factor", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("person","trips","age")

#####Colombia Bogota Lambed----

setwd('J:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Colombia/Bogota/Travel')

#data
person_0 <- read_excel("encuesta 2015 - personas.xlsx",sheet = 1, range = cell_cols("A:E"))
trip_0 <- read_excel("encuesta 2015 - viajes.xlsx",sheet = 1, range = cell_cols("A:K"))
stage_0 <- read_excel("encuesta 2015 - etapas.xlsx",sheet = 1, range = cell_cols("A:K"))

#lookup tables
lookup_trip_mode <- read_excel("lookup_trip_mode.xlsx",sheet = 1, range = cell_cols("A:B"))
lookup_stage_mode <- read_excel("lookup_stage_mode.xlsx",sheet = 1, range = cell_cols("A:B"))
lookup_trip_purpose <-  read_excel("lookup_trip_purpose.xlsx",sheet = 1, range = cell_cols("A:B"))
lookup_sex <- data.frame(SEXO = c("Hombre", "Mujer"), sex=c("Male", "Female"))


#select relevant varaibles
person <- person_0 %>% mutate(participant_id = paste(ID_ENCUESTA, NUMERO_PERSONA, sep = "_")) %>% select(participant_id, SEXO, EDAD)
trip <- trip_0 %>% mutate(participant_id = paste(ID_ENCUESTA, NUMERO_PERSONA, sep = "_"), trip_id = paste(participant_id, NUMERO_VIAJE, sep ="_")) %>% 
    left_join(lookup_trip_mode) %>% select(participant_id, trip_id, MOTIVOVIAJE, DURATION, trip_mode)
stage <- stage_0 %>% mutate(participant_id = paste(ID_ENCUESTA, NUMERO_PERSONA, sep = "_"), 
                            trip_id = paste(participant_id, NUMERO_VIAJE, sep ="_"), 
                            stage_id = paste(trip_id, NUMERO_ETAPA, sep ="_")) %>% 
    left_join(lookup_stage_mode) %>% select(participant_id, trip_id, stage_id, stage_mode)

#calcualte average mode time for unimodal trips and later use the ratio to calculate stage duration except for walk to public transport
stage <- stage %>% group_by(participant_id, trip_id) %>% summarise(c = n()) %>% filter(c == 1) %>% left_join(trip) %>% group_by(trip_mode) %>% 
    summarise(average_mode_time = mean(DURATION, na.rm = T)) %>% rename(stage_mode = trip_mode) %>% right_join(stage)

stage <- stage %>% filter(is.na(average_mode_time)) %>% left_join(trip) %>% group_by(trip_mode) %>% summarise(average_mode_time = mean(DURATION, na.rm = T)) %>% rename(stage_mode = trip_mode) %>% right_join(stage)

View()

stage <- stage %>% group_by(participant_id, trip_id) %>% 
    mutate(average_mode_time = ifelse("walk" %in% levels(as.factor(stage_mode)) & length(levels(as.factor(stage_mode)))> 1 & stage_mode == "walk", 1, 
                                      ifelse("walk" %in% levels(as.factor(stage_mode)) & length(levels(as.factor(stage_mode)))> 1 & stage_mode != "walk", 6,
                                             average_mode_time))) %>% #add walking to public transport ratio of 1:6
    left_join(group_by(.,participant_id, trip_id) %>% summarise(sum_average = sum(average_mode_time , na.rm = T)))
    

trip <- person %>% left_join(trip) %>% left_join(stage) %>% left_join(lookup_sex) %>% left_join(lookup_trip_purpose) %>% 
    mutate(stage_duration = round(DURATION*average_mode_time/sum_average))

trip <- trip %>% mutate(age= EDAD, trip_duration = DURATION) %>% 
    select(participant_id, sex, age, trip_id, trip_purpose, trip_mode, trip_duration, stage_id, stage_duration, stage_mode)

#replace mode = special with appropriate modes (car and bus)
trip <- trip %>% select(-trip_mode) %>% group_by(participant_id, trip_id) %>% summarise(trip_mode = stage_mode[which.is.max(stage_duration)]) %>% right_join(select(trip, - trip_mode))


quality_check(trip)
#write.csv(trip, "J:/Group/lambed/ITHIM-R/data/local/bogota/trip_bogota.csv")


#####Colombia Bogota (Use this one)####
##In the previous version, stages were used, but there is no benefit as the stages for public transport hardly reports walking
###added duration in the raw excel file (Viaje) as the column 'duration'-- calculated from the clock time of start and end
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Colombia/Bogota/Travel')
trips<- read.csv('encuesta 2015 - viajes.csv')
lookup_mode_name<-read.csv('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Colombia/Bogota/Travel/spanish_english_modes.csv')
##to map the mode names in trip file to that of stage file; both files have different names
#lookup<- read.csv('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Colombia/Bogota/Travel/lookup_mediotransporte_main_mode.csv')

trips<- subset(trips, select=c('ID_ENCUESTA',  'NUMERO_PERSONA', 'NUMERO_VIAJE','MEDIO_PREDOMINANTE','DURATION','FACTOR_AJUSTE'))
trips$trip_id<- NA
trips$trip_id<- paste0(trips$ID_ENCUESTA, trips$NUMERO_PERSONA, trips$NUMERO_VIAJE)
trips$person_id<- NA
trips$person_id<- paste0(trips$ID_ENCUESTA, trips$NUMERO_PERSONA)

trips<- subset(trips, select=c('trip_id', 'MEDIO_PREDOMINANTE', 'DURATION','person_id'))


##adding the persons who made no trips
person<-read.csv('encuesta 2015 - personas.csv')
person$person_id<- NA
person$person_id<- paste0(person$ID_ENCUESTA, person$NUMERO_PERSONA)
person<- subset(person, select=c('person_id','SEXO', 'EDAD', 'FE_TOTAL'))
person$FE_TOTAL<-as.character(person$FE_TOTAL)
for (i in 1:nrow(person))
{
    person$weights[i]<- as.numeric(strsplit(as.character(person$FE_TOTAL[i]), ',')[[1]][1])
}

trips<- trips %>% left_join(person, by="person_id")

trips<- trips[,-7]

## added these lines to get the mode share
#trips <- trips  %>% left_join(lookup_mode_name, by="MEDIO_PREDOMINANTE")
#trips %>% group_by(main_mode_eng) %>% summarise(sum(weights))

trips_temp<- trips[which(!duplicated(trips$person_id)),]
trips_temp$include<-1

trips_temp<- trips_temp[,-c(5,6,7)]



person<- person %>% left_join(trips_temp, by="person_id")

person<- person[which(is.na(person$include)),]

person<- subset(person, select=c("trip_id", "MEDIO_PREDOMINANTE", "DURATION","person_id", "SEXO", "EDAD", "weights" ))

bogota_data<- rbind(trips, person)
#names(bogota_data)[2]<-"Mode"
bogota_data<- bogota_data %>% left_join(lookup_mode_name, by="MEDIO_PREDOMINANTE")

bogota_data$sex<- "NA"
bogota_data$sex[which(bogota_data$SEXO=="Mujer")]<- "Female"
bogota_data$sex[which(bogota_data$SEXO=="Hombre")]<- "Male"
bogota_data<- bogota_data[,-5]
write.csv(bogota_data,'C:/ITHIM-R/data/local/bogota/bogota_trips.csv')

#####England LONDON#####
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/London/LTDS0514_Combined_V3U_v1.2')
trips<-read.table('Trip_ltds.txt', header = TRUE, sep=",")
stages<-read.table('Stage.txt', header=TRUE, sep=",")
persons<- read.table('Person.txt', header=TRUE, sep=",")
person_data<-read.table('Person_data.txt', header=TRUE, sep=",")
trips_subset<- subset(trips, select=c("ttid", "tdbmmode", "tlenn"))
stages_subset<- subset(stages, select=c("ssid", "stid", "smode", "swalkdur"))
names(stages_subset)[2]<-"ttid"
stages_subset <- stages_subset %>% left_join(trips_subset, by="ttid")


stages_subset$dist_cat[stages_subset$tlenn <=1 ] <-  "0 - 1 km"
stages_subset$dist_cat[stages_subset$tlenn >1 & stages_subset$tlenn <=5] <-  "2 - 5 km"
stages_subset$dist_cat[stages_subset$tlenn >5 & stages_subset$tlenn <=10] <-  "6 - 10 km"
stages_subset$dist_cat[stages_subset$tlenn >10 & stages_subset$tlenn <=20] <-  "11 - 20 km"
stages_subset$dist_cat[stages_subset$tlenn >20 & stages_subset$tlenn <=30] <-  "21 - 30 km"
stages_subset$dist_cat[stages_subset$tlenn >30 ] <-  "30+ km"


stages_bus<- stages_subset[which(stages_subset$swalkdur>0  & stages_subset$tdbmmode==13),]
stages_trains<- stages_subset[which(stages_subset$swalkdur>0  & stages_subset$tdbmmode==17),]
##trip wise total of walking duration

bus_walk<- stages_bus %>% group_by(ttid) %>% summarise(walk_dur=sum(swalkdur))
plot(density(bus_walk$walk_dur), xlim=c(0,30))

train_walk<- stages_trains %>% group_by(ttid) %>% summarise(walk_dur=sum(swalkdur))
plot(density(train_walk$walk_dur), xlim=c(0,30))

stages_2 %>% group_by(tdbmmode, dist_cat) %>% filter(swalkdur>0) %>% summarise(mean(swalkdur, na.rm=T), sd=sd(swalkdur, na.rm=T))


#####ENGLAND-- downloaded data (2002-2017)####
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/England/tab/')
trips<- read.delim('tripeul2017.tab', sep="\t")
ind<- read.delim('individualeul2017.tab', sep="\t")
str(trips)

trips_sel<-subset(trips, select=c('TripID', 'SurveyYear', 'IndividualID', 'HouseholdID', 'TripDisIncSW', 'TripTotalTime','MainMode_B04ID'))
ind_sel<- subset(ind, select=c('Age', 'Sex'))


#####ENGLAND###################
setwd('C:/Users/rg574/Dropbox/global cycling paper/Datasets')
eng_stg <- read.dta13("England/NTSEng_20022016_tigthat_stages.dta")
eng <- read.dta13("England/NTSEng_20022016_tigthat.dta")

eng_selected<-subset(eng, select=c("householdid","individualid", "home_gor","urban","tripid", "female", "age_rawcat", "trip_mainmode","trip_purpose","trip_dist_km", "trip_durationraw_min" ,"weight_trip","year"))

eng_selected$age_cat[eng_selected$age_rawcat == "Less than 1 year" | eng_selected$age_rawcat== "1 - 2 years" | eng_selected$age_rawcat== "3 - 4 years"| eng_selected$age_rawcat== "5 - 10 years"| eng_selected$age_rawcat== "11 - 15 years"] <-  "0 - 15 years"
eng_selected$age_cat[eng_selected$age_rawcat== "16 years" | eng_selected$age_rawcat== "17 years" | eng_selected$age_rawcat== "18 years"| eng_selected$age_rawcat== "19 years"| eng_selected$age_rawcat== "20 years"| eng_selected$age_rawcat== "21 - 25 years"| eng_selected$age_rawcat== "26 - 29 years"] <-  "16 - 29 years"
eng_selected$age_cat[eng_selected$age_rawcat== "30 - 39 years"] <-  "30 - 39 years"
eng_selected$age_cat[eng_selected$age_rawcat== "40 - 49 years"] <-  "40 - 49 years"
eng_selected$age_cat[eng_selected$age_rawcat== "50 - 59 years"] <-  "50 - 59 years"
eng_selected$age_cat[eng_selected$age_rawcat== "60 - 64 years" |eng_selected$age_rawcat== "65 - 69 years" |eng_selected$age_rawcat== "70 - 74 years"|eng_selected$age_rawcat== "75 - 79 years"|eng_selected$age_rawcat== "80 - 84 years"|eng_selected$age_rawcat== "85 years +"] <-  "60+ years"
eng_selected$age_cat<-as.factor(eng_selected$age_cat)

eng_selected$trip_main_mode<-0
eng_selected$trip_main_mode[which(as.character(eng_selected$trip_mainmode)=="Bicycle")]<-1
#eng_selected$trip_main_mode<-as.factor(eng_selected$trip_main_mode)

eng_selected$trip_purpose2<-0
eng_selected$trip_purpose2[which(as.character(eng_selected$trip_purpose)=="Commuting")]<-1

eng_selected$country<-"England"
eng_selected$age<-NA

trip_sel<- subset(eng_selected, select=c("country", "home_gor","urban","householdid","individualid","female", "age", "tripid", "trip_dist_km","trip_durationraw_min", "trip_purpose2", "trip_main_mode", "age_cat", "weight_trip", "year"))
colnames(trip_sel)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")

a<-trip_sel

rm("eng", "eng_stg", "eng_selected")
#####Finland####
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Finland/2016')
trips<-read.csv('M_MATKAT.csv')
background<- read.csv('T_TAUSTA.csv')
colnames(trips)[1]<-"T_TAUSTAID"

trips<-trips %>%  left_join(background, by="T_TAUSTAID")

trips$female<-0
trips$female[which(trips$T_SUKUPUOLI==2)]<-1

trips$urban<-NA  ##will need to obtain from the geography variable which gives the municipality
trips$country<-"Finland"
trips$mode<-0
trips$mode[trips$M_PAAKULKUTAPA ==2]<-1  ###cycling as 1
trips$purpose<-0
trips$purpose[which(trips$M_MR6==1)]<-1
trips$age_cat<-'NA'
trips$distance<-trips$NETWORK_DIST
trips$weights<-1  ## no weights in the dataset, however, T_RAPO/T_SEUTURAPO gives a code whether an individual was sampled from national sampling (1) or city-level oversampling (6= Helsinki, and so on)
trips$hh_id<-NA
trips$year<-2016

trips<-subset(trips, select=c("country", "M_LAHTO_KUNTANIMI", "urban","hh_id", "T_TAUSTAID", "female", "T_IKA",  "M_TRIPROUTESID","M_PITUUS", "M_KESTO", "purpose", "mode", "age_cat", "weights", "year"))
colnames(trips)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, trips)
rm("trips", "background")


#####Germany from Ralph####
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/USA 2017 and German 2008/Germany MOP')
german <- read.dta13("MOP 2014 2015 2016 Pooled for Rahul.dta")
#german <- read.dta13("MOP 2015 for Rahul.dta")
german$country<- "Germany"
german$age_cat<-'NA'
german$geog<-"NA"
german<-subset(german, select=c("country", "geog", "urban","hh_id", "ind_id", "gender", "age", "trip_id","distance", "time", "purpose", "mode", "age_cat", "weights", "cohort"))
colnames(german)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip", "year")
a<-rbind(a, german)
rm("german")



#####India Delhi#####
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/India/Delhi')
trips<- read.csv('trips_stages_delhi.csv')
head(trips)
trips$person_id<- NA
trips$person_id<- paste0(trips$Form.ID, trips$Member.No.)
head(trips)
person<- read.csv('persons.csv')
names(person)<- c('person_id', 'hh_id', 'hh_nr', 'person_nr', 'female', 'age', 'hh_weights')
person<- subset(person, select=c('person_id', 'female', 'age', 'hh_weights'))
person<- person[which(!duplicated(person$person_id)),]

trips<- trips %>% left_join(person, by='person_id')
names(trips)<- c("form_id", "form_id_new","colony_id", "person_nr","trip_nr", "trip_purpose", "stage", "mode", "distance" , "days_week", "duration", "trip_duration","trip_total_distance", "trip_id" ,
                 "priority", "main_mode", "main_mode_priority", "person_id",  "female", "age", "hh_weights")
trips_main_mode<- subset (trips,select= c('trip_id', 'main_mode'))
trips_main_mode<- trips_main_mode[which(!duplicated(trips_main_mode$trip_id)),]

trips<- subset(trips, select=c('person_id', 'female','age', 'trip_id', 'stage','mode', 'distance', 'duration', 'main_mode', 'hh_weights'))
#plot(density(trips$distance[which((trips$main_mode==8 |trips$main_mode==9 |trips$main_mode==10)&(trips$mode==1))], na.rm=TRUE))
#trips[which(trips$main_mode==8 |trips$main_mode==9 |trips$main_mode==10),]
#trips[which((trips$main_mode==8 |trips$main_mode==9 |trips$main_mode==10)&(trips$mode==1)&(trips$distance<3)),]

## walking distance and duration for walking to/from PT stops
walking_pt<-trips %>% filter((main_mode==8 | main_mode==9 | main_mode==10)&(mode==1)&(distance<3)) %>% group_by(trip_id) %>% summarise(sum(distance)) 
mean(walking_pt$`sum(distance)`)
## for buses only
walking_bus<-trips %>% filter((main_mode==8)&(mode==1)&(distance<3)) %>% group_by(trip_id) %>% summarise(walk_bus_dist=sum(distance)) 
walking_bus<- as.data.frame(walking_bus)
mean(walking_bus$walk_bus_dist)
##for metro and trains
walking_metro_train<-trips %>% filter((main_mode==9 | main_mode==10)&(mode==1)&(distance<3)) %>% group_by(trip_id) %>% summarise(walk_met_trn=sum(distance)) 
mean(walking_metro_train$walk_met_trn)


trips<- trips %>% left_join(walking_bus, by='trip_id')
trips<- trips %>% left_join(walking_metro_train, by='trip_id')
head(trips)

trip_id_no_pt<- trips$trip_id[which()]

#assigning average walking distance to those PT trips which do not have reported walking to PT distance
for (i in 1: nrow(trips))
{
    if (!is.na(trips$main_mode))
    {
        if (trips$main_mode[i]==8 & is.na(trips$walk_bus_dist[i]))
        {
            trips$walk_bus_dist[i]==mean(walking_bus$walk_bus_dist)
        }
        if ((trips$main_mode[i]==9 |trips$main_mode[i]==10) & is.na(trips$walk_met_trn[i]))
        {
            trips$walk_met_trn[i]==mean(walking_metro_train$walk_met_trn)
        }
    }
}

trips$walking_pt<-0
for (i in 1 : nrow(trips))
{
    if(!is.na(trips$walk_bus_dist[i]))
    {
        trips$walking_pt[i] <- trips$walk_bus_dist[i]
    }
    if(!is.na(trips$walk_met_trn[i]))
    {
        trips$walking_pt[i] <- trips$walk_met_trn[i]
    }
}

summary((trips$distance[which((trips$main_mode==8 |trips$main_mode==9 |trips$main_mode==10)&(trips$mode==1)&(trips$distance<3))]))

View(trips %>% filter(main_mode==8 | main_mode==9 | main_mode==10) %>% group_by(trip_id) %>% summarise(n()) )

trips_pt<- trips[which(trips$mode==trips$main_mode & trips$walking_pt>0),]
trips_pt$stage<-1
trips_pt_temp<-trips_pt[0,]

k<-1
for (i in 1: nrow(trips_pt))
{
    trips_pt_temp[k,]<- trips_pt[i,]
    k<- k+1
    trips_pt_temp[k,1]<-trips_pt[i,1]  ## person_id
    trips_pt_temp[k,2]<-trips_pt[i,2]  ## female
    trips_pt_temp[k,3]<-trips_pt[i,3]  ## age
    trips_pt_temp[k,4]<-trips_pt[i,4]  ## trip_id
    trips_pt_temp[k,5]<-2              ## stage
    trips_pt_temp[k,6]<-1              ## mode
    trips_pt_temp[k,7]<-trips_pt[i,13]             ## distance
    trips_pt_temp[k,8]<-trips_pt[i,13]/4.5  ## duration
    trips_pt_temp[k,9]<-trips_pt[i,9]  ## main_mode
    trips_pt_temp[k,10]<-trips_pt[i,10]  ## hh_weights
    trips_pt_temp[k,11]<-trips_pt[i,11]  ## walk_bus_dist
    trips_pt_temp[k,12]<-trips_pt[i,12]  ## walk_met_trn
    trips_pt_temp[k,13]<-trips_pt[i,13]  ## walking_pt
    k<- k+1
}
trips_others<- trips[which(trips$walking_pt==0),]

trips_new<-rbind(trips_pt_temp, trips_others)
trips_new<- trips_new[, -c(11,12,13)]

mode_name_lookup<-read.csv('mode_name_lookup.csv')
trips_new<- trips_new %>% left_join(mode_name_lookup, by='mode')

person<- read.csv('persons.csv')
names(person)<- c('person_id', 'hh_id', 'hh_nr', 'person_nr', 'female', 'age', 'hh_weights')
person<- subset(person, select=c('person_id','hh_id', 'female', 'age', 'hh_weights'))
person_subset<-subset(person, select=c('person_id','hh_id'))
trips_new<- trips_new %>% left_join(person_subset, by='person_id')
head(trips_new)
trips_new<- subset(trips_new, select=c('hh_id', 'person_id', 'female', 'age', 'trip_id', 'stage', 'mode', 'mode_name', 'distance', 'duration', 'hh_weights'))

## adding individuals who reported no trips
## adding the trips to the person file to select which households are in the trips database
person_trips<- person %>% left_join(trips_new, by ='hh_id')

## remove those where no trips were assigned to that household
person_trips<- person_trips[which(!is.na(person_trips$trip_id )),]

## removing those which are already included in the trips dataset
person_trips<- person_trips[which(person_trips$person_id.x !=person_trips$person_id.y),]

##removing the duplicates
person_trips<- person_trips[which(!duplicated(person_trips$person_id.x)),]

person_trips <- subset(person_trips, select=c('hh_id','person_id.x',  'female.x', 'age.x', 'trip_id', 'stage', 'mode','mode_name', 'distance', 'duration', 'hh_weights.x'))
names(person_trips)<- c('hh_id', 'person_id', 'female', 'age', 'trip_id', 'stage', 'mode', 'mode_name', 'distance', 'duration', 'hh_weights')
person_trips[,5:10]<- NA

trips_new<- rbind(trips_new, person_trips)

trips_new<- trips_new %>% left_join(trips_main_mode, by='trip_id')


saveRDS(person, 'households_delhi.RDS')
View(person)

saveRDS(trips_new, 'delhi_travel_survey_main_mode.RDS')
trips_new<-saveRDS(trips_new, 'delhi_travel_survey.RDS')
trips_new<-readRDS('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/India/Delhi/delhi_travel_survey_main_mode.RDS')
write.csv(trips_new, 'V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/India/Delhi/delhi_travel_survey_main_mode.csv')
##main mode lookup
main_mode_lookup<-read.csv('trips_main_mode_lookup.csv')
trips_new<- trips_new %>% left_join(main_mode_lookup, by= "trip_id")

###lookup table for mode name
mode_name_lookup<-read.csv('mode_name_lookup.csv')
names(mode_name_lookup)<- c("main_mode", "main_mode_name")
trips_new<- trips_new %>% left_join(mode_name_lookup, by='main_mode')

no_trips<-read.csv('no_trips_person.csv')

trips_new<- rbind(trips_new, no_trips)

delhi<- readRDS('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/India/Delhi/delhi_travel_survey.RDS')
head(delhi)


#####india- Bangalore (lambed checking after rob identified duplicates####
library(tidyverse)
library(readxl)
library(gsubfn)
library(nnet)

setwd("J:/Studies/MOVED/HealthImpact/Data/TIGTHAT/India/Bangalore")
data <- read_excel("COMPILED DATA final.xlsx",sheet = 1, range = cell_cols("A:AA"), col_types = c("text"))
bangalore <- data
bangalore <- bangalore[-1,-c(3, 4, 5, 11, 14,21,22,23)]# remove first row and other columns
bangalore <- rename_all(bangalore, funs(str_remove_all(.," "))) # modifying names of variables
names(bangalore)[c(6,7,8,9,10,15)]<- c("startinglanduse","startingaddress", "finishinglanduse", "finishingaddress","transfertime","distance")

#insert missing ward, hh, and person numbers

#ward number
for (i in 1: nrow(bangalore))
{
    print(i)
    value<- bangalore$WardNo.[i]
    repeat
    {
        i<- i+1
        if (!is.na(bangalore$WardNo.[i]))
        {
            break
        }
        bangalore$WardNo.[i] <- value
    }
}

#hh number
for (i in 1: nrow(bangalore))
{
    print(i)
    value<- bangalore$Householdserial.No[i]
    repeat
    {
        i<- i+1
        if (!is.na(bangalore$Householdserial.No[i]))
        {
            break
        }
        bangalore$Householdserial.No[i] <- value 
    }
}

# Peron number
for (i in 1: nrow(bangalore))
{
    print(i)
    value<- bangalore$No.ofPerson[i]
    repeat
    {
        i<- i+1
        if (bangalore$No.ofPerson[i]!="0")
        {
            break
        }
        bangalore$No.ofPerson[i] <- value 
    }
}

for (i in 1: nrow(bangalore))
{
    print(i)
    value<- bangalore$No.ofPerson[i]
    repeat
    {
        i<- i+1
        if (bangalore$No.ofPerson[i]!="-")
        {
            break
        }
        bangalore$No.ofPerson <- value 
    }
}
for (i in 1: nrow(bangalore))
{
    print(i)
    value<- bangalore$No.ofPerson[i]
    repeat
    {
        i<- i+1
        if (!is.na(bangalore$No.ofPerson[i]))
        {
            break
        }
        bangalore$No.ofPerson[i] <- value 
    }
}


data_1 <- bangalore
data_2 <- data_1

#compose trip_duration from transfertime, and diff btw starting and finishing time
bangalore$transfertime <- ifelse(grepl("E-",bangalore$transfertime),
                                 as.numeric(bangalore$transfertime)*1440, 
                                 bangalore$transfertime)
bangalore$transfertime <- gsub("([A-z]|:00|:|;|\\s|-|\\.$)","", bangalore$transfertime ) # remove non digits
#time difference between start and stop time
bangalore$time_diff = ifelse(as.numeric(bangalore$Startingtime)<1, 
                             (as.numeric(bangalore$FinishingTime) - as.numeric(bangalore$Startingtime))*1440, 
                             (as.numeric(bangalore$FinishingTime) - as.numeric(bangalore$Startingtime)))
#some 2's and 3's are minutes while some are hours, differentiate them
bangalore$transfertime <- ifelse((bangalore$transfertime == "2" | bangalore$transfertime == "3") & 
                                     (grepl("m|M", bangalore$transfertime) | (as.numeric(bangalore$time_diff <1) & !is.na(bangalore$time_diff))),
                                 as.numeric(bangalore$transfertime)/100, bangalore$transfertime)
#replace na, o and wired times in transfer time with time diff
bangalore$transfertime <- ifelse((bangalore$transfertime == 0 | is.na(bangalore$transfertime)) 
                                 & (bangalore$time_diff<180 & bangalore$time_diff>0), 
                                 bangalore$time_diff, bangalore$transfertime )
#calculate travel duration
bangalore$trip_duration <- ifelse(as.numeric(bangalore$transfertime) < 4,
                                  gsubfn("([0-3])(\\.*\\d*)", ~as.numeric(x)*60 + ifelse(is.na(as.numeric(y)),0,as.numeric(y)*100), bangalore$transfertime),
                                  bangalore$transfertime)
#Examining duplicated rows 
# duplicate <- bangalore[duplicated(bangalore),] # make a dataset of duplicated rows
# bangalore %>% distinct(WardNo.) %>% nrow # checking number of wards = 125
# duplicate %>% distinct(WardNo.) %>% nrow # checking wards with duplicates = 45
# write.table(table(factor(duplicate$WardNo.)), sep=",", quote = TRUE)
# overlap <- bangalore[duplicated(bangalore[,c('ID', 'Startingtime')]),]
# overlap %>% distinct(WardNo.) %>% nrow
# overlap_id <- overlap %>% distinct(ID)
# overlap_all <- left_join(overlap_id, bangalore, by ="ID") #all trips for individuals with overlapping trips
# overlap_all %>% filter(as.numeric(Startingtime)>1) %>% nrow #trips with stating time in 12hr format
# overlap_all[!duplicated(overlap_all[,c('ID', 'Startingtime')]),] %>%
#   group_by(ID) %>% summarise(c = n()) %>% filter(c == 1) %>% nrow # individuals with no return if duplicates removed
# write.table(table(factor(overlap$WardNo.)), sep=",", quote = TRUE)
# write.table(table(factor(overlap_all$WardNo.)), sep=",", quote = TRUE)
# a <- overlap_all %>% group_by(ID, Startingtime, row_number(),add = TRUE)  %>% summarise(c = n()) #visually explore all trips for persons with overlap
#    names(a)[3] <- "index"
#    a <- a[-4]
#    b <- overlap_all %>% mutate(index = row_number())
#    c <- left_join(a,b)


#remove complete duplicates and retain only longer trips where overlaping
no_duplicate <- bangalore[!duplicated(bangalore),] # remove complete duplicates
no_duplicate <- no_duplicate %>% mutate(index = row_number())

no_overlap <- no_duplicate %>% group_by(ID,Startingtime, add = TRUE) %>% summarise( trip_duration = max(trip_duration, na.rm = TRUE), index = index[which.is.max(trip_duration)])%>% left_join(no_duplicate)  # keep only long trips in case of overlap

#Examine long and short overlaping trips
# overlap_only <- inner_join(overlap %>% select(ID, Startingtime), bangalore, by = c("ID", "Startingtime"))# all averlapping trips
# overlap_only <- overlap_only %>% mutate(index = row_number())
# long_trips <- overlap_only %>% group_by(ID,Startingtime, add = TRUE) %>% summarise( trip_duration = max(trip_duration, na.rm = TRUE), index = index[which.is.max(trip_duration)])%>% left_join(overlap_only) # keep only long trips in case of overlap 
# short_trips <- setdiff(overlap_only, long_trips)
# write.table(table(factor(long_trips$ModeofTravel)), sep=",", quote = TRUE); write.table(table(factor(long_trips$Purposeoftravel)), sep=",", quote = TRUE)
# write.table(table(factor(short_trips$ModeofTravel)), sep=",", quote = TRUE); write.table(table(factor(short_trips$Purposeoftravel)), sep=",", quote = TRUE)


#Clean distance variable and use it to calculate trip duration where missing
bangalore <- no_overlap
bangalore$distance <- ifelse(grepl("^0\\.(\\d{3,})|E-",bangalore$distance),
                             as.numeric(bangalore$distance)*1440, bangalore$distance)
##Replace ":" at start of string assumed to be "."
bangalore$distance <- gsub("::|:|,", ".", bangalore$distance,ignore.case = TRUE)
bangalore$distance <- gsub("(\\-1\\/2|1\\/2)", ".5", bangalore$distance,ignore.case = TRUE)
bangalore$distance <- gsub("(1\\/4)", ".25", bangalore$distance,ignore.case = TRUE)
bangalore$distance <- gsub("(3\\/4)", ".75", bangalore$distance,ignore.case = TRUE)
bangalore$distance <- gsub("(1\\/5)", ".2", bangalore$distance,ignore.case = TRUE)
bangalore$distance <- gsub("(\\.\\.)", ".", bangalore$distance,ignore.case = TRUE)
##remove units from distances
bangalore$distance <- gsub("([a-z]|\\s|]|:|\\/\\-|)", "", bangalore$distance,ignore.case = TRUE)
bangalore$distance <- gsub("(^$|\\-)", "0", bangalore$distance,ignore.case = TRUE)
bangalore$distance <- gsub("\\.$", "", bangalore$distance,ignore.case = TRUE)
a <- as.numeric(bangalore$distance)
bangalore$distance <-ifelse((bangalore$ModeofTravel == 1 & a >30) | a >90, a/1000, a)#change m to km

#impute trip_duration with distance and mode speed where approapriate
bangalore <- bangalore%>%
    mutate(mode_speed = ifelse(ModeofTravel == "1",5,
                               ifelse(ModeofTravel == "2",15,
                                      ifelse(ModeofTravel == "3",25,
                                             ifelse(ModeofTravel == "4",25,
                                                    ifelse(ModeofTravel == "5",25,
                                                           ifelse(ModeofTravel == "6",25,
                                                                  ifelse(ModeofTravel == "7",25,
                                                                         ifelse(ModeofTravel == "8",15,
                                                                                ifelse(ModeofTravel == "9",15,
                                                                                       ifelse(ModeofTravel == "10",15,
                                                                                              ifelse(ModeofTravel== "11",15,
                                                                                                     ifelse(ModeofTravel == "12",30,
                                                                                                            NA)))))))))))))

bangalore <- bangalore %>% mutate(trip_duration = ifelse(as.numeric(trip_duration) <= 0 | as.numeric(trip_duration) >= 200 | is.na(trip_duration),
                                                         distance*60/mode_speed, as.numeric(trip_duration)))

#Some more discriptive stats
bagalore %>% group_by(Householdserial.No) %>% group_by(person)



person <- read_excel("HH information-urban bmr.xlsx",sheet = 1, range = cell_cols("AD:AL"),col_types = c("text"))
person <- person[-c(1,2),]
names(person) <- c("age", "female_2", "married_2", "education_asc","occupation_desc", "govt_employed_1", "school_asc", "id")
View(person)


trip <-  bangalore %>% group_by(ID,AGE, GENDER, add = TRUE) %>% summarise(trip = max(as.numeric(Trips), na.rm = TRUE)) 
trip <- trip[-c(1:3),-5]
hh <- person[-c(1,2),c(9, 1,2)]
names(hh)[c(1, 2, 3)] <- c("ID", "AGE", "GENDER")

hh_trip <- left_join(hh,trip)
View(hh_trip)




a <-  bangalore %>% group_by(ID) %>% summarise(trip = max(as.numeric(Trips), na.rm = T)) 


View(a)



















#####India- Bangalore (post cleaning, Lambed's help, 24-April-2019)#####
##cleaned the household member file -- this includes converting the age to numeral, when mentioned along with months, years, or fractions
##reading the HH file
persons<- read.csv('J:/Studies/MOVED/HealthImpact/Data/TIGTHAT/India/Bangalore/Post cleaning/HH information-urban bmr_cleaned_2019_04_24.csv')
persons$married<- as.numeric(persons$married)
for (i in 1: nrow(persons))
    
{
    if(is.na(persons$age[i]))
    {
        if (!is.na(persons$sex[i]))
        {
            
            
            if(persons$sex[i]>2 | persons$sex[i]<1 | (persons$sex[i]>1 & persons$sex[i]<2))
            {
                if( !is.na(persons$married[i]) & persons$married[i]<3)
                {
                    persons$age[i]<- persons$sex[i]
                    persons$sex[i]<- persons$married[i]
                }
            }
        }
    }
}

## shifting those columns where age is not NA, but the same value as person_nr

for (i in 1: nrow(persons))
{
    if (!is.na(persons$sex[i]))
    {
        
        
        if (!is.na(persons$person_nr[i]) & !is.na(persons$age[i]) & persons$sex[i]>2)
        {
            
            if(persons$age[i]==persons$person_nr[i])
            {
                
                persons$age[i]<- persons$sex[i]
                persons$sex[i]<- persons$married[i]
                
            }
            
        }
        
    }
}



for (i in 1: nrow(persons))
{
    
    value<- persons$ward_nr[i]
    repeat
    {
        i<- i+1
        if (persons$ward_nr[i]!="")
        {
            break
        }
        
        persons$ward_nr[i] <- value
        
    }
    
}

persons$person_id<-paste0(persons$ward_nr,  sep="_", persons$hh_nr, sep="_", persons$person_nr)
persons$hh_id<-paste0(persons$ward_nr,  sep="_", persons$hh_nr)

#saveRDS(persons, 'V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/India/Bangalore/Post cleaning/bangalore_persons_cleaned.RDS')
persons <- readRDS('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/India/Bangalore/Post cleaning/bangalore_persons_cleaned.RDS')
#write.csv(persons,'V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/India/Bangalore/Post cleaning/persons_cleaned_new_variables.csv' )

##removing duplicated person ids
persons<- persons[which(!duplicated(persons$person_id)),]

#persons<- persons[which(!duplicated(persons$hh_id)),]
#summary_wards_persons<- persons %>% group_by(ward_nr) %>% summarise(n())
#write.csv(summary_wards_persons, 'V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/India/Bangalore/Post cleaning/unique_hhs_per_ward_persons_file.csv')


##trips dataset
trips<- read.csv('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/India/Bangalore/Post cleaning/Lambed cleaned data/bangalo_rm_useless.csv')

###correcting ward numbers
##there are 2 instances of ward 46 in trips dataset, first of them is actually 41
trips$WardNum<-as.character(trips$WardNum)
trips$WardNum[which(trips$Sn<26700 & trips$WardNum=="46")]<- "41"

###there were blanks for ward column after 2A, which were wrongly filled as 2A, but is actually 3
trips$WardNum[which(trips$Sn>1440 & trips$WardNum=="2A")]<- "3"

##removing the wards for which corresponding data in persons file is absent
trips<- trips[which(!(trips$WardNum %in% c("47", "66", "85","118", "102a", "2A"))),]

##removing the wards in which all trips reported by person 1
trips<- trips[which(!(trips$WardNum %in% c("48", "70", "16A","17A", "52A", "99", "50"))),]



#trips<- trips[which(!duplicated(trips$hh_id)),]
#summary_wards_trips<- trips %>% group_by(WardNum) %>% summarise(n())
#write.csv(summary_wards_trips, 'V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/India/Bangalore/Post cleaning/unique_hhs_per_ward_trips_file.csv')

trips$person_id<- paste0(trips$WardNum,  sep="_", trips$HouseHoldSerialNum, sep="_", trips$PersonNum)

trips$hh_id<- paste0(trips$WardNum,  sep="_", trips$HouseHoldSerialNum)

trips$trip_id<-paste0(trips$WardNum,  sep="_", trips$HouseHoldSerialNum, sep="_", trips$PersonNum, sep="_", trips$Trips)

##calculating the stage with maximum travel time

trip_maxtt<- trips %>% group_by(trip_id) %>% summarise('maxtt'=max(Time))

trips <- trips  %>% left_join(trip_maxtt, by='trip_id')


trips$main_mode<-0
for (i in 1: nrow(trips))
{
    if(!is.na(trips$maxtt[i]))
    {
        
        if(trips$Time[i] == trips$maxtt[i])
        {
            
            trips$main_mode[i]<-1
        }
    }
}

trips<- trips[which(trips$Time!='Inf'),]
trips<- trips[which(trips$Time!=0),]
trips$Age<-as.numeric(trips$Age)
#trips<- trips[which(trips$Age>15),]


sum(trips$Time, na.rm=TRUE)/length(unique(trips$person_id))
sum(trips$Distance, na.rm=TRUE)/ length(unique(trips$person_id))

str(trips)

##trips database with unique person_id
trips_uni<- trips[which(!duplicated(trips$person_id)),]

##identifying persons which made any trips
persons <- persons %>% left_join(trips_uni, by="person_id")
persons_trips<- persons[which(!is.na(persons$trip_id)),]

##selecting households which contributed at least one trips
hh_trips<- persons_trips[which(!duplicated(persons_trips$hh_id.x)),]
hh_trips<- subset(hh_trips, select=c('hh_id.x'))
hh_trips$include<- 1 
names(hh_trips)[1]<-"hh_id"
names(persons)[8]<-"hh_id"
persons<- persons %>% left_join(hh_trips, by='hh_id')
persons<- persons[which(persons$include==1),]
nrow(persons)

persons<- subset(persons, select=c('age','sex', 'person_id','hh_id', 'ward_nr'))


##cleaning the trips from extra variables
trips <- subset(trips, select=c('hh_id', 'person_id', 'trip_id', 'Stage', 'ModeOfTravel','main_mode', 'Distance', 'Time'))

##selecting the trips for the households which are present in the persons file
persons_uni<- persons[which(!duplicated(persons$hh_id)),]

trips_temp <- trips %>% left_join(persons_uni, by='hh_id')
trips_temp <- trips_temp[which(!is.na(trips_temp$person_id.y)),]

###list of households in trips file which need to be included
trips_hhs<- trips_temp[which(!duplicated(trips_temp$hh_id)),]

trips_hhs<- subset(trips_hhs, select=c('hh_id'))
trips_hhs$include<-1


trips<- trips %>% left_join(trips_hhs, by="hh_id")

trips<- trips[which(trips$include==1),]

str(trips)

##attaching the persons file
#trips<- trips %>% left_join(persons, by="person_id")



str(persons)

trips_final <- persons %>% left_join(trips, by="person_id")

##mode names

ModeOfTravel<-as.data.frame(as.factor(seq(1,13)))
mode<- as.data.frame (c("walk","bicycle", "taxi","auto_rickshaw", "shared_auto", "mc", "car", "bus", "bus", "bus", "mini_bus", "train", "other"))
mode_lookup<- cbind(ModeOfTravel, mode)
names(mode_lookup)<-c("ModeOfTravel", "mode")
trips_final<- trips_final %>% left_join(mode_lookup, by="ModeOfTravel")

trips_final$sex[which(trips_final$sex==1)]<-"male"
trips_final$sex[which(trips_final$sex==2)]<-"female"

trips_final <- trips_final[,-c(4,5,6,9,13)]

str(trips_final)


write.csv(trips_final,'V:/Group/RG_PHM/ITHIM-R/data/local/bangalore/bangalore_travel_survey_April2019_post_cleaning.csv')




#####Mexico city ####
setwd("J:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Mexico/Travel surveys/Mexico City 2017/Databases/eod_2017_csv")

person <- read_csv('tsdem_eod2017/conjunto_de_datos/tsdem.csv')
trip <- read_csv('tviaje_eod2017/conjunto_de_datos/tviaje.csv')
stage <- read_csv('ttransporte_eod2017/conjunto_de_datos/ttransporte.csv')
lookup_trip_purpose <- read_csv('lookup_trip_purpose.csv')
lookup_stage_mode <- read_csv('lookup_stage_mode.csv')

##selecting relevant variables
person <- select(person, id_hog, id_soc, sexo, edad)  ## all ind id's sex and age
trip <- trip %>% select(id_soc,id_via, sexo, edad, p5_13, p5_9_1, p5_9_2, p5_10_1,p5_10_2) %>% # trips with purpose and start and end time
    mutate(p5_13 = as.numeric(p5_13), # make mode code as numeric for binding
           trip_duration = (as.numeric(p5_10_1) - as.numeric(p5_9_1))*60 + (as.numeric(p5_10_2) - as.numeric(p5_9_2))) %>% # calculate trip duration
           {.[,-c(6,7,8,9)]}
stage <- stage %>% select(id_via, id_tra, sexo, edad, p5_14, p5_16_1_1, p5_16_1_2) %>% # stage plus mode and stage time
    mutate(p5_14 = as.numeric(p5_14), # make mode code as numeric for binding
           stage_duration = as.numeric(p5_16_1_1)*60 + as.numeric(p5_16_1_2)) %>% {.[,-c(6,7)]}  #calculate stage time

#bind all datesets and retain only useful ones  
trip <- person %>% left_join(trip) %>% left_join(stage)  %>% left_join(lookup_trip_purpose) %>% left_join(lookup_stage_mode) %>% {.[,-c(1, 6,9,12,13)]}
names(trip) <-  c("participant_id", "sex", "age", "trip_id", "trip_duration", "stage_id", "stage_duration", "trip_purpose", "stage_mode")

#add trip mode as stage mode with londest time
trip <- trip %>% group_by(participant_id, sex, age, trip_id) %>% summarise(trip_mode = stage_mode[which.is.max(stage_duration)]) %>% left_join(trip)

quality_check(trip)
#write.csv(trip, "trip_mexico.csv")



## Message from Ralph: I just shared a dropbox folder with the US (2017) and German (2008) data as requested. 
###I followed the codebook you provided. Two items to note: weights are trip weights not not hh weights. 
##The variable geog includes metropolitan area size and not city names. 

#####NETHERLANDS###########
setwd('C:/Users/rg574/Dropbox/global cycling paper/Datasets')
neth <- read.dta13("Netherlands/NTSNed_20102016_tigthat.dta",generate.factors=T)

neth$age<-as.numeric(neth$age)
neth_selected<-subset(neth, select=c("individualid", "region","urban","tripid", "female", "age", "trip_mainmode","trip_duration_min","trip_purpose","trip_dist_km" , "weight_trip", "year"))
neth_selected<-neth_selected[!duplicated(neth_selected), ]
neth_selected<-neth_selected[which(!is.na(neth_selected$tripid)),]

neth_selected$age_cat[neth_selected$age <15] <-  "0 - 15 years"
neth_selected$age_cat[neth_selected$age >=16 & neth_selected$age <=29] <-  "16 - 29 years"
neth_selected$age_cat[neth_selected$age >=30 & neth_selected$age <=39] <-  "30 - 39 years"
neth_selected$age_cat[neth_selected$age >=40 & neth_selected$age <=49] <-  "40 - 49 years"
neth_selected$age_cat[neth_selected$age >=50 & neth_selected$age <=59] <-  "50 - 59 years"
neth_selected$age_cat[neth_selected$age >60] <-  "60+ years"


neth_selected$trip_main_mode<-0
neth_selected$trip_main_mode[which(as.character(neth_selected$trip_mainmode)=="Fiets (elektrisch of niet-elektrisch)")]<-1
#neth_selected$trip_main_mode<-as.factor(neth_selected$trip_main_mode)


neth_selected$trip_purpose2<-0
neth_selected$trip_purpose2[which(as.character(neth_selected$trip_purpose)=="Van en naar het werk")]<-1


neth_selected$householdid<-NA
neth_selected$country<-"Netherlands"

trip_sel<- subset(neth_selected, select=c("country", "region","urban","householdid","individualid","female", "age", "tripid", "trip_dist_km","trip_duration_min", "trip_purpose2", "trip_main_mode", "age_cat", "weight_trip","year"))
colnames(trip_sel)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip","year")

a<-rbind(a, trip_sel)
rm("neth", "neth_selected")

####Switzerland 2010#### no distinction between electric and non-electric bikes and variables for urban and city not clear
#setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Switzerland/MZMV2010_ohne_geo/4_DB_CSV/CSV_ohne_geo')
#stages<-read.csv('etappen.csv',sep=";")
#str(stages)
#stages<-stages[,c("HHNR","ETNR","WEGNR","rdist","e_dauer","f51300","f51700","E_Ausland")]
#stages[1:40,]
#trips<-read.csv('wege.csv',sep=";")
#head(trips)
## 2010 survey has Velo as a category for cycling #2 in f51300 and does not differentiate between e-bike and usual bike
## 2015 survey ddifferentiates between e-bike and other boke (#2 for normal and #20 for e-bike)
#stages$trip_id<- paste(stages$HHNR,stages$ZIELPNR,stages$DMOD,sep="")
#summary<- stages %>% group_by(trip_id) %>% summarise(max_dist=max(rdist))
#head(summary)
#stages<- stages %>% left_join(summary, by="trip_id")
#stages$main_mode<-0
#stages$main_mode[which(stages$rdist==stages$max_dist)]<-1
#head(stages)
#stages<-stages[which(stages$main_mode==1),]
#person<- read.csv('zielpersonen.csv', sep=";")
#str(person)
#person<- subset(person, select=c("HHNR","alter","gesl","WP"))
#household<-read.csv('haushalte.csv', sep=";")
#str(household)
#household<- subset(household, select=c('HHNR','W_Ort', 'DEG'))

#####Switzerland 2015####
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Switzerland/MZMV2015_ohne_geo/4_DB_csv/04_DB_CSV')
stages<-read.csv('etappen.csv')
stages<-stages[,c("HHNR","ETNR","WEGNR","rdist","e_dauer","f51300","f51700","E_Ausland",'WP')]
stages$trip_id<- paste(stages$HHNR,stages$WEGNR,sep="")
summary<- stages %>% group_by(trip_id) %>% summarise(max_dist=max(rdist))
stages<- stages %>% left_join(summary, by="trip_id")
stages$main_mode<-0
stages$main_mode[which(stages$rdist==stages$max_dist)]<-1
stages<-stages[which(stages$main_mode==1),]

trips<-read.csv('wege.csv')
trips$trip_id<- paste(trips$HHNR,trips$WEGNR,sep="")
trips<- subset(trips, select=c('trip_id','wzweck1'))

person<- read.csv('zielpersonen.csv')
person<- subset(person, select=c("HHNR","alter","gesl"))

household<-read.csv('haushalte.csv')
household<- subset(household, select=c('HHNR','W_Ort', 'W_DEGURBA'))

stages<- stages %>% left_join(trips, by="trip_id")
stages<- stages %>% left_join(person, by="HHNR")
stages<- stages %>% left_join(household, by="HHNR")

stages$female<-1
stages$female[which(stages$gesl==1)]<-0
stages$trip_purpose<-0
stages$trip_purpose[which(stages$wzweck1==2)]<-1

## 2010 survey has Velo as a category for cycling #2 in f51300 and does not differentiate between e-bike and usual bike
## 2015 survey differentiates between e-bike and other boke (#2 for normal and #20 for e-bike)
stages$trip_mode<- 0
stages$trip_mode[which(stages$f51300==2)]<- 1
stages$country<- "Switzerland"
stages$urban<-0
stages$urban[which(stages$W_DEGURBA<3)]<-1
stages$year<-2015
stages$age_cat="NA"

str(stages)
stages<- subset(stages, select=c("country", "W_Ort","urban","HHNR","HHNR","female", "alter", "WEGNR", "rdist","e_dauer", "trip_purpose", "trip_mode","age_cat","WP","year"))
colnames(stages)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip","year")
a<-rbind(a, stages)

rm("stages", "person","stages", "trips")




#####United States from Ralph#####
setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/USA 2017 and German 2008')
US <- read.dta13("NHTS 2017 USA for Rahul.dta")
US$country<- "USA"
US$age_cat<-'NA'
US$year<-2017
US$geog<- as.factor(US$geog)
US<-subset(US, select=c("country", "geog", "urban","hh_id", "ind_id", "gender", "age", "trip_id","distance", "time", "purpose", "mode", "age_cat", "weights", "year"))
colnames(US)<-c("country", "city/region","urban", "hh_ID", "ind_ID","female","age" ,"trip_ID", "trip_distance","trip_time", "trip_purpose", "trip_main_mode", "age_cat", "weight_trip","year")
a<-rbind(a, US)
rm("US")

