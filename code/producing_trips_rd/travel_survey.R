#####A Notes -------------------------------------------------------------------
## These codes were initiated by Raul. Lambed is modifying the codes to generate trip datasets for the selected TIGTHAT cities. Please change work directory from J to V if you are on medschool network. 

## trip data set is processed to stage level where possible

#####Argentina Buenos Aires############

rm(list =ls())

source("J:/Group/lambed/ITHIM-R/code/producing_trips_rd/used_functions.R")

setwd('J:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Argentina/WP1-TS/Buenos Aires/')

person_0 <- read_sav('ENMODO_PERSONAS_pub_20121115.sav')
trip_0 <- read_sav("ENMODO_VIAJES_pub_20121115.sav")
stage_0 <- read_sav('ENMODO_ETAPAS_pub_20121115.sav')

trip_purpose <- read_excel("lookup.xlsx",sheet = "trip_purpose", range = cell_cols("A:B"))
stage_mode <- read_excel("lookup.xlsx",sheet= "stage_mode", range = cell_cols("A:C"))# also ranks the modes 

#keep relevant variables
person <- person_0[,c("PARTIDO","IDH","IDP", "EDAD", "SEXO","wt1")]

trip <- trip_0 %>% 
    mutate(trip_duration_1 = ((HORALLEG - HORASALI)%%24)*60 + (MINLLEGA - MINSALID), 
           trip_duration_2 = difftime(HORAFIN,HORAINI,tz="GMT",units="mins"),
           trip_duration = ifelse(trip_duration_1 > 540, abs(trip_duration_2), trip_duration_1)) %>% 
    left_join(trip_purpose) %>%
    select(IDP, IDV, trip_duration, trip_purpose)

stage <- stage_0 %>% 
    left_join(stage_mode) %>%  #add mode names in english and ranks
    left_join(count(.,IDV)) %>%  # add number of stages for each trip --> to be used later
    left_join(trip[,c("IDV", "trip_duration")]) %>% #add trip duration
    mutate(stage_duration = ifelse(DURAMINU == 99 & n == 1, trip_duration, #get stage duration from trip duration
                                   ifelse(DURAMINU == 99 & n > 1, NA, 60*DURAHORA + DURAMINU))) %>% 
    
    left_join(group_by(., stage_mode) %>% summarise(average_mode_time = mean(stage_duration, na.rm=T))) %>% 
    
    group_by(IDV) %>% 
    mutate(trip_mode = stage_mode[which.is.max(rank)], # add trip main mode
           average_mode_time = ifelse(DURAMINU == 99 & n > 1 & "walk" %in% levels(as.factor(stage_mode)) & 
                                          stage_mode == "walk", 1,
                                      ifelse(DURAMINU == 99 & n > 1 & "walk" %in% levels(as.factor(stage_mode)) & 
                                                 stage_mode != "walk",6,average_mode_time))) %>% 
    
    left_join(group_by(.,IDV) %>% summarise(sum_average =sum(average_mode_time))) %>% 
    mutate(stage_duration = ifelse(is.na(stage_duration), 
                                   round(trip_duration*average_mode_time/sum_average),
                                   stage_duration)) %>% 
    {.[,c("IDP", "IDV", "IDE", "stage_mode", "stage_duration", "trip_mode")]}
       
#Join the three datasets and rename variables
trip <- person %>% 
    left_join(trip) %>% 
    left_join(stage) %>% 
    left_join(mode_speed %>% rename(trip_mode = mode)) %>%
    mutate(trip_distance = round(mode_speed*trip_duration/60)) %>% 
    select(-mode_speed) %>% 
    left_join(mode_speed %>% rename(stage_mode = mode)) %>% 
    mutate(stage_distance = round(mode_speed*stage_duration/60),
           sex = ifelse(SEXO == "Masculino", "Male", "Female")) %>% 
    rename(cluster_id = PARTIDO,
           household_id = IDH,
           participant_id = IDP,
           participant_wt = wt1,
           trip_id = IDV,
           age = EDAD,
           stage_id = IDE) %>% 
    select(cluster_id, household_id, participant_id, 
           participant_wt, age, sex, trip_id, trip_purpose, 
           trip_mode, trip_duration, trip_distance, stage_id,
           stage_mode, stage_duration, stage_distance)


trip$year <- "2012"


#quality_check(Buenos_Aires)
write.csv(trip, "J:/Group/lambed/ITHIM-R/data/local/buenos_aires/buenos_aires_trip.csv")

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

rm(list =ls())

source("J:/Group/lambed/ITHIM-R/code/producing_trips_rd/used_functions.R")


setwd('J:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Brazil/Sao Paulo/Pesquisa Origem Destino 2012')

trip_0 <- read.csv('Mobilidade_2012_v0.csv')

trip_mode <- data.frame(MODOPRIN = 1:17,
                        trip_mode = c("bus","bus","bus","bus","bus",
                                      "car","car", "taxi","van","van","van", 
                                      "metro", "train", "motorcycle", "bicycle", "walk", "other"))
stage_mode <- data.frame(stg_mode = 1:17,
                         stage_mode = c("bus","bus","bus","bus","bus",
                                       "car","car", "taxi","van","van","van", 
                                       "metro", "train", "motorcycle", "bicycle", "walk", "other"))
sex <- data.frame(sex = c("Male", "Female"), SEXO = 1:2)
trip_purpose <- data.frame(trip_purpose = c("other", "other","work", "school", "other", "other", 
                                            "other", "return", "other", "other"), 
                           MOTIVO_O = 1:10)


#select relevant data
no_trip <-  trip_0 %>% filter(is.na(MODO1) & is.na(MODO2) & is.na(MODO3) & is.na(MODO4))
trip <- setdiff(trip_0, no_trip) %>%
    gather("stage_id","stg_mode", MODO1, MODO2, MODO3, MODO4 ) %>% 
    filter(!is.na(stg_mode)) %>% 
    bind_rows(no_trip %>% rename(stage_id = MODO1, stg_mode= MODO2) %>% select(-c(MODO3, MODO4))) %>% 
    mutate(stage_id = ifelse(stage_id =="MODO1", 1, 
                             ifelse(stage_id=="MODO2",2,
                                    ifelse(stage_id == "MODO3", 3, 
                                           ifelse(stage_id == "MODO4", 4, NA)))),
           trip_distance = as.numeric(DISTANCIA)/1000) %>% 
    left_join(trip_mode) %>%
    left_join(stage_mode) %>%
    left_join(sex) %>% 
    left_join(trip_purpose) %>% 
    rename(cluster_id = ZONA,
           household_id = ID_DOM,
           participant_id = ID_PESS,
           participant_wt = FE_PESS,
           age = IDADE, 
           trip_id = N_VIAG,
           trip_duration = DURACAO) %>% 
    select(cluster_id, household_id, participant_id, participant_wt, age, sex, trip_id, trip_purpose, trip_mode, trip_duration,trip_distance, stage_id, stage_mode)

trip$year <- "2012"

#quality_check(trip)
write.csv(trip, "J:/Group/lambed/ITHIM-R/data/local/sao_paulo/sao_paulo.csv")

#####Brazil Belo Horizonte######

rm(list =ls())

source("J:/Group/lambed/ITHIM-R/code/producing_trips_rd/used_functions.R")

setwd('J:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Brazil/Belo Horizonte/Travel survey')

trip_0 <- read.table('dbo_TB_VIAGENS_INTERNAS_RMBH.txt', header = TRUE, sep=",")
person_0 <- read.table('dbo_TB_DOMICILIO_PESSOA_ENTREGA.txt', header = TRUE, sep=",")
#hh_weights_0<- read.table('dbo_TB_FATOR_EXPANSÂO_DOMICÍLIO.txt', header = TRUE, sep=",")


#lookup tables
trip_mode <- bind_cols(
    DS_SH_MEIO_TRANSPORTE =distinct(trip_0, DS_SH_MEIO_TRANSPORTE),
    trip_mode = factor(c("bus", "car", "walk", "metro", "van", "car", "motorcycle", "other", "car", "bicycle", "taxi"),
                       levels = c("bicycle","bus","car","metro","motorcycle","other" ,"taxi", "train","van","walk")))
sex <- data.frame(sex= c("Male", "Female"), DS_SEXO = c("Masculino", "Feminino"))
trip_purpose <- data.frame(distinct(trip_0, motivo_origem), 
                           trip_purpose =c("return", "school", "work", "other", "other", "other", 
                                           "work", "work", "other", "other", "other", "other", "other", "other") )

#selecting relevant variables
person <-  person_0 %>%
    mutate(cluster_id = 1,
           household_id = ID_DOMICILIO, 
           participant_id = paste0(ID_DOMICILIO,"_",ID_PESSOA),
           participant_wt = 1) %>%
    left_join(sex) %>% 
    select(cluster_id, household_id, participant_id,participant_wt, sex, IDADE)

trip <- trip_0 %>%
    mutate(participant_id = paste0(Domicilio,"_",Pessoa),
           trip_duration = (as.numeric(substr(trip_0$TEMPO.DE.DESLOCAMENTO, 12,13)))*60 + 
               as.numeric(substr(trip_0$TEMPO.DE.DESLOCAMENTO, 15,16))) %>% 
    left_join(trip_mode) %>%
    left_join(trip_purpose) %>% 
    select(participant_id, Viagem, trip_duration, trip_mode, trip_purpose)

trip <- person %>% 
    left_join(trip) %>%
    rename(age= IDADE, trip_id = Viagem)
#quality_check(trip)

trip$year <- "2012"

write.csv(trip, "J:/Group/lambed/ITHIM-R/data/local/belo_horizonte/belo_horizonte_trip.csv")


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



#####Chile Santiago####

rm(list =ls())

source("J:/Group/lambed/ITHIM-R/code/producing_trips_rd/used_functions.R")

setwd("J:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Chile/Travel Surveys/Santiago")

person_0 <- read.csv("Persona.csv")
trip_0 <- read.csv("Viaje.csv")
stage_0 <- read.csv("stages.csv")

age <- read.csv('EdadPersonas.csv')
trip_purpose <- read.csv("lookup_trip_purpose.csv")
trip_mode <- read.csv("lookup_trip_mode.csv")
stage_mode <- read.csv("lookup_stage_mode.csv")
sex <- bind_cols(sex= c("Male", "Female"), Sexo = c(1,2))


person <- person_0 %>% 
    mutate(cluster_id = 1,
           household_id = Hogar,
           participant_wt = Factor) %>% 
    left_join(rename(age, age= Edad)) %>% 
    left_join(sex) %>% 
    select(cluster_id, household_id, participant_wt, Persona, sex, age)

trip <- trip_0 %>% 
    left_join(trip_purpose) %>% 
    left_join(trip_mode) %>% 
    select(Persona, Viaje, TiempoViaje, trip_purpose, trip_mode)

stage <- stage_0 %>% 
    left_join(stage_mode) %>% 
    select(Persona, Viaje, Etapa, stage_mode)


trip <- person %>%
    left_join(trip) %>% 
    left_join(stage) %>% 
    rename(participant_id = Persona, trip_id = Viaje, 
           stage_id = Etapa, trip_duration = TiempoViaje)

#trip with "other" mode
trip_1 <- 
    trip %>%
    filter(trip_mode == "other") %>% 
    mutate(mode = stage_mode) %>% 
    left_join(mode_speed) %>% 
    group_by(cluster_id, household_id, trip_id) %>% 
    mutate(trip_mode = stage_mode[which.is.max(mode_speed)]) %>% 
    select(-mode, -mode_speed) 

#trip with modes other than "other"
trip_2 <- 
    trip %>% 
    setdiff(
        trip %>% 
            filter(trip_mode == "other")
    )
#bind rows  
santiago <- bind_rows(trip_2, trip_1, .id =NULL)


trip <- santiago
trip$year <- "2012"
    
#quality_check(santiago)
write.csv(trip, "J:/Group/lambed/ITHIM-R/data/local/santiago/santiago.csv")


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

#####Colombia Bogota----

rm(list =ls())
rm(list =ls())

source("J:/Group/lambed/ITHIM-R/code/producing_trips_rd/used_functions.R")


setwd('J:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Colombia/Bogota/Travel')

#data
person_0 <- read_excel("encuesta 2015 - personas.xlsx",sheet = 1, range = cell_cols("A:BS"))
trip_0 <- read_excel("encuesta 2015 - viajes.xlsx",sheet = 1, range = cell_cols("A:K"))
stage_0 <- read_excel("encuesta 2015 - etapas.xlsx",sheet = 1, range = cell_cols("A:K"))

#lookup tables
trip_mode <- read_excel("lookup_trip_mode.xlsx",sheet = 1, range = cell_cols("A:B"))
stage_mode <- read_excel("lookup_stage_mode.xlsx",sheet = 1, range = cell_cols("A:B"))
trip_purpose <-  read_excel("lookup_trip_purpose.xlsx",sheet = 1, range = cell_cols("A:B"))


#select relevant varaibles
person <- 
    person_0 %>%
    mutate(sex = ifelse(SEXO == "Hombre", "Male","Female"),
           cluster_id = 1) %>% 
    select(cluster_id, ID_ENCUESTA, NUMERO_PERSONA, FE_TOTAL,sex,EDAD)


trip_1 <- trip_0 %>% 
    mutate(DURATION = DURATION*.7) %>% #about 30 to 40 % of duaration seem not to be used for travelling
    left_join(trip_mode) %>% 
    select(ID_ENCUESTA, NUMERO_PERSONA, NUMERO_VIAJE, MOTIVOVIAJE, DURATION, trip_mode)

stage <- stage_0 %>% 
    left_join(stage_mode) %>% 
    select(ID_ENCUESTA, NUMERO_PERSONA, NUMERO_VIAJE,NUMERO_ETAPA, stage_mode)

#calcualte avg mode time for unimodal trips 
#later use the ratios to calculate stage duration except for walk to public transport
stage <- stage %>% 
    count(ID_ENCUESTA, NUMERO_PERSONA, NUMERO_VIAJE) %>% 
    filter(n == 1) %>% 
    left_join(trip_1) %>% 
    group_by(trip_mode) %>% 
    summarise(average_mode_time = mean(DURATION, na.rm = T)) %>% 
    rename(stage_mode = trip_mode) %>% 
    right_join(stage)

#average mode time for multimodal trips
stage <- stage %>% 
    filter(is.na(average_mode_time)) %>% 
    left_join(trip_1) %>% 
    group_by(trip_mode) %>% 
    summarise(average_mode_time = mean(DURATION, na.rm = T)) %>% 
    rename(stage_mode = trip_mode) %>% 
    right_join(stage)

#ratio of mode time for public transport trips with walking
stage <- stage %>% 
    group_by(ID_ENCUESTA, NUMERO_PERSONA, NUMERO_VIAJE) %>% 
    mutate(average_mode_time = ifelse("walk" %in% levels(as.factor(stage_mode)) & 
                                          length(levels(as.factor(stage_mode)))> 1 & 
                                          stage_mode == "walk", 1, 
                                      ifelse("walk" %in% levels(as.factor(stage_mode)) & 
                                                 length(levels(as.factor(stage_mode)))> 1 & 
                                                 stage_mode != "walk", 6,
                                             average_mode_time))) %>% #add walking to public transport ratio of 1:6
    left_join(group_by(.,ID_ENCUESTA, NUMERO_PERSONA, NUMERO_VIAJE) %>% summarise(sum_average = sum(average_mode_time , na.rm = T)))
    
#compose trip dataset
trip_2 <- person %>% 
    left_join(trip_1) %>% 
    left_join(stage) %>% 
    left_join(trip_purpose) %>% 
    mutate(stage_duration = round(DURATION*average_mode_time/sum_average)) %>% 
    rename(household_id = ID_ENCUESTA,
           participant_wt = FE_TOTAL,
           participant_id = NUMERO_PERSONA,
           trip_id = NUMERO_VIAJE,
           stage_id = NUMERO_ETAPA,
           age= EDAD, 
           trip_duration = DURATION) %>% 
    select(cluster_id, household_id, participant_id, sex, age,participant_wt,
           trip_id, trip_purpose, trip_mode, trip_duration, stage_id, stage_duration, stage_mode)

#replace mode = special with appropriate modes (car and bus)
trip_3 <- 
    trip_2 %>% 
    filter(!is.na(trip_id)) %>% 
    group_by(cluster_id, household_id, participant_id, trip_id) %>% 
    summarise(trip_mode = stage_mode[which.is.max(stage_duration)],
              trip_mode = ifelse(is.na(trip_mode), stage_mode, trip_mode)) 


trip <- 
    trip_2 %>%
    select(-trip_mode) %>% 
    left_join(trip_3) 

trip$year <- "2015"

write.csv(trip, "J:/Group/lambed/ITHIM-R/data/local/bogota/bogota_trip.csv")

#quality_check(trip)
#trip %>% filter(!is.na(trip_id) & is.na(trip_mode)) %>% View()




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


#####Ghana ####

rm(list =ls())

source("J:/Group/lambed/ITHIM-R/code/producing_trips_rd/used_functions.R")

setwd("J:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Accra/Accra data and microdata/Time Use Survey/Data")

# read data
time_use_0 <- read.spss("GTUS 2009 24 Hours Individual Diary.sav", to.data.frame = T)

#lookup
trip_mode <- data.frame(distinct(time_use_0, ActLoc2), 
                        trip_mode = c(NA, "walk","bus","taxi",  "bicycle",  "car", 
                                      "other","train","other","other","other" ))


dat <- 
    time_use_0 %>% 
    filter(region == "Greater Accra", URBRUR == "Urban") %>% 
    rename(sex = B102,
           age = B105,
           cluster_id = EANum,
           household_id = HHNum,
           participant_id = MemID,
           participant_wt = Adj_hh_wt) %>% 
    mutate(#separate start and end time,
        start = substr(Diary_hour, 1,2),
        end = substr(Diary_hour, 6,7),
        Duration = ActDur,
        Duplicate = "notDuplicate",
        Same = "notSame")

dat$ActCode1 <- ifelse(grepl("Work", dat$ActCode1), "work", ifelse(grepl("Learning",dat$ActCode1), "school", "other"))



levels(dat$ActLoc2) <- c(levels(dat$ActLoc2), "missing")
dat$ActLoc2[which(is.na(dat$ActLoc2))] <- "missing"

#for loop sums same activity
for(i in 2:nrow(dat)){
    if(dat$cluster_id[i]== dat$cluster_id[i-1] &
       dat$household_id[i] == dat$household_id[i-1] &
       dat$participant_id[i] == dat$participant_id[i-1] &
       dat$ActCode[i] == dat$ActCode[i-1] & 
       dat$ActLoc2[i] == dat$ActLoc2[i-1] &
       dat$start[i] == dat$end[i-1]){
        dat$Duration[i] <- `+`(dat$Duration[i], dat$Duration[i-1])
        dat$Duplicate[i] <- "Duplicate"
        dat$Same[i-1] <- "Same"		
    }
}

dat$ActLoc2[which(dat$ActLoc2 =="missing")] <- NA

dat <- dat %>% 
    #drop sub-activities that have been summed into one activity
    filter(!(Same == "Same")) %>% 
    #add modes
    left_join(trip_mode) 

    

trip <- 
    dat %>% 
    #filter trips only
    filter(ActLoc1 == "Travelling / Moving") %>% 
    mutate(#identify trip by row number
        trip_id = row_number(),
        trip_duration = Duration,
        trip_purpose = ActCode1)

no_trip <- dat %>% 
    anti_join(trip, by= c("cluster_id", "household_id", "participant_id")) %>% 
    mutate(trip_id = NA,
           trip_duration = NA,
           trip_purpose = NA)
#join datasets
trip <- bind_rows(trip, no_trip) %>% 
    select(cluster_id, household_id, participant_id, participant_wt, age, sex, 
           trip_id, trip_mode, trip_duration, trip_purpose) %>% 
           {.[!duplicated(.),]}

trip$trip_mode[which(!is.na(trip$trip_id) & is.na(trip$trip_mode))] <- "other"


trip$year <- "2009"
write.csv(trip, "J:/Group/lambed/ITHIM-R/data/local/accra/accra_trip.csv")
#quality_check(trip)



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

rm(list =ls())

source("J:/Group/lambed/ITHIM-R/code/producing_trips_rd/used_functions.R")
package()

setwd('J:/Studies/MOVED/HealthImpact/Data/TIGTHAT/India/Delhi')

#data
person_0 <- read.csv('persons.csv', na.strings = c("", "NA"))
stage_0 <- read.csv('trips_stages_delhi.csv')


#lookups
stage_mode <- bind_cols(stage_mode = c("walk", "bicycle", "motorcycle", "car", 
                                       "rickshaw", "rickshaw", "car", "bus", "metro", "train", "taxi"),
                        mode = 1:11)
trip_mode <- bind_cols(trip_mode = c("walk", "bicycle", "motorcycle", "car", 
                                      "rickshaw", "rickshaw", "car", "bus", "metro", "train", "taxi"),
                       Main.Mode = 1:11)
sex <-  bind_cols(sex = c("Male", "Female"), female = c(0,1))
trip_purpose <- bind_cols(Trip.Purpose = 0:12,
                          trip_purpose = c("return", "work", "school", "other","other","other","other",
                                           "other","other","other","other","other","other"))


#select relevant variables
person <- 
    person_0 %>% 
    rename(participant_id = Member.ID, household_id =Form.No.,
           age = Age, participant_wt = Weights_final) %>%
    left_join(sex) %>% 
    mutate(cluster_id = 1,
           participant_id = ifelse(is.na(participant_id), paste0("U0", row_number()), paste(participant_id))) %>% 
    {.[which(!duplicated(.$participant_id)),]} %>% 
    select(-Form_id_new, -Member.No., -female)


stage <- 
    stage_0 %>% 
    mutate(participant_id = paste0(stage_0$Form.ID, stage_0$Member.No.), 
           stage_id = row_number(),
           trip_duration = Trip_Time_Duration*60,
           stage_duration = Travel.Time*60) %>%
    left_join(stage_mode) %>% 
    left_join(trip_mode) %>% 
    left_join((trip_purpose)) %>% 
    rename(trip_id = Trip.ID, 
           trip_distance = Trip_total_distance, 
           stage_distance = Distance) %>% 
    select(participant_id, trip_id, stage_id, trip_mode, trip_purpose, trip_duration, trip_distance, stage_mode, 
           stage_duration, stage_distance)

#combine dataframes 
trip <- person %>% 
    left_join(stage)

#replace NA trip modes with other
trip$trip_mode[which(!is.na(trip$trip_id) & is.na(trip$trip_mode) )] <- "other"

trip$year <- "Year"
write.csv(trip, "J:/Group/lambed/ITHIM-R/data/local/delhi/delhi_trip.csv")

#quality_check(trip)

#####india Bangalore ####
rm(list =ls())

source("J:/Group/lambed/ITHIM-R/code/producing_trips_rd/used_functions.R")

setwd("J:/Studies/MOVED/HealthImpact/Data/TIGTHAT/India/Bangalore")

person_0 <- read_excel("HH information-urban bmr.xlsx",sheet = 1, range = cell_cols("A:AM"),col_types = c("text"))
stage_0 <- read_excel("COMPILED DATA final.xlsx",sheet = 1, range = cell_cols("A:AA"), col_types = c("text"))

#lookup
trip_purpose <-  bind_cols(distinct(stage_0, `Purpose of travel`),
                            trip_purpose = c("other", "work", "return", "school", "other","other","other","other","other",
                                             "other","other","other","other","other","other","other","other","other",
                                             "other","other","other"))
stage_mode <-  bind_cols( distinct(stage_0,`Mode of Travel`),
                           mode = c("other", "walk", "bus", "motorcycle", "bicycle", 
                           "bus", "car","bus","taxi", "rickshaw", 
                           "bus","other", "train","other", "taxi", "other","other"))


person <- person_0 %>% 
    {.[-c(1,2),]} %>% 
    filter(is.na(...39)) %>% 
    rename(household_id = `Household Serial Number`,
           cluster_id = `WARD NO`,
           person = ...28,
           age = `Age(Years)`,
           sex = `Sex:\r\n1. Male,\r\n2. Female`,
           id = ...38) %>% 
    select(household_id, cluster_id, person, age, sex, id) %>% 
    bind_rows(
        person_0 %>% 
            filter(!is.na(...39)) %>% 
            slice(1:186) %>% 
        rename(household_id = `Household Serial Number`,
               cluster_id = `WARD NO`,
               person = Name, 
               age = `Sex:\r\n1. Male,\r\n2. Female`, 
               sex = `Marital Status:\r\n1. Unmarried,\r\n2. Married,\r\n3. Others.`, 
               id = ...39) %>% 
            select(household_id, cluster_id, person, age, sex, id) %>% 
            bind_rows(
                person_0 %>% 
                    filter(!is.na(...39)) %>% 
                    slice(187:715) %>% 
                    rename(household_id = `Household Serial Number`,
                           cluster_id = `WARD NO`,
                           person = ...28,
                           age = `Sex:\r\n1. Male,\r\n2. Female`, 
                           sex = `Marital Status:\r\n1. Unmarried,\r\n2. Married,\r\n3. Others.`, 
                           id = ...39) %>% 
                    select(household_id, cluster_id, person, age, sex, id)  ) ) %>% 
    rename(participant_id = id) %>% 
    select(cluster_id, household_id, participant_id, age, sex) %>% 
    mutate(participant_wt =1, sex = ifelse(sex==1,"Male", "Female"))

#add omitted cluster_id and household_id
for(i in 2:nrow(person)){
    if(is.na(person$cluster_id[i])){
        person$cluster_id[i] = person$cluster_id[i-1]
    }
}
       

for(i in 2:nrow(person)){
    if(is.na(person$household_id[i])){
        person$household_id[i] = person$household_id[i-1]
    }
}




stage <- stage_0 %>% 
    mutate(x = `Transfer time in Min     (Walk time+wait time for next mode)`,
           duration = ifelse(grepl("E-",x), as.numeric(x)*1440,x),
           duration = gsub("([A-z]|:00|:|;|\\s|-|\\.$)","", duration ),
           time_diff = ifelse(as.numeric(`Starting time`)<1, 
                              (as.numeric(`Finishing Time`) - as.numeric(`Starting time`))*1440, 
                              (as.numeric(`Finishing Time`) - as.numeric(`Starting time`))),
           duration = ifelse((duration == "2" | duration == "3") & 
                                 (grepl("m|M", x) | time_diff < 1), as.numeric(duration)/100, duration),
           duration =  ifelse((duration == "0" | is.na(duration)) & (time_diff<180 & time_diff>0), time_diff, duration ),
           stage_duration = ifelse(as.numeric(duration) < 4,
                                   gsubfn("([0-3])(\\.*\\d*)",
                                          ~as.numeric(x)*60 + ifelse(is.na(as.numeric(y)),0,as.numeric(y)*100), duration),
                                   duration),
           stage_duration = as.numeric(stage_duration),
           
           #stage distance
           y = `Stage Distance(Kms)`,
           distance = ifelse(grepl("^0\\.(\\d{3,})|E-",y), as.numeric(y)*1440, y),
           #Replace ":" at start of string assumed to be "."
           distance = gsub("::|:|,", ".", distance,ignore.case = TRUE),
           distance = gsub("(\\-1\\/2|1\\/2)", ".5", distance,ignore.case = TRUE),
           distance = gsub("(1\\/4)", ".25", distance,ignore.case = TRUE),
           distance = gsub("(3\\/4)", ".75", distance,ignore.case = TRUE),
           distance = gsub("(1\\/5)", ".2", distance,ignore.case = TRUE),
           distance = gsub("(\\.\\.)", ".", distance,ignore.case = TRUE),
           ##remove units from distances
           distance = gsub("([a-z]|\\s|]|:|\\/\\-|)", "", distance,ignore.case = TRUE),
           distance = gsub("(^$|\\-)", "0", distance,ignore.case = TRUE),
           distance = gsub("\\.$", "", distance,ignore.case = TRUE),
           distance = as.numeric(distance),
           distance = ifelse((`Mode of Travel` == 1 & distance >30) | distance >90, distance/1000, distance)) %>%
    
    
    rename(`Purpose of travel`=`Purpose of travel`, `Mode of Travel`=`Mode of Travel` ) %>% 
    #add purpose
    left_join(trip_purpose) %>% 
    #add mode names
    left_join(stage_mode) %>% 
    #add mode speed
    left_join(mode_speed) %>% 
    mutate(stage_duratoion  = ifelse(stage_duration <= 0 | stage_duration >= 200 | is.na(stage_duration),
                                     distance*60/mode_speed, stage_duration),
           stage_distance = ifelse(distance <= 0 | is.na(distance), stage_duration*mode_speed/60, distance)) %>%
    rename(hh = `House hold serial.No`, ward =`Ward No.`, person = `No. of Person`, participant_id = ID ,
           trip_id = Trips, stage_id = Stage, age = AGE, stage_mode = mode ) %>%
           {.[-1,]} %>% 
    select(participant_id, trip_id, stage_id, age, trip_purpose, 
           stage_mode, stage_distance, stage_duration, mode_speed) %>% 
           {.[!duplicated(.),]}


trip <- 
    stage %>% 
    group_by(participant_id, trip_id) %>% 
    summarise(trip_mode = ifelse(is.na(stage_mode[which.is.max(stage_duration)]),
                                 stage_mode[which.is.max(mode_speed)], stage_mode[which.is.max(stage_duration)]),
              trip_duration = sum(stage_duration, na.rm=T))  
    
trip <- 
    person %>%
    left_join(trip) %>% 
    left_join(stage) %>% 
    select(-mode_speed)

trip$year <- "2011"
write.csv(trip, "J:/Group/lambed/ITHIM-R/data/local/bangalore/bangalore_trip.csv")

#quality_check(trip)

#####Mexico city ####

rm(list =ls())

source("J:/Group/lambed/ITHIM-R/code/producing_trips_rd/used_functions.R")

setwd("J:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Mexico/Travel surveys/Mexico City 2017/Databases/eod_2017_csv")

#data
person_0 <- read_csv('tsdem_eod2017/conjunto_de_datos/tsdem.csv')
trip_0 <- read_csv('tviaje_eod2017/conjunto_de_datos/tviaje.csv')
stage_0 <- read_csv('ttransporte_eod2017/conjunto_de_datos/ttransporte.csv')

#lookups
trip_purpose <- read_excel("lookup.xlsx",'trip_purpose')
stage_mode <- read_excel("lookup.xlsx",'stage_mode')
sex <- bind_cols(sex=c("Male", "Female"), sexo = 1:2)



##selecting relevant variables
person <- person_0 %>% 
    left_join(sex) %>%
    mutate(cluster_id = 1) %>% # select the appropriate cluster number
    select(cluster_id,id_hog, id_soc, sex, edad, factor) 
    ## all ind id's sex and age
trip <- trip_0 %>% 
    mutate(trip_duration = (as.numeric(p5_10_1) - as.numeric(p5_9_1))*60 + 
               (as.numeric(p5_10_2) - as.numeric(p5_9_2)),
           trip_duration = ifelse(trip_duration < (-450), 1440 + trip_duration, trip_duration),
           trip_duration = ifelse(trip_duration < 0, 0 - trip_duration, trip_duration),
           trip_duration = ifelse(trip_duration == 0, NA, trip_duration),
           p5_13 = as.numeric(p5_13)) %>% 
    left_join(trip_purpose) %>% 
    select(id_soc,id_via, trip_duration, trip_purpose)
    
stage <- stage_0 %>% 
    mutate(p5_14 = as.numeric(p5_14), # make mode code as numeric for binding
           stage_duration = as.numeric(p5_16_1_1)*60 + as.numeric(p5_16_1_2),
           stage_duration = ifelse(p5_16_1_1 == "99", NA,stage_duration)) %>%
    left_join(stage_mode) %>%
    mutate(mode = stage_mode) %>% 
    left_join(mode_speed) %>% 
    left_join(group_by(.,id_via) %>% 
                  summarise(trip_mode = ifelse(is.na(stage_mode[which.is.max(stage_duration)]), 
                                               stage_mode[which.is.max(mode_speed)],
                                               stage_mode[which.is.max(stage_duration)] ))) %>% 
    select(id_via, id_tra, stage_mode,trip_mode, stage_duration)


#bind all datesets and retain only useful ones  
mexico_city <- person %>% 
    left_join(trip) %>% 
    left_join(stage) %>% 
    rename(household_id = id_hog, participant_id = id_soc,participant_wt = factor,
           trip_id = id_via, age = edad, stage_id = id_tra) 

trip <- mexico_city
trip$year <- "2017"
#quality_check(mexico_city)
write.csv(trip, "J:/Group/lambed/ITHIM-R/data/local/mexico/mexico_city_trip.csv")

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

#####SOUTH AFRICA CAPE TOWN####


rm(list =ls())

source("V:/Group/lambed/ITHIM-R/code/producing_trips_rd/used_functions.R")
library(chron)

setwd('V:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Cape Town')

#original data
household_0 <- read_excel("2013 Household Survey Data Sets & Meta Data_RHDHV_230114/Final datasheets_incl metadata_05-12-2013/CT_Household_travel_survey_householdinfo/test.xlsx")


person_0 <- read_excel("2013 Household Survey Data Sets & Meta Data_RHDHV_230114/Final datasheets_incl metadata_05-12-2013/CT_Household_travel_survey_person info/CT_household_travel_survey_person.xlsx",
                       range = cell_cols("B:E"))

trip_0 <- read_excel("2013 Household Survey Data Sets & Meta Data_RHDHV_230114/Final datasheets_incl metadata_05-12-2013/CT_Trip diary/CT_household_travel_survey_diary.xlsx",
                     sheet = "Trip Diary_copy")


#get relevant household info
household <- household_0[-c(1:14),]
colnames(household)=household[1,]
household <- household[-1,]


#get person info (id, age, sex)
person <- person_0 %>% 
  mutate(cluster_id = 1, age = 2013-as.numeric(`Year of`), participant_wt = 1,
         participant_id = as.numeric(`Number of`),
         sex = ifelse(`Gender:`==1, "Male", ifelse(`Gender:`==2,"Female", `Gender:`))) %>% 
  rename(household_id = `Unique number`) %>% 
  {.[-c(1:15),-c(2:4)]}

#get relevant trip data
##label for variables

trip_mode <- data.frame(TD_Mode_Used = 1:13, trip_mode = c("walk", "car", "car", "train", "bus", "bus", 
                                                           "bicycle", "motorcycle", "motorcycle", "bus", 
                                                           "car", "bus", "other"))
  #original tip modes 1=walk, 2=car-driver, 3=car-passenger, 4=train, 5=bus, 6=minibus taxi, 7=bicycle, 
  #8=motorcycle driver, 9=motorcycle passenger, 10=MyCiti bus, 11=employer transport, 12=scholar trasnport, 13=othe

trip_purpose <- data.frame(TD_Trip_purpose =as.character(1:18), trip_purpose = c("return", "work", "school", "school", 
                                                                                 "school", "other", "other", "work", 
                                                                                 "other", "other", "other ", "other", 
                                                                                 "other ", "other", "other ", "other", 
                                                                                 "other ", "other"))
  #original trip purose: 1=home, 2=work, 3=school, 4=tertiary educ., 5=pick up/drop off children, 
  #6=pick up/drop off other person, 7=transfer, 8=errand at work, 9=shopping, 10=recreation, 
  #11=fuel station, 12=medicare, 13=post office/bank etc., 14=visit a person, 15=fetch water, 
  #16=tend to animals, 17=other(1), 18=other(2),

##mutate and rename trip variables 
trip <- trip_0 %>% 
  {.[-15495,]} %>% #remove the last row which is empty
  mutate(trip_duration = abs((times(Trip_end_Time)-times(Trip_start_Time))*24*60),
         Trip_no = ifelse(as.numeric(Trip_no)<0,NA,Trip_no),
         Trip_no = ifelse(is.na(Trip_no) & !is.na(TD_Trip_purpose), 5, Trip_no)) %>% 
  left_join(trip_mode) %>% 
  left_join(trip_purpose) %>% 
  rename(household_id = Unique_Input,participant_id = Person_ID, trip_id = Trip_no) %>% 
  select(household_id, participant_id,trip_id, trip_mode, trip_purpose, trip_duration)

##remove rows indicating no trips for individuals with trips
a <- trip %>% filter(is.na(trip_id))# rows with no trips
b <- trip %>% filter(!is.na(trip_id)) # rows with trips
c <- semi_join(a, b, by = c("household_id", "participant_id"))# rows without trips and have trips elsewhere
trip <- anti_join(trip, c)

##add person characteristics to trip data
trip <- left_join(trip, person, by=c("household_id", "participant_id"))

##assign participant numbers to rows with trip numbers and no participant
trip$participant_id <- ifelse(is.na(trip$participant_id)&!is.na(trip$trip_id), 10, trip$participant_id)

##remove rows with no participant number
trip <- trip[which(!is.na(trip$participant_id)),]


##add year of survey to trips
trip$year <- "2013"

write.csv(trip, "V:/Group/lambed/ITHIM-R/data/local/cape_town/cape_town_trip.csv")

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

