#' ---
#' title: "Preprocessing of Bogota's travel dataset"
#' author: "Daniel"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---

#' # **Understanding phase**
#+ warning=FALSE, message=FALSE, echo=FALSE
# Loading libraries
library(foreign) 
#library(RODBC)
library(kableExtra)
library(readxl)
library(tidyverse)


#+ warning=FALSE, message=FALSE, echo=FALSE
# Cleaning workspace
rm(list = ls());gc()

# Printing options
options(scipen = 50)

#' ## Documentation
#' Documentation is located in ".../Colombia/Bogota/Encuesta de Movilidad 2019/".
#' These files were sent by the team from Mobility Secretariat of Bogota, but
#' can also be found here: https://www.simur.gov.co/portal-simur/datos-del-sector/encuestas-de-movilidad/.
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("File1", "File2", "File3", "File4"),
  Description = c("Technical report and final results",
                  "Report with only final results",
                  "Pictures of trip modes",
                  "Questionnaire - Trip section"),
  Title = c("Caracterizacion de la movilidad - Encuesta de Movilidad de Bogota 2019",
            "Resultados de la encuesta de movilidad de Bogota y municipios vecinos",
            "Tarjeta 4 Medios de Transporte",
            "Formulario de viaje"),
  File = c("Informes de Indicadores/200109_Etapa_V_v3_Caracterización de la Movilidad.pdf",
           "Informes de Indicadores/Anexo H - Cartilla digital_Resulados de la Encuesta de Movilidad 2019.pdf",
           "Formularios/190219_Tarjetas 4-6_DPR_.pdf",
           "Formularios/190220_Módulo D_DPR (viajes).pdf")
) %>% kbl() %>% kable_classic()

#' ## Definition of a trip
#' 1. *Trip:* All trips that are longer than 3 minutes for all modes except
#' walking, where trips should be longer or equal to 15 minutes. 
#' Definition of trip in page 120 of **File2**: *Movement from one part to another made by one person with a specific reason/motive, A definite hour of start and end, a mode of transport, and a duration greater than 3 minutes. Or a movement from one part to another with reason/motive work or study of any duration.*
#' 
#' 2. *Collection:* Trips collected in this survey correspond to those made the 
#' day of reference, i.e., the day before the survey. **Results presented are** 
#' **for trips made in a single day.**
#' 
#' 
#' ## Replicate main results from raw datasets
#' To create this report I have to set the full route of each file, regardless
#' the location of the working directory.
#'
#' Loading standardize_modes function:
#+ warning=FALSE, message=FALSE
#external_route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/WorldBank/ITHIM-R/code/producing_trips_rd/"
#external_route <- "CAMBRIDGE_ROUTE V DRIVE"
#source(paste0(external_route, "used_functions.R")) 

#' It didn't work with knitr so I had to paste this function here. 
#' 
#' **Note: Before running this script, make sure this function is up to date**
standardize_modes <- function(trip, mode){
  # Read lookup table
  smodes <- read_csv('C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/global/modes/standardized_modes.csv')
  # Separate rows 
  smodes <- smodes %>% separate_rows(original, sep = ';')
  
  smodes <- smodes %>% 
    mutate(across(where(is.character), str_trim))
  
  if (length(mode) == 1) {
    if (mode == 'stage')
      trip$stage_mode <- smodes$exhaustive_list[match(trip$stage_mode, smodes$original)]
    else
      trip$trip_mode <- smodes$exhaustive_list[match(trip$trip_mode, smodes$original)]
  }else if (length(mode) == 2) {
    if (all(mode %in% c('stage', 'trip'))) {
      trip$trip_mode <- smodes$exhaustive_list[match(trip$trip_mode, smodes$original)]
      trip$stage_mode <- smodes$exhaustive_list[match(trip$stage_mode, smodes$original)]
    }
  }
  
  return(trip)
  
}

#' ### Importing datasets
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Colombia/Bogota/Encuesta de Movilidad 2019/BD EODH2019 FINAL v14022020/Archivos XLSX/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# Households (hh)
hh <- read_excel(paste0(route, "HogaresEODH2019.xlsx"), guess_max = 100000) 
# People
people <- read_excel(paste0(route, "PersonasEODH2019.xlsx"), guess_max = 100000)
# Vehicles
#vehicles <- read_excel(paste0(route, "VehículosEODH2019.xlsx"))
# Trips
trips <- read_excel(paste0(route, "ViajesEODH2019.xlsx"), guess_max = 100000)

#+ warning=FALSE, message=FALSE, cache=TRUE
# Stages: there's information about walk_to_pt stages
stages <- read_excel(paste0(route, "EtapasEODH2019.xlsx"), guess_max = 100000)


#' ### Number of rows
#' The first thing to do is verify that the number of rows of each dataset is 
#' the same to what is mentioned in page 201 (Tabla 6.1) of **File1**.
nrow(hh)
nrow(people)
nrow(trips)
nrow(stages)

#' In all of them is the same as reported, then we can continue.
#' 
#' ### Number of surveys per utam
#' As before, I'll verify that the number of surveys per UTAM is the same to
#' what is mentioned in page 20 (Tabla 2.1) of **File1**. The SQL code for this
#' output is in page 214 (Paragraph 6.26).
#View(hogar %>% group_by(Utam) %>% summarise(dplyr::n()))
#print(hh %>% group_by(Utam) %>% summarise(dplyr::n()), n = 50) 
hh %>% group_by(Utam) %>% summarise(dplyr::n()) %>% 
  kbl() %>% kable_classic(full_width = F)
#' Results are the same.
#' 
#' ### Average number of people per household by socio-economic strata (using weights)
#' Compare this with what is mentioned in page 32 (Figura 3.4) of **File1**. 
#' The SQL code for this output is in page 215 (Paragraph 6.32).
# First compute number of people per household
people_per_hh <- hh %>% 
  inner_join(people, by = c("Id_Hogar" = "id_hogar")) %>% 
  group_by(Id_Hogar) %>% summarise(cantidad = dplyr::n())

# Then compute the average by strata
hh %>% inner_join(people_per_hh, by = "Id_Hogar") %>% 
  filter(municipio == "11001") %>%
  rowwise() %>% 
  mutate(sumoff_exp_h = cantidad*Factor) %>% 
  group_by(p5_estrato) %>% 
  summarise(sum(sumoff_exp_h)/sum(Factor)) %>% 
  kbl() %>% kable_classic(full_width = F)

#' Results are the same.
#' 
#' ### Number of trips per municipality in the whole dataset
#' When talking about trips, the results presented in the report correspond to
#' those that are longer than 3 minutes for all modes except walking, where
#' trips should be longer or equal to 15 minutes.
#' 
#' To get the duration of each trip I used a file that has this already computed,
#' as well, as the main mode. The computation of the duration is explained in
#' page 202 (paragraphs 6.44 to 6.47). Now, regarding the main mode, in the
#' documentation says that **they defined a hierarchy between modes to define ** 
#' **the main mode of each trip**. This hierarchy is in page 126, table 4.4 of
#' **File1**.

# Reading file with trip duration and main mode
duration <- read_excel(paste0(route, "Aux_DuraciónEODH2019.xlsx"))

# Filtering out walking trips with duration shorter than 15 minutes.
duration_longer_15 <- duration %>% 
  filter(modo_principal != "A pie" | 
           (modo_principal == "A pie" & duracion >= 15))

# Computing number of trips per municipality
n_trips_larger_15 <- duration_longer_15 %>% 
    inner_join(hh[,c("municipio", "Id_Hogar")], 
               by = c("id_hogar" = "Id_Hogar")) %>% 
    #filter(p4_edad <= 103) %>% 
    group_by(municipio) %>% 
    summarise(suma = sum(f_exp)) %>% 
    arrange(desc(suma)) %>% 
  add_row(municipio = 99999, suma = sum(duration$f_exp))

n_trips_larger_15 %>% kbl() %>% kable_classic(full_width = F)

#' Comparing this table to *Tabla 4.2* (page 71 of **File1**), all municipalities
#' but Bogota have the same number of trips. I tried several filters to see 
#' whether there is something that I'm missing in Bogota, like trying to come up
#' with an upper bound of trip duration. At the end I got that using a limit of 
#' 540 minutes leads to a difference of just 1 trip in the total. The problem is
#' that the number of trips in other municipalities changes, which is not right.
#' 
#' Next, I'm comparing the total number of trips without the 15 minutes filter to
#' see if the reason for this inconvenience is the filter.
n_trips <- duration %>% 
  inner_join(hh[,c("municipio", "Id_Hogar")], 
             by = c("id_hogar" = "Id_Hogar")) %>% 
  group_by(municipio) %>% 
  summarise(suma = sum(f_exp)) %>% 
  arrange(desc(suma)) %>% 
  add_row(municipio = 99999, suma = sum(duration$f_exp))

n_trips %>% kbl() %>% kable_classic(full_width = F)

#' These values are exactly the same as mentioned in paragraph 4.5 for Bogota and
#' the grand total, so there must be an error in the filter (which I doubt) or in
#' the sampling weights.
#' 
#' Since I couldn't figure out a way to replicate Bogota's results, I decided to 
#' continue using only this filter of walking trips longer than 15 minutes.
#' 
#' ### Mode share in all municipalities (longer than 15 minutes)
#' Compare this with what is mentioned in page 113 (Figura 4.40) of **File1**.
#' Here trips should be larger than 15 minutes for walking.
#' The SQL code for this output is in page 203 (Paragraph 6.47).

# Compute absolute frequency
mode_share_longer_15 <- duration_longer_15 %>% group_by(modo_principal) %>% 
  summarise(suma = sum(f_exp)) 

# Compute proportion
mode_share_longer_15$proportion <- mode_share_longer_15$suma / 
  sum(duration_longer_15$f_exp) * 100

mode_share_longer_15 %>% arrange(desc(proportion)) %>% 
  kbl() %>% kable_classic(full_width = F)
#' Results are similar but not the same.
#' 
#' ### Mode share in all municipalities (all trips)
mode_share <- duration %>% group_by(modo_principal) %>% 
  summarise(suma = sum(f_exp)) 

# Compute proportion
mode_share$proportion <- mode_share$suma / 
  sum(duration$f_exp) * 100
mode_share %>% arrange(desc(proportion)) %>% 
  kbl() %>% kable_classic(full_width = F)
#' Results are the same.
#' 
#'  **To Do**: Check in other cities the definition of a trip. Here I can use
#' the dataset filtering walking trips longer than 15 minutes or use the complete
#' dataset ignoring this filter.
#' 
#'
#' # **Preprocessing phase**
#' ## Filtering Bogota trips only
#' Since the survey was conducted in 29 municipalities and we are only interested
#' in Bogota, then these trips are the only ones used. 
#' **Note**: I am using here all trips regardless of walking trips shorter than 
#' 15 minutes. 
#' 
#' **To Do**: If we choose to work with only walking trips longer than 15 
#' minutes, then I'll have to run this again
duration_bogota <- duration %>% 
  inner_join(hh[,c("municipio", "Id_Hogar")], 
             by = c("id_hogar" = "Id_Hogar")) %>% 
  filter(municipio == "11001")

duration_bogota_longer_15 <- duration_bogota %>% 
  filter(modo_principal != "A pie" | 
           (modo_principal == "A pie" & duracion >= 15))

stages_bogota <- stages %>% 
  inner_join(hh[,c("municipio", "Id_Hogar")], 
             by = c("id_hogar" = "Id_Hogar")) %>% 
  filter(municipio == "11001")

trips_bogota <- trips %>% 
  inner_join(hh[,c("municipio", "Id_Hogar")], 
             by = c("id_hogar" = "Id_Hogar")) %>% 
  filter(municipio == "11001")

people_bogota <- people %>% 
  inner_join(hh[,c("municipio", "Id_Hogar")], 
             by = c("id_hogar" = "Id_Hogar")) %>% 
  filter(municipio == "11001")

#' ## Classification and translation of trip modes and purpose
#' As it was mentioned before, there is already a table with all trip modes
#' collected in the survey in **File1** (page 111, Tabla 4.4). So I copied it 
#' and pasted it in an excel file to make the classification and translation. 
#' This is the result:
main_mode <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Colombia/Hierarchy.xlsx", sheet = "Bogota")
main_mode %>% kbl() %>% kable_classic()

#' The first two columns of this table have been defined in the documentation,
#' so I created the new classification (in Spanish) and its equivalence to Ithim,
#' taking into account travel modes defined in standardized_modes file (.../ITHIM-R/data/global/modes/standardized_modes.csv).
#' 
#' **Note**: Definition and pictures of modes are in **File3**.
#' 
#' Now with respect to trip purpose, I only had to translate them. This is the
#' result:
purpose <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Colombia/Hierarchy.xlsx", sheet = "Bogota_purpose")
purpose %>% kbl() %>% kable_classic()

#' The first two columns have been taken from the dataset and the questionnaire,
#' and the third column is the translation and classification of these motives. 
#' Last two columns correspond to what Lambed defined in 2015.
#' 
#'
#' ## Information at stage or trip level?
#' There is information at stage level although it seems that is not enough 
#' because of the duration of each stage. 
#' To understand how the information was collected at stage level, I selected a
#' person at random and followed the questionnaire (see **File4**) while checking
#' the information in the datasets. The person selected has ID_Hogar = 1117 and 
#' ID_persona = 2.
#' 
#' In *trips* and *duration* datasets there's information about the start and end
#' hour of the trip, and trip_mode (following the hierarchy mentioned). In 
#' *duration* datasets there's information about stage_mode and minutes walking
#' to the stage_mode, not the duration in the stage_mode. For instance, the first
#' trip made by this person says:
trips_bogota %>% filter(id_hogar == "1117", id_persona == "2", 
                        id_viaje == "1") %>% 
  dplyr::select(id_hogar, id_persona, id_viaje, hora_inicio_viaje, p30_camino_minutos,
         p31_hora_llegada) %>% 
  mutate(hora_inicio_viaje = format(ISOdatetime(1900,1,1,0,0,0, tz = "GMT") + 
                                      as.difftime(hora_inicio_viaje*24, 
                                                  unit = "hours"), "%H:%M"),
         p31_hora_llegada = format(ISOdatetime(1900,1,1,0,0,0, tz = "GMT") + 
                                      as.difftime(p31_hora_llegada*24, 
                                                  unit = "hours"), "%H:%M"),) %>%
  kbl() %>% kable_classic()

stages_bogota %>% filter(id_hogar == "1117", id_persona == "2", 
                        id_viaje == "1") %>% 
  dplyr::select(id_hogar, id_persona, id_viaje, id_etapa, p18_id_medio_transporte,
         p19_camino_minutos, p21_tiempo_arrancar_vehic) %>% 
  kbl() %>% kable_classic()
  
#' From both datasets I can see that the person started her trip at 6:30, 
#' then she had to walk for 5 minutes, to take the Bus 
#' (p18_id_medio_transporte = 4) where she had to wait 20 min. for it. Then she 
#' walked 5 minutes to take the Transmilenio where she had to wait 12 min. for it
#' , when she arrived she had to walk 15 minutes to get to her destination, at 
#' 7:30. 
#' 
#' It's clear that the number of minutes in Bus and Transmilenio is not there, 
#' even though there's an estimation of time waiting for the bus to come. I did
#' try to assume that variable *p21_tiempo_arrancar_vehic* could be the time
#' spent in the mode, so the total number of minutes was 5+20+5+21+15 = 63
#' minutes (close to computed duration). However, the following example 
#' contradicts this assumption.
#' 
#' In this example, the person selected has ID_Hogar = 12801 and ID_persona = 2.
trips_bogota %>% filter(id_hogar == "12801", id_persona == "2", 
                        id_viaje == "1") %>% 
  dplyr::select(id_hogar, id_persona, id_viaje, hora_inicio_viaje, p30_camino_minutos,
         p31_hora_llegada) %>% 
  mutate(hora_inicio_viaje = format(ISOdatetime(1900,1,1,0,0,0, tz = "GMT") + 
                                      as.difftime(hora_inicio_viaje*24, 
                                                  unit = "hours"), "%H:%M"),
         p31_hora_llegada = format(ISOdatetime(1900,1,1,0,0,0, tz = "GMT") + 
                                     as.difftime(p31_hora_llegada*24, 
                                                 unit = "hours"), "%H:%M"),) %>%
  kbl() %>% kable_classic()

stages_bogota %>% filter(id_hogar == "12801", id_persona == "2", 
                         id_viaje == "1") %>% 
  dplyr::select(id_hogar, id_persona, id_viaje, id_etapa,
                p18_id_medio_transporte,
                p19_camino_minutos, p21_tiempo_arrancar_vehic) %>% 
  kbl() %>% kable_classic()

#' In this case, the person started her trip at 11:00, then she had to walk for 
#' 5 minutes,to take the Transmilenio, where she had to wait 5 minutes for it.
#' Then she took another Transmilenio, waiting 8 minutes for it. Finally she had
#' to walk 8 minutes to get to her destination, at 13:00. 
#' 
#' The total number of minutes here is 5+5+8+8=26 minutes and the time spent in
#' the trip was of 120 minutes. Hence is not possible to assign a specific
#' duration to each stage.
#' 
#' **For this reason I conclude that even though there's information at stage**
#' **level, it's not enough to get the duration for each stage. **
#' 
#' **ToDo: since the proportion of trips with more than one stage is relatively**
#' **small, we could compute the difference between the trip duration and the**
#' **time spent walking to the PT, and then split this duration equally in the**
#' **remaining modes. In this way, we used all information provided for trips**
#' **with only one stage and have a rough estimate of duration for trips with**
#' **more than one stage.**
#' 
#' Only 10% of trips have more than 1 stage.
#names(stages_bogota)
n_stages <- stages_bogota %>% count(id_hogar, id_persona, id_viaje)
table(n_stages$n, useNA = "always")
table(n_stages$n, useNA = "always") / nrow(n_stages)

#' ## Row for each trip, translate trip_mode and create duration, sex and age
#' Since trips and duration datasets have the same information, I take duration
#' as a reference because it already has computed trip duration. But first I have
#' to make sure that the number of trips in both datasets is exactly the same.
trips_bogota <- trips_bogota %>% 
  mutate(trip_id_paste = paste(id_hogar, id_persona, id_viaje, sep = "-"))
duration_bogota <- duration_bogota %>% 
  mutate(trip_id_paste = paste(id_hogar, id_persona, id_viaje, sep = "-"))

#' The number of unique trip IDs is the same in both datasets and comparing these
#' trips I got a sum of 106.403, which corresponds to the number of rows in trips
#' dataset. Therefore, I can conclude that both datasets have the same trips.
length(unique(trips_bogota$trip_id_paste)) == length(unique(duration_bogota$trip_id_paste))

sum(sort(unique(trips_bogota$trip_id_paste)) == sort(unique(duration_bogota$trip_id_paste)))
identical(sort(unique(trips_bogota$trip_id_paste)), 
          sort(unique(duration_bogota$trip_id_paste)))

#' The next step consists in joining trip and duration datasets, and create trips
#' variables. 
#' 
#' Note: in the beginning I got the trip_mode from "modo_principal" but there's
#' a problem with "informal modes" because there are multiple of them and don't
#' correspond to the same mode. There could be an informal mototaxi and an 
#' informal bus, and they are classified as informal in the survey but we need
#' them to be motorycle and bus respectively. Thus, I will compute trip_mode
#' using the hierarchy at stage level.
trips_bogota_v2 <- trips_bogota %>% 
  left_join(duration_bogota, by = c("id_hogar", "id_persona", "id_viaje")) %>% 
  mutate(trip_id = id_viaje,
         trip_duration = duracion,
         # trip_mode = main_mode$ITHIM[
         #   match(modo_principal.y, main_mode$TripMode)],
         trip_purpose = purpose$ITHIM[
           match(p17_Id_motivo_viaje, purpose$Code)])

#' Now each stage need to be in a single row. The original stage dataset has 
#' walking stages in the same row as the regular stages, so I need to create one
#' for each walking stage.
#' 
#' As I said before, most of the trips consists on only one stage (without 
#' counting walking stages), and the processing is different when there's only
#' one, than when there's more than one. So I have to merge trip information and
#' then filter those trips with only one stage and more than one in different
#' datasets.
#' 
#' Before I do that I need to translate stage mode, merge information about the 
#' number of stages per trip, and merge the walking duration at the end of the
#' trip.Then I have to make sure that the total walking duration is less that the
#' total duration of the trip.

stages_bogota_v2 <- stages_bogota %>% 
  # Translate stage modes and merge mode hierarchy
  mutate(
    household_id = id_hogar,
    participant_id = id_persona,
    trip_id = id_viaje,
    stage_id = id_etapa,
    trip_id_paste = paste0(household_id, participant_id, trip_id),
    stage_id_paste = paste0(household_id, participant_id, trip_id, 
                            stage_id),
    stage_mode = main_mode$ITHIM[
    match(p18_id_medio_transporte, main_mode$IDMode)],
    hierarchy = main_mode$Hierarchy[
      match(p18_id_medio_transporte, main_mode$IDMode)]) %>% 
  # Merge number of stages per trip
  left_join(n_stages, by = c("id_hogar", "id_persona", "id_viaje")) %>% 
  # Merge trip duration and walking duration at the end of the trip
  left_join(trips_bogota_v2[, c("id_hogar", "id_persona", "id_viaje",
                                "trip_duration", "p30_camino_minutos",
                                "trip_purpose")], 
            by = c("id_hogar", "id_persona", "id_viaje"))
  
#' Now I'm going to compute stage duration. The processing is different in trips
#' with only one main stage (i.e. without counting walking stages) and with more
#' than one. In the first case, I will subtract the walking duration from the 
#' trip duration and assign this value to the stage. In the second case, I will
#' subtract the walking duration from the trip duration and then this value will
#' be split equally in the number of stages so every stage will have the same 
#' duration. This is solution is proposed under the assumption that other stages
#' are not as important as walking stages, so the estimation of duration is not 
#' sensible for the analysis.
#' 
#' **Trips with one stage:** 
stages_bogota_v2_1 <- stages_bogota_v2 %>% filter(n == 1) %>% 
  # Compute walking duration
  mutate(walking_duration = ifelse(is.na(p19_camino_minutos) & 
                                     is.na(p30_camino_minutos), 0,
                                   ifelse(is.na(p19_camino_minutos) & 
                                            !is.na(p30_camino_minutos), 
                                          p30_camino_minutos,
                                          ifelse(!is.na(p19_camino_minutos) & 
                                                   is.na(p30_camino_minutos),
                                                 p19_camino_minutos,
                                                 p19_camino_minutos +
                                                   p30_camino_minutos))),
         # Create a variable to see which trips need adjustment because the
         # walking duration is equal to or larger than trip duration
         need_adjustment = ifelse(p18_id_medio_transporte != 40 &
                                    walking_duration >= trip_duration, 1, 0),
          stage_duration = ifelse(p18_id_medio_transporte != 40,
            ifelse(need_adjustment == 0, trip_duration - walking_duration,
                                 trip_duration), trip_duration),
          # Since all these trips has only one stage, without counting walking
          # stages, then the trip mode will be the same as stage_mode
          trip_mode = stage_mode)

#View(stages_bogota_v2_1)
#length(unique(stages_bogota_v2_1$stage_id_paste))
#table(stages_bogota_v2_1$stage_mode, stages_bogota_v2_1$need_adjustment)
#table(stages_bogota_v2_1$need_adjustment)

#' In only 1% of trips the walking duration is the same or larger than the trip
#' duration. Since this proportion is small, then I will assume that
#' these trips didn't have the walking component
#sum(is.na(stages_bogota_v2$walking_duration))
table(stages_bogota_v2_1$need_adjustment, useNA = "always") / nrow(stages_bogota_v2_1)
#View(stages_bogota_v2_1[stages_bogota_v2_1$need_adjustment == 1, ])
#table(stages_bogota_v2_1[stages_bogota_v2_1$need_adjustment == 1, "stage_mode"])


#' In trips when the adjustment is needed or in walking stages, there will be
#' a single stage, meaning, no walking component for these other modes
stages_bogota_v2_1_adjust <- stages_bogota_v2_1 %>% 
  filter(need_adjustment == 1 | stage_mode == "walk") %>% 
  dplyr::select(household_id, participant_id, trip_id, trip_mode, 
                trip_duration, trip_purpose, stage_id, stage_mode,
                stage_duration, stage_id_paste, trip_id_paste)

#View(stages_bogota_v2_1_adjust)
#length(unique(stages_bogota_v2_1_adjust$stage_id_paste))
#nrow(stages_bogota_v2_1_adjust)

#' In trips when the adjustment is NOT needed, there will be multiple stages,
#' adding walking stages
stages_bogota_v2_1_noadjust <- stages_bogota_v2_1 %>% 
  filter(need_adjustment == 0 & stage_mode != "walk") %>% 
  # I use pivot_longer the put walking stages in a new row
  pivot_longer(c("p19_camino_minutos", "stage_duration",
                 "p30_camino_minutos"), names_to = "variable",
               values_to = "stage_duration") %>% 
  # Assign "walk_to_pt" to walking stages
  mutate(stage_mode = ifelse(variable %in% c("p19_camino_minutos",
                                             "p30_camino_minutos"), 
                             "walk_to_pt", stage_mode)) %>% 
  # Filter out walking stages without duration
  filter(!is.na(stage_duration) & stage_duration > 0) %>% 
  # Reassign stage_id
  # from https://stackoverflow.com/questions/54581440/r-count-consecutive-occurrences-of-values-in-a-single-column-and-by-group
  group_by(id_hogar, id_persona, id_viaje, 
           grp = with(rle(stage_id_paste), rep(seq_along(lengths), lengths))) %>%
  mutate(stage_id = seq_along(grp)) %>% 
  ungroup() %>% 
  dplyr::select(household_id, participant_id, trip_id, trip_mode, 
                trip_duration,
         trip_purpose, stage_id, stage_mode, stage_duration, stage_id_paste, 
         trip_id_paste)
                                      
# length(unique(stages_bogota_v2_1_noadjust$stage_id_paste))
# View(stages_bogota_v2_1_noadjust)
# sum(is.na(stages_bogota_v2_1_noadjust$stage_duration))

#' Append both datasets into one with trips with only one stage.
#' 
#' In this dataset *stage_id_paste* let me know the original stage_id so I can
#' trace back the original trip if I wanted to.
# names(stages_bogota_v2_1_adjust)
# names(stages_bogota_v2_1_noadjust)
stages_bogota_v2_1_ready <- stages_bogota_v2_1_adjust %>% 
  bind_rows(stages_bogota_v2_1_noadjust)

#View(stages_bogota_v2_1_ready)
#table(stages_bogota_v2_1_ready$stage_mode)

#' **Trips with more than one stage** 
#' 
#' In this dataset *stage_id_paste* and *trip_id_paste* let me know the original
#' stage_id and trip_id so I can trace back the original trip if I wanted to.
stages_bogota_v2_2 <- stages_bogota_v2 %>% filter(n > 1) %>% 
  mutate(
    # To avoid double counting the last walking stage, I set it up to zero
    p30_camino_minutos_ok = ifelse(id_etapa != n, 
                                        0, p30_camino_minutos)) %>% 
  # I compute the walking duration and the beginning and end of a stage, and also
  # the trip main mode.
  group_by(id_hogar, id_persona, id_viaje) %>% 
  mutate(p19_camino_minutos_sum = sum(p19_camino_minutos, na.rm = T),
         p30_camino_minutos_sum = sum(p30_camino_minutos_ok, na.rm = T),
         # I compute total walking duration
         walking_duration = p19_camino_minutos_sum + p30_camino_minutos_sum,
         stage_duration = (trip_duration - walking_duration) / n,
         # trip main mode
         #main_mode = min(hierarchy),
         trip_mode = main_mode$ITHIM[
           match(min(hierarchy), main_mode$Hierarchy)]) %>% 
  ungroup() %>% 
  # I use pivot_longer the put walking stages in a new row
  pivot_longer(c("p19_camino_minutos", "stage_duration",
                 "p30_camino_minutos_ok"), names_to = "variable",
               values_to = "stage_duration") %>% 
  # Assign "walk_to_pt" to walking stages
  mutate(stage_mode = ifelse(variable %in% c("p19_camino_minutos",
                                             "p30_camino_minutos_ok"), 
                             "walk_to_pt", stage_mode)) %>% 
  # Filter out walking stages without duration
  filter(!is.na(stage_duration) & stage_duration > 0) %>% 
  # Reassign stage_id
  # from https://stackoverflow.com/questions/54581440/r-count-consecutive-occurrences-of-values-in-a-single-column-and-by-group
  # group_by(id_hogar, id_persona, 
  #          grp = with(rle(id_viaje), rep(seq_along(lengths), lengths))) %>%
  group_by(trip_id_paste) %>%
  mutate(stage_id = seq_len(dplyr::n()),
         #stage_id2 = seq_len(dplyr::n()),
         # Recreate stage_id_paste
         stage_id_paste = paste0(household_id, participant_id, trip_id, 
                                 stage_id)#,
         #stage_id_paste2 = paste0(household_id, participant_id, trip_id, 
        #                         stage_id2)
        ) %>% 
  ungroup() %>% 
  dplyr::select(household_id, participant_id, trip_id, trip_mode, 
                trip_duration,
         trip_purpose, stage_id, stage_mode, stage_duration, stage_id_paste, 
         trip_id_paste#, stage_id_paste2, stage_id2
         )
         

# length(unique(stages_bogota_v2_2$trip_id_paste))
# length(unique(stages_bogota_v2_2$stage_id_paste))
# length(unique(stages_bogota_v2_2$stage_id_paste2))
#unique(stages_bogota_v2_2$id_etapa)
#sum(is.na(stages_bogota_v2_2$stage_duration))
#View(stages_bogota_v2_2)

#' I merge all stages again to create a unique dataset
stages_bogota_ready <- stages_bogota_v2_1_ready %>% 
  bind_rows(stages_bogota_v2_2) %>% 
  arrange(household_id, participant_id, trip_id, stage_id) 

#View(stages_bogota_ready)
#' Checking the number of stages before and after the preprocessing. There are
#' 8 stages that don't appear anymore although the number of trips is the same.
length(unique(stages_bogota_v2_1_ready$stage_id_paste)) + 
  length(unique(stages_bogota_v2_2$stage_id_paste)) 
length(unique(stages_bogota_v2$stage_id_paste))
length(unique(stages_bogota_ready$stage_id_paste))

length(unique(stages_bogota_v2_1_ready$trip_id_paste)) + 
  length(unique(stages_bogota_v2_2$trip_id_paste)) 
length(unique(stages_bogota_v2$trip_id_paste))
length(unique(stages_bogota_ready$trip_id_paste))


#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*.
report <- people_bogota %>% 
left_join(stages_bogota_ready, by = c("id_hogar" = "household_id", 
                                      "id_persona" = "participant_id")) %>% 
  mutate(cluster_id = 1,
         household_id = id_hogar,
         participant_id = id_persona,
         #trip_id_paste = paste(household_id, participant_id, trip_id, sep = "-"),
         #stage_id_paste = paste(household_id, participant_id, trip_id, stage_id,
         #                      sep = "-"),
         age = p4_edad,
         sex = ifelse(Sexo == "Hombre", "Male", "Female"),
         participant_wt = f_exp,
         meta_data = NA) %>% 
  dplyr::select(cluster_id, household_id, participant_id, sex, age,
                participant_wt,
         trip_id, trip_mode, trip_duration, trip_purpose,
         stage_id, stage_mode, stage_duration, stage_id_paste, 
         trip_id_paste) #%>% 
  #arrange(cluster_id, household_id, participant_id, trip_id)

report$meta_data[1] <- 9135800
report$meta_data[2] <- 17497 
report$meta_data[3] <- "Travel Survey"
report$meta_data[4] <- 2019
report$meta_data[5] <- "1 day"
report$meta_data[6] <- "Yes, but no stage duration" #Stage level data available
report$meta_data[7] <- "All purpose"#Overall trip purpose
report$meta_data[8] <- "Yes, but not here (yet)" # Short walks to PT
report$meta_data[9] <- "No" # Distance available
report$meta_data[10] <- "Truck, van" # missing modes#' 

#' I verify that every trip has the sex and age of the person who did it. Since
#' the sum of NAs is zero, then I can conclude that every trip has the
#' information.
sum(is.na(report$sex))
sum(is.na(report$age))
sum(is.na(report$trip_duration))
sum(is.na(report$trip_mode))

#' Export dataset to make the report
write_csv(report, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/bogota/bogota_trips.csv')

#' ## Standardize trip modes
#' There's already a function that standardize these modes so the package can use
#' these trips. I made sure to use translate trip modes so that the function
#' works perfectly (take a look at the *original* variable of *smodes* dataframe
#' in this function).

#trips_export <- standardize_modes(duration_bogota, mode = c('trip'))
trips_export <- standardize_modes(report, mode = c('trip', 'stage'))
table(report$trip_mode, useNA = "always")
table(trips_export$trip_mode, useNA = "always")
table(report$stage_mode, useNA = "always")
table(trips_export$stage_mode, useNA = "always")

#' *standardize_modes* function transforms walk to pedestrian, van to car, 
#' bicycle to cycle, train to rail, and rickshaw to auto_rickshaw.

# Checking the number of missing values
sapply(trips_export, function(x) sum(is.na(x)))

#' ## Creating again IDs
trips_export <- trips_export %>% mutate(
  participant_id2 = participant_id,
  trip_id2 = trip_id,
  participant_id = as.integer(as.factor(paste(cluster_id, household_id,
                                               participant_id, sep = "_"))),
  trip_id = as.integer(as.factor(paste(cluster_id, household_id,
                                        participant_id, trip_id, sep = "_"))),
  trip_id = ifelse(is.na(trip_mode), NA, trip_id))

sapply(trips_export, function(x) sum(is.na(x)))

#' # **Exporting phase**
#' ## Variables to export
#' Now I filter the columns I need
trips_export <- trips_export %>% 
  select(participant_id, age, sex, trip_id, trip_mode, trip_duration, 
         stage_id, stage_mode, stage_duration)

#' ## Export dataset
#' I'm exporting this dataset to two different locations:
#' 
#' 1. In .../inst/exdata folder so the dataset is installed with the package
#' 2. In Data/Colombia/Bogota/Cleaned, to have a copy 
#' 
write_csv(trips_export, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/inst/extdata/local/bogota/trips_bogota.csv')
write_csv(trips_export, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Colombia/Bogota/Cleaned/trips_bogota.csv')



