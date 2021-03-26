#' ---
#' title: "Preprocessing of Sao Paulo's travel dataset"
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
#' Documentation is located in ".../Brazil/SaoPaulo/Trips/".
#' These files were downloaded from https://transparencia.metrosp.com.br/dataset/pesquisa-origem-e-destino/resource/989b86c9-5d4b-4686-b584-1eceb10a62cd.
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("File1", "File2"),
  Description = c("Interviewer instruction manual",
                  "Final report with results"),
  Title = c("Manual de Intrucoes aos pesquisadores",
            "Pesquisa Origem Destino 2017 50 años"),
  File = c("Trips/Banco de Dados/Manuais/Manual_Pesquisa_Domiciliar-Treinamento_1.pdf",
           "Trips/Pesquisa Origem e Destino 2017.pdf")
  ) %>% kbl() %>% kable_classic()

#' ## Definition of a trip
#' 1. *Trip:* *Movement from one part to another made by one person with a specific reason/motive, using one or more modes of transport* (Page 30, *File1*).
#' **Walking trips are considered when: 1) the motive of the trip is work or study regardless the distance, 2) distance larger than 500m for remaining motives** (Page 24, *File2*).
#' 
#' 2. *Collection:* Trips collected in this survey correspond to those made the 
#' day of reference, i.e., the day before the survey (4:00am yesterday to 3:59am
#' today) (Page 30, *File1*). **Results presented are** 
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
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Brazil/SaoPaulo/Trips/Banco de Dados/Banco de dados/"

#' All the information is at trip level, so there's only one dataset
#+ warning=FALSE, message=FALSE, cache=TRUE
# Trips
trips <- read.dbf(paste0(route, "OD_2017.dbf"))
#write.csv(trips, paste0(route, "OD_2017_DG.csv"))

#' ### Number of people and trips
#' The first thing to do is verify that the number of people and trips is the
#' same to what is mentioned in page 26 and 27 of **File2**.
#' 
#' According to the file "Corresp2007_2017.xlsx", where there's a correspondence
#' between municipalities codes from survey of 2007 and 2017, the zones that
#' belong to each subregion are (sheet "Resumo"):
#' 1) Central: between 1 and 342
#' 2) Norte: between 343 and 361
#' 3) Nordeste: between 362 and 390
#' 4) Leste: between 391 and 416
#' 5) Sudeste: between 417 and 459
#' 6) Sudoeste: between 460 and 475
#' 7) Oeste: between 476 and 517
#' Then I create this variable to replicate results

# Population
trips <- trips %>% 
  mutate(subregion = case_when(
    ZONA >= 1 & ZONA <= 342 ~ "Central",
    ZONA >= 343 & ZONA <= 361 ~ "Norte",
    ZONA >= 362 & ZONA <= 390 ~ "Nordeste",
    ZONA >= 391 & ZONA <= 416 ~ "Leste",
    ZONA >= 417 & ZONA <= 459 ~ "Sudeste",
    ZONA >= 460 & ZONA <= 475 ~ "Sudoeste",
    ZONA >= 476 & ZONA <= 517 ~ "Oeste",
    TRUE ~ "Other"
  ))
#View(table(trips$ZONA, trips$subregion))

# Number of people
people <- trips %>% filter(F_PESS == 1) 
sum(people$FE_PESS)
people %>% group_by(subregion) %>% summarise(suma = sum(FE_PESS)) 

# Number of trips
sum(trips$FE_VIA, na.rm = T)

#' Both results are the same as reported, then we can continue. I also compared
#' these results with "Tabela 2" and "Tabela 16" from 
#' "Tabelas Gerais/Dados Gerais OD2017.xlsx" and they're the same.

#' ### Trips results
#' Check that the number of trips by type is the same to what is mentioned in
#' page 39 **File2**.
#' 
#' First I have to create variables that classify each mode into bigger groups
#' like colective or individual mode
trips <- trips %>% 
  mutate(modo_colectivo = ifelse(MODOPRIN %in% c(1:8), 1, 0),
         modo_individual = ifelse(MODOPRIN %in% c(9:14,17), 1, 0),
         motorizado = ifelse(modo_colectivo == 1 | modo_individual == 1, 1, 0),
         nao_motorizado = ifelse(MODOPRIN %in% c(15,16), 1, 0))

sum(trips[trips$modo_colectivo == 1, "FE_VIA"], na.rm = T) #OK
sum(trips[trips$modo_individual == 1, "FE_VIA"], na.rm = T) #OK
sum(trips[trips$motorizado == 1, "FE_VIA"], na.rm = T) #OK
sum(trips[trips$nao_motorizado == 1, "FE_VIA"], na.rm = T) #OK
sum(trips[trips$MODOPRIN == 15, "FE_VIA"], na.rm = T) #OK
sum(trips[trips$MODOPRIN == 16, "FE_VIA"], na.rm = T) #OK

#' All results are the same as reported. I also compared
#' these results with "Tabela 17" from "Tabelas Gerais/Dados Gerais OD2017.xlsx"
#' and they're the same.
#' 
#' 
#' # **Preprocessing phase**
#' ## Filtering Sao Paulo trips only
#' Since the survey was conducted in 39 municipalities and we are only interested
#' in Sao Paulo, then these trips are the only ones used. 
#' 
#' According to the documentation subregion "Central" corresponds to the
#' municipality of Sao Paulo, which is the city.
trips_saopaulo <- trips %>% filter(subregion == "Central") %>% 
  # Create variables that I'll need later
  mutate(cluster_id = 1,
         age = IDADE,
         sex = ifelse(SEXO == 1, "Male", "Female"),
         participant_wt = FE_PESS)

#' ## Classification and translation of trip modes and purpose
#' In the documentation there is already a table with the hierarchy they used
#' to define the main mode (page 24, **File2**). So I copied it and pasted it
#' in an excel file to make the classification and translation. 
#' This is the result:
main_mode <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Brazil/Hierarchy.xlsx", sheet = "SaoPaulo")
main_mode %>% kbl() %>% kable_classic()

#' The first two columns of this table have been defined in the documentation,
#' so I created the new classification (in Portuguese) and its equivalence to
#' Ithim, taking into account travel modes defined in standardized_modes file (.../ITHIM-R/data/global/modes/standardized_modes.csv).
#' 
#' 
#' Now with respect to trip purpose, I only had to translate them. This is the
#' result:
purpose <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Brazil/Hierarchy.xlsx", sheet = "SaoPaulo_purpose")
purpose %>% kbl() %>% kable_classic()

#' The first two columns have been taken from the data dictionary,
#' and the third column is the translation and classification of these motives. 
#' 
#'
#' ## Information at stage or trip level?
#' There is information at stage level although it seems that is not enough 
#' because of the duration of each stage. 
#' 
#' **Since the proportion of trips with more than one stage is relatively**
#' **small, we could compute the difference between the trip duration and the**
#' **time spent walking to the PT, and then split this duration equally in the**
#' **remaining modes. In this way, we used all information provided for trips**
#' **with only one stage and have a rough estimate of duration for trips with**
#' **more than one stage.**
#' 
#' Only 10% of trips have more than 1 stage.
table(trips_saopaulo$MODO1, useNA = "always")
table(trips_saopaulo$MODO1, useNA = "always") / nrow(trips_saopaulo)
table(trips_saopaulo$MODO2, useNA = "always")
table(trips_saopaulo$MODO2, useNA = "always") / nrow(trips_saopaulo)
table(trips_saopaulo$MODO3, useNA = "always")
table(trips_saopaulo$MODO3, useNA = "always") / nrow(trips_saopaulo)
table(trips_saopaulo$MODO4, useNA = "always")
table(trips_saopaulo$MODO4, useNA = "always") / nrow(trips_saopaulo)

#' ## Row for each trip, translate trip_mode and create duration, sex and age
#' First I filter out people that don't have trip information
no_trips_saopaulo <- trips_saopaulo %>% 
  filter(is.na(MODO1) & is.na(MODO2) & is.na(MODO3) & is.na(MODO4)) 

#' Then for those that have trips, I create new variables
trips_saopaulo_v2 <- setdiff(trips_saopaulo, no_trips_saopaulo) %>%
  mutate(household_id = ID_FAM,
         participant_id = ID_PESS,
         trip_id = N_VIAG,
         trip_id_paste = paste0(household_id, participant_id, trip_id),
         # Translate trip mode
         trip_mode = main_mode$ITHIM[
           match(MODOPRIN, main_mode$IDMode)],
         # Translate trip purpose
         trip_purpose = purpose$ITHIM[
           match(MOTIVO_O, purpose$Code)],
         # Create trip duration
         trip_duration = DURACAO,
         # Compute walking duration
         walking_duration = ifelse(is.na(ANDA_O) & is.na(ANDA_D), 0,
                                   ifelse(is.na(ANDA_O) & !is.na(ANDA_D), ANDA_D,
                                          ifelse(!is.na(ANDA_O) & 
                                                   is.na(ANDA_D), ANDA_O,
                                                 ANDA_O + ANDA_D))),
         # Create a variable to see which trips need adjustment because the
         # walking duration is equal to or larger than trip duration
         need_adjustment = ifelse(walking_duration >= trip_duration, 1, 0)) %>% 
  rowwise() %>% mutate(
         # Number of stages
         n_stages = 4 - sum(is.na(MODO1), is.na(MODO2), is.na(MODO3), 
                            is.na(MODO4)),
         # Compute stage duration, splitting equally the remaining
         stage_duration_computed = ifelse(need_adjustment == 0,
                                 (trip_duration - walking_duration) / n_stages,
                                 trip_duration),
         # In trips where walking duration is the same as the trips duration,
         # I assume that there were not walking stages. This only happens in 
         # 9 trips
         ANDA_O = ifelse(need_adjustment == 0, ANDA_O, NA),
         ANDA_D = ifelse(need_adjustment == 0, ANDA_D, NA)) %>% 
  # Create a row for each stage
  pivot_longer(c("ANDA_O", "MODO1", "MODO2", "MODO3", "MODO4", "ANDA_D"),
               names_to = "variable",
               values_to = "stage_duration") %>% 
  # Create new variables at stage level
  mutate(
    # From pivot longer, *stage_duration* has the mode for all stages but 
    # walking. In walking cases, stage duration is indeed duration
    stage_mode = ifelse(variable %in% c("ANDA_O", "ANDA_D"), "walk_to_pt",
                        main_mode$ITHIM[
                          match(stage_duration, main_mode$IDMode)]),
    stage_duration = ifelse(variable %in% c("ANDA_O", "ANDA_D"), 
                            stage_duration, stage_duration_computed)) %>% 
  # Filter out stages without duration nor mode
  filter(!is.na(stage_duration) & !is.na(stage_mode)) %>% 
  # Create stage_id
  # from https://stackoverflow.com/questions/54581440/r-count-consecutive-occurrences-of-values-in-a-single-column-and-by-group
  group_by(household_id, participant_id, 
           grp = with(rle(trip_id_paste), rep(seq_along(lengths), lengths))) %>%
    mutate(stage_id = seq_along(grp)) %>% 
    ungroup() %>% 
    select(cluster_id, household_id, participant_id, age, sex, participant_wt,
         trip_id, trip_mode, trip_duration, trip_purpose, stage_id, stage_mode,
         stage_duration, trip_id_paste)
         
#' Select some variables in no_trip dataset
no_trips_saopaulo_v2 <- no_trips_saopaulo %>% 
  mutate(household_id = ID_FAM,
         participant_id = ID_PESS,
         trip_id = NA, trip_mode = NA, trip_duration = NA, trip_purpose = NA,
         stage_id = NA, stage_mode = NA, stage_duration = NA, 
         trip_id_paste = NA) %>% 
  select(cluster_id, household_id, participant_id, age, sex, participant_wt,
         trip_id, trip_mode, trip_duration, trip_purpose, stage_id, stage_mode,
         stage_duration, trip_id_paste)

trips_saopaulo_ready <- trips_saopaulo_v2 %>% 
  bind_rows(no_trips_saopaulo_v2) %>%  
  arrange(household_id, participant_id, trip_id, stage_id)
  
#' Checking the number of people before and after the preprocessing. 
length(unique(trips_saopaulo_ready$participant_id))
length(unique(trips_saopaulo$ID_PESS)) 

#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*.
report <- trips_saopaulo_ready %>%  
  mutate(meta_data = NA) %>% 
  select(cluster_id, household_id, participant_id, sex, age, participant_wt,
         trip_id, trip_mode, trip_duration, trip_purpose,
         stage_id, stage_mode, stage_duration, trip_id_paste) #%>% 
  #arrange(cluster_id, household_id, participant_id, trip_id)

report$meta_data <- NA
report$meta_data[1] <- 20847500
report$meta_data[2] <- 20650 
report$meta_data[3] <- "Travel Survey"
report$meta_data[4] <- 2012
report$meta_data[5] <- "1 day"
report$meta_data[6] <- "Yes" #Stage level data available
report$meta_data[7] <- "All purpose"#Overall trip purpose
report$meta_data[8] <- "Yes" # Short walks to PT
report$meta_data[9] <- "Yes" # Distance available
report$meta_data[10] <- "" # missing modes

#' I verify that every trip has the sex and age of the person who did it. Since
#' the sum of NAs is zero, then I can conclude that every trip has the
#' information.
sum(is.na(report$sex))
sum(is.na(report$age))
sum(is.na(report$trip_duration))
sum(is.na(report$trip_mode))

#' Export dataset to make the report
write_csv(report, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/sao_paulo/sao_paulo_trip.csv')

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
#'
#' ## Creating again IDs
trips_export <- trips_export %>% mutate(
  participant_id2 = participant_id,
  trip_id2 = trip_id,
  participant_id = as.integer(as.factor(paste(cluster_id, household_id,
                                               participant_id, sep = "_"))),
  trip_id = as.integer(as.factor(paste(cluster_id, household_id,
                                        participant_id, trip_id, sep = "_"))))

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
write_csv(trips_export, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/inst/extdata/local/sao_paulo/trips_sao_paulo.csv')
write_csv(trips_export, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Brazil/SaoPaulo/Cleaned/trips_sao_paulo.csv')



