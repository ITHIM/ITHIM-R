#' ---
#' title: "Preprocessing of Osorno's injuries dataset"
#' author: "Daniel"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---

#' Note: in order to create the markdown properly from this file, Rstudio options need to be changed. Tools > Global Options > R Markdown > Evaluate chunks in directory > Current. By default this option is set to *Document*, but here you need to change it to *Current* so everything works as expected.
#' 
#' 
#' # **Preprocessing phase**
#+ warning=FALSE, message=FALSE, echo=FALSE
# Loading libraries
library(readxl)
library(tidyverse)

#+ warning=FALSE, message=FALSE, echo=FALSE
# Cleaning workspace
rm(list = ls());gc()

# Printing options
options(scipen = 50)

#' ## Importing datasets
#' I ran everything local because it is faster, but if someone wants to run this
#' script, then only the path needs to be changed.
# V-Drive folder
#path <- "V:/Studies/MOVED/HealthImpact/Data/Country/Chile/Injuries"
# Local folder
path <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Chile/Injuries/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# Injuries
# injuries <- read_excel(paste0(path, "Usuarios Participantes 2015-2019.xlsx"),
#                        sheet = "2015-2019", guess_max = 100000)
#' Since all warnings are related with column 16, "numero", and I'm not going
#' to use, then I ignore them.
#'
#' I took "Tipo" variable for the whole country and translated it, so this sheet
#' Works for every city in Chile
main_mode <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Chile/Hierarchy.xlsx", sheet = "Injuries")
#main_mode %>% kbl() %>% kable_classic()

#' I import this file to standardize modes
# Read lookup table
smodes <- read_csv('data/global/modes/standardized_modes.csv')
# Separate rows 
smodes <- smodes %>% separate_rows(original, sep = ';')
# Trim
smodes <- smodes %>% 
  mutate(across(where(is.character), str_trim))


#' 
#' ## Filtering injuries from the Comunas that appear in the travel survey.
#' According to the travel survey (page 2-1 of **Actualizacion_Plan_Transporte_Osorno_Inf_Final.pdf**
#' the survey cover only comuna Osorno
# unique(injuries$Región)
# sort(unique(injuries$Comuna))
# injuries <- injuries %>% filter(Comuna == "OSORNO")
# write_csv(injuries, paste0(path, "injuries_osorno.csv"))

injuries <- read_csv(paste0(path, "injuries_osorno.csv"))

#' Selecting the variables I need
#' 
#' Note: Servicio variable allows to identify taxi and other kinds of modes. 
#' Even though it's available, I don't use it here.
injuries_v2 <- injuries %>% 
  dplyr::select("Año", "IdAccidente", "Tipo Accidente", "Tipo (CONASET)", 
                "Causa (CONASET)", "Calidad", "Sexo", "Edad", "Fallecidos", "Graves",
                "Menos Graves", "Leves", "Tipo")

#' Explorando el dataset
length(unique(injuries_v2$IdAccidente))
table(injuries_v2$`Tipo Accidente`, 
      injuries_v2$`Tipo (CONASET)`)
unique(injuries_v2$Tipo)
sum(is.na(injuries_v2$Tipo))
unique(injuries_v2$`Causa (CONASET)`)
#' It looks like it's better to work with "Tipo CONASET" because it has less
#' categories.

#' Creating important variables
injuries_v2 <- injuries_v2 %>% 
  # Create an id for each row corresponding to the number of people involved in
  # an accident.
  group_by(IdAccidente,
           grp = with(rle(IdAccidente), rep(seq_along(lengths), lengths))) %>%
  mutate(participant_id = seq_along(grp),
         mode_type = main_mode$ITHIM[match(Tipo, main_mode$Mode)],
         hierarchy = main_mode$Hierarchy[match(Tipo, main_mode$Mode)],
         event_id = IdAccidente,
         year = Año) %>% 
  ungroup()

#' Filtering trips with fatalities
#sum(injuries_v2$Fallecidos, na.rm = T)
#length(which(injuries_v2$Fallecidos == 1))
# Find accidents ids with fatalities
fatalities_ids <- unique(injuries_v2$IdAccidente[which(injuries_v2$Fallecidos == 1)])
fatalities <- injuries_v2[injuries_v2$IdAccidente %in% fatalities_ids,]
#sum(fatalities$Fallecidos) # OK
#unique(fatalities$Sexo)

#' Since the processing for casualty's variables is different from strike ones, 
#' I do them in different datasets
casualties <- fatalities %>% filter(Fallecidos == 1)
strike <- fatalities %>% filter(Fallecidos == 0)

#' Create variables for casualties
casualties <- casualties %>% 
  mutate(cas_age = Edad,
         cas_gender = ifelse(Sexo == "MASCULINO", "Male", "Female"),
         cas_mode = mode_type)

#' Create variables for strike mode. For this I use the hierarchy when there's
#' more than 1 vehicle per accident
strike <- strike %>% 
  group_by(IdAccidente) %>% 
  mutate(min_mode = min(hierarchy),
         strike_mode = main_mode$ITHIM[match(min_mode, main_mode$Hierarchy)]) %>%
  ungroup()

#' Since I already identified the strike mode, I can delete duplicates
strike_v2 <- strike[!duplicated(strike$IdAccidente), ]

#' Merge strike_mode to casualties
casualties_V2 <- casualties %>% 
  left_join(strike_v2[,c("IdAccidente", "strike_mode")], by = "IdAccidente") %>% 
  # If there strike_mode is missing, this means that the accident didn't involve
  # other vehicle. So I assign them "NOV"
  mutate(strike_mode = ifelse(is.na(strike_mode), "NOV", strike_mode),
         weight = 5) # weight is 5 because these injures are from 2015-2019

#' Filter some columns and standardize cas and strike modes
whw <- casualties_V2 %>% 
  dplyr::select(event_id, year, cas_gender, cas_age, cas_mode, strike_mode)
# , weight) # Weight and year shouldn't be together

whw$cas_mode <- smodes$exhaustive_list[match(tolower(whw$cas_mode),
                                             smodes$original)]
whw$strike_mode <- smodes$exhaustive_list[match(tolower(whw$strike_mode),
                                                smodes$original)]

unique(whw$cas_mode) %in% smodes$exhaustive_list
unique(whw$strike_mode) %in% smodes$exhaustive_list

# Export file
#write_csv(whw, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Chile/Osorno/Cleaned/injuries_osorno_wb.csv')
write_csv(whw, 'inst/extdata/local/osorno/injuries_osorno.csv')
