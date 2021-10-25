#' ---
#' title: "Preprocessing of Montevideo's injuries dataset"
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
library(mice)

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
path <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/Uruguay/Montevideo/Injuries/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# Injuries
injuries <- read_excel(
  paste0(path, "Registro de incidentes viales 2018 2020 (1).xlsx"),
  sheet = "Personas involucradas", guess_max = 100000)

#' *Filtering only accidents with fatalities
#' I idenfify the accident ID of fatalities to filter those rows as well as those
#' involved in the accidents.
names(injuries)
unique(injuries$`Tipo de resultado`)
table(injuries$`Tipo de resultado`)
id_fatalities <- injuries %>% 
  filter(`Tipo de resultado` %in% c("FALLECIDO EN EL LUGAR", 
                                    "FALLECIDO EN CENTRO DE ASISTENCIA")) 
id_fatalities <- pull(id_fatalities, Novedad)  # There are 300 fatalities
length(unique(id_fatalities)) # There are in total 284 accidents with fatalities

#' Filtering fatalities
fatalities <- injuries %>% filter(Novedad %in% id_fatalities)
length(unique(fatalities$Novedad)) # Same 284 accidents
table(fatalities$`Tipo de resultado`) # Same 300 fatalities

#' I took " Vehiculo Víctima" variable and translated it
main_mode <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Uruguay/Hierarchy.xlsx", sheet = "Montevideo_injuries")
#main_mode %>% kbl() %>% kable_classic()

#' I import this file to standardize modes
# Read lookup table
smodes <- read_csv('data/global/modes/standardized_modes.csv')
# Separate rows 
smodes <- smodes %>% separate_rows(original, sep = ';')
# Trim
smodes <- smodes %>% 
  mutate(across(where(is.character), str_trim))

#' Selecting the variables I need and creating new ones
fatalities_v2 <- fatalities %>% 
  dplyr::select("Fecha", "Novedad", "Tipo de resultado", "Tipo de siniestro", 
                "Sexo", "Edad", "Vehiculo Víctima") %>% 
  mutate(
    event_id = Novedad,
    year = (as.numeric(format(Fecha, "%Y"))),
    mode_type = main_mode$ITHIM[match(`Vehiculo Víctima`, main_mode$Mode)],
    hierarchy = main_mode$Hierarchy[match(`Vehiculo Víctima`, main_mode$Mode)],
    Edad = as.numeric(ifelse(Edad == "SIN DATOS", NA, Edad)))

#' Since the processing for casualty's variables is different from strike ones, 
#' I do them in different datasets
casualties <- fatalities_v2 %>% 
  filter(`Tipo de resultado` %in% c("FALLECIDO EN EL LUGAR", 
                                    "FALLECIDO EN CENTRO DE ASISTENCIA"))
strike <- fatalities_v2 %>% 
  filter(!`Tipo de resultado` %in% c("FALLECIDO EN EL LUGAR", 
                                     "FALLECIDO EN CENTRO DE ASISTENCIA"))

#' Create variables for casualties
casualties <- casualties %>% 
  mutate(cas_age = Edad,
         cas_gender = ifelse(Sexo == "MASCULINO", "Male",
                             ifelse(Sexo == "FEMENINO", "Female", NA)),
         cas_mode = mode_type)

#' Create variables for strike mode. For this I use the hierarchy when there's
#' more than 1 vehicle per accident
strike <- strike %>% 
  group_by(event_id) %>% 
  mutate(min_mode = min(hierarchy),
         strike_mode = main_mode$ITHIM[match(min_mode, main_mode$Hierarchy)]) %>%
  ungroup()

#' Since I already identified the strike mode, I can delete duplicates
strike_v2 <- strike[!duplicated(strike$event_id), ]

#' Merge strike_mode to casualties
casualties_V2 <- casualties %>% 
  left_join(strike_v2[,c("event_id", "strike_mode")], by = "event_id") %>% 
  # If the strike_mode is missing, this means that the accident didn't involve
  # other vehicle. So I assign them "NOV"
  mutate(strike_mode = ifelse(is.na(strike_mode), "NOV", strike_mode),
         weight = 3) # weight is 3 because these injures are from 2018-2020

#' Filter some columns and standardize cas and strike modes
whw <- casualties_V2 %>% 
  dplyr::select(event_id, year, cas_gender, cas_age, cas_mode, strike_mode) %>% 
  #, weight) %>% # Weight and year shouldn't be together
  mutate(cas_gender = factor(cas_gender),
         cas_mode = factor(cas_mode),
         strike_mode = factor(strike_mode)) %>% 
  arrange(event_id)

#' Multiple imputation using mice
md.pattern(whw)
# Imputation using mice. I imputed the dataset 5 times (this is why is multiple
# imputation). The idea is to make a sensitivity analysis in the results.
imp1 <- mice(whw[,c("year","cas_gender", "cas_age", "cas_mode", "strike_mode")],
             m = 5, seed = 12345)
imp1

whw <- whw %>% rename(cas_age_original = cas_age,
                      cas_gender_original = cas_gender) %>% 
  bind_cols(complete(imp1) %>% dplyr::select(cas_age, cas_gender))

whw$cas_mode <- smodes$exhaustive_list[match(tolower(whw$cas_mode),
                                             smodes$original)]
whw$strike_mode <- smodes$exhaustive_list[match(tolower(whw$strike_mode),
                                                smodes$original)]

unique(whw$cas_mode) %in% smodes$exhaustive_list
unique(whw$strike_mode) %in% smodes$exhaustive_list

# Export file
#write_csv(whw, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Uruguay/Montevideo/Cleaned/injuries_montevideo_wb.csv')
write_csv(whw, 'inst/extdata/local/montevideo/injuries_montevideo.csv')
