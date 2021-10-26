#' ---
#' title: "Script to get the WHW matrices for each injury dataset"
#' author: "Daniel"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---
#' 

#' *Loading libraries*
library(dplyr)
library(readr)

#+ warning=FALSE, message=FALSE, echo=FALSE
# Cleaning workspace
rm(list = ls());gc()

# Printing options
options(scipen = 50)


#' *Definition of functions*
#' Sourcing package functions
#source("R/assign_age_groups.R")
#source("R/set_injury_contingency.R")

get_whw <- function(injuries, population = NULL, ithim = F,
                    min_age = 15, max_age = 69) {
  # Same preprocessing as in the package (ithim_load_data)
  #filename <- paste0(local_path,"/injuries_",CITY,".csv")
  #injuries <- read_csv(filename,col_types = cols())
  
  # Transform to lowercase all strings
  injuries$cas_mode <- tolower(injuries$cas_mode)
  injuries$strike_mode <- tolower(injuries$strike_mode)
  if('cas_gender'%in%colnames(injuries)) injuries$cas_gender <- tolower(injuries$cas_gender)
  #injuries$strike_mode[is.na(injuries$strike_mode)] <- 'listed_na'
  nov_words <- c('no.other.fixed.or.stationary.object','no other vehicle','none')
  injuries$strike_mode[injuries$strike_mode%in%nov_words] <- 'nov'
  ## add weight column if missing
  if(!'weight'%in%colnames(injuries)) injuries$weight <- 1
  
  # Similar preprocessing as in the package (ithim_load_data), i.e., filter age
  # age range and label incidents as "nov" when strike and cas mode are the same
  if(ithim){
    # Filter only injuries in the age range
    if('cas_age'%in%colnames(injuries)) {
      injuries <- injuries %>% filter(cas_age >= min_age & cas_age <= max_age)
      if (!is.null(population)) {
        pop <- population %>% 
          rowwise() %>% 
          mutate(age_lb = unlist(strsplit(age, "-"))[1], # lower bound
                 age_ub = unlist(strsplit(age, "-"))[2] # upper bound
          ) %>% 
          filter(age_lb >= min_age & age_ub <= max_age)
      } else {
        pop <- population
      } # End if poplation
    } # End if injuries
    
    ## AA - Hard-coded
    ## INJURIES - Make all incidents of car, bus, motorcycle and cycle with themselves, as NOV
    ## 25-02-2020
    
    # Get all injuries with same casualty and strike mode for car, bus, motorcycle and cycle
    # Treat bus_driver same as bus for strike mode
    same_cas_str_modes <- injuries %>% 
      filter((cas_mode == 'car' & strike_mode == 'car') |
               (cas_mode == 'bus' & (strike_mode %in% c('bus', 'bus_driver'))) |
               (cas_mode == 'motorcycle' & strike_mode == 'motorcycle') |
               (cas_mode == 'cycle' & strike_mode == 'cycle'))
    
    # Filter all those with similar casualty and strike mode
    injuries <- injuries %>% 
      filter(!((cas_mode == 'car' & strike_mode == 'car') |
                 (cas_mode == 'bus' & (strike_mode %in% c('bus','bus_driver'))) |
                 (cas_mode == 'motorcycle' & strike_mode == 'motorcycle') |
                 (cas_mode == 'cycle' & strike_mode == 'cycle')))
    
    # Mutate strike mode as NOV
    same_cas_str_modes <- same_cas_str_modes %>% mutate(strike_mode = 'nov')
    
    # Re-add with NOV
    injuries <- plyr::rbind.fill(injuries, same_cas_str_modes)
  } # End if ithim
  
  # Call function to set tables for WHW and NOV
  #set_injury_contingency(injuries)
  
  # Compute WHW 
  whw <- injuries %>% group_by(strike_mode, cas_mode) %>% 
    summarise(value = dplyr::n())
  
  # Normalize the frequency by the number of years in the data
  if ("year" %in% colnames(injuries)) {
    whw$value_1year <- whw$value / length(unique(injuries$year))
    whw$weight <- length(unique(injuries$year))
    # Compute WHW by year
    whw_year <- injuries %>% group_by(strike_mode, cas_mode, year) %>% 
      summarise(value = dplyr::n())
  } else {
    whw$value_1year <- whw$value / mean(injuries$weight)
    whw$weight <- mean(injuries$weight)
    whw_year <- NULL
  }
  # Normalize the frequency by 100k population
  if (!is.null(population)) {
    whw$value_100k <- whw$value / sum(pop$population) * 100000
    whw$value_1year_100k <- whw$value_1year / sum(pop$population) * 100000
    whw$population <- sum(pop$population)
    if ("year" %in% colnames(injuries)) {
      whw_year$value_100k <- whw_year$value / sum(pop$population) * 100000
    }
  }
  return(list(whw = whw, whw_year = whw_year))
} # End whw function

#' *WHW Antofagasta*
# route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/"
# injuries <- read_csv(paste0(route, "Chile/Antofagasta/Cleaned/injuries_antofagasta_wb.csv"))
# whw <- get_whw(injuries, pop)
# whw2 <- get_whw(injuries, pop, ithim = T)


#' *All cities*
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2021/Cambridge/Data/"
files <- data.frame(
  city = c(
    "Accra", "Antofagasta", "Arica", "Bangalore", "Belo_Horizonte",
    "Bogota", "Buenos_Aires", "Cali", "Cape_Town",
    "Copiapo", "Coquimbo_LaSerena", "Delhi", "Iquique_AltoHospicio","Medellin",
    "Mexico_City", "Montevideo", "Osorno", "Puerto_Montt",
    "San_Antonio", "Santiago", "Sao_Paulo", "Temuco", "Valdivia", 
    "Valparaiso", "Vizag"),
  filename = c(
    "inst/extdata/local/accra/",
    "inst/extdata/local/antofagasta/",
    "inst/extdata/local/arica/",
    "inst/extdata/local/bangalore/",
    "inst/extdata/local/belo_horizonte/",
    "inst/extdata/local/bogota/",
    "inst/extdata/local/buenos_aires/",
    "inst/extdata/local/cali/",
    "inst/extdata/local/cape_town/",
    "inst/extdata/local/copiapo/",
    "inst/extdata/local/coquimbo_laserena/",
    "inst/extdata/local/delhi/",
    "inst/extdata/local/iquique_altohospicio/",
    "inst/extdata/local/medellin/",
    "inst/extdata/local/mexico_city/",
    "inst/extdata/local/montevideo/",
    "inst/extdata/local/osorno/",
    "inst/extdata/local/puerto_montt/",
    "inst/extdata/local/san_antonio/",
    "inst/extdata/local/santiago/",
    "inst/extdata/local/sao_paulo/",
    "inst/extdata/local/temuco_padrelascasas/",
    "inst/extdata/local/valdivia/",
    "inst/extdata/local/gran_valparaiso/",
    "inst/extdata/local/vizag/"
  ), # End filename
  injuries = c(
    "injuries_accra.csv",
    "injuries_antofagasta.csv",
    "injuries_arica.csv",
    "injuries_bangalore.csv",
    "injuries_belo_horizonte.csv",
    "injuries_bogota.csv",
    "injuries_buenos_aires.csv",
    "injuries_cali.csv",
    "injuries_cape_town.csv",
    "injuries_copiapo.csv",
    "injuries_coquimbo_laserena.csv",
    "injuries_delhi.csv",
    "injuries_iquique_altohospicio.csv",
    "injuries_medellin.csv",
    "injuries_mexico_city.csv",
    "injuries_montevideo.csv",
    "injuries_osorno.csv",
    "injuries_puerto_montt.csv",
    "injuries_san_antonio.csv",
    "injuries_santiago.csv",
    "injuries_sao_paulo.csv",
    "injuries_temuco_padrelascasas.csv",
    "injuries_valdivia.csv",
    "injuries_gran_valparaiso.csv",
    "injuries_vizag.csv"
  ), # End injuries
  population = c(
    "population_accra.csv",
    "population_antofagasta.csv",
    "population_arica.csv",
    "population_bangalore.csv",
    "population_belo_horizonte.csv",
    "population_bogota.csv",
    "population_buenos_aires.csv",
    "population_cali.csv",
    "population_cape_town.csv",
    "population_copiapo.csv",
    "population_coquimbo_laserena.csv",
    "population_delhi.csv",
    "population_iquique_altohospicio.csv",
    "population_medellin.csv",
    "population_mexico_city.csv",
    "population_montevideo.csv",
    "population_osorno.csv",
    "population_puerto_montt.csv",
    "population_san_antonio.csv",
    "population_santiago.csv",
    "population_sao_paulo.csv",
    "population_temuco_padrelascasas.csv",
    "population_valdivia.csv",
    "population_gran_valparaiso.csv",
    "population_vizag.csv"
  ) # End population
) # End dataframe

#whw <-  list()
#whw_ithim <- list()
whw_df <- data.frame()
whw_df_year <- data.frame()
whw_ithim_df <- data.frame()
whw_ithim_df_year <- data.frame()
for (i in 1:nrow(files)) {
  print(files$city[i])
  injuries <- read_csv(paste0(files$filename[i], files$injuries[i]))
  pop <- read_csv(paste0(files$filename[i], files$population[i]))
  # whw[[files$city[i]]] <- get_whw(injuries) %>% mutate(city = files$city[i])
  # whw_ithim[[files$city[i]]] <- get_whw(injuries, ithim = T) %>% 
  #   mutate(city = files$city[i])
  whw_df <- whw_df %>% 
    bind_rows(get_whw(injuries, pop)$whw %>% mutate(city = files$city[i]))
  whw_ithim_df <- whw_ithim_df %>% 
    bind_rows(get_whw(injuries, pop, ithim = T)$whw %>% 
                mutate(city = files$city[i]))
  year <- get_whw(injuries, pop)$whw_year
  if (!is.null(year)) { # Check whether there's year or no
    whw_df_year <- whw_df_year %>% 
      bind_rows(year %>% mutate(city = files$city[i]))
    whw_ithim_df_year <- whw_ithim_df_year %>% 
      bind_rows(get_whw(injuries, pop, ithim = T)$whw_year %>% 
                  mutate(city = files$city[i]))
  }
  
}

write.table(whw_df,"clipboard-34567", sep = "\t", row.names = F)
write.table(whw_df_year,"clipboard-34567", sep = "\t", row.names = F)
write.table(whw_ithim_df,"clipboard-34567", sep = "\t", row.names = F)
write.table(whw_ithim_df_year,"clipboard-34567", sep = "\t", row.names = F)
