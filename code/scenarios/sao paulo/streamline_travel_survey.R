# Load libraries
library(tidyverse)

# Read sao paulo's travel survey
rd <- read_csv("data/scenarios/sao paulo/SP 2012 travel data.csv")

# Rename columns
rd <- rename(rd, participant_id = ID_PESS , 
             age  =  IDADE,
             sex = SEXO,
             trip_id = N_VIAG,
             total_trips = TOT_VIAG,
             walking_time_origin = ANDA_O,
             walking_time_dest = ANDA_D, 
             trip_duration = DURACAO, 
             mode = MODOPRIN,
             distance = DISTANCIA,
             row_id = ID_ORDEM
             
)