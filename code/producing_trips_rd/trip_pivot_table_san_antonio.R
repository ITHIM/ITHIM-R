library(rpivotTable)
library(htmlwidgets)
library(tidyverse)

io <- readRDS("results/multi_city/io.rds")

city <- 'buenos_aires'

tp <- io[[city]]$trip_scen_sets %>% mutate(scenario = case_when(
  scenario == "Scenario 1" ~ "CYC_SC",
  scenario == "Scenario 2" ~ "CAR_SC",
  scenario == "Scenario 3" ~ "BUS_SC",
  scenario == "Baseline" ~ "Baseline")) 


cities <- names(io)[!names(io) %in% 'scen_prop']

co2_pc <- list()

for (city in cities){
  co2_pc[[city]] <- (colSums(io[[city]]$outcomes$co2_conc, na.rm = T) / sum(io[[city]]$demographic$population)) %>% 
    as.data.frame() %>% tibble::rownames_to_column() %>% 
    set_names("scenario", "val") %>% mutate(city = city)  %>% mutate(diff_base = 1 - (nth(val, 4) / val))
  
}

co2_pc <- data.table::rbindlist(co2_pc) %>% mutate(scenario = case_when(
  scenario == "Scenario 1" ~ "CYC_SC",
  scenario == "Scenario 2" ~ "CAR_SC",
  scenario == "Scenario 3" ~ "BUS_SC",
  scenario == "Baseline" ~ "Baseline")) 

write_csv(tp, "results/multi_city/scen_distr/san_antonio_trips.csv")

tp %>% rpivotTable(
    rows = c("age_cat", "sex", "trip_distance_cat"),
    cols = c("trip_mode","scenario"),
    aggregatorName = "Count Unique Values",
    inclusions = list( Survived = list("Yes")),
    exclusions= list(trip_mode = list("truck"), scenario = list("CYC_SC", "CAR_SC")),
    vals = "trip_id",
    rendererName = "Table Barchart", subtotals = FALSE
  ) %>% saveWidget(file = "results/multi_city/scen_distr/san_antonio.html", selfcontained = T)
