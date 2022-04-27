library(rpivotTable)
library(htmlwidgets)
library(tidyverse)

io <- readRDS("results/multi_city/io.rds")

trips <- list()

cities <- names(io)[!names(io) %in% 'scen_prop']

for (city in cities){

tp <- io[[city]]$trip_scen_sets %>% mutate(scenario = case_when(
  scenario == "Scenario 1" ~ "CYC_SC",
  scenario == "Scenario 2" ~ "CAR_SC",
  scenario == "Scenario 3" ~ "BUS_SC",
  scenario == "Baseline" ~ "Baseline"), city = city) %>% 
  group_by(scenario, trip_mode, city) %>% 
  summarise(ntrips = n_distinct(trip_id), total_trip_mode = sum(trip_distance))

trips[[city]] <- tp

}

trips_combined <- data.table::rbindlist(trips)

write_csv(tp, "results/multi_city/scen_distr/san_antonio_trips.csv")

tp %>% rpivotTable(
    trips_combined,
    rows = c("sex", "age_cat"),
    cols = c("trip_mode","stage_mode", "scenario"),
    aggregatorName = "Sum",
    inclusions = list( Survived = list("Yes")),
    exclusions= list(trip_mode = list("truck"), scenario = list("CYC_SC", "CAR_SC")),
    vals = "stage_distance",
    rendererName = "Table Barchart", subtotals = FALSE
  ) %>% saveWidget(file = "results/multi_city/scen_distr/san_antonio.html", selfcontained = T)
