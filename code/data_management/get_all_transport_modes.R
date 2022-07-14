io <- io <- readRDS("results/multi_city/io.rds")

cities <- names(io)[!names(io) %in% 'scen_prop']

trav_modes <- list()
pm_modes <- list()

for (city in cities){
  
  trav_modes <- union(trav_modes, io[[city]]$trip_scen_sets$stage_mode |> unique()) |> as.character()
  
  pm_modes <- union(pm_modes, io$antofagasta$vehicle_inventory |> 
    filter(!is.na(PM_emission_inventory) & PM_emission_inventory != 0) |> 
    dplyr::select(PM_emission_inventory))
}

print(trav_modes)

