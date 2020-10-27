io <- readRDS("results/multi_city/io.rds")
cities <- names(io)[!names(io) %in% 'scen_prop']

l <- list()
rd <- list()
for (city in cities){
  print(city)
  td <- io[[city]]$outcomes$pm_conc_pp
  st <- do.call(cbind, lapply(td, summary)) %>% as.data.frame() %>% dplyr::select(-c(participant_id, work_ltpa_marg_met, sex, age_cat))
  st$city <- city
  st <- tibble::rownames_to_column(st, "val")
  l[[city]] <- st
  
  lrd <- pivot_longer(st, cols = -c(val, city, age))
  
  if (length(rd) == 0)
    rd <- lrd
  else
    rd <- rbind(rd, lrd)
  
  rd$name[rd$name == 'pm_conc_base'] <- 'Baseline'
  rd$name[rd$name == 'pm_conc_scen1'] <- 'Walking'
  rd$name[rd$name == 'pm_conc_scen2'] <- 'Bicycling'
  rd$name[rd$name == 'pm_conc_scen3'] <- 'Driving'
  rd$name[rd$name == 'pm_conc_scen4'] <- 'Motorcycling'
  rd$name[rd$name == 'pm_conc_scen5'] <- 'Public Transport'
  
  rd <- rename(rd, scenario = name)
  
  # if (var == 'bl'){
  #   qualified_scen_name <- 'Baseline'
  #   scen <- ''
  # }else if(var == "w_sc"){
  #   qualified_scen_name <- 'Walking'
  # }else if(var == "bi_sc"){
  #   qualified_scen_name <- 'Bicycling'
  # }else if(var == "car_sc"){
  #   qualified_scen_name <- 'Driving'
  # }else if(var == "mc_sc"){
  #   qualified_scen_name <- 'Motorcycling'
  # }else if(var == "bus_sc"){
  #   qualified_scen_name <- 'Public Transport'
  # }
  # 
  # 
  # rd$name["pm_conc_base"  "pm_conc_scen1" "pm_conc_scen2" "pm_conc_scen3" "pm_conc_scen4" "pm_conc_scen5"
  
}

write_csv(rd, 'results/multi_city/pm2-5-conc.csv')