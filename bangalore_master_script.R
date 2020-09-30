# Clear workspace
rm (list = ls())


require(ithimr)
#require(tidyverse)

io <- run_ithim_setup(CITY = 'bangalore',
                      TEST_WALK_SCENARIO = T,
                      ADD_WALK_TO_BUS_TRIPS = F,
                      ADD_TRUCK_DRIVERS = F,
                      ADD_BUS_DRIVERS = F,
                      PM_TRANS_SHARE = 0.281,
                      PM_CONC_BASE = 47.4,
                      speeds = list(subway = 32,
                                    bicycle = 15),
                      PM_emission_inventory = list(motorcycle = 1409,
                                                auto_rickshaw = 133,
                                                car = 2214,
                                                bus_driver = 644,
                                                big_truck = 4624,
                                                truck = 3337,
                                                van = 0,
                                                other = 0,
                                                taxi = 0))

io$outcomes <- run_ithim(io, seed = 1)


## plot results for YLLs
result_df <- data.frame(variable = names(io$outcome$hb$ylls[,3:ncol(io$outcome$hb$ylls)]),
                              value = colSums(io$outcome$hb$ylls[,3:ncol(io$outcome$hb$ylls)]))

result_df$variable <- sapply(strsplit(as.character(result_df$variable), "_"), tail, 1)

ggplot(data = result_df) +
  aes(x = variable, y = value) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()


plotly::ggplotly(ggplot(data = reshape2::melt(io$outcomes$mmets %>% dplyr::select(base_mmet, scen1_mmet) %>% rename(baseline = base_mmet, scenario = scen1_mmet) )) +
  aes(x = variable, y = value) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
)


