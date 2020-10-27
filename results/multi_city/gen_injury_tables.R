############ Load libraries and datasets
require(tidyverse)
require(flextable)
whw <- read_csv("results/multi_city/whw_matrices/whw_lng.csv")
injury_risks_per_100k <- read_csv("results/multi_city/whw_matrices/injury_risks_per_100k_pop.csv")
injury_risks_per_billion_kms <- read_csv('results/multi_city/whw_matrices/injury_risks_per_billion_kms_lng.csv')

# Read lookup table
smodes <- read_csv('data/global/modes/standardized_modes.csv')

get_summary_table <- function(mode = 'strike', scen = scen, mode_name){
  if (mode == 'strike'){
    
    temp1 <- whw %>% filter(scenario == scen & str_mode == mode_name) %>% spread(value = value, key = cas_mode)
    
    temp1 <- temp1[,c(1, 2, 3, na.omit(match(smodes$exhaustive_list, colnames(temp1))))]
    
    return(temp1 %>% mutate_if(is.numeric, .funs = funs(case_when( . < 1 ~ round(., 2) ,  . >= 1 ~  as.numeric(round(.))))))
    
  }else if (mode == 'casualty') {
    
    temp1 <- whw %>% filter(scenario == scen & cas_mode == mode_name) %>% spread(value = value, key = str_mode)
    
    temp1 <- temp1[,c(1, 2, 3, na.omit(match(smodes$exhaustive_list, colnames(temp1))))]
    
    return(temp1 <- temp1 %>% mutate_if(is.numeric, .funs = funs(case_when( . < 1 ~ round(., 2) ,  . >= 1 ~  as.numeric(round(.))))))
    
    
  }
}

get_summary_table_injury_risk <- function(obj = injury_risks_per_100k, mode = 'city', var = var){
  if (mode == 'scen'){
    td <- obj %>% filter(scenario == var) %>% spread(value = value, key = mode)
    return(td[,c(1, 2, na.omit(match(smodes$exhaustive_list, colnames(td))))] %>% 
             mutate_if(is.numeric, .funs = funs(case_when( . < 1 ~ round(., 2) ,  . >= 1 ~  as.numeric(round(.))))))
  }else if (mode == 'city'){
    td <- obj %>% filter(city == var) %>% spread(value = value, key = mode)
    return(td[,c(1, 2, na.omit(match(smodes$exhaustive_list, colnames(td))))] %>% 
             mutate_if(is.numeric, .funs = funs(case_when( . < 1 ~ round(., 2) ,  . >= 1 ~  as.numeric(round(.))))))
    
    #  %>%  mutate_if(is.numeric, ~as.character(.))
  }
  
  
}

############ Define scenario
scen_names <- unique(whw$scenario)
scen <- 'Baseline'
temp <- whw %>% filter(scenario == scen) %>% dplyr::select(cas_mode)
cas_mode <- unique(temp$cas_mode)
cm <- cas_mode[1]
cm <- 'pedestrian'

temp <- whw %>% filter(scenario == scen) %>% dplyr::select(str_mode)
str_mode <- unique(temp$str_mode)
sm <- str_mode[1]
sm <- 'unknown'

cn <- 'sao_paulo'

whw_cas <- get_summary_table(mode = 'casualty', scen = scen, mode_name = cm ) %>% janitor::adorn_totals(where = c('row', 'col'))
View(whw_cas)
whw_str <- get_summary_table(mode = 'strike', scen = scen, mode_name = sm ) %>% janitor::adorn_totals(where = c('row', 'col'))
View(whw_str)

# ft <- flextable()
# ft <- add_header_row(ft, values = c(' ', 'strike mode'), colwidths = c(3, ncol(temp1) - 3))
# ft <- merge_v(ft, j = 1:3, part = 'body')
# 
# ft

##################

injury_risks_per_100k_by_scen <- get_summary_table_injury_risk(injury_risks_per_100k, mode = 'scen', var = scen)  %>% janitor::adorn_totals(where = c('row', 'col'))
View(injury_risks_per_100k_by_scen)
injury_risks_per_100k_by_city <- get_summary_table_injury_risk(injury_risks_per_100k, mode = 'city', var = cn)  %>% janitor::adorn_totals(where = c('row', 'col'))
View(injury_risks_per_100k_by_city)

injury_risks_per_billion_kms_by_scen <- get_summary_table_injury_risk(injury_risks_per_billion_kms, mode = 'scen', var = scen)  %>% janitor::adorn_totals(where = c('row', 'col'))
View(injury_risks_per_billion_kms_by_scen)
injury_risks_per_billion_kms_by_city <- get_summary_table_injury_risk(injury_risks_per_billion_kms, mode = 'city', var = cn)  %>% janitor::adorn_totals(where = c('row', 'col'))
View(injury_risks_per_billion_kms_by_city)
