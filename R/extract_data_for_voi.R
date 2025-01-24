#' Get ITHIM-results into correct format for VoI analysis
#' 
#' This function extracts the relevant information from the multi_city_ithim object and gets the results into
#' the correct format for further analysis. 
#'
#' The function performs the following steps:
#' 
#'\itemize{
#'\item by looping through the cities:
#'
#'  \itemize{
#'    \item calculate average outcome (yll) per person in the population considered by the model 
#'    \item calculate the total ylls per 100 000 for each outcome age category, scenario and disease combination and model run
#'    \item calculate total yll outcome across all outcome age and sex categories per city and scenario and disease combinations
#'    \item calculate total yll outcome across all outcome sex categories per city and scenario and disease combinations
#'    \item create one dataframe for all cities with all outcomes for all model runs, age groups and disease and 
#'          scenario combinations
#'  } 
#'   
#'\item compute yll per hundred thousand by outcome age group by summing across all diseases (double counting!) 
#'      by city and scenario and also summing across all cities
#' 
#'\item create one dateframe with total ylls (median, 5th and 95th percentiles) per age group and city 
#'      (plus combined results as sum across all cities)
#' }  
#' 
#' @param NSCEN number of scenarios (not incl. baseline)
#' @param NSAMPLES number of model runs per city
#' @param SCEN_SHORT_NAME names of the scenarios (incl. baseline)
#' @param outcome_age_groups outcome age groups as defined as input parameters to the model 
#' @param cities list of cities for which the model was run
#' @param multi_city_ithim list containing the ithim model information including results for the various model runs
#' @param output_version the output version of the model run
#' 
#' @return ithim_results list with the following objects:
#' @return voi_data_all_df: dataframe for all cities with all outcomes for all model runs, age and sex categories and disease and scenario combinations

#' 
#' @export


extract_data_for_voi <- function(NSCEN, NSAMPLES, SCEN_SHORT_NAME,outcome_age_groups,cities,multi_city_ithim, output_version){
  
  
  # initialise dataframe for all cities with all outcomes for all model runs, age groups and disease and scenario combinations
  voi_data_all <- list()
  
  voi_data_all_df <- data.frame()
  voi_data_all_sex_df <- data.frame()
  
  age_pops <- list()
  age_populations <- rep(0,length(outcome_age_groups))
  
  # create matrix of 0s with the number of cities as rows and the length of outcome age groups as columns
  city_populations <- matrix(0,nrow=length(cities),ncol=length(outcome_age_groups))
  
  # create empty dataframe to save all the population numbers
  population_df <- data.frame()
  
  for(ci in 1:length(cities)){ # loop through cities
    city <- cities[ci]
    multi_city_ithim[[city]] <- readRDS(paste0('results/voi/',city,'_',output_version,'.Rds')) # read in city specific data
    
    DEMOGRAPHIC <- multi_city_ithim[[city]]$DEMOGRAPHIC
    
    # find the minimum and maximum ages for each age category used in the demographic information for the city
    age_pops[[city]] <- list()
    min_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
    max_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
    age_pops[[city]]$min_pop_ages <- min_pop_ages
    age_pops[[city]]$max_pop_ages <- max_pop_ages
    
    ## get outcomes
    # find min and max ages of the different age groups used in the outcome
    min_ages <- sapply(multi_city_ithim[[city]]$outcomes[[1]]$hb$ylls$age_cat,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
    max_ages <- sapply(multi_city_ithim[[city]]$outcomes[[1]]$hb$ylls$age_cat,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
    
    # define rows to keep with outcome ages at least the pre-defined minimum age but at most the pre-defined maximum age
    keep_rows <- which(min_ages>=min_age&max_ages<=max_age)
    
    # define columns to keep which are all columns except those that contain age or sex information
    keep_cols <- which(!sapply(names(multi_city_ithim[[city]]$outcomes[[1]]$hb$ylls),function(x)grepl('age|sex',as.character(x))))
    

    ### demographic stats total and by sex
    total_female <- sum(subset(DEMOGRAPHIC,min_pop_ages>=min_age&max_pop_ages<=max_age&sex=='female')$population)
    total_male <- sum(subset(DEMOGRAPHIC,min_pop_ages>=min_age&max_pop_ages<=max_age&sex=='male')$population)
    total_pop <- sum(subset(DEMOGRAPHIC,min_pop_ages>=min_age&max_pop_ages<=max_age)$population)
    
    population_df_city <- data.frame(sex = c('all','male','female'), age = c('all','all','all'),population = c(total_pop, total_male,total_female))
    population_df_city <- rbind(population_df_city, DEMOGRAPHIC)
    population_df_city$city <- city
    
    population_df <- rbind(population_df,population_df_city)
    

    # total yll outcome across all outcome age categories per city and scenario and disease combination
    outcome[[city]] <- t(sapply(multi_city_ithim[[city]]$outcomes, function(x) colSums(x$hb$ylls[keep_rows,keep_cols],na.rm=T)))
    colnames(outcome[[city]]) <- paste0(colnames(outcome[[city]]),'_',city)
    
   
    # create one dataframe for all cities with all outcomes for all model runs, age groups and disease and scenario combinations
    for(row in keep_rows){
      voi_data_all[[city]]$outcomes <- t(sapply(multi_city_ithim[[city]]$outcomes, function(x) rbind(x$hb$ylls[row,])))
      voi_dummy <- data.frame(voi_data_all[[city]])
      colnames(voi_dummy)<-colnames(multi_city_ithim[[city]]$outcomes[[1]]$hb$ylls)
      voi_dummy$city <- city
      voi_data_all_df <- rbind(voi_data_all_df, voi_dummy)
    }
    
    # create one dataframe for all cities with all outcomes for all model runs, sex and disease and scenario combinations
    
    # first for males 
    list_male <- list()
    for (i in 1:NSAMPLES){
      list_male[[i]] <- as.data.frame(multi_city_ithim[[city]]$outcomes[[i]]$hb$ylls) %>% filter(sex == 'male')
    }
    
    voi_city_male <- as.data.frame(t(sapply(list_male, function(x) colSums(x[keep_rows,keep_cols],na.rm=T))))
    voi_city_male$city <- city
    voi_city_male$sex <- 'male'
    

    # females
    list_female <- list()
    for (i in 1:NSAMPLES){
      list_female[[i]] <- as.data.frame(multi_city_ithim[[city]]$outcomes[[i]]$hb$ylls) %>% filter(sex == 'female')
    }
    
    voi_city_female <- as.data.frame(t(sapply(list_female, function(x) colSums(x[keep_rows,keep_cols],na.rm=T))))
    voi_city_female$city <- city
    voi_city_female$sex <- 'female'

    # all 
    voi_data_all_sex_df <- rbind(voi_data_all_sex_df, voi_city_male, voi_city_female)
    
    
  } # end of city loop
  
  # create an age and sex category column
  voi_data_all_df$age_sex <- paste(voi_data_all_df$sex, voi_data_all_df$age_cat, sep = )
  
  
  
  
  ################# create outcome statistics for different scenarios and health outcomes
  
  # number the different runs
  voi_data_all_df$run <- rep(1:nsamples,length(unique(voi_data_all_df$age_sex))*length(cities))
  voi_data_all_sex_df$run <- rep(1:nsamples,length(unique(voi_data_all_sex_df$sex))*length(cities))
  
  # add extra columns
  voi_data_all_sex_df$age_cat <- 'all'
  voi_data_all_sex_df$age_sex <- paste(voi_data_all_sex_df$sex, voi_data_all_sex_df$age_cat, sep = ' ')
  
  # create total for each run for all outcomes
  voi_data_total_df <- voi_data_all_sex_df %>% group_by( age_cat, run, city) %>% summarise(across(where(is.numeric), sum), .groups = 'drop')
  voi_data_total_df$sex <- 'all'
  voi_data_total_df$age_sex <- paste(voi_data_total_df$sex, voi_data_total_df$age_cat, sep = ' ')
  
  
  # create one dataset
  voi_data_all_df$sex <- as.character(voi_data_all_df$sex)
  voi_data_all_df$age_cat <- as.character(voi_data_all_df$age_cat)
  voi_data_all_df <- voi_data_all_df %>% mutate_if(is.list,as.numeric)
  voi_data_complete <- bind_rows(voi_data_all_df, voi_data_all_sex_df , voi_data_total_df)
  
  # add population data
  population_df <- population_df %>% rename(age_cat = age)
  voi_data_complete2 <- merge(voi_data_complete, population_df, by = c('age_cat','sex','city'))
  
  # calculate data for various levels
  
  # names of all scenarios excluding base
  scen_only_names <- scenario_names[2:length(scenario_names)]
  
  # different levels
  level1 <-c('pa_ap_all_cause','inj')
  level2 <- c('pa_total_cancer','pa_ap_CVD','ap_respiratory','inj')
  level3 <- c('pa_ap_IHD','pa_ap_lung_cancer','ap_COPD','pa_ap_stroke','pa_ap_T2D','ap_LRI',
              'pa_breast_cancer','pa_colon_cancer','pa_endo_cancer','pa_liver_cancer','pa_total_dementia',
              'pa_myeloma','pa_Parkinson','pa_head_neck_cancer', 'pa_stomach_cancer',
              'pa_myeloid_leukemia','inj')
  
  level_list <- list(level1, level2, level3)
  
  # loop through levels
  all_levels <- data.frame()
  l<-1
  for (level in list(level1, level2, level3)){
    dummy_level_df <- voi_data_complete2 %>% dplyr::select(matches(level))
    
    # loop trough scenarios and calculate sum
    for (scen in scen_only_names){
      dummy_level_df <- dummy_level_df %>%
        mutate(sum = rowSums(pick(matches(scen))))  %>% # calcualte sum
        rename_with(~paste0(scen, '_ylls_level',l), .cols= sum) # re-name new column
      
    }
    if (l ==1){
      all_levels <- dummy_level_df %>% dplyr::select(!matches(level))
    } else{
      all_levels <- cbind(all_levels, dummy_level_df%>% dplyr::select(!matches(level)))
    }
    l <- l +1
  }
  
  # add levels to dataframe
  voi_data_complete3 <- cbind(voi_data_complete2, all_levels)
  
  
  # add summary statistics
  voi_data_complete3_mean <- voi_data_complete3 %>% group_by(city, age_cat, sex, age_sex, population) %>% summarise(across(where(is.numeric)& !run, mean
  ), .groups = 'drop')
  voi_data_complete3_mean$value_type <- 'mean'
  
  # 2.5th percentile
  voi_data_complete3_2.5perc <- voi_data_complete3 %>% group_by(city, age_cat, sex, age_sex, population) %>% 
    summarise(across( .cols = where(is.numeric) & !run,  .fns = ~quantile(., 0.025),.names = "{col}"), .groups = 'drop')
  voi_data_complete3_2.5perc$value_type <- '2.5perc'
  
  # 97.5th percentile
  voi_data_complete3_97.5perc <- voi_data_complete3 %>% group_by(city, age_cat, sex, age_sex, population) %>% 
    summarise(across( .cols = where(is.numeric)& !run,  .fns = ~quantile(., 0.975),.names = "{col}"), .groups = 'drop')
  voi_data_complete3_97.5perc$value_type <- '97.5perc'
  
  # standard deviation
  voi_data_complete3_sd <- voi_data_complete3 %>% group_by(city, age_cat, sex, age_sex, population) %>% summarise(across(where(is.numeric)& !run,
                                                                                                                         sd), .groups = 'drop')
  voi_data_complete3_sd$value_type <- 'std'
  
  
  # combine all statistics 
  voi_data_complete3_summary <- rbind(voi_data_complete3_mean, voi_data_complete3_2.5perc,voi_data_complete3_97.5perc,voi_data_complete3_sd)
  
  # re-order columns
  voi_data_complete3_summary <- voi_data_complete3_summary %>% dplyr::select(city, age_cat, sex, age_sex, population, value_type, 
                                                                             sc_cycle_ylls_level1, sc_car_ylls_level1, sc_bus_ylls_level1,               
                                                                             sc_cycle_ylls_level2, sc_car_ylls_level2, sc_bus_ylls_level2,
                                                                             sc_cycle_ylls_level3, sc_car_ylls_level3, sc_bus_ylls_level3, everything())
  

  
  
  ## repeat for 100k
  
  # calculate per 100k
  voi_data_complete3_100k <- voi_data_complete3 %>% mutate(across(where(is.numeric) & !run & !population, ~round(as.numeric(.x)/population*100000,4)))
  
  # add summary statistics
  voi_data_complete3_100k_mean <- voi_data_complete3_100k %>% group_by(city, age_cat, sex, age_sex, population) %>% 
    summarise(across(where(is.numeric)& !run, mean), .groups = 'drop')
  voi_data_complete3_100k_mean$value_type <- 'mean'
  
  # 2.5th percentile
  voi_data_complete3_100k_2.5perc <- voi_data_complete3_100k %>% group_by(city, age_cat, sex, age_sex, population) %>%
    summarise(across( .cols = where(is.numeric) & !run,  .fns = ~quantile(., 0.025),.names = "{col}"), .groups = 'drop')
  voi_data_complete3_100k_2.5perc$value_type <- '2.5perc'
  
  # 97.5th percentile
  voi_data_complete3_100k_97.5perc <- voi_data_complete3_100k %>% group_by(city, age_cat, sex, age_sex, population) %>%
    summarise(across( .cols = where(is.numeric)& !run,  .fns = ~quantile(., 0.975),.names = "{col}"), .groups = 'drop')
  voi_data_complete3_100k_97.5perc$value_type <- '97.5perc'
  
  # standard deviation
  voi_data_complete3_100k_sd <- voi_data_complete3_100k %>% group_by(city, age_cat, sex, age_sex, population) %>% summarise(across(where(is.numeric)& !run,
                                                                                                                                   sd), .groups = 'drop')
  voi_data_complete3_100k_sd$value_type <- 'std'
  
  
  # combine all statistics
  voi_data_complete3_100k_summary <- rbind(voi_data_complete3_100k_mean, voi_data_complete3_100k_2.5perc,
                                           voi_data_complete3_100k_97.5perc,voi_data_complete3_100k_sd)
  
  # re-order columns
  voi_data_complete3_100k_summary <- voi_data_complete3_summary %>% dplyr::select(city, age_cat, sex, age_sex, population, value_type,
                                                                                  sc_cycle_ylls_level1, sc_car_ylls_level1, sc_bus_ylls_level1,
                                                                                  sc_cycle_ylls_level2, sc_car_ylls_level2, sc_bus_ylls_level2,
                                                                                  sc_cycle_ylls_level3, sc_car_ylls_level3, sc_bus_ylls_level3, everything())
  

  # set-up output list 
  ithim_results <- list()
  

  ithim_results$voi_complete <- voi_data_complete3
  ithim_results$voi_complete_summary <- voi_data_complete3_summary
  ithim_results$voi_complete_100k <- voi_data_complete3_100k
  ithim_results$voi_complete_100k_summary <- voi_data_complete3_100k_summary
  
  return(ithim_results)
  
  
}