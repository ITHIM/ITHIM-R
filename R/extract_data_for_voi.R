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
#'    \item extract the population statistics
#'    \item create one dataframe for all cities with all outcomes for all model runs, age groups and disease and 
#'          scenario combinations for the combined AP and PA results
#'    \item create one dataframe for all cities with all outcomes for all model runs, age groups and disease and 
#'          scenario combinations for the separate AP and PA results
#'          
#'  } 
#'   
#'\item compute statistics (mean, 2.5th and 97.5th percentiles, standard deviation) for total YLLs lost 
#'      for the combined AP and PA results
#'\item compute statistics (mean, 2.5th and 97.5th percentiles, standard deviation) for YLLs lost per 100k
#'      for the combined AP and PA results
#'\item compute statistics (mean, 2.5th and 97.5th percentiles, standard deviation) for total YLLs lost 
#'      for the separate AP and PA pathways
#'\item compute statistics (mean, 2.5th and 97.5th percentiles, standard deviation) for YLLs lost per 100k 
#'      for the separate AP and PA pathways
#' 
#' }  
#' 
#' @param NSCEN number of scenarios (not incl. baseline)
#' @param NSAMPLES number of model runs per city
#' @param SCEN_SHORT_NAME names of the scenarios (incl. baseline)
#' @param cities list of cities for which the model was run
#' @param multi_city_ithim list containing the ithim model information including results for the various model runs
#' @param output_version the output version of the model run
#' @param level1 list of diseases considered in level 1
#' @param level2 list of diseases considered in level 2
#' @param level3 list of diseases considered in level 3
#' 
#' @return ithim_results list with the following objects:
#' @return voi_complete - dataframe for all cities with all total YLL outcomes for all model runs, age and sex categories and disease and scenario combinations - combined AP and PA pathway
#' @return voi_complete_summary - summary statistics of YLLs for combined AP and PA pathway
#' @return voi_complete_100k - dataframe for all cities for all YLL outcomes per 100k - combined AP and PA pathway
#' @return voi_commplete_100k_summary - summary statistics per of YLL per 100k for combined AP and PA pathway 
#' @return voi_complete_pathway - dataframe for all cities with all total YLL outcomes for all model runs, age and sex categories and disease and scenario combinations - separate AP and PA pathways
#' @return voi_complete_summary_pathway - summary statistics of YLLs for separate AP and PA pathways
#' @return voi_complete_100k_pathway - dataframe for all cities for all YLL outcomes per 100k - separate AP and PA pathways
#' @return voi_commplete_100k_summary_pathway - summary statistics per of YLL per 100k for separate AP and PA pathways
#' 
#' @export

extract_data_for_voi <- function(NSCEN, NSAMPLES, SCEN_SHORT_NAME,cities,multi_city_ithim, output_version, level1, level2, level3){
  
  
  # initialise list / dataframes for all cities with all outcomes for all model runs, age groups
  # and disease and scenario combinations - combined AP and PA results
  voi_data_all <- list()
  voi_data_all_df <- data.frame()
  voi_data_all_sex_df <- data.frame()
  
  
  
  # create one dataframe for all cities with all outcomes for all model runs, age groups and disease and scenario combinations
  # separate result columns for AP and PA
  voi_data_all_ap_pa <- list()
  voi_data_all_df_ap_pa <- data.frame()
  voi_data_all_sex_df_ap_pa <- data.frame()
  
  
  
  
  # create empty dataframe to save all the population numbers
  population_df <- data.frame()
  
  for(ci in 1:length(cities)){ # loop through cities
    city <- cities[ci]
    multi_city_ithim[[city]] <- readRDS(paste0('results/voi/',city,'_',output_version,'.Rds')) # read in city specific data
    
    
    
    # Extract population data
    DEMOGRAPHIC <- multi_city_ithim[[city]]$DEMOGRAPHIC
    
    # find the minimum and maximum ages for each age category used in the demographic information for the city
    min_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
    max_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][2]))

    
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
    

    
    
    # # total yll outcome across all outcome age categories per city and scenario and disease combination
    # outcome[[city]] <- t(sapply(multi_city_ithim[[city]]$outcomes, function(x) colSums(x$hb$ylls[keep_rows,keep_cols],na.rm=T)))
    # colnames(outcome[[city]]) <- paste0(colnames(outcome[[city]]),'_',city)
    # 
    
    
    ### combined AP and PA
    # create one dataframe for all cities with all outcomes for all model runs, age groups and disease and scenario combinations
    # combined AP and PA
    for(row in keep_rows){
      voi_data_all[[city]]$outcomes <- t(sapply(multi_city_ithim[[city]]$outcomes, function(x) rbind(x$hb$ylls[row,])))
      voi_dummy <- data.frame(voi_data_all[[city]])
      colnames(voi_dummy)<-colnames(multi_city_ithim[[city]]$outcomes[[1]]$hb$ylls)
      voi_dummy$city <- city
      voi_data_all_df <- rbind(voi_data_all_df, voi_dummy)
    }
    
    # add information for males and females
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
    
    
    
    ###### consider AP and PA pathways separately

    for(row in keep_rows){
      voi_data_all_ap_pa[[city]]$outcomes <- t(sapply(multi_city_ithim[[city]]$outcomes, function(x) rbind(x$pathway_hb$ylls[row,])))
      voi_dummy <- data.frame(voi_data_all_ap_pa[[city]])
      colnames(voi_dummy)<-colnames(multi_city_ithim[[city]]$outcomes[[1]]$pathway_hb$ylls)
      voi_dummy$city <- city
      voi_data_all_df_ap_pa <- rbind(voi_data_all_df_ap_pa, voi_dummy)
    }
    
    
    
    # add information for males and females
    keep_cols_ap_pa <- which(!sapply(names(multi_city_ithim[[city]]$outcomes[[1]]$pathway_hb$ylls),function(x)grepl('age|sex',as.character(x))))
    
    # first for males 
    list_male <- list()
    for (i in 1:NSAMPLES){
      list_male[[i]] <- as.data.frame(multi_city_ithim[[city]]$outcomes[[i]]$pathway_hb$ylls) %>% filter(sex == 'male')
    }
    
    voi_city_male_ap_pa <- as.data.frame(t(sapply(list_male, function(x) colSums(x[keep_rows,keep_cols_ap_pa],na.rm=T))))
    voi_city_male_ap_pa$city <- city
    voi_city_male_ap_pa$sex <- 'male'
    
    
    # females
    list_female <- list()
    for (i in 1:NSAMPLES){
      list_female[[i]] <- as.data.frame(multi_city_ithim[[city]]$outcomes[[i]]$pathway_hb$ylls) %>% filter(sex == 'female')
    }
    
    voi_city_female_ap_pa <- as.data.frame(t(sapply(list_female, function(x) colSums(x[keep_rows,keep_cols_ap_pa],na.rm=T))))
    voi_city_female_ap_pa$city <- city
    voi_city_female_ap_pa$sex <- 'female'
    
    # all 
    voi_data_all_sex_df_ap_pa <- rbind(voi_data_all_sex_df_ap_pa, voi_city_male_ap_pa, voi_city_female_ap_pa)
    

  } # end of city loop
  
  # create an age and sex category column
  voi_data_all_df$age_sex <- paste(voi_data_all_df$sex, voi_data_all_df$age_cat, sep = )
  voi_data_all_df_ap_pa$age_sex <- paste(voi_data_all_df_ap_pa$sex, voi_data_all_df_ap_pa$age_cat, sep = )
  
  
  
  
  
  
  ################# create outcome statistics for different scenarios and health outcomes
  
  ### combined AP and PA
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
  voi_data_complete3_100k_summary <- voi_data_complete3_100k_summary %>% dplyr::select(city, age_cat, sex, age_sex, population, value_type,
                                                                                  sc_cycle_ylls_level1, sc_car_ylls_level1, sc_bus_ylls_level1,
                                                                                  sc_cycle_ylls_level2, sc_car_ylls_level2, sc_bus_ylls_level2,
                                                                                  sc_cycle_ylls_level3, sc_car_ylls_level3, sc_bus_ylls_level3, everything())
  
  
  
  
  ### AP and PA separate
  # number the different runs
  voi_data_all_df_ap_pa$run <- rep(1:nsamples,length(unique(voi_data_all_df_ap_pa$age_sex))*length(cities))
  voi_data_all_sex_df_ap_pa$run <- rep(1:nsamples,length(unique(voi_data_all_sex_df_ap_pa$sex))*length(cities))
  
  # add extra columns
  voi_data_all_sex_df_ap_pa$age_cat <- 'all'
  voi_data_all_sex_df_ap_pa$age_sex <- paste(voi_data_all_sex_df$sex, voi_data_all_sex_df$age_cat, sep = ' ')
  
  # create total for each run for all outcomes
  voi_data_total_df_ap_pa <- voi_data_all_sex_df_ap_pa %>% group_by( age_cat, run, city) %>% summarise(across(where(is.numeric), sum), .groups = 'drop')
  voi_data_total_df_ap_pa$sex <- 'all'
  voi_data_total_df_ap_pa$age_sex <- paste(voi_data_total_df_ap_pa$sex, voi_data_total_df_ap_pa$age_cat, sep = ' ')
  
  
  # create one dataset
  voi_data_all_df_ap_pa$sex <- as.character(voi_data_all_df_ap_pa$sex)
  voi_data_all_df_ap_pa$age_cat <- as.character(voi_data_all_df_ap_pa$age_cat)
  voi_data_all_df_ap_pa <- voi_data_all_df_ap_pa %>% mutate_if(is.list,as.numeric)
  voi_data_complete_ap_pa <- bind_rows(voi_data_all_df_ap_pa, voi_data_all_sex_df_ap_pa , voi_data_total_df_ap_pa)
  
  # add population data
  voi_data_complete2_ap_pa <- merge(voi_data_complete_ap_pa, population_df, by = c('age_cat','sex','city'))
  
 
  
  # add summary statistics
  voi_data_complete3_mean_ap_pa <- voi_data_complete2_ap_pa %>% group_by(city, age_cat, sex, age_sex, population
                                                                         ) %>% summarise(across(where(is.numeric)& !run, mean
                                                                                                ), .groups = 'drop')
  voi_data_complete3_mean_ap_pa$value_type <- 'mean'
  
  # 2.5th percentile
  voi_data_complete3_2.5perc_ap_pa <- voi_data_complete2_ap_pa %>% group_by(city, age_cat, sex, age_sex, population
                                                                            ) %>% summarise(
                                                                              across( .cols = where(is.numeric) & !run,  
                                                                                      .fns = ~quantile(., 0.025),.names = "{col}"), .groups = 'drop')
  voi_data_complete3_2.5perc_ap_pa$value_type <- '2.5perc'
  
  # 97.5th percentile
  voi_data_complete3_97.5perc_ap_pa <- voi_data_complete2_ap_pa %>% group_by(city, age_cat, sex, age_sex, population) %>% summarise(
                                                          across( .cols = where(is.numeric)& !run,  .fns = ~quantile(., 0.975
                                                                                                                     ),.names = "{col}"), .groups = 'drop')
  voi_data_complete3_97.5perc_ap_pa$value_type <- '97.5perc'
  
  # standard deviation
  voi_data_complete3_sd_ap_pa <- voi_data_complete2_ap_pa %>% group_by(city, age_cat, sex, age_sex, population) %>% summarise(across(where(is.numeric)& !run,
                                                                                                                         sd), .groups = 'drop')
  voi_data_complete3_sd_ap_pa$value_type <- 'std'
  
  
  # combine all statistics 
  voi_data_complete3_summary_ap_pa <- rbind(voi_data_complete3_mean_ap_pa, voi_data_complete3_2.5perc_ap_pa,
                                            voi_data_complete3_97.5perc_ap_pa,voi_data_complete3_sd_ap_pa)
  
  # re-order columns
  voi_data_complete3_summary_ap_pa <- voi_data_complete3_summary_ap_pa %>% dplyr::select(city, age_cat, sex, age_sex, population, value_type,everything())
  
  
  
  
  ## repeat for 100k
  
  # calculate per 100k
  voi_data_complete3_100k_ap_pa <- voi_data_complete2_ap_pa %>% mutate(across(where(is.numeric) & !run &
                                                                                !population, ~round(as.numeric(.x)/population*100000,4)))
  
  # add summary statistics
  voi_data_complete3_100k_mean_ap_pa <- voi_data_complete3_100k_ap_pa %>% group_by(city, age_cat, sex, age_sex, population
                                                                                   ) %>% summarise(across(where(is.numeric)&
                                                                                                            !run, mean), .groups = 'drop')
  voi_data_complete3_100k_mean_ap_pa$value_type <- 'mean'
  
  # 2.5th percentile
  voi_data_complete3_100k_2.5perc_ap_pa <- voi_data_complete3_100k_ap_pa %>% group_by(city, age_cat, sex, age_sex, population
                                                                                      ) %>% summarise(across( .cols = where(is.numeric) &
                                                                                            !run,  .fns = ~quantile(., 0.025),
                                                                                            .names = "{col}"), .groups = 'drop')
  voi_data_complete3_100k_2.5perc_ap_pa$value_type <- '2.5perc'
  
  # 97.5th percentile
  voi_data_complete3_100k_97.5perc_ap_pa <- voi_data_complete3_100k_ap_pa %>% group_by(city, age_cat, sex, age_sex, population
                                                                                       ) %>% summarise(across( .cols = where(is.numeric)& !run,
                                                                                      .fns = ~quantile(., 0.975),.names = "{col}"), .groups = 'drop')
  voi_data_complete3_100k_97.5perc_ap_pa$value_type <- '97.5perc'
  
  # standard deviation
  voi_data_complete3_100k_sd_ap_pa <- voi_data_complete3_100k_ap_pa %>% group_by(city, age_cat, sex, age_sex, population
                                                                                 ) %>% summarise(across(where(is.numeric)& !run,
                                                                                    sd), .groups = 'drop')
  voi_data_complete3_100k_sd_ap_pa$value_type <- 'std'
  
  
  # combine all statistics
  voi_data_complete3_100k_summary_ap_pa <- rbind(voi_data_complete3_100k_mean_ap_pa, voi_data_complete3_100k_2.5perc_ap_pa,
                                           voi_data_complete3_100k_97.5perc_ap_pa,voi_data_complete3_100k_sd_ap_pa)
  
  # re-order columns
  voi_data_complete3_100k_summary_ap_pa <- voi_data_complete3_100k_summary_ap_pa %>% dplyr::select(city, age_cat, sex, age_sex, population, value_type,
                                                                                                   everything())
  
  
  

  

  # set-up output list 
  ithim_results <- list()
  

  ithim_results$voi_complete <- voi_data_complete3
  ithim_results$voi_complete_summary <- voi_data_complete3_summary
  ithim_results$voi_complete_100k <- voi_data_complete3_100k
  ithim_results$voi_complete_100k_summary <- voi_data_complete3_100k_summary
  ithim_results$voi_complete_pathway <- voi_data_complete2_ap_pa
  ithim_results$voi_complete_summary_pathway <- voi_data_complete3_summary_ap_pa
  ithim_results$voi_complete_100k_pathway <- voi_data_complete3_100k_ap_pa
  ithim_results$voi_complete_100k_summary_pathway <- voi_data_complete3_100k_summary_ap_pa
  
  return(ithim_results)
  
  
}