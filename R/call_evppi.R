#' EVPPI wrapper function for total population YLLs
#' 
#' This function gets the ithim results into the correct format and calls the \code{\link{compute_evppi()}} script
#' to calculate the EVPPIs for all input parameters and required outcomes and scenarios for the entire
#' population considered in the model
#'
#' The function performs the following steps:
#' 
#'\itemize{
#'\item create a vector containing the global parameters but not including the dose response quantiles as they are treated separately
#'\item loop through the cities:
#'  \itemize{
#'    \item extract the PIF values for each dose response function and outcome, calculate the PIF for the 
#'          different levels and for the entire population
#'    \item extract the city specific parameters (excluding the CO2 and PM emission inventory parameters as they are not independent of each other)
#'    \item extract the outcomes of interest for each scenario using the outcome_voi_list
#'    \item call the \code{\link{compute_evppi()}} function to calculate the expected values of partially perfect information (EVPPI)
#'          for all parameters and diseases of interest
#'    \item if NSAMPLES >= 1000 then also calculate the EVPPI values for the emission inventory parameters
#'    } 
#' \item tidy up results data and add parameter classification
#' }  
#' 
#' @param parameter_samples table containing all the input parameter variables for the different model runs for all cities
#' @param outcome_voi_list vector detailing the outcomes to be considered in the VoI analysis
#' @param voi_complete_df total yll outcome for all outcome age categories per city and scenario and disease combination, also combined city result (sum)
#' @param cities vector of cities
#' @param NSCEN number of scenarios (not incl. baseline)
#' @param NSAMPLES number of times the model was run for each city
#' @param scenario_names gives the names of the scenarios (incl baseline)
#' @param output_version output version number
#' @param level1 list of outcomes included in level 1
#' @param level2 list of outcomes included in level 2
#' @param level3 list of outcomes included in level 3
#' 
#' @return list containg the following elements:
#' @return evppi_df containing the evppi values for all input parameters and scenario and disease outcomes
#' @return evppi_outcome_names, a vector containing all the scenario outcome combinations
#' 
#' @export


call_evppi <- function(parameter_samples, outcome_voi_list, voi_complete_df, cities, NSCEN, NSAMPLES,
                       SCEN_SHORT_NAME, scenario_names, output_version, level1, level2, level3){
  
  
  
 
  # first extract input parameters of interest
  # create list with global parameters that are relevant for all cities
  general_inputs <- sapply(colnames(parameter_samples),function(x)!grepl(paste(cities, collapse = "|"),x))
  general_parsampl <- parameter_samples[,general_inputs]
  
  # remove DR quantiles
  no_quantiles <- sapply(colnames(general_parsampl),function(x)!grepl('DOSE_RESPONSE_QUANTILE',x))
  general_parsampl <- general_parsampl[,no_quantiles]
  
 
  # set up dataframe to contain the final evppi results
  evppi_df <- data.frame()
  
  # extract city specific input parameters
  for (city in cities){ # loop through cities
    
    # read in dose response relative risk PIF values
    city_multicity <- readRDS(paste0('results/voi/',city,'_',output_version,'.Rds'))
    city_out_all <- city_multicity$outcomes
    city_multicity <- 0
    
   
    # extract PIF values from first model run
    dr_pif_all_age_sex <- city_out_all[[1]]$DR_pif$DR_PIF
    dr_pif_all_age_sex$run <- 1
    total_population <- sum(dr_pif_all_age_sex$population)

    # add PIF for all samples
    for (i in 2:NSAMPLES){
      pif_dummy <- city_out_all[[i]]$DR_pif$DR_PIF
      pif_dummy$run <- i
      dr_pif_all_age_sex <- rbind(dr_pif_all_age_sex, pif_dummy)
    }
    
    
    # calculate pif for different levels
    level_list <- list(level1, level2, level3)
    scen_only_names <- scenario_names[2:length(scenario_names)]
    
    # loop through levels
    all_levels <- data.frame()
    l<-1
    for (level in list(level1, level2, level3)){
      dummy_level_df <- dr_pif_all_age_sex %>% dplyr::select(matches(level))
      
      # loop trough scenarios and calculate sum
      for (scen in scen_only_names){
        dummy_level_df <- dummy_level_df %>%
          mutate(sum = rowSums(pick(matches(scen))))  %>% # calculate sum
          rename_with(~paste0(scen, '_DR_PIF_level',l), .cols= sum) # re-name new column
        
      }
      if (l ==1){
        all_levels <- dummy_level_df %>% dplyr::select(!matches(level))
      } else{
        all_levels <- cbind(all_levels, dummy_level_df%>% dplyr::select(!matches(level)))
      }
      l <- l +1
    }
    
    dr_pif_all_age_sex2 <- cbind(dr_pif_all_age_sex, all_levels)
    
   
    # multiply pif values by population
    dr_pif_all_age_sex_pif <- dr_pif_all_age_sex2 %>% dplyr::select(!c(sex, age_cat, population, dem_index, run))
    dr_pif_all_age_sex_pif <- dr_pif_all_age_sex_pif * dr_pif_all_age_sex$population
    dr_pif_all_age_sex3<- cbind(dr_pif_all_age_sex2 %>% dplyr::select(c(sex, age_cat, population, dem_index, run)),dr_pif_all_age_sex_pif)
    
    
    # calculate PIF for total population
    dr_pif <- dr_pif_all_age_sex3 %>% dplyr::select(!c(sex, age_cat, population, dem_index)) %>% group_by(
                                                  run) %>%summarise(across(where(is.numeric), sum ), .groups = 'drop')
    dr_pif <- dr_pif/total_population 
    dr_pif <- dr_pif %>% dplyr::select(!c(run))
    
    
    
    # extract background_pa_zeros from first model run
    background_pa_zeros_age_sex <- city_out_all[[1]]$background_pa_zero_prop
    background_pa_zeros_age_sex$run <- 1
    
    
    # add background_pa_zeros for all samples
    for (i in 2:NSAMPLES){
      background_pa_zeros_dummy <- city_out_all[[i]]$background_pa_zero_prop
      background_pa_zeros_dummy$run <- i
      background_pa_zeros_age_sex <- rbind(background_pa_zeros_age_sex, background_pa_zeros_dummy)
    }
    
    # calculate background_pa_zeros for entire population (weighted by population size)
    # match with demographic information
    pop <- unique(dr_pif_all_age_sex %>% dplyr::select(c(sex, age_cat, population)))
    background_pa_zeros_age_sex <- left_join(background_pa_zeros_age_sex, pop, by = c('sex', 'age_cat'))
    background_pa_zeros_age_sex$zero_prop_population <- background_pa_zeros_age_sex$zero_prop*background_pa_zeros_age_sex$population
    
    background_pa_zeros_all <- background_pa_zeros_age_sex %>% dplyr::select(!c(sex, age_cat, population, zero_prop)) %>% group_by(
                                                                run) %>% summarise(zero_prop_population = sum(zero_prop_population)/
                                                                                     total_population)
    
    
    city_out_all <- 0
    
    
    # extract city specific input parameters
    city_inputs <- sapply(colnames(parameter_samples),function(x)grepl(city,x))
    city_parsampl <- parameter_samples[,city_inputs]
    city_parsampl_copy <- city_parsampl
    
    # remove CO2 and PM2.5 emission inventory parameters
    city_noCO2para <- sapply(colnames(city_parsampl), function(x)!grepl('CO2',x))
    city_parsampl <- city_parsampl[,city_noCO2para]
    city_Co2_parasampl <- city_parsampl_copy[,!city_noCO2para]
    
    city_noPMemissionpara <- sapply(colnames(city_parsampl), function(x)!grepl('PM_EMI',x))
    city_parsampl_copy <- city_parsampl
    city_parsampl <- city_parsampl[,city_noPMemissionpara]
    city_PM_parasampl <- city_parsampl_copy[,!city_noPMemissionpara]
    

    # extract the required outcomes for each city
    cityname <- city
    city_out <- voi_complete_df %>% filter(city == cityname & age_sex == 'all all') %>% dplyr::select(!c(age_cat, age_sex, city,sex, population, run))
    
    # extract outcomes required as set up in outcome_voi_list
    city_outputs <- sapply(colnames(city_out),function(x)grepl(paste(outcome_voi_list, collapse = "|"),x))
    city_outcomes <- city_out[,city_outputs]
    
    scen_names_only <- scenario_names[1:NSCEN+1]

    # prep parameters
    city_para = cbind(as.data.frame(city_parsampl), background_pa_zeros_all$zero_prop_population)
    
    city_para <- city_para %>% rename(!!paste0('BACKGROUND_PA_ZERO_PROPORTIONS_',city) := 'background_pa_zeros_all$zero_prop_population')
    
    
    # calculate the total number of independent parameters to be considered in the VoI analysis
    param_no <- ncol(city_para) + ncol(general_parsampl) + 1
    
    
    # calculate the evppi for each city (still within the city loop), Dr quantiles not included
    evppi_city <- future_lapply(1:param_no,
                                FUN = ithimr::compute_evppi,
                                global_para = as.data.frame(general_parsampl),
                                city_para = city_para,
                                dr_pif = dr_pif,
                                outcome_voi_list = outcome_voi_list,
                                SCEN_SHORT_NAME = SCEN_SHORT_NAME,
                                city_outcomes = city_outcomes,
                                nsamples = NSAMPLES)

    # for (i in 1:param_no){
    #   print(i)
    #   out <- compute_evppi(i, global_para = as.data.frame(general_parsampl),city_para = as.data.frame(city_parsampl),dr_pif,
    #                        outcome_voi_list, SCEN_SHORT_NAME,city_outcomes, nsamples)
    # }

    evppi_city2 <- do.call(rbind,evppi_city) # bind list
    
    evppi_city3 <- as.data.frame(evppi_city2) # turn into dataframe
    
    # Manipulate evppi_city3 df into correct format
    # add column names without city part
    evppi_outcome_names <- strsplit(colnames(city_outcomes),paste("_",city,sep="")) 
    colnames(evppi_city3) <- evppi_outcome_names
    #evppi_outcome_names <- colnames(evppi_city3)
    
    evppi_city3$parameters <-  c(colnames(general_parsampl), colnames(city_para), 'AP_PA_DOSE_RESPONSE_QUANTILES') # add parameter name column
    evppi_city3$city <- city # add city name column
    
    
    #look at CO2 and PM emission inventories separately
    if(NSAMPLES>=1000){
      
      # Feb 2025: CO2 no impact on health outcomes, therefore not considered
      # first consider CO2 parameters
      # evppi_for_CO2_city <- future_lapply(1,
      #                                    FUN = ithimr:::compute_evppi,
      #                                    global_para = data.frame(),
      #                                    city_para = city_Co2_parasampl,
      #                                    city_outcomes = city_outcomes,
      #                                    dr_pif = data.frame(),
      #                                    outcome_voi_list = outcome_voi_list,
      #                                    SCEN_SHORT_NAME = SCEN_SHORT_NAME,
      #                                    nsamples = NSAMPLES,
      #                                    individual_para = FALSE)
      # 
      # evppi_for_CO2_city2 <- do.call(rbind,evppi_for_CO2_city) # bind list
      # evppi_for_CO2_city3 <- as.data.frame(evppi_for_CO2_city2) # turn into dataframe
      # colnames(evppi_for_CO2_city3) <- evppi_outcome_names
      # 
      # evppi_for_CO2_city3$parameters <-  c(paste0('CO2_emissions_inventory')) # add parameter name column
      # evppi_for_CO2_city3$city <- city # add city name column
      # 
      # evppi_city3 <- rbind(evppi_city3,evppi_for_CO2_city3)
      # 
      
      # consider PM parameters
      evppi_for_PM_city <- future_lapply(1,
                                          FUN = ithimr:::compute_evppi,
                                          global_para = data.frame(),
                                          city_para = city_PM_parasampl,
                                          city_outcomes = city_outcomes,
                                          dr_pif = data.frame(),
                                          outcome_voi_list = outcome_voi_list,
                                          SCEN_SHORT_NAME = SCEN_SHORT_NAME,
                                          nsamples = NSAMPLES,
                                          individual_para = FALSE)
      
      evppi_for_PM_city2 <- do.call(rbind,evppi_for_PM_city) # bind list
      evppi_for_PM_city3 <- as.data.frame(evppi_for_PM_city2) # turn into dataframe
      colnames(evppi_for_PM_city3) <- evppi_outcome_names
      
      evppi_for_PM_city3$parameters <-  c(paste0('PM_emissions_inventory')) # add parameter name column
      evppi_for_PM_city3$city <- city # add city name column
      
      evppi_city3 <- rbind(evppi_city3,evppi_for_PM_city3)
    }
    

    
    evppi_df <- rbind(evppi_df, evppi_city3) # add to total evppi dataframe
    
    # remove city from parameter names
    evppi_df$parameters <- unlist(strsplit(as.character(evppi_df$parameters),paste("_",city,sep="")))
    
    
  } # end of city loop
  
  
  
  
  # merge with parameter classification
  para_classification <- read.csv(paste0(global_path, "voi/parameter_explanations.csv"))
  #para_classification <- read.csv("inst/extdata/global/voi/parameter_explanations.csv")
  
  evppi_df <- merge(x = para_classification, y = evppi_df, by = 'parameters', all.y = TRUE)
  
  evppi_df2 <- evppi_df %>% arrange(ordering) %>% dplyr::select(-c(ordering))
  
  evppi_df2 <- evppi_df2 %>% relocate(city,classification, parameters, parameters_lowerCase) # change order of columns
  
  # re-order rows
  evppi_df2 <- evppi_df2[order(evppi_df2$city),]
  
  
  return(list(evppi_df2, evppi_outcome_names))
  
}
