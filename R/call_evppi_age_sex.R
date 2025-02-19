#' EVPPI wrapper function for total population YLLs by sex and age
#' 
#' This function gets the ithim results into the correct format and calls the \code{\link{compute_evppi()}} script
#' to calculate the EVPPIs for all input parameters and required outcomes and scenarios split by sex and age.
#'
#' The function performs the following steps:
#' 
#'\itemize{
#'\item create a vector containing the global parameters but not including the dose response quantiles as they are treated separately
#'\item loop through the cities:
#'  \itemize{
#'	  \item extract the PIF values for each dose response function and outcome, calculate the PIF for the 
#'          different levels
#'    \item extract the city specific parameters (excluding the CO2 and PM emission inventory parameters)

#'    \item loop through all age and sex categories:
#'      \itemize{   
#'        \item extract the outcomes of interest for each scenario using the outcome_voi_list
#'        \item call the \code{\link{compute_evppi()}} function to calculate the expected values of partially perfect information (EVPPI)
#'          for all parameters and diseases of interest
#'        }
#'    \item if NSAMPLES >= 1000 then also calculate the EVPPI values for the emission inventory parameters by looping through 
#'          all age and sex categories
#'    } 
#' \item tidy up results data and add parameter classification
#' }  
#' 
#' 
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
#' @return evppi_agesex_df dataframe containing all EVPPI outcomes for all age and sex categories and all cities
#' @return age_gender_cat vector with all age and sex categories
#' 
#' @export


call_evppi_age_sex <- function(parameter_samples, outcome_voi_list, voi_complete_df, cities, NSCEN, NSAMPLES,
                       SCEN_SHORT_NAME, scenario_names, output_version, level1, level2, level3){
  
  evppi_agesex_df <- data.frame()
  
  # first extract input parameters of interest
  # create list with global parameters that are relevant for all cities
  general_inputs <- sapply(colnames(parameter_samples),function(x)!grepl(paste(cities, collapse = "|"),x))
  general_parsampl <- parameter_samples[,general_inputs]

  # remove DR quantiles
  no_quantiles <- sapply(colnames(general_parsampl),function(x)!grepl('DOSE_RESPONSE_QUANTILE',x))
  general_parsampl <- general_parsampl[,no_quantiles]
 

  scen_names_only <- scenario_names[1:NSCEN+1]
  
  evppi_city_list_all <- list()
  ci <- 1
  
  for (city in cities){
    print(city)


    # read in dose response relative risk PIF values
    city_multicity <- readRDS(paste0('results/voi/',city,'_',output_version,'.Rds'))
    city_out_all <- city_multicity$outcomes
    city_multicity <- 0
    
   
    # extract PIF values from first model run
    dr_pif_all_age_sex <- city_out_all[[1]]$DR_pif$DR_PIF
    dr_pif_all_age_sex$run <- 1
    
    # find total population by sex
    dr_pif_male <- dr_pif_all_age_sex %>% filter(sex == 'male') %>% dplyr::select(sex, population)
    dr_pif_male$total_population <- sum(dr_pif_male$population)
    dr_pif_female <- dr_pif_all_age_sex %>% filter(sex == 'female') %>% dplyr::select(sex, population)
    dr_pif_female$total_population <- sum(dr_pif_female$population)
    
    dr_pif_pop_sex <- unique(rbind(dr_pif_male, dr_pif_female) %>% dplyr::select(!population))

    # add PIF for all samples
    for (i in 2:NSAMPLES){
      pif_dummy <- city_out_all[[i]]$DR_pif$DR_PIF
      pif_dummy$run <- i
      dr_pif_all_age_sex <- rbind(dr_pif_all_age_sex, pif_dummy)
    }
    
    city_out_all <- 0
    
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
    
    dr_age_sex <- cbind(dr_pif_all_age_sex, all_levels)

    # create age_gender column
    dr_age_sex$age_sex <- paste0(dr_age_sex$sex, " ", dr_age_sex$age_cat)
    
    # remove columns that aren't needed
    dr_age_sex <- dr_age_sex %>% dplyr::select(!c(age_cat, sex, population, dem_index, run))

    
    # extract city specific input parameters
    city_inputs <- sapply(colnames(parameter_samples),function(x)grepl(city,x))
    city_parsampl <- parameter_samples[,city_inputs]
    city_parsampl_copy <- city_parsampl
    
    # remove CO2 parameters
    city_noCO2para <- sapply(colnames(city_parsampl), function(x)!grepl('CO2',x))
    city_parsampl <- city_parsampl[,city_noCO2para]
    city_Co2_parasampl <- city_parsampl_copy[,!city_noCO2para]
    
    city_noPMemissionpara <- sapply(colnames(city_parsampl), function(x)!grepl('PM_EMI',x))
    city_parsampl_copy <- city_parsampl
    city_parsampl <- city_parsampl[,city_noPMemissionpara]
    city_PM_parasampl <- city_parsampl_copy[,!city_noPMemissionpara]
    
    # extract the required outcomes for each city - loop through age and gender categories
    city_name <- city
    city_agesex_out <- voi_complete_df %>% filter(city == city_name & !age_cat== 'all' & !sex=='all')
    age_gender_cat <- unique(city_agesex_out$age_sex)
    
    k <- 1
    
    for(age_gender in age_gender_cat){
      
      city_agesex_out2 <- city_agesex_out %>% filter(age_sex == age_gender)
      city_agesex_outputs <- sapply(colnames(city_agesex_out2),function(x)grepl(paste(outcome_voi_list, collapse = "|"),x))
      city_agesex_outcomes <- city_agesex_out2[,city_agesex_outputs]
      
      # find DR functions by sex
      dr_pif_age_sex <- dr_age_sex %>% filter(age_sex == age_gender ) %>% dplyr::select(!c(age_sex))

      # calculate the total number of independent parameters to be considered in the VoI analysis
      param_no <- ncol(city_parsampl) + ncol(general_parsampl) + 1
      
      # replace NA with 0s, note that the evppi analysis returns NA for all outcomes that are all 0
      city_agesex_outcomes_na_cols <- names(which(colSums(is.na(city_agesex_outcomes))>0)) # record colnames
      city_agesex_outcomes[is.na(city_agesex_outcomes)] <- 0 # replace NAs with 0
      
      # calculate evppi
      evppi_agesex_city <- future_lapply(1:param_no, # calculate the evppi for each city
                                         FUN = ithimr::compute_evppi,
                                         global_para = as.data.frame(general_parsampl),
                                         city_para = as.data.frame(city_parsampl),
					                               dr_pif = dr_pif_age_sex,
                                         outcome_voi_list = outcome_voi_list,
                                         SCEN_SHORT_NAME = SCEN_SHORT_NAME,
                                         city_outcomes = city_agesex_outcomes,
                                         nsamples = NSAMPLES)
      
      
      evppi_agesex_city2 <- do.call(rbind,evppi_agesex_city)
      evppi_agesex_city3 <- as.data.frame(evppi_agesex_city2) # turn into dataframe
      
      evppi_agesex_outcome_names <- strsplit(colnames(city_agesex_outcomes),paste("_",city,sep="")) # add column names without city part
      colnames(evppi_agesex_city3) <- paste(evppi_agesex_outcome_names, age_gender, sep = "_")
      evppi_agesex_outcome_names <- colnames(evppi_agesex_city3)
      
      # replace columns for which the outcomes where originally NA by NA again
      if (length(city_agesex_outcomes_na_cols)>0){
        city_agesex_outcomes_na_cols2 <- strsplit(city_agesex_outcomes_na_cols,paste("_",city,sep=""))
        city_agesex_outcomes_na_cols3 <- paste(city_agesex_outcomes_na_cols2,age_gender, sep = "_")
        
        evppi_agesex_city3[,city_agesex_outcomes_na_cols3] <- NaN
      }
      
      if(k == 1){
        evppi_agesex_city_df <- evppi_agesex_city3
      }else{
        evppi_agesex_city_df <- cbind(evppi_agesex_city_df, evppi_agesex_city3)
      }
      
      k <- k + 1
    }
    
    evppi_agesex_city_df$parameters <-  c(colnames(general_parsampl), colnames(city_parsampl), 'AP_PA_DOSE_RESPONSE_QUANTILES') # add parameter name column
    evppi_agesex_city_df$city <- city # add city name column
    
    
    
    #look at CO2 and PM emission inventories separately
    if(NSAMPLES>=1000){
      
      k <- 1
      
      for(age_gender in age_gender_cat){
        
        city_agesex_out2 <- city_agesex_out %>% filter(age_sex == age_gender)
        city_agesex_outputs <- sapply(colnames(city_agesex_out2),function(x)grepl(paste(outcome_voi_list, collapse = "|"),x))
        city_agesex_outcomes <- city_agesex_out2[,city_agesex_outputs]
        
        
        # replace NA with 0s, note that the evppi analysis returns NA for all outcomes that are all 0
        city_agesex_outcomes_na_cols <- names(which(colSums(is.na(city_agesex_outcomes))>0)) # record colnames
        city_agesex_outcomes[is.na(city_agesex_outcomes)] <- 0 # replace NAs with 0
        
        evppi_agesex_for_CO2_city <- future_lapply(1,
                                                   FUN = ithimr:::compute_evppi,
                                                   global_para = data.frame(),
                                                   city_para = city_Co2_parasampl,
                                                   city_outcomes = city_agesex_outcomes,
                                                   dr_pif = data.frame(),
                                                   outcome_voi_list = outcome_voi_list,
                                                   SCEN_SHORT_NAME = SCEN_SHORT_NAME,
                                                   nsamples = NSAMPLES,
                                                   individual_para = FALSE)
        
        evppi_agesex_for_CO2_city2 <- do.call(rbind,evppi_agesex_for_CO2_city) # bind list
        evppi_agesex_for_CO2_city3 <- as.data.frame(evppi_agesex_for_CO2_city2) # turn into dataframe
        
        evppi_agesex_for_AP_outcome_names <- strsplit(colnames(city_agesex_outcomes),paste("_",city,sep="")) # add column names without city part
        colnames(evppi_agesex_for_CO2_city3) <- paste(evppi_agesex_for_AP_outcome_names, age_gender, sep = "_")
        
        # replace columns for which the outcomes where originally NA by NA again
        if (length(city_agesex_outcomes_na_cols)>0){
          city_agesex_outcomes_na_cols2 <- strsplit(city_agesex_outcomes_na_cols,paste("_",city,sep=""))
          city_agesex_outcomes_na_cols3 <- paste(city_agesex_outcomes_na_cols2,age_gender, sep = "_")
          evppi_agesex_for_CO2_city3[,city_agesex_outcomes_na_cols3] <- NaN
        }
        
        
        # repeat for PM
        evppi_agesex_for_PM_city <- future_lapply(1,
                                                  FUN = ithimr:::compute_evppi,
                                                  global_para = data.frame(),
                                                  city_para = city_PM_parasampl,
                                                  city_outcomes = city_agesex_outcomes,
                                                  dr_pif = data.frame(),
                                                  outcome_voi_list = outcome_voi_list,
                                                  SCEN_SHORT_NAME = SCEN_SHORT_NAME,
                                                  nsamples = NSAMPLES,
                                                  individual_para = FALSE)
        
        evppi_agesex_for_PM_city2 <- do.call(rbind,evppi_agesex_for_PM_city) # bind list
        evppi_agesex_for_PM_city3 <- as.data.frame(evppi_agesex_for_PM_city2) # turn into dataframe
        
        colnames(evppi_agesex_for_PM_city3) <- paste(evppi_agesex_for_AP_outcome_names, age_gender, sep = "_")
        
        # replace columns for which the outcomes where originally NA by NA again
        if (length(city_agesex_outcomes_na_cols)>0){
          city_agesex_outcomes_na_cols2 <- strsplit(city_agesex_outcomes_na_cols,paste("_",city,sep=""))
          city_agesex_outcomes_na_cols3 <- paste(city_agesex_outcomes_na_cols2,age_gender, sep = "_")
          evppi_agesex_for_PM_city3[,city_agesex_outcomes_na_cols3] <- NaN
        }
        

        
        if(k == 1){
          evppi_agesex_CO2_city_df <- evppi_agesex_for_CO2_city3
          evppi_agesex_PM_city_df <- evppi_agesex_for_PM_city3
        } else{
          evppi_agesex_CO2_city_df <- cbind(evppi_agesex_CO2_city_df, evppi_agesex_for_CO2_city3)
          evppi_agesex_PM_city_df <- cbind(evppi_agesex_PM_city_df, evppi_agesex_for_PM_city3)
        }
        k <- k + 1
      }
      
      
      evppi_agesex_CO2_city_df$parameters <-  c(paste0('CO2_emissions_inventory')) # add parameter name column
      evppi_agesex_PM_city_df$parameters <-  c(paste0('PM_emissions_inventory')) # add parameter name column

      
      # combine CO2 and PM data
      evppi_agesex_AP_city_df <- rbind(evppi_agesex_CO2_city_df,evppi_agesex_PM_city_df)
      
      evppi_agesex_AP_city_df$city <- city # add city name column
      
      evppi_agesex_city_df <- rbind(evppi_agesex_city_df,evppi_agesex_AP_city_df)
      
    } # end of sample >= 1000 loop
    
    
    
    # change structure of evppi_agesex_city_df to make it more flexible when different age categories are used for different cities
    # create one df for each city
    # assign(paste0("evppi_agesex_",city_name,'_df'), evppi_agesex_city_df) 
    # 
    # evppi_city_list_all[[ci]] <- get(paste0("evppi_agesex_",city_name,'_df'))
    
    evppi_agesex_city_df_rearranged <- data.frame()
    k <- 1
    
    for (ag in age_gender_cat){
      ag_colnames <- sapply(colnames(evppi_agesex_city_df),function(x)grepl(paste0("_",ag),x)) # find column names depending on age and gender
      ag_columns_df <- evppi_agesex_city_df[,ag_colnames] # only keep those columns that are dependent on age and gender
      new_col_names <- sapply(colnames(ag_columns_df),function(x)strsplit(x,paste0('_',ag))[[1]]) # remove age and gender part from those columns
      colnames(ag_columns_df) = new_col_names
      
      ag_columns_df$parameters <- evppi_agesex_city_df$parameters
      ag_columns_df$city <- evppi_agesex_city_df$city
      ag_columns_df$gender <- strsplit(ag, " ")[[1]][1]
      ag_columns_df$age <- strsplit(ag, " ")[[1]][2]
      ag_columns_df$age_gender <- ag
      if (k == 1){
        evppi_agesex_city_df_rearranged <- ag_columns_df
      }else{
        evppi_agesex_city_df_rearranged <- rbind(evppi_agesex_city_df_rearranged, ag_columns_df)
      }
      k <- k +1
    }
    
    evppi_agesex_df <- rbind(evppi_agesex_df, evppi_agesex_city_df_rearranged) # add to total evppi dataframe
    
    ci <- ci + 1
    
    # remove city from parameter names
    evppi_agesex_df$parameters <- unlist(strsplit(as.character(evppi_agesex_df$parameters),paste("_",city,sep="")))
    
    
  } # end of city loop
  
  
  # merge with parameter classification
  para_classification <- read.csv(paste0(global_path, "voi/parameter_explanations.csv"))
  #para_classification <- read.csv("inst/extdata/global/voi/parameter_explanations.csv")
  
  evppi_agesex_df <- merge(x = para_classification, y = evppi_agesex_df, by = 'parameters', all.y = TRUE)
  
  evppi_agesex_df2 <- evppi_agesex_df %>% arrange(ordering) %>% dplyr::select(-c(ordering))
  
  
  # change order of columns such that ordered by scenario and demographic group
  evppi_agesex_df2 <- evppi_agesex_df2 %>% relocate(city,classification, parameters, parameters_lowerCase, age, gender, age_gender) # change order of columns
  
  # re-order rows
  evppi_agesex_df2 <- evppi_agesex_df2[order(evppi_agesex_df2$city, evppi_agesex_df2$age_gender),]
  
  
  
  return(list(evppi_agesex_df2, age_gender_cat))
  
}
