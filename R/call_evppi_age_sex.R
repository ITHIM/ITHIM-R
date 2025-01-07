#' EVPPI wrapper function for total population YLLs by sex and age
#' 
#' This function gets the ithim results into the correct format and calls the \code{\link{compute_evppi()}} script
#' to calculate the EVPPIs for all input parameters and required outcomes and scenarios split by sex and age.
#'
#' The function performs the following steps:
#' 
#'\itemize{
#'\item create a vector containing the global parameters but not including the emission inventory 
#'      parameters as they are not independent of each other
#'
#'\item loop through the cities:
#'  \itemize{
#'    \item extract the city specific parameters (excluding the CO2 and PM emission inventory parameters)
#'    \item loop through all age and sex categories:
#'      \itemize{   
#'        \item extract the outcomes of interest for each scenario using the outcome_voi_list
#'        \item if voi_add_sum == TRUE, calculate the total YLLs by summing across all 
#'          diseases in the outcome_voi_list - this only makes sense if the diseases
#'          in the outcome_voi_list are independent of each other
#'        \item call the \code{\link{compute_evppi()}} function to calculate the expected values of partially perfect information (EVPPI)
#'          for all parameters and diseases of interest
#'        }
#'    \item if NSAMPLES >= 1000 then also calculate the EVPPI values for the emission inventory parameters by looping through 
#'          all age and sex categories
#'    } 
#' }  
#' 
#' 
#' 
#' @param voi_data_all_df dataframe containing all outcomes by age and sex
#' @param parameter_samples table containing all the input parameter variables for the different model runs for all cities
#' @param outcome_voi_list vector detailing the outcomes to be considered in the VoI analysis
#' @param outcome total yll outcome for all outcome age categories per city and scenario and disease combination, also combined city result (sum)
#' @param cities vector of cities
#' @param voi_add_sum if the sum of YLLs across all disease outcomes is to be considered
#' @param NSCEN number of scenarios (not incl. baseline)
#' @param NSAMPLES number of times the model was run for each city
#' @param scenario_names gives the names of the scenarios (incl baseline)
#' @param evppi_df outcome dataframe containing voi analysis for total yll across all age and sex categories
#'
#' @return evppi_agesex_df dataframe containing all EVPPI outcomes for all age and sex categories and all cities
#' @return age_gender_cat vector with all age and sex categories
#' @return evppi_city_list_all list where each list entry is a dataframe containing all EvPPI outcomes for all age and sex categories for one city
#' 
#' @export


call_evppi_age_sex <- function(voi_data_all_df,parameter_samples, outcome_voi_list, outcome, cities, voi_add_sum, NSCEN, NSAMPLES,
                       scenario_names , evppi_df){
  
  evppi_agesex_df <- data.frame()
  
  # first extract input parameters of interest
  # create list with global parameters that are relevant for all cities
  general_inputs <- sapply(colnames(parameter_samples),function(x)!grepl(paste(cities, collapse = "|"),x))
  general_parsampl <- parameter_samples[,general_inputs]
  
  scen_names_only <- scenario_names[1:NSCEN+1]
  
  evppi_city_list_all <- list()
  ci <- 1
  
  for (city in cities){
    print(city)
    
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
    city_agesex_out <- voi_data_all_df %>% filter(city == city_name)
    age_gender_cat <- unique(city_agesex_out$age_sex)
    
    k <- 1
    
    for(age_gender in age_gender_cat){
      
      city_agesex_out2 <- city_agesex_out %>% filter(age_sex == age_gender)
      city_agesex_outputs <- sapply(colnames(city_agesex_out2),function(x)grepl(paste(outcome_voi_list, collapse = "|"),x))
      city_agesex_outcomes <- city_agesex_out2[,city_agesex_outputs]
      
      if(voi_add_sum){
        # add total result for each scenario - only makes sense if results are independent of each other
        # i.e. combining e.g. "total_cancer" with "lung_cancer" results in double-counting and invalid VOI analysis for the sum
        for (n in 1:NSCEN){
          scen_outputs <- sapply(colnames(city_agesex_outcomes), function(x)grepl(scen_names_only[n],x))
          if (length(outcome_voi_list) == 1){
            city_agesex_outcomes[paste0(scen_names_only[n],"_ylls_sum_",city)] <- sapply(city_agesex_outcomes[,scen_outputs], unlist)
          } else{
            city_agesex_outcomes[paste0(scen_names_only[n],"_ylls_sum_",city)] <- rowSums(sapply(city_agesex_outcomes[,scen_outputs], unlist))
          }
        }
      }
      
      # calculate the total number of independent parameters to be considered in the VoI analysis
      param_no <- ncol(city_parsampl) + ncol(general_parsampl)
      
      # replace NA with 0s, note that the evppi analysis returns NA for all outcomes that are all 0
      city_agesex_outcomes_na_cols <- names(which(colSums(is.na(city_agesex_outcomes))>0)) # record colnames
      city_agesex_outcomes[is.na(city_agesex_outcomes)] <- 0 # replace NAs with 0
      
      # calculate evppi
      evppi_agesex_city <- future_lapply(1:param_no, # calculate the evppi for each city
                                         FUN = ithimr::compute_evppi,
                                         global_para = as.data.frame(general_parsampl),
                                         city_para = as.data.frame(city_parsampl),
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
    
    evppi_agesex_city_df$parameters <-  c(colnames(general_parsampl), colnames(city_parsampl)) # add parameter name column
    evppi_agesex_city_df$city <- city # add city name column
    
    
    
    #look at CO2 and PM emission inventories separately
    if(NSAMPLES>=1000){
      
      k <- 1
      
      for(age_gender in age_gender_cat){
        
        city_agesex_out2 <- city_agesex_out %>% filter(age_sex == age_gender)
        city_agesex_outputs <- sapply(colnames(city_agesex_out2),function(x)grepl(paste(outcome_voi_list, collapse = "|"),x))
        city_agesex_outcomes <- city_agesex_out2[,city_agesex_outputs]
        
        
        if(voi_add_sum){
          # add total result for each scenario - only makes sense if results are independent of each other
          # i.e. combining e.g. "total_cancer" with "lung_cancer" results in double-counting and invalid VOI analysis for the sum
          for (n in 1:NSCEN){
            scen_outputs <- sapply(colnames(city_agesex_outcomes), function(x)grepl(scen_names_only[n],x))
            if (length(outcome_voi_list) == 1){
              city_agesex_outcomes[paste0(scen_names_only[n],"_ylls_sum_",city)] <- sapply(city_agesex_outcomes[,scen_outputs], unlist)
            } else{
              city_agesex_outcomes[paste0(scen_names_only[n],"_ylls_sum_",city)] <- rowSums(sapply(city_agesex_outcomes[,scen_outputs], unlist))
            }
          }
        }
        
        # replace NA with 0s, note that the evppi analysis returns NA for all outcomes that are all 0
        city_agesex_outcomes_na_cols <- names(which(colSums(is.na(city_agesex_outcomes))>0)) # record colnames
        city_agesex_outcomes[is.na(city_agesex_outcomes)] <- 0 # replace NAs with 0
        
        evppi_agesex_for_CO2_city <- future_lapply(1,
                                                   FUN = ithimr:::compute_evppi,
                                                   global_para = data.frame(),
                                                   city_para = city_Co2_parasampl,
                                                   city_outcomes = city_agesex_outcomes,
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
    assign(paste0("evppi_agesex_",city_name,'_df'), evppi_agesex_city_df) 

    evppi_city_list_all[[ci]] <- get(paste0("evppi_agesex_",city_name,'_df'))
    
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
    
  } # end of city loop
  
  
  # merge with evppi_df data
  evppi_df$gender <- 'all'
  evppi_df$age <- 'all'
  evppi_df$age_gender <- 'all'
  evppi_agesex_df <- rbind(evppi_agesex_df,evppi_df)
  
  
  # change order of columns such that ordered by scenario and demographic group
  evppi_agesex_df <- evppi_agesex_df %>% relocate(city,parameters, age, gender, age_gender)
  
  # re-order rows
  evppi_agesex_df <- evppi_agesex_df[order(evppi_agesex_df$city, evppi_agesex_df$age_gender),]
  
  
  
  return(list(evppi_agesex_df, age_gender_cat,evppi_city_list_all))
  
}
