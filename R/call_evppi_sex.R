#' EVPPI wrapper function for total population YLLs by sex
#' 
#' This function gets the ithim results into the correct format and calls the \code{\link{compute_evppi()}} script
#' to calculate the EVPPIs for all input parameters and required outcomes and scenarios split by sex.
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
#'    \item loop through the two sexes:
#'      \itemize{   
#'        \item extract the outcomes of interest for each scenario using the outcome_voi_list
#'        \item if voi_add_sum == TRUE, calculate the total YLLs by summing across all 
#'          diseases in the outcome_voi_list - this only makes sense if the diseases
#'          in the outcome_voi_list are independent of each other
#'        \item call the \code{\link{compute_evppi()}} function to calculate the expected values of partially perfect information (EVPPI)
#'          for all parameters and diseases of interest
#'        }
#'    \item if NSAMPLES >= 1000 then also calculate the EVPPI values for the emission inventory parameters by looping through both sexes
#'    } 
#' }  
#' 
#' 
#' @param voi_complete_df dataframe containing all outcomes 
#' @param parameter_samples table containing all the input parameter variables for the different model runs for all cities
#' @param outcome_voi_list vector detailing the outcomes to be considered in the VoI analysis
#' @param cities vector of cities
#' @param voi_add_sum if the sum of YLLs across all disease outcomes is to be considered
#' @param NSCEN number of scenarios (not incl. baseline)
#' @param NSAMPLES number of times the model was run for each city
#' @param scenario_names gives the names of the scenarios (incl baseline)
#' 
#' @return a list containing the following elements:
#' @return evppi_ex_df dataframe containing all EVPPI outcomes for all sex categories and all cities
#' @return sex_cat vector with both sexes
#' @return evppi_city_list_all_sex list where each list entry is a dataframe containing all EvPPI outcomes for all sex categories for one city
#' 
#' 
#' @export


call_evppi_sex <- function(voi_complete_df, parameter_samples, outcome_voi_list, cities, voi_add_sum, NSCEN, NSAMPLES,
                       scenario_names){
  
  evppi_sex_df <- data.frame()
  
  # first extract input parameters of interest
  # create list with global parameters that are relevant for all cities
  general_inputs <- sapply(colnames(parameter_samples),function(x)!grepl(paste(cities, collapse = "|"),x))
  general_parsampl <- parameter_samples[,general_inputs]
  
  scen_names_only <- scenario_names[1:NSCEN+1]
  
  evppi_city_list_all_sex <- list()
  ci <- 1
  
  for (city in cities){
    #print(city)
    
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
    
    # extract the required outcomes for each city - loop sex categories
    city_name <- city
    city_sex_out <- voi_complete_df %>% filter(city == city_name & age_cat== 'all' & !sex=='all')
    sex_cat <- unique(city_sex_out$sex)
    
    k <- 1
    
    for(s in sex_cat){
      # s <- 'male'
      city_sex_out2 <- city_sex_out %>% filter(sex == s)
      city_sex_outputs <- sapply(colnames(city_sex_out2),function(x)grepl(paste(outcome_voi_list, collapse = "|"),x))
      city_sex_outcomes <- city_sex_out2[,city_sex_outputs]
      
      if(voi_add_sum){
        # add total result for each scenario - only makes sense if results are independent of each other
        # i.e. combining e.g. "total_cancer" with "lung_cancer" results in double-counting and invalid VOI analysis for the sum
        for (n in 1:NSCEN){
          scen_outputs <- sapply(colnames(city_sex_outcomes), function(x)grepl(scen_names_only[n],x))
          if (length(outcome_voi_list) == 1){
            city_sex_outcomes[paste0(scen_names_only[n],"_ylls_sum_",city)] <- sapply(city_sex_outcomes[,scen_outputs], unlist)
          } else{
            city_sex_outcomes[paste0(scen_names_only[n],"_ylls_sum_",city)] <- rowSums(sapply(city_sex_outcomes[,scen_outputs], unlist))
          }
        }
      }
      
      # calculate the total number of independent parameters to be considered in the VoI analysis
      param_no <- ncol(city_parsampl) + ncol(general_parsampl)
      
      # replace NA with 0s, note that the evppi analysis returns NA for all outcomes that are all 0
      city_sex_outcomes_na_cols <- names(which(colSums(is.na(city_sex_outcomes))>0)) # record colnames
      city_sex_outcomes[is.na(city_sex_outcomes)] <- 0 # replace NAs with 0
      
      # calculate evppi
      evppi_sex_city <- future_lapply(1:param_no, # calculate the evppi for each city
                                         FUN = ithimr::compute_evppi,
                                         global_para = as.data.frame(general_parsampl),
                                         city_para = as.data.frame(city_parsampl),
                                         city_outcomes = city_sex_outcomes,
                                         nsamples = NSAMPLES)
      
      
      evppi_sex_city2 <- do.call(rbind,evppi_sex_city)
      evppi_sex_city3 <- as.data.frame(evppi_sex_city2) # turn into dataframe
      
      evppi_sex_outcome_names <- strsplit(colnames(city_sex_outcomes),paste("_",city,sep="")) # add column names without city part
      colnames(evppi_sex_city3) <- paste(evppi_sex_outcome_names, s, sep = "_")
      evppi_sex_outcome_names <- colnames(evppi_sex_city3)
      
      # replace columns for which the outcomes where originally NA by NA again
      if (length(city_sex_outcomes_na_cols)>0){
        city_sex_outcomes_na_cols2 <- strsplit(city_sex_outcomes_na_cols,paste("_",city,sep=""))
        city_sex_outcomes_na_cols3 <- paste(city_sex_outcomes_na_cols2,sex, sep = "_")
        
        evppi_sex_city3[,city_sex_outcomes_na_cols3] <- NaN
      }
      
      if(k == 1){
        evppi_sex_city_df <- evppi_sex_city3
      }else{
        evppi_sex_city_df <- cbind(evppi_sex_city_df, evppi_sex_city3)
      }
      
      k <- k + 1
    } # end of sex categories
    
    
    evppi_sex_city_df$parameters <-  c(colnames(general_parsampl), colnames(city_parsampl)) # add parameter name column
    evppi_sex_city_df$city <- city # add city name column
    
    
    
    #look at CO2 and PM emission inventories separately
    if(NSAMPLES>=1000){
      
      k <- 1
      
      for(s in sex_cat){
        
        city_sex_out2 <- city_sex_out %>% filter(sex == s)
        city_sex_outputs <- sapply(colnames(city_sex_out2),function(x)grepl(paste(outcome_voi_list, collapse = "|"),x))
        city_sex_outcomes <- city_sex_out2[,city_sex_outputs]
        
        
        if(voi_add_sum){
          # add total result for each scenario - only makes sense if results are independent of each other
          # i.e. combining e.g. "total_cancer" with "lung_cancer" results in double-counting and invalid VOI analysis for the sum
          for (n in 1:NSCEN){
            scen_outputs <- sapply(colnames(city_sex_outcomes), function(x)grepl(scen_names_only[n],x))
            if (length(outcome_voi_list) == 1){
              city_sex_outcomes[paste0(scen_names_only[n],"_ylls_sum_",city)] <- sapply(city_sex_outcomes[,scen_outputs], unlist)
            } else{
              city_sex_outcomes[paste0(scen_names_only[n],"_ylls_sum_",city)] <- rowSums(sapply(city_sex_outcomes[,scen_outputs], unlist))
            }
          }
        }
        
        # replace NA with 0s, note that the evppi analysis returns NA for all outcomes that are all 0
        city_sex_outcomes_na_cols <- names(which(colSums(is.na(city_sex_outcomes))>0)) # record colnames
        city_sex_outcomes[is.na(city_sex_outcomes)] <- 0 # replace NAs with 0
        
        evppi_sex_for_CO2_city <- future_lapply(1,
                                                   FUN = ithimr:::compute_evppi,
                                                   global_para = data.frame(),
                                                   city_para = city_Co2_parasampl,
                                                   city_outcomes = city_sex_outcomes,
                                                   nsamples = NSAMPLES,
                                                   individual_para = FALSE)
        
        evppi_sex_for_CO2_city2 <- do.call(rbind,evppi_sex_for_CO2_city) # bind list
        evppi_sex_for_CO2_city3 <- as.data.frame(evppi_sex_for_CO2_city2) # turn into dataframe
        
        evppi_sex_for_AP_outcome_names <- strsplit(colnames(city_sex_outcomes),paste("_",city,sep="")) # add column names without city part
        colnames(evppi_sex_for_CO2_city3) <- paste(evppi_sex_for_AP_outcome_names, s, sep = "_")
        
        # replace columns for which the outcomes where originally NA by NA again
        if (length(city_sex_outcomes_na_cols)>0){
          city_sex_outcomes_na_cols2 <- strsplit(city_sex_outcomes_na_cols,paste("_",city,sep=""))
          city_sex_outcomes_na_cols3 <- paste(city_sex_outcomes_na_cols2,s, sep = "_")
          evppi_sex_for_CO2_city3[,city_sex_outcomes_na_cols3] <- NaN
        }
        
        
        # repeat for PM
        evppi_sex_for_PM_city <- future_lapply(1,
                                                  FUN = ithimr:::compute_evppi,
                                                  global_para = data.frame(),
                                                  city_para = city_PM_parasampl,
                                                  city_outcomes = city_sex_outcomes,
                                                  nsamples = NSAMPLES,
                                                  individual_para = FALSE)
        
        evppi_sex_for_PM_city2 <- do.call(rbind,evppi_sex_for_PM_city) # bind list
        evppi_sex_for_PM_city3 <- as.data.frame(evppi_sex_for_PM_city2) # turn into dataframe
        
        colnames(evppi_sex_for_PM_city3) <- paste(evppi_sex_for_AP_outcome_names, s, sep = "_")
        
        # replace columns for which the outcomes where originally NA by NA again
        if (length(city_sex_outcomes_na_cols)>0){
          city_sex_outcomes_na_cols2 <- strsplit(city_sex_outcomes_na_cols,paste("_",city,sep=""))
          city_sex_outcomes_na_cols3 <- paste(city_sex_outcomes_na_cols2,s, sep = "_")
          evppi_sex_for_PM_city3[,city_sex_outcomes_na_cols3] <- NaN
        }
        

        
        if(k == 1){
          evppi_sex_CO2_city_df <- evppi_sex_for_CO2_city3
          evppi_sex_PM_city_df <- evppi_sex_for_PM_city3
        } else{
          evppi_sex_CO2_city_df <- cbind(evppi_sex_CO2_city_df, evppi_sex_for_CO2_city3)
          evppi_sex_PM_city_df <- cbind(evppi_sex_PM_city_df, evppi_sex_for_PM_city3)
        }
        k <- k + 1
      }
      
      
      evppi_sex_CO2_city_df$parameters <-  c(paste0('CO2_emissions_inventory')) # add parameter name column
      evppi_sex_PM_city_df$parameters <-  c(paste0('PM_emissions_inventory')) # add parameter name column

      
      # combine CO2 and PM data
      evppi_sex_AP_city_df <- rbind(evppi_sex_CO2_city_df,evppi_sex_PM_city_df)
      
      evppi_sex_AP_city_df$city <- city # add city name column
      
      evppi_sex_city_df <- rbind(evppi_sex_city_df,evppi_sex_AP_city_df)
      
    } # end of sample >= 1000 loop
    
    
    
    # change structure of evppi_sex_city_df to make it more flexible when different sex categories are used for different cities
    # create one df for each city
    assign(paste0("evppi_sex_",city_name,'_df'), evppi_sex_city_df) 

    evppi_city_list_all_sex[[ci]] <- get(paste0("evppi_sex_",city_name,'_df'))
    
    evppi_sex_city_df_rearranged <- data.frame()
    k <- 1
    
    for (ag in sex_cat){
      ag_colnames <- sapply(colnames(evppi_sex_city_df),function(x)grepl(paste0("_",ag),x)) # find column names depending sex
      ag_columns_df <- evppi_sex_city_df[,ag_colnames] # only keep those columns that are dependent on sex
      new_col_names <- sapply(colnames(ag_columns_df),function(x)strsplit(x,paste0('_',ag))[[1]]) # remove gender part from those columns
      colnames(ag_columns_df) = new_col_names
      
      ag_columns_df$parameters <- evppi_sex_city_df$parameters
      ag_columns_df$city <- evppi_sex_city_df$city
      ag_columns_df$gender <- ag
     if (k == 1){
        evppi_sex_city_df_rearranged <- ag_columns_df
      }else{
        evppi_sex_city_df_rearranged <- rbind(evppi_sex_city_df_rearranged, ag_columns_df)
      }
      k <- k +1
    }
    
    evppi_sex_df <- rbind(evppi_sex_df, evppi_sex_city_df_rearranged) # add to total evppi dataframe
    
    ci <- ci + 1
    
  } # end of city loop
  
  
  # # merge with evppi_df data
  # evppi_df$gender <- 'all'
  # #evppi_df$age <- 'all'
  # #evppi_df$age_gender <- 'all'
  # evppi_sex_df <- rbind(evppi_sex_df,evppi_df)
  # 
  
  # change order of columns such that ordered by scenario and demographic group
  evppi_sex_df <- evppi_sex_df %>% relocate(city,parameters, gender)
  
  # re-order rows
  evppi_sex_df <- evppi_sex_df[order(evppi_sex_df$city, evppi_sex_df$gender),]
  
  
  
  return(list(evppi_sex_df, sex_cat,evppi_city_list_all_sex))
  
}
