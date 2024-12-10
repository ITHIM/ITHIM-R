#' EVPPI wrapper function for total population YLLs
#' 
#' This function gets the ithim results into the correct format and calls the compute_evppi.R script
#' to calculate the EVPPIs for all input parameters and required outcomes and scenarios for the entire
#' population considered in the model
#'
#' The function performs the following steps:
#' 
#'\itemize{
#'\item create a vector containing the global parameters but not including the AP dose response 
#'      parameters as they are not independent of each other
#'
#'\item loop through the cities:
#'  \itemize{
#'    \item extract the city specific parameters (excluding the CO2 parameters)
#'    \item extract the outcomes of interest for each scenario using the outcome_voi_list
#'    \item if voi_add_sum == TRUE, calculate the total YLLs by summing across all 
#'          diseases in the outcome_voi_list - this only makes sense if the diseases
#'          in the outcome_voi_list are independent of each other
#'    \item call the compute_evppi.R function to calculate the expected values of partially perfect information (EVPPI)
#'          for all parameters and diseases of interest
#'    \item if NSAMPLES >= 300 and the AP dose response paramters have been sampled, then also
#'          calculate the EVPPI values for the AP dose response parameters 
#'    } 
#' }  
#' 
#' @param parameter_samples table containing all the input parameter variables for the different model runs for all cities
#' @param outcome_voi_list vector detailing the outcomes to be considered in the VoI analysis
#' @param outcome total yll outcome for all outcome age categories per city and scenario and disease combination, also combined city result (sum)
#' @param cities vector of cities
#' @param voi_add_sum if the sum of YLLs across all disease outcomes is to be considered
#' @param NSCEN number of scenarios (not incl. baseline)
#' @param NSAMPLES number of times the model was run for each city
#' @param scenario_names gives the names of the scenarios (incl baseline)
#' 
#' @return evppi_df containing the evppi values for all input parameters and scenario and disease outcomes
#' 
#' @export


call_evppi <- function(parameter_samples, outcome_voi_list, outcome, cities, voi_add_sum, NSCEN, NSAMPLES,
                       scenario_names){
  
 
  # first extract input parameters of interest
  # create list with global parameters that are relevant for all cities
  general_inputs <- sapply(colnames(parameter_samples),function(x)!grepl(paste(cities, collapse = "|"),x))
  general_parsampl <- parameter_samples[,general_inputs]
  
 
  # set up dataframe to contain the final evppi results
  evppi_df <- data.frame()
  
  # extract city specific input parameters
  for (city in cities){ # loop through cities
    
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
    

    # extract the required outcomes for each city
    city_out <- as.data.frame(outcome[[city]]) # take total YLLs for each scenario and disease combination
    # extract outcomes required as set up in outcome_voi_list
    city_outputs <- sapply(colnames(city_out),function(x)grepl(paste(outcome_voi_list, collapse = "|"),x))
    city_outcomes <- city_out[,city_outputs]
    
    scen_names_only <- scenario_names[1:NSCEN+1]
    
    if(voi_add_sum){
      for (n in 1:NSCEN){
        scen_outputs <- sapply(colnames(city_outcomes), function(x)grepl(scen_names_only[n],x))
        if (length(outcome_voi_list) == 1){
          city_outcomes[paste0(scen_names_only[n],"_ylls_sum_",city)] <- city_outcomes[,scen_outputs]
        } else{
          city_outcomes[paste0(scen_names_only[n],"_ylls_sum_",city)] <- rowSums(city_outcomes[,scen_outputs])
        }
      }
    }

    
    # calculate the total number of independent parameters to be considered in the VoI analysis
    param_no <- ncol(city_parsampl) + ncol(general_parsampl)
    
    
    # calculate the evppi for each city (still within the city loop)
    evppi_city <- future_lapply(1:param_no, 
                                FUN = ithimr::compute_evppi,
                                global_para = as.data.frame(general_parsampl),
                                city_para = as.data.frame(city_parsampl),
                                city_outcomes = city_outcomes,
                                nsamples = NSAMPLES)
    
    # for (i in 1:param_no){
    #   print(i)
    #   out <- compute_evppi(i, global_para,city_para,city_outcomes, nsamples)
    # }
    
    
    
    evppi_city2 <- do.call(rbind,evppi_city) # bind list
    
    evppi_city3 <- as.data.frame(evppi_city2) # turn into dataframe
    
    # Manipulate evppi_city3 df into correct format
    # add column names without city part
    evppi_outcome_names <- strsplit(colnames(city_outcomes),paste("_",city,sep="")) 
    colnames(evppi_city3) <- evppi_outcome_names
    #evppi_outcome_names <- colnames(evppi_city3)
    
    evppi_city3$parameters <-  c(colnames(general_parsampl), colnames(city_parsampl)) # add parameter name column
    evppi_city3$city <- city # add city name column
    
    
    #look at CO2 and PM emission inventories separately
    if(NSAMPLES>=1000){
      
      # first consider CO2 parameters
      evppi_for_CO2_city <- future_lapply(1,
                                         FUN = ithimr:::compute_evppi,
                                         global_para = data.frame(),
                                         city_para = city_Co2_parasampl,
                                         city_outcomes = city_outcomes,
                                         nsamples = NSAMPLES,
                                         individual_para = FALSE)
    
      evppi_for_CO2_city2 <- do.call(rbind,evppi_for_CO2_city) # bind list
      evppi_for_CO2_city3 <- as.data.frame(evppi_for_CO2_city2) # turn into dataframe
      colnames(evppi_for_CO2_city3) <- evppi_outcome_names
    
      evppi_for_CO2_city3$parameters <-  c(paste0('CO2_emissions_inventory')) # add parameter name column
      evppi_for_CO2_city3$city <- city # add city name column
    
      evppi_city3 <- rbind(evppi_city3,evppi_for_CO2_city3)
      
      
      # consider PM parameters
      evppi_for_PM_city <- future_lapply(1,
                                          FUN = ithimr:::compute_evppi,
                                          global_para = data.frame(),
                                          city_para = city_PM_parasampl,
                                          city_outcomes = city_outcomes,
                                          nsamples = NSAMPLES,
                                          individual_para = FALSE)
      
      evppi_for_PM_city2 <- do.call(rbind,evppi_for_PM_city) # bind list
      evppi_for_PM_city3 <- as.data.frame(evppi_for_PM_city2) # turn into dataframe
      colnames(evppi_for_PM_city3) <- evppi_outcome_names
      
      evppi_for_PM_city3$parameters <-  c(paste0('PM_emissions_inventory')) # add parameter name column
      evppi_for_PM_city3$city <- city # add city name column
      
      evppi_city3 <- rbind(evppi_city3,evppi_for_PM_city3)
    }
    
    
        
    
    # look at dose response AP input parameters separately, as alpha, beta, gammy and trmel are dependent on each other
    # if(any(ap_dr_quantile)&&NSAMPLES>=300){
    #   AP_names <- sapply(colnames(parameter_samples),function(x)length(strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA')[[1]])>1)
    #   diseases <- sapply(colnames(parameter_samples)[AP_names],function(x)strsplit(x,'AP_DOSE_RESPONSE_QUANTILE_ALPHA_')[[1]][2])
    #   sources <- list()
    #   for(di in diseases){ 
    #     col_names <- sapply(colnames(parameter_samples),function(x)grepl('AP_DOSE_RESPONSE_QUANTILE',x)&grepl(di,x))
    #     sources[[di]] <- parameter_samples[,col_names]
    #   }
    #   evppi_for_AP_city <- future_lapply(1:length(sources),
    #                                      FUN = ithimr:::compute_evppi,
    #                                      global_para = sources,
    #                                      city_para = data.frame(),
    #                                      city_outcomes = city_outcomes,
    #                                      nsamples = NSAMPLES)
    #   
    #   evppi_for_AP_city2 <- do.call(rbind,evppi_for_AP_city) # bind list
    #   evppi_for_AP_city3 <- as.data.frame(evppi_for_AP_city2) # turn into dataframe
    #   colnames(evppi_for_AP_city3) <- evppi_outcome_names
    #   
    #   evppi_for_AP_city3$parameters <-  c(paste0('AP_DOSE_RESPONSE_QUANTILE_',diseases)) # add parameter name column
    #   evppi_for_AP_city3$city <- city # add city name column
    #   
    #   evppi_city3 <- rbind(evppi_city3, evppi_for_AP_city3)
    # }
    
    evppi_df <- rbind(evppi_df, evppi_city3) # add to total evppi dataframe
  } # end of city loop
  
  
  evppi_df <- evppi_df %>% relocate(city,parameters) # change order of columns
  
  return(list(evppi_df, evppi_outcome_names))
  
}
