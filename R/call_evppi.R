#' EVPPI wrapper function for total population YLLs
#' 
#' This function gets the ithim results into the correct format and calls the \code{\link{compute_evppi()}} script
#' to calculate the EVPPIs for all input parameters and required outcomes and scenarios for the entire
#' population considered in the model
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
#'    \item extract the outcomes of interest for each scenario using the outcome_voi_list
#'    \item if voi_add_sum == TRUE, calculate the total YLLs by summing across all 
#'          diseases in the outcome_voi_list - this only makes sense if the diseases
#'          in the outcome_voi_list are independent of each other
#'    \item call the \code{\link{compute_evppi()}} function to calculate the expected values of partially perfect information (EVPPI)
#'          for all parameters and diseases of interest
#'    \item if NSAMPLES >= 1000 then also calculate the EVPPI values for the emission inventory parameters
#'    } 
#' }  
#' 
#' @param parameter_samples table containing all the input parameter variables for the different model runs for all cities
#' @param outcome_voi_list vector detailing the outcomes to be considered in the VoI analysis
#' @param voi_complete_df total yll outcome for all outcome age categories per city and scenario and disease combination, also combined city result (sum)
#' @param cities vector of cities
#' @param voi_add_sum if the sum of YLLs across all disease outcomes is to be considered
#' @param NSCEN number of scenarios (not incl. baseline)
#' @param NSAMPLES number of times the model was run for each city
#' @param scenario_names gives the names of the scenarios (incl baseline)
#' 
#' @return list containg the following elements:
#' @return evppi_df containing the evppi values for all input parameters and scenario and disease outcomes
#' @return evppi_outcome_names, a vector containing all the scenario outcome combinations
#' 
#' @export


call_evppi <- function(parameter_samples, outcome_voi_list, voi_complete_df, cities, voi_add_sum, NSCEN, NSAMPLES,
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
