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
#'    \item calculate total yll outcome across all outcome age categories per city and scenario and disease combinations
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
#' 
#' @return ithim_results list with the following objects:
#' @return summary_ylls_df: dateframe with total ylls (median, 5th and 95th percentiles) per age group and city (plus combined results)
#' @return voi_data_all_df: dataframe for all cities with all outcomes for all model runs, age groups and disease and scenario combinations
#' @return yll_per_hundred_thousand: yll per 100,000 people for each city, outcome age category, model run and disease and scen combination
#' @return yll_per_hundred_thousand_stats: total ylls per 100,000 (median, 5th and 95th percentiles) as sum across all disease per outcome age group, scenario and city (plus combined results)
#' @return outcome: total yll outcome for all outcome age categories per city and scenario and disease combination, also combined city result (sum)
#' 
#' @export


extract_data_for_voi <- function(NSCEN, NSAMPLES, SCEN_SHORT_NAME,outcome_age_groups,cities,multi_city_ithim){
  
  
  # initialise dataframe for all cities with all outcomes for all model runs, age groups and disease and scenario combinations
  voi_data_all <- list()
  voi_data_all_df <- data.frame()
  
  age_pops <- list()
  age_populations <- rep(0,length(outcome_age_groups))
  
  # create matrix of 0s with the number of cities as rows and the length of outcome age groups as columns
  city_populations <- matrix(0,nrow=length(cities),ncol=length(outcome_age_groups))
  
  for(ci in 1:length(cities)){ # loop through cities
    city <- cities[ci]
    multi_city_ithim[[city]] <- readRDS(paste0('results/multi_city/',city,'.Rds')) # read in city specific data
    
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
    
    # calculate average outcome per person in the population considered by the model 
    # each column contains the information for a scenario and disease combination and each row contains the outputs from one of the 
    # sampled model runs
    outcome_pp[[city]] <- t(sapply(multi_city_ithim[[city]]$outcomes, function(x) colSums(x$hb$ylls[keep_rows,keep_cols],na.rm=T)))
    outcome_pp[[city]] <- outcome_pp[[city]]/sum(subset(DEMOGRAPHIC,min_pop_ages>=min_age&max_pop_ages<=max_age)$population)
    colnames(outcome_pp[[city]]) <- paste0(colnames(outcome_pp[[city]]),'_',city)
    
    
    ## get yll per 100,000 by age
    yll_per_hundred_thousand[[city]] <- list()
    for(aa in 1:length(outcome_age_groups)){ # loop through outcome age groups
      age <- outcome_age_groups[aa]
      
      # find total population for age group at the city level
      city_populations[ci,aa] <- sum(subset(DEMOGRAPHIC,min_pop_ages>=outcome_age_min[aa]&max_pop_ages<=outcome_age_max[aa])$population)
      # find total population by age group summing across all cities
      age_populations[aa] <- age_populations[aa] + city_populations[ci,aa] 
      
      # define age range to keep based on the age group currently used by the loop
      keep_rows2 <- which(min_ages>=outcome_age_min[aa]&max_ages<=outcome_age_max[aa])
      
      # calculate the total ylls per 100 000 for the given age category
      tmp <- t(sapply(multi_city_ithim[[city]]$outcomes, function(x) colSums(x$hb$ylls[keep_rows2,keep_cols],na.rm=T)))
      tmp <- tmp/city_populations[ci,aa]*100000
      yll_per_hundred_thousand[[city]][[age]] <- tmp # df to contain the YLLs for all cities, outcome age groups and model runs
    }
    
    # total yll outcome across all outcome age categories per city and scenario and disease combination
    outcome[[city]] <- t(sapply(multi_city_ithim[[city]]$outcomes, function(x) colSums(x$hb$ylls[keep_rows,keep_cols],na.rm=T)))
    colnames(outcome[[city]]) <- paste0(colnames(outcome[[city]]),'_',city)
    
    # create one dataframe for all cities with all outcomes for all model runs, age groups and disease and scenario combinations
    for(row in keep_rows){
      voi_data_all[[city]]$outcomes <- t(sapply(multi_city_ithim[[ci]]$outcomes, function(x) rbind(x$hb$ylls[row,])))
      voi_dummy <- data.frame(voi_data_all[[city]])
      colnames(voi_dummy)<-colnames(multi_city_ithim[[ci]]$outcomes[[1]]$hb$ylls)
      voi_dummy$city <- city
      voi_data_all_df <- rbind(voi_data_all_df, voi_dummy)
    }
    
  }
  
  # create an age and sex category column
  voi_data_all_df$age_sex <- paste(voi_data_all_df$sex, voi_data_all_df$age_cat, sep = )
  
  
  # needed for plotting but outcome$city gives total YLL for each disease and scenario combination, whereas outcome$combined gives
  # the YLL per person
  outcomes_pp <- do.call(cbind,outcome_pp) # bind outcome for all cities
  outcome$combined <- outcomes_pp
  
  
  
  ## compute yll per hundred thousand by age by summing across all diseases (double counting!) by city 
  # and also summing across all cities
  yll_per_hundred_thousand_stats <- list()
  combined_yll <- list() # for summing across all cities
  
  # set up matrix for all age groups
  for(aa in 1:length(outcome_age_groups)){
    age <- outcome_age_groups[aa]
    combined_yll[[age]] <- matrix(0,ncol=NSCEN,nrow=NSAMPLES)
  }
  
  for(ci in 1:length(cities)){ # loop through cities
    city <- cities[ci]
    case <- yll_per_hundred_thousand[[city]] # ylls across all age groups
    yll_per_hundred_thousand_stats[[city]] <- list()
    
    for(aa in 1:length(outcome_age_groups)){ # loop through age groups
      # initialise variables
      age <- outcome_age_groups[aa]
      min_pop_ages <- age_pops[[city]]$min_pop_ages
      max_pop_ages <- age_pops[[city]]$max_pop_ages
      population <- city_populations[ci,aa]
      
      # set up matrix to contain median and the 5th and 95th percentiles YLLs for each city and age group
      yll_per_hundred_thousand_stats[[city]][[age]] <- matrix(0,nrow=NSCEN,ncol=3) 
      colnames(yll_per_hundred_thousand_stats[[city]][[age]]) <- c('median','5%','95%')
      rownames(yll_per_hundred_thousand_stats[[city]][[age]]) <- SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)]
      
      case_age <- case[[age]]
      
      # calculate total ylls by summing across all diseases (includes a lot of double counting!)
      #### TO DO: Split results by level of disease????
      for(k in 1:NSCEN){ # loop through scenarios
        scen_case <- case_age[,seq(k,ncol(case_age),by=NSCEN)] # extract scenario results
        if(nsamples==1){ # if only one sample, then scen_case gives a 1 dimensional vector
          y <- sum(scen_case)
        }else{
          y <- rowSums(scen_case)
        }
        yll_per_hundred_thousand_stats[[city]][[age]][k,] <- quantile(y,c(0.5,0.05,0.95)) # summary stats based on individual run results
        combined_yll[[age]][,k] <- combined_yll[[age]][,k] + y*population/100000 # calculate total yll for the population age group summing across all cities
      }
    }
  }
  
  # calculate yll per age group summed across all cities (we have already summed across all diseases)
  yll_per_hundred_thousand_stats$combined <- list()
  for(aa in 1:length(outcome_age_groups)){
    age <- outcome_age_groups[aa]
    yll_per_hundred_thousand_stats$combined[[age]] <- t(apply(combined_yll[[age]]/age_populations[aa]*100000,2,quantile,c(0.5,0.05,0.95)))
    colnames(yll_per_hundred_thousand_stats$combined[[age]]) <- c('median','5%','95%')
    rownames(yll_per_hundred_thousand_stats$combined[[age]]) <- SCEN_SHORT_NAME[2:length(SCEN_SHORT_NAME)]
  }
  
  
  # create one dateframe with total ylls (median, 5th and 95th percentiles) per age group and city (plus combined results)
  summary_ylls_df <- data.frame()
  for(i in 1:length(yll_per_hundred_thousand_stats)){ # loop through cities plus combined
    for(j in 1:length(yll_per_hundred_thousand_stats[[i]])){ # loop through age groups
      if(length(summary_ylls_df) == 0){
        summary_ylls_df <- as.data.frame(yll_per_hundred_thousand_stats[[i]][[j]])
        summary_ylls_df$age <- names(yll_per_hundred_thousand_stats[[i]])[j]
        summary_ylls_df$city <- names(yll_per_hundred_thousand_stats)[i]
        summary_ylls_df$scenario <- rownames(summary_ylls_df)
      } else {
        summary_ylls_df_dummy <- as.data.frame(yll_per_hundred_thousand_stats[[i]][[j]])
        summary_ylls_df_dummy$age <- names(yll_per_hundred_thousand_stats[[i]])[j]
        summary_ylls_df_dummy$city <- names(yll_per_hundred_thousand_stats)[i]
        summary_ylls_df_dummy$scenario <- rownames(summary_ylls_df_dummy)
        summary_ylls_df <- rbind(summary_ylls_df, summary_ylls_df_dummy)
      }
    } 
  }    
  # re-arrange columns
  summary_ylls_df <- summary_ylls_df[,c("city","scenario","age", "median", "5%", "95%")]
  rownames(summary_ylls_df) <- 1:nrow(summary_ylls_df)
  
  
  # set-up output list 
  ithim_results <- list()
  
  ithim_results$summary_ylls_df <- summary_ylls_df
  ithim_results$voi_data_all_df <- voi_data_all_df
  ithim_results$outcome <- outcome
  ithim_results$yll_per_hundred_thousand <- yll_per_hundred_thousand
  ithim_results$yll_per_hundred_thousand_stats <- yll_per_hundred_thousand_stats
  
  
  return(ithim_results)
  
  
}