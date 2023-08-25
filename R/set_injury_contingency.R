#' Injury summary statistics
#'
#' Creates summarised injury tables for 'who hit who' and 'no other vehicle' fatality counts 
#' for each casualty and strike mode combination (and age and sex combination where this information exists)
#' 
#' 
#' The function performs the following steps using the individual fatality injury input file:
#' 
#' \itemize{ 
#' \item The data is split into a WHW (who hit who) matrix where both casualty and strike mode are given
#'   and a NOV (no other vehicle) matrix where strike mode was set to NOV in ithim_load_data.R or 
#'   no other vehicle was involved in the accident.
#' 
#' \item If no age and gender information is given, then the counts are multiplied by the 
#'   proportion of injuries relevant to the proportion of the population considered 
#'   in the model (e.g. 15 - 65 year olds) based on the GBD data
#' 
#' \item Data is aggregated by casualty and strike mode, and age and sex where such information exists
#' 
#' \item Complete whw and nov matrices containing all casualty and strike (and age and sex)
#'   combinations are created with zero counts for those combinations with no fatalities
#' 
#' \item a list of aggregated whw and nov matrices is set to the Global environment
#' }
#' 
#' @param injuries data frame of individual injury events
#' 
#' 
#' @export


set_injury_contingency <- function(injuries){

  # create list containing all modes in trip data set with speeds
  mode_names <- c(intersect(unique(TRIP_SET$stage_mode),MODE_SPEEDS$stage_mode)) 
  
  
  # add bus drivers, car drivers and motorcyclists if the corresponding flags are set to true
  if(ADD_BUS_DRIVERS) mode_names <- c(mode_names,'bus_driver')
  if(ADD_TRUCK_DRIVERS) mode_names <- c(mode_names,'truck')
  if(ADD_MOTORCYCLE_FLEET | ADD_PERSONAL_MOTORCYCLE_TRIPS != 'no') mode_names <- c(mode_names,'motorcycle')
  
  injury_list <- list()
  injury_table_types <- c()
  
  # check whether there are any whw injuries given and create a whw matrix
  if(length(unique(injuries$strike_mode))==1&&!'nov'%in%injuries$strike_mode||length(unique(injuries$strike_mode))>1){
    injury_list$whw <- subset(injuries,cas_mode%in%mode_names&strike_mode!='nov')
    injury_table_types <- c(injury_table_types,'whw')
  }
  # check if there are any nov injuries and create a nov matrix
  if('nov'%in%injuries$strike_mode){
    injury_list$nov <- subset(injuries,cas_mode%in%mode_names&strike_mode=='nov')
    injury_table_types <- c(injury_table_types,'nov')
  }
  
  injury_table <- list()
  
  # define column names to keep
  for(type in c(injury_table_types)){ # loop through 'whw' and 'nov'
    keep_names <- names(injury_list[[type]])%in%c('cas_mode','strike_mode','age_cat','cas_gender') 
    
    # summarise list of injuries by cas_mode, strike_mode, age_cat and cas_gender where this information exists
    setDT(injury_list[[type]])
    injury_summary <- as.data.frame(injury_list[[type]][,.(count=.N,weight=mean(weight)),by=c(names(injury_list[[type]])[keep_names])])
    
    # Conditional to restrict the number of injuries in dataset that don't have
    # cas_age nor cas_gender
    if (sum(names(injury_summary) %in% c('age_cat','cas_gender')) == 0) {
      
      # If no age nor gender exists, then each count is multiplied by the 
      # proportion of deaths found in the GBD dataset
      injury_summary$count <- injury_summary$count * PROPORTION_INJURIES_AGERANGE
    }
    # 
    
    ## create matrices for all cas and strike mode combinations (and all age and gender combinations) with 0 counts
    injury_table[[type]] <- expand.grid(lapply(as.data.frame(injury_list[[type]])[,keep_names],unique))
    
    # match summary numbers to table indices
    injury_summary_index <- apply(injury_summary[,-c(ncol(injury_summary)-0:1)],1,function(x)which(apply(injury_table[[type]], 1, function(y) all(x==y))))
    
    # initialise all at 0
    injury_table[[type]]$count <- 0
    injury_table[[type]]$weight <- mean(injury_list[[type]]$weight)
    
    # slot in non-zero counts where accidents exist
    injury_table[[type]]$count[injury_summary_index] <- injury_summary[,ncol(injury_summary)-1]
    injury_table[[type]]$weight[injury_summary_index] <- injury_summary[,ncol(injury_summary)]
  }
  INJURY_TABLE <<- injury_table
  INJURY_TABLE_TYPES <<- injury_table_types
}
