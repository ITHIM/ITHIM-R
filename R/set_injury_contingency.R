#' Create contingency table from itemised list of injuries
#' 
#' One of the inputs is a list of injury events. This function aggregates injuries by type into a long contingency table with prespecified column names.
#' Write tables to global environment.
#' 
#' @param injuries data frame of injury events
#' 
#' 
#' @export
set_injury_contingency <- function(injuries){
  ##RJ previously (up to 14/5/19)
  # ##!! need to work out of logic of how we know which modes there are distances for!
  # mode_names <- c(intersect(unique(TRIP_SET$stage_mode),MODE_SPEEDS$stage_mode),"pedestrian")
  # mode_names <- mode_names[mode_names!='other']
  # if(ADD_BUS_DRIVERS) mode_names <- c(mode_names,'bus_driver')
  # if(ADD_TRUCK_DRIVERS) mode_names <- c(mode_names,'truck')
  # if(CITY=='accra') mode_names <- c(mode_names,'motorcycle')
  # #mode_names <- c("bicycle","bus","bus_driver","motorcycle","truck","pedestrian","car")
  # # strike mode bus -> bus_driver
  # if('bus'%in%injuries$strike_mode) injuries$strike_mode[injuries$strike_mode=='bus'] <- 'bus_driver'
  # # divide injuries into those for which we can write a WHW matrix, i.e. we know distances of both striker and casualty, 
  # ## and those for which we don't know striker distance: no or other vehicle (noov)
  # ## we can only model casualties for which we know distance travelled (i.e. no truck casualties for Accra)
  # injury_list <- list()
  # injury_list$whw <- subset(injuries,cas_mode%in%mode_names&strike_mode%in%mode_names)
  # injury_list$noov <- subset(injuries,cas_mode%in%mode_names&!strike_mode%in%mode_names)
  ##
  
  ##!! need to work out of logic of how we know which modes there are distances for!
  mode_names <- c(intersect(unique(TRIP_SET$stage_mode),MODE_SPEEDS$stage_mode),"pedestrian")
  mode_names <- mode_names[mode_names!='other']
  if(ADD_BUS_DRIVERS) mode_names <- c(mode_names,'bus_driver')
  if(ADD_TRUCK_DRIVERS) mode_names <- c(mode_names,'truck')
  if(CITY=='accra') mode_names <- c(mode_names,'motorcycle')
  injury_list <- list()
  injury_table_types <- c()
  if(length(unique(injuries$strike_mode))==1&&!'nov'%in%injuries$strike_mode||length(unique(injuries$strike_mode))>1){
    injury_list$whw <- subset(injuries,cas_mode%in%mode_names&strike_mode!='nov')
    injury_table_types <- c(injury_table_types,'whw')
  }
  if('nov'%in%injuries$strike_mode){
    injury_list$nov <- subset(injuries,cas_mode%in%mode_names&strike_mode=='nov')
    injury_table_types <- c(injury_table_types,'nov')
  }
  injury_table <- list()
  for(type in c(injury_table_types)){
    keep_names <- names(injury_list[[type]])%in%c('year','cas_mode','strike_mode','age_cat','cas_gender')
    # summarise list of injuries by group
    setDT(injury_list[[type]])
    injury_summary <- as.data.frame(injury_list[[type]][,.(count=.N,weight=mean(weight)),by=c(names(injury_list[[type]])[keep_names])])
    #injury_summary <- plyr::count(injury_list[[type]][,keep_names],names(injury_list[[type]])[keep_names])
    # make contingency table without prior knowledge of column names
    
    injury_table[[type]] <- expand.grid(lapply(as.data.frame(injury_list[[type]])[,keep_names],unique))
    # match summary numbers to table indices
    injury_summary_index <- apply(injury_summary[,-c(ncol(injury_summary)-0:1)],1,function(x)which(apply(injury_table[[type]], 1, function(y) all(x==y))))
    # initialise all at 0
    injury_table[[type]]$count <- 0
    injury_table[[type]]$weight <- mean(injury_list[[type]]$weight)
    # slot in non-zero counts
    injury_table[[type]]$count[injury_summary_index] <- injury_summary[,ncol(injury_summary)-1]
    injury_table[[type]]$weight[injury_summary_index] <- injury_summary[,ncol(injury_summary)]
  }
  INJURY_TABLE <<- injury_table
  INJURY_TABLE_TYPES <<- injury_table_types
}
