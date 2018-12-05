set_injury_contingency <- function(injuries){
  ##!! need to work out of logic of how we know which modes there are!
  mode_names <- c("Bicycle","Bus","Bus_driver","Motorcycle","Truck","Pedestrian","Car")
  # divide injuries into those for which we can write a WHW matrix, i.e. we know distances of both striker and casualty, 
  ## and those for which we don't know striker distance: no or other vehicle (noov)
  ## we can only model casualties for which we know distance travelled (i.e. no Truck casualties for Accra)
  injury_list <- list()
  injury_list$whw <- subset(injuries,cas_mode%in%mode_names&strike_mode%in%mode_names)
  injury_list$noov <- subset(injuries,cas_mode%in%mode_names&!strike_mode%in%mode_names)
  injury_table <- list()
  for(type in c('whw','noov')){
    ##TODO make contingency table without prior knowledge of column names
    injury_table[[type]] <- expand.grid(year=unique(injury_list[[type]]$year),cas_mode=unique(injury_list[[type]]$cas_mode),
                                        strike_mode=unique(injury_list[[type]]$strike_mode),cas_age=unique(injuries$age_cat),cas_gender=unique(injury_list[[type]]$cas_gender),stringsAsFactors = F)
    
    injury_table[[type]]$count <- apply(injury_table[[type]],1,function(x)nrow(subset(injury_list[[type]],year==as.numeric(x[1])&cas_mode==as.character(x[2])&
                                                                                        strike_mode==as.character(x[3])&cas_gender==as.character(x[5])&
                                                                                        cas_age>=AGE_LOWER_BOUNDS[which(AGE_CATEGORY==x[4])]&
                                                                                        cas_age<AGE_LOWER_BOUNDS[which(AGE_CATEGORY==x[4])+1]))) 
  }
  INJURY_TABLE <<- injury_table
}