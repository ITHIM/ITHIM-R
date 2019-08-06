## Data from James Derry
## Script by RJ 29/01/2019

library(RCurl)
library(XML)
library(tm)

#####################################################
## parse files

# if dataset already exists, load it. Else, create it.
if(file.exists('processed_data/accra_injury_records_2.Rds')){
  injury_records_2 <- readRDS('processed_data/accra_injury_records_2.Rds')
}else{
  # the root to the folders of files
  root_path <- 'data/GAMA/'
  
  # all the folder names
  paths <- list.files(root_path)
  
  # object to store records
  injury_records <- list()
  for(f_index in 1:length(paths)){
    print(f_index)
    # extract all file names in the folder by file extension 
    fomc <- list.files(path=paste0(root_path,paths[f_index]),pattern="\\.(htm|html)$") 
    # this is a regular expression that says only get .htm or .html files from the folder
    
    # Loop through the list of names, reading in the file content and storing each 
    docs  <- NULL
    for (file in paste0(root_path,paths[f_index],'/',fomc)) {
      html <- htmlTreeParse(file, useInternal=T)
      docs <- c(docs,html)
    }
    
    # store contents as a list 
    pars <- NULL 
    for (i in length(docs):1){
      tryCatch({
        p <- unlist(xpathApply(docs[[i]], '//p', xmlValue))
        p <- list(p)
        pars <- c(p,pars)
      }, warning = function(w) {
        NULL
      }, error = function(e) {
        fomc <<- fomc[-i]
      }, finally = {})
    } 
    # Each object is a list of the paragraphs in each doc
    
    # strip formatting to extract data
    vehicle_records <- list()
    for(j in 1:length(pars)){
      x <- pars[[j]]
      x <- gsub('\\r\\n', '', x)
      x <- gsub('\u0085', ' ', x)
      # assume each file starts with 'Report No', followed by 'Veh Typ V1'. 
      # This must change if the file changes!
      starts <- which(x%in%c('Report No','Report No\r\n'))
      seconds <- which(x%in%c('Veh Typ V1','Veh Typ V1\r\n'))
      # get number of entries in this file
      lengths <- seconds - starts
      starts <- c(starts, length(x)+2)
      sub_as_array <- list()
      for(i in 1:length(lengths)){
        # extract each block of events by indexing
        subsection <- x[starts[i]:(starts[i+1]-2)]
        # set all row names to lower case
        sub_as_array[[i]] <- matrix(tolower(subsection),ncol=lengths[i],byrow=T)
        # replace any instance of 'casualty' with 'cas'
        sub_as_array[[i]][,1] <- sapply(sub_as_array[[i]][,1],function(x) gsub('casualty','cas',x))
        # remove the text 'no '
        sub_as_array[[i]][,1] <- sapply(sub_as_array[[i]][,1],function(x) gsub('no ','',x))
      }
      vehicle_records[[fomc[j]]] <- do.call(cbind,sub_as_array)
    }
    
    injury_records[[paths[[f_index]]]] <- vehicle_records
  }
  
  # collate each folder into one record = one record per year
  injury_records_2 <- list()
  for(f_index in 1:length(injury_records)){
    injury_records_2[[names(injury_records)[f_index]]] <- injury_records[[f_index]][[1]]
    for(j in 2:length(injury_records[[f_index]])){
      injury_records_2[[names(injury_records)[f_index]]] <- merge(
        injury_records_2[[names(injury_records)[f_index]]],
        injury_records[[f_index]][[j]],
        all=T,sort=F,by=1,suffixes=c(paste0('x',j),paste0('y',j)))
    }
    # set first column to be the row names
    rownames(injury_records_2[[names(injury_records)[f_index]]]) <- injury_records_2[[names(injury_records)[f_index]]][,1]
    # remove any extra columns with row names (from appended tables)
    name_repeats <- apply(injury_records_2[[names(injury_records)[f_index]]],2,function(y)(any(y%in%rownames(injury_records_2[[names(injury_records)[f_index]]]))))
    injury_records_2[[names(injury_records)[f_index]]] <- t(injury_records_2[[names(injury_records)[f_index]]][,!name_repeats])
  }
  # recode any injury severities, where 1=fatal, 2=hosp, 3=injur
  for(i in 1:length(injury_records_2)){
    inj_indices <- which(sapply(colnames(injury_records_2[[i]]),function(x)'inj'%in%strsplit(x,' ')[[1]]))
    for(j in inj_indices) #print(injury_records_2[[i]][,j])
      injury_records_2[[i]][injury_records_2[[i]][,j]==1,j] <- 'fatal'
  }
  saveRDS(injury_records_2,'processed_data/accra_injury_records_2.Rds')
}

#####################################################
## read into table
if(file.exists('processed_data/accra_injuries_dataset.Rds')){
  injury_dataset <- readRDS('processed_data/accra_injuries_dataset.Rds')
}else{
  # list modes in order of striking priority
  all_modes <- c("hgv","bus","mini","picku","tract","car","m/cyc","cycle","other","unk","?")
  
  # set up dataframe to store covariates
  injury_dataset <- data.frame(event_id=character(),year=double(),cas_mode=character(),strike_mode=character(),cas_age=double(),cas_gender=character(),stringsAsFactors=FALSE)
  # iterate over years to populate data frame
  for(i in 1:length(injury_records_2)){
    year <- names(injury_records_2)[i]
    for(j in 1:nrow(injury_records_2[[i]])){
      event_id <- injury_records_2[[i]][j,1]
      # which columns denote fatal casualties
      killed <- which(injury_records_2[[i]][j,]=='fatal')
      # iterate casualty by casualty
      for(k in killed){
        # what was the c number of the casualty
        killed_vic_number <- sapply(k,function(x)strsplit(colnames(injury_records_2[[i]])[x],'c')[[1]][3])
        # where is the vehicle reference for the casualty
        killed_veh_number_index <- which(colnames(injury_records_2[[i]])==paste0('veh ref c',killed_vic_number))
        # what is the vehicle reference for the casualty
        killed_veh_number <- injury_records_2[[i]][j,killed_veh_number_index]
        # where is the vehicle listed
        killed_veh_index <- which(colnames(injury_records_2[[i]])==paste0('veh typ v',killed_veh_number))
        # if there's no vehicle with that index, set to column one. else, casualty mode is that vehicle.
        if(length(killed_veh_index)==0){
          killed_veh_index <- 1
          cas_mode <- 'pedestrian' 
        }else{
          cas_mode <- injury_records_2[[i]][j,killed_veh_index]
        }
        # where is the class stored for the casualty
        killed_class_index <- which(colnames(injury_records_2[[i]])==paste0('cas class c',killed_vic_number))
        # what is the class of the casualty
        cas_class <- injury_records_2[[i]][j,killed_class_index]
        # what are the other casualty classes
        cas_classes <- injury_records_2[[i]][j,-killed_class_index][which(sapply(colnames(injury_records_2[[i]])[-killed_class_index],function(x)strsplit(x,' ')[[1]][2]=='class' ))]
        # if the class is pedestrian but the mode isn't, reset the casualty and strike modes. else, find all the other vehicles.
        if(cas_class=='pedes'&&cas_mode!='pedestrian'){
          strike_modes <- cas_mode
          cas_mode <- 'pedestrian'
        }else if(length(injury_records_2[[i]][j,-killed_veh_index])>0){
          strike_modes <- injury_records_2[[i]][j,-killed_veh_index][which(sapply(colnames(injury_records_2[[i]])[-killed_veh_index],function(x)strsplit(x,' ')[[1]][2]=='typ' ))]
        }else{
          strike_modes <- ''
        }
        # choose the largest vehicle. if no vehicles, but pedestrians, set pedestrian. else, NOV.
        strike_mode <- if(any(all_modes%in%strike_modes)) all_modes[which(all_modes%in%strike_modes)[1]] else if ('pedes'%in%cas_classes) 'pedestrian' else 'NOV'
        # where is the age listed
        killed_age_index <- which(colnames(injury_records_2[[i]])==paste0('cas age c',killed_vic_number))
        # what is the age
        cas_age <- injury_records_2[[i]][j,killed_age_index]
        # where is the gender listed
        killed_gender_index <- which(colnames(injury_records_2[[i]])==paste0('gender c',killed_vic_number))
        # what is the gender
        cas_gender <- injury_records_2[[i]][j,killed_gender_index]
        # add to data frame
        injury_dataset[nrow(injury_dataset)+1,] <- c(event_id,year,cas_mode,strike_mode,cas_age,cas_gender)
      }
    }
  }
  saveRDS(injury_dataset,'processed_data/accra_injuries_dataset.Rds')
}

#####################################################
## re-write for ITHIM-R implementation
injury_dataset$cas_mode[injury_dataset$cas_mode=='pedestrian'] <- 'Pedestrian'
injury_dataset$cas_mode[injury_dataset$cas_mode=='bus'] <- 'Bus'
injury_dataset$cas_mode[injury_dataset$cas_mode=='car'] <- 'Car'
injury_dataset$cas_mode[injury_dataset$cas_mode=='m/cyc'] <- 'Motorcycle'
injury_dataset$cas_mode[injury_dataset$cas_mode=='cycle'] <- 'Bicycle'
injury_dataset$cas_mode[injury_dataset$cas_mode=='hgv'] <- 'Truck'
injury_dataset$strike_mode[injury_dataset$strike_mode=='hgv'] <- 'Truck'
injury_dataset$strike_mode[injury_dataset$strike_mode=='pedestrian'] <- 'Pedestrian'
injury_dataset$strike_mode[injury_dataset$strike_mode=='bus'] <- 'Bus_driver'
injury_dataset$strike_mode[injury_dataset$strike_mode=='car'] <- 'Car'
injury_dataset$strike_mode[injury_dataset$strike_mode=='m/cyc'] <- 'Motorcycle'
injury_dataset$strike_mode[injury_dataset$strike_mode=='cycle'] <- 'Bicycle'
injury_dataset$cas_gender[injury_dataset$cas_gender=='male'] <- 'Male'
injury_dataset$cas_gender[injury_dataset$cas_gender=='femal'] <- 'Female'

##################################


sapply(injury_records_2,function(x)sum(x=='fatal',na.rm=T))
sum(sapply(injury_records_2,function(x)sum(x=='fatal',na.rm=T)))
sapply(2007:2016,function(x)nrow(subset(injury_dataset,year==x)))
sum(sapply(2007:2016,function(x)nrow(subset(injury_dataset,year==x))))

duplicated_ids <- injury_dataset$event_id[duplicated(injury_dataset)]
subset(injury_dataset,event_id%in%duplicated_ids)

injury_dataset_unique <- injury_dataset[!duplicated(injury_dataset),]
sapply(2007:2016,function(x)nrow(subset(injury_dataset_unique,year==x)))
sum(sapply(2007:2016,function(x)nrow(subset(injury_dataset_unique,year==x))))

saveRDS(injury_dataset_unique,'processed_data/accra_injuries_long.Rds')
write.csv(injury_dataset_unique,'processed_data/injuries_accra.csv')
write.csv(injury_dataset_unique,'../../../inst/extdata/local/accra/injuries_accra.csv')
