library(RCurl)
library(XML)
library(tm)

setwd('~/overflow_dropbox/ITHIM-R/code/injuries/')


# if dataset already exists, load it. Else, create it.
if(file.exists('data/accra_injury_records_2.Rds')){
  injury_records_2 <- readRDS('data/accra_injury_records_2.Rds')
}else{
  # the root to the folders of files
  root_path <- '~/overflow_dropbox/ITHIM-R/code/injuries/data/AMA/'
  
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
  saveRDS(injury_records_2,'data/accra_injury_records_2.Rds')
}

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

# generate who hit whom
sapply(unique(injury_dataset$strike_mode),
       function(x) sapply(unique(injury_dataset$cas_mode),
                          function(y)sum(injury_dataset$strike_mode==x&injury_dataset$cas_mode==y&injury_dataset$year==2015)))

# count over years
sapply(unique(injury_dataset$year),function(x)nrow(subset(injury_dataset,year==x)))

injury_dataset$cas_mode[injury_dataset$cas_mode=='pedestrian'] <- 'Pedestrian'
injury_dataset$cas_mode[injury_dataset$cas_mode=='bus'] <- 'Bus'
injury_dataset$cas_mode[injury_dataset$cas_mode=='car'] <- 'Car'
injury_dataset$cas_mode[injury_dataset$cas_mode=='m/cyc'] <- 'Motorcycle'
injury_dataset$cas_mode[injury_dataset$cas_mode=='cycle'] <- 'Bicycle'
injury_dataset$cas_mode[injury_dataset$cas_mode=='hgv'] <- 'Truck'
injury_dataset$strike_mode[injury_dataset$strike_mode=='hgv'] <- 'Truck'
injury_dataset$strike_mode[injury_dataset$strike_mode=='pedestrian'] <- 'Pedestrian'
injury_dataset$strike_mode[injury_dataset$strike_mode=='bus'] <- 'Bus'
injury_dataset$strike_mode[injury_dataset$strike_mode=='car'] <- 'Car'
injury_dataset$strike_mode[injury_dataset$strike_mode=='m/cyc'] <- 'Motorcycle'
injury_dataset$strike_mode[injury_dataset$strike_mode=='cycle'] <- 'Bicycle'
injury_dataset$cas_gender[injury_dataset$cas_gender=='male'] <- 'Male'
injury_dataset$cas_gender[injury_dataset$cas_gender=='femal'] <- 'Female'
saveRDS(injury_dataset,'data/accra_injuries_long.Rds')

##################################


whw <- sapply(c('pedestrian','cycle','m/cyc','car','picku','bus','hgv','mini','other','NOV','unk','?'),
        function(x) sapply(c('pedestrian','cycle','m/cyc','car','picku','bus','hgv','mini','other','?'),
                           function(y)sum(injury_dataset$strike_mode==x&injury_dataset$cas_mode==y)))
saveRDS(list(whw=whw,injury_dataset=injury_dataset),'accra_data.Rds')
inj <- readRDS('accra_data.Rds')
inj$whw
head(inj$injury_dataset)


dup <- unlist(sapply(injury_records_2,function(x)x[,1]))[duplicated(unlist(sapply(injury_records_2,function(x)x[,1])))]
dup_entries <- sapply(injury_records_2,function(x)x[x[,1]%in%dup,])
dup_entries <- sapply(dup_entries,function(x)x[order(x[,1]),])
for(x in 1:length(dup_entries)) View(dup_entries[[x]])
