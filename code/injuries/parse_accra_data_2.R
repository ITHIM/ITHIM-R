library(RCurl)
library(XML)
library(tm)

setwd('~/overflow_dropbox/ITHIM-R/code/injuries/')



# First create an 'object' (called 'fomc' - all of these object names are arbitrary) that includes the list of filenames from your folder that end in .htm or .html 
root_path <- '~/overflow_dropbox/ITHIM-R/code/injuries/data/AMA/'

if(file.exists('data/accra_injury_records_2.Rds')){
  injury_records_2 <- readRDS('data/accra_injury_records_2.Rds')
}else{
  paths <- list.files(root_path)
  
  injury_records <- list()
  
  for(f_index in 1:length(paths)){
    print(f_index)
    fomc <- list.files(path=paste0(root_path,paths[f_index]),pattern="\\.(htm|html)$") # this is a regular expression that says only get .htm or .html files from the folder
    
    # Loop through the new fomc list of names, reading in the file content and storing each as an html object in a vector or 'list' called 'docs). 
    
    docs  <- NULL
    for (file in paste0(root_path,paths[f_index],'/',fomc)) {
      html <- htmlTreeParse(file, useInternal=T)
      docs <- c(docs,html)
    }
    
    # Create a new 'list' called pars that contains a list of  paragraphs for each of the three files in docs (this is done by using the '/p' (paragraph break) delimiter in the html encoding, starting at the root of the document). 
    
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
    
    vehicle_records <- list()
    for(j in 1:length(pars)){
      x <- pars[[j]]
      x <- gsub('\\r\\n', '', x)
      x <- gsub('\u0085', ' ', x)
      #if("Acc Severity"%in%x){
      #  print(c(f_index,j))
        #starts <- which(x%in%c('Acc Severity','Acc Severity\r\n'))
        #seconds <- which(x%in%c('No of Vehs','No of Vehs\r\n'))
      #}else{
      starts <- which(x%in%c('Report No','Report No\r\n'))
      seconds <- which(x%in%c('Veh Typ V1','Veh Typ V1\r\n'))
      #}
      lengths <- seconds - starts
      starts <- c(starts, length(x)+2)
      sub_as_array <- list()
      for(i in 1:length(lengths)){
        subsection <- x[starts[i]:(starts[i+1]-2)]
        sub_as_array[[i]] <- matrix(tolower(subsection),ncol=lengths[i],byrow=T)
        sub_as_array[[i]][,1] <- sapply(sub_as_array[[i]][,1],function(x) gsub('casualty','cas',x))
        sub_as_array[[i]][,1] <- sapply(sub_as_array[[i]][,1],function(x) gsub('no ','',x))
      }
      vehicle_records[[fomc[j]]] <- do.call(cbind,sub_as_array)
    }
    
    injury_records[[paths[[f_index]]]] <- vehicle_records
  }
  
  injury_records_2 <- list()
  
  for(f_index in 1:length(injury_records)){
    injury_records_2[[names(injury_records)[f_index]]] <- injury_records[[f_index]][[1]]
    for(j in 2:length(injury_records[[f_index]])){
      injury_records_2[[names(injury_records)[f_index]]] <- merge(
        injury_records_2[[names(injury_records)[f_index]]],
        injury_records[[f_index]][[j]],
        all=T,sort=F,by=1,suffixes=c(paste0('x',j),paste0('y',j)))
    }
    rownames(injury_records_2[[names(injury_records)[f_index]]]) <- injury_records_2[[names(injury_records)[f_index]]][,1]
    name_repeats <- apply(injury_records_2[[names(injury_records)[f_index]]],2,function(y)(any(y%in%rownames(injury_records_2[[names(injury_records)[f_index]]]))))
    injury_records_2[[names(injury_records)[f_index]]] <- t(injury_records_2[[names(injury_records)[f_index]]][,!name_repeats])
  }
  for(i in 1:length(injury_records_2)){
    inj_indices <- which(sapply(colnames(injury_records_2[[i]]),function(x)'inj'%in%strsplit(x,' ')[[1]]))
    for(j in inj_indices) #print(injury_records_2[[i]][,j])
      injury_records_2[[i]][injury_records_2[[i]][,j]==1,j] <- 'fatal'
  }
  saveRDS(injury_records_2,'data/accra_injury_records_2.Rds')
}

all_modes <- c("hgv","bus","mini","picku","tract","car","m/cyc","cycle","other","unk","?")
injury_dataset <- data.frame(event_id=character(),year=double(),cas_mode=character(),strike_mode=character(),cas_age=double(),cas_gender=character(),stringsAsFactors=FALSE)
for(i in 1:length(injury_records_2)){
  year <- names(injury_records_2)[i]
  colnames(injury_records_2[[i]]) <- sapply(colnames(injury_records_2[[i]]),function(x) gsub('no ','',x))
  colnames(injury_records_2[[i]]) <- sapply(colnames(injury_records_2[[i]]),function(x) gsub('casualty','cas',x))
  for(j in 1:nrow(injury_records_2[[i]])){
    event_id <- injury_records_2[[i]][j,1]
    killed <- which(injury_records_2[[i]][j,]=='fatal')
    for(k in killed){
      killed_vic_number <- sapply(k,function(x)strsplit(colnames(injury_records_2[[i]])[x],'c')[[1]][3])
      killed_veh_number_index <- which(colnames(injury_records_2[[i]])==paste0('veh ref c',killed_vic_number))
      killed_veh_number <- injury_records_2[[i]][j,killed_veh_number_index]
      killed_veh_index <- which(colnames(injury_records_2[[i]])==paste0('veh typ v',killed_veh_number))
      if(length(killed_veh_index)==0){
        killed_veh_index <- 1
        cas_mode <- 'pedestrian' 
      }else{
        cas_mode <- injury_records_2[[i]][j,killed_veh_index]
      }
      killed_class_index <- which(colnames(injury_records_2[[i]])==paste0('cas class c',killed_vic_number))
      cas_class <- injury_records_2[[i]][j,killed_class_index]
      cas_classes <- injury_records_2[[i]][j,-killed_class_index][which(sapply(colnames(injury_records_2[[i]])[-killed_class_index],function(x)strsplit(x,' ')[[1]][2]=='class' ))]
      if(cas_class=='pedes'&&cas_mode!='pedestrian'){
        strike_modes <- cas_mode
        cas_mode <- 'pedestrian'
      }else if(length(injury_records_2[[i]][j,-killed_veh_index])>0){
        strike_modes <- injury_records_2[[i]][j,-killed_veh_index][which(sapply(colnames(injury_records_2[[i]])[-killed_veh_index],function(x)strsplit(x,' ')[[1]][2]=='typ' ))]
      }else{
        strike_modes <- ''
      }
      strike_mode <- if(any(all_modes%in%strike_modes)) all_modes[which(all_modes%in%strike_modes)[1]] else if ('pedes'%in%cas_classes) 'pedestrian' else 'NOV'
      killed_age_index <- which(colnames(injury_records_2[[i]])==paste0('cas age c',killed_vic_number))
      cas_age <- injury_records_2[[i]][j,killed_age_index]
      killed_gender_index <- which(colnames(injury_records_2[[i]])==paste0('gender c',killed_vic_number))
      cas_gender <- injury_records_2[[i]][j,killed_gender_index]
      if(length(c(event_id,year,cas_mode,strike_mode,cas_age,cas_gender))>6) print(c(i,j))
      injury_dataset[nrow(injury_dataset)+1,] <- c(event_id,year,cas_mode,strike_mode,cas_age,cas_gender)
    }
  }
}
# fix nov
sapply(c('pedestrian','cycle','m/cyc','car','picku','bus','hgv','mini','other','NOV','unk','?'),
       function(x) sapply(c('pedestrian','cycle','m/cyc','car','picku','bus','hgv','mini','other','?'),
                          function(y)sum(injury_dataset$strike_mode==x&injury_dataset$cas_mode==y)))

##################################

sapply(injury_records_2,function(x)'Year'%in%rownames(x))
merge(injury_records_2$Car[,which(injury_records_2$Car[2,]=='Cycle'&injury_records_2$Car[3,]=='Fast'&injury_records_2$Car[4,]=='Care')],injury_records_2$Cycle[,6:7],all=T,by=0,sort=F)[1:13,c(1,3,4,5)]
injury_records_2$MiniBus[,!is.na(injury_records_2$MiniBus[88,])]
sapply(injury_records_2,dim)
View(injury_records_2$Car[,which(apply(injury_records_2$Car,2,function(y)y[1]=='Mini'&y[2]=='Mini'))])
which(apply(injury_records_2$Other,2,function(y)y[1]=='HGV'))
which(apply(injury_records_2$HGV,2,function(y)y[2]=='Other'))
View(injury_records_2$HGV[,which(apply(injury_records_2$HGV,2,function(y)y[2]=='Other'))])
View(injury_records_2$Other[,which(apply(injury_records_2$Other,2,function(y)y[1]=='HGV'))])


injury_records_2$Cycle[,apply(injury_records_2$Cycle,2,function(x)
  any(
    apply(
      cbind(c(12,9,10),c(5,6,11)),1,function(y) isTRUE(all.equal(c(as.character(x[y[1]]),as.character(x[y[2]])),c('Fatal','Drive')))
      )
    )
  )]
  








