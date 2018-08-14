library(RCurl)
library(XML)
library(tm)
library(SnowballC)
library(wordcloud)

setwd('~/overflow_dropbox/ITHIM-R/injuries/')



# First create an 'object' (called 'fomc' - all of these object names are arbitrary) that includes the list of filenames from your folder that end in .htm or .html 
root_path <- '~/overflow_dropbox/ITHIM-R/injuries/data/accra_injuries/'
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
    if("Acc Severity"%in%x){
      print(c(f_index,j))
      starts <- which(x%in%c('Acc Severity','Acc Severity\r\n'))
      seconds <- which(x%in%c('No of Vehs','No of Vehs\r\n'))
    }else{
      starts <- which(x%in%c('Veh Type V1','Veh Type V1\r\n'))
      seconds <- which(x%in%c('Veh Type V2','Veh Type V2\r\n'))
    }
    lengths <- seconds - starts
    starts <- c(starts, length(x)+2)
    sub_as_array <- list()
    for(i in 1:length(lengths)){
      subsection <- x[starts[i]:(starts[i+1]-2)]
      sub_as_array[[i]] <- matrix(subsection,ncol=lengths[i],byrow=T)
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
  injury_records_2[[names(injury_records)[f_index]]] <- injury_records_2[[names(injury_records)[f_index]]][,!name_repeats]
}

sapply(injury_records_2,function(x)'Year'%in%rownames(x))
merge(injury_records_2$Car[,which(injury_records_2$Car[2,]=='Cycle'&injury_records_2$Car[3,]=='Fast'&injury_records_2$Car[4,]=='Care')],injury_records_2$Cycle[,6:7],all=T,by=0,sort=F)[1:13,c(1,3,4,5)]
injury_records_2$MiniBus[,!is.na(injury_records_2$MiniBus[88,])]
sapply(injury_records_2,dim)
View(injury_records_2$Car[,which(apply(injury_records_2$Car,2,function(y)y[1]=='Mini'&y[2]=='Mini'))])
which(apply(injury_records_2$Other,2,function(y)y[1]=='HGV'))
which(apply(injury_records_2$HGV,2,function(y)y[2]=='Other'))
View(injury_records_2$HGV[,which(apply(injury_records_2$HGV,2,function(y)y[2]=='Other'))])
View(injury_records_2$Other[,which(apply(injury_records_2$Other,2,function(y)y[1]=='HGV'))])











