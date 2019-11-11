library(xlsx)
setwd('~/overflow_dropbox/ITHIM-R/project_mgmt/')
x <- xlsx::read.xlsx2('case_cities_data.xlsx',sheetIndex=1,colClasses = 'character',stringsAsFactors=FALSE)

start_index <- which(x[,1]=='Country') + 1
stop_index <- which(x[,1]=='Extras') - 1

countries <- x[start_index:stop_index,1]
cities <- x[start_index:stop_index,2]

sections <- which(sapply(colnames(x),function(x)!grepl('X',x)))

## Go through section by section 
for(sec in 2:length(sections)){
  this_section <- sections[sec]
  section_name <- colnames(x)[this_section]
  print(section_name)
  
  end_this_section <- ifelse(sec<length(sections),sections[which(sections==this_section)+1] - 1,ncol(x))
  
  cat(paste0('| City ',paste(' | ',x[1,this_section:end_this_section],collapse='')))
  cat('|\n')
  cat(paste0(paste(rep('| --- ',end_this_section-this_section+2),collapse=''),'|\n'))
  for(i in start_index:stop_index){
    cat(paste0('| ',cities[i-start_index+1]))
    for(j in this_section:end_this_section){
      cat(' | ')
      cat(gsub('\n',' ',x[i,j]))
    }
    cat('|\n')
  }
}


