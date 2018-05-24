setwd('/home/rob/overflow_dropbox/ITHIM-R/injuries/')
source('shinyinjury.R')

input <- list()
input$injuryfile <- 'input_injuries.xlsx'
input$distancefile <- 'mexico_city_travel_data.csv' #'synthetic_population.ods' #
input$file2 <- 'saved_mexico_model.Rdata'
input$file3 <- 'saved_mexico_model_NB.Rdata'
input$group <- 'casualty mode'
input$over <- 'strike mode'
input$subgroup <- 'pedestrian'
input$SE <- F
input$lq <- 0.05
input$uq <- 0.95
input$sin <- T
input$sinuncertainty <- T
inFile <- list()
object_store <- list(fit_whw=NULL,fit_nov=NULL,scenario_tabs=NULL,injuries=NULL,plotButton=NULL,covariates=NULL,
  mexicoButton=NULL,distance=NULL,model='poisson',nScenarios=1,lq=0.25,uq=0.75,sinfile='default_sin_exponents.Rdata')

model <- 'poisson'
if (model=='NB') object_store$model <- 'NB'

useShiny <- F
useMexicoModel <- 0

if(useMexicoModel==0){
  ## GET INJURY INPUT
  inFile$datapath <- input$injuryfile
  if (is.null(inFile)) return(NULL)
  ##TODO allow many file types
  injuries <- read.xlsx(inFile$datapath,sheetIndex=1,stringsAsFactors=FALSE,header=F)
  ##TODO many checks here
  # are the ages interpretable? How are they coded?
  # are the ages the same as the distance ages?
  # are there travel data for hgvs?
  # what are the covariates? are they all in the distance data?
  # V codes comprehensible, numbers as numbers not strings...
  object_store$injuries <- injuries
  
  inFile$datapath <- input$distancefile
  if (is.null(inFile)) return(NULL)
  if(file_ext(inFile$datapath)=='ods'){
    distance <- read.ods(inFile$datapath)[[1]]
    names(distance) <- distance[1,]
    distance <- distance[-1,]
    ##TODO choose columns to change based on column names
    distance[,c(2,4:dim(distance)[2])]<-sapply(distance[,c(2,4:dim(distance)[2])],as.numeric)
  }else if(file_ext(inFile$datapath)=='csv'){
    distance <- read.csv(inFile$datapath)
  }
  object_store$distance <- distance
  
  object_store <- getModelFits(object_store,input)
  
  
}else{
  inFile$datapath <- input[[paste0('file',useMexicoModel)]]
  if (is.null(inFile)) return(NULL)
  object_store_temp <- readRDS(inFile$datapath)
  for(x in names(object_store_temp)) object_store[[x]] <- object_store_temp[[x]]
}


input$SE <- T
groups <- c('casualty mode','casualty age','casualty gender','strike mode')
for(i in 1:length(groups)){
  input$group <- paste(strsplit(groups[i],' ')[[1]],collapse='_')
  instances <- unique(c(levels(object_store$scenario_tabs[[1]][[1]][[input$group]]),levels(object_store$scenario_tabs[[1]][[2]][[input$group]])))
  for(j in c(1:length(groups))[-i]){
    input$over <- paste(strsplit(groups[j],' ')[[1]],collapse='_')
    for(k in instances){
      input$subgroup <- k
      print(c(i,j,input$group,input$over,k))
      prepPlots(object_store,input)
    }
  }
}



