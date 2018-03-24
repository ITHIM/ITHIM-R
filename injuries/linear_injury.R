setwd('/home/rob/overflow_dropbox/ITHIM-R/injuries/')
source('shinyinjury.R')

input <- list()
input$injuryfile <- 'input_injuries.xlsx'
input$distancefile <- 'synthetic_population.ods'
input$file2 <- 'saved_mexico_model.Rdata'
input$file3 <- 'saved_mexico_model_NB.Rdata'
input$group <- 'Strike mode'
input$over <- 'Casualty age'
input$subgroup <- 'cyclist'
input$SE <- F
input$lq <- 0.25
input$uq <- 0.75
inFile <- list()
object_store <- list(fit_whw=NULL,fit_nov=NULL,scenario_tabs=NULL,injuries=NULL,plotButton=NULL,covariates=NULL,
  mexicoButton=NULL,distance=NULL,model='poisson',nScenarios=1,lq=0.25,uq=0.75)

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
  distance <- read.ods(inFile$datapath)[[1]]
  object_store$distance <- distance
  
  object_store <- getModelFits(object_store,input)
  
  
}else{
  inFile$datapath <- input[[paste0('file',useMexicoModel)]]
  if (is.null(inFile)) return(NULL)
  object_store_temp <- readRDS(inFile$datapath)
  for(x in names(object_store_temp)) object_store[[x]] <- object_store_temp[[x]]
}




prepPlots(object_store,input)



