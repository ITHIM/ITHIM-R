server <- function(input, output){
  source('functions/injury_calculation_functions.R')
  # object storing all values
  object_store <- reactiveValues( ## models
    fit_whw=NULL,
    fit_nov=NULL,
    ## data
    nScenarios=1,
    scenario_tabs=NULL,
    injuries=NULL,
    distance=NULL,
    covariates=NULL,
    ## switches
    plotButton=NULL,
    mexicoButton=NULL,
    model='poisson',
    ## quantiles
    lq=0.25,
    uq=0.75,
    ## SIN
    sin=F,
    sinuncertainty=F,
    sinfile='data/default_sin_exponents.Rdata',
    ## reporting rate
    rr=F,
    temp_rrdistribution='Beta',
    temp_rrp=c(5,1),
    temp_rrdist=Beta(5,1),
    rrdistribution='Beta',
    rrp=c(5,1),
    rrdist=Beta(5,1))
  # response to uploading injury. Saves injury; reveals distance file button
  ##TODO remove distance data file response when we have true synthetic population
  output$ui.distance <- renderUI({
    inFile <- input$injuryfile
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
    ##TODO this will eventually be read directly from synthetic population, not uploaded by user.
    fileInput(inputId='distancefile', label='Upload distance data',
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".xlsx",
        ".csv")
    )
  })
  # reads distance file
  observeEvent(input$distancefile, {
    inFile <- input$distancefile
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
  })
  # sin box
  output$ui.sin <- renderUI({
    checkboxInput("sin", "Apply SIN exponents", object_store$sin)
  })
  # reveal sin uncertainty box
  output$ui.sinuncertainty <- renderUI({
    if (!isTRUE(input$sin)) return(NULL)
    checkboxInput("sinuncertainty", "Apply uncertainty to SIN", object_store$sinuncertainty)
  })
  # reporting rate box
  output$ui.rrbox <- renderUI({
    checkboxInput("rr", "Apply reporting rate", object_store$rr)
  })
  # reveal SE radio if plotButton tag = TRUE
  output$ui.rr.distribution <- renderUI({
    if (!isTRUE(input$rr)) return(NULL)
    actionButton("rr.distribution", paste0(object_store$temp_rrdistribution,'(',paste(object_store$temp_rrp,collapse=','),')'))
  })
  observeEvent(input$rr.distribution,{
    showModal(dataModal())
  })
  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      selectInput("distribution.select","Choose distribution",c('Beta','Uniform','Lognormal'),selected=object_store$temp_rrdistribution,multiple=FALSE),
      textInput("rrp1", "Parameter 1",value=object_store$temp_rrp[1]),
      textInput("rrp2", "Parameter 2",value=object_store$temp_rrp[2]),
      if (failed)
        div(tags$b("Invalid parameters", style = "color: red;")),
      if(!is.null(input$rrp1)&&!is.null(input$rrp2)&&failed==F)
        renderPlot({plot(seq(0,1.25,0.01),d(object_store$temp_rrdist)(seq(0,1.25,0.01)),typ='l',
          xlab='Reporting rate',ylab='Density',cex.axis=1.5,cex.lab=1.5)}),
      footer = tagList(
        modalButton("Close"),
        actionButton("ok", "OK")
      )
    )
  }
  
  # When OK button is pressed, check parameters. If successful,
  # remove the modal. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$ok, {
    # Check that data object exists and is data frame.
    pars <- as.numeric(c(input$rrp1,input$rrp2))
    if (sum(is.na(pars))>0||
        input$distribution.select=='Beta'&&(pars[1]<=0||pars[2]<=0)||
        input$distribution.select=='Uniform'&&(pars[1]>=pars[2])||
        input$distribution.select=='Lognormal'&&(pars[2]<=0)
    ) {
      showModal(dataModal(failed = TRUE))
    } else {
      object_store$temp_rrp[1] <- pars[1]
      object_store$temp_rrp[2] <- pars[2]
      object_store$temp_rrdistribution <- input$distribution.select
      if(object_store$temp_rrdistribution=='Beta') object_store$temp_rrdist <- Beta(pars[1],pars[2])
      if(object_store$temp_rrdistribution=='Uniform') object_store$temp_rrdist <- Unif(pars[1],pars[2])
      if(object_store$temp_rrdistribution=='Lognormal') object_store$temp_rrdist <- Lnorm(pars[1],pars[2])
      showModal(dataModal(failed = FALSE))
      #vals$data <- get(input$dataset)
    #  removeModal()
    }
  })
  # reveals compute button when distance file has been uploaded
  ##TODO change to when injury file has been uploaded
  output$ui.compute <- renderUI({
    inFile <- input$distancefile
    if (is.null(inFile)&&is.null(input$file2)&&is.null(object_store$mexicoButton)) return(NULL)
    if(is.null(object_store$fit_whw)){
      actionButton("compute", "Compute predictions")
    }else{
      print(c('poisson','NB')[as.numeric(input$modeltoggle)+1]!=object_store$model)
      print(c(c('poisson','NB')[as.numeric(input$modeltoggle)+1],object_store$model))
      print(input$sin!=object_store$sin)
      print(c(input$sin,object_store$sin))
      print(!is.null(input$sinuncertainty)&&input$sinuncertainty==T&&(object_store$lq!=input$lq||object_store$uq!=input$uq))
      print(c(!is.null(input$sinuncertainty),input$sinuncertainty==T,c(object_store$lq,input$lq,object_store$uq,input$uq)))
      print(object_store$lq!=input$lq||object_store$uq!=input$uq)
      print(c(object_store$lq,input$lq,object_store$uq,input$uq))
      print(object_store$sinuncertainty==T&&(is.null(input$sinuncertainty)||input$sinuncertainty==F))
      print(c(object_store$sinuncertainty==T,c(is.null(input$sinuncertainty),input$sinuncertainty==F)))
      print(object_store$sinuncertainty==F&&!is.null(input$sinuncertainty)&&input$sinuncertainty==T)
      print(c(object_store$sinuncertainty==F,!is.null(input$sinuncertainty),input$sinuncertainty==T))
      print(object_store$rr!=input$rr)
      print(c(object_store$rr,input$rr))
      print(input$rr==T&&
          (object_store$temp_rrdistribution!=object_store$rrdistribution||object_store$temp_rrp[1]!=object_store$rrp[1]||object_store$temp_rrp[2]!=object_store$rrp[2]))
      print(c(input$rr==T,c
          (object_store$temp_rrdistribution,object_store$rrdistribution,object_store$temp_rrp[1],object_store$rrp[1],object_store$temp_rrp[2],object_store$rrp[2])))
      ##TODO what if SIN uncertainty and quantiles were changed at different times?
      if(c('poisson','NB')[as.numeric(input$modeltoggle)+1]!=object_store$model || input$sin!=object_store$sin ||
          !is.null(input$sinuncertainty)&&input$sinuncertainty==T&&(object_store$lq!=input$lq||object_store$uq!=input$uq)){
        actionButton("compute", "Recompute models",style = "color: red")
      }else{
        if(object_store$lq!=input$lq||object_store$uq!=input$uq||
            object_store$sinuncertainty==T&&(is.null(input$sinuncertainty)||input$sinuncertainty==F)||
            object_store$sinuncertainty==F&&!is.null(input$sinuncertainty)&&input$sinuncertainty==T||
            object_store$rr!=input$rr||input$rr==T&&
            (object_store$temp_rrdistribution!=object_store$rrdistribution||object_store$temp_rrp[1]!=object_store$rrp[1]||object_store$temp_rrp[2]!=object_store$rrp[2])){
          actionButton("compute", "Recompute predictions",style = "color: red")
        }
      }
    }
  })
  # if distance file has been uploaded
  ##TODO change to if injury file has been uploaded
  # reveals toggle for whether the model is poisson (default) or NB (if chosen by user)
  output$ui.modeltoggle <- renderUI({
    inFile <- input$distancefile
    if (is.null(inFile)&&is.null(input$file2)&&is.null(object_store$mexicoButton)) return(NULL)
    value <- object_store$model=='NB'
    materialSwitch("modeltoggle", label = "Negative binomial model (default: Poisson)", status = "primary", right = TRUE,value=value)
  })
  # toggle for whether the model is poisson (default) or NB (if chosen by user)
  observeEvent(input$modeltoggle, {})
  ##TODO get min and max to work
  # reveals lower quantile input
  output$ui.lq <- renderUI({
    inFile <- input$distancefile
    if (is.null(inFile)&&is.null(input$file2)&&is.null(object_store$mexicoButton)) return(NULL)
    numericInput("lq", "Lower quantile", object_store$lq,min=1e-5,max=0.5)
  })
  # reveals upper quantile input
  output$ui.uq <- renderUI({
    inFile <- input$distancefile
    if (is.null(inFile)&&is.null(input$file2)&&is.null(object_store$mexicoButton)) return(NULL)
    numericInput("uq", "Upper quantile", object_store$uq,min=0.5,max=1-1e-5)
  })
  # read saved model
  observeEvent(input$file2, {
    inFile <- input$file2
    if (is.null(inFile)) return(NULL)
    object_store_temp <- readRDS(inFile$datapath)
    for(x in names(object_store_temp)) object_store[[x]] <- object_store_temp[[x]]
  })
  ##TODO superfluous object? Reports use of Mexico data
  observeEvent(input$mexico, {
    object_store$mexicoButton <- T
  })
  ##TODO make better
  # if using Mexico model, load saved model
  observeEvent(object_store$mexicoButton, {
    inFile <- 'saved_models/saved_mexico_city_NB_model.Rdata'
    if (is.null(inFile)) return(NULL)
    object_store_temp <- readRDS(inFile)
    for(x in names(object_store_temp)) object_store[[x]] <- object_store_temp[[x]]
  })
  # reveal button to save current model
  output$ui.save <- renderUI({
    if (is.null(object_store$plotButton)) return(NULL)
    shinySaveButton("save","Save computed model", "Save file as ...", filetype=list(Rdata="Rdata"))
  })
  # save current model
  ##TODO tidy(?)
  observeEvent(input$save, {
    save <- input$save
    volumes <- c("UserFolder"=paste0(getwd(),'/'))
    shinyFileSave(input, "save", roots=volumes)
    object_store_temp <- list()
    for(x in names(object_store)) object_store_temp[[x]] <- object_store[[x]]
    saveRDS(object_store_temp, paste0(volumes, input$save$name))
  })
  # reveal plot button if plotButton tag = TRUE
  output$ui.plot <- renderUI({
    if (is.null(object_store$plotButton)) return(NULL)
    actionButton("plot", "Plot")
  })
  # reveal SE radio if plotButton tag = TRUE
  output$ui.se <- renderUI({
    if (is.null(object_store$plotButton)) return(NULL)
    checkboxInput("SE", "Show quantiles", FALSE)
  })
  # reveal 'group to plot' choice if plotButton tag = TRUE
  output$ui.group <- renderUI({
    if (is.null(object_store$plotButton)) return(NULL)
    items <- object_store$covariates
    selectInput("group","Select group to view",items,multiple=FALSE)
  })
  # reveal 'item from group to plot' choice if plotButton tag = TRUE
  output$ui.subgroup <- renderUI({
    if (is.null(object_store$plotButton)||is.null(input$group)) return(NULL)
    # there are two models, each with different strike modes. If choosing from another covariate, we can list options from either data set as they should be the same.
    items <- unique(object_store$scenario_tabs[[1]][[1]][[paste(strsplit(input$group,' ')[[1]],collapse='_')]])
    # if choosing from strike mode, need to combine options from both data sets
    if(input$group=='Strike_mode')
    items <- unique(c(levels(object_store$scenario_tabs[[1]][[1]][[paste(strsplit(input$group,' ')[[1]],collapse='_')]]),levels(object_store$scenario_tabs[[1]][[2]][[paste(strsplit(input$group,' ')[[1]],collapse='_')]])))
    ##TODO check how this generalises to e.g. strike age, should we have it. Should be fine...?
    ##TODO add in 'sum' option, to sum over subgroups
    selectInput("subgroup","Select subgroup to view",items,multiple=FALSE)
  })
  # choose which other covariate to plot over
  output$ui.over <- renderUI({
    if (is.null(object_store$plotButton)||is.null(input$group)) return(NULL)
    items <- object_store$covariates[object_store$covariates!=input$group]
    selectInput("over","view over",items,multiple=FALSE)
  })
  # instruction to compute model. saves into object_store
  observeEvent(input$compute,{
    object_store <- getModelFits(object_store,input)
  })
  ##TODO superfluous object? Tells us to plot 
  plotObjects <- eventReactive(input$plot, {
    print(1)
  })
  # makes the plots
  output$plot <- renderPlot({
    print(2)
    plotObjects()
    print(c(input$lq,object_store$lq))
    prepPlots(object_store,input)
  })
}