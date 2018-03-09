server <- function(input, output){
  # object storing all values
  object_store <- reactiveValues(fit=NULL,tab=NULL,injuries=NULL,plotButton=NULL,covariates=NULL,
    mexicoButton=NULL,distance=NULL,model='poisson')
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
  # if distance file has been uploaded
  ##TODO change to if injury file has been uploaded
  # reveals toggle for whether the model is poisson (default) or NB (if chosen by user)
  output$ui.modeltoggle <- renderUI({
    inFile <- input$distancefile
    if (is.null(inFile)) return(NULL)
    value <- object_store$model=='NB'
    materialSwitch("modeltoggle", label = "Negative binomial model (default: Poisson)", status = "primary", right = TRUE,value=value)
  })
  # toggle for whether the model is poisson (default) or NB (if chosen by user)
  observeEvent(input$modeltoggle, {
    models <- c('poisson','NB')
    object_store$model <- models[as.numeric(input$modeltoggle)+1]
  })
  # reads distance file
  # reveals compute button when distance file has been uploaded
  ##TODO change to when injury file has been uploaded
  output$ui.compute <- renderUI({
    inFile <- input$distancefile
    if (is.null(inFile)) return(NULL)
    distance <- read.ods(inFile$datapath)[[1]]
    object_store$distance <- distance
    actionButton("compute", "Compute predictions")
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
    inFile <- 'save_mexico.Rdata'
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
    items <- unique(object_store$tab_whw[[paste(strsplit(input$group,' ')[[1]],collapse='_')]])
    # if choosing from strike mode, need to combine options from both data sets
    if(input$group=='Strike_mode')
    items <- unique(c(levels(object_store$tab_whw[[paste(strsplit(input$group,' ')[[1]],collapse='_')]]),levels(object_store$tab_nov[[paste(strsplit(input$group,' ')[[1]],collapse='_')]])))
    ##TODO check how this generalises to e.g. strike age, should we have it. Should be fine...?
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
    object_store <- fitModel(object_store)
  })
  ##TODO superfluous object? Tells us to plot 
  plotObjects <- eventReactive(input$plot, {
  })
  # makes the plots
  output$plot <- renderPlot({
    plotObjects()
    prepPlots(object_store,input)
  })
}