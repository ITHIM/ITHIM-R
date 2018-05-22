ui <- fluidPage(
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  sidebarLayout(position='left',
    # Sidebar panel for inputs
    sidebarPanel('Inputs',
      # Injury file input
      fileInput(inputId='injuryfile', label='Upload injury data',
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".xlsx",
          ".csv")
      ),
      # Distance file input
      ##TODO this will go when we have a true synthetic population from which to obtain distance data
      uiOutput('ui.distance'),
      ## To use SIN values
      checkboxInput("sin", "Use SIN exponents", FALSE),
      # Use SIN uncertainty
      uiOutput('ui.sinuncertainty'),
      # Toggle switch to choose between poisson and NB models
      uiOutput('ui.modeltoggle'),
      # Choose quantiles
      uiOutput('ui.lq'),
      uiOutput('ui.uq'),
      # Button to compute model
      uiOutput('ui.compute'),
      # Button to save model
      uiOutput('ui.save'),
      hr(),
      # Button to import saved model
      fileInput(inputId='file2', label='Import saved model',
        accept = c(
          ".Rdata",
          ".RData")
      ),
      # Rob's shortcut to get Mexico model
      ##TODO remove
      actionButton('mexico','Use saved Mexico model'),
      hr(),
      # Choose which covariate to plot
      uiOutput('ui.group'),
      # Choose which realisation of covariate to plot
      uiOutput('ui.subgroup'),
      # Choose which other covariate to plot over
      uiOutput('ui.over'),
      # Radio button: plot SE or not
      uiOutput('ui.se'),
      # Button to plot/renew plot
      uiOutput('ui.plot')
    ),
    # Main panel shows the results
    mainPanel(
      plotOutput(outputId = "plot", width = "500px", height = "800px")
    )
  )
)
