library(shiny)
library(rpivotTable)
library(tidyverse)
library(gridlayout)
library(shinyWidgets)

rel_path <- "../results/multi_city/health_impacts/"
ylls_pathway <- read_csv(paste0(rel_path, "ylls_pathway.csv"))
deaths_pathway <- read_csv(paste0(rel_path, "deaths_pathway.csv"))
ylls_pathway$measures <- "ylls"
deaths_pathway$measures <- "deaths"

combined_health_dataset <- rbind(ylls_pathway, deaths_pathway)

level_choices <- ylls_pathway |> 
  dplyr::select(contains("lev")) |> 
  names()
cities <- ylls_pathway$city |> unique()
scens <- ylls_pathway$scenario |> unique()
dose <- ylls_pathway$dose |> unique()

ui <- fluidPage(

  fluidRow(
    column(width = 2, radioButtons(inputId = "in_measure", 
                                     label = "Health Outcome",
                                     choices = c("ylls", "deaths"))
    ),
    column(width = 2, radioButtons(inputId = "in_level", 
                                   label = "Disease/cause levels",
                                   choices = level_choices)
    ),
    column(width = 2, radioButtons(inputId = "in_scens", 
                                   label = "Scenario",
                                   choices = scens)
    ),
    column(width = 2, pickerInput(inputId = "in_pathways", 
                                   label = "Pathway/Dose",
                                   choices = dose,
                                  selected = dose,
                                  multiple = TRUE)
    ),
    column(width = 4, pickerInput(inputId = "in_cities", 
                                label = "Cities",  
                                choices = cities, 
                                selected = cities,
                                options = list(`actions-box` = TRUE), multiple = T)
    ),
    
  ),
  fluidRow(wellPanel(rpivotTableOutput("in_pivot")))
)

server <- function(input, output) {
  
  res_mod <- callModule(
    module = pickerGroupServer,
    id = "my-filters",
    data = ylls_pathway,
    vars = c("scenario", "city", "dose")
  )

  output$in_pivot <- renderRpivotTable({
    in_col_lvl <- input$in_level
    in_measure <- input$in_measure
    filtered_cities <- input$in_cities
    filtered_scens <- input$in_scens
    filtered_pathways <- input$in_pathways
    
    if (is.null(in_col_lvl))
      in_col_lvl <- "level1"
    if (is.null(filtered_cities))
      filtered_cities <- cities
    
    combined_health_dataset |> 
      filter(measures == in_measure) |> 
      filter(!str_detect(cause, "lb|ub")) |> 
      filter((!is.na(!!rlang::sym(in_col_lvl)))) |>
      filter(city %in% filtered_cities) |> 
      filter(scenario %in% filtered_scens) |> 
      filter(dose %in% filtered_pathways) |> rpivotTable(
        cols = c("city"),
        rows = c("dose","scenario"),
        aggregatorName = "Sum",
        vals = "measure_100k",
        rendererName = "Stacked Bar Chart", 
        #inclusions = list(scenario = list("BUS_SC")),
        #exclusions= list(city = list("buenos_aires", "santiago")),
        subtotals = FALSE#,
        #height = "100%", 
        #overflow = "scroll"
      )
           
  })
}

# Run the application 
shinyApp(ui = ui, server = server)