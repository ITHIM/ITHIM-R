library(shiny)
library(plotly)
library(tidyverse)
library(gridlayout)
library(shinyWidgets)

options(scipen = 10000)
rel_path <- "../results/multi_city/health_impacts/"
ylls <- read_csv(paste0(rel_path, "ylls.csv"))
deaths <- read_csv(paste0(rel_path, "deaths.csv"))
ylls$measures <- "Years of Life Lost (YLLs)"
deaths$measures <- "Deaths"

ylls_pathway <- read_csv(paste0(rel_path, "ylls_pathway.csv"))
deaths_pathway <- read_csv(paste0(rel_path, "deaths_pathway.csv"))
ylls_pathway$measures <- "Years of Life Lost (YLLs)"
deaths_pathway$measures <- "Deaths"


colours = c("BUS_SC" = "blue", "CAR_SC" = "red", 
            "CYC_SC" = "green", "MOT_SC" = "orange") 

scen_colours <- c("Cycling Scenario" = "green",
                           "Car Scenario" = "red",
                           "Bus Scenario" = "blue",
                           "Motorcycle Scenario" = "orange")

cities <- data.frame(
  continent = c('Africa/Asia', 'Latin_America', 'Africa/Asia', 'Africa/Asia', 'Africa/Asia', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Africa/Asia', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Africa/Asia', 'Africa/Asia', 'Africa/Asia'),
  country = c('Ghana', 'Brazil', 'India', 'India', 'India', 'Brazil', 'Colombia', 'Chile', 'Mexico', 'Argentina', 'South_Africa', 'Colombia', 'Colombia', 'Uruquay', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Kenya', 'Kenya', 'Mauritius'),
  city = c('accra', 'sao_paulo', 'delhi', 'bangalore', 'vizag', 'belo_horizonte', 'bogota', 'santiago', 'mexico_city', 'buenos_aires', 'cape_town', 'medellin', 'cali', 'montevideo', 'antofagasta', 'arica', 'copiapo', 'coquimbo_laserena', 'iquique_altohospicio', 'osorno', 'puerto_montt', 'san_antonio', 'temuco_padrelascasas', 'valdivia', 'gran_valparaiso', 'nairobi', 'kisumu', 'port_louis'),
  stringsAsFactors = FALSE
)

cities[cities$country == 'Chile' & cities$city != 'santiago',]$continent <- "Small Chilean cities"
cities[cities$continent == 'Africa/Asia',]$continent <- "African/Asian cities"
cities[cities$continent == 'Latin_America',]$continent <- "Big Latin American cities"


combined_health_dataset <- rbind(ylls, deaths)
combined_health_dataset_pathway <- rbind(ylls_pathway, deaths_pathway)

level_choices <- c("All-cause mortality" = "level1",
                   "CVD/Respiratory diseases" = "level2",
                   "Individual diseases (cancers, IHD, diabetes)" = "level3")


scens <- c("Cycling Scenario" = "CYC_SC",
           "Car Scenario" = "CAR_SC",
           "Bus Scenario" = "BUS_SC",
           "Motorcycle Scenario" = "MOT_SC")

dose <- ylls |> filter(!is.na(level1)) |> distinct(dose)  |> pull()
dose_level2 <- ylls |> filter(!is.na(level2)) |> distinct(dose) |> pull()
dose_level3 <- ylls |> filter(!is.na(level3)) |> distinct(dose) |> pull()

dose_pathway <- ylls_pathway |> filter(!is.na(level1)) |> distinct(dose)  |> pull()
dose_pathway_level2 <- ylls_pathway |> filter(!is.na(level2)) |> distinct(dose) |> pull()
dose_pathway_level3 <- ylls_pathway |> filter(!is.na(level3)) |> distinct(dose) |> pull()


ui <- fillPage(
  
  fluidRow(
    column(width = 1, radioButtons(inputId = "in_measure", 
                                   label = "Health Outcome",
                                   choices = c("Years of Life Lost (YLLs)", "Deaths"))
    ),
    column(width = 1, radioButtons(inputId = "in_pathway", 
                                   label = "Pathways Interaction",
                                   choices = c("No", "Yes"))
    ),
    column(width = 2, radioButtons(inputId = "in_level", 
                                   label = "Disease/cause levels",
                                   choices = level_choices)
    ),
    column(width = 2, pickerInput(inputId = "in_scens", 
                                  label = "Scenario (5% increase)",
                                  choices = scens,
                                  selected = scens[1],
                                  options = list(`actions-box` = TRUE), 
                                  multiple = TRUE)
    ),
    column(width = 2, pickerInput(inputId = "in_pathways", 
                                  label = "Pathway/Dose",
                                  choices = dose_pathway,
                                  selected = dose_pathway,
                                  options = list(`actions-box` = TRUE), 
                                  multiple = TRUE)
    ),
    column(width = 1, radioButtons(inputId = "in_CIs", 
                                   label = "Conf. Interval",
                                   choices = c("No", "Yes"),
                                   selected = "No")
    ),
    column(
      width = 2,
      treeInput(
        inputId = "in_cities",
        label = "Select cities:",
        choices = create_tree(cities),
        selected = cities$city,
        returnValue = "text",
        closeDepth = 0
      )
    )),
    fluidRow(
      column(width = 3, downloadButton("download_top_data", "Download data", icon = shiny::icon("file-download"))
             ),
  ),
  fluidRow(
    column(width = 12, plotlyOutput("in_pivot", height = "100%"))
  )
)

server <- function(input, output, session) {
  
  observe({
    x <- input$in_level
    # browser()
    in_pathway <- input$in_pathway
    updated_dose <- dose_pathway
    
    if (x == "level1"){
      updated_dose <- dose_pathway
      if (in_pathway == "Yes")
        updated_dose <- dose
    }
    else if (x == "level2"){
      updated_dose <- dose_pathway_level2
      if (in_pathway == "Yes")
        updated_dose <- dose_level2
    }
    else if (x == "level2"){
      updated_dose <- dose_pathway_level3
      if (in_pathway == "Yes")
        updated_dose <- dose_level3
    }
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    updatePickerInput(session, "in_pathways",
                      choices = updated_dose,
                      selected = updated_dose
    )
  })
  
  output$in_pivot <- renderPlotly({
    
    in_col_lvl <- input$in_level
    in_measure <- input$in_measure
    in_CIs <- input$in_CIs
    filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
    filtered_scens <- input$in_scens
    filtered_pathways <- input$in_pathways
    
    y_lab <- "Years of Life Lost (YLLs) per 100k"
    if (in_measure == "Deaths")
      y_lab <- "Deaths per 100k"
    
    ld <- get_health_data()    
    
    if(nrow(ld) < 1)
      plotly::ggplotly(ggplot(data.frame()))
    else{
      gg <- ggplot(ld) +
        aes(x = city, y = metric_100k, fill = scenario) +
        {if(in_CIs == "Yes") geom_boxplot()} +
        {if(in_CIs == "No") geom_col(alpha = 0.7)}+
        scale_fill_hue(direction = 1) +
        coord_flip() +
        theme_minimal() +
        facet_grid(vars(), vars(dose))  +
        scale_fill_manual(values = scen_colours) +
        labs(y = y_lab)
      
      plotly::ggplotly(gg)
    }
  })
  
  
  get_health_data <- reactive({
    
    in_col_lvl <- input$in_level
    in_measure <- input$in_measure
    in_pathway <- input$in_pathway
    in_CIs <- input$in_CIs
    filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
    filtered_scens <- input$in_scens
    filtered_pathways <- input$in_pathways
    
    local_dataset <- combined_health_dataset
    
    if (in_pathway == "Yes")
      local_dataset <- combined_health_dataset
    else
      local_dataset <- combined_health_dataset_pathway
    
    if (in_CIs == "Yes"){
      
      ld <- local_dataset |> 
        filter(measures == in_measure) |> 
        filter((!is.na(!!rlang::sym(in_col_lvl)))) |>
        filter(city %in% filtered_cities) |> 
        filter(scenario %in% filtered_scens) |> 
        filter(dose %in% filtered_pathways) |> 
        group_by(city, scenario, dose, cause) |> 
        summarise(metric_100k = round(sum(metric_100k), 1))
      
      if (length(filtered_pathways) > 1){
        
        total_dose <- ld |> 
          filter(str_detect(cause, "lb")) |> 
          ungroup() |> 
          group_by(city, scenario) |> 
          summarise(metric_100k = round(sum(metric_100k, 1))) |> 
          mutate(dose = "total", cause = "total_lb")
        
        total_dose <- rbind(total_dose,
                            ld |> 
                              filter(str_detect(cause, "ub")) |> 
                              ungroup() |> 
                              group_by(city, scenario) |> 
                              summarise(metric_100k = round(sum(metric_100k, 1))) |> 
                              mutate(dose = "total", cause = "total_ub")
                            
        )
        
        total_dose <- rbind(total_dose,
                            ld |> 
                              filter(!str_detect(cause, "lb|ub")) |> 
                              ungroup() |> 
                              group_by(city, scenario) |> 
                              summarise(metric_100k = round(sum(metric_100k, 1))) |> 
                              mutate(dose = "total", cause = "total")
                            
        )
        ld <- plyr::rbind.fill(ld, total_dose)
      }
    }else{
      
      ld <- local_dataset |> 
        filter(measures == in_measure) |> 
        filter(!str_detect(cause, "lb|ub")) |> 
        filter((!is.na(!!rlang::sym(in_col_lvl)))) |>
        filter(city %in% filtered_cities) |> 
        filter(scenario %in% filtered_scens) |> 
        filter(dose %in% filtered_pathways) |> 
        group_by(city, scenario, dose) |> 
        summarise(metric_100k = round(sum(metric_100k), 1))
      
      if (length(filtered_pathways) > 1){
        
        total_dose <- ld |> 
          group_by(city, scenario) |> 
          summarise(metric_100k = round(sum(metric_100k), 1)) |> 
          mutate(dose = "total")
        
        ld <- plyr::rbind.fill(ld, total_dose)
      }
      
      
    }
    
    ld <- ld |> 
      mutate(scenario = case_when(
        scenario == "CYC_SC" ~ "Cycling Scenario",
        scenario == "CAR_SC" ~ "Car Scenario",
        scenario == "BUS_SC" ~ "Bus Scenario",
        scenario == "MOT_SC" ~ "Motorcycle Scenario"))
    
    if (is.null(ld) || nrow(ld) == 0)
      ld <- data.frame()
    
    ld
    
  })
  
  
  output$download_top_data <- downloadHandler(
    filename = function() {
      paste("health-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      
      data <- get_health_data()
      write.csv(data, file)
    }
    
  )
}

# Run the application 
shinyApp(ui = ui, server = server)