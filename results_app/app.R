library(shiny)
library(plotly)
library(tidyverse)
library(gridlayout)
library(shinyWidgets)
library(bslib)
library(readxl)
#library(shiny.semantic)

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

injury_risks_per_billion_kms_lng <- read_csv("../results/multi_city/whw_matrices/injury_risks_per_billion_kms_lng.csv")
injury_risks_per_100k_pop <- read_csv("../results/multi_city/whw_matrices/injury_risks_per_100k_pop.csv")
injury_risks_per_100million_h_lng <- read_csv("../results/multi_city/whw_matrices/injury_risks_per_100million_h_lng.csv")


# Input params
input_parameter_file_path <- "../InputParameters_v24.0.xlsx"
city_input_params <- read_excel(input_parameter_file_path, sheet = "all_city_parameter_inputs")
global_input_params <- read_excel(input_parameter_file_path, sheet = "all_global_parameter_inputs")

# output$table <- DT::renderDataTable(DT::datatable({

ren_scen <- function(df){
  df |> 
    filter(scenario != "Baseline") |> 
    mutate(scenario = case_when(
      grepl("Baseline_predicted", scenario) ~ "Baseline",
      grepl("Bicycling", scenario) ~ "CYC_SC",
      grepl("Public Transport", scenario) ~ "BUS_SC",
      grepl("Motorcycling", scenario) ~ "MOT_SC",
      grepl("Car", scenario) ~ "CAR_SC"
      )
    )
}

injury_risks_per_billion_kms_lng <- ren_scen(injury_risks_per_billion_kms_lng)
injury_risks_per_100k_pop <- ren_scen(injury_risks_per_100k_pop)
injury_risks_per_100million_h_lng <- ren_scen(injury_risks_per_100million_h_lng)

colours = c("Baseline" = "brown" , 
            "BUS_SC" = "purple", 
            "CAR_SC" = "red", 
            "CYC_SC" = "green", 
            "MOT_SC" = "orange") 

scen_colours <- c("Baseline" = "brown" ,
                  "Cycling Scenario" = "green",
                  "Car Scenario" = "red",
                  "Bus Scenario" = "purple",
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

inj_scens <- c("Baseline" = "Baseline",
               "Cycling Scenario" = "CYC_SC",
               "Car Scenario" = "CAR_SC",
               "Bus Scenario" = "BUS_SC",
               "Motorcycle Scenario" = "MOT_SC")

dose <- ylls |> filter(!is.na(level1)) |> distinct(dose)  |> pull()
dose_level2 <- ylls |> filter(!is.na(level2)) |> distinct(dose) |> pull()
dose_level3 <- ylls |> filter(!is.na(level3)) |> distinct(dose) |> pull()

dose_pathway <- ylls_pathway |> filter(!is.na(level1)) |> distinct(dose)  |> pull()
dose_pathway_level2 <- ylls_pathway |> filter(!is.na(level2)) |> distinct(dose) |> pull()
dose_pathway_level3 <- ylls_pathway |> filter(!is.na(level3)) |> distinct(dose) |> pull()

inj_modes <- injury_risks_per_billion_kms_lng$mode |> unique() |> sort()

inj_risk_types <- c("Billion kms", "Population by 100k", "100 Million hours")

# “cerulean”, “cosmo”, “cyborg”, “darkly”, “flatly”, “journal”, “litera”, “lumen”, “lux”, 
# “materia”, “minty”, “morph”, “pulse”, “quartz”, “sandstone”, “simplex”, “sketchy”, “slate”, 
# “solar”, “spacelab”, “superhero”, “united”, “vapor”, “yeti”, “zephyr”

ui <- grid_page(
  theme = bs_theme(bootswatch = "yeti"),
  layout = c(
    "area1 area2"
  ),
  row_sizes = c(
    "1fr"
  ),
  col_sizes = c(
    "0.15fr",
    "0.85fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "area2",
    tabsetPanel(
      id = "main_tab",
      tabPanel("Health Outcomes", plotlyOutput("in_pivot", width = "100%", height = "100%")),
      tabPanel("Injury Risks", plotlyOutput("in_inj_pivot", width = "100%", height = "100%")),
      tabPanel("Params", DT::dataTableOutput("input_params"))
    )
  ),
  grid_card(
    area = "area1",
    conditionalPanel(
      condition = "input.main_tab == 'Health Outcomes'",
      radioButtons(inputId = "in_measure", 
                   label = "Health Outcome",
                   choices = c("Years of Life Lost (YLLs)", "Deaths")),
      radioButtons(inputId = "in_int_pathway", 
                   label = "Pathways Interaction",
                   choices = c("No", "Yes")),
      radioButtons(inputId = "in_level", 
                   label = "Disease/cause levels",
                   choices = level_choices),
      radioButtons(inputId = "in_CIs", 
                   label = "Conf. Interval",
                   choices = c("No", "Yes"),
                   selected = "No"),
      pickerInput(inputId = "in_pathways", 
                  label = "Pathway/Dose",
                  choices = dose_pathway,
                  selected = dose_pathway,
                  options = list(`actions-box` = TRUE), 
                  multiple = TRUE)
    ),
    pickerInput(inputId = "in_scens", 
                label = "Scenario (5% increase)",
                choices = scens,
                selected = scens[1],
                options = list(`actions-box` = TRUE), 
                multiple = TRUE),
    treeInput(
      inputId = "in_cities",
      label = "Select cities:",
      choices = create_tree(cities),
      selected = cities$city,
      returnValue = "text",
      closeDepth = 0
    ),
    
    conditionalPanel(
      condition = "input.main_tab == 'Injury Risks'",
      pickerInput(inputId = "in_inj_modes", 
                  label = "Select modes:",
                  choices = inj_modes,
                  selected = inj_modes[1],
                  options = list(`actions-box` = TRUE), 
                  multiple = TRUE),
      radioButtons(inputId = "in_risk_type", 
                   label = "Risk Type: ",
                   choices = inj_risk_types,
                   selected = inj_risk_types[1])
    ),
    downloadButton("download_top_data", "Download data", icon = shiny::icon("file-download"))
    
  )
  
)
server <- function(input, output, session) {
  
  observeEvent(input$main_tab,{
    selected_scens <- input$in_scens[input$in_scens != "Baseline"]
    
    if(input$main_tab == "Health Outcomes"){
      updatePickerInput(session, "in_scens",
                        choices = scens,
                        selected = selected_scens
      )
    }else{
      updatePickerInput(session, "in_scens",
                        choices = inj_scens,
                        selected = selected_scens
      )
      
    }
  })
  
  observe({
    req(input$in_level)
    req(input$in_int_pathway)
    x <- input$in_level
    in_int_pathway <- input$in_int_pathway
    updated_dose <- dose_pathway
    
    if (x == "level1"){
      updated_dose <- dose_pathway
      if (in_int_pathway == "Yes")
        updated_dose <- dose
    }
    else if (x == "level2"){
      updated_dose <- dose_pathway_level2
      if (in_int_pathway == "Yes")
        updated_dose <- dose_level2
    }
    else if (x == "level2"){
      updated_dose <- dose_pathway_level3
      if (in_int_pathway == "Yes")
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
  
  output$in_inj_pivot <- renderPlotly({
    
    filtered_scens <- input$in_scens
    filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
    filtered_modes <- input$in_inj_modes
    
    local_df <- get_inj_data()
    text_colour <- "black"
    
    if(nrow(local_df) < 1)
      plotly::ggplotly(ggplot(data.frame()))
    else{
      gg <- ggplot(local_df) +
        aes(x = city, y = value, fill = scenario) +
        geom_col(position = "dodge") +
        {if(length(filtered_scens) == 1) geom_text(aes(label = round(value, 1)), position = position_stack(vjust = 0.9))} +
        scale_fill_hue(direction = 1) +
        coord_flip() +
        theme_minimal() +
        scale_fill_manual(values = scen_colours) +
        labs(y = ylab) + 
        facet_wrap(vars(mode))
      
      plotly::ggplotly(gg)
    }
  })
  
  output$in_pivot <- renderPlotly({
    
    in_col_lvl <- input$in_level
    in_measure <- input$in_measure
    in_CIs <- input$in_CIs
    filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
    filtered_scens <- input$in_scens
    filtered_pathways <- input$in_pathways
    
    if (!is.null(in_col_lvl)){
      
      text_colour <- "black"
        y_lab <- "Years of Life Lost (YLLs) per 100k"
        if (in_measure == "Deaths")
          y_lab <- "Deaths per 100k"
        
        ld <- get_health_data()
        
        if(nrow(ld) < 1)
          plotly::ggplotly(ggplot(data.frame()))
        else{
          
          gg <- ggplot(ld) +
            aes(x = city, y = metric_100k, fill = scenario) +
            {if(in_CIs == "Yes") geom_violin()} +# geom_boxplot(position = position_dodge(width = 1.5))} +
            {if(in_CIs == "No") geom_col(width = 0.5, colour="black", alpha = 0.7)}+
            # {if(in_CIs == "No" && length(filtered_scens) == 1) geom_text(aes(label = round(metric_100k)), hjust = -5, size = 3, colour = text_colour)}+
            {if(in_CIs == "No" && length(filtered_scens) == 1) geom_text(aes(label = round(metric_100k)), position = position_stack(vjust = 0.9), size = 3, colour = text_colour)} +
            scale_fill_hue(direction = 1) +
            coord_flip() +
            theme_minimal() +
            facet_grid(vars(), vars(dose))  +
            scale_fill_manual(values = scen_colours) +
            labs(y = y_lab)
          browser()
          plotly::ggplotly(gg) #|> style(text = ld$metric_100k, textposition = "auto", textfont = 12)
        }
    }else{
      plotly::ggplotly(ggplot(data.frame()))
    }
  }) |> bindCache(input$in_level,
                  input$in_measure,
                  input$in_CIs,
                  input$in_cities,
                  input$in_scens,
                  input$in_pathways,
                  input$in_int_pathway)
  
  
  get_inj_data <- reactive({
    
    filtered_scens <- input$in_scens
    filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
    filtered_modes <- input$in_inj_modes
    
    local_df <- injury_risks_per_billion_kms_lng
    ylab <- "Distance: risk per Billion kilometeres"
    if (input$in_risk_type == "Population by 100k"){
      local_df <- injury_risks_per_100k_pop
      ylab <- "Population: risk per 100K"
    }
    else if (input$in_risk_type == "100 Million hours"){
      local_df <- injury_risks_per_100million_h_lng
      ylab <- "Duration: risk per 100 Million hours"
    }
    
    text_colour <- "black"
      
    local_df <- local_df |>
      as.data.frame() |>
      filter(measure == "mean" &
               city %in% filtered_cities &
               scenario %in% filtered_scens &
               mode %in% filtered_modes) |>
      mutate(scenario = case_when(
        scenario == "CYC_SC" ~ "Cycling Scenario",
        scenario == "CAR_SC" ~ "Car Scenario",
        scenario == "BUS_SC" ~ "Bus Scenario",
        scenario == "MOT_SC" ~ "Motorcycle Scenario",
        scenario == "Baseline" ~ "Baseline"))
    
    if (is.null(local_df) || nrow(local_df) == 0)
      local_df <- data.frame()
    
    local_df
    
  })
  
  
  get_health_data <- reactive({
    
    in_col_lvl <- input$in_level
    in_measure <- input$in_measure
    in_int_pathway <- input$in_int_pathway
    in_CIs <- input$in_CIs
    filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
    filtered_scens <- input$in_scens
    filtered_pathways <- input$in_pathways
    
    local_dataset <- combined_health_dataset
    
    if (in_int_pathway == "Yes")
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
      # measure <- 'YLLs'
      # if (input$in_measure == "Deaths")
      #   measure <- 'deaths'
      # paste("health-data-selected-cities-", measure, "-", Sys.Date(), ".csv", sep="")
      
      paste("output.csv")
    },
    content = function(file) {
      
      if(input$main_tab == "Health Outcomes")
        data <- get_health_data()
      else(input$main_tab == "Health Outcomes")
        data <- get_inj_data()
      
      # data$measure <- input$in_measure
      write_csv(data, file)
    }
    
  )
  
  output$input_params <- DT::renderDataTable(DT::datatable({
    data <- city_input_params
    data
  }))
}

# Run the application 
shinyApp(ui = ui, server = server)
# run_with_themer(shinyApp(ui = ui, server = server))