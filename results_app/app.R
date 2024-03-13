library(shiny)
library(plotly)
library(tidyverse)
library(gridlayout)
library(shinyWidgets)
library(bslib)
library(readxl)
library(ggridges)

options(scipen = 10000)

# if (!exists("output_version")){
#   repo_sha <-  as.character(readLines(file.path("../repo_sha")))
#   output_version <- paste0(repo_sha, "_test_run")
# }

# hard coded repo_sha
# repo_sha <- "c601003c"

## Get the current repo sha
gitArgs <- c("rev-parse", "--short", "HEAD", ">", file.path("repo_sha"))
# Use shell command for Windows as it's failing with system2 for Windows (giving status 128)
if (.Platform$OS.type == "windows"){
  shell(paste(append("git", gitArgs), collapse = " "), wait = T)
} else {
  system2("git", gitArgs, wait = T)
}

repo_sha <-  as.character(readLines(file.path("repo_sha")))
# repo_sha <- "f7292509"
output_version <- paste0(repo_sha, "_test_run")
github_path <- "https://raw.githubusercontent.com/ITHIM/ITHIM-R/bogota/"
# github_path <- "../"
rel_path_health <- paste0(github_path, "results/multi_city/health_impacts/")
 

ylls <- read_csv(paste0(rel_path_health, "ylls.csv"))
deaths <- read_csv(paste0(rel_path_health, "deaths.csv"))

ylls$measures <- "Years of Life Lost (YLLs)"
deaths$measures <- "Deaths"

ylls_pathway <- read_csv(paste0(rel_path_health, "ylls_pathway.csv"))
deaths_pathway <- read_csv(paste0(rel_path_health, "deaths_pathway.csv"))

ylls_pathway$measures <- "Years of Life Lost (YLLs)"
deaths_pathway$measures <- "Deaths"

overall_pop <- ylls |> distinct(sex, age_cat, .keep_all = T) |> summarise(sum(pop_age_sex)) |> pull()

rel_path_inj <- paste0(github_path, "results/multi_city/inj/")

injury_risks_per_billion_kms_lng <- read_csv(paste0(rel_path_inj, "injury_risks_per_billion_kms.csv"))
injury_risks_per_100k_pop <- read_csv(paste0(rel_path_inj, "injury_risks_per_100k_pop.csv"))
injury_risks_per_100million_h_lng <- read_csv(paste0(rel_path_inj, "injury_risks_per_100million_h.csv"))

# Make sure all injury datasets have distinct rows and are in wide format (for mean, lb, and ub values)
# injury_risks_per_billion_kms_lng <- injury_risks_per_billion_kms_lng |> 
#   dplyr::group_by(mode, scenario, city, country, continent, mode_distance, measure) |>
#   dplyr::summarise(n = dplyr::n(), value = sum(value), .groups = "drop") |> 
#   dplyr::filter(n == 1L) |> 
#   dplyr::select(-n)
#   pivot_wider(injury_risks_per_billion_kms_lng, names_from = measure, values_from = value)
injury_risks_per_billion_kms_lng <- injury_risks_per_billion_kms_lng |> 
  dplyr::select(-mode_distance) |> 
  distinct() |> 
  pivot_wider(names_from = measure, values_from = value)
injury_risks_per_100k_pop <- injury_risks_per_100k_pop |> 
  distinct()  |> 
  pivot_wider(names_from = measure, values_from = value)
injury_risks_per_100million_h_lng <- injury_risks_per_100million_h_lng |> 
  distinct()  |> pivot_wider(names_from = measure, values_from = value)

# # Input params  
# input_parameter_file_path <- paste0(github_path, "InputParameters_v28.0.xlsx")
# city_input_params <- read_excel(input_parameter_file_path, sheet = "all_city_parameter_inputs")
# global_input_params <- read_excel(input_parameter_file_path, sheet = "all_global_parameter_inputs")

# output$table <- DT::renderDataTable(DT::datatable({

ren_scen <- function(df){
  df |> 
    filter(scenario != "Baseline") |> 
    mutate(scenario = case_when(
      grepl("Baseline_predicted|Baseline predicted", scenario) ~ "Baseline",
      grepl("Bicycling|cycle", scenario) ~ "CYC_SC",
      grepl("Public Transport|bus", scenario) ~ "BUS_SC",
      grepl("Motorcycling|motorcycle", scenario) ~ "MOT_SC",
      grepl("Car|car", scenario) ~ "CAR_SC"
      )
    )
}

injury_risks_per_billion_kms_lng <- ren_scen(injury_risks_per_billion_kms_lng)
injury_risks_per_100k_pop <- ren_scen(injury_risks_per_100k_pop)
injury_risks_per_100million_h_lng <- ren_scen(injury_risks_per_100million_h_lng)

# Ref: https://colorbrewer2.org/#type=diverging&scheme=Spectral&n=5
#['#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba']

# https://colorbrewer2.org/#type=qualitative&scheme=Set1&n=5
# ['#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00']
 
scen_colours <- c("Baseline" = '#ffffbf',
                  "Cycling Scenario" = '#abdda4',
                  "Car Scenario" = '#d7191c',
                  "Bus Scenario" = '#2b83ba',
                  "Motorcycle Scenario" = '#fdae61')

global_alpha_val <- 0.7

cities <- data.frame(
  continent = c('Africa/Asia', 'Latin_America', 'Africa/Asia', 'Africa/Asia', 'Africa/Asia', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Africa/Asia', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Africa/Asia', 'Africa/Asia', 'Africa/Asia'),
  country = c('Ghana', 'Brazil', 'India', 'India', 'India', 'Brazil', 'Colombia', 'Chile', 'Mexico', 'Argentina', 'South_Africa', 'Colombia', 'Colombia', 'Uruquay', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Kenya', 'Kenya', 'Mauritius'),
  city = c('accra', 'sao_paulo', 'delhi', 'bangalore', 'vizag', 'belo_horizonte', 'bogota', 'santiago', 'mexico_city', 'buenos_aires', 'cape_town', 'medellin', 'cali', 'montevideo', 'antofagasta', 'arica', 'copiapo', 'coquimbo_laserena', 'iquique_altohospicio', 'osorno', 'puerto_montt', 'san_antonio', 'temuco_padrelascasas', 'valdivia', 'gran_valparaiso', 'nairobi', 'kisumu', 'port_louis'),
  stringsAsFactors = FALSE
)


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

# Keep it only for those cities on which we have health data
cities <- cities |> filter(city %in% unique(combined_health_dataset$city))


ren_scen_health <- function(df){
  df[df$scenario == "motorcycle"  | df$scenario == "Motorcycle",]$scenario <- "MOT_SC"
  df[df$scenario == "car" | df$scenario == "Car",]$scenario <- "CAR_SC"
  df[df$scenario == "bus" | df$scenario == "Public Transport",]$scenario <- "BUS_SC"
  df[df$scenario == "cycle" | df$scenario == "Bicycling",]$scenario <- "CYC_SC"

  df
  
}

combined_health_dataset_pathway <- ren_scen_health(combined_health_dataset_pathway)
combined_health_dataset <- ren_scen_health(combined_health_dataset)

level_choices <- c("All-cause mortality: L1" = "level1",
                   "CVD/Respiratory diseases/Others: L2" = "level2",
                   "Individual diseases (cancers, IHD, diabetes): L3" = "level3")

per_100k <- c("Per 100k")

scens <- c("Cycling Scenario" = "CYC_SC",
           "Car Scenario" = "CAR_SC",
           "Bus Scenario" = "BUS_SC")#,
           #"Motorcycle Scenario" = "MOT_SC")

inj_scens <- c("Baseline" = "Baseline",
               "Cycling Scenario" = "CYC_SC",
               "Car Scenario" = "CAR_SC",
               "Bus Scenario" = "BUS_SC")#,
               #"Motorcycle Scenario" = "MOT_SC")

dose <- ylls |> filter(!is.na(level1)) |> distinct(dose)  |> pull()
dose_level2 <- ylls |> filter(!is.na(level2)) |> distinct(dose) |> pull()
dose_level3 <- ylls |> filter(!is.na(level3)) |> distinct(dose) |> pull()

dose_pathway <- ylls_pathway |> filter(!is.na(level1)) |> distinct(dose)  |> pull()
dose_pathway_level2 <- ylls_pathway |> filter(!is.na(level2)) |> distinct(dose) |> pull()
dose_pathway_level3 <- ylls_pathway |> filter(!is.na(level3)) |> distinct(dose) |> pull()

inj_modes <- injury_risks_per_billion_kms_lng$mode |> unique() |> sort()

inj_risk_types <- c("Billion kms", "Population by 100k", "100 Million hours")

in_cities <- cities$city

# “cerulean”, “cosmo”, “cyborg”, “darkly”, “flatly”, “journal”, “litera”, “lumen”, “lux”, 
# “materia”, “minty”, “morph”, “pulse”, “quartz”, “sandstone”, “simplex”, “sketchy”, “slate”, 
# “solar”, “spacelab”, “superhero”, “united”, “vapor”, “yeti”, “zephyr”

ui <- grid_page(
  theme = bs_theme(bootswatch = "yeti"),
  tags$head(HTML("<title>ITHIM Results</title>")), 
  # shinythemes::themeSelector(),
  layout = c(
    "area1 area2"
  ),
  row_sizes = c(
    "1fr"
  ),
  col_sizes = c(
    "0.20fr",
    "0.80fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "area2",
    tabsetPanel(
      id = "main_tab",
      tabPanel("Health Outcomes", 
               plotlyOutput("in_pivot_int", width = "100%", height = "100%")
              ),
      tabPanel("Injury Risks", plotlyOutput("in_inj_pivot", width = "100%", height = "100%"))#,
      # tabPanel("Params", DT::dataTableOutput("input_params"))
    )
  ),
  grid_card(
    area = "area1",
    pickerInput(inputId = "in_scens", 
                label = "Scenario (5% increase)",
                choices = scens,
                selected = scens[1],
                options = list(`actions-box` = TRUE), 
                multiple = TRUE),
    # br(),
    # treeInput(
    #   inputId = "in_cities",
    #   label = "Select cities:",
    #   choices = create_tree(cities),
    #   selected = cities$city,
    #   returnValue = "text",
    #   closeDepth = 0
    # ),
    br(),
    conditionalPanel(
      condition = "input.main_tab == 'Health Outcomes'",
      radioButtons(inputId = "in_measure", 
                   label = "Health Outcome",
                   inline = TRUE,
                   choices = c("Deaths", "Years of Life Lost (YLLs)")),
      radioButtons(inputId = "in_level", 
                   label = "Disease/cause levels",
                   choices = level_choices),
      radioButtons(inputId = "in_strata", 
                    label = "Stratification",
                    choices = c("None", "Sex", "Age Group"),
                    inline = TRUE,
                    select = "None"),
      checkboxInput(inputId = "in_per_100k", 
                    label = "per_100k",
                    value =  FALSE),
      pickerInput(inputId = "in_pathways", 
                  label = "Pathway/Dose",
                  choices = dose_pathway,
                  selected = dose_pathway,
                  options = list(`actions-box` = TRUE), 
                  multiple = TRUE),
      radioButtons(inputId = "in_int_pathway", 
                   label = "Pathways Interaction",
                   inline = TRUE,
                   choices = c("No", "Yes")),
      radioButtons(inputId = "in_CIs", 
                   label = "Conf. Interval",
                   inline = TRUE,
                   choices = c("No", "Yes"),
                   selected = "No")
      
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
    else if (x == "level3"){
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
    
    req(input$in_scens)
    # req(input$in_cities)
    req(input$in_inj_modes)
    
    filtered_scens <- input$in_scens
    # filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
    filtered_cities <- in_cities
    filtered_modes <- input$in_inj_modes
    
    local_df <- get_inj_data()
    text_colour <- "black"
      
    ylab <- "Distance: risk per Billion kilometeres"
    if (input$in_risk_type == "Population by 100k")
      ylab <- "Population: risk per 100K"
    else if (input$in_risk_type == "100 Million hours")
      ylab <- "Duration: risk per 100 Million hours"
    
    if(nrow(local_df) < 1)
      plotly::ggplotly(ggplot(data.frame()))
    else{
      
      gg <- ggplot(local_df) +
        aes(x = city, y = mean, fill = scenario) +
        geom_col(position = "dodge", alpha = global_alpha_val) +
        geom_text(aes(label = round(mean, 1)), size = 3, position = position_dodge(width = 0.9), vjust = -0.5) +
        scale_fill_hue(direction = 1) +
        coord_flip() +
        theme_minimal() +
        scale_fill_manual(values = scen_colours) +
        labs(title = paste(ylab, "(by mode)"), y = ylab) + 
        facet_wrap(vars(mode))
      
      plotly::ggplotly(gg)
    }
  })
  
  output$in_pivot_int <- renderPlotly({
    
    req(input$in_scens)
    # req(input$in_cities)
    req(input$in_level)
    req(input$in_measure)
    req(input$in_CIs)
    req(input$in_pathways)
    req(!is.null(input$in_strata))
    
    in_col_lvl <- input$in_level
    in_measure <- input$in_measure
    in_CIs <- input$in_CIs
    in_strata <- input$in_strata
    filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
    filtered_cities <- in_cities
    filtered_scens <- input$in_scens
    filtered_pathways <- input$in_pathways
    in_per_100 <- input$in_per_100k
    
    m <- list(
      l = 10,
      r = 10,
      b = 10,
      t = 50,
      pad = 50
    )
    
    
    if (!is.null(in_col_lvl)){
      
      text_colour <- "black"
        
        if (in_measure == "Deaths"){
          y_lab <- "Averted deaths per 100k"
          if (!in_per_100)
            y_lab <- "Averted Deaths"
        }else{
          y_lab <- "Saved Years of Life Lost (YLLs) per 100k"#<---- harms      #      benefits ---->  
          if (!in_per_100)
            y_lab <- "Saved Years of Life Lost (YLLs)"
        }
      
        if (in_strata == "Sex") y_lab <- paste(y_lab, "(stratified by sex)")
        else if (in_strata == "Age Group") y_lab <- paste(y_lab, " (stratified by age groups)")
        
        ld <- get_health_data()
        
        if(nrow(ld) < 1)
          plotly::ggplotly(ggplot(data.frame()))
        else{
          
          gg <- ggplot(ld, aes(x = metric_100k, y = dose, fill = scenario)) +
            {if(in_CIs == "No") geom_col(position=position_dodge2(), alpha = global_alpha_val)} +
            {if(in_CIs == "No") geom_text(aes(label = round(metric_100k, 1)), 
                                          size = 3, 
                                          position = position_dodge(width = 0.9), 
                                          vjust = -0.5)} +
            {if(in_CIs == "Yes") geom_boxplot(data = ld, aes(y = metric_100k, x = dose, fill = scenario), 
                     width = 0.5, position=position_dodge2(), alpha = global_alpha_val)} +
            {if(in_strata == "Sex") facet_wrap(~sex) else if(in_strata == "Age Group") facet_wrap(~age_cat)} +
            {if(in_CIs == "Yes") coord_flip()} +
            scale_fill_hue(direction = 1) +
            theme_minimal() +
            scale_fill_manual(values = scen_colours) +
            labs(title = y_lab,
                 y = ifelse(in_CIs == "Yes", y_lab, ""),
                 x = ifelse(in_CIs == "No", y_lab, ""))
          
          plotly::ggplotly(gg) |> plotly::layout(boxmode = "group"#,
                                                 #margin = m
                                                 )
        }
    }else{
      plotly::ggplotly(ggplot(data.frame()))
    }
  }) |> bindCache(input$in_level,
                  input$in_measure,
                  input$in_per_100k,
                  input$in_strata,
                  input$in_CIs,
                  # input$in_cities,
                  input$in_scens,
                  input$in_pathways,
                  input$in_int_pathway)
  
  
  get_inj_data <- reactive({
    
    filtered_scens <- input$in_scens
    # filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
    filtered_cities <- in_cities
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
      filter(city %in% filtered_cities &
               scenario %in% filtered_scens &
               mode %in% filtered_modes) |>
      mutate(scenario = case_when(
        scenario == "CYC_SC" ~ "Cycling Scenario",
        scenario == "CAR_SC" ~ "Car Scenario",
        scenario == "BUS_SC" ~ "Bus Scenario",
        scenario == "MOT_SC" ~ "Motorcycle Scenario",
        scenario == "Baseline" ~ "Baseline")) |> 
      distinct()
    
    if (is.null(local_df) || nrow(local_df) == 0)
      local_df <- data.frame()
    
    local_df
    
  })
  
  
  get_health_data <- reactive({
    
    in_col_lvl <- input$in_level
    in_measure <- input$in_measure
    in_int_pathway <- input$in_int_pathway
    in_strata <- input$in_strata
    in_CIs <- input$in_CIs
    # filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
    filtered_cities <- in_cities
    filtered_scens <- input$in_scens
    filtered_pathways <- input$in_pathways
    in_per_100 <- input$in_per_100k
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
        filter(dose %in% filtered_pathways) %>%
        {if(in_strata == "Sex") group_by(., sex, city, scenario, dose, cause) 
          else if(in_strata == "Age Group") group_by(., age_cat, city, scenario, dose, cause) 
          else group_by(., city, scenario, dose, cause)} %>% 
        {if(in_strata == "Sex") left_join(., (local_dataset |> distinct(sex, age_cat, .keep_all = T) |> group_by(sex) |> summarise(pop = sum(pop_age_sex)))) 
          else if (in_strata == "Age Group") left_join(., (local_dataset |> distinct(sex, age_cat, .keep_all = T) |> group_by(age_cat) |> summarise(pop = sum(pop_age_sex)))) 
          else cbind(., (local_dataset |> distinct(sex, age_cat, .keep_all = T) |> summarise(pop = sum(pop_age_sex))))} |> 
        summarise(metric_100k = round(ifelse(in_per_100,(sum(measure) / pop * 100000), sum(measure)), 1))
      
      if (length(filtered_pathways) > 1){
        
        total_dose <- ld |>
          filter(str_detect(cause, "lb")) |>
          ungroup() %>%
          {if(in_strata == "Sex") group_by(., sex, city, scenario) 
            else if(in_strata == "Age Group") group_by(., age_cat, city, scenario) 
            else group_by(., city, scenario)} %>%
          summarise(metric_100k = sum(metric_100k)) |>
          mutate(dose = "total", cause = "total_lb")
        
        total_dose <- rbind(total_dose,
                            ld |>
                              filter(str_detect(cause, "ub")) |>
                              ungroup() %>%
                              {if(in_strata == "Sex") group_by(., sex, city, scenario) 
                                else if(in_strata == "Age Group") group_by(., age_cat, city, scenario) 
                                else group_by(., city, scenario)} %>%
                              summarise(metric_100k = sum(metric_100k)) |>
                              mutate(dose = "total", cause = "total_ub")
                            
        )
        
        total_dose <- rbind(total_dose,
                            ld |>
                              filter(!str_detect(cause, "lb|ub")) |>
                              ungroup() %>%
                              {if(in_strata == "Sex") group_by(., sex, city, scenario) 
                                else if(in_strata == "Age Group") group_by(., age_cat, city, scenario) 
                                else group_by(., city, scenario)} %>%
                              summarise(metric_100k = sum(metric_100k)) |>
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
        filter(dose %in% filtered_pathways) %>% 
        {if(in_strata == "Sex") group_by(., sex, city, scenario, dose) 
          else if(in_strata == "Age Group") group_by(., age_cat, city, scenario, dose) 
          else group_by(., city, scenario, dose)} %>% 
        {if(in_strata == "Sex") left_join(., (local_dataset |> distinct(sex, age_cat, .keep_all = T) |> group_by(sex) |> summarise(pop = sum(pop_age_sex)))) 
          else if(in_strata == "Age Group") left_join(., (local_dataset |> distinct(sex, age_cat, .keep_all = T) |> group_by(age_cat) |> summarise(pop = sum(pop_age_sex)))) 
          else cbind(., (local_dataset |> distinct(sex, age_cat, .keep_all = T) |> summarise(pop = sum(pop_age_sex))))} |> 
        summarise(metric_100k = round(ifelse(in_per_100,(sum(measure) / pop * 100000), sum(measure)), 1))
      
      if (length(filtered_pathways) > 1){
        
        total_dose <- ld %>% 
          {if(in_strata == "Sex") group_by(., sex, city, scenario) 
            else if(in_strata == "Age Group") group_by(., age_cat, city, scenario) 
            else group_by(., city, scenario)} %>% 
          summarise(metric_100k = sum(metric_100k), dose = "total")
        
        ld <- plyr::rbind.fill(ld, total_dose)
      }
      
      
    }
    
    ld <- ld |>
      mutate(scenario = case_when(
        scenario == "CYC_SC" ~ "Cycling Scenario",
        scenario == "CAR_SC" ~ "Car Scenario",
        scenario == "BUS_SC" ~ "Bus Scenario",
        scenario == "MOT_SC" ~ "Motorcycle Scenario"),
        measure = in_measure)
    
    if (is.null(ld) || nrow(ld) == 0)
      ld <- data.frame()
    
    ld
    
  })
  
  output$download_top_data <- downloadHandler(
    filename = function() {
      
      fname <- ""
        
      if(input$main_tab == "Health Outcomes"){
        measure <- 'YLLs'
        if (input$in_measure == "Deaths")
          measure <- 'deaths'
        paste("health-data-selected-cities-", measure, "-", Sys.Date(), ".csv", sep="")
      }else if (input$main_tab == "Injury Risks"){
        
        measure <- "distance-risk-per-billion-kms"
        if (input$in_risk_type == "Population by 100k")
          measure <- "population-risk-per-100K"
        else if (input$in_risk_type == "100 Million hours")
          measure <- "duration-risk-per-100M-hrs"
        paste(measure, "-", Sys.Date(), ".csv", sep="")
        
      }else{
        paste("output.csv")
      }
    },
    content = function(file) {
      
      data <- NA
      
      if(input$main_tab == "Health Outcomes"){
        data <- get_health_data()
      }else if (input$main_tab == "Injury Risks"){
        data <- get_inj_data()
      }
      
      # data$measure <- input$in_measure
      write_csv(data, file)
    }
    
  )
  
  output$input_params <- DT::renderDataTable(DT::datatable({
    data <- city_input_params
    data
  }))
  
  is_interactive_plot <- reactive({
    
    if (input$in_int == 'Yes')
      return(TRUE)
    else
      return(FALSE)
  })
  
  output$plot_health <- renderUI({
    plotly::plotlyOutput("in_pivot_int")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
# run_with_themer(shinyApp(ui = ui, server = server))