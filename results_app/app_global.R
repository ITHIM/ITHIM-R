library(shiny)
library(plotly)
library(tidyverse)
library(shinyWidgets)
library(bslib)
library(readxl)
library(ggridges)
library(gt)

options(scipen = 10000)
SAVE_FIGURES <- FALSE
SVG <- FALSE

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
output_version <- paste0(repo_sha, ".rds")

# Assumes that multi_city_script.R has been run  
# read in input file
#io <- readRDS(paste0("../results/multi_city/io_3b3a1723_test_run.rds"))
# io <- readRDS(paste0("../results/multi_city/io_64b71ae5_test_run.rds"))
io <- readRDS(paste0("../results/multi_city/io_", output_version))




#github_path <- "https://raw.githubusercontent.com/ITHIM/ITHIM-R/global/"
github_path <- "../"

# io_path <- ("https://raw.githubusercontent.com/ITHIM/ITHIM-R/global/results/multi_city/io_ac8aa8a0_test_run.rds")
# #io_path <- readRDS("https://github.com/ITHIM/ITHIM-R/blob/fec8e705bd50590a473ae1708523a02546460b65/results/multi_city/io_ac8aa8a0_test_run.rds")
# #githubURL <- ("https://raw.githubusercontent.com/derek-corcoran-barrios/LastBat/master/best2.My.Lu2.rds")
# download.file(io_path,"io.rds", method="curl")
# io <- readRDS("io.rds")


# results_file
results_file <- 'multi_city'

rel_path_health <- paste0(github_path, "results/",results_file,"/health_impacts/")
# rel_path_health <- paste0("https://github.com/ITHIM/ITHIM-R/blob/46ef9f826fe94efc0110feeb0311f9759ad5dca3/results/multi_city/health_impacts/")

# https://github.com/ITHIM/ITHIM-R/blob/46ef9f826fe94efc0110feeb0311f9759ad5dca3/results/multi_city/health_impacts/deaths.csv

ren_dose <- function(df){
  df[df$dose == "RTI",]$dose <- "Road Traffic Fatalities"
  df[df$dose == "AP",]$dose <- "Air Pollution"
  df[df$dose == "PA",]$dose <- "Physical Activity"
  df[df$dose == "PA and AP",]$dose <- "Physical Activity and Air Pollution"
  df
}

ren_sex <- function(df){
  df[df$sex == "male",]$sex <- "Male"
  df[df$sex == "female",]$sex <- "Female"
  df
}

ylls <- read_csv(paste0(rel_path_health, "ylls.csv"))
deaths <- read_csv(paste0(rel_path_health, "deaths.csv"))

ylls$measures <- "Years of Life Lost (YLLs)"
deaths$measures <- "Deaths"

ylls_pathway <- read_csv(paste0(rel_path_health, "ylls_pathway.csv"))
deaths_pathway <- read_csv(paste0(rel_path_health, "deaths_pathway.csv"))

ylls_pathway$measures <- "Years of Life Lost (YLLs)"
deaths_pathway$measures <- "Deaths"

ylls <- ren_dose(ylls)
deaths <- ren_dose(deaths)
ylls_pathway <- ren_dose(ylls_pathway)
deaths_pathway <- ren_dose(deaths_pathway)

# ylls <- ren_sex(ylls)
# deaths <- ren_sex(deaths)
# ylls_pathway <- ren_sex(ylls_pathway)
# deaths_pathway <- ren_sex(deaths_pathway)

overall_pop <- ylls |> distinct(sex, age_cat, .keep_all = T) |> summarise(sum(pop_age_sex)) |> pull()

rel_path_inj <- paste0(github_path, "results/",results_file,"/inj/")

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
      grepl("Bicycling|cycle|Cycling", scenario) ~ "CYC_SC",
      grepl("Public Transport|bus|Bus", scenario) ~ "BUS_SC",
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

# Plot colours for each scenario
scen_colours <- c("Baseline" = '#b15928',
                  "Cycling" = '#abdda4',
                  "Car" = '#d7191c',
                  "Bus" = '#2b83ba',
                  "Motorcycle" = '#fdae61')

global_alpha_val <- 0.7

cities <- data.frame(
  continent = c('Africa/Asia', 'Latin_America', 'Africa/Asia', 'Africa/Asia', 'Africa/Asia', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Africa/Asia', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Africa/Asia', 'Africa/Asia', 'Africa/Asia'),
  country = c('Ghana', 'Brazil', 'India', 'India', 'India', 'Brazil', 'Colombia', 'Chile', 'Mexico', 'Argentina', 'South_Africa', 'Colombia', 'Colombia', 'Uruquay', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Kenya', 'Kenya', 'Mauritius'),
  city = c('accra', 'sao_paulo', 'delhi', 'bangalore', 'vizag', 'belo_horizonte', 'bogota', 'santiago', 'mexico_city', 'buenos_aires', 'cape_town', 'medellin', 'cali', 'montevideo', 'antofagasta', 'arica', 'copiapo', 'coquimbo_laserena', 'iquique_altohospicio', 'osorno', 'puerto_montt', 'san_antonio', 'temuco_padrelascasas', 'valdivia', 'gran_valparaiso', 'nairobi', 'kisumu', 'port_louis'),
  stringsAsFactors = FALSE
)


cities <- data.frame(
  continent = c('Africa/Asia', 'Africa/Asia', 'Latin_America', 'Africa/Asia', 'Africa/Asia', 'Africa/Asia', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Africa/Asia', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Latin_America', 'Africa/Asia', 'Africa/Asia', 'Africa/Asia'),
  country = c('Ghana', 'Ghana', 'Brazil', 'India', 'India', 'India', 'Brazil', 'Colombia', 'Chile', 'Mexico', 'Argentina', 'South_Africa', 'Colombia', 'Colombia', 'Uruquay', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Chile', 'Kenya', 'Kenya', 'Mauritius'),
  city = c('accra', 'accra_copy', 'sao_paulo', 'delhi', 'bangalore', 'vizag', 'belo_horizonte', 'bogota', 'santiago', 'mexico_city', 'buenos_aires', 'cape_town', 'medellin', 'cali', 'montevideo', 'antofagasta', 'arica', 'copiapo', 'coquimbo_laserena', 'iquique_altohospicio', 'osorno', 'puerto_montt', 'san_antonio', 'temuco_padrelascasas', 'valdivia', 'gran_valparaiso', 'nairobi', 'kisumu', 'port_louis'),
  stringsAsFactors = FALSE
)

cities[cities$country == 'Chile' & cities$city != 'santiago',]$continent <- "Small Chilean cities"
cities[cities$continent == 'Africa/Asia',]$continent <- "African/Asian cities"
cities[cities$continent == 'Latin_America',]$continent <- "Big Latin American cities"

combined_health_dataset <- rbind(ylls, deaths)
combined_health_dataset_pathway <- rbind(ylls_pathway, deaths_pathway)

# Keep it only for those cities on which we have health data
cities <- cities |> filter(city %in% unique(combined_health_dataset$city)) |> mutate(city = str_to_title(city))


ren_scen_health <- function(df){
  df[df$scenario == "Motorcycle" | df$scenario == "motorcycle"  | df$scenario == "Motorcycle" | df$scenario == "Motorcycling",]$scenario <- "MOT_SC"
  df[df$scenario == "Car" | df$scenario == "car" | df$scenario == "Car",]$scenario <- "CAR_SC"
  df[df$scenario == "Bus" | df$scenario == "bus" | df$scenario == "Public Transport",]$scenario <- "BUS_SC"
  df[df$scenario == "Cycling" | df$scenario == "cycle" | df$scenario == "Bicycling",]$scenario <- "CYC_SC"
  df
  
}

combined_health_dataset_pathway <- ren_scen_health(combined_health_dataset_pathway)
combined_health_dataset <- ren_scen_health(combined_health_dataset)

combined_health_dataset_pathway <- ren_dose(combined_health_dataset_pathway)
combined_health_dataset <- ren_dose(combined_health_dataset)


ren_at <- function(df, colname){
  df[df[[colname]] == "active_travel",][[colname]] <- "Active Travel (Walking and Cycling)"
  df
}

injury_risks_per_100k_pop <- ren_at(injury_risks_per_100k_pop, colname = "mode")
injury_risks_per_100million_h_lng <- ren_at(injury_risks_per_100million_h_lng, colname = "mode")
injury_risks_per_billion_kms_lng <- ren_at(injury_risks_per_billion_kms_lng, "mode")

capitalize_cols <- function(df, colname){
  df[[colname]] <- str_to_title(df[[colname]])
  df
}

injury_risks_per_100k_pop <- capitalize_cols(injury_risks_per_100k_pop, colname = "mode")
injury_risks_per_100million_h_lng <- capitalize_cols(injury_risks_per_100million_h_lng, colname = "mode")
injury_risks_per_billion_kms_lng <- capitalize_cols(injury_risks_per_billion_kms_lng, "mode")

injury_risks_per_100k_pop <- capitalize_cols(injury_risks_per_100k_pop, colname = "city")
injury_risks_per_100million_h_lng <- capitalize_cols(injury_risks_per_100million_h_lng, colname = "city")
injury_risks_per_billion_kms_lng <- capitalize_cols(injury_risks_per_billion_kms_lng, "city")


level_choices <- c("All-cause mortality: L1" = "level1",
                   "Cancer, cardiovascular, respiratory, other mortality: L2" = "level2",
                   "Individual diseases mortality (e.g., COPD, stroke, type-2 diabetes, lung cancers, etc.): L3" = "level3")

per_100k <- c("Per 100k")

scens <- c("Cycling" = "CYC_SC",
           "Car" = "CAR_SC",
           "Bus" = "BUS_SC",
           "Motorcycle" = "MOT_SC")

inj_scens <- c("Baseline" = "Baseline",
               "Cycling" = "CYC_SC",
               "Car" = "CAR_SC",
               "Bus" = "BUS_SC",
               "Motorcycle" = "MOT_SC")

dose <- ylls |> filter(!is.na(level1)) |> distinct(dose)  |> pull()
dose_level2 <- ylls |> filter(!is.na(level2)) |> distinct(dose) |> pull()
dose_level3 <- ylls |> filter(!is.na(level3)) |> distinct(dose) |> pull()

dose_pathway <- ylls_pathway |> filter(!is.na(level1)) |> distinct(dose)  |> pull()
dose_pathway_level2 <- ylls_pathway |> filter(!is.na(level2)) |> distinct(dose) |> pull()
dose_pathway_level3 <- ylls_pathway |> filter(!is.na(level3)) |> distinct(dose) |> pull()

um <- unique(injury_risks_per_billion_kms_lng$mode)

inj_modes <- append(um[!um %in% 'Total'] |> sort(), "Total")

inj_risk_types <- c("Billion kms", "Population by 100k people", "100 million hours")

in_cities <- "osorno"# cities$city

# “cerulean”, “cosmo”, “cyborg”, “darkly”, “flatly”, “journal”, “litera”, “lumen”, “lux”, 
# “materia”, “minty”, “morph”, “pulse”, “quartz”, “sandstone”, “simplex”, “sketchy”, “slate”, 
# “solar”, “spacelab”, “superhero”, “united”, “vapor”, “yeti”, “zephyr”
# 

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "yeti"),
  title = paste0("ITHIM results for multi cities"),
  sidebar = sidebar(
    pickerInput(inputId = "in_scens", 
                label = "Scenario (5% increase)",
                choices = scens,
                selected = scens,
                options = list(`actions-box` = TRUE), 
                multiple = TRUE),
    br(),
    
    treeInput(
      inputId = "in_cities",
      label = "Select cities:",
      choices = create_tree(cities),
      selected = "Osorno", #cities |> filter(city == "Osorno"),
      returnValue = "text",
      closeDepth = 0
    ),
    br(),
    conditionalPanel(
      condition = "input.main_tab == 'Health Outcomes'",
      radioButtons(inputId = "in_measure", 
                   label = "Health Outcome",
                   inline = TRUE,
                   choices = c("Deaths", "Years of Life Lost (YLLs)")),
      radioButtons(inputId = "in_level", 
                   label = "Outcome levels",
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
      # radioButtons(inputId = "in_CIs", 
      #              label = "Conf. Interval",
      #              inline = TRUE,
      #              choices = c("No", "Yes"),
      #              selected = "No")
      
    ),
    
    conditionalPanel(
      condition = "input.main_tab == 'Trip behaviour'",
      radioButtons(inputId = "in_trip_measure", 
                   label = "Trip measure",
                   inline = TRUE,
                   choices = c("Scenario", "Trip", "Distance"))
    ),
    conditionalPanel(
      condition = "input.main_tab == 'PA Exposures' || input.main_tab == 'AP Exposures'",
      radioButtons("display_choice", "Choose display:",
                   choices = c("Figure", "Table")),
    ),
    
    conditionalPanel(
      condition = "input.main_tab == 'Injury Risks'",
      pickerInput(inputId = "in_inj_modes",
                  label = "Select modes:",
                  choices = inj_modes,
                  selected = inj_modes[!inj_modes %in% 'Truck'],# inj_modes[length(inj_modes)],
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE),
      radioButtons(inputId = "in_risk_type",
                   label = "Risk Type: ",
                   choices = inj_risk_types,
                   selected = inj_risk_types[1])
    ),
    downloadButton("download_top_data", "Download data", icon = shiny::icon("file-download"))
  ),
  navset_card_underline(
    id = "main_tab",
    full_screen = TRUE,
    nav_panel("Health Outcomes", 
              plotlyOutput("in_pivot_int")),
    nav_panel("PA Exposures", 
              uiOutput("pa_exp")),
    #plotlyOutput("in_pa_exp"),
    #DT::dataTableOutput("plotScenariosPATable")),
    nav_panel("AP Exposures", 
              uiOutput("ap_exp")),
    nav_panel("Trip behaviour",
              gt_output("trip_table")
    ),
    nav_panel("Injury Risks", 
              plotlyOutput("in_inj_pivot"))
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
    req(input$in_cities)
    req(input$in_inj_modes)
    
    filtered_scens <- input$in_scens
    filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull() |> tolower()
    #filtered_cities <- tolower(in_cities)
    filtered_modes <- input$in_inj_modes
    
    local_df <- get_inj_data()
    text_colour <- "black"
    
    ylab <- "Distance: risk per billion kilometers"
    if (input$in_risk_type == "Population by 100k people")
      ylab <- "Population: risk per 100K people"
    else if (input$in_risk_type == "100 million hours")
      ylab <- "Duration: risk per 100 million hours"
    
    
    fname <- do.call(paste, c(as.list(filtered_scens),
                              as.list(filtered_modes),
                              as.list(input$in_risk_type),
                              sep = "-"))
    
    if(nrow(local_df) < 1)
      plotly::ggplotly(ggplot(data.frame()))
    else{
      
      gg <- ggplot(local_df |> rename(City = city)) +
        aes(x = City, y = mean, fill = scenario) +
        geom_col(position = "dodge", alpha = global_alpha_val) +
        geom_text(aes(label = round(.data[["mean"]], 1)),
                  size = 5,
                  position = position_dodge(width = 0.9),
                  #vjust = -0.5,
                  fontface = "bold",
                  hjust = 1,
                  halight = 1) +
        scale_fill_hue(direction = 1) +
        coord_flip() +
        theme_minimal() +
        scale_fill_manual(values = scen_colours) +
        labs(title = paste(ylab, "(by mode)"), 
             y = ylab,
             x = "",
             fill='Scenario') + 
        facet_wrap(vars(mode))
      # browser()
      if (SAVE_FIGURES)
        ggsave(paste0("figures/", fname, ifelse(SVG, ".svg", ".png")), plot = gg, width=10, height=8)
      
      
      plotly::ggplotly(gg) |>       plotly::config(
        toImageButtonOptions = list(
          format = "svg",
          filename = fname,
          width = NULL,
          height = NULL
        )) |> layout(
          margin = list(b = 50, l = 50) # to fully display the x and y axis labels
        )
      
    }
  })
  
  
  get_city_outcoems_df <- function(cities, obj){
    
    return (cities |>
              purrr::map(function(city) {
                io[[city]]$outcomes[[obj]] |>
                  dplyr::mutate(city_name = city)
              }) |> list_rbind())
  }
  
  output$in_ap_exp_fig <- renderPlotly({
    
    req(input$in_scens)
    req(input$in_cities)
    
    filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
    filtered_cities <- tolower(filtered_cities)
    filtered_scens <- input$in_scens
    
    
    # Desired arguments
    qymax <- 0.9
    qymin <- 0.1
    qmiddle <- 0.5
    qupper <- 0.8
    qlower <- 0.2
    
    
    pm_conc_pp <- get_summary_data("pm_conc_pp", filtered_cities, filtered_scens)
    
    plotly::ggplotly(
      ggplot(pm_conc_pp) +
        aes(
          x = name,
          y = middle,
          fill = name,
          group = city_name,
          ymin = ymin,
          ymax = ymax
        ) +
        geom_bar(stat = "summary", fun = "sum") +
        geom_errorbar(aes(ymin = ymin,ymax = ymax)) +
        scale_fill_manual(values = scen_colours) +
        coord_flip() +
        theme_minimal() +
        facet_wrap(vars(city_name))
    )
    
    
    #browser()
    #print(g)
    #g
    
    
  })  |> bindCache(input$in_cities,
                   input$in_scens)
  
  
  output$in_pa_exp_fig <- renderPlotly({
    
    req(input$in_scens)
    req(input$in_cities)
    
    filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
    filtered_cities <- tolower(filtered_cities)
    filtered_scens <- input$in_scens
    
    df <- get_summary_data("mmets", filtered_cities, filtered_scens)
    
    plotly::ggplotly(
      ggplot(df) +
        aes(
          x = name,
          y = middle,
          fill = name,
          group = city_name,
          ymin = ymin,
          ymax = ymax
        ) +
        geom_bar(stat = "summary", fun = "sum") +
        geom_errorbar(aes(ymin = ymin,ymax = ymax)) +
        scale_fill_manual(values = scen_colours) +
        coord_flip() +
        theme_minimal() +
        facet_wrap(vars(city_name))
    )
    
    
    #browser()
    #print(g)
    #g
    
    
  })  |> bindCache(input$in_cities,
                   input$in_scens)
  
  get_summary_data <- function(var_name, filtered_cities, filtered_scens){
    
    
    # Desired arguments
    qymax <- 0.9
    qymin <- 0.1
    qmiddle <- 0.5
    qupper <- 0.8
    qlower <- 0.2
    
    
    return(get_city_outcoems_df(filtered_cities, var_name) |> 
             pivot_longer(cols = -c(participant_id, age, sex, age_cat, city_name)) |> 
             group_by(city_name, name) |> 
             mutate(name = case_when(
               grepl("base", name) ~ "Baseline",
               grepl("sc_bus", name) ~ "BUS_SC",
               grepl("sc_cycle", name) ~ "CYC_SC",
               grepl("sc_motorcycle", name) ~ "MOT_SC",
               grepl("sc_car", name) ~ "CAR_SC"
             )) |> 
             filter(name %in% filtered_scens) |> 
             mutate(name = case_when(
               name == "CYC_SC" ~ "Cycling",
               name == "CAR_SC" ~ "Car",
               name == "BUS_SC" ~ "Bus",
               name == "MOT_SC" ~ "Motorcycle",
               name == "Baseline" ~ "Baseline")) |> 
             summarise(lower = quantile(value, qlower),
                       upper = quantile(value, qupper), 
                       middle = quantile(value, qmiddle), 
                       IQR = diff(c(lower, upper)),
                       ymin = max(quantile(value, qymin), lower - 1.5 * IQR), 
                       ymax = min(quantile(value, qymax), upper + 1.5 * IQR),
                       outliers = list(value[which(value > upper + 1.5 * IQR | 
                                                     value < lower - 1.5 * IQR)])))
    
  }
  
  
  output$in_pivot_int <- renderPlotly({
    
    req(input$in_scens)
    req(input$in_cities)
    req(input$in_level)
    req(input$in_measure)
    # req(input$in_CIs)
    req(input$in_pathways)
    req(!is.null(input$in_strata))
    
    in_col_lvl <- input$in_level
    in_measure <- input$in_measure
    in_CIs <- "No"# input$in_CIs
    in_strata <- input$in_strata
    filtered_cities <- cities |> filter(city %in% tolower(input$in_cities)) |> dplyr::select(city) |> pull()
    #filtered_cities <- in_cities
    filtered_scens <- input$in_scens
    filtered_pathways <- input$in_pathways
    in_per_100k <- input$in_per_100k
    
    t <- list(
      
      family = "Courier New",
      
      size = 14,
      
      color = "blue")
    
    t1 <- list(
      
      family = "Times New Roman",
      
      color = "red"
      
    )
    
    t2 <- list(
      family = "Arial",
      # size = 10,
      color = "black")
    
    t3 <- list(family = 'Arial')
    
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
        y_lab <- "Averted deaths per 100k people"
        if (!in_per_100k)
          y_lab <- "Averted deaths"
      }else{
        y_lab <- "Saved Years of Life Lost (YLLs) per 100k people"#<---- harms      #      benefits ---->  
        if (!in_per_100k)
          y_lab <- "Saved Years of Life Lost (YLLs)"
      }
      
      if (in_strata == "Sex") y_lab <- paste(y_lab, "(stratified by sex)")
      else if (in_strata == "Age Group") y_lab <- paste(y_lab, " (stratified by age groups)")
      
      ld <- get_health_data()
      
      if(nrow(ld) < 1)
        plotly::ggplotly(ggplot(data.frame()))
      else{
        
        
        
        var.choice <- ifelse(in_per_100k, "metric_100k", "metric")
        gg <- ggplot(data = ld, aes(x = .data[[var.choice]], y = dose, fill = scenario, group = city)) +
          {if(in_CIs == "No") geom_col(position=position_dodge2(), alpha = global_alpha_val)} +
          #{if(in_CIs == "No") geom_text(aes(label = city), size = 3, position = position_dodge(width = 0.9))} + 
          # {if(in_CIs == "No") geom_text(aes(label = round(.data[[var.choice]], 1)),
          #                               size = 3,
          #                               position = position_dodge(width = 0.9),
          #                               vjust = -0.5)} +
          {if(in_CIs == "Yes") geom_boxplot(data = ld, aes(y = .data[[var.choice]], x = dose, fill = scenario), 
                                            width = 0.5, position=position_dodge2(), alpha = global_alpha_val)} +
          {if(in_strata == "Sex") facet_wrap(~sex) else if(in_strata == "Age Group") facet_wrap(~age_cat)} +
          {if(in_CIs == "Yes") coord_flip()} +
          scale_fill_hue(direction = 1) +
          theme_minimal() +
          scale_fill_manual(values = scen_colours) +
          labs(title = y_lab,
               y = ifelse(in_CIs == "Yes", y_lab, ""),
               x = ifelse(in_CIs == "No", y_lab, ""),
               fill='Scenario')
        
        fname <- do.call(paste, c(as.list(filtered_pathways), 
                                  as.list(filtered_scens),
                                  as.list(in_col_lvl),
                                  "strata", as.list(in_strata),
                                  as.list(in_measure),
                                  "path_inter", as.list(input$in_int_pathway),
                                  "per_100k", as.list(input$in_per_100k),
                                  sep = "-"))
        
        # ggsave(fname,units="in", width=5, height=4, dpi=300)
        # 
        if (SAVE_FIGURES)
          ggsave(paste0("figures/", fname, ifelse(SVG, ".svg", ".png")), plot = gg, width=10, height=8)
        
        
        plotly::ggplotly(gg) |> 
          # plotly::layout(legend = list(orientation = "h", 
          #                              xanchor = "center",  
          #                              x = 0.5,
          #                              font = t2)) |> 
          plotly::config(
            toImageButtonOptions = list(
              format = "svg",
              filename = fname,
              width = NULL,
              height = NULL
            )) |> layout(
              margin = list(b = 50, l = 50) # to fully display the x and y axis labels
            )
        
      }
    }else{
      plotly::ggplotly(ggplot(data.frame()))
    }
  }) |> bindCache(input$in_level,
                  input$in_measure,
                  input$in_per_100k,
                  input$in_strata,
                  # input$in_CIs,
                  input$in_cities,
                  input$in_scens,
                  input$in_pathways,
                  input$in_int_pathway)
  
  
  get_inj_data <- reactive({
    
    filtered_scens <- input$in_scens
    filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
    #filtered_cities <- in_cities
    filtered_modes <- input$in_inj_modes
    
    local_df <- injury_risks_per_billion_kms_lng
    ylab <- "Distance: risk per Billion kilometers"
    if (input$in_risk_type == "Population by 100k people"){
      local_df <- injury_risks_per_100k_pop
      ylab <- "Population: risk per 100K"
    }
    else if (input$in_risk_type == "100 million hours"){
      local_df <- injury_risks_per_100million_h_lng
      ylab <- "Duration: risk per 100 million hours"
    }
    
    text_colour <- "black"
    
    local_df <- local_df |>
      as.data.frame() |>
      filter(city %in% filtered_cities &
               scenario %in% filtered_scens &
               mode %in% filtered_modes) |>
      mutate(scenario = case_when(
        scenario == "CYC_SC" ~ "Cycling",
        scenario == "CAR_SC" ~ "Car",
        scenario == "BUS_SC" ~ "Bus",
        scenario == "MOT_SC" ~ "Motorcycle",
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
    in_CIs <- "No" #input$in_CIs
    filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
    #filtered_cities <- in_cities
    filtered_scens <- input$in_scens
    filtered_pathways <- input$in_pathways
    in_per_100k <- input$in_per_100k
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
        summarise(metric_100k = round(ifelse(in_per_100k,(sum(measure) / pop * 100000), sum(measure)), 1)) 
      
      if (length(filtered_pathways) > 1){
        
        total_dose <- ld |>
          filter(str_detect(cause, "lb")) |>
          ungroup() %>%
          {if(in_strata == "Sex") group_by(., sex, city, scenario) 
            else if(in_strata == "Age Group") group_by(., age_cat, city, scenario) 
            else group_by(., city, scenario)} %>%
          summarise(metric_100k = sum(metric_100k)) |>
          mutate(dose = "Total", cause = "total_lb")
        
        total_dose <- rbind(total_dose,
                            ld |>
                              filter(str_detect(cause, "ub")) |>
                              ungroup() %>%
                              {if(in_strata == "Sex") group_by(., sex, city, scenario) 
                                else if(in_strata == "Age Group") group_by(., age_cat, city, scenario) 
                                else group_by(., city, scenario)} %>%
                              summarise(metric_100k = sum(metric_100k)) |>  
                              mutate(dose = "Total", cause = "total_ub")
                            
        )
        
        total_dose <- rbind(total_dose,
                            ld |>
                              filter(!str_detect(cause, "lb|ub")) |>
                              ungroup() %>%
                              {if(in_strata == "Sex") group_by(., sex, city, scenario) 
                                else if(in_strata == "Age Group") group_by(., age_cat, city, scenario) 
                                else group_by(., city, scenario)} %>%
                              summarise(metric_100k = sum(metric_100k)) |> 
                              mutate(dose = "Total", cause = "Total")
                            
        )
        ld <- plyr::rbind.fill(ld, total_dose)
      }
    }else{
      
      # browser()
      
      ld <- local_dataset |>
        filter(measures == in_measure) |>
        filter(!str_detect(cause, "lb|ub")) |>
        filter((!is.na(!!rlang::sym(in_col_lvl)))) |>
        filter(city %in% tolower(filtered_cities)) |>
        filter(scenario %in% filtered_scens) |>
        filter(dose %in% filtered_pathways) %>% 
        {if(in_strata == "Sex") group_by(., sex, city, scenario, dose) 
          else if(in_strata == "Age Group") group_by(., age_cat, city, scenario, dose) 
          else group_by(., city, scenario, dose)} %>% 
        {if(in_strata == "Sex") left_join(., (local_dataset |> distinct(city, sex, age_cat, .keep_all = T) |> group_by(city, sex) |> summarise(pop = sum(pop_age_sex)))) 
          else if(in_strata == "Age Group") left_join(., (local_dataset |> distinct(city, sex, age_cat, .keep_all = T) |> group_by(city, age_cat) |> summarise(pop = sum(pop_age_sex)))) 
          else left_join(., local_dataset |> distinct(city, sex, age_cat, .keep_all = T) |> group_by(city) |> summarise(pop = sum(pop_age_sex)))} |>  
          #else cbind(., (local_dataset |> distinct(sex, age_cat, .keep_all = T) |> summarise(pop = sum(pop_age_sex))))} |> 
        summarise(metric_100k = round(ifelse(in_per_100k,(sum(measure) / pop * 100000), sum(measure)), 2), pop = pop) |> 
        distinct(.keep_all = T) |> 
        left_join(cities |> mutate(city = tolower(city)))
      
      # browser()
      
      if (length(filtered_pathways) > 1){
        
        total_dose <- ld %>% 
          {if(in_strata == "Sex") group_by(., sex, city, scenario) 
            else if(in_strata == "Age Group") group_by(., age_cat, city, scenario) 
            else group_by(., city, scenario)} %>% 
          summarise(metric_100k = sum(metric_100k), dose = "Total")
        
        ld <- plyr::rbind.fill(ld, total_dose)
      }
      
      
    }
    
    ld <- ld |>
      mutate(scenario = case_when(
        scenario == "CYC_SC" ~ "Cycling",
        scenario == "CAR_SC" ~ "Car",
        scenario == "BUS_SC" ~ "Bus",
        scenario == "MOT_SC" ~ "Motorcycle"),
        measure = in_measure)
    
    if (is.null(ld) || nrow(ld) == 0)
      ld <- data.frame()
    
    if (!in_per_100k)
      names(ld)[names(ld) == "metric_100k"] <- "metric"
    
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
        if (input$in_risk_type == "Population by 100k people")
          measure <- "population-risk-per-100K"
        else if (input$in_risk_type == "100 million hours")
          measure <- "duration-risk-per-100M-hrs"
        paste(measure, "-", Sys.Date(), ".csv", sep="")
        
      } else if(input$main_tab == "PA Exposures"){
        paste("pa-exp-", Sys.Date(), ".csv", sep="")
      } else if(input$main_tab == "AP Exposures"){
        paste("ap-exp-", Sys.Date(), ".csv", sep="")
      }
      else{
        paste("output.csv")
      }
    },
    content = function(file) {
      
      data <- NA
      
      if(input$main_tab == "Health Outcomes"){
        data <- get_health_data()
      }else if (input$main_tab == "Injury Risks"){
        data <- get_inj_data()
      }else if(input$main_tab == "PA Exposures"){
        
        filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
        filtered_cities <- tolower(filtered_cities)
        filtered_scens <- input$in_scens
        
        data <- get_summary_data("mmets", filtered_cities, filtered_scens)
      }else if(input$main_tab == "AP Exposures"){
        
        filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
        filtered_cities <- tolower(filtered_cities)
        filtered_scens <- input$in_scens
        
        data <- get_summary_data("pm_conc_pm", filtered_cities, filtered_scens)
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
  
  
  output$plotScenariosPATable <- DT::renderDataTable({
    
    
    
  }
  ,server=F
  ,rownames = T
  ,options = list(
    scrollX = T,
    dom = 't',
    bSort = F,          # enable/disable the sorting feature for all columns
    bInfo = F,          # enable/disable the 'Showing 1 to 10 of 10 entries'
    bLengthChange = T,
    pageLength = 7,
    bFilter = T,        # enable/disable the up-right filter
    bSort = F,          # enable/disable the sorting feature for all columns
    bInfo = F          # enable/disable the 'Showing 1 to 10 of 10 entries'
  )
  
  )
  
  
  get_trip_tbl <- reactive({
    req(input$in_cities)
    req(input$in_trip_measure)
    
    filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull() |> tolower()
    tm <- input$in_trip_measure  
    
    df <- NULL
    
    if (tm == "Distance"){
      df <- get_city_df(filtered_cities, "dist") |>  
        gt(rowname_col = "row", groupname_col = "city_name") |> 
        data_color(columns = 2:6, method = "numeric", palette = "viridis") 
    }else if (tm == "Scenario"){
      df <- get_city_df(filtered_cities, "scen") |> 
        gt(rowname_col = "row", groupname_col = "city_name")|> 
        data_color(columns = 2:6, method = "numeric", palette = "viridis") 
      
    }else if (tm == "Trip"){
      df <- get_city_df(filtered_cities, "trip") |> 
        gt(rowname_col = "row", groupname_col = "city_name")|> 
        data_color(columns = 2:6, method = "numeric", palette = "viridis") 
    }
    
    
    #c("Scenario", "Trip", "Distance")
    
    
    # gt_tbl <-
    #   gtcars |>
    #   gt() |>
    #   fmt_currency(columns = msrp, decimals = 0) |>
    #   cols_hide(columns = -c(mfr, model, year, mpg_c, msrp)) |>
    #   cols_label_with(columns = everything(), fn = toupper) |>
    #   data_color(columns = msrp, method = "numeric", palette = "viridis") |>
    #   sub_missing() |>
    #   opt_interactive(use_compact_mode = TRUE)
    
    return(df)
    
    
  })
  
  get_city_df <- function(cities, obj){
    
    return (cities |>
              purrr::map(function(city) {
                
                if (obj == "dist"){
                  io[[city]][[obj]] |>
                    dplyr::mutate(city_name = city) |> 
                    mutate_if(is.numeric, list(~round((.) / nrow(io[[city]]$base_pop), 2)))
                }else if (obj == "scen"){
                  io[[city]]$trip_scen_sets |> 
                    filter(participant_id !=0) |> 
                    distinct(trip_id, scenario, .keep_all = T) |> 
                    group_by(scenario, trip_mode) |> 
                    reframe(freq = round(sum(dplyr::n())/ (io[[city]]$trip_scen_sets |> filter(scenario == "baseline") |> 
                                                             distinct(trip_id, scenario, .keep_all = T) |> nrow()) * 100, 1)) |> 
                    mutate(pd = freq - freq[scenario == 'baseline']) |> 
                    filter(pd != 0) |> 
                    dplyr::mutate(city_name = city) |> 
                    dplyr::select(-freq) |> 
                    rename(value = pd) |> 
                    pivot_wider(names_from = scenario)
                }else if (obj == "trip"){
                  
                  io[[city]]$trip_scen_sets %>% distinct(trip_id, scenario, .keep_all = T) %>% 
                    filter(!trip_mode %in% c("bus_driver", "taxi", "rail", "auto_rickshaw", "truck", "other", "car_driver")) |> 
                    group_by(trip_mode, scenario) %>% 
                    summarise(p = round(dplyr::n() / (io[[city]]$trip_scen_sets %>% dplyr::filter(scenario == "baseline") %>% 
                                                        summarise(uid = n_distinct(trip_id)) %>% as.numeric()) * 100, 1)) %>% 
                    spread(key = trip_mode, value = p) %>% 
                    mutate(row_sums = rowSums(.[sapply(., is.numeric)], na.rm = TRUE)) |> 
                    dplyr::mutate(city_name = city) |> 
                    dplyr::select(-row_sums)
                }
              }) |> list_rbind())
  }
  
  
  
  output$trip_table <- 
    render_gt( 
      { 
        get_trip_tbl() |> tab_header(title = paste("Measure: ", input$in_trip_measure))
      } 
    )  
  
  
  output$in_pa_exp_tbl <- render_gt({ 
    req(input$in_scens)
    req(input$in_cities)
    
    filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
    filtered_cities <- tolower(filtered_cities)
    filtered_scens <- input$in_scens
    
    df <- get_summary_data("mmets", filtered_cities, filtered_scens) |> dplyr::select(-outliers)
    
    df |> 
      mutate_if(is.numeric, round, 2) |> 
      gt(rowname_col = "row", groupname_col = "city_name") |> 
      data_color(columns = 3:8, method = "numeric", palette = "viridis") |> 
      tab_header(title = paste("PA exposures by city and scenario"))
    
  } 
  )  
  
  output$in_ap_exp_tbl <- render_gt({ 
    req(input$in_scens)
    req(input$in_cities)
    
    filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
    filtered_cities <- tolower(filtered_cities)
    filtered_scens <- input$in_scens
    
    df <- get_summary_data("pm_conc_pp", filtered_cities, filtered_scens) |> 
      dplyr::select(-outliers) |> 
      mutate_if(is.numeric, round, 2)
    
    df |> 
      gt(rowname_col = "row", groupname_col = "city_name") |> 
      data_color(columns = 3:8, method = "numeric", palette = "viridis") |> 
      tab_header(title = paste("AP exposures by city and scenario"))
  } 
  )  
  
  
  output$pa_exp <- renderUI({
    if (input$display_choice == "Figure") {
      plotlyOutput("in_pa_exp_fig")
    } else {
      tableOutput("in_pa_exp_tbl")
    }
  })
  
  output$ap_exp <- renderUI({
    if (input$display_choice == "Figure") {
      plotlyOutput("in_ap_exp_fig")
    } else {
      tableOutput("in_ap_exp_tbl")
    }
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
# run_with_themer(shinyApp(ui = ui, server = server))