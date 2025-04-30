require(tidyverse)

if (!exists("output_version")){
  ## Get the current repo sha
  gitArgs <- c("rev-parse", "--short", "HEAD", ">", file.path("repo_sha"))
  # Use shell command for Windows as it's failing with system2 for Windows (giving status 128)
  if (.Platform$OS.type == "windows"){
    shell(paste(append("git", gitArgs), collapse = " "), wait = T)
  } else {
    system2("git", gitArgs, wait = T)
  }
  
  repo_sha <-  as.character(readLines(file.path("repo_sha")))
  output_version <- repo_sha
}

io <- readRDS(paste0("results/multi_city/io_", output_version, ".rds"))

get_qualified_scen_name <- function(cs){
  qualified_scen_name <- ""
  if (cs == 'base' | cs == 'baseline' | cs == 'Baseline')
    qualified_scen_name <- 'Baseline'
  else if(cs == "sc_walk")
    qualified_scen_name <- 'Walking'
  else if(cs == "sc_cycle")
    qualified_scen_name <- 'Cycling'
  else if(cs == "sc_car")
    qualified_scen_name <- 'Car'
  else if(cs == "sc_motorcycle")
    qualified_scen_name <- 'Motorcycling'
  else if(cs == "sc_bus")
    qualified_scen_name <- 'Bus'
  
  return(qualified_scen_name)
}


# initialise variables
health <- list()
measure <- c("ylls", "deaths")
pathway <- c("hb", "pathway_hb")

for (k in measure) { # loop for ylls or deaths
  for (j in pathway) { # loop for hb or pathway_hb
    for (i in names(io)[!names(io) %in% c('scen_prop','ithim_run' )]) { # loop for cities
      
      # Temporal dataset for health outcomes
      temp <- io[[i]]$outcomes[[j]][[k]]
      
      # Total population
      #overall_pop <- sum(io[[i]]$demographic$population)
      
      # Population by age and sex
      pop_by_age_sex <- io[[i]]$demographic %>% group_by(age, sex) %>%
        summarize(pop_age_sex = sum(population))
      
      # get the health burden into the correct format by creating a long format and adding additional columns
      # create long format
      health[[k]][[j]][[i]] <- gather(temp, key = "cause", "measure",
                                      -sex, -age_cat) %>%
        mutate(city = i, # create city column
               scenario = case_when( # create scenario column
                 grepl("sc_cycle", cause) ~ get_qualified_scen_name('sc_cycle'),
                 grepl("sc_car", cause) ~ get_qualified_scen_name('sc_car'),
                 grepl("sc_bus", cause) ~ get_qualified_scen_name('sc_bus'),
                 grepl("sc_motorcycle", cause) ~ get_qualified_scen_name('sc_motorcycle')
               ),
               level1 = case_when( # create level 1 column
                 grepl("all_cause", cause) ~ "L1: All Cause",
                 grepl("inj", cause) ~ "L1: RTI"
               ),
               level2 = case_when( # create level 2 column
                 grepl("total_cancer", cause) ~ "L2: Cancer",
                 grepl("CVD", cause) ~ "L2: CVD",
                 grepl("respiratory", cause) ~ "L2: Respiratory",
                 grepl("T2D", cause) ~ "L2: Other",
                 grepl("total_dementia", cause) ~ "L2: Other",
                 grepl("Parkinson", cause) ~ "L2: Other",
                 grepl("inj", cause) ~ "L2: RTI"
               ),
               level3 = case_when( # create level 3 column
                 grepl("IHD", cause) ~ "L3: IHD",
                 grepl("lung_cancer", cause) ~ "L3: lung_cancer",
                 grepl("COPD", cause) ~ "L3: COPD",
                 grepl("stroke", cause) ~ "L3: stroke",
                 grepl("T2D", cause) ~ "L3: T2D",
                 grepl("LRI", cause) ~ "L3: LRI",
                 grepl("breast_cancer", cause) ~ "L3: breast_cancer",
                 grepl("colon_cancer", cause) ~ "L3: colon_cancer",
                 grepl("endo_cancer", cause) ~ "L3: endo_cancer",
                 grepl("liver_cancer", cause) ~ "L3: liver_cancer",
                 grepl("total_dementia", cause) ~ "L3: total_dementia",
                 grepl("myeloma", cause) ~ "L3: myeloma",
                 grepl("myeloid_leukemia", cause) ~ "L3: Myeloid Leukemia",
                 grepl("Parkinson", cause) ~ "L3: Parkinson",
                 grepl("head_neck_cancer", cause) ~ "L3: head_neck_cancer",
                 grepl("stomach_cancer", cause) ~ "L3: stomach_cancer",
                 grepl("inj", cause) ~ "L3: RTI"
               ),
               dose = case_when( # create dose column
                 grepl("_pa_ap_", cause) ~ "PA and AP",
                 grepl("_pa_", cause) ~ "PA",
                 grepl("_ap_", cause) ~ "AP",
                 grepl("_inj", cause) ~ "RTI"
               )
        ) %>% # End mutate
        # join with demographic information
        left_join(pop_by_age_sex, by = c("age_cat" = "age", "sex" = "sex"))
      
      # %>%
      # create new columns showing burden per 100000 people by age or by age and sex
      # mutate(metric_100k = measure / overall_pop * 100000,
      #        metric_100k_sex = measure / overall_pop * 100000)
      
    } # Loop for each city
  } # Loop for each pathway
} # Loop for each measure


# for each health burden and pathway join all cities into one dataframe
ylls <-  rbindlist(health$ylls$hb, use.names = T) #|> filter(!str_detect(cause, "_lb") & !str_detect(cause, "_ub"))
ylls_pathway <- rbindlist(health$ylls$pathway_hb, use.names = T) #|> filter(!str_detect(cause, "_lb") & !str_detect(cause, "_ub"))
deaths <- rbindlist(health$deaths$hb, use.names = T) #|> filter(!str_detect(cause, "_lb") & !str_detect(cause, "_ub"))
deaths_pathway <- rbindlist(health$deaths$pathway_hb, use.names = T) #|> filter(!str_detect(cause, "_lb") & !str_detect(cause, "_ub"))

# write csv without output version number
write.csv(ylls,
          paste0('results/multi_city/health_impacts/ylls.csv'),
          row.names = F)
# Export ylls PATHWAY_HB
write.csv(ylls_pathway,
          paste0('results/multi_city/health_impacts/ylls_pathway.csv'),
          row.names = F)
# Export deaths HB
write.csv(deaths,
          paste0('results/multi_city/health_impacts/deaths.csv'),
          row.names = F)
# Export deaths PATHWAY_HB
write.csv(deaths_pathway,
          paste0('results/multi_city/health_impacts/deaths_pathway.csv'),
          row.names = F)
