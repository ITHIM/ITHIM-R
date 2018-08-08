# Clear workspace and load libraries
source("R/scenarios/accra/setup.R")

# Load all health related functions
source("R/PA/code/functions.R")

ind <- read_csv("data/synth_pop_data/accra/RR/RR_PA_AP_calculations.csv")

# Redefine age_cat to match with GBD's
# Make age category
age_category <- c("15-49", "50-69", ">70")
ind$age_cat[ind$age >= 15 & ind$age < 50] <- age_category[1]
ind$age_cat[ind$age >= 50 & ind$age < 70] <- age_category[2]

# Read disease lt
disease_lt <- read_csv("data/dose_response/disease_outcomes_lookup.csv")

# Replace NAs with 0
disease_lt[is.na(disease_lt)] <- 0

# Initialize global variables
gdeaths <- NULL
gdeaths_red <- NULL
gylls <- NULL
gylls_red <- NULL

# Index for global datasets
index <- 1
### iterating over all all disease outcomes
for ( j in 1:nrow(disease_lt)){
  ## checking whether to calculate this health outcome for PA
  #if (disease_lt$physical_activity[j] == 1 & disease_lt$air_pollution[j] == 1)
  {
    # Disease acronym
    ac <- disease_lt$acronym[j] %>% as.character()
    # GBD's disease name
    gbd_dn <- disease_lt$GBD_name[j] %>% as.character()
    # Loop through all three scenarios
    for (scen in c('scen1', 'scen2', 'scen3', 'scen4', 'scen5')){
      
      print(ac)
      print(index)
      
      if (disease_lt$physical_activity[j] == 1 & disease_lt$air_pollution[j] == 1){
        # Initialize base and scenario var name
        #base_var <- paste0('RR_pa_ap_base_', ac)
        base_var <- paste0('RR_pa_ap_scen1_', ac)
        scen_var <- paste0('RR_pa_ap_', scen, '_', ac)
        
      }else if (disease_lt$physical_activity[j] == 1 & disease_lt$air_pollution[j] != 1){
        # Initialize base and scenario var name
        # base_var <- paste0('RR_pa_base_', ac)
        base_var <- paste0('RR_pa_scen1_', ac)
        scen_var <- paste0('RR_pa_', scen, '_', ac)
        
      }else if (disease_lt$physical_activity[j] != 1 & disease_lt$air_pollution[j] == 1){
        # Initialize base and scenario var name
        # base_var <- paste0('RR_ap_base_', ac)
        base_var <- paste0('RR_ap_scen1_', ac)
        scen_var <- paste0('RR_ap_', scen, '_', ac)
        
      }
      
      print(base_var)
      print(scen_var)
      
      print(' Columsn exists? ')
      print(base_var %in% names(ind))
      print(scen_var %in% names(ind))
      
      # browser()
      
      # Calculate PIFs for baseline and selected scenario
      pif <- data.frame(PAF(pop = ind, attr = c('sex', 'age_cat'), cn = c(base_var, scen_var)))
      pif <- arrange(pif, age.band, gender)
      
      # Redefine non-factor based column classes
      pif[,c("age.band", "gender")] <- lapply(pif[,c("age.band", "gender")], as.character)
      cols = c(3, 4)    
      pif[,cols] = apply(pif[,cols], 2, function(x) as.numeric(as.character(x)))
      
      # Read gbd data
      gbd_data <- read.csv("data/demographics/gbd/accra/GBD Accra.csv")
      
      # Calculate ylls (total and red)
      yll_dfs <- combine_health_and_pif(
        pop = pif,
        hc = gbd_data,
        hm = "YLLs (Years of Life Lost)",
        cn = c(base_var, scen_var),
        hm_cause <- gbd_dn,
        hm_cn <- 'value_gama')
      
      
      # Subset to get yll
      yll <- as.data.frame(yll_dfs[1])
      # Subset to get yll_reductions
      yll_red <- as.data.frame(yll_dfs[2])
      
      # Calculate deaths (total and red)
      death_dfs <- combine_health_and_pif(
        pop = pif,
        hc = gbd_data,
        hm = "Deaths",
        cn = c(base_var, scen_var),
        hm_cause <- gbd_dn,
        hm_cn <- 'value_gama')
      
      # Subset to get yll
      deaths <- as.data.frame(death_dfs[1])
      # Subset to get yll_reductions
      deaths_red <- as.data.frame(death_dfs[2])
      # Remove baseline vars
      #deaths <- select(deaths, -one_of(base_var))
      #deaths_red <- select(deaths_red, -one_of(base_var))
      #yll <- select(yll, -one_of(base_var))
      #yll_red <- select(yll_red, -one_of(base_var))
      
      if (disease_lt$physical_activity[j] == 1 & disease_lt$air_pollution[j] == 1){
        # Rename var names
        deaths <- rename(deaths, !! paste0(scen, '_deaths_pa_ap_',ac) := scen_var)
        deaths_red <- rename(deaths_red, !! paste0(scen, '_deaths_red_pa_ap_',ac) := scen_var)
        yll <- rename(yll, !! paste0(scen, '_ylls_pa_ap_',ac) := scen_var)
        yll_red <- rename(yll_red, !! paste0(scen, '_ylls_red_pa_ap_',ac) := scen_var)
        
      }else if (disease_lt$physical_activity[j] == 1 & disease_lt$air_pollution[j] != 1){
        # Rename var names
        deaths <- rename(deaths, !! paste0(scen, '_deaths_pa_',ac) := scen_var)
        deaths_red <- rename(deaths_red, !! paste0(scen, '_deaths_red_pa_',ac) := scen_var)
        yll <- rename(yll, !! paste0(scen, '_ylls_pa_',ac) := scen_var)
        yll_red <- rename(yll_red, !! paste0(scen, '_ylls_red_pa_',ac) := scen_var)
        
      }else if (disease_lt$physical_activity[j] != 1 & disease_lt$air_pollution[j] == 1){
        # Rename var names
        deaths <- rename(deaths, !! paste0(scen, '_deaths_ap_',ac) := scen_var)
        deaths_red <- rename(deaths_red, !! paste0(scen, '_deaths_red_ap_',ac) := scen_var)
        yll <- rename(yll, !! paste0(scen, '_ylls_ap_',ac) := scen_var)
        yll_red <- rename(yll_red, !! paste0(scen, '_ylls_red_ap_',ac) := scen_var)
        
      }
      
      deaths <- select(deaths, -contains("RR_"))
      deaths_red <- select(deaths_red, -contains("RR_"))
      yll <- select(yll, -contains("RR_"))
      yll_red <- select(yll_red, -contains("RR_"))
      
      #deaths[[base_var]] <- deaths_red[[base_var]] <- yll[[base_var]] <- yll_red[[base_var]] <- 0
      
      if (index == 1){
        
        # If global vars are not initiliazed, copy vars
        gdeaths <- deaths
        gdeaths_red <- deaths_red
        gylls <- yll
        gylls_red <- yll_red

      }else{
        # global vars are already initialized. Join new datasets with old ones.
        
        gdeaths <- left_join(gdeaths, deaths)
        gdeaths_red <- left_join(gdeaths_red, deaths_red)
        gylls <- left_join(gylls, yll)
        gylls_red <- left_join(gylls_red, yll_red)

      }
      
      # Increase index by 1
      index <- index + 1
      
    }
  }

}

gdeaths[,names(select(gdeaths, contains("scen1_")))] <- 0



# read injuries dataset for both ylls and deaths
inj <- read_csv("R/injuries/accra/deaths_yll_injuries.csv")

# rename columns
inj <- rename(inj, age.band = age_cat, gender = sex)

# Select deaths columns
inj_deaths <- select(inj, c(age.band, gender, contains("deaths")))

# Select yll columns
inj_ylls <- select(inj, c(age.band, gender, contains("yll")))

# Join injuries data to global datasets
gdeaths <- left_join(gdeaths, inj_deaths)
gylls <- left_join(gylls, inj_ylls)

# Write ylls and deaths datasets
write_csv(gdeaths, "data/scenarios/accra/health_burden/total_deaths.csv")
write_csv(gdeaths_red, "data/scenarios/accra/health_burden/total_deaths_red.csv")
write_csv(gylls, "data/scenarios/accra/health_burden/total_ylls.csv")
write_csv(gylls_red, "data/scenarios/accra/health_burden/total_ylls_red.csv")