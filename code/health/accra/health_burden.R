# Clear workspace and load libraries
#source("R/scenarios/accra/setup.R")

# Load all health related functions
source("R/PA/code/functions.R")

ind <- RR_PA_AP_calculations[[INDEX]]

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
    for (scen in c('base','scen1', 'scen2', 'scen3', 'scen4', 'scen5')){
      
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
      local_ylls <- as.data.frame(yll_dfs[1])
      # Subset to get yll_reductions
      local_ylls_red <- as.data.frame(yll_dfs[2])
      
      # Calculate deaths (total and red)
      death_dfs <- combine_health_and_pif(
        pop = pif,
        hc = gbd_data,
        hm = "Deaths",
        cn = c(base_var, scen_var),
        hm_cause <- gbd_dn,
        hm_cn <- 'value_gama')
      
      # Subset to get yll
      local_deaths <- as.data.frame(death_dfs[1])
      # Subset to get yll_reductions
      local_deaths_red <- as.data.frame(death_dfs[2])
      # Remove baseline vars
      #local_deaths <- select(local_deaths, -one_of(base_var))
      #local_deaths_red <- select(local_deaths_red, -one_of(base_var))
      #local_ylls <- select(local_ylls, -one_of(base_var))
      #local_ylls_red <- select(local_ylls_red, -one_of(base_var))
      
      if (disease_lt$physical_activity[j] == 1 & disease_lt$air_pollution[j] == 1){
        # Rename var names
        local_deaths <- rename(local_deaths, !! paste0(scen, '_deaths_pa_ap_',ac) := scen_var)
        local_deaths_red <- rename(local_deaths_red, !! paste0(scen, '_deaths_red_pa_ap_',ac) := scen_var)
        local_ylls <- rename(local_ylls, !! paste0(scen, '_ylls_pa_ap_',ac) := scen_var)
        local_ylls_red <- rename(local_ylls_red, !! paste0(scen, '_ylls_red_pa_ap_',ac) := scen_var)
        
      }else if (disease_lt$physical_activity[j] == 1 & disease_lt$air_pollution[j] != 1){
        # Rename var names
        local_deaths <- rename(local_deaths, !! paste0(scen, '_deaths_pa_',ac) := scen_var)
        local_deaths_red <- rename(local_deaths_red, !! paste0(scen, '_deaths_red_pa_',ac) := scen_var)
        local_ylls <- rename(local_ylls, !! paste0(scen, '_ylls_pa_',ac) := scen_var)
        local_ylls_red <- rename(local_ylls_red, !! paste0(scen, '_ylls_red_pa_',ac) := scen_var)
        
      }else if (disease_lt$physical_activity[j] != 1 & disease_lt$air_pollution[j] == 1){
        # Rename var names
        local_deaths <- rename(local_deaths, !! paste0(scen, '_deaths_ap_',ac) := scen_var)
        local_deaths_red <- rename(local_deaths_red, !! paste0(scen, '_deaths_red_ap_',ac) := scen_var)
        local_ylls <- rename(local_ylls, !! paste0(scen, '_ylls_ap_',ac) := scen_var)
        local_ylls_red <- rename(local_ylls_red, !! paste0(scen, '_ylls_red_ap_',ac) := scen_var)
        
      }
      
      local_deaths <- select(local_deaths, -contains("RR_"))
      local_deaths_red <- select(local_deaths_red, -contains("RR_"))
      local_ylls <- select(local_ylls, -contains("RR_"))
      local_ylls_red <- select(local_ylls_red, -contains("RR_"))
      
      #deaths[[base_var]] <- local_deaths_red[[base_var]] <- local_ylls[[base_var]] <- local_ylls_red[[base_var]] <- 0
      
      if (index == 1){
        
        # If global vars are not initiliazed, copy vars
        gdeaths <- local_deaths
        gdeaths_red <- local_deaths_red
        gylls <- local_ylls
        gylls_red <- local_ylls_red

      }else{
        # global vars are already initialized. Join new datasets with old ones.
        
        gdeaths <- left_join(gdeaths, local_deaths)
        gdeaths_red <- left_join(gdeaths_red, local_deaths_red)
        gylls <- left_join(gylls, local_ylls)
        gylls_red <- left_join(gylls_red, local_ylls_red)

      }
      
      # Increase index by 1
      index <- index + 1
      
    }
  }

}

gdeaths[,names(select(gdeaths, contains("scen1_")))] <- 0


# read injuries dataset for both ylls and deaths
inj <- deaths_yll_injuries[[INDEX]] # read_csv("R/injuries/accra/deaths_yll_injuries.csv")

# rename columns
inj <- rename(inj, age.band = age_cat, gender = sex)

# Select deaths columns
inj_deaths <- select(inj, c(age.band, gender, contains("deaths")))

# Select yll columns
inj_ylls <- select(inj, c(age.band, gender, contains("yll")))

# Join injuries data to global datasets
gdeaths <- left_join(gdeaths, inj_deaths)
gylls <- left_join(gylls, inj_ylls)


deaths[[INDEX]] <- gdeaths

deaths_red[[INDEX]] <- gdeaths_red

ylls[[INDEX]] <- gylls

ylls_red[[INDEX]] <- gylls_red

# Write ylls and deaths datasets
#write_csv(gdeaths, "data/scenarios/accra/health_burden/total_deaths.csv")
#write_csv(gdeaths_red, "data/scenarios/accra/health_burden/total_deaths_red.csv")
#write_csv(gylls, "data/scenarios/accra/health_burden/total_ylls.csv")
#write_csv(gylls_red, "data/scenarios/accra/health_burden/total_ylls_red.csv")