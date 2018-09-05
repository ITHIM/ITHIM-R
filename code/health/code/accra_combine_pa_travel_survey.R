# set seed
set.seed(1)

# Read ind file with travel mmet
ind <- read_csv("data/synth_pop_data/accra/processed_data/indiv_mmet/ap_rr_pa_mmet_weekly.csv")

# Read PA data
pa <- read_csv("data/synth_pop_data/accra/pa/pa_dataset.csv")

pa$sex[pa$sex == "woman"] <- "Female"
pa$sex[pa$sex == "man"] <- "Male"


# Make age category for ind
age_category <- c("15-49", "50-70", ">70")
ind$age_cat[ind$age >= 15 & ind$age < 50] <- age_category[1]
ind$age_cat[ind$age >= 50 & ind$age <= 70] <- age_category[2]
ind$age_cat[ind$age > 70] <- age_category[3]

# Make age category for pa
age_category <- c("15-55", "56+")
pa$age_cat[pa$age >= 15 & pa$age <= 55] <- age_category[1]
pa$age_cat[pa$age > 55] <- age_category[2]


for (i in 1:nrow(ind)){
  
  rage <- ind$age_cat[i]
  rsex <- ind$sex[i]
  v <- 0
  if (rage == "15-49"){
    v <- filter(pa, age_cat == "15-55" & sex == rsex) %>% select(work_ltpa_marg_met) %>% sample_n(1) %>% as.double()
    
    
    #cat(i, ind$base_MET[i], v)
  }
  
  else {
    
    v <- filter(pa, age_cat == "56+" & sex == rsex) %>% select(work_ltpa_marg_met) %>% sample_n(1) %>% as.double()
    
  }
  
  v <- ifelse(v != 0, v/60, 0)
  ind$base_MET[i] <- ind$base_MET[i] + v
  ind$scen1_MET[i] <- ind$scen1_MET[i] + v
  ind$scen2_MET[i] <- ind$scen2_MET[i] + v
  ind$scen3_MET[i] <- ind$scen3_MET[i] + v
  
  #cat(i, ind$base_MET[i], v, "\n")
  # print (i)
  # print (v)
  
}

write_csv(ind, "data/synth_pop_data/accra/processed_data/indiv_mmet/ap_rr_pa_total_mmet_weekly.csv")


