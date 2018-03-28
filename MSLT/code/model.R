##########################Prepare data and environment to run code###########################################################

##### Clean the Global Environment

rm (list = ls())

##### Prevent scientific notation in data frame

options(scipen=999)

##### Load all functions/packages

source("code/functions.R")


##### Read data. idata: life table and disease life tables (including trends). edata: exposure data (e.g. physical activity).
#### irr.data: relative risks data and ee: energy expenditure
#### For exposure (edata) and input (idata) data I am using already generated categories, but a function should be developed to work from raw data. 

idata <- read.csv("data/idata.csv", stringsAsFactors = F)
edata <- read.csv("data/edata.csv", stringsAsFactors = F)
irr <- read.csv("data/irr.csv", stringsAsFactors = F)
ee <- read.csv("data/ee.csv", stringsAsFactors = F)

#####To do

## Note that disease trends are excluded for now. We need to generate a trends function for diseases, mortality (all cause), 
## change in disease due to change in risk factor exposure (discuss with Niel M), road injuries effect over time. 
## Eventually, we want to add (1) CRA ITHIM approach to compare results and (2) use all cause mortaltiy instead of 
## disease specific dose responses for incidence to understand the difference in results for all cause mortality. 

#################################Model parameters#########################################################################

p_age_cohort <- c(22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97)

p_sex <- c("males", "females")

p_disease <- c("ihd", "istroke", "diabetes", "colon_cancer", "breast_cancer")

###As an expample, an increase in 1000 MET-mins per week

p_intervention_effect <- 100

##############################Prepare general life table (age, death rate, population #)########

##### Make lower cases values for sex column

idata$sex <- tolower(idata$sex)

##### Rename mortality_rate to mx

idata$mx <- idata$mortality_rate

##### Remove mortality_rate column

idata$mortality_rate <- NULL

##### Rename male for males and female for females in edata to match idata. 

edata$sex[edata$sex=="male"] <- "males"
edata$sex[edata$sex=="female"] <- "females"

##### Create new variable for 5_year population (age cohorts). Depends on the age-cohorts of interest (5-yrs here)

idata$five_year_population <- NA
start_index <- 3
index <- 1
end_index <- 4
for (i in 1:20){
  if (i == 1)
    index <- 1
  else 
    index <- ((i - 1)  * 5) + 1
  if (index == 96){
    end_index <- 5
  }
  cat("population sum ", start_index, "start index ", index, " and end index ", index + end_index, "\n")
  idata$five_year_population[start_index] <- sum(idata$population[index:(index + end_index)])
  start_index <- start_index + 5
}

end_index <- 4
start_index <- 104

for (i in 1:20){
  if (i == 1)
    index <- 102
  else
    index <- (((i - 1)  * 5) + 1) + 101
  if (index == 197){
    end_index <- 5
  }
  cat("population sum ", start_index, "start index ", index, " and end index ", index + end_index, "\n")
  idata$five_year_population[start_index] <- sum(idata$population[index:(index + end_index)])
  start_index <- start_index + 5
}



##### Generate list of general life tables (baseline and scenario), disease life tables (baseline and sceanrio),
##### potential impact fraction (pif, one for now, number depends on interventions), and general and disease life
##### tables for interventions. 

##########################Prepare general life table###########################################

##### General lifetable. Starts from population numbers per one year interval and mortality rates
##### mortaltiy rates = deaths (1-yr/people 1-yr)

##### Create a new variable for the probability of dying between age now and now + 1
##### Formula = IF(age<100,1-EXP(-mortality rate),1)


# idata$qx <- ifelse(idata$age < 100, 1 - exp(-1 * idata$mx), 1)

########################Generate baseline (bl) general life tables##################################

#### p_age_cohort and p_sex are defined parameters at the start of the code. Here, we generate a general
#### life table per age and sex. 

general_life_table_list_bl <- list()
index <- 1

for (age in p_age_cohort){
  for (sex in p_sex){
    cat("age ", age, " and sex ", sex, "\n")
    general_life_table_list_bl[[index]] <- run_life_table(in_idata = idata, in_sex = sex, in_mid_age = age)
    index <- index + 1
  }
}

##### Uncommnet to check life table list
View(general_life_table_list_bl[[1]])

######################Generate baseline disease life tables##################################

##### p_age_cohort, p_sex and p_disease are defined parameters at the start of the code. Here, we generate a disease
##### life table per age, sex and disease. 

disease_life_table_list_bl <- list()
index <- 1

for (age in p_age_cohort){
  for (sex in p_sex){
    for (disease in p_disease) {
      # Exclude breast_cancer for Males
      if (sex == "males" && disease == "breast_cancer"){
        cat("\n")
      }
      else {
        cat("age ", age, " sex ", sex, "and disease", disease, "\n")
        disease_life_table_list_bl[[index]] <- run_disease(in_idata = idata, in_sex = sex, in_mid_age = age, in_disease = disease)
        index <- index + 1
      }
    }
  }
}

##### Uncommnet to check disease life table list
# View(disease_life_table_list_bl[[1]])

#######################Generate pifs#########################################################

##### p_age_cohort, p_sex, p_disease and p_intervention_effect are defined parameters at the start of the code. 
##### Here, we generate a pif per age, sex and disease.  

pifs <- list()
index <- 1

for (age in p_age_cohort){
  for (sex in p_sex){
    for (disease in p_disease) {
      for (effect in p_intervention_effect) {
        # Exclude breast_cancer for Males
        if (sex == "males" && disease == "breast_cancer"){
          cat("\n")
        }
        else {
          cat("age ", age, " sex ", sex, "disease", disease, "and effect", effect,  "\n")
          pifs[[index]] <- run_pif(in_idata = idata, i_irr = irr, i_exposure = edata, in_mid_age = age, in_sex = sex, in_disease = disease, in_met_sc = effect) 
          index <- index + 1
        }
      }
    }
  }
}

##### Uncommnet to check pifs
# View(pifs[[1]])

########################Scenario calculations####################################################

##### The mechanism of change start with incidence in the disease life tables. Incidence is modified with 
##### (1-PIF)


#####Generate scenario incidence (for each disease)

incidence_sc <- list()
index <- 1

for (age in p_age_cohort){
  for (sex in p_sex){
    for (disease in p_disease) {
      
      # Exclude breast_cancer for Males
      if (sex == "males" && disease == "breast_cancer"){
        cat("\n")
      }
      else {
        
        incidence_sc[[index]] <- disease_life_table_list_bl[[index]]$incidence_disease * (1-(pifs[[index]]$pif))
        index <- index + 1
        
      }
    }
  }
}

##### Uncommnet to check scenario incidence
# View(incidence_sc[[1]])


##### Calculate disease life tables with new incidence (this in turn, modifies prevalence and mortality)

disease_life_table_list_sc <- list()
index <- 1


for (age in p_age_cohort){
  for (sex in p_sex){
    for (disease in p_disease) {
      # Exclude breast_cancer for Males
      if (sex == "males" && disease == "breast_cancer"){
        cat("\n")
      }
      else {
        
        cat("age ", age, " sex ", sex, "and disease", disease, "\n")
        # modify idata's incidence for the said scenario
        td1 <- idata
        td1[td1$age >= age & td1$sex == sex,][[paste("incidence", disease, sep = "_")]] <- incidence_sc[[index]]
        
        # Instead of idata, feed td to run scenarios
        disease_life_table_list_sc[[index]] <- run_disease(in_idata = td1, in_sex = sex, in_mid_age = age, in_disease = disease)
        disease_life_table_list_sc[[index]]$diff_inc_disease <- disease_life_table_list_sc[[index]]$incidence_disease - disease_life_table_list_bl[[index]]$incidence_disease
        disease_life_table_list_sc[[index]]$diff_prev_disease <- disease_life_table_list_sc[[index]]$px - disease_life_table_list_bl[[index]]$px
        disease_life_table_list_sc[[index]]$diff_mort_disease <- disease_life_table_list_sc[[index]]$mx - disease_life_table_list_bl[[index]]$mx
        disease_life_table_list_sc[[index]]$diff_pylds_disease <- (disease_life_table_list_sc[[index]]$px - disease_life_table_list_bl[[index]]$px) * disease_life_table_list_bl[[index]]$dw_disease
        
        
        index <- index + 1
      }
    }
  }
}
##### Uncommnet to check scenario life tables
# View(disease_life_table_list_sc[[1]])


##########################################Calculate scenario general life table parameters###########################################

#####Generate total change in mortality rate
##### Sum mortality rate scenarios (mx_sc_total)


mx_sc_total <- list()
l_index <- 1
index <- 1
for (age in p_age_cohort){
  for (sex in p_sex){
    mortality_sum <- NULL
    create_new <- T
    
    for (disease in p_disease) {
      if (sex == "males" && disease == "breast_cancer"){
        cat("\n")
      }else{
        
        if (create_new){
          mortality_sum <- select(disease_life_table_list_sc[[index]], c('age', 'sex'))
          mortality_sum$total <- 0
          create_new <- F
          mortality_sum$total <- mortality_sum$total + (disease_life_table_list_sc[[index]]$diff_mort_disease)
        }else{
          mortality_sum$total <- mortality_sum$total + (disease_life_table_list_sc[[index]]$diff_mort_disease)
        }
        
        cat(age, " - ", sex," - ",  disease," - ",  index, " - ", l_index,  "\n")
        index <- index + 1
      }
      
    }
    mx_sc_total[[l_index]] <- mortality_sum 
    l_index <- l_index + 1
  }
}  

##### Uncommnet to check sceanrio mortality and changes 
# View(mx_sc_total[[1]])


#####Generate total change in prevalent yld rates
#####total ylds rate= sum (change prevalence disease*dw)


pylds_sc_total <- list()
l_index <- 1
index <- 1
for (age in p_age_cohort){
  for (sex in p_sex){
    pylds_sum <- NULL
    create_new <- T
    
    for (disease in p_disease) {
      if (sex == "males" && disease == "breast_cancer"){
        cat("\n")
      }else{
        
        if (create_new){
          pylds_sum <- select(disease_life_table_list_sc[[index]], c('age', 'sex'))
          pylds_sum$total <- 0
          create_new <- F
          pylds_sum$total <- pylds_sum$total + (disease_life_table_list_sc[[index]]$diff_pylds_disease)
        }else{
          pylds_sum$total <- pylds_sum$total + (disease_life_table_list_sc[[index]]$diff_pylds_disease)
        }
        
        cat(age, " - ", sex," - ",  disease," - ",  index, " - ", l_index,  "\n")
        index <- index + 1
      }
      
    }
    pylds_sc_total[[l_index]] <- pylds_sum
    l_index <- l_index + 1
  }
}  

##### Uncommnet to check scenario pyld change
# View(pylds_sc_total[[2]])

##### Calculate general life tables with modified mortality and pylds total#############################
###Original mortality rate is modified by the mx_sc_total (total change in mortality from diseases)
###Original pyld rate is modified by the change in each disease pylds


general_life_table_list_sc <- list()
index <- 1


for (age in p_age_cohort){
  for (sex in p_sex){
    
    
    
    
    cat("age ", age, " and sex ", sex, "\n")
    # modify idata's mortality and pyld total for the said scenario
    td2 <- idata
    td2[td2$age >= age & td2$sex == sex,][[paste("mx")]] <- general_life_table_list_bl[[index]]$mx + mx_sc_total[[index]]$total
    td2[td2$age >= age & td2$sex == sex,][[paste("pyld_rate")]] <- general_life_table_list_bl[[index]]$pyld_rate + pylds_sc_total[[index]]$total
    
    
    # Instead of idata, feed td to run scenarios
    general_life_table_list_sc[[index]] <- run_life_table(in_idata = td2, in_sex = sex, in_mid_age = age)
    
    
    
    index <- index + 1
    
    
  }
}
##### Uncommnet to check scenario life tables
View(general_life_table_list_sc[[32]])
View(general_life_table_list_bl[[32]])


############################################################Outputs########################################################################################

#####In the following list "output_life_table", 32 data frames are nested per age and sex cohort


output_burden <- list()
l_index <- 1
index <- 1
for (age in p_age_cohort){
  for (sex in p_sex){
    
    #Males do not have breast cancer, that is why we need the if/else. 
    #We create a TRUE/FALSE variable for the loop to move into the next disease
    
    create_new <- T
    for (disease in p_disease) {
      if (sex == "males" && disease == "breast_cancer"){
        cat("\n")
      }else{
        
        if (create_new){
          output_burden_sc <- select(disease_life_table_list_sc[[index]], c('age', 'sex', 'incidence_disease', 'mx', 'px'))
          names(output_burden_sc)[names(output_burden_sc) == 'incidence_disease'] <- paste('incidence_disease', disease, "sc", sep = "_")
          names(output_burden_sc)[names(output_burden_sc) == 'mx'] <- paste('mx', disease, "sc", sep = "_")
          names(output_burden_sc)[names(output_burden_sc) == 'px'] <- paste('px', disease, "sc", sep = "_")
          
          output_burden_bl <- select(disease_life_table_list_bl[[index]], c('incidence_disease', 'mx', 'px'))
          names(output_burden_bl)[names(output_burden_bl) == 'incidence_disease'] <- paste('incidence_disease', disease, "bl", sep = "_")
          names(output_burden_bl)[names(output_burden_bl) == 'mx'] <- paste('mx', disease, "bl", sep = "_")
          names(output_burden_bl)[names(output_burden_bl) == 'px'] <- paste('px', disease, "bl", sep = "_")
          
          ####New list to add calculations for changes in burden of disease (incidence and mortality numbers)
          
          output_burden_change <- list()
          
          output_burden_change$inc_num_bl <- disease_life_table_list_bl[[index]]$incidence_disease * (1 - disease_life_table_list_bl[[index]]$px) * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change$inc_num_sc <- disease_life_table_list_sc[[index]]$incidence_disease * (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change$inc_num_diff <- (disease_life_table_list_sc[[index]]$incidence_disease * (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx) - (disease_life_table_list_bl[[index]]$incidence_disease * (1 - disease_life_table_list_bl[[index]]$px) * general_life_table_list_bl[[l_index]]$Lx)
          
          output_burden_change$mx_num_bl <- disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change$mx_num_sc <- disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change$mx_num_diff <- (disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx) - (disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx) 
          
          names(output_burden_change)[names(output_burden_change) == 'inc_num_bl'] <- paste('inc_num_bl', disease, sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'inc_num_sc'] <- paste('inc_num_sc', disease, sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'inc_num_diff'] <- paste('inc_num_diff', disease, sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'mx_num_bl'] <- paste('mx_num_bl', disease, sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'mx_num_sc'] <- paste('mx_num_sc', disease, sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'mx_num_diff'] <- paste('mx_num_diff', disease, sep = "_")
          
          
          ###Bind all lists
          
          output_burden_sc <- cbind(output_burden_sc, output_burden_bl)
          output_burden_sc <- cbind(output_burden_sc, output_burden_change)
          
          create_new <- F
          
          #Here the calculations above are repeated, here is where the F is telling to move into the next disease
          
        }else{
          
          td3 <- select(disease_life_table_list_sc[[index]], c('incidence_disease', 'mx', 'px'))
          names(td3)[names(td3) == 'incidence_disease'] <- paste('incidence_disease', disease, "sc", sep = "_")
          names(td3)[names(td3) == 'mx'] <- paste('mx', disease, "sc", sep = "_")
          names(td3)[names(td3) == 'px'] <- paste('px', disease, "sc", sep = "_")
          
          td4 <- select(disease_life_table_list_bl[[index]], c('incidence_disease', 'mx', 'px'))
          names(td4)[names(td4) == 'incidence_disease'] <- paste('incidence_disease', disease, "bl", sep = "_")
          names(td4)[names(td4) == 'mx'] <- paste('mx', disease, "bl", sep = "_")
          names(td4)[names(td4) == 'px'] <- paste('px', disease, "bl", sep = "_")
          
          
          
          output_burden_change2 <- list()
          
          output_burden_change2$inc_num_bl <- disease_life_table_list_bl[[index]]$incidence_disease * (1 - disease_life_table_list_bl[[index]]$px) * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change2$inc_num_sc <- disease_life_table_list_sc[[index]]$incidence_disease * (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change2$inc_num_diff <- (disease_life_table_list_sc[[index]]$incidence_disease * (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx) - (disease_life_table_list_bl[[index]]$incidence_disease * (1 - disease_life_table_list_bl[[index]]$px) * general_life_table_list_bl[[l_index]]$Lx)
          
          output_burden_change2$mx_num_bl <- disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change2$mx_num_sc <- disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change2$mx_num_diff <- (disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx) - (disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx) 
          
          names(output_burden_change2)[names(output_burden_change2) == 'inc_num_bl'] <- paste('inc_num_bl', disease, sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'inc_num_sc'] <- paste('inc_num_sc', disease, sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'inc_num_diff'] <- paste('inc_num_diff', disease, sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'mx_num_bl'] <- paste('mx_num_bl', disease, sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'mx_num_sc'] <- paste('mx_num_sc', disease, sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'mx_num_diff'] <- paste('mx_num_diff', disease, sep = "_")
          
          
          ###Bind all lists
          
          output_burden_sc <- cbind(output_burden_sc, td3)
          output_burden_sc <- cbind(output_burden_sc, td4)
          output_burden_sc$age_cohort <- age 
          output_burden_sc <- cbind(output_burden_sc, output_burden_change2)
          
        }
        
        cat(age, " - ", sex," - ",  disease," - ",  index, " - ", l_index,  "\n")
        index <- index + 1
      }
      
    }
    
    # general_life_table_list_sc and general_life_table_list_bl (Lx)
    output_burden_lf_sc <- select(general_life_table_list_sc[[l_index]], c('Lx', 'Lwx'))
    names(output_burden_lf_sc)[names(output_burden_lf_sc) == 'Lx'] <- paste('Lx', "sc", sep = "_")
    names(output_burden_lf_sc)[names(output_burden_lf_sc) == 'Lwx'] <- paste('Lwx', "sc", sep = "_")
    
    output_burden_lf_bl <- select(general_life_table_list_bl[[l_index]], c('Lx', 'Lwx'))
    names(output_burden_lf_bl)[names(output_burden_lf_bl) == 'Lx'] <- paste('Lx', "bl", sep = "_")
    names(output_burden_lf_bl)[names(output_burden_lf_bl) == 'Lwx'] <- paste('Lwx', "bl", sep = "_")
    
    
    output_burden_lf_sc$Lx_diff <- general_life_table_list_bl[[l_index]]$Lx - general_life_table_list_sc[[l_index]]$Lx
    output_burden_lf_sc$Lwx_diff <- general_life_table_list_bl[[l_index]]$Lwx - general_life_table_list_sc[[l_index]]$Lwx
    
    output_burden_sc <- cbind(output_burden_sc, output_burden_lf_sc)
    output_burden_sc <- cbind(output_burden_sc, output_burden_lf_bl)
    
    
    output_burden[[l_index]] <- output_burden_sc
    l_index <- l_index + 1
    
  }
}

#Uncomment to check
View(output_burden[[32]])

############################################################Outputs########################################################################################

#####Generate a data frame for all results and create function to get outcomes. 


output_df <- plyr::ldply(output_burden, rbind)


#Remove variables that are not used in the generation of outputs. CHANGE THIS NAMES, TOO LONG

output_df <- subset(output_df, select = -c(incidence_disease_ihd_bl, incidence_disease_ihd_sc, 
                                           incidence_disease_istroke_bl, incidence_disease_istroke_sc, 
                                           incidence_disease_diabetes_bl, incidence_disease_diabetes_sc, 
                                           incidence_disease_breast_cancer_bl, incidence_disease_breast_cancer_sc, 
                                           incidence_disease_colon_cancer_bl, incidence_disease_colon_cancer_sc, 
                                           mx_ihd_bl, mx_ihd_sc, mx_istroke_bl, mx_istroke_sc, mx_diabetes_bl, mx_diabetes_sc, 
                                           mx_breast_cancer_bl, mx_breast_cancer_sc, mx_colon_cancer_bl, mx_colon_cancer_sc, 
                                           px_ihd_bl, px_ihd_sc, px_istroke_bl, px_istroke_sc, px_diabetes_bl, px_diabetes_sc, 
                                           px_breast_cancer_bl, px_breast_cancer_sc, px_colon_cancer_bl, px_colon_cancer_sc))

#############################################Plot outputs#####################################################

###### Generate outcomes graphs by age and sex and outcome of interest. 
######Here sepcify in_population with sex (males or females), in_age for the starting age of the cohort of interest
###### and in_outomes with age (to show evolution of cohort over time) and outcome of interest (bl is for baseline, sc for scenario 
###### and dif for the difference between the two). 

##### Second variable in in_outcomes should be the baseline, the second the scenario and the third the difference.
###In_legend to speficy the name of the disease plotted and outcome (incidence or mortality)


plot_eg <- plot_output(in_data = output_df, in_age = 22, in_population = "males", in_outcomes = c("age", "mx_num_bl_ihd", "mx_num_sc_ihd", "mx_num_diff_ihd"), in_legend = "Ischemic Heart Disease Deaths")


###########################################Outputs numbers####################################################
####Generate data frame with all outputs to graph total change in burden by simulation year. 
###first, need to run function for males and females separetly, in_cohorts indicates the number of age cohorts to 
####include. To show all select 16. what is specified in in_outcomes will be graphed or can also be presented as a total. 
ddaggregate_frame_males <- gen_aggregate(in_data = output_df, in_cohorts = 16, in_population = "males", in_outcomes = c('inc_num_bl_ihd','inc_num_sc_ihd', "inc_num_diff_ihd"))

aggregate_frame_females <- gen_aggregate(in_data = output_df, in_cohorts = 16, in_population = "females", in_outcomes = c('inc_num_bl_ihd','inc_num_sc_ihd', "inc_num_diff_ihd"))

#####The following adds up both males and females
# Remove non-numeric columns starting with age and sex
aggregate_frame_males <- aggregate_frame_males %>% select(-starts_with("age"), -starts_with("sex"))

aggregate_frame_females <- aggregate_frame_females %>% select(-starts_with("age"), -starts_with("sex"))

# Create a copy of aggregate_frame_females
total_aggr <- aggregate_frame_females
# Add aggregate_frame_males values to it
for (i in 1:ncol(aggregate_frame_females)){
  total_aggr[i] <- total_aggr[i] + aggregate_frame_males[i]
}
total_aggr$sim_year <- seq.int(nrow(total_aggr))

######################Plot total_aggr

####This plot has to be customised to in_outcomes, here, only totals shown, but specifications are up to the user

####[] is used here to indicate the number of simulation years into the future. 
####Disease outcomes has to be changed to the outcome of interest

disease_outcome <- "Deaths ischemic heart disease"

total_plot <- ggplot(total_aggr[1:20,], aes(x = sim_year)) +
  
  geom_line(mapping = aes(y = total_inc_num_bl_ihd, colour = "total_mx_num_bl_ihd")) +
  theme_classic() +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_line(mapping = aes(y = total_inc_num_sc_ihd, colour = "total_mx_num_sc_ihd")) +
  geom_line(mapping = aes(y = total_inc_num_diff_ihd, colour = "total_mx_num_diff_ihd")) +
  xlab ("Simulation years") + ylab ("Cases") + labs (title = paste(disease_outcome)) +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  scale_color_discrete(name = paste(""), labels = c("Baseline", "Scenario", "Difference"))
  

#Print to view
print(total_plot)
