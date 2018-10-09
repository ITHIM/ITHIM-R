setwd("code/MSLT")
# Change to own wd

# ---- chunk-intro ----
require(dplyr)
require(tidyverse)
require(knitr)
require(kableExtra)
require(citr)
require(gridExtra)
require(cowplot)
require(ggpubr)
require(grid)
require(ggplot2)

# ---- chunk-1 ----
rm (list = ls())

# ---- chunk-2 ----
options(scipen=999)

# ---- chunk-3 ----
source("code/functions.R")



# ------------------- Population Numbers ---------------------------#


gbd <- readxl::read_excel("data/england/gbd2016.xlsx")

disease_short_names <- data.frame(disease = c("All causes", 
                                              "Diabetes mellitus",
                                              "Tracheal, bronchus, and lung cancer",
                                              "Breast cancer", 
                                              "Colon and rectum cancer",
                                              "Ischemic heart disease",
                                              "Ischemic stroke"),
                                  sname = c("ac", "dm", "tblc", "bc",
                                            "cc", "ihd", "is"))

disease_short_names$disease <- as.character(disease_short_names$disease)
disease_short_names$sname <- as.character(disease_short_names$sname)

disease_measures <- list("Prevalence", "Incidence", "Deaths", "YLDs (Years Lived with Disability)")

gbd_df <- NULL

for (ag in 1:length(unique(gbd$age))){
  for (gender in c("Male", "Female")){
    age_sex_df <- NULL
    for (dm in 1:length(disease_measures)){
      for (d in 1:nrow(disease_short_names)){
        dn <- disease_short_names$disease[d]
        dmeasure <- disease_measures[dm] %>% as.character()
        # gender <- "Male"
        agroup <- unique(gbd$age)[ag]
        
        idf <- filter(gbd, sex == gender & age == agroup & measure == dmeasure & cause == dn)
        
        if (nrow(idf) > 0){
          
          population_numbers <- filter(idf, metric == "Number") %>% select("val")
          
          idf_rate <- filter(idf, metric == "Rate") %>% select("val")
          
          idf$population_number <- (100000 * population_numbers$val) / idf_rate$val
          
          idf$rate_per_1 <- round(idf_rate$val / 100000, 6)
          
          idf[[tolower(paste(dmeasure, "rate", disease_short_names$sname[d], sep = "_"))]] <- idf$rate_per_1
          
          idf[[tolower(paste(dmeasure, "number", disease_short_names$sname[d], sep = "_"))]] <- population_numbers$val
          
          idf$rate_per_1 <- NULL
          
          idf <- filter(idf, metric == "Number")
          
          if (is.null(age_sex_df)){
            #browser()
            # print("if")
            # print(names(idf)[ncol(idf) - 1])
            # print(names(idf)[ncol(idf)])
            age_sex_df <- select(idf, age, sex, population_number, location, names(idf)[ncol(idf) - 1] , names(idf)[ncol(idf)])
          }
          else{
            #browser()
            # print("else")
            # print(names(idf)[ncol(idf) - 1])
            # print(names(idf)[ncol(idf)])
            
            age_sex_df <- cbind(age_sex_df, select(idf, names(idf)[ncol(idf) - 1] , names(idf)[ncol(idf)]))
          }
          
        }
        
        #age_range <- years %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric
        
      }
    }
    
    # browser()
    
    if (is.null(gbd_df)){
      # browser()
      gbd_df <- age_sex_df
    }
    else{
      # browser()
      age_sex_df[setdiff(names(gbd_df), names(age_sex_df))] <- 0
      gbd_df[setdiff(names(age_sex_df), names(gbd_df))] <- 0
      gbd_df <- rbind(gbd_df, age_sex_df)
    }
  }
  
}


write_csv(gbd_df, "data/england/dismod/input_data.csv")

gbd_df[["ac_ylds_rate_1"]] <- gbd_df$`ylds (years lived with disability)_number_ac` / gbd_df$population_number

all_ylds_df <- dplyr::select(gbd_df, starts_with("ylds (years lived with disability)_number"))

gbd_df[["ac_ylds_adj_rate_1"]] <- (gbd_df$`ylds (years lived with disability)_number_ac`  - rowSums(select(all_ylds_df, -`ylds (years lived with disability)_number_ac`))) / 
                                     gbd_df$population_number


# ---- chunk-4 ----

data.input <- read.csv("data/legacy/UK/idata.csv", stringsAsFactors = F)

### Population for England: Look up in Table 2 with data for Local Goverment areas.

#population <-  read.xls("data/legacy/UK/Population & Mortality/table2(5-year, +90).xls", stringsAsFactors = F)

#mortality, here some look up table, change location of data to make it easier

tylds <- read.csv("data/legacy/UK/totalYLDs.csv", stringsAsFactors = F)

# ---- chunk-5 ----

data.input <- mutate_all(disease, funs(tolower))

GBDdata <- GBDdata %>% mutate(measure = ifelse(measure == "ylds (years lived with disability)", "ylds", measure))

# ---- chunk-6 ----
# 22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97
GBDdata$age_cat [GBDdata$age =="under 5"] <- 2
GBDdata$age_cat [GBDdata$age =="5 to 9"] <- 7
GBDdata$age_cat [GBDdata$age =="10 to 14"] <- 12
GBDdata$age_cat [GBDdata$age =="15 to 19"] <- 17
GBDdata$age_cat [GBDdata$age =="20 to 24"] <- 22
GBDdata$age_cat [GBDdata$age =="25 to 29"] <- 27
GBDdata$age_cat [GBDdata$age =="30 to 34"] <- 32
GBDdata$age_cat [GBDdata$age =="35 to 39"] <- 37
GBDdata$age_cat [GBDdata$age =="40 to 44"] <- 42
GBDdata$age_cat [GBDdata$age =="45 to 49"] <- 47
GBDdata$age_cat [GBDdata$age =="50 to 54"] <- 52
GBDdata$age_cat [GBDdata$age =="55 to 59"] <- 57
GBDdata$age_cat [GBDdata$age =="60 to 64"] <- 62
GBDdata$age_cat [GBDdata$age =="65 to 69"] <- 67
GBDdata$age_cat [GBDdata$age =="70 to 74"] <- 72
GBDdata$age_cat [GBDdata$age =="75 to 79"] <- 77
GBDdata$age_cat [GBDdata$age =="80 to 84"] <- 82
GBDdata$age_cat [GBDdata$age =="85 to 89"] <- 87
GBDdata$age_cat [GBDdata$age =="90 to 94"] <- 92
GBDdata$age_cat [GBDdata$age =="95 plus"] <- 97


# ---- chunk-7 ----
GBDdata$sex_age_cat <- paste(GBDdata$sex,GBDdata$age_cat, sep = "_"  )



# ---- chunk-8 ----

GBDdata$val <- as.numeric(as.character(GBDdata$val))


# ---- chunk-9 ----
GBD_population <- filter(GBDdata, measure == "deaths", cause == "all causes",
                         metric == "rate" | metric == "number" ) %>% select(metric, age_cat, sex, val,
                                                                            sex_age_cat, location)
# ---- chunk-10 ----

for (i in 1:nrow(GBD_population)) {
  if (GBD_population$metric[i] == "number") 
    GBD_population$five_year_population[i] <- GBD_population$val[i] * 100000/ GBD_population$val[i + 2]
  else
    GBD_population$five_year_population[i] <- NA
}

# ---- chunk-11 ----

GBD_population <- GBD_population[!is.na(GBD_population$five_year_population),]


# ---- chunk-12 ----

GBD_population <- filter(GBD_population) %>%
  select(sex_age_cat, sex, age_cat, five_year_population, location)


# ---- chunk-13 ----

GBD_population_GL <-  filter(GBD_population, location == "greater london") %>%
  select(sex_age_cat, age_cat, sex, five_year_population, location, sex_age_cat)

# ---- chunk-14 ----

GreaterLondon <- sum(GBD_population_GL$five_year_population)



# ---- chunk-15 ----

GBDGL <- filter(GBDdata, location == "greater london" & metric == "rate") %>%
  select(measure, location, sex, age, metric, cause, val, age_cat, sex_age_cat)
GBDGL$one_rate <- GBDGL$val/100000



# ---- chunk-16 ----

## Loop to generate interpolated rates for all cause mortality and ylds for males and females
## UPDATE WITH YLDS RATES ALL CAUSES ADJUSTED FOR ALL OTHER MODELLED DISEASES.

i_sex <- c("male", "female")
i_measure <- c("deaths", "ylds") #" (years lived with disability)")

index <- 1

for(sex_index in i_sex) {
  for (measure_index in i_measure) {
    
    data <- filter(GBDGL, measure == measure_index, sex == sex_index,
                   cause == "all causes") %>% select(measure, location, sex, age, metric,
                                                     cause, val, age_cat, one_rate)
    assign(paste(sex_index, measure_index, "interpolated_data", sep = "_"), data)
    x <- data$age_cat
    y <- log(data$one_rate)
    
    
    interpolation_func <- stats::splinefun(x, y, method = "natural", ties = mean)
    
    interpolated <- as.data.frame(interpolation_func(seq(0, 100, 1)))
    age <- seq(0, 100, by = 1)
    interpolated <- cbind(interpolated, age)
    interpolated[,1] <- exp(interpolated[,1])
    colnames(interpolated)[1] <- paste(measure_index)
    ## Add column with sex to create age_sex category to then merge with input_life table
    interpolated$sex <- paste(sex_index)
    interpolated$sex_age_cat <- paste(interpolated$sex, interpolated$age, sep = "_")
    ## Change name of column death to mx and ylds to pyld_rate to then merge
    ## with input_life table
    
    if (colnames(interpolated)[1] == "deaths")
      colnames(interpolated)[1] <- paste("mx")
    else
      colnames(interpolated)[1] <- paste("pyld_rate")
    
    # Name data frame
    assign(paste(sex_index, measure_index, "interpolated", sep = "_"), interpolated)
    
  }
}


## Do graph with another layer for original rates in age groups for comparison purposes.

p_interpolation_list <- list()

interpolation_index <- 1

for(sex_index in i_sex) {
  for (measure_index in i_measure) {
    
    
    p_interpolation_index <- ggplot(data = interpolated,mapping = aes(age, interpolated[,1])) +
      geom_line(aes(color = "Interpolated")) +
      geom_point(
        data = data,
        mapping = aes(age_cat, one_rate, color = "Original")) +
      labs(colour="",x="Age",y="Rates", sep = " ") +
      labs (title = paste("Rates", sex_index, "all cause",
                          measure_index, sep = " "), size=14) +
      theme_classic() +
      theme (plot.title = element_text(hjust = 0.5))
    
    p_interpolation_list[[interpolation_index]] <- p_interpolation_index
    interpolation_index <- interpolation_index + 1
    
  }
}

## Save plots to jpeg. Makes a separate file for each plot.

interpolation_index <- 1
for(sex_index in i_sex) {
  for (measure_index in i_measure) {
    file_name = paste0("output/graphs1/", paste("interpolation_rates", sex_index, measure_index, sep="_"), ".jpeg" )
    jpeg(file_name)
    print(p_interpolation_list[[interpolation_index]])
    interpolation_index <- interpolation_index + 1
    dev.off()
  }
}


p_interpolated <- do.call(marrangeGrob, list(grobs=p_interpolation_list, nrow = 2, ncol = 2))
p_interpolated



# ---- chunk-17 ----

## Create empty data frame

input_life_table <- as.data.frame(matrix(0, ncol=2, nrow = 202))

## Give variables names (mx=total mortality rate,
## yldsx = total years lived with disability rates )

names(input_life_table) <- c("age", "sex")

## Populate life table: Age, 0 to 100

input_life_table[1:101, 1] <- c(0:100)
input_life_table[102:202, 1] <- c(0:100)

## Populate life table: sex, male, female

input_life_table[1:101, 2] <- "male"
input_life_table[102:202, 2] <- "female"

## Create variable age_sex to match with population data

input_life_table$sex_age_cat <- paste(input_life_table$sex,input_life_table$age,
                                      sep = "_"  )
## Populate life table: five_year_pop.

input_life_table <- merge(input_life_table, select(GBD_population_GL,
                                                   c(sex_age_cat, five_year_population)), by = 'sex_age_cat', all = TRUE)

## Populate life table: mortality rates (from interpolated rates)
## Males mortality rates. NEED MATCHING NAMES

input_life_table <- merge(input_life_table, select(male_deaths_interpolated,
                                                   c(sex_age_cat, mx)), by = 'sex_age_cat', all.x = TRUE)
input_life_table <- merge(input_life_table, select(female_deaths_interpolated,
                                                   c(sex_age_cat, mx)), by = 'sex_age_cat', all.x = TRUE)
input_life_table <- merge(input_life_table, select(male_ylds_interpolated,
                                                   c(sex_age_cat, pyld_rate)) , by = 'sex_age_cat', all.x = TRUE)
input_life_table <- merge(input_life_table, select(female_ylds_interpolated,
                                                   c(sex_age_cat, pyld_rate)), by = 'sex_age_cat', all.x = TRUE)

## DISCUSS WITH ALI TO IMPROVE

input_life_table$mx <-  ifelse(input_life_table$sex == "male",
                               input_life_table$mx.x, input_life_table$mx.y)

input_life_table$pyld_rate <-  ifelse(input_life_table$sex == "male",
                                      input_life_table$pyld_rate.x, input_life_table$pyld_rate.y)
## Drop redundant variables
input_life_table <- subset(input_life_table, select = -c(mx.y, mx.x, pyld_rate.x, pyld_rate.y))

## Cross check population numbers

sum(input_life_table$five_year_population, na.rm = TRUE)

## Sort data by sex and age to use in life table function

input_life_table<-input_life_table[order(input_life_table$sex, input_life_table$age),]



# ---- chunk-18 ----

i_age_cohort <- c(22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97)

general_life_table_list_bl <- list()

index <- 1

for (age in i_age_cohort){
  for (sex in i_sex){
    # cat("age ", age, " and sex ", sex, "\n") #Uncomment to see index
    general_life_table_list_bl[[index]] <- run_life_table(in_idata = input_life_table,
                                                          in_sex = sex, in_mid_age = age)
    index <- index + 1
  }
}

## Uncommnet to check life table list
# View(general_life_table_list_bl[[32]])



# ---- chunk-19 ----
## Use externaly generated inputs for for disease life table. The previous code will serve to to some of the data preparation for DISMOD. Also need to devlop code to calculated DWs, for now, all generated in excel.

idata <- read.csv("data/legacy/UK/idata.csv", stringsAsFactors = F)
## Use run_disease

i_disease <- c("ihd", "istroke", "diabetes", "colon_cancer", "breast_cancer")


disease_life_table_list_bl <- list()
index <- 1

for (age in i_age_cohort){
  for (sex in i_sex){
    for (disease in i_disease) {
      # Exclude breast_cancer for Males
      if (sex == "male" && disease == "breast_cancer"){
        # cat("\n") #Uncomment to see list
      }
      else {
        # cat("age ", age, " sex ", sex, "and disease", disease, "\n") #Uncomment to see list
        disease_life_table_list_bl[[index]] <- run_disease(in_idata = idata, in_sex = sex, in_mid_age = age, in_disease = disease)
        index <- index + 1
      }
    }
  }
}
## Uncommnet to check disease life table list
# View(disease_life_table_list_bl[[8]])



# ---- chunk-20 ----

## Create value to use as factor changing incidence rates.

incidence_change <- 0.95

## Generate scenario incidence (for each disease)

incidence_sc <- list()
index <- 1

for (age in i_age_cohort){
  for (sex in i_sex){
    for (disease in i_disease) {
      
      # Exclude breast_cancer for Males
      if (sex == "male" && disease == "breast_cancer"){
        # cat("\n") # Uncomment to see list
      }
      else {
        
        incidence_sc[[index]] <- disease_life_table_list_bl[[index]]$incidence_disease *
          incidence_change
        index <- index + 1
        
      }
    }
  }
}

## Uncommnet to check scenario incidence
# View(incidence_sc[[1]])




# ---- chunk-21 ----

disease_life_table_list_sc <- list()
index <- 1
for (age in i_age_cohort){
  for (sex in i_sex){
    for (disease in i_disease) {
      # Exclude breast_cancer for Males
      if (sex == "male" && disease == "breast_cancer"){
        # cat("\n")
      }
      else {
        # cat("age ", age, " sex ", sex, "and disease", disease, "\n")
        # modify idata's incidence for the said scenario
        td1 <- idata
        td1[td1$age >= age & td1$sex == sex,][[paste("incidence", disease, sep = "_")]] <- incidence_sc[[index]]
        
        # Instead of idata, feed td to run scenarios
        disease_life_table_list_sc[[index]] <- run_disease(in_idata = td1, in_sex = sex,
                                                           in_mid_age = age, in_disease = disease)
        disease_life_table_list_sc[[index]]$diff_inc_disease <-
          disease_life_table_list_sc[[index]]$incidence_disease -   disease_life_table_list_bl[[index]]$incidence_disease
        disease_life_table_list_sc[[index]]$diff_prev_disease <-
          disease_life_table_list_sc[[index]]$px  - disease_life_table_list_bl[[index]]$px
        disease_life_table_list_sc[[index]]$diff_mort_disease <-
          disease_life_table_list_sc[[index]]$mx - disease_life_table_list_bl[[index]]$mx
        disease_life_table_list_sc[[index]]$diff_pylds_disease <-
          (disease_life_table_list_sc[[index]]$px - disease_life_table_list_bl[[index]]$px) * disease_life_table_list_bl[[index]]$dw_disease
        index <- index + 1
      }
    }
  }
}
## Uncommnet to check scenario life tables
# View(disease_life_table_list_sc[[1]])






# ---- chunk-22 ----

## Generate total change in mortality rate

## Sum mortality rate change scenarios (mx_sc_total)

mx_sc_total <- list()
l_index <- 1
index <- 1
for (age in i_age_cohort){
  for (sex in i_sex){
    mortality_sum <- NULL
    create_new <- T
    
    for (disease in i_disease) {
      if (sex == "male" && disease == "breast_cancer"){
        # cat("\n")
      }else{
        
        if (create_new){
          mortality_sum <- select(disease_life_table_list_sc[[index]],
                                  c('age', 'sex'))
          mortality_sum$total <- 0
          create_new <- F
          mortality_sum$total <- mortality_sum$total +
            (disease_life_table_list_sc[[index]]$diff_mort_disease)
        }else{
          mortality_sum$total <- mortality_sum$total +
            (disease_life_table_list_sc[[index]]$diff_mort_disease)
        }
        
        # cat(age, " - ", sex," - ",  disease," - ",  index, " - ", l_index,  "\n")
        index <- index + 1
      }
    }
    mx_sc_total[[l_index]] <- mortality_sum
    
    l_index <- l_index + 1
  }
}

## Uncommnet to check sceanrio mortality and changes
# View(mx_sc_total[[3]])

## Generate total change in prevalent yld rates
## Total ylds rate= sum (change prevalence disease*dw)

pylds_sc_total <- list()
l_index <- 1
index <- 1
for (age in i_age_cohort){
  for (sex in i_sex){
    pylds_sum <- NULL
    create_new <- T
    
    for (disease in i_disease) {
      if (sex == "male" && disease == "breast_cancer"){
        # cat("\n")
      }else{
        
        if (create_new){
          pylds_sum <- select(disease_life_table_list_sc[[index]], c('age', 'sex'))
          pylds_sum$total <- 0
          create_new <- F
          pylds_sum$total <- pylds_sum$total +
            (disease_life_table_list_sc[[index]]$diff_pylds_disease)
        }else{
          pylds_sum$total <- pylds_sum$total +
            (disease_life_table_list_sc[[index]]$diff_pylds_disease)
        }
        
        # cat(age, " - ", sex," - ",  disease," - ",  index, " - ", l_index,  "\n")
        index <- index + 1
      }
      
    }
    pylds_sc_total[[l_index]] <- pylds_sum
    l_index <- l_index + 1
  }
}

## Uncommnet to check scenario pyld change
# View(pylds_sc_total[[2]])

## Calculate general life tables with modified mortality and pylds total
## Original mortality rate is modified by the mx_sc_total (total change in mortality from diseases)
## Original pyld rate is modified by the change in each disease pylds



general_life_table_list_sc <- list()
index <- 1


for (age in i_age_cohort){
  for (sex in i_sex){
    
    
    # cat("age ", age, " and sex ", sex, "\n")
    # modify idata's mortality and pyld total for the said scenario
    td2 <- input_life_table
    # td2 <- subset(td2, select = -c(mx, pyld_rate))
    td2[td2$age >= age & td2$sex == sex,][[paste("mx")]] <- general_life_table_list_bl[[index]]$mx + mx_sc_total[[index]]$total
    td2[td2$age >= age & td2$sex == sex,][[paste("pyld_rate")]] <- general_life_table_list_bl[[index]]$pyld_rate + pylds_sc_total[[index]]$total
    
    
    # Instead of idata, feed td to run scenarios
    general_life_table_list_sc[[index]] <- run_life_table(in_idata = td2, in_sex = sex, in_mid_age = age)
    #
    
    
    index <- index + 1
  }
}
## Uncommnet to check scenario life tables
# View(general_life_table_list_sc[[32]])
# View(general_life_table_list_bl[[1]])

## Check difference life table baseline and scenario
general_life_table_list_bl[[1]]$Lx - general_life_table_list_sc[[1]]$Lx
general_life_table_list_bl[[1]]$Lwx - general_life_table_list_sc[[1]]$Lwx




# ---- chunk-23 ----

## In the following list "output_life_table", 32 data frames are nested per age and sex cohort

output_burden <- list()
l_index <- 1
index <- 1
for (age in i_age_cohort){
  for (sex in i_sex){
    
    # Males do not have breast cancer, that is why we need the if/else.
    # We create a TRUE/FALSE variable for the loop to move into the next disease
    
    create_new <- T
    for (disease in i_disease) {
      if (sex == "male" && disease == "breast_cancer"){
        # cat("\n")
      }else{
        
        if (create_new){
          output_burden_sc <- select(disease_life_table_list_sc[[index]],
                                     c('age', 'sex', 'incidence_disease', 'mx', 'px'))
          names(output_burden_sc)[names(output_burden_sc) == 'incidence_disease'] <-
            paste('incidence_disease', disease, "sc", sep = "_")
          names(output_burden_sc)[names(output_burden_sc) == 'mx'] <-
            paste('mx', disease, "sc", sep = "_")
          names(output_burden_sc)[names(output_burden_sc) == 'px'] <-
            paste('px', disease, "sc", sep = "_")
          output_burden_bl <- select(disease_life_table_list_bl[[index]],
                                     c('incidence_disease', 'mx', 'px'))
          names(output_burden_bl)[names(output_burden_bl) == 'incidence_disease'] <-
            paste('incidence_disease', disease, "bl", sep = "_")
          names(output_burden_bl)[names(output_burden_bl) == 'mx'] <-
            paste('mx', disease, "bl", sep = "_")
          names(output_burden_bl)[names(output_burden_bl) == 'px'] <-
            paste('px', disease, "bl", sep = "_")
          
          ## New list to add calculations for changes in burden of disease (incidence and mortality numbers)
          
          output_burden_change <- list()
          
          output_burden_change$inc_num_bl <- disease_life_table_list_bl[[index]]$incidence_disease *
            (1 - disease_life_table_list_bl[[index]]$px) * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change$inc_num_sc <- disease_life_table_list_sc[[index]]$incidence_disease *
            (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change$inc_num_diff <- (disease_life_table_list_sc[[index]]$incidence_disease *
                                                  (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx) - (disease_life_table_list_bl[[index]]$incidence_disease * (1 - disease_life_table_list_bl[[index]]$px)
                                                                                                                                              * general_life_table_list_bl[[l_index]]$Lx)
          
          output_burden_change$mx_num_bl <- disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change$mx_num_sc <- disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change$mx_num_diff <- (disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx) - (disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx)
          
          names(output_burden_change)[names(output_burden_change) == 'inc_num_bl'] <-
            paste('inc_num_bl', disease, sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'inc_num_sc'] <-
            paste('inc_num_sc', disease, sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'inc_num_diff'] <-
            paste('inc_num_diff', disease, sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'mx_num_bl'] <-
            paste('mx_num_bl', disease, sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'mx_num_sc'] <-
            paste('mx_num_sc', disease, sep = "_")
          names(output_burden_change)[names(output_burden_change) == 'mx_num_diff'] <-
            paste('mx_num_diff', disease, sep = "_")
          
          ## Bind all lists
          
          output_burden_sc <- cbind(output_burden_sc, output_burden_bl)
          output_burden_sc <- cbind(output_burden_sc, output_burden_change)
          
          create_new <- F
          
          ## Here the calculations above are repeated, here is where the F is telling to move into the next disease
          
        }else{
          
          td3 <- select(disease_life_table_list_sc[[index]],
                        c('incidence_disease', 'mx', 'px'))
          names(td3)[names(td3) == 'incidence_disease'] <-
            paste('incidence_disease', disease, "sc", sep = "_")
          names(td3)[names(td3) == 'mx'] <-
            paste('mx', disease, "sc", sep = "_")
          names(td3)[names(td3) == 'px'] <-
            paste('px', disease, "sc", sep = "_")
          
          td4 <- select(disease_life_table_list_bl[[index]],
                        c('incidence_disease', 'mx', 'px'))
          names(td4)[names(td4) == 'incidence_disease'] <-
            paste('incidence_disease', disease, "bl", sep = "_")
          names(td4)[names(td4) == 'mx'] <-
            paste('mx', disease, "bl", sep = "_")
          names(td4)[names(td4) == 'px'] <-
            paste('px', disease, "bl", sep = "_")
          
          output_burden_change2 <- list()
          
          output_burden_change2$inc_num_bl <- disease_life_table_list_bl[[index]]$incidence_disease * (1 - disease_life_table_list_bl[[index]]$px) * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change2$inc_num_sc <- disease_life_table_list_sc[[index]]$incidence_disease * (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change2$inc_num_diff <- (disease_life_table_list_sc[[index]]$incidence_disease * (1 - disease_life_table_list_sc[[index]]$px) * general_life_table_list_sc[[l_index]]$Lx) - (disease_life_table_list_bl[[index]]$incidence_disease * (1 - disease_life_table_list_bl[[index]]$px) * general_life_table_list_bl[[l_index]]$Lx)
          
          output_burden_change2$mx_num_bl <- disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx
          output_burden_change2$mx_num_sc <- disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx
          output_burden_change2$mx_num_diff <- (disease_life_table_list_sc[[index]]$mx * general_life_table_list_sc[[l_index]]$Lx) - (disease_life_table_list_bl[[index]]$mx * general_life_table_list_bl[[l_index]]$Lx)
          
          names(output_burden_change2)[names(output_burden_change2) == 'inc_num_bl'] <-
            paste('inc_num_bl', disease, sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'inc_num_sc'] <-
            paste('inc_num_sc', disease, sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'inc_num_diff'] <-
            paste('inc_num_diff', disease, sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'mx_num_bl'] <-
            paste('mx_num_bl', disease, sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'mx_num_sc'] <-
            paste('mx_num_sc', disease, sep = "_")
          names(output_burden_change2)[names(output_burden_change2) == 'mx_num_diff'] <-
            paste('mx_num_diff', disease, sep = "_")
          
          
          ## Bind all lists
          
          output_burden_sc <- cbind(output_burden_sc, td3)
          output_burden_sc <- cbind(output_burden_sc, td4)
          output_burden_sc$age_cohort <- age
          output_burden_sc <- cbind(output_burden_sc, output_burden_change2)
          
        }
        
        # cat(age, " - ", sex," - ",  disease," - ",  index, " - ", l_index,  "\n")
        index <- index + 1
      }
      
    }
    
    ## general_life_table_list_sc and general_life_table_list_bl (Lx)
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

## Uncomment to check
# View(output_burden[[1]])



# ---- chunk-24 ----

#####Generate a data frame for all results and create function to get outcomes.

output_df <- plyr::ldply(output_burden, rbind)

# View(output_df)

#Remove variables that are not used in the generation of outputs. CHANGE THIS NAMES, TOO LONG

output_df <- subset(output_df, select = -c(incidence_disease_ihd_bl, incidence_disease_ihd_sc, incidence_disease_istroke_bl, incidence_disease_istroke_sc, incidence_disease_diabetes_bl, incidence_disease_diabetes_sc, incidence_disease_breast_cancer_bl, incidence_disease_breast_cancer_sc,
                                           incidence_disease_colon_cancer_bl, incidence_disease_colon_cancer_sc,  mx_ihd_bl, mx_ihd_sc, mx_istroke_bl, mx_istroke_sc, mx_diabetes_bl, mx_diabetes_sc, mx_breast_cancer_bl, mx_breast_cancer_sc, mx_colon_cancer_bl, mx_colon_cancer_sc, px_ihd_bl, px_ihd_sc, px_istroke_bl, px_istroke_sc, px_diabetes_bl, px_diabetes_sc, px_breast_cancer_bl, px_breast_cancer_sc, px_colon_cancer_bl, px_colon_cancer_sc))



# ---- chunk-25 ----

output_dir = "output/graphs2"

i_age_cohort <- c(22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97)
i_sex <- c("male", "female")
i_measure <- c("deaths", "ylds") #" (years lived with disability)")
i_outcome <- c("mx", "inc")
output_dir <- "output/graphs2"
i_disease <- c("ihd", "istroke", "diabetes", "colon_cancer", "breast_cancer")

i_outcome <- c("mx", "inc")
p_list_male <- list()
p_list_female <- list()
male_index <- 1
female_index <- 1
for (age in i_age_cohort){
  for (sex in i_sex) {
    for (outcome in i_outcome) {
      for (disease in i_disease){
        
        if (sex == "male" && disease == "breast_cancer"){
          # cat("\n")
        }else{
          
          p_index  <- plot_output(in_data = output_df, in_age = age, in_population = sex, in_outcomes = c("age", paste(outcome, "num", "bl", disease, sep = "_"), paste(outcome, "num", "sc", disease, sep = "_"), paste(outcome, "num", "diff", disease, sep = "_")), in_disease = get_qualified_disease_name(disease))
          
          if (sex == "male"){
            
            p_list_male[[male_index]] <- p_index
            
            if (male_index %% 4 == 0 && male_index > 0){
              
              p1 <- p_list_male[[male_index - 3]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p2 <- p_list_male[[male_index - 2]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p3 <- p_list_male[[male_index - 1]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p4 <- p_index + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              
              jpeg(paste0(output_dir, "/", paste(age, sex, outcome, sep="_"), ".jpeg"))
              grid_arrange_shared_legend (p1, p2, p3, p4, ncol = 2, nrow = 2, mainTitle = paste(ifelse(outcome == "mx", "Deaths", "Incidence"), sex, "cohort mid age", age),
                                          mainLeft = 'Cases', mainBottom = 'Age')
              dev.off()
              
            }
            
            male_index <- male_index + 1
            
          }
          
          if (sex == "female" && female_index > 0){
            p_list_female[[female_index]] <- p_index
            
            if (female_index %% 5 == 0){
              p1 <- p_list_female[[female_index - 4]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p2 <- p_list_female[[female_index - 3]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p3 <- p_list_female[[female_index - 2]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p4 <- p_list_female[[female_index - 1]] + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              p5 <- p_index + theme(legend.position="none", axis.title.x = element_blank(),  axis.title.y = element_blank())
              
              jpeg(paste0(output_dir, "/", paste(age, sex, outcome, sep="_"), ".jpeg"))
              grid_arrange_shared_legend (p1, p2, p3, p4, p5, ncol = 2, nrow = 3, mainTitle = paste(ifelse(outcome == "mx", "Deaths", "Incidence"), sex, "cohort mid age", age), mainLeft = 'Cases', mainBottom = 'Age')
              dev.off()
              
            }
            female_index <- female_index + 1
          }
          
        }
      }
    }
  }
}





## Loop to include graphs in the document

# graphs_doc <- list()
# index <- 1
# for (age in i_age_cohort) {
#   for (sex in i_sex)  {
#     for (outcome in i_outcome) {
# 
#       graphs_doc [[index]] <- c(paste(output_dir, "/",age,"_",sex,"_", outcome,".jpeg", sep = ""))
#       knitr::include_graphics(graphs_doc [[index]])
# 
#       index <- index + 1
#     }
#   }
# }






# ---- chunk-26 ----


## Try binding function

aggregate_frame_males <- list()
aggregate_frame_females <- list()

index <- 1

for (outcome in i_outcome) {
  for (disease in i_disease) {
    
    aggregate_frame_males[[index]] <- gen_aggregate(in_data = output_df, in_cohorts = 16, in_population = "male", in_outcomes = c(paste(outcome, "num", "bl", disease, sep = "_"), paste(outcome, "num", "sc", disease, sep = "_"), paste(outcome, "num", "diff", disease, sep = "_")))
    
    aggregate_frame_females[[index]] <- gen_aggregate(in_data = output_df, in_cohorts = 16, in_population = "female", in_outcomes = c(paste(outcome, "num", "bl", disease, sep = "_"), paste(outcome, "num", "sc", disease, sep = "_"), paste(outcome, "num", "diff", disease, sep = "_")))
    
    # Remove non-numeric columns starting with age and sex
    aggregate_frame_males[[index]] <- aggregate_frame_males[[index]] %>% select(-starts_with("age"), -starts_with("sex"))
    
    aggregate_frame_females[[index]] <- aggregate_frame_females[[index]] %>% select(-starts_with("age"), -starts_with("sex"))
    
    index <- index + 1
  }
}


## Loop for life years (Lx) and health adjusted life years (Lwx) to then add to total aggregated data.

## RUN ANOTHER LOOP FOR LX AND LWX

i_outcome2 <- c("Lx", "Lwx")

aggregate_frame_males2 <- list()
aggregate_frame_females2 <- list()

for (i in i_outcome2){
  
  aggregate_frame_males2[[i]] <- gen_aggregate(in_data = output_df, in_cohorts = 16, in_population = "male", in_outcomes = c(paste(i, "bl", sep = "_"), paste(i, "sc", sep = "_"), paste(i, "diff",sep = "_")))
  
  aggregate_frame_females2[[i]] <- gen_aggregate(in_data = output_df, in_cohorts = 16, in_population = "female", in_outcomes = c(paste(i, "bl", sep = "_"), paste(i,  "sc", sep = "_"), paste(i, "diff",sep = "_")))
  
  
  aggregate_frame_males2[[i]] <- aggregate_frame_males2[[i]] %>% select(-starts_with("age"), -starts_with("sex"))
  
  aggregate_frame_females2[[i]] <- aggregate_frame_females2[[i]] %>% select(-starts_with("age"), -starts_with("sex"))
  
}

## Uncomment to check data frames (list correspond to a disease and outcome combination, for example,
## incidence breast cancer)

# View(aggregate_frame_females[[2]])

## Transform list to data frame (this is not working after the first disease)

aggregate_frame_females <- do.call(cbind, aggregate_frame_females)
aggregate_frame_males <- do.call(cbind, aggregate_frame_males)
aggregate_frame_females2 <- do.call(cbind, aggregate_frame_females2)
aggregate_frame_males2 <- do.call(cbind, aggregate_frame_males2)

View(aggregate_frame_females2)

## Drop ending of variables names _males/_females to add up all outcomes in next step.

names(aggregate_frame_females) = gsub(pattern = "_female", replacement = "", x = names(aggregate_frame_females))

names(aggregate_frame_males) = gsub(pattern = "_male", replacement = "", x = names(aggregate_frame_males))

names(aggregate_frame_females2) = gsub(pattern = "_female", replacement = "", x = names(aggregate_frame_females2))

names(aggregate_frame_males2) = gsub(pattern = "_male", replacement = "", x = names(aggregate_frame_males2))


## Uncomment to check that the code is dropping the male/female ending.
# View(aggregate_frame_males)

## Create a copy of aggregate_frame_females.
total_aggr1 <- aggregate_frame_females
## Add aggregate_frame_males values to it
for (i in 1:ncol(aggregate_frame_females)){
  total_aggr1[i] <- total_aggr1[i] + aggregate_frame_males[i]
}

## Add data frames 2 with life years (check adds Lx and Lwx at the beginning of the variable name)
total_aggr2 <- aggregate_frame_females2
## Add aggregate_frame_males values to it
for (i in 1:ncol(aggregate_frame_females2)){
  total_aggr2[i] <- total_aggr2[i] + aggregate_frame_males2[i]
}

## Combine data frames

total_aggr <- cbind.data.frame(total_aggr1, total_aggr2)

total_aggr$sim_year <- seq.int(nrow(total_aggr))


## Uncomment to see total data frame
View(total_aggr)

## Test that total_aggr is adding males and females dataframes
#
# (aggregate_frame_females2$Lx.Lx_bl_22 + aggregate_frame_males2$Lx.Lx_bl_22) -total_aggr$Lx.Lx_bl_22




# ---- chunk-27 ----


####This plot has to be customised to in_outcomes, here, only totals shown, but specifications are up to the user. ADD LOOP for all outcomes over time and total TABLE.

####[] is used here to indicate the number of simulation years into the future.
####Disease outcomes has to be changed to the outcome of interest

#### Test code with loops for aggregated outcomes diseases burden. NOT WORKING.

### Compare with loops for age and sex cohort outcomes.

p_aggr_list <- list()
index <- 1

for (outcome in i_outcome) {
  for (disease in i_disease) {
    # outcome <- i_outcome[1]
    # disease <- i_disease[1]
    
    p_aggr_list_index <- ggplot(total_aggr[1:79,], aes(x = total_aggr[["sim_year"]])) +
      
      geom_line(mapping = aes(y = total_aggr[[paste("total", outcome, "num_bl", disease, sep = "_")]], colour = paste("total", outcome, "num_bl", disease, sep = "_"))) +
      theme_classic() +
      geom_hline(yintercept=0, linetype="dashed", color = "black") +
      geom_line(mapping = aes(y = total_aggr[[paste("total", outcome, "num_sc", disease, sep = "_")]], colour = paste("total", outcome, "num_sc", disease, sep = "_"))) +
      geom_line(mapping = aes(y = total_aggr[[paste("total", outcome, "num_diff", disease, sep = "_")]], colour = paste("total", outcome, "num_diff", disease, sep = "_"))) +
      xlab ("Simulation years") + ylab ("Cases") + labs (title = paste(disease, outcome)) +
      theme(plot.title = element_text(hjust = 0.5, size = 12)) +
      scale_color_discrete(name = paste(""), labels = c("Baseline", "Difference", "Scenario")) +
      theme(plot.title = element_text(hjust = 0.5))
    p_aggr_list[[index]] <- p_aggr_list_index
    index <- index + 1
    
    
  }
}

index <- 1

interpolation_index <- 1
for (outcome in i_outcome) {
  for (disease in i_disease) {
    file_name = paste("output/graphs2/", "Aggregated Outcomes", outcome, disease, ".jpeg", sep=" ")
    jpeg(file_name)
    print(p_aggr_list[[index]])
    index <- index + 1
    dev.off()
  }
}


p_aggregated <- do.call(marrangeGrob, list(grobs=p_aggr_list, nrow = 2, ncol = 2))
p_aggregated






