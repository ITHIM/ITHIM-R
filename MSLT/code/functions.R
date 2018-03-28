#####Packages

require(dplyr)
require(tidyverse)

#############################Explanation method###################################

#####Briefly, the proportional multi-state multi cohort life table consists of a 
#####general life table and a life table for each of the modelled diseases.
#####The diseases are those associated to the studied risk factor/s. 
#####The link between the general life table and disease life tables is via the 
#####potential impact fraction (pif), also called paf (population attributable fraction)
#####The pif combines exposure to the risk factor and relative risks. The pif is 
#####appleid to modify incidence in the individual disease life tables, which in turn
#####modify prevalence and mortality. Changes in mortality and prevalence rates
#####feed bacak into the general life table to modify total mortality and disability. 
####Changes in total mortality impact on life years and changes in disability impact 
####the disability adjusment of life years. 

#### Method reference: 1.	Barendregt JJ, Oortmarssen vGJ, Murray CJ, Vos T. A generic model for the assessment of disease epidemiology: the computational basis of DisMod II. Popul Health Metr. 2003;1(1):4-.


####The functions: run_life_table, run_disease, and run_pif serve to generate outputs
####per age and sex (see model.R). The function run_output to facilitate the generation
####of results. 


#############################Function for general life table###################################
#####Function to generate age and sex life tables. The function is then use in the model.R script
#####to calculate variables for the baseline and scenario life tables. 

run_life_table <- function(in_idata, in_sex, in_mid_age)
{
  
  # Create a life table data frame
  
  # Filter in_idata by age and select columns for lifetable calculations
  lf_df <- filter(in_idata, age >= in_mid_age & sex == in_sex) %>% select(sex, age, pyld_rate, mx)
  
  # Create list of required columns (variables)
  
  # probability of dying
  lf_df$qx <-  ifelse(lf_df$age < 100, 1 - exp(-1 * lf_df$mx), 1)
  
  # number of survivors
  lf_df$lx <- 0
  # Create it for males population
  lf_df$lx[1] <- filter(in_idata, age == in_mid_age & sex == in_sex) %>% select(five_year_population)
  lf_df$lx <- as.numeric(lf_df$lx)
  
  # number died
  lf_df$dx <- 0
  
  # Create it for males population
  lf_df$dx[1] <- lf_df$lx[1] * lf_df$qx[1]
  
  for (i in 2:nrow(lf_df)){
    lf_df$lx[i] <- lf_df$lx[i - 1] - lf_df$dx[i - 1]
    lf_df$dx[i] <- lf_df$lx[i] * lf_df$qx[i]
  }
  
  # number of persons lived by cohort to age x + 1/2 (average people)
  lf_df$Lx <- 0
  
  for (i in 1:nrow(lf_df)){
    if (i < nrow(lf_df))
      lf_df$Lx[i] <- sum(lf_df$lx[i] + lf_df$lx[i + 1]) / 2
    else
      lf_df$Lx[i] <- lf_df$lx[i] / lf_df$mx[i]
    
  }
  
  # create life expectancy variable
  for (i in 1:nrow(lf_df)){
    lf_df$ex[i] <- sum(lf_df$Lx[i:nrow(lf_df)]) / lf_df$lx[i]
  }
  
  # create health adjusted life years variable
  lf_df$Lwx <- lf_df$Lx * (1 - lf_df$pyld_rate)
  
  # create health adjusted life expectancy variable
  for (i in 1:nrow(lf_df)){
    lf_df$ewx[i] <- sum(lf_df$Lwx[i:nrow(lf_df)]) / lf_df$lx[i]
  }
  
  lf_df
  
}


#############################Function for disease life tables###################################

run_disease <- function(in_idata, in_mid_age, in_sex, in_disease) 
  
{
  
  # Uncomment the variables below to debug your code  
  # in_idata = sub_idata
  # in_sex = "males"
  # in_mid_age = 22
  # in_disease = "ihd"
  
  # create disease variable for the disease life table function 
  dw_disease <- paste("dw", in_disease, sep = "_")
  incidence_disease <- paste("incidence", in_disease, sep = "_")
  case_fatality_disease <- paste("case_fatality", in_disease, sep = "_")
  
  ## add generic variable names to the source data frame (in_idata)
  in_idata$dw_disease <- in_idata[[dw_disease]]
  in_idata$incidence_disease <- in_idata[[incidence_disease]]
  in_idata$case_fatality_disease <- in_idata[[case_fatality_disease]]
  
  # filter in_idata by age and select columns for lifetable calculations
  dlt_df <- filter(in_idata, age >= in_mid_age & sex == in_sex) %>% 
  select(sex, age, dw_disease, incidence_disease, case_fatality_disease)
  
  dlt_df$disease <- in_disease
  
  # create list of required columns
  ## intermediate variables lx, qx, wx and vx
  ###lx
  dlt_df$lx <- dlt_df$incidence_disease + dlt_df$case_fatality_disease
  ###qx
  dlt_df$qx <-  sqrt((dlt_df$incidence_disease - dlt_df$case_fatality_disease) * (dlt_df$incidence_disease - dlt_df$case_fatality_disease))
  ### wx
  dlt_df$wx <- exp(-1*(dlt_df$lx+dlt_df$qx)/2)
  ### vx
  dlt_df$vx <- exp(-1*(dlt_df$lx-dlt_df$qx)/2)
  
  ## Healthy (Sx), Disease (Cx) and Death (Dx), total (Tx) (control check, has to be 1000), total alive (Ax)
  ## persons years live at risk (PYx), prevalence rate (px), mortality rate (mx)
  ### Remission and mortality from other causes were replaced by zero in the formulas (as we assume no remission and independence of disease mortality with total mortlaity). 
  
  #### first create empty variables
  
  dlt_df$Sx <- 0
  dlt_df$Cx <- 0
  dlt_df$Dx <- 0
  dlt_df$Tx  <- 0
  dlt_df$Ax <- 0
  dlt_df$PYx <- 0
  dlt_df$px <- 0
  dlt_df$mx <- 0
  
  ##### start with variables without calculation exceptions
  
  for (i in 1:nrow(dlt_df)){
    dlt_df$Tx   <- dlt_df$Sx + dlt_df$Cx + dlt_df$Dx 
    dlt_df$Ax <- dlt_df$Sx + dlt_df$Cx
    
    ##### variables with exceptions  
    
    for (i in 1:nrow(dlt_df)){
      if (i < nrow(dlt_df))
        dlt_df$PYx[i] <- sum(dlt_df$Ax[i] + dlt_df$Ax[i + 1])/2
      else
        dlt_df$PYx[i] <- 0
      
      
      if(is.na(sum(dlt_df$Cx[i] + dlt_df$Cx[i + 1])/2)) { 
        dlt_df$px[i] <- 0
      }
      else{
        dlt_df$px[i] <- (sum(dlt_df$Cx[i] + dlt_df$Cx[i + 1])/2) / dlt_df$PYx[i]    
        
        
        if ((dlt_df$Dx[i+1] - dlt_df$Dx[i]) < 0){
          dlt_df$mx[i] <- 0
        }
        else{
          dlt_df$mx[i] <- ((dlt_df$Dx[i+1] - dlt_df$Dx[i])/dlt_df$PYx[i])
          
          
          if (dlt_df$age[i] == in_mid_age){
            dlt_df$Sx[i] <- 1000
            dlt_df$Cx[i] <- 0
            dlt_df$Dx[i] <- 0
            
          }
          else{
            dlt_df$Sx[i] <- ifelse(dlt_df$qx[i-1] > 0, (2*(dlt_df$vx[i-1] - dlt_df$wx[i-1]) * 
                                                          (dlt_df$Sx[i-1] * (dlt_df$case_fatality_disease [i-1] + 0 +0) + 
                                                             dlt_df$Cx[i-1] * 0) + dlt_df$Sx[i-1] * (dlt_df$vx[i-1] * (dlt_df$qx[i-1] 
                                                                                                                       - dlt_df$lx[i-1]) + dlt_df$wx[i-1] * (dlt_df$qx[i-1] + dlt_df$lx[i-1]))) 
                                   / (2 * (dlt_df$qx[i-1])), dlt_df$Sx[i - 1] )
            dlt_df$Cx[i] <- ifelse(dlt_df$qx[i-1] > 0, -1*((dlt_df$vx[i-1] - dlt_df$wx[i-1])*
                                                             (2*((dlt_df$case_fatality_disease[i-1] + 0 + 0) * 
                                                                   (dlt_df$Sx[i-1]+dlt_df$Cx[i-1]) - dlt_df$lx[i-1] * dlt_df$Sx[i-1] + 0 * 
                                                                   dlt_df$Sx[i-1]) - dlt_df$Cx[i-1] * dlt_df$lx[i-1]) - dlt_df$Cx[i-1] * 
                                                             dlt_df$qx[i-1] * (dlt_df$vx[i-1]+dlt_df$wx[i-1])) 
                                   / (2 * (dlt_df$qx[i-1])), dlt_df$Cx[i - 1])
            dlt_df$Dx[i] <- ifelse(dlt_df$qx[i-1] > 0, ((dlt_df$vx[i-1] - dlt_df$wx[i-1]) * 
                                                          (2 * dlt_df$case_fatality_disease[i-1] * 
                                                             dlt_df$Cx[i-1] - dlt_df$lx[i-1]*
                                                             (dlt_df$Sx[i-1] + dlt_df$Cx[i-1]))
                                                        - dlt_df$qx[i-1] * (dlt_df$Sx[i-1] + dlt_df$Cx[i-1])
                                                        *(dlt_df$vx[i-1]+dlt_df$wx[i-1]) + 2 * dlt_df$qx[i-1] * 
                                                          (dlt_df$Sx[i-1]+dlt_df$Cx[i-1]+dlt_df$Dx[i-1])) 
                                   / (2 * (dlt_df$qx[i-1])), dlt_df$Dx[i - 1])
            
            
          }}}}}
  dlt_df
}


#######################################Function for PIFs########################################

##### the code for PIFs will depend on the data sources. 


run_pif <- function(in_idata, i_irr, i_exposure, in_mid_age, in_sex, in_disease, in_met_sc) 
  # 
{
  
  ## uncomment to debug function
  
  # in_idata = idata
  # i_irr = irr
  # i_exposure = edata
  # in_sex = "females"
  # in_mid_age = 22
  # in_disease = "breast_cancer"
  # in_met_sc <- effect
  
  ### filter data to use in pif calculations (age and sex). Add rrs, ee and calculations
  
  pif_df <- filter(in_idata, age >= in_mid_age & sex == in_sex) %>%
    select(sex, age)
  
  ### add varaibles to data.frame (different age category for breast cancer)
  
  pif_df$disease <- in_disease
  
  if(in_disease == "breast_cancer") {
    pif_df$age_cat [pif_df$age <=30] <- 30
    pif_df$age_cat [pif_df$age >30 & pif_df$age <=45 ] <- 45
    pif_df$age_cat [pif_df$age >45 & pif_df$age <=70 ] <- 70
    pif_df$age_cat [pif_df$age >70 & pif_df$age <=100 ] <- 80
  }
  else {
    pif_df$age_cat [pif_df$age <=30] <- 30
    pif_df$age_cat [pif_df$age >30 & pif_df$age <=70 ] <- 70
    pif_df$age_cat [pif_df$age >70 & pif_df$age <=100 ] <- 80
  }
  
  pif_df$sex_cat <- ifelse(in_disease == "breast_cancer", "female", "female_male")
  
  # create concatenated variables to match pif_df with i_irr
  pif_df$sex_age_dis_cat <- paste(pif_df$disease,pif_df$age_cat, pif_df$sex_cat, sep = "_"  )
  i_irr$sex_age_dis_cat <- paste(i_irr$disease,i_irr$age, i_irr$sex, sep = "_"  )
  
  # remove sex, age and disease variables from i_irr df, as they are not needed
  i_irr <- select(i_irr, -one_of('sex','age', 'disease'))
  
  # the code below is working but copies age, sex and disease for x and y, how can this be avoided?
  pif_df <-  inner_join(pif_df, i_irr, by = c("sex_age_dis_cat" = "sex_age_dis_cat") , copy = FALSE)
  
  # creation of splineFun which uses baseline's RR and EE to use to estimate intervention RRs
  for (i in 1:nrow(pif_df)){
    sp_obj <-  splinefun(y = c(pif_df$rr_inactive[i], 
                               pif_df$rr_insufficiently_active[i], 
                               pif_df$rr_recommended_level_active[i], 
                               pif_df$rr_highly_active[i]), 
                         x = c(pif_df$ee_inactive[i], 
                               pif_df$ee_insufficiently_active[i], 
                               pif_df$ee_recommended_level_active[i], 
                               pif_df$ee_highly_active[i]), 
                         method = "hyman")
    
    # use created spline function above to estimate intervention RRs
    
    pif_df$sc_rr_inactive[i] <- sp_obj(x = pif_df$ee_inactive[i] + in_met_sc)
    pif_df$sc_rr_insufficiently_active[i] <-  sp_obj(x = pif_df$ee_insufficiently_active[i] + in_met_sc)
    pif_df$sc_rr_recommended_level_active[i] <-  sp_obj(x = pif_df$ee_recommended_level_active[i] + in_met_sc)
    pif_df$sc_rr_highly_active[i] <-  sp_obj(x = pif_df$ee_highly_active[i] + in_met_sc)
    
    # plot(sp_obj, xlab = "RR", ylab = "EE", main = paste("Spline ", i))
  }
  
  
  ## round sc_rr_highly_active column - it should be 1
  pif_df$sc_rr_highly_active <- round(pif_df$sc_rr_highly_active)
  
  ##Calculate PIFs. I already process the data to generate categories in stata.
  ##First add PA categories to pif_df
  
  pif_df$sex_age_cat <- paste(pif_df$sex, pif_df$age, sep = "_"  )
  i_exposure$sex_age_cat <- paste(i_exposure$sex, i_exposure$age, sep = "_"  )
  
  # remove sex, age and disease variables from i_irr df, as they are not needed
  i_exposure <- select(i_exposure, -one_of('sex','age'))
  
  # join edata (PA prevalence to pif_df)
  
  pif_df <-  inner_join(pif_df, i_exposure, by = c("sex_age_cat" = "sex_age_cat") , copy = FALSE)
  
 
  # we need to adapt to ITHIMR developments. REPLACE DATA FRAME FROM WHICH PREVALENCE OF PA IS TAKEN
  
  pif_df$pif <- 1-(pif_df$sc_rr_inactive *pif_df$inactive +
                     pif_df$sc_rr_insufficiently_active*pif_df$insufficiently_active +
                     pif_df$sc_rr_recommended_level_active*pif_df$recommended_level_active +
                     pif_df$sc_rr_highly_active *pif_df$highly_active)/
    (pif_df$rr_inactive *pif_df$inactive  +
       pif_df$rr_insufficiently_active *pif_df$insufficiently_active +
       pif_df$rr_recommended_level_active *pif_df$recommended_level_active +
       pif_df$rr_highly_active *pif_df$highly_active)
  
  
  pif_df
  
}


################################################Function for output#############################################

##### function to generate graphs by age and sex, per ooutcome of interest. 


plot_output <- function(in_data, in_age, in_population, in_outcomes, in_legend){
  
  # in_data <- output_df
  # in_population <- "males"
  # in_age <- 22
  # in_outcomes <- c('age', 'inc_num_bl_ihd', 'inc_num_sc_ihd')
  # in_cols <- c('alpha', 'beta')
  
  data <- in_data
  
  if (in_population != "total")
    data <- filter(data, sex == in_population)
  if (length(in_age) > 0)
    data <- filter(data, age_cohort == in_age)
  if (length(in_outcomes) > 0)
    data <- select(data, in_outcomes)
  
  td <- data
  p <- ggplot(data = td, aes (x = td[[in_outcomes[[1]]]]))
  
  # loop
  for (i in 2:length(in_outcomes)) {
    # use aes_string with names of the data.frame
    p <- p + geom_line(aes_string(y = td[[in_outcomes[i]]], color = as.factor(in_outcomes[i])), size = 0.8) +
      theme_classic() 
    
    
  }
  
  p <- p + scale_color_discrete(name = paste(in_legend), labels = c("Baseline", "Scenario", "Difference"))
  
  p <- p + xlab ('Age') + ylab ('Cases') + labs (title = paste('Cohort', in_age, "years old", in_population, sep = " ")) +
    theme(plot.title = element_text(hjust = 0.5, size = 12)) + 
    xlim(in_age, 100) +
    geom_hline(yintercept=0, linetype="dashed", color = "black")
  
  
  # print the result
  print(p)
  

}


###Function to generate aggregated outcome for all age groups and gender


gen_aggregate <- function(in_data, in_cohorts, in_population, in_outcomes){
  
  
  # in_data <- output_df
  # in_population <- "males"
  # in_cohorts <- 10
  # in_outcomes <- c('inc_num_bl_ihd', 'inc_num_sc_ihd')
  
  age_cohort_list <- list()
  td <- in_data
  aggr <- list()
  l_age <-  min(td$age_cohort)
  for (i in 1:in_cohorts){
    if (l_age <= 100){
      ld <- dplyr::filter(td, age_cohort == l_age)
      
      if (in_population != "total")
        ld <- filter(ld, sex == in_population)
      if (length(in_outcomes) > 0)
        ld <- select(ld, age, sex, in_outcomes)
      if (i == 1){
        aggr <- append(aggr, as.list(ld))
        aggr <- as.data.frame(aggr)
        names(aggr) <- paste(names(aggr), l_age, in_population, sep = "_" )
      }
      else {
        n_rows <-  nrow(aggr) - nrow(ld)
        ld[(nrow(ld) + 1):(nrow(ld) + n_rows),] <- NA
        names(ld) <- paste(names(ld), l_age, in_population, sep = "_" )
        aggr <- cbind(aggr, ld)
      }
      
      l_age <- l_age + 5
    }
  }
  
  for (i in 1:length(in_outcomes)){
    aggr[[paste0("total_",in_outcomes[i])]] <- select(aggr, starts_with(in_outcomes[i])) %>% rowSums(na.rm = T)
    
  }
  
  aggr
}
