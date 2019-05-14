# ---- This code is to organise the the data downloaded from GBD (http://ghdx.healthdata.org/gbd-results-tool) ----

### City regions: 

# Sheffield City Region Combined Authority: Barnsley, Doncaster, Rotherham, Sheffield.

# North East Combined Authority: County Durham, Gateshead, Newcastle upon Tyne, North Tyneside, Northumberland, South Tyneside, Sunderland.

# Greater Manchester Combined Authority: Bolton, Bury, Manchester, Oldham, Rochdale, Salford, Stockport, Tameside, Trafford, Wigan.

# Liverpool City Region Combined Authority: Halton, Knowsley, Liverpool, St. Helens, Sefton, Wirral.

# West Yorkshire Combined Authority: Bradford, Calderdale, Kirklees, Leeds, Wakefield.

# Bristol: Bath and North East Somerset, City of Bristol, North Somerset, South Gloucestershire.

# Nottingham (Only GBD data for Nottingham): Ashfield, Bassetlaw, Broxtowe, Gedling, Mansfield, Nottingham, Newark and Sherwood, Rushcliffe.

# West Midlands Combined Authority: Birmingham, Coventry, Dudley, Sandwell, Solihull, Walsall, Wolverhampton.

# London: all London boroughs and the City of London.

### GBD data is in 5-year age groups. Check whether GBD has population data. 
### Data needs: PMSLT: general life table: population numbers, all-cause mortality rates and total YLDs rates
###  disease life tables: ylds numbers, prevalence numbers (check calcls elsewhere)
### dismod or disbayes: population numbers and all-cause mortality rates (Dismod) and incidence and mortality numbers

### Disbayes (Chris Jackson Bayesian Dismod mode, https://chjackson.github.io/disbayes/vignette.html)
install.packages("rstan")
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


### Prepare data for Bristol City Region (got data from 2007 to 2017 for trends, here only use 2017 data) 
### Do the same per region if it works (then do a loop)

gbd_bristol <- read.csv(file="code/MSLT/data/city regions/bristol/gbd_data_bristol.csv")
as.integer(gbd_bristol$year)

## May not need if loop works to create a list of localities by year 
##gbd_bristol_2017 <- gbd_bristol[which(gbd_bristol$year=='2017'),]

localities <- c('Bristol, City of', 'Bath and North East Somerset', 'North Somerset', 'South Gloucestershire')
year <- c(2007:2017)

### Loop to create a data set for 2017 for each of the localities to calculate population numbers

gbd_data_localities <- list()
index <- 1


for (l in localities){
  for (y in year){
    
    region_data[[index]] <- sort_gbd_input(in_data = gbd_bristol, in_year = y, in_locality = l)
    
    index <- index + 1
  }
}
    
    

disease_life_table_list_bl <- list()
index <- 1

for (age in i_age_cohort){
  for (sex in i_sex){
    for (disease in i_disease) {
      # Exclude bc for Males
      if (sex == "male" && disease == "bc"){
        # cat("\n") #Uncomment to see list
      }
      else {
        # cat("age ", age, " sex ", sex, "and disease", disease, "\n") #Uncomment to see list
        disease_life_table_list_bl[[index]] <- run_disease(in_idata = mslt_df, in_sex = sex, in_mid_age = age, in_disease = disease)
        index <- index + 1
      }
    }
  }
}
