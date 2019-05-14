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


### Prepare data for Bristol City Region

gbd_bristol <- read.csv(file="code/MSLT/data/city regions/bristol/gbd_data_bristol.csv")


