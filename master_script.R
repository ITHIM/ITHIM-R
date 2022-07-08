# Runs - and knits, all relevant codes for running the model in constant mode
# AA - 8th July 2022

# Source the multi city script
# Please make sure that the list of cities and the version of the inputparameters file are correctly updated
source("multi_city_script.R")

# Once the multi_city_script.R has run, the ithim_object (io) is saved at results/multi_city/io.rds
# Knit summary document for dist/dur and scenario settings
rmarkdown::render("summary_tables.Rmd")

# Knit summary document for PA and AP distribution
rmarkdown::render("summary_tables_PA_AP.Rmd")

# Knit summary document for dist/dur and scenario settings
rmarkdown::render("injury_tables.Rmd")
