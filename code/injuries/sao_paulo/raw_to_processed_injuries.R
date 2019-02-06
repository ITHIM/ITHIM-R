# Read library
require(tidyverse)

#Read raw sp injuries
rd <- read_csv("inst/extdata/local/sao_paulo/sao_paulo_processed_2009_2013.csv")

# Convert to the required structure
# Make sure that we have the all required variables
# Make sure modes are standardized
# Rob can you have a look please?