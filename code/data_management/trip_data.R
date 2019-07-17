library(tidyverse)

##Rob complained that one id in Sao Paolo is associated with two people
x <- read_csv('J:/Group/lambed/ITHIM-R/inst/extdata/local/sao_paulo/trips_sao_paulo.csv')
y[y$participant_id==94609,]

y <- read_csv("J:/Group/lambed/ITHIM-R/data/local/sao_paulo/sao_paulo_processed_2009_2013.csv")

w <- read_csv("J:/Studies/MOVED/HealthImpact/Data/TIGTHAT/Brazil/Sao Paulo/Pesquisa Origem Destino 2012/Mobilidade_2012_v0.csv")

View(w)

w[w$ID_PESS==94609,]


Hi Rob,

You are correct. I randomly picked out a few more id's too: 94606, 94609, 175128, 175068...

After glancing through available datasets for Sao Paulo, I'm not sure how it was processed: the initial dataset has ~25000 people while the final has 8x that number (I am wondering if this dataset we are looking at resulted from imputation).

Ali, do you have any memory of what went on or should I flag this problem up on Monday for further digging.

Kind regards,
Lambed
