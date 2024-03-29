% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/health_burden.R
\name{join_hb_and_injury}
\alias{join_hb_and_injury}
\title{Join disease health burden and injury data}
\usage{
join_hb_and_injury(ind_ap_pa, inj)
}
\arguments{
\item{ind_ap_pa}{list (deaths, YLLs) of data frames of all demographic groups' burdens of diseases}

\item{inj}{list (deaths, YLLs) of data frames of all demographic groups' burdens for road-traffic injury}
}
\value{
list of dataframes: one for deaths per cause per demographic group, and likewise for YLLs
}
\description{
Join the two data frames for health burden: that from disease, and that from road-traffic injury
}
\details{
This function performs the following steps:

\itemize{
 \item extract the yll and deaths data from the AP and PA pathways
 \item extract the yll and deaths data from the injury data
 \item create one dataframe for yll and one for deaths containing all the AP, PA and injury data
}
}
