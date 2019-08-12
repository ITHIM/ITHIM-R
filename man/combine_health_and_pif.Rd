% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combine_health_and_pif.R
\name{combine_health_and_pif}
\alias{combine_health_and_pif}
\title{Combine health and PIF}
\usage{
combine_health_and_pif(pif_values, hc = DISEASE_BURDEN)
}
\arguments{
\item{pif_values}{vector of values of PIFs}

\item{hc}{data frame of current burden of disease}
}
\value{
estimated scenario burden of disease
}
\description{
Applies PIF calculated from RRs to the current observed health burden from GBD to generate scenario health burden
}