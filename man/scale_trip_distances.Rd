% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/walk_to_pt_and_combine_scen.R
\name{scale_trip_distances}
\alias{scale_trip_distances}
\title{Scale trip distances}
\usage{
scale_trip_distances(trips)
}
\arguments{
\item{trips}{data frame, all trips from all scenarios}
}
\value{
data frame, all trips from all scenarios with scaled distances
}
\description{
Applies mode-specific distance scalars to all trips
}
\details{
The function is used to multiply all trip stages belonging to a certain mode
by a city specific scalar. Note that walk to pt stages are counted as
public transport stages and are multiplied by the DISTANCE_SCALAR_PT

The function performs the following steps:

\itemize{
\item define all car and public transport modes

\item multiply all stage distances and stage durations by the corresponding distance scalars
}
}
