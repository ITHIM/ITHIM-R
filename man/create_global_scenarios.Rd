% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_global_scenarios.R
\name{create_global_scenarios}
\alias{create_global_scenarios}
\title{Create global scenarios}
\usage{
create_global_scenarios(trip_set)
}
\arguments{
\item{trip_set}{data frame, baseline scenario}
}
\value{
list of baseline scenario and four mode scenarios
}
\description{
Creates four scenarios where in each one, the mode share of a given mode is elevated by a set
percentage of the total trips. The scenario modes are cycle, car, and bus and motorcycle.
}
\details{
This function creates four scenarios increasing the mode shares of cycling, car,
bus and motorcycle by the same pre-defined percentage of the total mode shares.
We assume that:

\itemize{
\item the total number of trips remains the same but trips from other modes
  (apart from truck, bus driver, car driver and commercial motorcycle trips
  which remain unchanged - at least initially) are converted to the mode in
  question; truck and commercial motorcycle trips remain constant across all
  scenarios whilst bus driver and car driver trips are updated based on the new
  total distance of car and bus trips once the increase in mode share has been
  conducted for each scenario

\item the percentage share across the three distance bands of the mode that
  is increased is preserved, i.e. if 10% of all cycling trips are in distance
  band 0-2km then after increasing the cycling mode share by x% of all trips
  we still have 10% of all cycling trips in this distance band. Here we use the
  mean mode split across all 28 cities (Antofagasta, Arica, Belo Horizonte,
  Bogota, Buenos Aires, Cali, Copiapo, Coquimbo / La Serena, Gran Valparaiso,
  Iquique / Alto Hospicio, Medellin, Mexico City, Montevideo, Osorno, Puerto Montt,
  San Antonio, Santiago, Sao Paulo, Temuco / Padre las Casas, Valdivia, Accra,
  Bangalore, Cape Town, Delhi, Vizag, Kisumu, Nairobi and Port Louis) as
  the baseline mode split for each mode.

\item For each scenario we always convert the same % of total trips to the mode
  in question, independently of the original overall mode share of that mode.
  I.e. if e.g. 1% of all trips are cycle trips and 50% car trips and we apply a
  5% increase, then in the cycle scenario 6% of all trips are cycling trips and
  in the car scenario 55% of all trips are car trips. We always add a 5% increase
  of the total trips.

\item we preserve the proportion of trips in each distance band. E.g. if 20% of
  all trips are in distance band 0-2km, then in each scenario 20% of all
  trips are still in distance band 0-2km. These proportions are calculated for
  each city individually.
}


Example:

Assume that there are only two distance bands A and B and that 80% of all
cycling trips lie in distance band A and the remaining 20% in distance
band B.
Assume that 60% of all trips are in distance band A and 40% in distance
band B.
Assume we want to increase the cycling mode share by 5% of all trips.

Then, we need to convert 5% x 80% / 60% = 6.67% of non-cycling trips in distance band A
to cycling trips and 5% x 20% / 40% = 2.5% of non-cycling trips in distance band B
to cycling trips.

Overall, this leads to an increase of
(5% x 80% / 60%) x 60% + (5% x 20% / 40%) * 40% = 5%
of cycling trips, whilst preserving the cycling mode shares of 80% in distance
band A and 20% in distance band B and preserving the total mode shares of 60% in
distance band A and 40% in distance band B. The total number of trips is also
preserved.


The function performs the following steps:

\itemize{
\item the overall mode shares for each of the cycle, car, bus and motorcycle
 modes across the three distance categories is defined

\item from the trip data extract the trip information, calculate the total number of trips
  and find the proportion of trips in each distance category

\item find the proportion of trips to be converted for each mode, scenario and distance category

\item define the modes that are not changeable (at least initially) between the scenarios and
  divide the trip data into a set with the modes that can be changed and another set with the
  trips whose modes cannot be changed

\item split the changeable trips and also all trips by distance band

\item to create the scenarios loop through the scenarios, i.e. the 4 different modes:
  \itemize{
  \item loop through each distance band:
    \itemize{
    \item find the changeable trips that are not of the mode to be increased

    \item count the number of trips made by the mode to be increased

    \item for the bus scenario we aim to increase all public transport trips,
          i.e. we find the changeable trips not made by bus or rail and we count
          the number of trips made by bus or rail

    \item find the number of total trips that we would like to convert

    \item if the number of trips that are changeable equals the number of trips to be
          converted, all changeable trips are converted to the mode in question

    \item if there are more trips that are changeable than we want to change,
          sample the number of trips to change from the changeable trip ids

    \item if there are more trips to be converted than there are changeable trips, then
          convert as many trips as possible and create a warning message

    \item convert the required trips to the new mode in question
    }
  \item add all trips across the distance bands and add the non-changeable trips

  \item update the bus_driver and car_driver trips
  }
\item create a list containing the trips for each scenario as elements
}
}
