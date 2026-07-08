# Create Latam scenarios

Creates four scenarios where in each one, the mode share of a given mode
is elevated by a set percentage of the total trips. The scenario modes
are cycle, car, and bus and motorcycle.

## Usage

``` r
create_latam_scenarios(trip_set)
```

## Arguments

- trip_set:

  data frame, baseline scenario

## Value

list of baseline scenario and four mode scenarios

## Details

This function creates four scenarios increasing the mode shares of
cycling, car, bus and motorcycle by the same pre-defined percentage of
the total mode shares. We assume that:

- the total number of trips remains the same but trips from other modes
  (apart from truck, bus driver, car driver and commercial motorcycle
  trips which remain unchanged - at least initially) are converted to
  the mode in question; truck and commercial motorcycle trips remain
  constant across all scenarios whilst bus driver and car driver trips
  are updated based on the new total distance of car and bus trips once
  the increase in mode share has been conducted for each scenario

- the percentage share across the three distance bands of the mode that
  is increased is preserved, i.e. if 10 band 0-2km then after increasing
  the cycling mode share by x we still have 10 mean mode split across
  all 28 cities (Antofagasta, Arica, Belo Horizonte, Bogota, Buenos
  Aires, Cali, Copiapo, Coquimbo / La Serena, Gran Valparaiso, Iquique /
  Alto Hospicio, Medellin, Mexico City, Montevideo, Osorno, Puerto
  Montt, San Antonio, Santiago, Sao Paulo, Temuco / Padre las Casas,
  Valdivia, Accra, Bangalore, Cape Town, Delhi, Vizag, Kisumu, Nairobi
  and Port Louis) as the baseline mode split for each mode.

- For each scenario we always convert the same in question,
  independently of the original overall mode share of that mode. I.e. if
  e.g. 1 5 in the car scenario 55 of the total trips.

- we preserve the proportion of trips in each distance band. E.g. if 20
  all trips are in distance band 0-2km, then in each scenario 20 trips
  are still in distance band 0-2km. These proportions are calculated for
  each city individually.

Example:

Assume that there are only two distance bands A and B and that 80
cycling trips lie in distance band A and the remaining 20 band B. Assume
that 60 band B. Assume we want to increase the cycling mode share by 5

Then, we need to convert 5 to cycling trips and 5 to cycling trips.

Overall, this leads to an increase of (5 of cycling trips, whilst
preserving the cycling mode shares of 80 band A and 20 distance band A
and 40 preserved.

The function performs the following steps:

- the overall mode shares for each of the cycle, car, bus and motorcycle
  modes across the three distance categories is defined

- from the trip data extract the trip information, calculate the total
  number of trips and find the proportion of trips in each distance
  category

- find the proportion of trips to be converted for each mode, scenario
  and distance category

- define the modes that are not changeable (at least initially) between
  the scenarios and divide the trip data into a set with the modes that
  can be changed and another set with the trips whose modes cannot be
  changed

- split the changeable trips and also all trips by distance band

- to create the scenarios loop through the scenarios, i.e. the 4
  different modes:

  - loop through each distance band:

    - find the changeable trips that are not of the mode to be increased

    - count the number of trips made by the mode to be increased

    - for the bus scenario we aim to increase all public transport
      trips, i.e. we find the changeable trips not made by bus or rail
      and we count the number of trips made by bus or rail

    - find the number of total trips that we would like to convert

    - if the number of trips that are changeable equals the number of
      trips to be converted, all changeable trips are converted to the
      mode in question

    - if there are more trips that are changeable than we want to
      change, sample the number of trips to change from the changeable
      trip ids

    - if there are more trips to be converted than there are changeable
      trips, then convert as many trips as possible and create a warning
      message

    - convert the required trips to the new mode in question

  - add all trips across the distance bands and add the non-changeable
    trips

  - update the bus_driver and car_driver trips

- create a list containing the trips for each scenario as elements
