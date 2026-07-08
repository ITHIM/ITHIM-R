# Additional trips for trip data set

Creates a data frame with given characteristics to be added to an
existing trip data set.

## Usage

``` r
add_trips(
  trip_ids = 0,
  new_mode = "pedestrian",
  distance = 1,
  participant_id = 0,
  age = 20,
  sex = "male",
  nTrips = 3,
  speed = 4.8
)
```

## Arguments

- trip_ids:

  ids for new trips

- new_mode:

  mode for new trips

- distance:

  distances to sample from

- participant_id:

  participant id for new trips

- age:

  age for participant

- sex:

  sex for participant

- nTrips:

  number of trips for each participant

- speed:

  speed for new trips

## Value

data frame of trips

## Details

This function is used to create new trips with certain characteristics
that can be added to an existing trip data set. The input distance given
is the upper limit of trip distances added and the function takes the
number of trips per person (nTrips) random samples between 1 and the
input distance given. Age and sex of each participant are also sampled
from a range of input age and sex values.

The function performs the following steps:

- create nTrips new trips sampling from distances, ages and sexes
