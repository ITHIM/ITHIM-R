# Add walk to public transport stages

Adds a short walk stage to any public transport (PT) trip if required.

## Usage

``` r
walk_to_pt_and_combine_scen(SYNTHETIC_TRIPS)
```

## Arguments

- trip_set:

  list of data frames, trips from all scenarios

## Value

data frame, all trips from all scenarios

## Details

This function performs the following steps:

- create a list containing the dataframes of the synthetic trips for
  each scenario

- if ADD_WALK_TO_PT_TRIPS == T, i.e if additional 'walk to pt' stages
  are to be added:

  - filter out all trips with a public transport stage mode

  - divide public transport trips into those with and without a 'walk to
    pt' stage

  - add a 'walk to pt' stage to those public transport trips without a
    walking stage
    ([`add_walk_trips()`](https://usr110.github.io/ITHIM-R/reference/add_walk_trips.md))

- combine all trips from all scenarios into one dataframe

- scale the stage distances and durations by calling the
  scale_trip_distances.R function
