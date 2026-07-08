# Scale trip distances

Applies mode-specific distance scalars to all trips

## Usage

``` r
scale_trip_distances(trips)
```

## Arguments

- trips:

  data frame, all trips from all scenarios

## Value

data frame, all trips from all scenarios with scaled distances

## Details

The function is used to multiply all trip stages belonging to a certain
mode by a city specific scalar. Note that walk to pt stages are counted
as public transport stages and are multiplied by the DISTANCE_SCALAR_PT

The function performs the following steps:

- define all car and public transport modes

- multiply all stage distances and stage durations by the corresponding
  distance scalars
