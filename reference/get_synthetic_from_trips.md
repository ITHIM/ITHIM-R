# Generate baseline population from trip data

Sequence of functions to set up the baseline population, the synthetic
trips, and the scenarios.

## Usage

``` r
get_synthetic_from_trips()
```

## Value

data frame of all synthetic trips from all scenarios and the the
proportion of people with non non-occupational

mMET values for each age and sex category

## Details

This function performs the following steps:

- The columns from the TRIP_SET are put into the correct order

- multiply the trip distances, stage distances, and durations by the
  day_to_week scalar and then divide by 7 to get the distances and
  durations of an 'average' day of the week

- add bus_driver and truck trips if required
  ([`add_ghost_trips()`](https://usr110.github.io/ITHIM-R/reference/add_ghost_trips.md))

- add personal motorcycle trips if needed (call the appropriate
  function)

- add commercial motorcycle trips if required
  ([`add_ghost_trips()`](https://usr110.github.io/ITHIM-R/reference/add_ghost_trips.md))

- build the baseline population by creating a data set that contains the
  (non-zero) participant ids and demographic information from the trip
  data set and adds work and leisure MMET values by calling
  [`create_base_pop()`](https://usr110.github.io/ITHIM-R/reference/create_base_pop.md)
  (non travel entries in the trip data set are also removed)

- adds car driver trips if required
  ([`add_ghost_trips()`](https://usr110.github.io/ITHIM-R/reference/add_ghost_trips.md))

- call the
  [`ithim_setup_baseline_scenario()`](https://usr110.github.io/ITHIM-R/reference/ithim_setup_baseline_scenario.md)
  function to get the baseline data into the correct format for the
  creation of the different scenarios

- create the required scenarios by calling the appropriate function

- add walk to pt trips and combine the scenarios into one dataframe by
  calling the
  [`walk_to_pt_and_combine_scen()`](https://usr110.github.io/ITHIM-R/reference/walk_to_pt_and_combine_scen.md)
  function
