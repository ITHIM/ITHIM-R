# Calculate total mMETs per person

Calculate total mMETs per person in the baseline population based on
non-travel PA and active travel for each scenario

## Usage

``` r
total_mmet(trip_scen_sets)
```

## Arguments

- trip_scen_sets:

  data frame of all trips from all scenarios

## Value

mmets - total mMETs per week per person in each scenario

## Details

This function performs the following steps:

- extract all people from the trip set with an active travel (walk or
  cycle) stage mode (non-ghost trips only)

- calculate the weekly time spent on active travel

- for each scenario:

  - scale the non-travel mMET value for the people in the baseline
    population by the BACKGROUND_PA_SCALAR to adjust for any biases in
    the PA data

  - calculate the total cycling and walking mMET values for each
    relevant person in the trip set and scale up to a week

  - add the active travel mMET to the non-travel mMET values for each
    person in the baseline population

- create one dataframe with total MMET for all people in the baseline
  population for all scenarios
