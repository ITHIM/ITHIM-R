# Get distances and duration summaries by mode

Summaries of total distances and durations spent travelling per mode and
per scenario, for the baseline population

## Usage

``` r
dist_dur_tbls(trip_scen_sets)
```

## Arguments

- trip_scen_sets:

  list of synthetic trip sets for each scenario including the baseline

## Value

list of table of (total) distances and durations per mode per scenario

## Details

The function performs the following steps:

- loop through the scenarios (incl Baseline):

  - using the trip data, sum across the distances by stage mode to get
    total distance by mode for each scenario for the baseline population

  - sum across the duration by stage mode to get total duration by mode
    for each scenario

  - if 'walk_to_pt' stages exist, add them to the pedestrian stages for
    both duration and distance

- create one dataframe containing the total distances by mode for each
  scenario

- create one dataframe containing the total duration by mode for each
  scenario

- remove any 'walk_to_pt' stages as they have been added to the
  pedestrian stages

- update the bus and car driver distances and duration in the scenarios
  using the ratio of bus/car to bus_driver/car_driver in the baseline
  scenario (this is redundant for the GLOBAL, BOGOTA, AFRICA_INDIA and
  LATAM scenario definitions as bus and car driver distances are already
  updated during the scenario creation)
