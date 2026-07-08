# Calculate total AP exposure per person

Calculate total AP exposure per person based on population and personal
travel

## Usage

``` r
scenario_pm_calculations(dist, trip_scen_sets)
```

## Arguments

- dist:

  total distance travelled by mode by the population for all scenarios

- trip_scen_sets:

  trips data frame of all trips from all scenarios

## Value

background PM concentration for baseline and all scenarios

total AP exposure per person in the baseline population (for baseline
and scenarios)

## Details

This function performs the following steps:

- the exposure factor rates by activity are defined - these parameters
  are fixed

- calculate pm concentration not related to transport

- calculate PM emission factors for each mode by dividing total
  emissions by distances travelled

- calculate PM emissions for each mode in each scenario by multiplying
  the scenario distance by the emission factors

- for modes without any assigned distance, use the PM emissions from the
  VEHICLE_INVENTORY instead

- calculate the total PM concentrations for each scenario

- add exposure factors to the trip set by stage mode

- add total scenario PM concentrations to the trip set

- calculate ventilation rate for each stage taking into account
  demographic characteristics and exposure factors

- calculate the inhaled air and total PM (in micro grams) in the trip
  set

- calculate the amount of time per day spent in sleep, moderate and
  vigorous activities

- add total time spent travelling by each participant to the trip set

- calculate ventilation rate for sleep, moderate and vigorous activities

- for each participant in the baseline population (with travel
  component), calculate the total air inhaled, the total PM inhaled and
  the total PM concentration inhaled for each scenario

- assign all participants in the baseline population without travel
  component, the baseline or scenario PM concentrations

- join all people with and without travel in the baseline population
