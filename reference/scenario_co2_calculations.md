# Calculate total CO2 exposure per mode and scenario

Calculate total CO2 exposure for each mode and for each scenario based
on the CO2 emissions inventory

## Usage

``` r
scenario_co2_calculations(dist)
```

## Arguments

- dist:

  data frame of population travel from all scenarios

## Value

total CO2 exposure per mode

## Details

This function performs the following steps:

- calculate emission factors for each mode by dividing total emissions
  by distances travelled

- calculate CO2 emissions for each mode in each scenario by multiplying
  the scenario distance times the emission factors

- for modes without any assigned distance, use the CO2 emissions from
  the VEHICLE_INVENTORY instead
