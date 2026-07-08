# Collate all vehicle information

Puts all vehicle information in one place including speeds and emission
factors. Writes to global environment.

## Usage

``` r
set_vehicle_inventory()
```

## Details

This function performs the following operations:

- Based on the city specific (or default) mode speeds, the vehicle
  inventory is initialised

- PM emissions are added from the city specific (or default) PM
  inventory

- heavy_truck mode is added for which there is no distance but it this
  mode is used in the air pollution modules

- other mode is added if it does not already exist in the travel survey
  (and hence in the mode speeds)

- CO2 emissions are added from the city specific (or default) CO2
  inventory

- if car_driver exists, the car_driver emissions are set to the PM and
  CO2 car emissions

- VEHICLE_INVENTORY is set as a global variable
