# Sampling routine for running ITHIM with uncertainty

Sets sampled parameters to the global environment and calls the ITHIM
routine

## Usage

``` r
ithim_uncertainty(ithim_object, seed = 1)
```

## Arguments

- ithim_object:

  list of necessary inputs, including parameters

- seed:

  which sample to take

## Value

list of ITHIM outcomes

## Details

This function works by performing the following steps:

- extract the ithim_object list entries

- extract the sampled parameters which are stored in the ithim_object

- call
  [`set_vehicle_inventory()`](https://usr110.github.io/ITHIM-R/reference/set_vehicle_inventory.md)
  to update emissions if any emission parameters have been sampled from
  a distribution

- call
  [`get_synthetic_from_trips()`](https://usr110.github.io/ITHIM-R/reference/get_synthetic_from_trips.md)
  to update the synthetic trips and baseline population if any relevant
  input parameters have been sampled from a distribution

- call
  [`get_all_distances()`](https://usr110.github.io/ITHIM-R/reference/get_all_distances.md)
  to recalculate the distances if any of the relevant input parameters
  have been sampled from a distribution

- run ITHIM-Global model by calling
  [`ithim_calculation_sequence()`](https://usr110.github.io/ITHIM-R/reference/ithim_calculation_sequence.md)
