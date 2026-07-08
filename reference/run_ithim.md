# Wrapper for running ITHIM

Switch to either calculate the health burden using the constant input
parameter values or to sample from distributions first

## Usage

``` r
run_ithim(ithim_object, seed = 1)
```

## Arguments

- ithim_object:

  list of input data needed to calculate the health burden

- seed:

  random seed

## Value

ithim_object list of items giving the input data and output results

## Details

This function works by creating a switch to run the computation by
calling
[`ithim_calculation_sequence()`](https://usr110.github.io/ITHIM-R/reference/ithim_calculation_sequence.md)
directly, or to divert to the sampling case
[`ithim_uncertainty()`](https://usr110.github.io/ITHIM-R/reference/ithim_uncertainty.md)
which first extracts the sampled parameters and then calls the
[`ithim_calculation_sequence()`](https://usr110.github.io/ITHIM-R/reference/ithim_calculation_sequence.md).
