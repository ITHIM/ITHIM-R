# Function to calculate the EVPPI values for given input parameters and outcome values

This function calculates the expected value of partially perfect
information, i.e. the percentage by which we could reduce the variance
of the final outcome if we knew this one parameter (or several
interdependent parameters) perfectly.

## Usage

``` r
compute_evppi(
  p,
  global_para,
  city_para,
  dr_rr,
  outcome_voi_list,
  SCEN_SHORT_NAME,
  city_outcomes,
  nsamples,
  individual_para = TRUE
)
```

## Arguments

- p:

  input parameter index

- global_para:

  list of global input parameters that are the same across all cities

- city_para:

  list of city specific input parameters

- dr_rr:

  dose response RR values for each scenario

- outcome_voi_list:

  list containing the outcomes of interest

- SCEN_SHORT_NAME:

  scenario names including baseline

- city_outcomes:

  list of outcomes for a specific city

- nsamples:

  number of samples

- individual_para:

  whether each parameter is to be considered individually or not

## Value

list of of EVPPI standard variation values for specific city

## Details

The function performs the following steps:

- create empty vector to be filled with EVPPI values

- depending on the input parameter considered (depends on value of p),
  set the paramter(s) for which the evppi value is to be calculated Note
  that the calculations are slightly different for the dose response
  input parameters, denoted by the flag call_dr_rr

- if the input parameter are not related to any dose response function
  loop through the various outcomes :

  - calculate the EVPPI value for each outcome using the evppivar()
    function from https://github.com/chjackson/voi

  - calculate the percentage of standard deviation in the outcome we
    could reduce were we to know the input parameter / parameters
    perfectly.

- if the input parameter is related to the dose response functions:

  - loop through the scenarios and different outcomes as the evppi
    values are calculated by matching the dose response functions to the
    respective outcomes

  - calculate the EVPPI value for each outcome and scenario using the
    evppivar() function from https://github.com/chjackson/voi

  - calculate the percentage of standard deviation in the outcome we
    could reduce were we to know the input parameter / parameters
    perfectly.
