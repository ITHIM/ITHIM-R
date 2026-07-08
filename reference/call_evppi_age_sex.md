# EVPPI wrapper function for total population YLLs by sex and age

This function gets the ithim results into the correct format and calls
the
[`compute_evppi()`](https://usr110.github.io/ITHIM-R/reference/compute_evppi.md)
script to calculate the EVPPIs for all input parameters and required
outcomes and scenarios split by sex and age.

## Usage

``` r
call_evppi_age_sex(
  parameter_samples,
  outcome_voi_list,
  voi_complete_df,
  cities,
  NSCEN,
  NSAMPLES,
  SCEN_SHORT_NAME,
  scenario_names,
  output_version,
  level1,
  level2,
  level3
)
```

## Arguments

- parameter_samples:

  table containing all the input parameter variables for the different
  model runs for all cities

- outcome_voi_list:

  vector detailing the outcomes to be considered in the VoI analysis

- voi_complete_df:

  total yll outcome for all outcome age categories per city and scenario
  and disease combination, also combined city result (sum)

- cities:

  vector of cities

- NSCEN:

  number of scenarios (not incl. baseline)

- NSAMPLES:

  number of times the model was run for each city

- scenario_names:

  gives the names of the scenarios (incl baseline)

- output_version:

  output version number

- level1:

  list of outcomes included in level 1

- level2:

  list of outcomes included in level 2

- level3:

  list of outcomes included in level 3

## Value

evppi_agesex_df dataframe containing all EVPPI outcomes for all age and
sex categories and all cities

age_gender_cat vector with all age and sex categories

## Details

The function performs the following steps:

- create a vector containing the global parameters but not including the
  dose response quantiles as they are treated separately

- loop through the cities:

  - extract the RR values for each dose response function and outcome,
    calculate the RR for the different levels

  - extract the city specific parameters (excluding the CO2 and PM
    emission inventory parameters)

  - loop through all age and sex categories:

    - extract the outcomes of interest for each scenario using the
      outcome_voi_list

    - call the
      [`compute_evppi()`](https://usr110.github.io/ITHIM-R/reference/compute_evppi.md)
      function to calculate the expected values of partially perfect
      information (EVPPI) for all parameters and diseases of interest

  - if NSAMPLES \>= 1000 then also calculate the EVPPI values for the
    emission inventory parameters by looping through all age and sex
    categories

- tidy up results data and add parameter classification
