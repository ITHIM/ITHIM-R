# Predict injuries

Predict injuries for baseline and scenarios based on Poisson regression
model fitted on baseline fatality counts and distances

## Usage

``` r
injuries_function_2(
  true_distances,
  injuries_list,
  reg_model,
  constant_mode = F
)
```

## Arguments

- true_distances:

  data frame containing population distances for each scenario

- injuries_list:

  list of dataframes set up with scenario specific information to supply
  to regression model for prediction

- reg_model:

  Poisson injury regression model

- constant_mode:

  whether or not we are in constant (vs sampling) mode

## Value

injuries2 - dataframe containing predicted fatality counts for each
casualty mode by age and sex and for each scenario, plus confidence
interval limits for constant mode

whw_temp - list containing the fatality predictions for each casualty
and strike mode pair split into whw and nov matrices for each scenario.
Upper and lower confidence interval predictions are also included for
the constant mode

## Details

This function uses the Poisson regression model built in the
[`distances_for_injury_function()`](https://usr110.github.io/ITHIM-R/reference/distances_for_injury_function.md)
to predict fatality counts for the Baseline and all the scenarios. It
performs the following steps:

- create an injuries data frame containing all the distances travelled
  by mode, age, sex and scenario

- predict the fatalities for each strike and casualty mode combination,
  age and sex category and each scenario (incl Baseline). If the sample
  mode is set to 'constant' (and not 'sample'), we also predict upper
  and lower confidence interval boundaries

- if running in constant mode, create a whw_temp list containing the
  total predicted fatality counts for each casualty and strike mode pair
  for each scenario split into whw and nov matrices and also give the
  upper and lower confidence interval limit predictions. Also create a
  combined outcome table where NOV fatalities are added as casualty mode
  equals strike mode fatalities.

- create an injuries2 data frame containing the total predicted fatality
  counts for each casualty mode by age and sex for each scenario. This
  dataframe also contains total death per age and sex category and, for
  the constant mode the upper and lower total death predictions of the
  confidence interval.
