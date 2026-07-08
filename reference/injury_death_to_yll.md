# Map injury death burden to YLL (years of life lost) burden

Calculate the YLL burden from the death burden of injury based on the
ratio in the GBD data.

## Usage

``` r
injury_death_to_yll(injuries)
```

## Arguments

- injuries:

  data frame of injury deaths by age and sex category for each scenario
  incl. baseline

## Value

list of injury deaths and YLLs (given as differences to reference
scenario) plus the values in reference scenario

## Details

The function performs the following steps:

- join the estimated injury deaths with the global burden of disease
  (GBD) injury data by age and sex

- multiply the estimated injury deaths by the yll to injury death ratio
  in the GBD data to predict YLL from the estimated injury deaths

- extract and create matrices for deaths and ylls with one column for
  each scenario

- create dataframe A with ylls and deaths of reference scenario

- create dataframe B showing the differences in deaths and yll for each
  non-reference scenario to the reference scenario

- if confidence intervals are required:

  - create dataframe with ylls and deaths of reference scenario using
    both the lower and upper relative risk boundary values

  - create dataframe showing the differences in deaths and yll for each
    non-reference scenario to the reference scenario using both the
    lower and upper relative risk boundary values

  - add the confidence upper and lower interval boundary values to the
    two output dataframes A and B
