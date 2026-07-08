# Compute health burden

Compute health burden for population in scenarios given relative risks
for diseases

## Usage

``` r
health_burden(ind_ap_pa, conf_int = F, combined_AP_PA = T)
```

## Arguments

- ind_ap_pa:

  dataframe of all individuals' relative risks for diseases

- conf_int=F:

  logic: whether to include confidence interval from dose response
  relationships or not

- combined_AP_PA=T:

  logic: whether to combine the two exposure pathways (AP and PA) or to
  compute them independently

## Value

list of dataframes: one for deaths per disease per demographic group and
scenario, and likewise for YLLs

## Details

This function performs the following steps:

- get the demographic and disease burden (subset of Global Burden of
  Disease dataset) data into the correct formats and join the two
  datasets

- scale the burden data by the CHRONIC_DISEASE_SCALAR to account for
  bias in the data

- split the above dataframe into two dataframes, one for deaths and one
  for years of life lost (YLLs)

- add a demographic index (by age and sex category) to the dataframe
  containing the individual relative risk for different diseases

- set the reference and the other scenarios

- iterate over all disease outcomes:

  - define column names

  - loop either over 1 or 2 pathways depending on whether both PA and AP
    are affecting the disease and whether the AP and PA pathways are
    combined or not:

    - extract the relevant burden of disease for the specific scenario
      both for YLLs and deaths

    - find the sum of the relative risks (RR) for the specific disease
      for each age and sex category for the reference scenario

    - loop through the non-reference scenarios:

      - define column names

      - find the sum of the relative risks (RR) for the specific disease
        for each age and sex category for the non-reference scenario

      - calculate the PIF (potential impact fraction), i.e the
        proportional change in the sum of relative risks between the
        reference and the non-reference scenario for each age and sex
        category

      - calculate the health burden (deaths and ylls) for the
        non-reference scenario compared to the reference scenario by
        multiplying the current burden of disease by the PIF
        (combine_health_and_pif.R)

    - if confidence intervals are required, loop through the upper and
      lower confidence interval limits and calculate the health burden
      for deaths and YLLs using the upper and lower confidence relative
      risks. If no upper and lower relative risk values exist, use the
      median value instead
