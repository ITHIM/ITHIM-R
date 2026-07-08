# Combine health and potential impact fraction (PIF)

Applies PIF calculated from relative risks (RRs) to the current observed
health burden from the Global Burden of Disease data to generate the
scenario health burdens

## Usage

``` r
combine_health_and_pif(pif_values, hc = DISEASE_BURDEN)
```

## Arguments

- pif_values:

  vector of values of PIFs for all age and sex categories

- hc:

  data frame of current burden of disease

## Value

estimated scenario burden of disease for all age and sex categories

## Details

This function performs the following steps:

- the current observed health burden for a particular disease is
  multiplied by the PIF, i.e the change in fraction in disease expected
  for the current scenario compared with the reference scenario
