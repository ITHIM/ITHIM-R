# Combine relative risks from air pollution and physical activity

Combine relative risks (RR) from air pollution (AP) and physical
activity (PA) through multiplication for diseases affected by both AP
and PA

## Usage

``` r
combined_rr_ap_pa(ind_pa, ind_ap, conf_int = FALSE)
```

## Arguments

- ind_pa:

  data frame of individual RRs for diseases affected by PA

- ind_ap:

  data frame of individual RRs for diseases affected by AP

- conf_int:

  logic: whether to include confidence interval from dose response
  relationships or not

## Value

dataframe giving the RR risk for AP, PA and combined AP and PA exposure
levels for every person in the baseline population and for each scenario

## Details

This function performs the following steps:

- join the ap and pa relative risk datasets

- loop through all disease outcomes that are affected by both PA and AP:

  - for each scenario multiply the relative risks for PA and AP and
    store in a new column

  - if confidence intervals are required, multiply the upper and lower
    RR for AP and PA respectively wherever possible, otherwise use the
    given median RR values instead
