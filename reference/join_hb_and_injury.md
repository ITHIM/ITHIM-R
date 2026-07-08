# Join disease health burden and injury data

Join the two data frames for health burden: that from disease, and that
from road-traffic injury

## Usage

``` r
join_hb_and_injury(ind_ap_pa, inj)
```

## Arguments

- ind_ap_pa:

  list (deaths, YLLs) of data frames of all demographic groups' burdens
  of diseases

- inj:

  list (deaths, YLLs) of data frames of all demographic groups' burdens
  for road-traffic injury

## Value

list of dataframes: one for deaths per cause per demographic group, and
likewise for YLLs

## Details

This function performs the following steps:

- extract the yll and deaths data from the AP and PA pathways

- extract the yll and deaths data from the injury data

- create one dataframe for yll and one for deaths containing all the AP,
  PA and injury data
