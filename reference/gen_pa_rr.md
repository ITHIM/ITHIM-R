# Get relative risk for diseases given mMETs

Computes the relative risks (RR) for individuals in the baseline
population for each disease given their mMETs (PA exposure)

## Usage

``` r
gen_pa_rr(mmets_pp, conf_int = F)
```

## Arguments

- mmets_pp:

  individual mMETs

## Value

data frame of relative risks per person per disease

## Details

This function performs the following steps:

- loop through all diseases that are related to physical activity
  levels:

  - set the quantile of the value of the dose response curves to be
    extracted. If running in constant mode, the quantile is usually set
    to 0.5, i.e. to the median of the dose response curves. If running
    in sample mode, then the quantile can be set to be sampled from a
    distribution in the input parameters.

  - create one vector containing all the mMET values for all scenarios

  - assign the relative risk for the given disease and quantile to the
    given mMET values for all people in the baseline population for all
    scenarios by calling the drpa::dose_response function

  - extract the RR for each scenario from the vector containing all RR
    for all scenarios

  - if confidence intervals are required, also extract the RR upper and
    lower confidence values for each scenario
