# Cascade of computations that form ITHIM-Global

Ordered set of computations as part of ITHIM-Global that calculates the
required output parameters

## Usage

``` r
ithim_calculation_sequence(ithim_object, seed = 1)
```

## Arguments

- ithim_object:

  name of disease

- seed:

  random seed

## Value

ithim_object - list of items making up the ithim result

## Details

This function performs the following steps:

1.  extract all lists and variables from the ithim_object list

2.  air pollution pathway:

    - calculate the PM2.5 exposure for each person in the baseline
      population and PM2.5 emissions for each mode and scenario
      ([`scenario_pm_calculations()`](https://ithim.github.io/ITHIM-R/reference/scenario_pm_calculations.md))

    - calculate the CO2 emissions for each mode and scenario
      ([`scenario_co2_calculations()`](https://ithim.github.io/ITHIM-R/reference/scenario_co2_calculations.md))

    - assign relative risk to each person in the baseline population for
      each disease related to PM pollution and each scenario based on
      the individual PM exposure levels
      ([`gen_ap_rr()`](https://ithim.github.io/ITHIM-R/reference/gen_ap_rr.md))

3.  physical activity pathway:

    - calculate total mMETs for each person in the baseline population
      ([`total_mmet()`](https://ithim.github.io/ITHIM-R/reference/total_mmet.md))

    - assign relative risk to each person in the baseline population for
      each disease related to physical activity levels and each scenario
      based on the individual mMET values
      ([`gen_pa_rr()`](https://ithim.github.io/ITHIM-R/reference/gen_pa_rr.md))

4.  physical activity and air pollution combined:

    - combine the PA and AP datasets by joining the two datasets. For
      disease affected by both PA and AP calculate the joined relative
      risk by multiplying the PA and AP relative risks
      ([`combined_rr_ap_pa()`](https://ithim.github.io/ITHIM-R/reference/combined_rr_ap_pa.md))

    - calculate the health burden (Yll and deaths) for each disease and
      age and sex category
      ([`health_burden()`](https://ithim.github.io/ITHIM-R/reference/health_burden.md)):

      - calculate the health burden (Yll and deaths) for each disease
        and age and sex category. Combine the AP and PA pathways for
        diseases affected by both AP and PA

      - calculate the health burden for both the AP and PA pathways
        separately

5.  injury pathway:

    - estimate the injury deaths for the baseline and each scenario by
      age and sex category, also estimate the total injury deaths counts
      for the who-hit-whom and no-other-vehicle matrices by casualty
      (and strike) mode again for the baseline and each scenario
      (`injuries_function2()`)

      - if running in constant mode include upper and lower confidence
        intervals

    - calculate the years of life lost from the injury deaths
      ([`injury_death_to_yll()`](https://ithim.github.io/ITHIM-R/reference/injury_death_to_yll.md))

6.  combine all pathways using the outputs from 3. and 4.:

    - combine the AP, PA and injury health burden data for ylls and
      deaths
      ([`join_hb_and_injury()`](https://ithim.github.io/ITHIM-R/reference/join_hb_and_injury.md))
      for all diseases, injuries and scenarios
