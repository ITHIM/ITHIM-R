# Get ITHIM-results into correct format for VoI analysis

This function extracts the relevant information from the
multi_city_ithim object and gets the results into the correct format for
further analysis.

## Usage

``` r
extract_data_for_voi(
  NSCEN,
  NSAMPLES,
  SCEN_SHORT_NAME,
  cities,
  output_version,
  level1,
  level2,
  level3
)
```

## Arguments

- NSCEN:

  number of scenarios (not incl. baseline)

- NSAMPLES:

  number of model runs per city

- SCEN_SHORT_NAME:

  names of the scenarios (incl. baseline)

- cities:

  list of cities for which the model was run

- output_version:

  the output version of the model run

- level1:

  list of diseases considered in level 1

- level2:

  list of diseases considered in level 2

- level3:

  list of diseases considered in level 3

- multi_city_ithim:

  list containing the ithim model information including results for the
  various model runs

## Value

ithim_results list with the following objects:

voi_complete - dataframe for all cities with all total YLL outcomes for
all model runs, age and sex categories and disease and scenario
combinations - combined AP and PA pathway

voi_complete_summary - summary statistics of YLLs for combined AP and PA
pathway

voi_complete_100k - dataframe for all cities for all YLL outcomes per
100k - combined AP and PA pathway

voi_commplete_100k_summary - summary statistics per of YLL per 100k for
combined AP and PA pathway

voi_complete_pathway - dataframe for all cities with all total YLL
outcomes for all model runs, age and sex categories and disease and
scenario combinations - separate AP and PA pathways

voi_complete_summary_pathway - summary statistics of YLLs for separate
AP and PA pathways

voi_complete_100k_pathway - dataframe for all cities for all YLL
outcomes per 100k - separate AP and PA pathways

voi_commplete_100k_summary_pathway - summary statistics per of YLL per
100k for separate AP and PA pathways

## Details

The function performs the following steps:

- by looping through the cities:

  - extract the population statistics

  - create one dataframe for all cities with all outcomes for all model
    runs, age and sex groups and disease and scenario combinations for
    the combined AP and PA results

  - create one dataframe for all cities with all outcomes for all model
    runs, age and sex groups and disease and scenario combinations for
    the separate AP and PA results

- compute statistics (mean, 2.5th and 97.5th percentiles, standard
  deviation) for total YLLs lost for the combined AP and PA results

- compute statistics (mean, 2.5th and 97.5th percentiles, standard
  deviation) for YLLs lost per 100k for the combined AP and PA results

- compute statistics (mean, 2.5th and 97.5th percentiles, standard
  deviation) for total YLLs lost for the separate AP and PA pathways

- compute statistics (mean, 2.5th and 97.5th percentiles, standard
  deviation) for YLLs lost per 100k for the separate AP and PA pathways
