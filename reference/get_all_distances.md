# Find population distances by mode for entire population

Function to find the distances travelled by age, sex, scenario and mode
for the entire population rather than just the baseline population.

## Usage

``` r
get_all_distances(ithim_object)
```

## Arguments

- ithim_object:

  list containing city specific information including the baseline trip
  set

## Value

ithim_object again, with additional total population distance for each
mode and scenario, distances for injury pathway plus parameterised
Poisson injury regression model

## Details

This function performs the following steps:

- generate distance and duration matrices by age, sex, mode and scenario
  from the ithim_object\$trip_scen_sets for the baseline population by
  calling the
  [`dist_dur_tbls()`](https://usr110.github.io/ITHIM-R/reference/dist_dur_tbls.md)
  function

- find the total mode distances for each scenario and scale this up to
  the distance travelled by the entire population by using the
  demographic information for the city

- in order to scale the distances by age, sex, mode and scenario to the
  entire population, the proportion of the distances travelled by each
  age, sex, mode and scenario combination in the baseline population to
  total distances by mode and scenario in the baseline population is
  found. The total population distances by mode are then multiplied by
  these proportions to find the total population distances travelled by
  each mode and age and sex category.

- the
  [`distances_for_injury_function()`](https://usr110.github.io/ITHIM-R/reference/distances_for_injury_function.md)
  function is called which creates a list inj_distances that is added to
  ithim_object containing the following matrices:

  - true_distances (population mode distances by age and sex with all
    walking modes and all car modes combined and bus drivers added where
    relevant)

  - injuries_list (list of all strike, casualty, age, sex and mode
    distance combinations for baseline and all scenarios, used to
    predict fatalities later in the model run)

  - reg_model (parameterised Poisson injury regression model)

  - injuries_for_model (baseline data containing injury counts for all
    casualty and strike mode combinations with associated distance data)
