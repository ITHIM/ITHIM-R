# Get distances and parameterise Poisson regression model for injuries

Computes exposures (distances) by mode to parameterise the injury
regression model, which is computed as a Poisson model and which is used
in the ITHIM-Global model to predict injury fatalities at a later stage

## Usage

``` r
distances_for_injury_function(journeys, dist)
```

## Arguments

- journeys:

  data frame with total distance (by total population) for each age and
  sex category and for each scenario

- dist:

  table of (total population) distances per mode

## Value

true_distances (mode distances by age and sex with all walking modes and
all car modes combined and bus drivers added where relevant),

injuries_list (list of all strike, casualty, age, sex and mode distance
combinations for baseline and all scenarios),

reg_model (parameterised Poisson regression model),

injuries_for_model (baseline data containing injury counts for all
casualty and strike mode combinations with associated distance data)

## Details

The function uses distance data and the individual injury fatality data
to perform the following steps to parameterise the Poisson injury
regression model:

- taxi, shared_taxi and auto_rickshaw distances are multiplied by 2 to
  add drivers (note if there are multiple passengers in a vehicle apart
  then this will overestimate these vehicle distances but taxis also
  spend a significant amount of time driving around without any
  passenger which is not captured in the travel surveys.)

- stage modes are aggregated such that all walk related stages (walk and
  walk to pt) are of the same mode (walk), similarly for all car related
  journeys

- bus drivers are added to bus journeys (where relevant) to accurately
  represent all people on a bus

- Takes Baseline injury tables, split into who-hit whom (whw) and
  no-other-vehicle (nov) parts, and adds total population distances for
  each strike and casualty mode (add_distance_columns.R). Distances are
  added by age and gender category if there exists such information for
  the injury counts (injuries_for_model dataframe). If there exists a
  fatality for some casualty and strike mode and age and sex category
  but no mode distance for this age and sex category, then fatalities
  and distances are aggregated by strike and casualty mode. If, after
  the aggregation there still exist fatalities for which either casualty
  or strike mode distance are missing, then these fatalities are removed
  as we cannot predict injury counts on zero distances. However, this
  should not happen as we should have total distances for all modes
  (possibly inferred from other modes) that appear in the injury data. -
  This data is used to parameterise the Poisson injury model.

- A new list (injuries_list) is created containing all strike and
  casualty mode and age and sex combinations together with strike and
  casualty mode distances
  ([`add_distance_columns()`](https://usr110.github.io/ITHIM-R/reference/add_distance_columns.md))
  for the baseline and all scenarios. For the whw model, any strike mode
  and casualty pairs where strike mode equals casualty mode are removed
  as fatalities for these combinations have already been added to the
  nov matrix. Combinations which do not have a non-zero strike or
  casualty mode distance are also removed. This list will later be used
  in the
  [`injuries_function_2()`](https://usr110.github.io/ITHIM-R/reference/injuries_function_2.md)
  function to predict fatality counts using the Poisson injury
  regression model.

- The casualty and strike mode exponents used to account for the safety
  in number effect are added to both the injuries_for_model and
  injuries_list.

- The best possible regression model is being built using Baseline
  injury counts and distances (injuries_for_model) such that the
  standard errors are small wherever possible. Strike and casualty mode
  pairs where cas mode = strike mode are removed if they still exist
  which they should not as they should have been removed by the
  ithim_load_data.R function. Two different forms for whw and nov
  matrices are defined, taking into account age and sex information
  where it exists. The standard errors of the newly built regression
  models are checked and if they are too large and if the data has not
  been aggregated by age and sex yet, then the data is aggregated and a
  new Poisson regression model is build. If the standard errors are
  still large after this aggregation, then a message is printed to the
  screen warning that the standard errors are large.
