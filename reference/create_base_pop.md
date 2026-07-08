# Creates baseline population

Creates a baseline population by matching individuals in the trip set to
individuals in the physical activity (PA) dataset

## Usage

``` r
create_base_pop(raw_trip_set)
```

## Arguments

- raw_trip_set:

  data frame of raw trips taken, bus_driver, new motorcycle and truck
  trips have already been added

## Value

the baseline population and the trip set which has been pruned and also
the proportion of people with non non-occupational

mMET values for each age and sex category

## Details

The function performs the following steps:

- adds age category to trip and physical activity datasets by calling
  assign_age_groups.R

- To match people in trip data with people in the physical activity
  dataset:

  - create a baseline population by taking the unique participant ids
    together with age and gender information from the trip data (not
    including bus driver, truck, car driver and commercial motorcycle
    trips)

  - to assign non-occupational physical activity MMET values to this
    baseline population, the following steps are performed:

    - for each sex and age category, find the proportion of people with
      zero non-occupational MMET values and also find the list of people
      with non-zero MMET values

    - if BACKGROUND_PA_CONFIDENCE \< 1 when calling the value of
      information script, i.e. when input values are sampled from
      distributions, a beta distribution is built from which the
      proportion of people with zero work and leisure MMET values is
      sampled using the known proportion as mean of this distribution

    - sample with replacement from a vector with 0 MMET values and the
      vector non-zero MMET values (from the people having non-zero work
      and leisure MMET values) using the proportion of people with zero
      work and leisure MMET values and assign those sampled MMET values
      to the baseline population

- remove participants with trip or stage modes that are not in Vehicle
  inventory
