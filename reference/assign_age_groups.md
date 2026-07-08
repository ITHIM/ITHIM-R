# Assign age groups to individuals

Prunes the dataset given max and min ages, i.e. removes entries of
people with ages outside the scope of the model and assigns age group
labels given age

## Usage

``` r
assign_age_groups(
  dataset,
  age_category = AGE_CATEGORY,
  age_lower_bounds = AGE_LOWER_BOUNDS,
  max_age = MAX_AGE,
  min_age = AGE_LOWER_BOUNDS[1],
  age_label = "age"
)
```

## Arguments

- dataset:

  data frame to which age categories are to be added and ages outside
  model scope are removed

- age_category:

  vector of strings giving age categories

- age_lower_bounds:

  lower boundaries of age categories

- max_age:

  maximum age for model

- min_age:

  minimum age for model

- age_label:

  string label for age column

## Value

edited data frame

## Details

The function contains the following steps:

- remove any dataset entries with ages above or below the max and min
  ages or with missing age information

- add age categories
