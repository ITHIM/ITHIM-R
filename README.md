---
output:
  html_document: default
  pdf_document: default
editor_options: 
  markdown: 
    wrap: 72
---

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/117988409.svg)](https://zenodo.org/badge/latestdoi/117988409)

<!-- badges: end -->

# ITHIM-R

Development of the ITHIM-R, also known as ITHIM version 3.0. Started in
January 2018.

ITHIM it is a city level model for estimating the change in health
impacts of passenger travel scenarios through physical activity, air
pollution, and traffic injuries.

This document aims to be a comprehensive record of the calculations in
the ITHIM pipeline, specifically the ITHIM- R package. Some details here
(and the default inputs to the functions) are specific to the Accra
version of that model, as Accra has been the setting for construction of
the prototype.

### Citation

To cite package 'ithimr' in publications use:

> Rob Johnson and Ali Abbas (2021). ithimr: Integrated Transport and
> Health Impact Model. R package version 0.1.1.

A BibTeX entry for LaTeX users is

> \@Manual{, title = {ithimr: Integrated Transport and Health Impact
> Model}, author = {Rob Johnson and Ali Abbas}, year = {2021}, note = {R
> package version 0.1.1}, }

### Outline

![Model Layout](images/BigPicture_v3-7.svg)

### Data inputs

ITHIM-R requires 5 user defined input files in csv format, saved in a
directory of the city's name. See
[inst/ext/local/bogota](inst/extdata/local/bogota) for example files.
There are also numerous assumptions that you can parameterize in the
model.

#### File inputs

This section talks about all the files (datasets) required to setup and
run the model. There are two subsections, which are:

-   case study specific datasets

-   global datasets - such as dose-response relationship for Air
    Pollution module etc

### Case study specifics

This section covers file inputs (specific to a city) required to run the
model.

-   **Travel survey** [example trips
    dataset](inst/extdata/local/bogota/trips_bogota.csv) . A table of
    all trips taken by a group of people on a given day. It also
    includes people who take no trips.

    -   One row per trip (or stage of trip)
    -   Minimal columns: `participant_id`, `age`, `sex`, `trip_mode`,
        `trip_duration` (or `trip_distance`)
    -   Other columns: `stage_mode`, `stage_duration` (or
        `stage_distance`)

-   **Injury events** [(example injuries
    dataset)](inst/extdata/local/bogota/injuries_bogota.csv). A table of
    recorded road-traffic injury (fatality) events in a city in one or
    more years.

    -   One row per event
    -   Minimal columns: **victim mode** (`cas_mode`) and **strike
        mode** (`strike_mode`)
    -   Other columns: `year`, `cas_age`, `cas_gender`, `weight` (e.g.
        multiple years combined)

-   **Disease burden data** [(example burden
    dataset)](inst/extdata/local/bogota/gbd_bogota.csv).

    -   One row per disease/metric/age/gender combination
    -   Minimal rows: `Measure` (death/YLL); `sex_name` (Male/Female);
        `age_name` ('x to y'); `cause_name` (disease names); `val`
        (value of burden); `population` (number of people `val`
        corresponds to, e.g. population of country)

-   **Population of city** [(example population
    dataset)](inst/extdata/local/bogota/population_bogota.csv)

-   in order to scale the burden in Disease burden data to the city
    under study

    -   One row per demographic group
    -   Columns: `sex`, `age`, `population`
    -   `age` column should share boundaries with `age_name` in Disease
        burden data, but can be more aggregated

-   **Physical activity survey** [(example physical activity
    dataset)](inst/extdata/local/bogota/pa_bogota.csv)

    -   One row per person
    -   Columns: `sex`, `age`, `ltpa_marg_met` (total non-occupational
        PA in a week)

### Global file inputs

In order to setup the model, we need a fixed list of tables/datasets,
such as:

-   [**Disease interaction table**](inst/extdata/global/dose_response/disease_outcomes_lookup.csv) . A table with
    a list of diseases/causes for a specific module such as
    `Air Pollution` and `Physical Activity` and also the interaction
    between them
-   **Air Pollution Exposure Response Functions (ERFs)** exposure
    response relationships of air pollution (PM2.5) and its impact on
    health via diseases/causes. We have collected/cleaned datasets from
    published studies for this.
-   **Physical Activity Dose-Response Functions (DRFs)** dose response
    relationships of physical activity and its impact on health via
    diseases/causes. Similar to AP, this too comes from published
    studies
