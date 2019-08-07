---
output:
  html_document: default
  pdf_document: default
---
# ITHIM-R

Development of the ITHIM-R, also known as ITHIM version 3.0. Started in January 2018.

This document aims to be a comprehensive record of the calculations in the ITHIM pipeline, specifically the ITHIM- R package. Some details here are specific to the Accra version of that model, as Accra has been the setting for construction of the prototype.

### Outline
In this document, in general, lower-case letters correspond to indices, or dimensions, of objects, and take one of a set of possible values, detailed in Table 1 (Convert from LATEX).

The set of fixed input data items are denoted by capital letters, and variable parameters are denoted by greek letters. These are tabulated in Table 2 (Convert from LATEX).


### Data inputs
In general ITHIM-R requires 5 user defined input files in csv format, saved in a directory of the city's name. See inst/ext/local/accra for example files. There are also numerous assumptions that you can parameterize in the model. 

#### File inputs
  * Travel survey - a table of all trips taken by a group of people on a given day. Includes people who take no trips.
      * One row per trip (or stage of trip)
      * Minimal columns: participant_id, age, sex, trip_mode, trip_duration (or trip_distance)
      * Other columns: stage_mode, stage_duration (or stage_distance)
  * Recorded injury events - a table of recorded road-traffic injury (fatality) events in a city in one or more years.
      * One row per event
      * Minimal columns: cas_mode and strike_mode
      * Other columns: year, cas_age, cas_gender, weight (e.g. multiple years combined)
  * Disease burden data (gbd_CITY.csv)
      * One row per disease/metric/age/gender combination
      * Minimal rows: Measure (death/YLL); sex_name (Male/Female); age_name ('x to y'); Cause_name (disease names); Val (value of burden); population (number of people Val corresponds to, e.g. population of country)
  * Population of city - in order to scale the burden in Disease burden data to the city under study
      * One row per demographic group
      * Columns: sex, age, population
      * age column should share boundaries with age_name in Disease burden data, but can be more aggregated
  * Physical activity survey
      * One row per person
      * Columns: sex, age, work_ltpa_marg_met = total leisure and work PA in a week
      
#### Function-call inputs


#### Synthetic Population

**Description** 
The first input you will need to provide is the synthetic population data. This data typically comes from a household travel survey or travel time use survey, and a self-report leisure time physical activity survey. These data will be uses throughout the process. 

**Synthetic Population Dataset Format**
You should have a table with the following variables

* trip_id
* trip_mode
* trip_duration
* participant_id
* age
* sex
* age_cat
* ltpa_marg_met
* work_marg_met

**Montreal Synthetic Population Dataset Example**
```{r}


```

#### Who Hit Who Matrix

**Description**
TEXT HERE

**Who Hit Who Matrix Dataset Format**
A matrix 

**Montreal Who Hit Who Matrix Dataset Example**
```{r}


```


### Air Pollution

**Description**
TEXT HERE

**Air Pollution Dataset Format**
A matrix 

**Montreal Air Pollution Dataset Example**
```{r}


```

**We are currently working on developing a separate package to create a synthetic population**  


For further documentation consult the wiki. 
For ongoing discussions, see issues.
In addition, relevant documents are stored in shared GDrive folder, and a Slack channel is available for communications among contributors.

See [communication channels on Wiki](https://github.com/ITHIM/ITHIM-R/wiki/Communication-channels)

## Related Repositories 
* [ITHIM (R, Physical Activity)](https://github.com/ITHIM/ITHIM)
* [ITHIM Interface (R, Shiny)](https://github.com/ITHIM/ithim-r-interface)
