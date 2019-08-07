---
output:
  html_document: default
  pdf_document: default
---
# ITHIM-R

Development of the ITHIM-R, also known as ITHIM version 3.0. Started in January 2018.

This document aims to be a comprehensive record of the calculations in the ITHIM pipeline, specifically the ITHIM- R package. Some details here (and the default inputs to the functions) are specific to the Accra version of that model, as Accra has been the setting for construction of the prototype.

### Outline
In this document, in general, lower-case letters correspond to indices, or dimensions, of objects, and take one of a set of possible values, detailed in Table 1 (Convert from LATEX).

The set of fixed input data items are denoted by capital letters, and variable parameters are denoted by greek letters. These are tabulated in Table 2 (Convert from LATEX).


### Data inputs
ITHIM-R requires 5 user defined input files in csv format, saved in a directory of the city's name. See inst/ext/local/accra for example files. There are also numerous assumptions that you can parameterize in the model. 

#### File inputs
  * Travel survey (trips_CITY.csv) - a table of all trips taken by a group of people on a given day. Includes people who take no trips.
      * One row per trip (or stage of trip)
      * Minimal columns: participant_id, age, sex, trip_mode, trip_duration (or trip_distance)
      * Other columns: stage_mode, stage_duration (or stage_distance)
  * Recorded injury events (injuries_CITY.csv) - a table of recorded road-traffic injury (fatality) events in a city in one or more years.
      * One row per event
      * Minimal columns: cas_mode and strike_mode
      * Other columns: year, cas_age, cas_gender, weight (e.g. multiple years combined)
  * Disease burden data (gbd_CITY.csv)
      * One row per disease/metric/age/gender combination
      * Minimal rows: Measure (death/YLL); sex_name (Male/Female); age_name ('x to y'); Cause_name (disease names); Val (value of burden); population (number of people Val corresponds to, e.g. population of country)
  * Population of city (population_CITY.csv) - in order to scale the burden in Disease burden data to the city under study
      * One row per demographic group
      * Columns: sex, age, population
      * age column should share boundaries with age_name in Disease burden data, but can be more aggregated
  * Physical activity survey (pa_CITY.csv)
      * One row per person
      * Columns: sex, age, work_ltpa_marg_met = total leisure and work PA in a week
      
#### Function-call inputs

The following values are set in the call to the set-up function (run_ithim_setup).

  * Values for navigation:
      * CITY - the name of the city, which is also the name of the directory contain the 5 files
      * setup_call_summary_filename
      * PATH_TO_LOCAL_DATA
  * City-specific values
      * speeds
      * emission_inventory
  * Model parameters
      * DIST_CAT
      * AGE_RANGE
      * ADD_WALK_TO_BUS_TRIPS
      * ADD_BUS_DRIVERS
      * ADD_TRUCK_DRIVERS
      * TEST_WALK_SCENARIO
      * TEST_CYCLE_SCENARIO
      * MAX_MODE_SHARE_SCENARIO
      * REFERENCE_SCENARIO
      * NSAMPLES

The following values can be uncertain - i.e. they can be sampled from pre-specificied distributions, and the model run multiple (NSAMPLES) times, in order to evaluate the output with varying inputs. 

For distributions (lognormal and beta), a single numeric input becomes the set value for that parameter. A vector of length 2 becomes the parameters for the distribution from which samples are taken.

Confidence values between 0 and 1 are used to parametrise a beta distribution or a Dirichlet distribution. A confidence value of 1 corresponds to a Delta function, i.e. there is no uncertainty.

Logic values determine how the dose--response relationships are used: if F, the mean DR relationship is used. If T, DR curves are sampled.

  * Pollution values:
      * PM_CONC_BASE - lognormal - background PM2.5 concentration
      * PM_TRANS_SHARE - beta - proportion of background PM2.5 attributable to transport
      * EMISSION_INVENTORY_CONFIDENCE - value between 0 and 1 - how confident we are about the emission inventory
  * PA values:
      * BACKGROUND_PA_SCALAR - lognormal - scalar for physical activity
      * BACKGROUND_PA_CONFIDENCE - value between 0 and 1 - how confident we are about the PA survey (in terms of the number of people who report 0 PA)
      * MMET_CYCLING - lognormal - mMET value associated with cycling
      * MMET_WALKING - lognormal - mMET value associated with walking
  * Travel values:
      * BUS_WALK_TIME - lognormal - time taken to walk to PT
      * DAY_TO_WEEK_TRAVEL_SCALAR - beta - how daily travel scales to a week
      * BUS_TO_PASSENGER_RATIO - beta - number of buses per passenger
      * TRUCK_TO_CAR_RATIO - beta - number of trucks per car
      * DISTANCE_SCALAR_CAR_TAXI - lognormal - scalar for car/taxi distance
      * DISTANCE_SCALAR_WALKING - lognormal - scalar for walking distance
      * DISTANCE_SCALAR_PT - lognormal - scalar for PT distance
      * DISTANCE_SCALAR_CYCLING - lognormal - scalar for cycling distance
      * DISTANCE_SCALAR_MOTORCYCLE - lognormal - scalar for motorcycle distance
  * Health values:
      * PA_DOSE_RESPONSE_QUANTILE - logic - whether or not to sample from DR curves
      * AP_DOSE_RESPONSE_QUANTILE - logic - whether or not to sample from DR curves
      * CHRONIC_DISEASE_SCALAR - lognormal - scalar for GBD data
  * Injury values:
      * INJURY_REPORTING_RATE - lognormal - the rate at which injuries are reported
      * INJURY_LINEARITY - lognormal - how linear injuries are in space
      * CASUALTY_EXPONENT_FRACTION - beta - fraction of injury linearity attributed to casualty mode
      
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

## How The Code Works

There are three main blocks to the code: setting up, running the model, and assessing uncertainty. The first block, **run_ithim_setup**, sets the variables for computing the ITHIM. This is done in the second block, **run_ithim**. The separation of these blocks aims to isolate computations that are processing steps from those that contribute to the ITHIM calculation. (This distinction should mean that Section **run_ithim_setup** is a data harmonisation step and deals with all location-specific issues, whereas **run_ithim** should be able to be exactly the same for all settings. Another view of grouping the steps is those that, when sampling, we do only once, and those we do in a loop. This breaks down when distance-related parameters become uncertain.) Finally, **ithim_uncertainty** is a wrapper for sampling from the ITHIM output in a loop. **ithim_uncertainty** is used for the **sample** use case. We refer to using **run_ithim** alone as the **constant** use case, as all parameters are set to constant values.


### Setting up

**run_ithim_setup** sets the variables for computing the ITHIM. There are functional settings, such as:

  * the number of samples for the **sample** use case;
  *  the city;
  *  the travel modes' speeds, distance relative to car (where relevant), and emission inventory; 
  * the distance categories (for mode-shift scenarios): for Accra these are short (<7km), medium (7--9km), and long (>9km);
  * a number of constant parameters are set, e.g. the $C_i$ parameters listed in Table \ref{inputs} which are fixed;
  * the name of a file to which an input summary is written;
  * whether or not to add bus drivers;
  * whether or not to add truck drivers;
  * which scenario acts as the ''reference'';
  * whether or not to add walk trips to bus trips;
  * the path to local data files (if not using an ITHIM-R example case).

Raw data are loaded in **ithim_load_data**; the parameters are set or sampled in **ithim_setup_parameters**; then some distance calculations are made. At present, we compute **set_vehicle_inventory**, **get_synthetic_from_trips**, and **get_all_distances** if (a) ITHIM is running in ''constant'' mode, or (b) ITHIM is running in ''sample'' mode and the variable parameters do not impact the distances or synthetic populations. This separation is to increase efficiency. It might not be necessary if we are unlikely to have sampling use cases that do not impact on distances.

####Data

We load two types of data: location-specific data, and global data. These will be structurally separated in code organisation. The global data persist across settings. Location-specific data will be particular to a single case study and will have to be provided in a particular format (TBC).

The global data include the disease inventory, which lists the diseases we model, whether they depend on PA and AP, and their codenames and acronyms for accessing the relevant quantities elsewhere in the model. The AP acronym will match the cause code in the dose--response AP dataset. The PA acronym will match a file name in the PA dataset for that dose--response relationship.  %Distance exponents for injuries will also be global datasets, likewise emission factors, though {these will have to have city-specific versions depending on regulatory standards.}

Location-specific data include the emission inventory, population, and GBD data. It makes sense for the age categories to be used in the model to be defined in this dataset. Column labels include age, sex, cause (matching the disease inventory), measure, metric, population and burden. The ages and genders should match in the population dataset to allow burden imputation for the city. The trip set (survey data) which forms the basis of our synthetic trip set, and PA data, which form the basis of our synthetic population, are also case specific. 

Injuries will also be case specific. They are presented in long form, i.e. there is one row per event, and the columns list the specifics of the incident. Column headers should include ''strike_mode'' and ''cas_mode'', potentially also ''year'', ''cas_gender'' and ''cas_age''. From this dataset we form the injury contingency table for regression **set_injury_contingency**. 

#### Injury contingency table
**set_injury_contingency** uses injury data alone to create a contingency table of injuries. Distance data are added later. This contingency table is typically sparse. It enumerates all possible combinations of the covariates in the injury dataset and counts the number of injury (fatality) events fitting that row. It requires matching of covariates across the modules.

#### Parameters
Parameters are set or sampled according to what the user supplies. For most variables, a single value will set the variable to that value, whereas a vector of two values will prompt sampling from a prespecified distribution with the given parameters. These distributions are all log normal or beta distributions. AP and PA dose--response relationships work differently. They are binary logical variables which, if false, fix the relationships to their medians and if true prompt sampling of dose--response curves.

#### Vehicle inventory
The vehicle inventory should be a comprehensive record of all vehicle attributes, including: name, aliases, speed, and emission inventory.