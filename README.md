---
output:
  html_document: default
  pdf_document: default
---

# ITHIM-R

Development of the ITHIM-R, also known as ITHIM version 3.0. Started in
January 2018.

This document aims to be a comprehensive record of the calculations in
the ITHIM pipeline, specifically the ITHIM- R package. Some details here
(and the default inputs to the functions) are specific to the Accra
version of that model, as Accra has been the setting for construction of
the prototype.

### Citation

To cite package 'ithimr' in publications use:

> Rob Johnson and Ali Abbas (2021). ithimr: Integrated Transport and
Health Impact Model. R package version 0.1.1.

A BibTeX entry for LaTeX users is

> \@Manual{, title = {ithimr: Integrated Transport and Health Impact
Model}, author = {Rob Johnson and Ali Abbas}, year = {2021}, note = {R
package version 0.1.1}, }

### Outline

In this document, in general, lower-case letters correspond to indices,
or dimensions, of objects, and take one of a set of possible values,
detailed in Table 1 (Convert from LATEX).

The set of fixed input data items are denoted by capital letters, and
variable parameters are denoted by greek letters. These are tabulated in
Table 2 (Convert from LATEX).

### Data inputs

ITHIM-R requires 5 user defined input files in csv format, saved in a
directory of the city's name. See inst/ext/local/accra for example
files. There are also numerous assumptions that you can parameterize in
the model.

#### File inputs

-   Travel survey (trips_CITY.csv) - a table of all trips taken by a
    group of people on a given day. Includes people who take no trips.

    -   One row per trip (or stage of trip)
    -   Minimal columns: participant_id, age, sex, trip_mode,
        trip_duration (or trip_distance)
    -   Other columns: stage_mode, stage_duration (or stage_distance)

-   Recorded injury events (injuries_CITY.csv) - a table of recorded
    road-traffic injury (fatality) events in a city in one or more
    years.

    -   One row per event
    -   Minimal columns: cas_mode and strike_mode
    -   Other columns: year, cas_age, cas_gender, weight (e.g. multiple
        years combined)

-   Disease burden data (gbd_CITY.csv)

    -   One row per disease/metric/age/gender combination
    -   Minimal rows: Measure (death/YLL); sex_name (Male/Female);
        age_name ('x to y'); Cause_name (disease names); Val (value of
        burden); population (number of people Val corresponds to, e.g.
        population of country)

-   Population of city (population_CITY.csv) - in order to scale the
    burden in Disease burden data to the city under study

    -   One row per demographic group
    -   Columns: sex, age, population
    -   age column should share boundaries with age_name in Disease
        burden data, but can be more aggregated

-   Physical activity survey (pa_CITY.csv)

    -   One row per person
    -   Columns: sex, age, work_ltpa_marg_met = total leisure and work
        PA in a week

#### Function-call inputs

The following values are set in the call to the set-up function
(run_ithim_setup).

-   Values for navigation:

    -   CITY - the name of the city, which is also the name of the
        directory contain the 5 files
    -   setup_call_summary_filename
    -   PATH_TO_LOCAL_DATA

-   City-specific values

    -   speeds
    -   emission_inventory

-   Model parameters

    -   DIST_CAT
    -   AGE_RANGE
    -   ADD_WALK_TO_BUS_TRIPS
    -   ADD_BUS_DRIVERS
    -   ADD_TRUCK_DRIVERS
    -   TEST_WALK_SCENARIO
    -   TEST_CYCLE_SCENARIO
    -   MAX_MODE_SHARE_SCENARIO
    -   REFERENCE_SCENARIO
    -   NSAMPLES

The following values can be uncertain - i.e. they can be sampled from
pre-specificied distributions, and the model run multiple (NSAMPLES)
times, in order to evaluate the output with varying inputs.

For distributions (lognormal and beta), a single numeric input becomes
the set value for that parameter. A vector of length 2 becomes the
parameters for the distribution from which samples are taken.

Confidence values between 0 and 1 are used to parametrise a beta
distribution or a Dirichlet distribution. A confidence value of 1
corresponds to a Delta function, i.e. there is no uncertainty.

Logic values determine how the dose--response relationships are used: if
F, the mean DR relationship is used. If T, DR curves are sampled.

-   Pollution values:

    -   PM_CONC_BASE - lognormal - background PM2.5 concentration
    -   PM_TRANS_SHARE - beta - proportion of background PM2.5
        attributable to transport
    -   EMISSION_INVENTORY_CONFIDENCE - value between 0 and 1 - how
        confident we are about the emission inventory

-   PA values:

    -   BACKGROUND_PA_SCALAR - lognormal - scalar for physical activity
    -   BACKGROUND_PA_CONFIDENCE - value between 0 and 1 - how confident
        we are about the PA survey (in terms of the number of people who
        report 0 PA)
    -   MMET_CYCLING - lognormal - mMET value associated with cycling
    -   MMET_WALKING - lognormal - mMET value associated with walking

-   Travel values:

    -   BUS_WALK_TIME - lognormal - time taken to walk to PT
    -   DAY_TO_WEEK_TRAVEL_SCALAR - beta - how daily travel scales to a
        week
    -   BUS_TO_PASSENGER_RATIO - beta - number of buses per passenger
    -   TRUCK_TO_CAR_RATIO - beta - number of trucks per car
    -   DISTANCE_SCALAR_CAR_TAXI - lognormal - scalar for car/taxi
        distance
    -   DISTANCE_SCALAR_WALKING - lognormal - scalar for walking
        distance
    -   DISTANCE_SCALAR_PT - lognormal - scalar for PT distance
    -   DISTANCE_SCALAR_CYCLING - lognormal - scalar for cycling
        distance
    -   DISTANCE_SCALAR_MOTORCYCLE - lognormal - scalar for motorcycle
        distance

-   Health values:

    -   PA_DOSE_RESPONSE_QUANTILE - logic - whether or not to sample
        from DR curves
    -   AP_DOSE_RESPONSE_QUANTILE - logic - whether or not to sample
        from DR curves
    -   CHRONIC_DISEASE_SCALAR - lognormal - scalar for GBD data

-   Injury values:

    -   INJURY_REPORTING_RATE - lognormal - the rate at which injuries
        are reported
    -   SIN_EXPONENT_SUM lognormal parameter: linearity of injuries with
        respect to two modes. SIN_EXPONENT_SUM=2 means no safety in
        numbers.
    -   CASUALTY_EXPONENT_FRACTION - beta - fraction of SIN_EXPONENT_SUM
        attributed to casualty mode

#### Synthetic Population

**Description** The first input you will need to provide is the
synthetic population data. This data typically comes from a household
travel survey or travel time use survey, and a self-report leisure time
physical activity survey. These data will be uses throughout the
process.

**Synthetic Population Dataset Format** You should have a table with the
following variables

-   trip_id
-   trip_mode
-   trip_duration
-   participant_id
-   age
-   sex
-   age_cat
-   ltpa_marg_met
-   work_marg_met

**Montreal Synthetic Population Dataset Example**

```{r}


```

#### Who Hit Who Matrix

**Description** TEXT HERE

**Who Hit Who Matrix Dataset Format** A matrix

**Montreal Who Hit Who Matrix Dataset Example**

```{r}


```

### Air Pollution

**Description** TEXT HERE

**Air Pollution Dataset Format** A matrix

**Montreal Air Pollution Dataset Example**

```{r}


```

**We are currently working on developing a separate package to create a
synthetic population**

For further documentation consult the wiki. For ongoing discussions, see
issues. In addition, relevant documents are stored in shared GDrive
folder, and a Slack channel is available for communications among
contributors.

See [communication channels on
Wiki](https://github.com/ITHIM/ITHIM-R/wiki/Communication-channels)

## Related Repositories

-   [ITHIM (R, Physical Activity)](https://github.com/ITHIM/ITHIM)
-   [ITHIM Interface (R,
    Shiny)](https://github.com/ITHIM/ithim-r-interface)

## How The Code Works

There are three main blocks to the code: setting up, running the model,
and assessing uncertainty. The first block, `run_ithim_setup`, sets the
variables for computing the ITHIM. This is done in the second block,
`run_ithim`. The separation of these blocks aims to isolate computations
that are processing steps from those that contribute to the ITHIM
calculation. (This distinction should mean that Section
`run_ithim_setup` is a data harmonisation step and deals with all
location-specific issues, whereas `run_ithim` should be able to be
exactly the same for all settings. Another view of grouping the steps is
those that, when sampling, we do only once, and those we do in a loop.
This breaks down when distance-related parameters become uncertain.)
Finally, `ithim_uncertainty` is a wrapper for sampling from the ITHIM
output in a loop. `ithim_uncertainty` is used for the ''sample'' use
case. We refer to using `run_ithim` alone as the ''constant'' use case,
as all parameters are set to constant values.

### Setting up

`run_ithim_setup` sets the variables for computing the ITHIM. There are
functional settings, such as:

-   the number of samples for the ''sample'' use case;
-   the city;
-   the travel modes' speeds, distance relative to car (where relevant),
    and emission inventory;
-   the distance categories (for mode-shift scenarios): for Accra these
    are short (\<7km), medium (7--9km), and long (\>9km);
-   a number of constant parameters are set, e.g. the C_i parameters for
    the pollution calculation which are fixed
    (<https://www.overleaf.com/read/mrjtkhffzfzr>);
-   the name of a file to which an input summary is written;
-   whether or not to add bus drivers;
-   whether or not to add truck drivers;
-   which scenario acts as the ''reference'';
-   whether or not to add walk trips to bus trips;
-   the path to local data files (if not using an ITHIM-R example case).

Raw data are loaded in `ithim_load_data`; the parameters are set or
sampled in `ithim_setup_parameters`; then some distance calculations are
made. At present, we compute `set_vehicle_inventory`,
`get_synthetic_from_trips`, and `get_all_distances` if (a) ITHIM is
running in ''constant'' mode, or (b) ITHIM is running in ''sample'' mode
and the variable parameters do not impact the distances or synthetic
populations. This separation is to increase efficiency. It might not be
necessary if we are unlikely to have sampling use cases that do not
impact on distances.

#### Data

We load two types of data: location-specific data, and global data.
These will be structurally separated in code organisation. The global
data persist across settings. Location-specific data will be particular
to a single case study and will have to be provided in a particular
format (TBC).

The global data include the disease inventory, which lists the diseases
we model, whether they depend on PA and AP, and their codenames and
acronyms for accessing the relevant quantities elsewhere in the model.
The AP acronym will match the cause code in the dose--response AP
dataset. The PA acronym will match a file name in the PA dataset for
that dose--response relationship. %Distance exponents for injuries will
also be global datasets, likewise emission factors, though {these will
have to have city-specific versions depending on regulatory standards.}

Location-specific data include the emission inventory, population, and
GBD data. It makes sense for the age categories to be used in the model
to be defined in this dataset. Column labels include age, sex, cause
(matching the disease inventory), measure, metric, population and
burden. The ages and genders should match in the population dataset to
allow burden imputation for the city. The trip set (survey data) which
forms the basis of our synthetic trip set, and PA data, which form the
basis of our synthetic population, are also case specific.

Injuries will also be case specific. They are presented in long form,
i.e. there is one row per event, and the columns list the specifics of
the incident. Column headers should include ''strike_mode'' and
''cas_mode'', potentially also ''year'', ''cas_gender'' and ''cas_age''.
From this dataset we form the injury contingency table for regression
`set_injury_contingency`.

#### Injury contingency table

`set_injury_contingency` uses injury data alone to create a contingency
table of injuries. Distance data are added later. This contingency table
is typically sparse. It enumerates all possible combinations of the
covariates in the injury dataset and counts the number of injury
(fatality) events fitting that row. It requires matching of covariates
across the modules.

#### Parameters

Parameters are set or sampled according to what the user supplies. For
most variables, a single value will set the variable to that value,
whereas a vector of two values will prompt sampling from a prespecified
distribution with the given parameters. These distributions are all log
normal or beta distributions. AP and PA dose--response relationships
work differently. They are binary logical variables which, if false, fix
the relationships to their medians and if true prompt sampling of
dose--response curves.

#### Vehicle inventory

The vehicle inventory should be a comprehensive record of all vehicle
attributes, including: name, aliases, speed, and emission inventory.

#### Synthetic

**Synthetic population** The synthetic population is created by
isolating the trip-set individuals and matching to them the individuals
surveyed for physical activity. They are matched based on age and
gender. For Accra, the age groups are different: the PA people are
grouped into 15--55 and 56--69 and matched to the trip set ages grouped
as 15--49 and 50--69. These random allocations are not captured anywhere
in the VOI analysis.

**Synthetic trips** `get_synthetic_from_trips` takes the loaded trip set
and forms from it the synthetic trip set. We add truck and bus driver
trips (`add_ghost_trips`), if the input dictates that they should be
added. We create two male truck drivers and two male bus drivers who
each take one trip. (N.B. the bus drivers' mode is ''bus_driver'' to
distinguish it from the mode ''bus'', which is assumed to be taken as a
passenger.) Again, the ages are uniformly sampled from the lowest and
highest ages of our existing population. The total duration is
calculated as before, using the ''TRUCK_TO_CAR_RATIO'' (0.21) and
''BUS_TO_PASSENGER_RATIO'' (0.022, CIRT (2011), State Transport
Undertakings, Profile & Performance 2008-09, Central Institute of Road
Transport, Pune, India) respectively, and is split as before evenly
between the drivers.

The bus and truck drivers do not get a person ID and so are not counted
in the synthetic population. Therefore, they contribute to other
people's exposure to injury risk and pollution, but they do not
contribute to the health burden via the pollution or PA pathways. As
they have ages and genders, they do contribute to the at-risk-of-injury
road-user population.

**Scenarios** `ithim_setup_baseline_scenario` creates the baseline
scenario (by adding distances and the column ''Scenario'' with entries
''Baseline'') and a scenario-generation function creates all the pre-set
scenarios by changing the modes of randomly sampled trips (e.g.
`create_max_mode_share_scenarios` and `create_all_scenarios` for Accra
and Sao Paulo) or adding trips for everyone (e.g. `create_walk_scenario`
and `create_cycle_scenario`). This is another unaccounted-for source of
randomness.

#### Calculating distances

ITHIM-R requires the distance data in various formats. All these are
processed from one function, `get_all_distances`.

**Adding walk trips**

First, bus journeys are augmented with a walk component (if the user
input dictates it) in the function `walk_to_bus_and_combine_scen`: we
take the set of PT trips, which has J entries. We add J journeys with
mode ''walk_to_pt'' and duration WALK_TO_BUS_TIME to the set, with
trip_id and participant_id to match that of the corresponding trip in
the original set.

**Travel summaries**

The total distance and duration matrices are calculated in
`dist_dur_tbls`. The distance set is the total distance travelled per
mode per scenario, and the duration set is constructed analogously. The
distance for bus drivers scales linearly with bus passengers, where the
distance in the baseline is defined based on the bus driver distance
relative to car.

**Injuries**

Distances are calculated for the injury module in
`distances_for_injury_function`, where we match mode names to those in
the injury dataset, e.g. ''taxi'', ''shared_auto'', ''shared_taxi'' and
''car'' combine to form ''car'', and ''auto_rickshaw'' is added to
''motorcycle'' to represent two/three-wheeled striking vehicle. Some
work is required to separate drivers from passengers; we currently
assume all travellers are drivers, with the exception of ''bus''. (This
is a problem for the examples of Delhi and Bangalore, whose trip sets
have car travellers under the age at which driving is permitted and who
therefore should not contribute to striking distance.)

We model injuries via regression by predicting the number of fatalities
of each demographic group on each mode, which we calculate as a sum over
all the ways in which they might have been injured (i.e., all the modes
with which they might have collided).

The predictive covariates include the distances travelled by the parties
and their demographic details. These requirements lead to a natural
separation of the (training) dataset into two groups: a set for which we
have distance data for the other party, and a set for which we do not
have distance data for the other party. The former equates to a ''who
hit whom'' (''whw'') matrix (albeit in a higher dimension), and will
account for changes in injuries resulting of a change in strike-mode
travel. The latter corresponds to causes of injury that will not change
across scenarios, including ''no other vehicle'' and modes of transport
that we do not consider to change, which might include trucks and buses
if they are not somehow explicitly included in the trip set. We label
this group ''noov'': no or other vehicle.

We model the number of injuries as a Poisson-distributed variable with
an offset depending on the distance(s) and the reporting rate
(<https://www.overleaf.com/read/mrjtkhffzfzr>). We choose this form of
equation to enable (a) linearity in injuries with respect to total
travel combined across modes and (b) linearity in injuries as subdivided
by travellers of each mode. Note that this methodology is under
development. See
<https://github.com/robj411/safety_in_numbers_power_law>.

We use, for Accra, the covariates ''casualty_mode\*strike_mode'',
''casualty_age'', and ''casualty_gender'', to form the model matrix X.
We fit the coefficients of the model using data for ten years, assuming
the same travel each year, which corresponds to the baseline scenario.
To incorporate the injury reporting rate, we set it as an offset, whose
value is 1 in fitting the model, and as specified in making predictions.
For settings where there are no demographic data (e.g. Delhi), we fit
the model using ''casualty_mode'' and ''strike_mode'' alone. Predictions
are made, as before, by demographic group.

### The ITHIM programme

`run_ithim` computes the ITHIM output by calling each module in turn and
summarising their outputs in terms of the health burden. Other outputs
of the model include: MMETs per person, PM2.5 in each scenario, PM2.5
per person, and injuries burden.

#### Calculating background PM2.5

`scenario_pm_calculations` uses total distance by mode to compute the
total PM2.5 in the scenario. First we compute the emission inventory in
terms of fractions. If the confidence is 1 then we use the inventory
directly. If P\<1, we sample the emission inventory fractions as is
(<https://www.overleaf.com/read/mrjtkhffzfzr>).

Then we multiply vehicle distance by vehicle emission factor (where the
emission factor is the emission inventory (fraction) divided by distance
at baseline).

This table is augmented with rows corresponding to any vehicle missing
from the trip set. For Accra, these modes include ''big trucks'' and
''other''. The baseline row is equal to the emission inventory fraction.
For each scenario, it is scaled. We take the sum over modes to get a
scalar for emissions in scenarios. The background PM2.5 in each scenario
is the sum of the transport component and the non-transport component.

#### Calculating PM2.5 per person

Individual exposures to PM2.5 are calculated using the background PM2.5
and the trip sets. There are three major components to daily exposure:
one, a person's total inhalation off road; two, a person's inhalation on
road in a vehicle, and, three, a person's inhalation on road while
undertaking active transport. Each category has an amount of background
PM2.5 and a ventilation rate which together inform overall exposure.

The ratio of exposure off road to that on road is a function of total
PM2.5, defined in <https://www.overleaf.com/read/mrjtkhffzfzr> (Goel,
2015). This defines the exposure of a person in an open vehicle (i.e.
pedestrian, cyclist or motorist), and it is used to calculate in-vehicle
exposure, assuming an exposure with the window closed, and the
proportion of vehicles having closed windows. The exposure in a subway
is constant and not dependent on the road ratio.

Ventilation rates are calculated for each mode, assuming a base-level
inhalation rate, and the mMETs for the mode (Ainsworth compendium 2011
sites.google.com/site/ compendiumofphysicalactivities for walking and
cycling). Then the air inhaled during travel per person is the sum over
their travel, and the rate of PM2.5 inhalation during travel is a
function of the exposure when travelling.

The air inhaled when not travelling is the base-level ventilation times
the time spent not travelling. Together, the PM2.5 exposure is
calculated as the total PM2.5 inhaled per hour
(<https://www.overleaf.com/read/mrjtkhffzfzr>).

#### AP--disease dose--response relative risk

`gen_ap_rr` uses each person's exposure to PM2.5 to compute their
relative risk of five diseases (IHD, lung cancer, COPD, stroke, LRI),
using curves parametrised by four disease-specific variables (Burnett,
2014). Of the five diseases, two (IHD and stroke) have parameters
specific to age groups starting at age 25. (For any person of age lower
than 25, we set the relative risk to 1.) The other three (lung cancer,
LRI and COPD) have one set of parameters for all ages.

The curves are in the form of samples of the set of four parameters. We
model the densities of these samples (using a quantile for parameter 3,
kernel density estimation for parameter 2, and GAMs for parameters 1 and
4) in order to draw either the median or random samples via their
quantiles. (The four parameters refer, in numerical order, to alpha,
beta, gamma and tmrel in Burnett (2014).)

From these parameters, the relative risk of mortality is defined as in
Burnett (2014) as RR = 1 + alpha \* (1 - exp( - beta \* (AP - tmrel) ^
gamma ) ).

#### Individual-level MMETs

Using trip sets and the synthetic population, `total_mmet` computes
total MMETs per person as the sum of walking MMETs per day and cycling
MMETs per day, which are scaled up to a week via the PA scalar, and
work/leisure MMETs per week.

We calculate the proportion of people who do no work/leisure PA
according to the confidence in the PA survey. First, we calculate the
raw probability that a person in demographic group completes no
non-travel PA. Then, we set the probability to use as the raw value if
our confidence is 1. If it's less than, we map the confidence to
parametrise a Beta distribution.

Finally, for each person in the population, we sample non-travel MMETs
as zero with the calculated probability and from the raw non-zero
density of their demographic group.

#### PA--disease dose--response relative risk

`gen_pa_rr` uses each person's MMETs per week to compute their relative
risk of six diseases, using curves found from meta analysis. Each
disease (except type 2 diabetes) has a threshold beyond which there is
no further change in relative risk. (Total cancer, breast cancer, colon
& rectum cancer, endometrial cancer, & coronary heart disease: 35; lung
cancer: 10; stroke: 32; all cause: 16.08.) For all other diseases, we
use mortality.

The relative risk for each person for each disease is calculated in
`PA_dose_response` using the curve selected, which is an interpolated
mean or a sample of functions. See
<https://www.overleaf.com/read/mrjtkhffzfzr> for an example.

#### Combining PA and AP relative risks

`combined_rr_ap_pa` combines the relative risks of `gen_ap_rr` and
`gen_pa_rr` through multiplication. Not all diseases have a
dose--response relationship for both AP and PA. Just stroke, lung
cancer, and IHD have both. For the other diseases, only one of the RRs
is different from one.

#### Calculating injuries

`injuries_function` predicts the number of injuries in each scenario
based on the training model built from the baseline scenario. We predict
for the year 2016 (for Accra), using scenario-specific travel data. The
number of fatalities is taken as the sum of fatalities over the strike
modes. `injury_death_to_yll` extrapolates injury deaths to injury YLLs
via the ratio in the GBD.

#### Calculating the health burden

`health_burden` calculates the total health burden relative to the
reference scenario (which, by default, is the baseline) using the injury
and health outputs combined with GBD data, via population attributable
fractions (PAF). We then calculate the PAF relative to the reference
scenario.

We estimate the background burden of disease using Global Burden of
Disease data and scaling based on the ratio of populations between
country and city. If we are scaling the background burden of diseases,
we do so here.

We combine the burden with the PAF through multiplication and, finally,
for the injuries, we sum over modes to compute the burden and subtract
from the values for the reference scenario. Then we have change in
burden by cause, per demographic group, per scenario.

### Uncertain parameters

`ithim_uncertainty` is a wrapper for sampling from the ITHIM output in a
loop. The number of samples to take is set at the initiation step, along
with specifications of parametric distributions, from which the required
number of samples are taken. First, it sets the parameters to the
environment for the current sample. Then it recalculates any
distance-related objects: `set_vehicle_inventory` and
`get_synthetic_from_trips` if any raw distances change, and
`get_all_distances` if e.g. the walk-to-bus time has changed. Then the
basic `run_ithim` function is called.

Running ITHIM with uncertain parameters allows assessment of their
impact on the outcome (AKA sensitivity analysis). We use EVPPI to
calculate the expected reduction in uncertainty in the outcome were we
to learn a parameter perfectly. This means we can implement models that
are basic in their parametrisation, and learn at the end for which
parameters it would be worthwhile spending dedicated time learning
better.

#### Parametric distributions for uncertain variables

Cycling and walking MMETs are the number of MMETs per hour when
undertaking cycling and walking, and determine also the ventilation
rates. Motorcycle distance is the total distance travelled by
motorcycles relative to the total distance travelled by cars in the
baseline scenario. Non-travel PA, injury reporting rate and NCD burden
all act as scalars for the relevant datasets. Note that the non-travel
PA scalar does not affect the \~40% of the population whose non-travel
PA is 0.

#### Dose--response relationships

For the dose--response relationships between physical activity (PA) and
disease and air pollution (AP) and disease, we assume that there is
uncertainty, but no variability, in the relationship. This means that we
sample a relationship from the distribution of relationships, and apply
that relationship to all individuals precisely. This means that, given
fixed doses, responses between individuals will be perfectly correlated.

We achieve this by use of the probability integral transform: we sample
a random variable uniformly distributed on the space (0,1) and map it,
via a cumulative distribution function, to the distribution describing
the dose--response relationship.

#### Physical activity

Each disease's PA dose--response relationship is defined by a truncated
normal. For each dose, there is a mean value, an upper bound, and a
lower bound. For each person's dose, we get the response by mapping the
uniform random variable onto the truncated normal defined by the mean
and bounds for that dosage.

#### Air pollution

For the AP relationship, there are four parameters per disease. We
sample the first from an empirical distribution using the probability
integral transform. We sample the second via the same method,
conditioned on the value of the first, constructing their joint density
with e.g. kde2d. The third parameter is sampled conditioned on the first
and second, constructing their joint density using a GAM. The final
parameter is sampled conditioned on the first, second, and third,
constructing their joint density using a GAM.

As before, there is perfect correlation between individuals, i.e. if
person A's dose is greater than person B's, then person A's response is
strictly greater than person B's response.

The empirical distributions come from Burnett (2014). There are four
parameters per disease: IHD, lung cancer, COPD, and stroke. In addition,
for stroke and IHD, there is a set of four parameters for each age group
from 25 to 95 in five-year increments. In addition to our assumption
that there is perfect correlation between individuals for diseases, we
assume perfect correlation between ages for diseases. I.e., our four
quantiles per disease will be applied to all age groups.
