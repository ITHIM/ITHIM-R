# Package index

## Main functions

Run the ITHIM model - the setting up the environments (variables and
loading files), running the model and generating the outputs.

- [`run_ithim()`](https://usr110.github.io/ITHIM-R/reference/run_ithim.md)
  : Wrapper for running ITHIM
- [`run_ithim_setup()`](https://usr110.github.io/ITHIM-R/reference/run_ithim_setup.md)
  : Run the set up scripts for ITHIM

## Others

Other functions

- [`add_distance_columns()`](https://usr110.github.io/ITHIM-R/reference/add_distance_columns.md)
  : Add strike and casualty distances to injury tables
- [`add_ghost_trips()`](https://usr110.github.io/ITHIM-R/reference/add_ghost_trips.md)
  : Addition of ghost trips
- [`add_motorcycle_trips_Chile()`](https://usr110.github.io/ITHIM-R/reference/add_motorcycle_trips_Chile.md)
  : Personal motorcycle trips for Chilean cities
- [`add_trips()`](https://usr110.github.io/ITHIM-R/reference/add_trips.md)
  : Additional trips for trip data set
- [`add_walk_trips()`](https://usr110.github.io/ITHIM-R/reference/add_walk_trips.md)
  : Addition of 'walk to public transport' stages to trip set
- [`AP_dose_response()`](https://usr110.github.io/ITHIM-R/reference/AP_dose_response.md)
  : Calculate relative risk given PM exposure level
- [`ap_dose_response_curve()`](https://usr110.github.io/ITHIM-R/reference/ap_dose_response_curve.md)
  : Computes RR as a DR relationship - CURRENTLY NOT USED
- [`assign_age_groups()`](https://usr110.github.io/ITHIM-R/reference/assign_age_groups.md)
  : Assign age groups to individuals
- [`beta_pointiness()`](https://usr110.github.io/ITHIM-R/reference/beta_pointiness.md)
  : Parameterise confidence in PA data - CURRENTLY NOT IN USE
- [`combined_rr_ap_pa()`](https://usr110.github.io/ITHIM-R/reference/combined_rr_ap_pa.md)
  : Combine relative risks from air pollution and physical activity
- [`combine_health_and_pif()`](https://usr110.github.io/ITHIM-R/reference/combine_health_and_pif.md)
  : Combine health and potential impact fraction (PIF)
- [`complete_trip_distance_duration()`](https://usr110.github.io/ITHIM-R/reference/complete_trip_distance_duration.md)
  : Add missing trip information
- [`create_bogota_scenarios()`](https://usr110.github.io/ITHIM-R/reference/create_bogota_scenarios.md)
  : Create scenarios for Bogota
- [`create_global_scenarios()`](https://usr110.github.io/ITHIM-R/reference/create_global_scenarios.md)
  : Create global scenarios
- [`create_base_pop()`](https://usr110.github.io/ITHIM-R/reference/create_base_pop.md)
  : Creates baseline population
- [`dirichlet_pointiness()`](https://usr110.github.io/ITHIM-R/reference/dirichlet_pointiness.md)
  : Function for Dirichlet parameters
- [`distances_for_injury_function()`](https://usr110.github.io/ITHIM-R/reference/distances_for_injury_function.md)
  : Get distances and parameterise Poisson regression model for injuries
- [`dist_dur_tbls()`](https://usr110.github.io/ITHIM-R/reference/dist_dur_tbls.md)
  : Get distances and duration summaries by mode
- [`extract_data_for_voi()`](https://usr110.github.io/ITHIM-R/reference/extract_data_for_voi.md)
  : Get ITHIM-results into correct format for VoI analysis
- [`gen_ap_rr()`](https://usr110.github.io/ITHIM-R/reference/gen_ap_rr.md)
  : Get relative risk for diseases given PM exposure
- [`gen_pa_rr()`](https://usr110.github.io/ITHIM-R/reference/gen_pa_rr.md)
  : Get relative risk for diseases given mMETs
- [`get_all_distances()`](https://usr110.github.io/ITHIM-R/reference/get_all_distances.md)
  : Find population distances by mode for entire population
- [`get_scenario_settings()`](https://usr110.github.io/ITHIM-R/reference/get_scenario_settings.md)
  : Get values for mode share across distance categories and for max
  mode share scenario - CURRENTLY only used when calling
  summary_tables.Rmd
- [`get_synthetic_from_trips()`](https://usr110.github.io/ITHIM-R/reference/get_synthetic_from_trips.md)
  : Generate baseline population from trip data
- [`health_burden()`](https://usr110.github.io/ITHIM-R/reference/health_burden.md)
  : Compute health burden
- [`injuries_function_2()`](https://usr110.github.io/ITHIM-R/reference/injuries_function_2.md)
  : Predict injuries
- [`injury_death_to_yll()`](https://usr110.github.io/ITHIM-R/reference/injury_death_to_yll.md)
  : Map injury death burden to YLL (years of life lost) burden
- [`ithim_calculation_sequence()`](https://usr110.github.io/ITHIM-R/reference/ithim_calculation_sequence.md)
  : Cascade of computations that form ITHIM-Global
- [`ithim_load_data()`](https://usr110.github.io/ITHIM-R/reference/ithim_load_data.md)
  : Load data for model and prepare input data for model
- [`ithim_setup_baseline_scenario()`](https://usr110.github.io/ITHIM-R/reference/ithim_setup_baseline_scenario.md)
  : Set up baseline scenario data frame
- [`ithim_setup_parameters()`](https://usr110.github.io/ITHIM-R/reference/ithim_setup_parameters.md)
  : Set parameters for ITHIM-Global run
- [`ithim_uncertainty()`](https://usr110.github.io/ITHIM-R/reference/ithim_uncertainty.md)
  : Sampling routine for running ITHIM with uncertainty
- [`join_hb_and_injury()`](https://usr110.github.io/ITHIM-R/reference/join_hb_and_injury.md)
  : Join disease health burden and injury data
- [`scale_trip_distances()`](https://usr110.github.io/ITHIM-R/reference/scale_trip_distances.md)
  : Scale trip distances
- [`scenario_co2_calculations()`](https://usr110.github.io/ITHIM-R/reference/scenario_co2_calculations.md)
  : Calculate total CO2 exposure per mode and scenario
- [`scenario_pm_calculations()`](https://usr110.github.io/ITHIM-R/reference/scenario_pm_calculations.md)
  : Calculate total AP exposure per person
- [`set_injury_contingency()`](https://usr110.github.io/ITHIM-R/reference/set_injury_contingency.md)
  : Injury summary statistics
- [`set_scenario_specific_variables()`](https://usr110.github.io/ITHIM-R/reference/set_scenario_specific_variables.md)
  : Set scenario specific variables - CURRENTLY NOT CALLED
- [`set_vehicle_inventory()`](https://usr110.github.io/ITHIM-R/reference/set_vehicle_inventory.md)
  : Collate all vehicle information
- [`total_mmet()`](https://usr110.github.io/ITHIM-R/reference/total_mmet.md)
  : Calculate total mMETs per person
- [`walk_to_pt_and_combine_scen()`](https://usr110.github.io/ITHIM-R/reference/walk_to_pt_and_combine_scen.md)
  : Add walk to public transport stages
