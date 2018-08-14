# Generate all scenarios
source("R/scenarios/accra/scenarios.R")

# Generate distance and duration matrices
source("R/scenarios/accra/dist_dur_tbls.R")

# Calculate total PA MMET
source("R/PA/accra/total_mmet.R")

# Calculate PM calculations
source("R/pollution/accra/scenario_pm_calculations.R")

# Calculate RR 2.5
source("R/health/accra/gen_ap_rr.R")

# Calculate RR PA
source("R/health/accra/gen_pa_rr.R")

# Combine RR for PA and AP for common diseases
source("R/health/accra/combined_rr_pa_pa.R")

# Calculate disease burden for injuries
source("R/injuries/accra/accra_injuries.R")

# Calculate disease burden for AP, PA and Injuries
source("R/health/accra/health_burden.R")
