# Assumes accra_injuries script has been run

# Read inj dataaset
inj <- read_csv("data/synth_pop_data/accra/injuries/deaths_by_mode.csv")
# Remove sex_age column
inj <- select(inj, -c(sex_age))
# Rename column
inj <- rename(inj, total = Deaths)
inj <- reshape2::melt(inj)

# Remove NAs
inj[is.na(inj)] <- 0

# Write clean data in long format
write_csv(inj, "data/synth_pop_data/accra/injuries/deaths_by_mode_long.csv")