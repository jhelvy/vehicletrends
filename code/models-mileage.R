# Load functions, libraries, and other settings
source(here::here("code", "0-setup.R"))

ds <- load_ds() |>
  filter(age_years >= 2 & age_years <= 9) |>
  filter(inventory_type == 'used')

# Function to get age coefficient for vehicle type and powertrain
get_age_coefficient <- function(vehicle_type, powertrain) {
  model <- feols(
    fml = miles ~ age_years,
    data = ds |>
      filter(vehicle_type == !!vehicle_type) |>
      filter(powertrain == !!powertrain) |>
      select(miles, age_years) |>
      collect()
  )

  return(coef(model)["age_years"])
}

# Get all unique combinations of vehicle_type and powertrain
combinations <- ds |>
  distinct(vehicle_type, powertrain) |>
  collect()

# Apply function to all combinations
age_coefficients <- combinations |>
  mutate(
    annual_mileage = map2_dbl(vehicle_type, powertrain, get_age_coefficient)
  )

age_coefficients

write_csv(age_coefficients, here('data', 'annual_mileage.csv'))
