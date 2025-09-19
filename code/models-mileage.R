# Load functions, libraries, and other settings
source(here::here("code", "0-setup.R"))

ds <- load_ds() |>
  filter(age_years >= 2 & age_years <= 9) |>
  filter(inventory_type == 'used') %>%
  select(vehicle_type, powertrain, miles, age_years)

# Function to get age coefficient for vehicle type and powertrain
get_annual_mileage_coef <- function(vt, pt) {
  model <- feols(
    fml = miles ~ age_years,
    data = ds |>
      filter(vehicle_type == {{ vt }}) |>
      filter(powertrain == {{ pt }}) |>
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
mileage_coefficients <- combinations |>
  mutate(
    annual_mileage = map2_dbl(vehicle_type, powertrain, get_annual_mileage_coef)
  )

mileage_coefficients

write_csv(mileage_coefficients, here('data', 'annual_mileage.csv'))
