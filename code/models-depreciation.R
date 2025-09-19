# Load functions, libraries, and other settings
source(here::here("code", "0-setup.R"))

# Basic exponential decline model: r = a*exp(b1*x1 + b2*x2)
# where
#   r = Retention rate
#   a = Intercept
#   x1, x2 = Covariates (e.g. age, miles, etc.)
#   b1, b2 = Covariate coefficients
#
# Log transformation: ln(r) = ln(a) + b1*x1 + b2*x2
#
# To convert log-space estimated coefficients back to original model:
# a = exp(int)

ds <- load_ds() |>
  filter(age_years >= 1 & age_years <= 8) |>
  filter(inventory_type == 'used') %>%
  filter(!is.na(price), !is.na(msrp)) %>%
  # Compute RR for all vehicles
  mutate(rr = msrp / price) %>%
  select(make, model, vehicle_type, powertrain, rr, age_years)

# Mileage coefficient by vehicle type and powertrain ----

# Function to get age coefficient for vehicle type and powertrain
get_dep_coefficient <- function(vt, pt) {
  model <- feols(
    fml = log(rr) ~ age_years,
    data = ds |>
      filter(vehicle_type == {{ vt }}) |>
      filter(powertrain == {{ pt }}) |>
      select(rr, age_years) |>
      collect()
  )

  return(exp(coef(model)["age_years"]) - 1)
}

# Get all unique combinations of vehicle_type and powertrain
combinations <- ds |>
  distinct(vehicle_type, powertrain) |>
  collect()

# Apply function to all combinations
dep_coefficients <- combinations |>
  mutate(
    annual_dep = map2_dbl(vehicle_type, powertrain, get_dep_coefficient)
  )

dep_coefficients

write_csv(
  dep_coefficients,
  here('data', 'annual_depreciation_powertrain_type.csv')
)

# Mileage coefficient by make and model ----

get_dep_coefficient_make_model <- function(mk, mdl) {
  model <- feols(
    fml = log(rr) ~ age_years,
    data = ds |>
      filter(make == {{ mk }}) |>
      filter(model == {{ mdl }}) |>
      select(rr, age_years) |>
      collect()
  )

  return(exp(coef(model)["age_years"]) - 1)
}

# Get all unique combinations of make and model
combinations_make_model <- ds |>
  distinct(make, model) |>
  collect()

# Apply function to all combinations
dep_coefficients_make_model <- combinations_make_model |>
  mutate(
    annual_dep = map2_dbl(make, model, get_dep_coefficient_make_model)
  )

dep_coefficients_make_model

write_csv(
  dep_coefficients_make_model,
  here('data', 'annual_depreciation_make_model.csv')
)
