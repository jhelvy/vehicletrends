# Load functions, libraries, and other settings
source(here::here("code", "0-setup.R"))

ds <- load_ds() |>
  filter(age_years >= 2 & age_years <= 9) |>
  filter(inventory_type == 'used') %>%
  select(make, model, vehicle_type, powertrain, miles, age_years)

# Mileage coefficient by vehicle type and powertrain ----

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

# Function to get predicted mileage for vehicle type and powertrain
get_annual_mileage_pred <- function(vt, pt) {
  model <- feols(
    fml = miles ~ age_years,
    data = ds |>
      filter(vehicle_type == vt) |>
      filter(powertrain == pt) |>
      select(miles, age_years) |>
      collect()
  )

  # Predict retention rates at different ages
  pred_data <- data.frame(age_years = seq(2, 9, 0.5))
  pred_data$mileage_predicted <- exp(predict(model, newdata = pred_data))
  pred_data$vehicle_type <- vt
  pred_data$powertrain <- pt

  return(pred_data)
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

write_csv(, here('data', 'annual_mileage_powertrain_type.csv'))

# Loop through each row in combinations to get predictions
results <- list()
for (i in 1:nrow(combinations)) {
  results[[i]] <- get_annual_mileage_pred(
    combinations$vehicle_type[i],
    combinations$powertrain[i]
  )
}
mileage_pred <- rbindlist(results)

mileage_pred

write_csv(
  mileage_pred,
  here('data', 'mileage_pred_powertrain_type.csv')
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

  # Predict retention rates at different ages
  pred_data <- data.frame(age_years = seq(1, 5, 0.5))
  pred_data$rr_predicted <- exp(predict(model, newdata = pred_data))
  pred_data$make <- mk
  pred_data$model <- mdl

  return(pred_data)
}

# Get all unique combinations of make and model
combinations_make_model <- ds |>
  distinct(make, model, powertrain, vehicle_type) |>
  collect()

# Loop through each row in combinations
results <- list()
for (i in 1:nrow(combinations_make_model)) {
  results[[i]] <- get_dep_coefficient_make_model(
    combinations_make_model$make[i],
    combinations_make_model$model[i]
  )
}
dep_coefficients_make_model <- rbindlist(results)

dep_coefficients_make_model

write_csv(
  dep_coefficients_make_model,
  here('data', 'annual_depreciation_make_model.csv')
)
