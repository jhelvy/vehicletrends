source(here::here('code', '0-setup.R'))

# The raw listings are stored on an external drive. Since they are
# under a license agreement, they cannot be made publicly
# available. This script is included in this repository to show
# the calculations made to produce the "data/listings.parquet"
# file that is generated from the raw data. It also produces the
# "data/data_summary.parquet" file that is used to create Table 1

# Open arrow dataset

ds <- open_dataset(path_raw_data) %>%
  raw_data_filters() # Function in '0-setup.R'

# Number of powertrain-vehicle_type listings
ds %>%
  count(powertrain, vehicle_type) %>%
  collect() %>%
  arrange(powertrain, vehicle_type) %>%
  data.frame()

#    powertrain vehicle_type        n
# 1         bev          car   825307
# 2         bev          cuv  1126752
# 3         bev      minivan     3307
# 4         bev       pickup    76026
# 5         bev          suv    14992
# 6          cv          car 48138188
# 7          cv          cuv 55710305
# 8          cv      minivan  3298662
# 9          cv       pickup 23257135
# 10         cv          suv 17127703
# 11     diesel          car   279671
# 12     diesel          cuv   104342
# 13     diesel      minivan     1189
# 14     diesel       pickup  2582986
# 15     diesel          suv    95378
# 16       flex          car  2201442
# 17       flex          cuv  2248284
# 18       flex      minivan  1146029
# 19       flex       pickup  5277710
# 20       flex          suv  2748372
# 21        hev          car  2367604
# 22        hev          cuv  1403962
# 23        hev      minivan   524800
# 24        hev       pickup   133571
# 25        hev          suv   138641
# 26       phev          car   345805
# 27       phev          cuv   126413
# 28       phev      minivan       90
# 29       phev          suv    25328

# Number of make-model combinations for each powertrain-vehicle_type:
ds %>%
  count(vehicle_type, powertrain, make, model) %>%
  count(powertrain, vehicle_type) %>%
  collect() %>%
  arrange(powertrain, vehicle_type) %>%
  data.frame()

#    powertrain vehicle_type  n
# 1         bev          car 21
# 2         bev          cuv 30
# 3         bev      minivan  2
# 4         bev       pickup  6
# 5         bev          suv  6
# 6          cv          car 59
# 7          cv          cuv 80
# 8          cv      minivan 24
# 9          cv       pickup 33
# 10         cv          suv 43
# 11     diesel          car 19
# 12     diesel          cuv 18
# 13     diesel      minivan  3
# 14     diesel       pickup 24
# 15     diesel          suv 21
# 16       flex          car 27
# 17       flex          cuv 12
# 18       flex      minivan  6
# 19       flex       pickup 15
# 20       flex          suv 15
# 21        hev          car 29
# 22        hev          cuv 26
# 23        hev      minivan  4
# 24        hev       pickup  6
# 25        hev          suv 11
# 26       phev          car 11
# 27       phev          cuv 20
# 28       phev      minivan  1
# 29       phev          suv  3

# Add other necessary variables

ds <- ds %>%
  mutate(tesla = ifelse(make == 'tesla', 1, 0))

# # Anonymize models
#
# unique_models <- ds %>%
#   distinct(model) %>%
#   collect()
#
# model_mapping <- unique_models %>%
#   mutate(
#     model_anon = replicate(
#       nrow(.),
#       paste0(sample(LETTERS, 6, replace = TRUE), collapse = "")
#     )
#   )
#
# # Confirm uniqueness
# length(unique(model_mapping$model)) == length(unique(model_mapping$model_anon))
#
# ds <- ds %>%
#   left_join(model_mapping, by = "model") %>%
#   select(-model) %>%
#   rename(model = model_anon)

# Merge duplicate dealer_ids based on common state and coordinates
# to form a harmonized dealer_id variable

dealers <- ds %>%
  distinct(state, latitude, longitude) %>%
  collect() %>%
  mutate(dealer_id_clean = row_number())

# Finalize the data

ds <- ds %>%
  left_join(dealers, by = c('state', 'latitude', 'longitude')) %>%
  select(-dealer_id) %>%
  # Select final variables
  select(
    dealer_id = dealer_id_clean,
    vehicle_id,
    inventory_type,
    powertrain,
    vehicle_type,
    year,
    make,
    model,
    msrp,
    price,
    class,
    miles,
    range,
    body_type,
    status_date,
    listing_year,
    state,
    latitude,
    longitude,
    tesla,
    age_years,
    mpg,
    gal100mi,
    kwhp100mi
  )

# Inflation adjust the listing prices and MSRPs

cpi <- read_csv(here::here('data', 'inflation-cpi.csv')) %>%
  clean_names() %>%
  pivot_longer(
    names_to = 'month',
    values_to = 'cpi',
    cols = -year
  ) %>%
  # Convert month to number and add a date
  mutate(
    date = mdy(paste(month, '01,', year)),
    month = month(date)
  )

# Adjust to 2024 as reference year
cpi2024 <- cpi %>%
  filter(year == 2024, month == 1) %>%
  pull(cpi)
cpi$cpi <- cpi$cpi / cpi2024
cpi <- cpi %>%
  filter(!is.na(cpi)) %>%
  rename(
    listing_year = year,
    cpi_listing = cpi
  ) %>%
  select(-date)

ds <- ds %>%
  mutate(month = as.integer(month(status_date))) %>%
  # Join CPI factors
  left_join(cpi, by = c('listing_year', 'month')) %>%
  mutate(
    price_raw = price,
    price = price_raw / cpi_listing,
    msrp_raw = msrp,
    msrp = msrp / cpi_listing
  )

# Convert prices to monthly budgets

loan_rates <- read_csv(here::here('data', 'RIFLPBCIANM60NM.csv')) %>%
  rename(date = observation_date, rate = RIFLPBCIANM60NM) %>%
  filter(!is.na(rate)) %>%
  filter(date > ymd('2016-01-01'), date < ymd('2025-01-01'))
rate_mean <- mean(loan_rates$rate) / 100
# 0.05553056 - we'll just use 5% for the base case

ds <- ds %>%
  mutate(
    price_monthly_base = calculate_monthly_payment(
      principal = price,
      annual_rate = 0.05,
      term_months = 68
    ),
    price_monthly_low = calculate_monthly_payment(
      principal = price,
      annual_rate = 0.04,
      term_months = 72
    ),
    price_monthly_high = calculate_monthly_payment(
      principal = price,
      annual_rate = 0.06,
      term_months = 60
    )
  )

# Write the "data/listings.parquet" file
ds %>%
  write_parquet(here::here('data', 'listings.parquet'))


# Prepare smaller listings_2024_new.parquet file filtered
load_ds() %>%
  filter(inventory_type == 'new') %>%
  filter(listing_year == 2024) %>%
  add_price_bins() %>%
  select(
    dealer_id,
    vehicle_id,
    inventory_type,
    powertrain,
    vehicle_type,
    make,
    price,
    price_bin,
    miles,
    price_monthly_base
  ) %>%
  write_parquet(here::here('data', 'listings_2024_new.parquet'))

# Prepare smaller listings_2018_new.parquet file filtered
load_ds() %>%
  filter(inventory_type == 'new') %>%
  filter(listing_year == 2018) %>%
  add_price_bins() %>%
  select(
    dealer_id,
    vehicle_id,
    inventory_type,
    powertrain,
    vehicle_type,
    make,
    price,
    price_bin,
    miles,
    price_monthly_base
  ) %>%
  write_parquet(here::here('data', 'listings_2018_new.parquet'))
