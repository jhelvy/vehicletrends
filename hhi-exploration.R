# Load in setup
source(here::here("code", "0-setup.R"))

cars <- open_dataset(here('data_local', 'listings.parquet'))

names(cars)
