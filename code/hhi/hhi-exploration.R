# Load in setup
source(here::here("code", "0-setup.R"))

counts_30 <- open_dataset(here('data_local','counts', 'counts-2024','counts_60.parquet'))

counts_30 <- counts_30 %>% 
    select(-price_bin)

make_counts <- counts_30 %>% 
    select(GEOID, n, make) %>% 
    group_by(GEOID, make) %>% 
    summarise(n = sum(n), .groups = "drop") %>% 
    collect()
