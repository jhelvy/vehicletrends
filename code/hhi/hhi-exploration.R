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

make_totals <- make_counts %>%
    group_by(make) %>%
    summarise(total_vehicles = sum(n)) %>%
    arrange(desc(total_vehicles)) %>%
    collect()

get_hhi <- function(counts, var) {
    result <- counts %>%
        group_by(GEOID, powertrain, {{ var }}) %>%
        summarise(n = sum(n)) %>%
        ungroup() %>%
        collect() %>%
        group_by(GEOID, powertrain) %>%
        mutate(total = sum(n)) %>%
        ungroup() %>%
        mutate(
            p = n / total,
            p2 = p^2
        ) %>%
        group_by(GEOID, powertrain) %>%
        summarise(hhi = sum(p2))
    return(result)
}

get_hhi(counts_30, make)
