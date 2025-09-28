# Load functions, libraries, and other settings
source(here::here("code", "0-setup.R"))

font <- "Roboto Condensed"

# State border geometries
states_sf <- readRDS(here::here('data', 'states_sf.Rds'))

# Get census tract geometries for the entire US
tracts <- readRDS(here::here('data', 'tracts.Rds')) %>%
  select(GEOID = geoid, geometry)

# HHI by powertrain ----

counts_total <- open_dataset(here('data', 'counts', 'counts_total.parquet'))
hhi_powertrain <- counts_total %>%
  group_by(GEOID, powertrain, make) %>%
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
  summarise(hhi = sum(p2)) %>%
  clean_factors_powertrain() %>%
  filter(powertrain %in% c('BEV', 'Hybrid', 'Conventional'))

# Join the time data with geographic data
tracts_with_hhi <- tracts %>%
  left_join(hhi_powertrain, by = "GEOID")

map <- tracts_with_hhi %>%
  filter(!is.na(powertrain)) %>%
  ggplot() +
  geom_sf(aes(fill = hhi), color = NA) +
  # Add state borders
  geom_sf(data = states_sf, fill = NA, color = "#808080", size = 0.4) +
  facet_wrap(vars(powertrain), ncol = 3) +
  viridis::scale_fill_viridis(
    option = "inferno",
    name = "Vehicle brand level HHI",
    na.value = "grey85",
    # n.breaks = 6  # You can adjust the number of breaks
    # Alternatively, you can specify exact breaks:
    # breaks = seq(0, 1, 0.2),
    direction = -1
  ) +
  theme_minimal(base_family = font) +
  theme(
    # Center the title
    plot.title = element_text(hjust = 0.5, size = 24),
    # Make facet labels larger
    strip.text = element_text(size = 16, face = "bold"),
    # Adjust legend
    legend.position = "bottom",
    legend.title = element_text(size = 18, hjust = 0.5),
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.2, "cm"), # Make legend boxes larger
    legend.title.position = "top", # Move legend title to top
    # Keep other existing theme elements
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA)
  ) +
  labs(y = NULL)

ggsave(
  here::here('figs', 'hhi-powertrain-map.png'),
  map,
  width = 20,
  height = 10
)
