# Load functions, libraries, and other settings
source(here::here("code", "0-setup.R"))

nrel_dt <- open_dataset(here::here("data", "nrel_data_budget.parquet")) %>%
  filter(!is.na(budget), !is.na(pop_over_25), !is.na(income)) %>%
  collect()
tract_dt <- read_parquet(here('data', 'tract_dt.parquet'))
pop <- nrel_dt %>%
  distinct(GEOID, pop_over_25) %>%
  group_by(GEOID) %>%
  summarise(pop_over_25 = mean(pop_over_25))

ds_total <- open_dataset(here(
  'data',
  'counts-2024',
  'counts_30.parquet'
))

# Powertrain analysis ----

dt_summaries <- ds_total %>%
  group_by(GEOID, powertrain) %>%
  summarise(n = sum(n)) %>%
  collect() %>%
  group_by(GEOID) %>%
  mutate(p = n / sum(n)) %>%
  clean_factors_powertrain()

dt_summaries %>%
  ggplot() +
  geom_boxplot(
    aes(
      x = p,
      y = powertrain
    ),
    width = 0.5,
    outlier.shape = NA
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1),
    labels = scales::percent
  ) +
  theme_minimal_vgrid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    plot.title.position = "plot",
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA),
    legend.position = 'none'
  ) +
  panel_border() +
  labs(
    x = 'Share of all local listings',
    y = 'Powertrain',
    title = 'Share of local vehicle listings by powertrain'
  )

# ggsave(
#   here("figs", "percent-market-powertrain.png"),
#   height = 5,
#   width = 8
# )

# Powertrain by price bin ----

dt_summaries <- ds_total %>%
  group_by(GEOID, powertrain, price_bin) %>%
  summarise(n = sum(n)) %>%
  collect() %>%
  group_by(GEOID, price_bin) %>%
  mutate(p = n / sum(n)) %>%
  clean_factors_powertrain()

dt_summaries %>%
  ggplot() +
  geom_boxplot(
    aes(
      x = p,
      y = powertrain,
      fill = price_bin
    ),
    width = 0.5,
    outlier.shape = NA
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1),
    labels = scales::percent
  ) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme_minimal_vgrid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    plot.title.position = "plot",
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA)
  ) +
  panel_border() +
  labs(
    fill = 'Price bin',
    x = 'Share of all local listings',
    y = 'Powertrain',
    title = 'Share of local vehicle listings by powertrain'
  )

# Vehicle type analysis ----

dt_summaries <- ds_total %>%
  group_by(GEOID, vehicle_type) %>%
  summarise(n = sum(n)) %>%
  collect() %>%
  group_by(GEOID) %>%
  mutate(p = n / sum(n)) %>%
  clean_factors_vehicle_type()

dt_summaries %>%
  ggplot() +
  geom_boxplot(
    aes(
      x = p,
      y = vehicle_type
    ),
    width = 0.5,
    outlier.shape = NA
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1),
    labels = scales::percent
  ) +
  theme_minimal_vgrid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    plot.title.position = "plot",
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA),
    legend.position = 'none'
  ) +
  panel_border() +
  labs(
    x = 'Share of all local listings',
    y = 'Vehicle Type',
    title = 'Share of local vehicle listings by vehicle type'
  )

# ggsave(
#   here("figs", "percent-market-vehicle-type.png"),
#   height = 5,
#   width = 8
# )

# Powertrain - urbanicity ----

dt_summaries <- ds_total %>%
  left_join(
    tract_dt %>%
      select(GEOID, class),
    by = 'GEOID'
  ) %>%
  group_by(GEOID, class, powertrain) %>%
  summarise(n = sum(n)) %>%
  collect() %>%
  group_by(GEOID, class) %>%
  mutate(p = n / sum(n)) %>%
  clean_factors_powertrain()

dt_summaries %>%
  ggplot() +
  geom_boxplot(
    aes(
      x = p,
      y = powertrain,
      fill = class
    ),
    width = 0.5,
    outlier.shape = NA
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1),
    labels = scales::percent
  ) +
  theme_minimal_grid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    plot.title.position = "plot",
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA)
  ) +
  panel_border() +
  labs(
    fill = 'Urbanicity',
    x = 'Share of all local listings',
    y = 'Powertrain',
    title = 'Share of local vehicle listings by powertrain'
  )

# ggsave(
#   here("figs", "percent-market-powertrain-urbanicity.png"),
#   height = 5,
#   width = 8
# )

# Vehicle type - urbanicity ----

dt_summaries <- ds_total %>%
  left_join(
    tract_dt %>%
      select(GEOID, class),
    by = 'GEOID'
  ) %>%
  group_by(GEOID, class, vehicle_type) %>%
  summarise(n = sum(n)) %>%
  collect() %>%
  group_by(GEOID, class) %>%
  mutate(p = n / sum(n)) %>%
  clean_factors_vehicle_type()

dt_summaries %>%
  ggplot() +
  geom_boxplot(
    aes(
      x = p,
      y = vehicle_type,
      fill = class
    ),
    width = 0.5,
    outlier.shape = NA
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1),
    labels = scales::percent
  ) +
  theme_minimal_vgrid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    plot.title.position = "plot",
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA)
  ) +
  panel_border() +
  labs(
    fill = 'Urbanicity',
    x = 'Share of all local listings',
    y = 'Vehicle type',
    title = 'Share of local vehicle listings by vehicle type'
  )

# ggsave(
#   here("figs", "percent-market-vehicle-type-urbanicity.png"),
#   height = 5,
#   width = 8
# )
