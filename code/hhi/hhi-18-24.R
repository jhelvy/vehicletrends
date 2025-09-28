# Load functions, libraries, and other settings
source(here::here("code", "0-setup.R"))

tract_dt <- read_parquet(here('data_local', 'tract_dt.parquet')) %>%
  rename(income = med_inc_hh) %>%
  mutate(
    income = parse_number(income),
    inc_bin = case_when(
      income <= 50000 ~ '< $50k',
      income > 50000 & income <= 85000 ~ '$50k - $85k',
      income > 85000 & income <= 120000 ~ '$85k - $120k',
      TRUE ~ '> $120k'
    )
  )
 
# Total counts only

counts_total_18 <- open_dataset(here(
  'data_local',
  'counts',
  'counts-2018',
  'counts_30.parquet'
))

counts_total_24 <- open_dataset(here(
  'data_local',
  'counts',
  'counts-2024',
  'counts_30.parquet'
)) 

font <- "Arial"
col_red <- "#FF0000"
col_blue <- "#0000FF"

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

# HHI by powertrain ----

hhi_make <- bind_rows(
  get_hhi(counts_total_18, make) %>%
    mutate(year = "2018"),
  get_hhi(counts_total_24, make) %>%
    mutate(year = "2024")
)

hhi_make %>%
  clean_factors_powertrain() %>%
  ggplot() +
  geom_boxplot(
    aes(
      x = hhi,
      y = powertrain,
      fill = year
    ),
    width = 0.5,
    outlier.shape = NA
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal_vgrid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    plot.title.position = "plot",
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA)
  ) +
  scale_fill_manual(values = c(col_red, col_blue)) +
  panel_border() +
  labs(
    x = 'HHI',
    y = 'Powertrain',
    fill = 'Year',
    title = 'Vehicle brand HHI by powertrain',
    subtitle = 'Higher number indicates greater concentration by brand'
  )

ggsave(
  here("figs", "hhi_make_powertrain.png"),
  width = 7,
  height = 5,
  dpi = 300
)

# HHI vehicle type ----

hhi_type <- bind_rows(
  get_hhi(counts_total_18, vehicle_type) %>%
    mutate(year = "2018"),
  get_hhi(counts_total_24, vehicle_type) %>%
    mutate(year = "2024")
)

hhi_type %>%
  clean_factors_powertrain() %>%
  ggplot() +
  geom_boxplot(
    aes(
      x = hhi,
      y = powertrain,
      fill = year
    ),
    width = 0.5,
    outlier.shape = NA
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal_vgrid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    plot.title.position = "plot",
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA)
  ) +
  scale_fill_manual(values = c(col_red, col_blue)) +
  panel_border() +
  labs(
    x = 'HHI',
    y = 'Powertrain',
    fill = 'Year',
    title = 'Vehicle type HHI by powertrain',
    subtitle = 'Higher number indicates greater concentration by brand'
  )



# HHI price_bin ----

hhi_price <- bind_rows(
  get_hhi(counts_total_18, price_bin) %>%
    mutate(year = "2018"),
  get_hhi(counts_total_24, price_bin) %>%
    mutate(year = "2024")
)

hhi_price %>%
  clean_factors_powertrain() %>%
  ggplot() +
  geom_boxplot(
    aes(
      x = hhi,
      y = powertrain,
      fill = year
    ),
    width = 0.5,
    outlier.shape = NA
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal_vgrid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    plot.title.position = "plot",
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA)
  ) +
  scale_fill_manual(values = c(col_red, col_blue)) +
  panel_border() +
  labs(
    x = 'HHI',
    y = 'Powertrain',
    fill = 'Year',
    title = 'Price bin HHI by powertrain',
    subtitle = 'Higher number indicates greater concentration by brand'
  )


bind_rows(
  hhi_price %>%
    mutate(type = 'Price bin'),
  hhi_make %>%
    mutate(type = 'Vehicle Brand'),
  hhi_type %>%
    mutate(type = 'Vehicle Type'),
) %>%
  group_by(powertrain, type, year) %>%
  summarise(hhi = mean(hhi)) %>%
  clean_factors_powertrain() %>%
  pivot_wider(
    names_from = year,
    values_from = hhi
  ) %>%
  rename(y18 = `2018`, y24 = `2024`) %>%
  ggplot() +
  geom_segment(
    aes(
      x = y18,
      xend = y24,
      y = powertrain
    ),
    color = "grey60",
    linewidth = 0.8
  ) +
  geom_point(
    aes(
      x = y18,
      y = powertrain
    ),
    color = 'red',
    size = 3
  ) +
  geom_point(
    aes(
      x = y24,
      y = powertrain
    ),
    color = 'blue',
    size = 3
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  labs(
    x = 'HHI',
    y = 'Powertrain',
    title = "Change in mean HHI between <span style = 'color: red;'>2018</span> and <span style = 'color: blue;'>2024</span> by powertrain",
    subtitle = 'HHI separately computed by price bin, vehicle brand, and vehicle type'
  ) +
  facet_wrap(vars(type)) +
  theme_minimal_vgrid(font_family = font, font_size = 16) +
  theme(
    strip.background = element_rect("grey80"),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.title = element_markdown(),
    panel.background = element_rect(fill = 'white', color = NA),
    plot.background = element_rect(fill = 'white', color = NA)
  ) +
  panel_border()
