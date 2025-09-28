# Load functions, libraries, and other settings
source(here::here("code", "0-setup.R"))

font <- "Roboto Condensed"

nrel_dt <- open_dataset(here::here("data", "nrel_data_budget.parquet")) %>%
  filter(!is.na(budget), !is.na(pop_over_25), !is.na(income)) %>%
  collect()
tract_dt <- read_parquet(here('data', 'tract_dt.parquet')) %>%
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
pop <- nrel_dt %>%
  distinct(GEOID, pop_over_25) %>%
  group_by(GEOID) %>%
  summarise(pop_over_25 = mean(pop_over_25))

# Total counts only

counts_total <- open_dataset(here('data', 'counts', 'counts_60.parquet')) %>%
  # Add population
  left_join(pop, by = "GEOID")
counts_total_30 <- open_dataset(here('data', 'counts', 'counts_30.parquet')) %>%
  # Add population
  left_join(pop, by = "GEOID")
counts_total_90 <- open_dataset(here('data', 'counts', 'counts_90.parquet')) %>%
  # Add population
  left_join(pop, by = "GEOID")

# HHI by powertrain ----

hhi_powertrain <- counts_total %>%
  group_by(GEOID, powertrain, vehicle_type) %>%
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
  group_by(powertrain) %>%
  summarise(
    hhi_mean = mean(hhi),
    lower = quantile(hhi, 0.025),
    upper = quantile(hhi, 0.975)
  )

hhi_powertrain %>%
  clean_factors_powertrain() %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = hhi_mean,
      y = powertrain,
      xmin = lower,
      xmax = upper
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
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
    x = 'HHI',
    y = 'Powertrain',
    title = 'Vehicle type level HHI by powertrain',
    subtitle = 'Higher number indicates greater concentration by vehicle type\nPoint reflects mean across all census tracts, line is 95% lower and upper bounds'
  )

ggsave(
  here("figs", "hhi_type_powertrain.png"),
  width = 7,
  height = 5,
  dpi = 300
)

# HHI powertrain & price bin ----

hhi_powertrain <- counts_total %>%
  group_by(GEOID, price_bin, powertrain, vehicle_type) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  collect() %>%
  group_by(GEOID, price_bin, powertrain) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(
    p = n / total,
    p2 = p^2
  ) %>%
  group_by(GEOID, price_bin, powertrain) %>%
  summarise(hhi = sum(p2)) %>%
  group_by(price_bin, powertrain) %>%
  summarise(
    hhi_mean = mean(hhi),
    lower = quantile(hhi, 0.025),
    upper = quantile(hhi, 0.975)
  )

hhi_powertrain %>%
  clean_factors_powertrain() %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = hhi_mean,
      y = powertrain,
      xmin = lower,
      xmax = upper,
      color = price_bin
    ),
    position = position_dodge(width = 0.5)
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
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
    color = 'Price bin',
    x = 'HHI',
    y = 'Powertrain',
    title = 'Vehicle type level HHI by powertrain and price bin',
    subtitle = 'Higher number indicates greater concentration by vehicle type\nPoint reflects mean across all census tracts, line is 95% lower and upper bounds'
  )

ggsave(
  here("figs", "hhi_type_powertrain_price_bin.png"),
  width = 8,
  height = 5.5,
  dpi = 300
)

# HHI powertrain & urbanicity ----

hhi_powertrain <- counts_total %>%
  left_join(
    tract_dt %>%
      select(GEOID, class),
    by = 'GEOID'
  ) %>%
  group_by(GEOID, class, powertrain, vehicle_type) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  collect() %>%
  group_by(GEOID, class, powertrain) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(
    p = n / total,
    p2 = p^2
  ) %>%
  group_by(GEOID, class, powertrain) %>%
  summarise(hhi = sum(p2)) %>%
  group_by(class, powertrain) %>%
  summarise(
    hhi_mean = mean(hhi),
    lower = quantile(hhi, 0.025),
    upper = quantile(hhi, 0.975)
  )

hhi_powertrain %>%
  clean_factors_powertrain() %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = hhi_mean,
      y = powertrain,
      xmin = lower,
      xmax = upper,
      color = class
    ),
    position = position_dodge(width = 0.5)
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
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
    color = 'Urbanicity',
    x = 'HHI',
    y = 'Powertrain',
    title = 'Vehicle type level HHI by powertrain and urbanicity',
    subtitle = 'Higher number indicates greater concentration by vehicle type\nPoint reflects mean across all census tracts, line is 95% lower and upper bounds'
  )

ggsave(
  here("figs", "hhi_type_powertrain_urbanicity.png"),
  width = 8,
  height = 5.5,
  dpi = 300
)

# HHI powertrain & income ----

hhi_powertrain <- counts_total %>%
  left_join(
    tract_dt %>%
      select(GEOID, inc_bin),
    by = 'GEOID'
  ) %>%
  group_by(GEOID, inc_bin, powertrain, vehicle_type) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  collect() %>%
  group_by(GEOID, inc_bin, powertrain) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(
    p = n / total,
    p2 = p^2
  ) %>%
  group_by(GEOID, inc_bin, powertrain) %>%
  summarise(hhi = sum(p2)) %>%
  group_by(inc_bin, powertrain) %>%
  summarise(
    hhi_mean = mean(hhi),
    lower = quantile(hhi, 0.025),
    upper = quantile(hhi, 0.975)
  )

hhi_powertrain %>%
  clean_factors_powertrain() %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = hhi_mean,
      y = powertrain,
      xmin = lower,
      xmax = upper,
      color = inc_bin
    ),
    position = position_dodge(width = 0.5)
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
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
    color = 'Income Bin',
    x = 'HHI',
    y = 'Powertrain',
    title = 'Vehicle type level HHI by powertrain and income',
    subtitle = 'Higher number indicates greater concentration by vehicle type\nPoint reflects mean across all census tracts, line is 95% lower and upper bounds'
  )

ggsave(
  here("figs", "hhi_type_powertrain_income_bin.png"),
  width = 8,
  height = 5.5,
  dpi = 300
)

# HHI powertrain & ZEV status ----

hhi_powertrain <- counts_total %>%
  left_join(
    tract_dt %>%
      select(state, GEOID),
    by = 'GEOID'
  ) %>%
  mutate(zev = ifelse(state %in% zev_state, 'ZEV', 'Non-ZEV')) %>%
  group_by(GEOID, zev, powertrain, vehicle_type) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  collect() %>%
  group_by(GEOID, zev, powertrain) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(
    p = n / total,
    p2 = p^2
  ) %>%
  group_by(GEOID, zev, powertrain) %>%
  summarise(hhi = sum(p2)) %>%
  group_by(zev, powertrain) %>%
  summarise(
    hhi_mean = mean(hhi),
    lower = quantile(hhi, 0.025),
    upper = quantile(hhi, 0.975)
  )

hhi_powertrain %>%
  clean_factors_powertrain() %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = hhi_mean,
      y = powertrain,
      xmin = lower,
      xmax = upper,
      color = zev
    ),
    position = position_dodge(width = 0.5)
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
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
    color = 'ZEV status',
    x = 'HHI',
    y = 'Powertrain',
    title = 'Vehicle type level HHI by powertrain and ZEV status',
    subtitle = 'Higher number indicates greater concentration by vehicle type\nPoint reflects mean across all census tracts, line is 95% lower and upper bounds'
  )

ggsave(
  here("figs", "hhi_type_powertrain_zev.png"),
  width = 8,
  height = 5.5,
  dpi = 300
)

# HHI powertrain & HOV status ----

hhi_powertrain <- counts_total %>%
  left_join(
    tract_dt %>%
      select(state, GEOID),
    by = 'GEOID'
  ) %>%
  mutate(hov = ifelse(state %in% hov_state, 'HOV', 'Non-HOV')) %>%
  group_by(GEOID, hov, powertrain, vehicle_type) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  collect() %>%
  group_by(GEOID, hov, powertrain) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(
    p = n / total,
    p2 = p^2
  ) %>%
  group_by(GEOID, hov, powertrain) %>%
  summarise(hhi = sum(p2)) %>%
  group_by(hov, powertrain) %>%
  summarise(
    hhi_mean = mean(hhi),
    lower = quantile(hhi, 0.025),
    upper = quantile(hhi, 0.975)
  )

hhi_powertrain %>%
  clean_factors_powertrain() %>%
  mutate(hov = factor(hov, levels = c('Non-HOV', 'HOV'))) %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = hhi_mean,
      y = powertrain,
      xmin = lower,
      xmax = upper,
      color = hov
    ),
    position = position_dodge(width = 0.5)
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
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
    color = 'HOV status',
    x = 'HHI',
    y = 'Powertrain',
    title = 'Vehicle type level HHI by powertrain and HOV status',
    subtitle = 'Higher number indicates greater concentration by vehicle type\nPoint reflects mean across all census tracts, line is 95% lower and upper bounds'
  )

ggsave(
  here("figs", "hhi_type_powertrain_hov.png"),
  width = 8,
  height = 5.5,
  dpi = 300
)

# HHI powertrain & isochrone distance ----

hhi_powertrain_60 <- counts_total %>%
  group_by(GEOID, powertrain, vehicle_type) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  collect() %>%
  mutate(isochrone = 60)
hhi_powertrain_30 <- counts_total_30 %>%
  group_by(GEOID, powertrain, vehicle_type) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  collect() %>%
  mutate(isochrone = 30)
hhi_powertrain_90 <- counts_total_90 %>%
  group_by(GEOID, powertrain, vehicle_type) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  collect() %>%
  mutate(isochrone = 90)

hhi <- rbind(
  hhi_powertrain_30,
  hhi_powertrain_60,
  hhi_powertrain_90
) %>%
  group_by(GEOID, isochrone, powertrain) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(
    p = n / total,
    p2 = p^2
  ) %>%
  group_by(GEOID, isochrone, powertrain) %>%
  summarise(hhi = sum(p2)) %>%
  group_by(isochrone, powertrain) %>%
  summarise(
    hhi_mean = mean(hhi),
    lower = quantile(hhi, 0.025),
    upper = quantile(hhi, 0.975)
  )

hhi %>%
  clean_factors_powertrain() %>%
  mutate(isochrone = as.factor(isochrone)) %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = hhi_mean,
      y = powertrain,
      xmin = lower,
      xmax = upper,
      color = isochrone
    ),
    position = position_dodge(width = 0.5)
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
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
    color = 'Isochrone (min)',
    x = 'HHI',
    y = 'Powertrain',
    title = 'Vehicle type level HHI by powertrain and isochrone',
    subtitle = 'Higher number indicates greater concentration by vehicle type\nPoint reflects mean across all census tracts, line is 95% lower and upper bounds'
  )

ggsave(
  here("figs", "hhi_type_powertrain_isochrone.png"),
  width = 8,
  height = 5.5,
  dpi = 300
)


library(shiny)
library(bslib)

link_shiny <- tags$a(
  shiny::icon("github"),
  "Shiny",
  href = "https://github.com/rstudio/shiny",
  target = "_blank"
)
link_posit <- tags$a(
  shiny::icon("r-project"),
  "Posit",
  href = "https://posit.co",
  target = "_blank"
)

ui <- page_navbar(
  title = "My App",
  nav_panel(title = "One", p("First page content.")),
  nav_panel(title = "Two", p("Second page content.")),
  nav_panel("Three", p("Third page content.")),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(link_shiny),
    nav_item(link_posit)
  )
)

server <- function(...) {} # not used in this example

shinyApp(ui, server)
