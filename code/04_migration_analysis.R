# Migration Analysis in Munich
# StatProg II Project

library(tidyverse)

# Load processed population dataset

population_data <- read_csv(
  "data/processed/population_data.csv"
)

# Explore migration-related categories

population_data %>%
  distinct(MONATSZAHL)

# Create migration dataset

migration_data <- population_data %>%
  filter(
    MONATSZAHL %in% c(
      "Zugezogene",
      "Weggezogene",
      "Wanderungssaldo"
    )
  )

# Aggregate monthly data into yearly means

migration_yearly <- migration_data %>%
  filter(year >= 2000, year <= 2024) %>%
  group_by(MONATSZAHL, year) %>%
  summarise(
    mean_value = mean(WERT, na.rm = TRUE),
    .groups = "drop"
  )

# Plot migration trends over time

migration_trend_plot <- migration_yearly %>%
  mutate(
    migration_type = recode(
      MONATSZAHL,
      "Zugezogene" = "Arrivals",
      "Weggezogene" = "Departures",
      "Wanderungssaldo" = "Net migration"
    )
  ) %>%
  ggplot(
    aes(
      x = year,
      y = mean_value,
      color = migration_type
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Migration dynamics in Munich, 2000-2024",
    subtitle = "Arrivals and departures are shown as yearly averages of monthly values.",
    x = "Year",
    y = "Yearly average number of people",
    color = "Migration type",
    caption = "Source: Munich Open Data, Monatszahlen Bevölkerung; own calculations."
  ) +
  theme_minimal()

migration_trend_plot

# Save migration trend plot

ggsave(
  filename = "figures/migration_trend_plot.png",
  plot = migration_trend_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# Migration balance analysis

migration_balance <- migration_yearly %>%
  filter(
    MONATSZAHL == "Wanderungssaldo"
  )

# Migration balance plot

migration_balance_plot <- migration_balance %>%
  ggplot(
    aes(
      x = year,
      y = mean_value
    )
  ) +
  geom_line(
    linewidth = 1.2,
    color = "#d95f02"
  ) +
  geom_point(
    size = 2,
    color = "#d95f02"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray40"
  ) +
  labs(
    title = "Net migration balance in Munich, 2000-2024",
    subtitle = "Positive values indicate net migration gains.",
    x = "Year",
    y = "Average monthly net migration balance",
    caption = "Source: Munich Open Data, Monatszahlen Bevölkerung; own calculations."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

migration_balance_plot

# Save migration balance plot

ggsave(
  filename = "figures/migration_balance_plot.png",
  plot = migration_balance_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# 3-year rolling average for migration balance

library(zoo)

migration_balance_rolling <- migration_balance %>%
  arrange(year) %>%
  mutate(
    rolling_avg = zoo::rollmean(
      mean_value,
      k = 3,
      fill = NA,
      align = "center"
    )
  )

migration_balance_rolling_plot <- migration_balance_rolling %>%
  ggplot(aes(x = year)) +
  
  # Original yearly values
  geom_line(
    aes(y = mean_value),
    color = "gray80",
    linewidth = 0.7
  ) +
  
  geom_point(
    aes(y = mean_value),
    color = "gray80",
    size = 1.2
  ) +
  
  # Rolling average
  geom_line(
    aes(y = rolling_avg),
    color = "#d95f02",
    linewidth = 1.5
  ) +
  
  geom_point(
    aes(y = rolling_avg),
    color = "#d95f02",
    size = 2
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray40"
  ) +
  
  labs(
    title = "Net migration balance in Munich, 2000-2024",
    subtitle = "The orange line shows a 3-year rolling average of monthly net migration balance.",
    x = "Year",
    y = "Average monthly net migration balance",
    caption = "Source: Munich Open Data, Monatszahlen Bevölkerung; own calculations."
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold")
  )

migration_balance_rolling_plot

# Save rolling average plot

ggsave(
  filename = "figures/migration_balance_rolling_plot.png",
  plot = migration_balance_rolling_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# Explore continental groups

continent_groups <- population_data %>%
  filter(
    MONATSZAHL == "Kontinente"
  ) %>%
  distinct(AUSPRAEGUNG)

continent_groups

# Create continent dataset

continent_data <- population_data %>%
  filter(
    MONATSZAHL == "Kontinente"
  )

# Aggregate continent data yearly

continent_yearly <- continent_data %>%
  filter(year >= 2000, year <= 2024) %>%
  group_by(AUSPRAEGUNG, year) %>%
  summarise(
    mean_population = mean(WERT, na.rm = TRUE),
    .groups = "drop"
  )

# Faceted continent trend plot

continent_facet_plot <- continent_yearly %>%
  mutate(
    continent_group = recode(
      AUSPRAEGUNG,
      "afrikanisch" = "African",
      "amerikanisch" = "American",
      "asiatisch" = "Asian",
      "australisch" = "Australian",
      "europäisch" = "European"
    )
  ) %>%
  ggplot(
    aes(
      x = year,
      y = mean_population
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(
    ~ continent_group,
    scales = "free_y"
  ) +
  labs(
    title = "Population by continental group in Munich, 2000-2024",
    subtitle = "Each panel uses its own y-axis to show group-specific changes.",
    x = "Year",
    y = "Yearly mean population",
    caption = "Source: Munich Open Data, Monatszahlen Bevölkerung; own calculations."
  ) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2020, 2024)
  ) +
  theme_minimal()

continent_facet_plot

# Save continent plot

ggsave(
  filename = "figures/continent_facet_plot.png",
  plot = continent_facet_plot,
  width = 10,
  height = 8,
  dpi = 300
)

# Explore EU nationality groups

eu_groups <- population_data %>%
  filter(
    MONATSZAHL == "EU-Staatsangehörigkeiten"
  ) %>%
  distinct(AUSPRAEGUNG)

eu_groups

# Create EU dataset

eu_data <- population_data %>%
  filter(
    MONATSZAHL == "EU-Staatsangehörigkeiten"
  )

# Aggregate EU data yearly

eu_yearly <- eu_data %>%
  filter(year >= 2000, year <= 2024) %>%
  group_by(AUSPRAEGUNG, year) %>%
  summarise(
    mean_population = mean(WERT, na.rm = TRUE),
    .groups = "drop"
  )

# EU nationality trend plot

eu_trend_plot <- eu_yearly %>%
  mutate(
    nationality = recode(
      AUSPRAEGUNG,
      "bulgarisch" = "Bulgarian",
      "französisch" = "French",
      "griechisch" = "Greek",
      "insgesamt" = "Total",
      "italienisch" = "Italian",
      "kroatisch" = "Croatian",
      "österreichisch" = "Austrian",
      "polnisch" = "Polish",
      "rumänisch" = "Romanian",
      "ungarisch" = "Hungarian"
    )
  ) %>%
  ggplot(
    aes(
      x = year,
      y = mean_population,
      color = nationality
    )
  ) +
  geom_line(linewidth = 1) +
  labs(
    title = "EU nationality groups in Munich, 2000-2024",
    subtitle = "Lines show yearly mean population by selected EU nationality group.",
    x = "Year",
    y = "Yearly mean population",
    color = "Nationality",
    caption = "Source: Munich Open Data, Monatszahlen Bevölkerung; own calculations."
  ) +
  scale_color_manual(
    values = c(
      "Bulgarian" = "#1b9e77",
      "French" = "#d95f02",
      "Greek" = "#7570b3",
      "Total" = "#000000",
      "Italian" = "#e7298a",
      "Croatian" = "#66a61e",
      "Austrian" = "#e6ab02",
      "Polish" = "#a6761d",
      "Romanian" = "#1f78b4",
      "Hungarian" = "#b2df8a"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

eu_trend_plot

# Save EU trend plot

ggsave(
  filename = "figures/eu_trend_plot.png",
  plot = eu_trend_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# EU vs non-EU demographic trends

eu_non_eu_data <- population_data %>%
  filter(
    MONATSZAHL %in% c(
      "EU-Staatsangehörigkeiten",
      "Nicht-EU-Staatsangehörigkeiten"
    )
  ) %>%
  filter(AUSPRAEGUNG == "insgesamt")

# Aggregate yearly values

eu_non_eu_yearly <- eu_non_eu_data %>%
  filter(year >= 2000, year <= 2024) %>%
  group_by(MONATSZAHL, year) %>%
  summarise(
    mean_population = mean(WERT, na.rm = TRUE),
    .groups = "drop"
  )

# Plot EU vs non-EU trends

eu_non_eu_plot <- eu_non_eu_yearly %>%
  mutate(
    citizenship_group = recode(
      MONATSZAHL,
      "EU-Staatsangehörigkeiten" = "EU citizens",
      "Nicht-EU-Staatsangehörigkeiten" = "Non-EU citizens"
    )
  ) %>%
  ggplot(
    aes(
      x = year,
      y = mean_population,
      color = citizenship_group
    )
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  scale_color_manual(
    values = c(
      "EU citizens" = "#1b9e77",
      "Non-EU citizens" = "#d95f02"
    )
  ) +
  labs(
    title = "EU and non-EU populations in Munich, 2000-2024",
    subtitle = "Non-EU populations became larger again after the late 2010s.",
    x = "Year",
    y = "Yearly mean population",
    color = "Citizenship group",
    caption = "Source: Munich Open Data, Monatszahlen Bevölkerung; own calculations."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

eu_non_eu_plot

# Save plot

ggsave(
  filename = "figures/eu_non_eu_plot.png",
  plot = eu_non_eu_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# Summary table: migration change between 2000 and 2024

migration_change_table <- migration_yearly %>%
  filter(year %in% c(2000, 2024)) %>%
  pivot_wider(
    names_from = year,
    values_from = mean_value,
    names_prefix = "year_"
  ) %>%
  mutate(
    absolute_change = year_2024 - year_2000,
    relative_change_percent = absolute_change / year_2000 * 100
  ) %>%
  arrange(MONATSZAHL)

migration_change_table

# Summary table: highest and lowest yearly average migration values

migration_extreme_years <- migration_yearly %>%
  group_by(MONATSZAHL) %>%
  summarise(
    lowest_year = year[which.min(mean_value)],
    lowest_value = min(mean_value, na.rm = TRUE),
    highest_year = year[which.max(mean_value)],
    highest_value = max(mean_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(MONATSZAHL)

migration_extreme_years

# Summary table: EU and non-EU population change between 2000 and 2024

eu_non_eu_change_table <- eu_non_eu_yearly %>%
  filter(year %in% c(2000, 2024)) %>%
  pivot_wider(
    names_from = year,
    values_from = mean_population,
    names_prefix = "year_"
  ) %>%
  mutate(
    absolute_change = year_2024 - year_2000,
    relative_change_percent = absolute_change / year_2000 * 100
  ) %>%
  arrange(MONATSZAHL)

eu_non_eu_change_table

# Export processed migration tables

write_csv(
  migration_yearly,
  "data/processed/migration_yearly.csv"
)

write_csv(
  continent_yearly,
  "data/processed/continent_yearly.csv"
)

write_csv(
  eu_yearly,
  "data/processed/eu_yearly.csv"
)

write_csv(
  eu_non_eu_yearly,
  "data/processed/eu_non_eu_yearly.csv"
)

write_csv(
  migration_change_table,
  "data/processed/migration_change_table.csv"
)

write_csv(
  migration_extreme_years,
  "data/processed/migration_extreme_years.csv"
)

write_csv(
  eu_non_eu_change_table,
  "data/processed/eu_non_eu_change_table.csv"
)
