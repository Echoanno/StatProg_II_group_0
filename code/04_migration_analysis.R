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
  ggplot(
    aes(
      x = year,
      y = mean_value,
      color = MONATSZAHL
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Migration trends in Munich, 2000–2024",
    subtitle = "Yearly mean values for migration inflow, outflow and migration balance",
    x = "Year",
    y = "Mean yearly migration",
    color = "Migration type"
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
    title = "Net migration balance in Munich, 2000–2024",
    subtitle = "Positive values indicate net migration gains",
    x = "Year",
    y = "Mean yearly migration balance"
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
    title = "Net migration balance in Munich (3-year rolling average)",
    subtitle = "Smoothed long-term migration trend",
    x = "Year",
    y = "Migration balance"
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
  ggplot(
    aes(
      x = year,
      y = mean_population
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(
    ~ AUSPRAEGUNG,
    scales = "free_y"
  ) +
  labs(
    title = "Population trends by continental groups in Munich",
    subtitle = "Yearly mean population by continent group",
    x = "Year",
    y = "Mean population"
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
  ggplot(
    aes(
      x = year,
      y = mean_population,
      color = AUSPRAEGUNG
    )
  ) +
  geom_line(linewidth = 1) +
  labs(
    title = "EU nationality trends in Munich",
    subtitle = "Yearly mean population by selected EU nationalities",
    x = "Year",
    y = "Mean population",
    color = "Nationality"
  ) +
  scale_color_manual(
    values = c(
      "bulgarisch" = "#1b9e77",
      "französisch" = "#d95f02",
      "griechisch" = "#7570b3",
      "insgesamt" = "#000000",
      "italienisch" = "#e7298a",
      "kroatisch" = "#66a61e",
      "österreichisch" = "#e6ab02",
      "polnisch" = "#a6761d",
      "rumänisch" = "#1f78b4",
      "ungarisch" = "#b2df8a"
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
  ggplot(
    aes(
      x = year,
      y = mean_population,
      color = MONATSZAHL
    )
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.8) +
  scale_color_manual(
    values = c(
      "EU-Staatsangehörigkeiten" = "#1b9e77",
      "Nicht-EU-Staatsangehörigkeiten" = "#d95f02"
    )
  ) +
  labs(
    title = "EU and non-EU demographic trends in Munich",
    subtitle = "Yearly mean population by citizenship group",
    x = "Year",
    y = "Mean population",
    color = "Group"
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
