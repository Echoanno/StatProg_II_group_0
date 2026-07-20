library(tidyverse)

age_share <- read_csv("data/processed/age_structure_share.csv")
migration_yearly <- read_csv("data/processed/migration_yearly.csv")
eu_non_eu_yearly <- read_csv("data/processed/eu_non_eu_yearly.csv")

age_indicators <- age_share %>%
    filter(year >= 2000, year <= 2024) %>%
    mutate(age_category = case_when(
        age_group == "Rentner*innen (65 Jahre und älter)" ~ "share_65_plus",
        age_group == "18 bis 64 Jahre" ~ "share_working_age", 
        age_group %in% c(
            "noch nicht Schulpflichtige (0 bis 5 Jahre)",
            "Schulpflichtige (6 bis 14 Jahre)",
            "Berufsschulpflichtige (15 bis 17 Jahre)"
    ) ~ "share_younger",
    TRUE ~ NA_character_
    )) %>%
    filter(!is.na(age_category)) %>%
    group_by(year, age_category) %>%
    summarise(
        value = sum(population_share, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = age_category,
        values_from = value
    )

migration_indicators <- migration_yearly %>%
    filter( year >= 2000, year <= 2024) %>%
    mutate(indicator = case_when(
        MONATSZAHL == "Wanderungssaldo" ~ "migration_balance",
        MONATSZAHL == "Zugezogene" ~ "migration_inflows",
        MONATSZAHL == "Weggezogene" ~ "migration_outflows",
        TRUE ~ NA_character_
    )) %>%
    filter(!is.na(indicator)) %>%
    select(year, indicator, mean_value) %>%
    pivot_wider(
        names_from = indicator,
        values_from = mean_value
    )

eu_non_eu_indicators <- eu_non_eu_yearly %>%
  filter(year >= 2000, year <= 2024) %>%
  mutate(indicator = case_when(
    MONATSZAHL == "EU-Staatsangehörigkeiten" ~ "eu_population",
    MONATSZAHL == "Nicht-EU-Staatsangehörigkeiten" ~ "non_eu_population",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(indicator)) %>%
  select(year, indicator, mean_population) %>%
  pivot_wider(
    names_from = indicator,
    values_from = mean_population
  )


age_migration_combined <- age_indicators %>%
    left_join(migration_indicators, by = "year") %>%
    left_join(eu_non_eu_indicators, by = "year")

write_csv(age_migration_combined, "data/processed/age_migration_combined.csv")

age_migration_indexed <- age_migration_combined %>%
    mutate(across(
        -year,
        ~ .x/ .x[year == 2000] * 100
    ))

write_csv(
  age_migration_indexed,
  "data/processed/age_migration_indexed.csv"
)

age_migration_indexed_long <- age_migration_indexed %>%
  pivot_longer(
    cols = -year,
    names_to = "indicator",
    values_to = "index_value"
  ) %>%
  mutate(
    indicator_label = recode(
      indicator,
      "share_65_plus" = "Retirement age share (65+)",
      "share_working_age" = "Working-age share (18-64)",
      "share_younger" = "Younger age groups share (0-17)",
      "migration_balance" = "Net migration balance",
      "migration_inflows" = "Arrivals",
      "migration_outflows" = "Departures",
      "eu_population" = "EU citizens",
      "non_eu_population" = "Non-EU citizens"
    )
  )

age_migration_indexed_plot <- age_migration_indexed_long %>%
  ggplot(aes(x = year, y = index_value, color = indicator_label)) +  
  geom_line(linewidth = 1.0) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black") +
  labs(
    title = "Indexed age-structure and migration indicators in Munich, 2000-2024",
    subtitle = "All indicators are indexed to 2000 = 100.",
    x = "Year",
    y = "Index value (2000 = 100)",
    color = "Indicator",
    caption = "Source: Munich Open Data, Monatszahlen Bevölkerung; own calculations."
  ) + 
  theme_minimal()

ggsave(
  filename = "figures/age_migration_indexed_plot.png",
  plot = age_migration_indexed_plot,
  width = 10,
  height = 6,
  dpi = 300
)
