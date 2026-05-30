library(tidyverse)

setwd("C:/Users/youpe/Desktop/Informatik bachelor/StatsProg2/Statprog_Project/StatProg_II_group_0")

age_data <- read_csv("data/processed/age_data.csv")

glimpse(age_data)

age_data %>%
  distinct(AUSPRAEGUNG) %>%
  arrange(AUSPRAEGUNG)

age_data %>%
  count(AUSPRAEGUNG, sort = TRUE)

age_data %>%
  group_by(AUSPRAEGUNG) %>%
  summarise(
    n_rows = n(),
    missing_wert = sum(is.na(WERT)),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  ) %>%
  arrange(desc(missing_wert))

age_selected <- age_data %>%
  filter(AUSPRAEGUNG %in% c(
    "noch nicht Schulpflichtige (0 bis 5 Jahre)",
    "Schulpflichtige (6 bis 14 Jahre)",
    "Berufsschulpflichtige (15 bis 17 Jahre)",
    "Erwerbsfähige (15 bis 64 Jahre)",
    "Rentner*innen (65 Jahre und älter)"
  ))

age_selected %>% 
    distinct(AUSPRAEGUNG)

age_yearly <- age_selected %>%
    filter(year >= 2000, year <= 2024) %>%
    group_by(AUSPRAEGUNG, year) %>%
    summarise(
        mean_population = mean(WERT, na.rm = TRUE),
        .groups = "drop"
    )

age_wide <- age_yearly %>%
    pivot_wider(
        names_from = AUSPRAEGUNG,
        values_from = mean_population
    )

age_structure <- age_wide %>%
    mutate(
        `18 bis 64 Jahre` = 
        `Erwerbsfähige (15 bis 64 Jahre)` -
         `Berufsschulpflichtige (15 bis 17 Jahre)`
    )

age_structure_long <- age_structure %>%
    select(
     year,
    `noch nicht Schulpflichtige (0 bis 5 Jahre)`,
    `Schulpflichtige (6 bis 14 Jahre)`,
    `Berufsschulpflichtige (15 bis 17 Jahre)`,
    `18 bis 64 Jahre`,
    `Rentner*innen (65 Jahre und älter)`
    ) %>% 
    pivot_longer(
        cols = -year,
        names_to = "age_group",
        values_to = "population"
    )


age_trend_plot <- age_structure_long %>%
    ggplot(aes(x = year, y = population, color = age_group)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    labs(
        title = "Population trends by age group in Munich, 2000–2024",
        x = "Year",
        y = "Mean population",
        color = "Age group"
  ) +
  theme_minimal()

age_trend_plot

ggsave(
    filename = "figures/age_trend_plot.png",
    plot = age_trend_plot,
    width = 9,
    height = 6,
    dpi = 300
)

age_trend_facet_plot <- ggplot(
  age_structure_long,
  aes(
    x = year,
    y = population
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(
    ~ age_group,
    scales = "free_y"
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Population trends by age group in Munich, 2000–2024",
    subtitle = "Yearly mean population, shown with separate y-axes for each age group",
    x = "Year",
    y = "Mean population"
  ) +
  theme_minimal()

ggsave(
  file = "figures/age_trend_facet_plot.png",
  plot = age_trend_facet_plot,
  width = 10,
  height= 8,
  dpi = 300
)

age_structure_share <- age_structure_long %>%
    group_by (year) %>%
    mutate(
      total_population =sum(population,na.rm = TRUE),
      population_share = population/total_population
    ) %>%
    ungroup()

age_share_selected_years <- age_structure_share %>%
    filter(year %in% c(2000,2010,2020,2024))

age_share_bar_plot <- age_share_selected_years %>%
    ggplot(aes(x = factor(year), y = population_share, fill = age_group)) +
    geom_col() +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "Age structure of Munich's population in selected years",
      subtitle = "Comparison of age group shares in 2000, 2010, 2020 and 2024",
      x = "Year",
      y = "Share of population",
      fill = "Age group"
  ) +
  theme_minimal()
 
 ggsave(
  filename = "figures/age_share_bar_plot.png",
  plot = age_share_bar_plot,
  width = 9,
  height = 6,
  dpi = 300 
 )

 age_change_table <- age_structure_long %>%
  filter(year %in% c(2000, 2024)) %>%
  select(year, age_group, population) %>%
  pivot_wider(
    names_from = year,
    values_from = population
  ) %>%
  mutate(
    absolute_change = `2024` - `2000`,
    relative_change = absolute_change / `2000` * 100
  ) %>%
  arrange(desc(relative_change))

write_csv(age_change_table, "data/processed/age_change_table.csv")

write_csv(
  age_structure_long,
  "data/processed/age_structure_long.csv"
)

write_csv(
  age_structure_share,
  "data/processed/age_structure_share.csv"
)
