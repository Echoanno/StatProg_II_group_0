# Household structure analysis in Munich
pop_data<-read.csv("data/raw/monatszahlen_bevoelkerung.csv")
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)

# 1.Trend

# Filter by Year (2012-2024), December baseline, and Household sizes
household_base_filtered <- pop_data %>%
  filter(
    JAHR >= 2012 & JAHR <= 2024,
    MONATSZAHL == "Haushalte nach Personenzahl",
    str_detect(MONAT, "12$"), # Select December to represent the annual stock data
    AUSPRAEGUNG %in% c(
      "1 Person",
      "2 Personen",
      "3 Personen",
      "4 Personen",
      "5 Personen und mehr"
    )
  ) %>%
  select(year = JAHR, category = AUSPRAEGUNG, count = WERT)
write_csv(household_base_filtered, "data/processed/household_base_filtered.csv")
# Based on data availability in the Munich Open Data platform, our household baseline analysis focuses on the period from 2012 to 2024.

# Pivot from long to wide format to examine the clean structure
household_base_wide <- household_base_filtered %>%
  pivot_wider(
    names_from = category,
    values_from = count
  )
print(household_base_wide)

# Calculate row totals and convert absolute counts into percentages
household_percentage_wide <- household_base_wide %>%
  mutate(
    total_households = `1 Person` + `2 Personen` + `3 Personen` + `4 Personen` + `5 Personen und mehr`
  ) %>%
  mutate(
    across(
      c(`1 Person`, `2 Personen`, `3 Personen`, `4 Personen`, `5 Personen und mehr`),
      ~ (. / total_households) * 100
    )
  )

# Pivot back to long format for ggplot2 line mapping
household_size_baseline <- household_percentage_wide %>%
  pivot_longer(
    cols = c(`1 Person`, `2 Personen`, `3 Personen`, `4 Personen`, `5 Personen und mehr`),
    names_to = "household_size",
    values_to = "percentage"
  ) %>%
  select(year, household_size, percentage) %>%
  arrange(year, household_size)

write_csv(household_size_baseline, "data/processed/household_size_baseline.csv")

# Preview the long format output
print(household_size_baseline)


# Ensure the dataset is loaded (using the long format table from the previous step)
# household_size_baseline should already be in R environment

# Plot the household percentage trend
household_percentage_plot <- ggplot(
  household_size_baseline, 
  aes(
    x = year, 
    y = percentage, 
    color = household_size, 
    group = household_size)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Household Size Trends in Munich (2012-2024)",
    subtitle = "Percentage share of total households by person count.",
    x = "Year",
    y = "Percentage of Total Households (%)",
    color = "Household Type"
  ) +
  theme_minimal()
household_percentage_plot

# Save the plot
ggsave(
  filename = "figures/household_percentage_plot.png",
  plot = household_percentage_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# Plot the household count trend
household_count_plot <- ggplot(
  household_base_filtered,
  aes(x = year,
      y = count,
      color = category,
      group = category)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Household Size Count Trends in Munich (2012-2024)",
    subtitle = "Count change by person count.",
    x = "Year",
    y = "Count of Total Household",
    color = "Household Type"
  ) +
  theme_minimal()
household_count_plot

# Save the plot
ggsave(
  filename = "figures/household_count_plot.png",
  plot = household_count_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# 2.Strongest changes

# Changes in relative household shares between 2012 and 2024

household_share_change <- household_size_baseline %>%
  filter(year %in% c(2012, 2024)) %>%
  pivot_wider(
    names_from = year,
    values_from = percentage,
    names_prefix = "percentage_"
  ) %>%
  mutate(
    percentage_point_change = percentage_2024 - percentage_2012
  ) %>%
  arrange(desc(percentage_point_change))

print(household_share_change)

# Plot the percentage change
household_share_change_plot <- ggplot(
  household_share_change,
  aes(
    x = reorder(household_size, percentage_point_change),
    y = percentage_point_change
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Change in Household Shares in Munich (2012-2024)",
    subtitle = "Percentage point change by household type.",
    x = "Household Type",
    y = "Percentage Point Change"
  ) +
  theme_minimal()

household_share_change_plot

# Save the plot
ggsave(
  filename = "figures/household_share_change_plot.png",
  plot = household_share_change_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# 3.The trend towards smaller in Munich

# Regroup household types into broader household categories
household_grouped <- household_base_filtered %>%
  mutate(
    household_group = case_when(
      category == "1 Person" ~ "1 Person",
      category == "2 Personen" ~ "2 Personen",
      category %in% c("3 Personen", "4 Personen", "5 Personen und mehr") ~ "3+ Personen",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(year, household_group) %>%
  summarise(
    count = sum(count, na.rm = TRUE),
    .groups = "drop"
  )

print(household_grouped)

write_csv(household_grouped, "data/processed/household_grouped.csv")

# Calculate percentage share of regrouped household categories
household_grouped_percentage <- household_grouped %>%
  group_by(year) %>%
  mutate(
    total_households = sum(count, na.rm = TRUE),
    percentage = count / total_households * 100
  ) %>%
  ungroup()

print(household_grouped_percentage)

write_csv(household_grouped_percentage, "data/processed/household_grouped_percentage.csv")

# Plot regrouped household structure trend
household_grouped_percentage_plot <- ggplot(
  household_grouped_percentage,
  aes(
    x = year,
    y = percentage,
    color = household_group,
    group = household_group
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Regrouped Household Structure in Munich (2012-2024)",
    subtitle = "Percentage share of 1-person, 2-person, and 3+ person households.",
    x = "Year",
    y = "Percentage of Total Households (%)",
    color = "Household Group"
  ) +
  theme_minimal()

household_grouped_percentage_plot

# Save
ggsave(
  filename = "figures/household_grouped_percentage_plot.png",
  plot = household_grouped_percentage_plot,
  width = 9,
  height = 6,
  dpi = 300
)