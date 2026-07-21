library(tidyverse)

age_structure_share <- read_csv("data/processed/age_structure_share.csv")

age_65_share <- age_structure_share %>%
  filter(
    year >= 2000,
    year <= 2024,
    str_detect(age_group, "Rentner")
  ) %>%
  transmute(
    year,
    population_share,
    population_share_percent = population_share * 100
  )

age_65_share_model <- lm(
  population_share_percent ~ year,
  data = age_65_share
)

age_65_share_model_summary <- tibble(
  term = names(coef(age_65_share_model)),
  estimate = unname(coef(age_65_share_model)),
  standard_error = summary(age_65_share_model)$coefficients[, "Std. Error"],
  statistic = summary(age_65_share_model)$coefficients[, "t value"],
  p_value = summary(age_65_share_model)$coefficients[, "Pr(>|t|)"]
)

write_csv(
  age_65_share,
  "data/processed/age_65_share_model_data.csv"
)

write_csv(
  age_65_share_model_summary,
  "data/processed/age_65_share_model_summary.csv"
)

age_65_share_model_plot <- age_65_share %>%
  ggplot(aes(x = year, y = population_share_percent)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  labs(
    title = "Trend in Munich's 65+ population share, 2000-2024",
    subtitle = "Linear model fitted to yearly mean population shares",
    x = "Year",
    y = "65+ population share"
  ) +
  theme_minimal()

ggsave(
  filename = "figures/age_65_share_model_plot.png",
  plot = age_65_share_model_plot,
  width = 8,
  height = 5,
  dpi = 300
)
