library(tidyverse)
library(lubridate)

population_raw <- read_csv("data/raw/monatszahlen_bevoelkerung.csv")
population_raw_monthly <- population_raw %>% filter (MONAT != "Summe") 
population_raw_monthly <- population_raw_monthly %>% mutate(
    date = ymd(paste(MONAT, "01")),
    month = month(date),
    year = year(date)    
)

age_data <- population_raw_monthly %>% filter(MONATSZAHL == "Altersgruppen")
glimpse(age_data)

write_csv(age_data, "data/processed/age_data.csv")
write_csv(population_raw_monthly, "data/processed/population_data.csv")