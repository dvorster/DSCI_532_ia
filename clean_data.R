library(tidyverse)
library(arrow)

# Load and clean raw crime data
df_raw <- read_csv("data/raw/crime_rate_data_raw.csv") %>%
  select(-source, -url) %>%
  rename(city = department_name, state_id = ORI) %>%
  mutate(
    city = str_split_i(city, ",", 1), # Equivalent to partition[0]
    state_id = str_sub(state_id, 1, 2)
  )

# Load and clean cities data
cities <- read_csv("data/raw/uscities_raw.csv") %>%
  filter(state_name != "Puerto Rico") %>%
  select(city, state_id, lat, lng)

# Merge datasets
df_merged <- inner_join(df_raw, cities, by = c("city", "state_id"))

# Save to a CSV file
write_csv(df_merged, "processed.csv")

