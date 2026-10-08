library(tidyverse)
library(readxl)

# Import ------------------------------------------------------------------

visits <- read_excel("data-raw/pantry-visits-2025.xlsx", skip = 3)
population <- read_csv("data-raw/county-population.csv") |>
  # The source file lists Lane twice; a duplicate would double that county's visits
  distinct()

# Clean -------------------------------------------------------------------

visits_clean <- visits |>
  rename(county = County, month = Month, households = Households) |>
  mutate(
    households = as.numeric(households),
    month = ym(month)
  )

visits_with_population <- visits_clean |>
  left_join(population, by = "county", relationship = "many-to-one")

stopifnot(nrow(visits_with_population) == nrow(visits_clean))

# Statewide total ---------------------------------------------------------

visits_with_population |>
  summarize(total_households = sum(households, na.rm = TRUE))

# Households per 1,000 residents ------------------------------------------

county_rates <- visits_with_population |>
  group_by(county, population) |>
  summarize(households = sum(households, na.rm = TRUE)) |>
  mutate(per_1000 = households / population * 1000)

ggplot(county_rates, aes(x = per_1000, y = reorder(county, per_1000))) +
  geom_col() +
  labs(
    title = "Households served per 1,000 residents, 2025",
    x = NULL,
    y = NULL
  )

# Monthly trend -----------------------------------------------------------

visits_with_population |>
  group_by(month) |>
  summarize(households = sum(households, na.rm = TRUE)) |>
  ggplot(aes(x = month, y = households)) +
  geom_line() +
  labs(
    title = "Households served each month, 2025",
    x = NULL,
    y = NULL
  )
