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
    # Some months spell the county in lowercase ("multnomah")
    county = str_to_title(county),
    # Counts under 10 are suppressed in the source and shown as "<10"
    suppressed = households == "<10",
    households = as.numeric(households),
    # October to December use "2025-10" for some counties and "Oct 2025" for others
    month = coalesce(ym(month, quiet = TRUE), my(month, quiet = TRUE))
  )

stopifnot(!anyNA(visits_clean$month))

visits_with_population <- visits_clean |>
  left_join(population, by = "county", relationship = "many-to-one")

stopifnot(
  nrow(visits_with_population) == nrow(visits_clean),
  !anyNA(visits_with_population$population)
)

# Statewide total ---------------------------------------------------------

statewide_total <- sum(visits_with_population$households, na.rm = TRUE)

# Chart theme -------------------------------------------------------------

chart_blue <- "#2a78d6"
ink <- "#0b0b0b"
ink_secondary <- "#52514e"

theme_report <- function() {
  theme_minimal(base_family = "Inter", base_size = 11) +
    theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(face = "bold", size = 13, color = ink),
      plot.subtitle = element_text(color = ink_secondary, margin = margin(b = 12)),
      plot.caption = element_text(
        hjust = 0,
        color = ink_secondary,
        size = 8,
        lineheight = 1.2,
        margin = margin(t = 12)
      ),
      axis.text = element_text(color = ink, size = 10),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(16, 16, 16, 16)
    )
}

# Households per 1,000 residents ------------------------------------------

county_rates <- visits_with_population |>
  summarize(
    households = sum(households, na.rm = TRUE),
    suppressed_months = sum(suppressed),
    .by = c(county, population)
  ) |>
  mutate(
    per_1000 = households / population * 1000,
    county_label = if_else(suppressed_months > 0, str_c(county, "*"), county)
  )

per_1000_chart <- ggplot(
  county_rates,
  aes(x = per_1000, y = fct_reorder(county_label, per_1000))
) +
  geom_col(fill = chart_blue, width = 0.62) +
  geom_text(
    aes(label = round(per_1000)),
    hjust = 0,
    nudge_x = 2,
    family = "Inter",
    size = 3.6,
    color = ink
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Multnomah, Marion, and Jackson had the most pantry visits per resident",
    subtitle = "Household visits to partner pantries per 1,000 county residents, 2025",
    caption = str_c(
      "A household that visited in more than one month is counted once in each month.\n",
      "*Wheeler is a minimum: 4 months had fewer than 10 households and are suppressed in the source data.\n",
      "Source: Partner pantry visit data, exported January 15, 2026."
    ),
    x = NULL,
    y = NULL
  ) +
  theme_report() +
  theme(axis.text.x = element_blank())

per_1000_chart

dir.create("outputs", showWarnings = FALSE)
ggsave(
  "outputs/households-per-1000.png",
  per_1000_chart,
  width = 7,
  height = 4.5,
  dpi = 300,
  device = ragg::agg_png
)
ggsave(
  "outputs/households-per-1000.pdf",
  per_1000_chart,
  width = 7,
  height = 4.5,
  device = cairo_pdf
)

# Monthly trend -----------------------------------------------------------

monthly_totals <- visits_with_population |>
  summarize(households = sum(households, na.rm = TRUE), .by = month) |>
  arrange(month)

monthly_chart <- ggplot(monthly_totals, aes(x = month, y = households)) +
  geom_line(color = chart_blue, linewidth = 0.9, lineend = "round") +
  geom_point(
    data = slice_tail(monthly_totals, n = 1),
    color = chart_blue,
    size = 3
  ) +
  geom_text(
    data = slice_tail(monthly_totals, n = 1),
    aes(label = scales::comma(households)),
    hjust = 0,
    nudge_x = 8,
    family = "Inter",
    size = 3.6,
    color = ink
  ) +
  scale_x_date(
    breaks = monthly_totals$month,
    date_labels = "%b",
    expand = expansion(mult = c(0.02, 0.1))
  ) +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Pantry visits climbed through the fall, reaching a 2025 high in December",
    subtitle = "Household visits to partner pantries each month, all partner counties",
    caption = "Source: Partner pantry visit data, exported January 15, 2026.",
    x = NULL,
    y = NULL
  ) +
  theme_report() +
  theme(panel.grid.major.y = element_line(color = "#e8e7e3", linewidth = 0.3))

monthly_chart

ggsave(
  "outputs/monthly-visits.png",
  monthly_chart,
  width = 7,
  height = 4.5,
  dpi = 300,
  device = ragg::agg_png
)
ggsave(
  "outputs/monthly-visits.pdf",
  monthly_chart,
  width = 7,
  height = 4.5,
  device = cairo_pdf
)
