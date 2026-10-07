library(tidyverse)
library(palmerpenguins)

penguins |>
    filter(species == "Adelie") |>
    summarize(avg_bill_length = mean(bill_length_mm, na.rm = TRUE))

penguins |>
    ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
    geom_point(size = 8)
