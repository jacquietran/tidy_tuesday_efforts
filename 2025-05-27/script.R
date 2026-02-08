# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(roughVizR)

# Read data into R--------------------------------------------------------------

monsters <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-27/monsters.csv')

# Tidy the data ----------------------------------------------------------------

monsters_hp <- monsters %>%
  group_by(type) %>%
  summarise(
    hp_number = mean(hp_number),
    .groups = "drop") %>%
  arrange(desc(hp_number))

# Build plot -------------------------------------------------------------------

roughBarH(
  data = monsters_hp,
  labels = "type",
  values = "hp_number",
  title = "Average HP of D&D monsters by type",
  color = "pink",
  fillStyle = "zigzag",
  roughness = 1
)

# Export plot ------------------------------------------------------------------
