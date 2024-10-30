# Load libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)

# Read data into R -------------------------------------------------------------

tidycheeseday <- tidytuesdayR::tt_load('2024-06-04')

cheese_data <- tidycheeseday$cheeses

# Tidy the data ----------------------------------------------------------------

cheese_tidy <- cheese_data %>%
  filter(!is.na(calcium_content)) %>%
  separate(
    col = "calcium_content", into = c("calcium_content_tidy", "col_to_drop"),
    sep = " ", remove = FALSE) %>%
  mutate(
    calcium_content_tidy = as.numeric(calcium_content_tidy),
    calcium_content_pct = calcium_content_tidy / 1000) %>%
  select(
    cheese, url, milk, country, region, type, calcium_content,
    calcium_content_tidy, calcium_content_pct, texture, rind, color,
    flavor, aroma) %>%
  slice_max(calcium_content_pct, n = 10)

# Build plot -------------------------------------------------------------------

# Save plot to file ------------------------------------------------------------
