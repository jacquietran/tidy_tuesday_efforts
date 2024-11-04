# Load libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)

# Read data into R -------------------------------------------------------------

tidycheeseday <- tidytuesdayR::tt_load('2024-06-04')

cheese_data <- tidycheeseday$cheeses

# Tidy the data ----------------------------------------------------------------

# Tidy data from cheese.com
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
  slice_max(calcium_content_pct, n = 10) %>%
  arrange(calcium_content_tidy) %>%
  mutate(
    cheese = factor(cheese, levels = cheese))

# Customise labels to include cheese name and country flags
axis_labels <- cheese_tidy %>%
  select(cheese) %>%
  mutate(
    img = case_when(
      cheese == "Castelmagno"              ~ paste0(cheese, "<br><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/0/03/Flag_of_Italy.svg/800px-Flag_of_Italy.svg.png' width='30' />"),
      cheese %in% c(
        "Prima Donna leggero",
        "Prima Donna forte",
        "Prima Donna fino",
        "Prima Donna maturo")              ~ paste0(cheese, "<br><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/2/20/Flag_of_the_Netherlands.svg/800px-Flag_of_the_Netherlands.svg.png' width='30' />"),
      cheese == "Brebis du Lavort"         ~ paste0(cheese, "<br><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Flag_of_France_%281794%E2%80%931815%2C_1830%E2%80%931974%2C_2020%E2%80%93present%29.svg/800px-Flag_of_France_%281794%E2%80%931815%2C_1830%E2%80%931974%2C_2020%E2%80%93present%29.svg.png' width='30' />"),
      cheese == "Seriously Strong Cheddar" ~ paste0(cheese, "<br><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/b/be/Flag_of_England.svg/800px-Flag_of_England.svg.png' width='30' />#&nbsp;<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/1/10/Flag_of_Scotland.svg/1200px-Flag_of_Scotland.svg.png' width='30' />&nbsp;<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/8/83/Flag_of_the_United_Kingdom_%283-5%29.svg/800px-Flag_of_the_United_Kingdom_%283-5%29.svg.png' width='30' />"),
      cheese %in% c(
        "Bianco",
        "Basils Original Rauchkäse")       ~ paste0(cheese, "<br><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/b/ba/Flag_of_Germany.svg/800px-Flag_of_Germany.svg.png' width='30' />"),
      cheese == "Limburger"                ~ paste0(cheese, "<br><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/9/92/Flag_of_Belgium_%28civil%29.svg/800px-Flag_of_Belgium_%28civil%29.svg.png' width='30' />#&nbsp;<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/b/ba/Flag_of_Germany.svg/800px-Flag_of_Germany.svg.png' width='30' />#&nbsp;<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/2/20/Flag_of_the_Netherlands.svg/800px-Flag_of_the_Netherlands.svg.png' width='30' />")
    )) %>%
  pull(var = img, name = cheese)

# Prep to plot -----------------------------------------------------------------

# Load custom fonts
#font_add_google("Bellota", "bellota")
#font_add_google("Fira Sans Extra Condensed", "fira")

# showtext_auto()

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_bar(
    data = cheese_tidy,
    aes(x = calcium_content_tidy, y = cheese),
    stat = "identity") +
  scale_y_discrete(
    name = NULL,
    labels = axis_labels) +
  #labs(
  #  title = "Cheesin' for calcium",
  #  subtitle = "Top 10 cheeses with known calcium content, per cheese.com",
  #  x = NULL, y = NULL,
  #  caption = "**Plot:** Jacquie Tran") +
  theme(
    axis.text.y = element_markdown()
    )


# Save plot to file ------------------------------------------------------------

#ggsave(
#  here::here("2021-12-07/plot_cryptic_spiders.png"),
#  width = 12, height = 7, units = "cm", dpi = 320)

#showtext_auto(FALSE)
