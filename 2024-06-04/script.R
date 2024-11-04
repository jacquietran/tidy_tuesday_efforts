# Load libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(showtext)
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
      cheese == "Castelmagno"              ~ paste0(cheese, "<br><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/0/03/Flag_of_Italy.svg/800px-Flag_of_Italy.svg.png' width='20' />"),
      cheese %in% c(
        "Prima Donna leggero",
        "Prima Donna forte",
        "Prima Donna fino",
        "Prima Donna maturo")              ~ paste0(cheese, "<br><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/2/20/Flag_of_the_Netherlands.svg/800px-Flag_of_the_Netherlands.svg.png' width='20' />"),
      cheese == "Brebis du Lavort"         ~ paste0(cheese, "<br><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Flag_of_France_%281794%E2%80%931815%2C_1830%E2%80%931974%2C_2020%E2%80%93present%29.svg/800px-Flag_of_France_%281794%E2%80%931815%2C_1830%E2%80%931974%2C_2020%E2%80%93present%29.svg.png' width='20' />"),
      cheese == "Seriously Strong Cheddar" ~ paste0(cheese, "<br><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/b/be/Flag_of_England.svg/800px-Flag_of_England.svg.png' width='20' /><span style='color: #23305C;'>.</span><img src='https://raw.githubusercontent.com/jacquietran/tidy_tuesday_efforts/refs/heads/master/2024-06-04/Flag_of_Scotland.png' width='20' /><span style='color: #23305C;'>.</span><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/8/83/Flag_of_the_United_Kingdom_%283-5%29.svg/800px-Flag_of_the_United_Kingdom_%283-5%29.svg.png' width='20' />"),
      cheese %in% c(
        "Bianco",
        "Basils Original Rauchkäse")       ~ paste0(cheese, "<br><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/b/ba/Flag_of_Germany.svg/800px-Flag_of_Germany.svg.png' width='20' />"),
      cheese == "Limburger"                ~ paste0(cheese, "<br><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/9/92/Flag_of_Belgium_%28civil%29.svg/800px-Flag_of_Belgium_%28civil%29.svg.png' width='20' /><span style='color: #23305C;'>.</span><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/b/ba/Flag_of_Germany.svg/800px-Flag_of_Germany.svg.png' width='20' /><span style='color: #23305C;'>.</span><img src='https://upload.wikimedia.org/wikipedia/commons/thumb/2/20/Flag_of_the_Netherlands.svg/800px-Flag_of_the_Netherlands.svg.png' width='20' />")
    )) %>%
  pull(var = img, name = cheese)

# Prep to plot -----------------------------------------------------------------

# Load custom fonts
font_add_google("Londrina Solid", "londrina")
font_add_google("M PLUS 2", "mplus2")

showtext_auto()

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_bar(
    data = cheese_tidy,
    aes(x = calcium_content_tidy, y = cheese),
    stat = "identity", width = 0.7, fill = "#FFF0C8") +
  geom_text(
    data = cheese_tidy,
    aes(x = calcium_content_tidy, y = cheese, label = calcium_content),
    nudge_x = 650, colour = "#FFF0C8", size = rel(24), family = "mplus2") +
  scale_x_continuous(
    limits = c(0,5800)) +
  scale_y_discrete(
    name = NULL,
    labels = axis_labels) +
  labs(
    title = "**Cheese to your health!**",
    subtitle = "Top 10 cheeses by calcium content, per cheese.com",
    x = NULL, y = NULL,
    caption = "Plot by Jacquie Tran") +
  theme_minimal() +
  theme(
    plot.background = element_rect(colour = "#1C274A", fill = "#1C274A"),
    plot.margin = margin(20, 20, 20, 20, unit = "pt"),
    panel.grid = element_blank(),
    text = element_text(family = "mplus2"),
    plot.title.position = "plot",
    plot.title = element_markdown(
      colour = "#FF7648", family = "londrina", size = rel(18), hjust = 0.5),
    plot.subtitle = element_text(
      colour = "#FF7648", size = rel(9), hjust = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_markdown(
      colour = "#FFF0C8", size = rel(10), face = "bold", family = "londrina",
      lineheight = 0.2, margin = margin(r = -15)),
    plot.caption = element_text(
      colour = "#FFF0C8", size = rel(5), family = "mplus2")
    )

# Save plot to file ------------------------------------------------------------

ggsave(
  here::here("2024-06-04/plot_cheezu.png"),
  width = 8, height = 8, units = "in", dpi = 600)

#showtext_auto(FALSE)
