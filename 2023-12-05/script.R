# Load libraries ---------------------------------------------------------------

library(dplyr)
library(stringr)
library(showtext)
library(ggplot2)
# Also requires {here}, {ggtext}

# Read data into R -------------------------------------------------------------

life_exp_fm <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-05/life_expectancy_female_male.csv')

# Wrangle data -----------------------------------------------------------------

life_exp_tidy <- life_exp_fm %>%
  filter(is.na(Code)) %>%
  filter(str_detect(Entity, "UN"))

# Build plot -------------------------------------------------------------------

# Import font from Google
font_add_google("Cabin Condensed", "cabin")
showtext_auto()

# Set plot colours
bg_colour <- "#1F1F1E"
line_colour <- "#3E3E3C"
title_text_colour <- "#E9CE2C"
axis_text_colour <- "#5E5E5A"
entity_colours <- c("#ff5883", "#ff91ad", "#fec9d7", "#b9eee1", "#79d3be", "#39b89a")

# Build plot
plot <- ggplot(data = life_exp_tidy) +
  facet_wrap(~ Entity, nrow = 2) +
  geom_smooth(
    aes(x = Year, y = LifeExpectancyDiffFM),
    method = "loess", linewidth = 0) +
  geom_point(
    aes(x = Year, y = LifeExpectancyDiffFM, colour = Entity),
    size = 2, alpha = 0.8, shape = 16) +
  scale_colour_manual(values = entity_colours) +
  labs(
    title = "Gaps closing between female and male life expectancy in Europe, Oceania, and Northern America",
    x = NULL, y = NULL,
    caption = "**Data source:** Our World in Data | **Plot:** Jacquie Tran") +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(family = "cabin", colour = title_text_colour),
    strip.text = element_text(size = rel(3), colour = title_text_colour),
    axis.text = element_text(size = rel(4), colour = axis_text_colour),
    plot.title = element_text(
      size = rel(4.5), face = "bold", margin = margin(0,0,10,0, unit = "pt")),
    plot.caption = ggtext::element_markdown(
      size = rel(4), margin = margin(20,0,0,0, unit = "pt")),
    panel.grid.major = element_line(colour = line_colour, linetype = "dashed", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines"),
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin = margin(20, 20, 20, 20, unit = "pt"))

# Export plot to PNG -----------------------------------------------------------

ggsave(
  here::here("2023-12-05/life_oh_life.png"),
  last_plot(), width = 9, height = 6, units = "in", dpi = 300)

# showtext_auto(FALSE)

