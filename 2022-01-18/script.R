# Load libraries ---------------------------------------------------------------

library(dplyr)
library(stringr)
library(showtext)
library(ggplot2)

# Import data ------------------------------------------------------------------

chocolate <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

# Tidy data --------------------------------------------------------------------

# Prepare data for scatterplot
chocolate %>%
  mutate(
    counting_var = 1,
    record_id = cumsum(counting_var),
    cocoa_percent_num = as.numeric(str_remove_all(cocoa_percent, "%"))
    ) -> chocolate_tidy

chocolate_tidy %>%
  select(
    record_id, cocoa_percent_num, rating,
    most_memorable_characteristics) %>%
  filter(record_id %in% c(137, 62, 1044)) -> chocolate_labels

# Build plot -------------------------------------------------------------------

# Import font from Google
font_add_google("Barlow Semi Condensed", "barlow")
showtext_auto()

p <- ggplot() +
  # Plot the data points
  geom_jitter(
    data = chocolate_tidy,
    aes(x = cocoa_percent_num, y = rating), size = 3, colour = "#92374D",
    alpha = 0.2, 
    shape = 16, stroke = 0, position = position_jitter(seed = 4)) +
  geom_jitter(
    data = chocolate_labels,
    aes(x = cocoa_percent_num, y = rating), size = 12, colour = "#6CB29D", 
    shape = 16, stroke = 0, position = position_jitter(seed = 4)) +
  # Plot the labels
  geom_label(
    data = chocolate_labels,
    aes(x = cocoa_percent_num, y = rating,
        label = most_memorable_characteristics),
    size = 12, nudge_x = 4, nudge_y = 0.4, colour = "#386B5C") +
  scale_x_continuous(
    breaks = seq(50, 100, by = 10),
    labels = c("50%", "60%", "70%", "80%", "90%", "100%")) +
  scale_y_continuous(limits = c(0,5)) +
  scale_colour_identity() +
  scale_alpha_identity() +
  scale_size_identity() +
  labs(
    title = "A chocolate experience you'll never forget...",
    subtitle = "Cocoa percentage vs. Rating (1-5)",
    caption = "**Data source:** FlavorsOfCacao.com | **Plot:** @jacquietran",
    x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(family = "barlow", colour = "#92374D"),
    axis.text = element_text(size = rel(4)),
    plot.title = element_text(
      size = rel(6), face = "bold", margin = margin(0,0,5,0, unit = "pt")),
    plot.subtitle = element_text(
      size = rel(4), margin = margin(0,0,20,0, unit = "pt")),
    plot.caption = ggtext::element_markdown(
      size = rel(4), margin = margin(20,0,0,0, unit = "pt")),
    panel.grid.major = element_line(colour = "#BAC69C", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#E6E1C5", colour = "#E6E1C5"),
    plot.margin = margin(20, 20, 20, 20, unit = "pt"))

# Export to file
ggsave(
  here::here("2022-01-18/delicious_chocolate.png"),
  last_plot(), width = 8, height = 6, units = "in", dpi = 300)

showtext_auto(FALSE)

