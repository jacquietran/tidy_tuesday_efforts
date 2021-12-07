# Load libraries ---------------------------------------------------------------

library(dplyr)
library(showtext)
library(ggplot2)
library(forcats)
library(ggtext)
library(paletteer)
library(magick)

# Retrieve data ----------------------------------------------------------------

spiders <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')

# Tidy data --------------------------------------------------------------------

salticidae_top_10_genera <- spiders %>%
  filter(stringr::str_detect(distribution, "Australia")) %>%
  filter(stringr::str_detect(distribution, "Queensland")) %>%
  filter(family == "Salticidae") %>%
  group_by(genus) %>%
  summarise(n_species = n()) %>%
  ungroup() %>%
  slice_max(order_by = n_species, n = 10)

# Create annotation
maratus_annotation <- stringr::str_wrap(
  "Spiders of the Maratus genus are more commonly referred to as peacock spiders. This name arises from the usually iridescent colouring of males' abdomens. Females are not as colourful as they are 'cryptic' in appearance.",
  width = 40)

# Build plot -------------------------------------------------------------------

# Load custom fonts
font_add_google("Bellota", "bellota")
font_add_google("Fira Sans Extra Condensed", "fira")

# Top 10 genera in the Salticidae family of spiders that are distributed in QLD
showtext_auto()

p <- ggplot(
  salticidae_top_10_genera,
  aes(x = n_species, y = fct_reorder(genus, n_species), fill = n_species))
p <- p + geom_bar(stat = "identity", width = 0.7)
p <- p + geom_curve(
  aes(x = 19.5, y = "Maratus", xend = 18, yend = "Myrmarachne"),
  curvature = -0.35, linetype = "dotted", size = 0.35)
p <- p + annotate(
  "text", x = 17.5, y = "Zenodorus", size = 8, label = maratus_annotation,
  family = "fira", lineheight = 0.25, vjust = 0.6,
  colour = paletteer_c("pals::ocean.thermal", n = 20)[16])
p <- p + scale_fill_gradient(
  low = paletteer_c("pals::ocean.thermal", n = 20)[4],
  high = paletteer_c("pals::ocean.thermal", n = 20)[16])
p <- p + labs(
  title = "Peacock spiders abound!",
  subtitle = "Top 10 genera in the Salticidae family by number of species (distributed in Queensland)",
  x = NULL, y = NULL,
  caption = "**Data:** World Spider Database | **Plot:** @jacquietran | **Kewl spider factz:** Wikipedia")
p <- p + ggdark::dark_mode()
p <- p + theme(
  legend.position = "none",
  text = element_text(
    size = 32, family = "fira", colour = "#00c1ec"),
  axis.text = element_text(colour = "#FFFFFF", family = "fira"),
  plot.title = element_text(
    colour = "#00c1ec", family = "bellota", size = 52, face = "bold"),
  plot.subtitle = element_text(
    colour = "#00c1ec", family = "fira", size = 26,
    margin = margin(0, 0, 10, 0)),
  plot.caption = element_markdown(
    colour = "#00c1ec", family = "bellota", size = 18,
    margin = margin(10, 0, 0, 0)),
  panel.grid.minor.x = element_blank(),
  plot.margin = margin(10, 10, 10, 10))

ggsave(
  here::here("2021-12-07/plot_cryptic_spiders.png"),
  width = 12, height = 7, units = "cm", dpi = 320)

showtext_auto(FALSE)

# Add images to plot -----------------------------------------------------------

# Read supplementary images in
maratus <- image_read(here::here("2021-12-07/plot_cryptic_spiders.png"))
cobweb <- image_read(here::here("2021-12-07/cobweb_sm.png"))
volans <- image_read(here::here("2021-12-07/volans_framed_rotated.png"))

# Add cobweb
plot_with_web <- image_mosaic(c(maratus, cobweb))
# Add spider polaroid
plot_with_web_and_spider <- image_composite(
  plot_with_web, volans, offset = "+1305+270")

# Export composite image
image_write(
  plot_with_web_and_spider,
  path = here::here("2021-12-07/plot_composite.png"),
  quality = 100, format = "png")

