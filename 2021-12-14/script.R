# Load libraries ---------------------------------------------------------------

library(dplyr)
library(stringr)
library(showtext)
library(ggplot2)
library(ggtext)
library(patchwork)

# Read data into R -------------------------------------------------------------

studio_album_tracks <- readr::read_csv(
  "https://github.com/jacquietran/spice_girls_data/raw/main/data/studio_album_tracks.csv")

# Tidy data --------------------------------------------------------------------

# Danceability
studio_album_tracks %>%
  select(album_name, track_name, danceability, tempo) %>%
  mutate(
    y_var = 1,
    y_label = "danceability"
    ) -> danceability_df

danceability_annotation <- str_wrap(
  "The most danceable tracks are 'Get Down With Me' and 'Tell Me Why', from the album 'Forever'.",
  width = 50)

# Energy
studio_album_tracks %>%
  select(album_name, track_name, energy, tempo) %>%
  mutate(
    y_var = 1,
    y_label = "energy"
    ) -> energy_df

energy_annotation <- str_wrap(
  "The most energetic song is 'Spice Up Your Life', from the album 'Spiceworld'.",
  width = 50)

# Acousticness
studio_album_tracks %>%
  select(album_name, track_name, acousticness, tempo) %>%
  mutate(
    y_var = 1,
    y_label = "acousticness"
    ) -> acousticness_df

acousticness_annotation <- str_wrap(
  "Most Spice Girls songs score low for acousticness.",
  width = 50)

# Valence
studio_album_tracks %>%
  select(album_name, track_name, valence, tempo) %>%
  mutate(
    y_var = 1,
    y_label = "valence"
    ) -> valence_df

valence_annotation <- "'Wannabe' is the 5th most positive Spice Girls song."

# Plot features ----------------------------------------------------------------

# Set custom colours
album_name_cols <- c(
  "Spice" = "#A1221B",
  "Spiceworld" = "#DC6918",
  "Forever" = "#765057")

# Load custom fonts
font_add_google("Space Grotesk", "spacegrotesk")
font_add_google("Chivo", "chivo")

# Set consistent plot features
plot_features <- list(
  geom_hline(yintercept = 1, size = 0.1, alpha = 0.3),
  geom_point(aes(size = tempo), stroke = 0, alpha = 0.6),
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)),
  scale_size_continuous(range = c(2, 12)),
  scale_colour_manual(values = album_name_cols),
  labs(y = NULL),
  theme_minimal(),
  theme(
    text = element_text(size = 48, colour = "#292929", family = "chivo"),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(
      size = 42, family = "spacegrotesk", face = "bold")
    ))

# Build plots ------------------------------------------------------------------

showtext_auto()

# Danceability
p1 <- ggplot(
  danceability_df, aes(x = danceability, y = y_var, colour = album_name))
p1 <- p1 + plot_features
p1 <- p1 + labs(
  title = "<strong>If U Can't Dance:</strong> Songs from the <span style = 'color:#A1221B;'><strong>Spice</strong></span>, <span style = 'color:#DC6918;'><strong>Spiceworld</strong></span>, and <span style = 'color:#765057;'><strong>Forever</strong></span> albums.",
  subtitle = "Point size indicates tempo; larger dots are faster songs.",
  x = NULL)
p1 <- p1 + scale_y_continuous(
  limits = c(0.95, 1.08),
  breaks = seq(0.95, 1.05, by = 0.05),
  labels = c(
    "0.95" = "", "1" = "danceability", "1.05" = ""))
p1 <- p1 + annotate(
  "text", x = 0.75, y = 1.065, size = 11, label = danceability_annotation,
  family = "chivo", lineheight = 0.25)
p1 <- p1 + geom_curve(
  aes(x = 0.84, y = 1.04, xend = 0.86, yend = 1.01),
  curvature = -0.35, size = 0.35, colour = "#000000")
p1 <- p1 + geom_curve(
  aes(x = 0.84, y = 1.04, xend = 0.83, yend = 1.01),
  curvature = -0.3, size = 0.35, colour = "#000000")
p1 <- p1 + theme(
  plot.title = element_markdown(size = 46, family = "spacegrotesk"),
  plot.subtitle = element_text(
    size = 40, family = "spacegrotesk", margin = margin(0, 0, 20, 0)),
  axis.text.x = element_blank())

# Energy
p2 <- ggplot(
  energy_df, aes(x = energy, y = y_var, colour = album_name))
p2 <- p2 + plot_features
p2 <- p2 + labs(x = NULL)
p2 <- p2 + scale_y_continuous(
  limits = c(0.95, 1.08),
  breaks = seq(0.95, 1.05, by = 0.05),
  labels = c(
    "0.95" = "", "1" = "energy", "1.05" = ""))
p2 <- p2 + annotate(
  "text", x = 0.85, y = 1.06, size = 11, label = energy_annotation,
  family = "chivo", lineheight = 0.25)
p2 <- p2 + geom_curve(
  aes(x = 0.99, y = 1.055, xend = 0.996, yend = 1.01),
  curvature = -0.4, size = 0.35, colour = "#000000")
p2 <- p2 + theme(
  axis.text.x = element_blank())

# Acousticness
p3 <- ggplot(
  acousticness_df, aes(x = acousticness, y = y_var, colour = album_name))
p3 <- p3 + plot_features
p3 <- p3 + labs(x = NULL)
p3 <- p3 + scale_y_continuous(
  limits = c(0.95, 1.08),
  breaks = seq(0.95, 1.05, by = 0.05),
  labels = c(
    "0.95" = "", "1" = "acousticness", "1.05" = ""))
p3 <- p3 + annotate(
  "text", x = 0.20, y = 1.07, size = 11, label = acousticness_annotation,
  family = "chivo", lineheight = 0.25)
p3 <- p3 + ggbrace::geom_brace(
  aes(c(0, 0.35), c(1.025, 1.05)), inherit.data = FALSE)
p3 <- p3 + theme(
  axis.text.x = element_blank())

# Valence
p4 <- ggplot(
  valence_df, aes(x = valence, y = y_var, colour = album_name))
p4 <- p4 + plot_features
p4 <- p4 + labs(
  x = "Intensity of the audio feature (0 = low, 1 = high)",
  caption = "**Data source:** Spotify | **Plot:** @jacquietran")
p4 <- p4 + scale_y_continuous(
  limits = c(0.95, 1.08),
  breaks = seq(0.95, 1.05, by = 0.05),
  labels = c(
    "0.95" = "", "1" = "valence", "1.05" = ""))
p4 <- p4 + annotate(
  "text", x = 0.60, y = 1.06, size = 11, label = valence_annotation,
  family = "chivo", lineheight = 0.25)
p4 <- p4 + geom_curve(
  aes(x = 0.835, y = 1.06, xend = 0.883, yend = 1.01),
  curvature = -0.4, size = 0.35, colour = "#000000")
p4 <- p4 + theme(
  axis.text.x = element_text(vjust = 9),
  axis.title.x = element_text(family = "spacegrotesk", vjust = 4),
  plot.caption = element_markdown(
    size = NULL, family = "spacegrotesk", margin = margin(15, 0, 0, 0)))

# Quilt the plots --------------------------------------------------------------

quilted_plots <- (p1 + p2 + p3 + p4) +
  plot_layout(ncol = 1) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "#F1E3E4"),
      plot.margin = margin(5, 5, 5, 5)))

ggsave(
  here::here("2021-12-14/spice_girls_features.png"),
  quilted_plots,
  type = "cairo", width = 9, height = 6, units = "in", dpi = 320)

showtext_auto(FALSE)