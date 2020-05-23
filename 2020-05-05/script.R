# Load libraries ---------------------------------------------------------------

library(here)
library(readr)
library(tidytext)
library(dplyr)
# extrafont::font_import()
extrafont::loadfonts(device = "win")
library(ggplot2)
library(ggtext)
library(tidyr)
library(textdata)
library(patchwork)
library(paletteer)
library(magick)

# Import the data --------------------------------------------------------------

user_reviews <- read_tsv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')

# Set objects for text tidying -------------------------------------------------

negation_words <- c("not", "no", "never")

AFINN <- get_sentiments("afinn")

# Tidy the data: User reviews --------------------------------------------------

user_reviews %>%
  # Categorise grades into 1 of 3 categories
  mutate(
    grade_category = case_when(
      grade >= 7 ~ "High",
      grade <= 3 ~ "Low",
      TRUE       ~ "Middle")) %>%
  # Tokenise text to bigrams
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  # Separate tokens into distinct columns
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  # Include only bigrams that begin with a negation word
  filter(word1 %in% negation_words) %>%
  # Exclude records for bigrams where word2 is a stop word
  filter(!word2 %in% stop_words$word) %>%
  # Use AFINN lexicon to apply sentiment score to word2
  inner_join(AFINN, by = c(word2 = "word")
             ) -> user_reviews_bigrams_negation

# Low grades
user_reviews_bigrams_negation %>%
  filter(grade_category == "Low") %>%
  count(word1, word2, value, sort = TRUE) %>%
  ungroup() %>%
  mutate(contribution = n * value) %>%
  group_by(word1) %>%
  top_n(5, abs(contribution)) %>%
  ungroup() %>%
  arrange(word1, contribution) %>%
  mutate(order = row_number()
         )-> low_grades_negation

# Middle grades
user_reviews_bigrams_negation %>%
  filter(grade_category == "Middle") %>%
  count(word1, word2, value, sort = TRUE) %>%
  ungroup() %>%
  mutate(contribution = n * value) %>%
  group_by(word1) %>%
  top_n(5, abs(contribution)) %>%
  ungroup() %>%
  arrange(word1, contribution) %>%
  mutate(order = row_number()
  )-> middle_grades_negation

# High grades
user_reviews_bigrams_negation %>%
  filter(grade_category == "High") %>%
  count(word1, word2, value, sort = TRUE) %>%
  ungroup() %>%
  mutate(contribution = n * value) %>%
  group_by(word1) %>%
  top_n(5, abs(contribution)) %>%
  ungroup() %>%
  arrange(word1, contribution) %>%
  mutate(order = row_number()
  )-> high_grades_negation

# Set plot features as a list --------------------------------------------------

plot_features <- list(
  facet_wrap(~word1, nrow = 1, scales = "free"),
  geom_bar(stat = "identity"),
  scale_fill_manual(
    values = c(paletteer_d("LaCroixColoR::paired")[1],
               paletteer_d("LaCroixColoR::KeyLime")[4])),
  coord_flip(),
  theme_minimal(),
  theme(
    legend.position = "none",
    text = element_text(
      family = "Arvo",
      size = 9),
    plot.title = element_markdown(size = NULL),
    plot.subtitle = element_markdown(size = NULL),
    plot.caption = element_markdown(size = NULL),
    panel.spacing = unit(1, "lines"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(
      fill = paletteer_d("dutchmasters::little_street")[5],
      colour = NA),
    strip.background = element_rect(
      fill = paletteer_d("dutchmasters::pearl_earring")[5],
      colour = NA))
  )

# Plot: Low grades -------------------------------------------------------------

p <- ggplot(
  low_grades_negation,
  aes(x = order,
      y = contribution,
      fill = contribution > 0))
p <- p + plot_features
p <- p + scale_x_continuous(
  breaks = low_grades_negation$order,
  labels = low_grades_negation$word2, # Add categories to axis
  expand = c(0,0))
p <- p + labs(
  title = "Negated words from user reviews of **Animal Crossing: New Horizons**",
  subtitle = "From **low-scoring** reviews (<= 3 out of 10)",
  x = NULL, y = NULL)
plot_low_grades_negation <- p

# Plot: Middle grades ----------------------------------------------------------

p <- ggplot(
  middle_grades_negation,
  aes(x = order,
      y = contribution,
      fill = contribution > 0))
p <- p + plot_features
p <- p + scale_x_continuous(
  breaks = middle_grades_negation$order,
  labels = middle_grades_negation$word2, # Add categories to axis
  expand = c(0,0))
p <- p + labs(
  subtitle = "From **middling** user reviews (4-6 out of 10)",
  x = NULL, y = NULL)
plot_middle_grades_negation <- p

# Plot: High grades ------------------------------------------------------------

p <- ggplot(
  high_grades_negation,
  aes(x = order,
      y = contribution,
      fill = contribution > 0))
p <- p + plot_features
p <- p + scale_x_continuous(
  breaks = high_grades_negation$order,
  labels = high_grades_negation$word2, # Add categories to axis
  expand = c(0,0))
p <- p + labs(
  subtitle = "From **high-scoring** user reviews (>= 7 out of 10)",
  x = NULL, y = "\nSentiment value * # of occurrences\n",
  caption = "**Image credit:** Ijen-Ekusas on DeviantArt")
plot_high_grades_negation <- p

# Image source:
# https://www.deviantart.com/ijen-ekusas/art/Animal-Crossing-Celeste-656730811

# Patch plots together ---------------------------------------------------------

quilted_plots <- plot_low_grades_negation / plot_middle_grades_negation / plot_high_grades_negation

# Export quilted plots
ggsave(
  here("2020-05-05/quilted_plots.png"),
  quilted_plots,
  device = "png",
  type = "cairo",
  width = 18, height = 12, units = "cm", dpi = 300)

# Add image to plot ------------------------------------------------------------

# Load image file of quilted plots
image_read(here("2020-05-05/quilted_plots.png")
           ) -> plot_base

# Load image of Celeste
image_read(here("2020-05-05/celeste.png")) %>%
  image_scale("250"
              ) -> celeste_still

# Combine images
composite <- image_composite(
  plot_base, celeste_still, offset = "+35+1140")

# Export composite image to PNG
image_write(
  composite,
  here("2020-05-05/composite.png"),
  "png")