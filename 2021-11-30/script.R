# Load libraries ---------------------------------------------------------------

library(dplyr)
library(forcats)
library(showtext)
library(ggplot2)
library(ggbeeswarm)
library(ggtext)

# Read data into R -------------------------------------------------------------

matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')

# Tidy data --------------------------------------------------------------------

# Ways to win:
# by # of wickets (win by chasing team) or # of runs (setting team)
matches %>%
  rename(
    wickets_team1 = wickets_team2,
    wickets_team2 = wickets_team) %>%
  select(winner, margin, margin_type) %>%
  mutate(
    margin_type = case_when(
      margin_type %in% c(
        "wicket", "wickets") ~ "wickets",
      margin_type %in% c(
        "run", "runs")       ~ "runs",
      TRUE                   ~ margin_type)) %>%
  filter(
    margin_type != "default"
    ) -> ways_to_win_wrangled

ways_to_win_wrangled %>%
  group_by(winner, margin_type) %>%
  summarise(
    n_wins_of_margin_type = n(),
    margin_mean = round(mean(margin), 1),
    margin_min = round(min(margin), 1),
    margin_max = round(max(margin), 1),
    .groups = "keep") %>%
  ungroup() %>%
  arrange(winner, margin_type) %>%
  group_by(winner) %>%
  mutate(
    n_wins_total = sum(n_wins_of_margin_type),
    pct_wins_of_margin_type = round(
      n_wins_of_margin_type / n_wins_total * 100, 0)) %>%
  ungroup() %>%
  filter(n_wins_total >= 50) -> ways_to_win_summarised

# Order team names on margin_mean where margin_type == "runs"
ways_to_win_summarised %>%
  filter(margin_type == "runs") %>%
  arrange(margin_mean) %>%
  select(winner) %>%
  pull() -> team_order

ways_to_win_summarised %>%
  mutate(
    winner = factor(winner, levels = team_order),
    margin_mean_label = format(margin_mean, n.small = 1),
    label_xpos = case_when(
      margin_type == "runs"    ~ 240,
      margin_type == "wickets" ~ 9.4)
    ) -> ways_to_win_summarised_tidy

ways_to_win_wrangled %>%
  filter(winner %in% team_order) %>%
  mutate(
    winner = factor(winner, levels = team_order)
    ) -> ways_to_win_tidy
  
# Build plot 1: Beeswarm! ------------------------------------------------------

# Import Google Fonts
font_add_google(name = "Exo 2", family = "exo")
font_add_google(name = "Fira Sans Extra Condensed", family = "firacondensed")

# Set different x axis breaks based on the data range
# Thanks to coolbutuseless! https://coolbutuseless.github.io/2019/03/07/custom-axis-breaks-on-facetted-ggplot/
breaks_fun <- function(x) {
  if (max(x) > 11) {
    seq(0, 275, 50)
  } else {
    seq(0, 10, 2)
  }
}

# Set custom strip text labels
strip_labels <- c(
  "runs" = "win margin by runs",
  "wickets" = "win margin by wickets")

# Margins when winning by runs (target successfully defended) and
# when winning by wickets (target successfully chased)
showtext_auto()
p <- ggplot(
  ways_to_win_tidy, aes(
    x = margin, y = winner,
    group = margin_type, colour = winner))
p <- p + facet_wrap(
  ~margin_type, nrow = 1, scales = "free_x",
  labeller = labeller(margin_type = strip_labels))
p <- p + geom_quasirandom(
  shape = 19, size = 1.7, alpha = 0.4, groupOnX = FALSE)
p <- p + geom_point(
  data = ways_to_win_summarised_tidy,
  aes(x = margin_mean, y = winner),
  shape = 23, size = 4, fill = "#000000", colour = "#000000", alpha = 0.15)
p <- p + geom_point(
  data = ways_to_win_summarised_tidy,
  aes(x = margin_mean, y = winner),
  shape = 18, size = 3.7, fill = "#FFFFFF", colour = "#FFFFFF")
p <- p + geom_text(
  data = ways_to_win_summarised_tidy,
  aes(x = label_xpos, y = winner, label = margin_mean_label),
  size = 12, fontface = "bold", vjust = 0.6, colour = "#000000",
  alpha = 0.8, family = "exo")
p <- p + geom_text(
  data = ways_to_win_summarised_tidy,
  aes(x = label_xpos, y = winner, label = margin_mean_label),
  size = 12, fontface = "bold", vjust = 0.5, colour = "#FFFFFF",
  family = "exo")
p <- p + scale_x_continuous(breaks = breaks_fun, limits = c(0, NA))
p <- p + scale_colour_manual(
  values = c(
    "South Africa" = "#f605ff",
    "Pakistan" = "#d94aff",
    "Australia" = "#b966ff",
    "Zimbabwe" = "#9879ff",
    "India" = "#7687ff",
    "New Zealand" = "#5392ff",
    "Sri Lanka" = "#2f9bff",
    "England" = "#05a2ff",
    "West Indies" = "#04a7fe"))
p <- p + labs(
  title = "Convincing winners: SA, PAK, and AUS wins were by ~70 runs when setting and ~6 wickets when chasing.",
  subtitle = "Men's teams with 50+ ODI wins (1996-2005, inclusive). Diamonds / figures are mean values.",
  x = NULL, y = NULL,
  caption = "**Data source:** Hassanasir & ESPN Cricinfo | **Plot:** @jacquietran")
p <- p + ggdark::dark_mode()
p <- p + theme(
  text = element_text(size = 36, family = "firacondensed", colour = "#FFFFFF"),
  plot.title = element_text(
    size = 24, family = "exo", face = "bold"),
  plot.subtitle = element_text(
    size = 24, family = "exo", margin=margin(0,0,10,0)),
  plot.caption = element_markdown(
    size = NULL, margin=margin(10,0,0,0)),
  strip.text = element_text(
    family = "exo", face = "bold"),
  axis.text = element_text(colour = "#FFFFFF"),
  panel.spacing = unit(1, "lines"),
  plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
  legend.position = "none")

ggsave(
  here::here("2021-11-30/average_win_margins.png"), last_plot(),
  width = 6, height = 4, units = "in", dpi = 320)

showtext_auto(FALSE)

# More data tidying ------------------------------------------------------------

# Order team names on margin_mean where margin_type == "runs"
ways_to_win_summarised_tidy %>%
  mutate(
    winner = factor(
      winner, levels = c(
        "England",
        "West Indies",
        "New Zealand",
        "South Africa",
        "India",
        "Zimbabwe",
        "Sri Lanka",
        "Australia",
        "Pakistan")),
    pct_label = case_when(
      winner == "Pakistan" &
        margin_type == "wickets" ~ glue::glue(
          "{pct_wins_of_margin_type}% of wins from batting 2nd"),
      winner == "Pakistan" &
        margin_type == "runs" ~ glue::glue(
          "{pct_wins_of_margin_type}% of wins from batting 1st"),
      TRUE ~ glue::glue("{pct_wins_of_margin_type}%")),
    margin_type = factor(
      margin_type, levels = c("wickets", "runs"))) %>%
  group_by(winner) %>%
  mutate(
    label_xpos_runs = case_when(
      margin_type == "runs" ~ 0.5*pct_wins_of_margin_type,
      TRUE                  ~ NA_real_)) %>%
  tidyr::fill(label_xpos_runs) %>%
  mutate(
    label_xpos_wickets = case_when(
      margin_type == "wickets" ~ (
        (2*label_xpos_runs) + 0.5*pct_wins_of_margin_type),
      TRUE                     ~ NA_real_)) %>%
  tidyr::fill(label_xpos_wickets, .direction = "up") %>%
  ungroup() %>%
  mutate(
    label_xpos = case_when(
      margin_type == "runs"    ~ label_xpos_runs,
      margin_type == "wickets" ~ label_xpos_wickets)) %>%
  select(
    -label_xpos_runs, -label_xpos_wickets
  ) -> pct_of_win_types

# Build plot 2 -----------------------------------------------------------------

# Proportion of wins that were won by runs vs. by wickets
showtext_auto()
p <- ggplot(
  pct_of_win_types,
  aes(x = pct_wins_of_margin_type, y = winner))
p <- p + geom_bar(
  stat = "identity", aes(fill = margin_type), width = 0.7)
p <- p + geom_text(
  aes(x = label_xpos, y = winner, label = pct_label),
  size = 8, hjust = 0.5, colour = "#FFFFFF", fontface = "bold")
p <- p + scale_fill_manual(
  values = c("runs" = "#669000", "wickets" = "#a00c56"))
p <- p + labs(
  title = "Most PAK wins came from batting 1st, most ENG wins from batting 2nd",
  subtitle = "Men's teams with 50+ ODI wins (1996-2005, inclusive).",
  x = NULL, y = NULL,
  caption = "**Data source:** Hassanasir & ESPN Cricinfo | **Plot:** @jacquietran")
p <- p + ggdark::dark_mode()
p <- p + theme(
  text = element_text(size = 36, family = "firacondensed", colour = "#FFFFFF"),
  plot.title = element_text(
    size = 36, family = "exo", face = "bold"),
  plot.subtitle = element_text(
    size = 24, family = "exo", margin=margin(0,0,10,0)),
  plot.caption = element_markdown(
    size = NULL, margin=margin(10,0,0,0)),
  strip.text = element_text(
    family = "exo", face = "bold"),
  axis.text = element_text(colour = "#FFFFFF"),
  panel.spacing = unit(1, "lines"),
  plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
  legend.position = "none")

ggsave(
  here::here("2021-11-30/pct_of_win_types.png"), last_plot(),
  width = 6, height = 4, units = "in", dpi = 320)

showtext_auto(FALSE)