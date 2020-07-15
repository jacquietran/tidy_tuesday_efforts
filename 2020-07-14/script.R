# Load libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(paletteer)
library(showtext)
# Load font
font_add_google("Dosis", "dosis")
font_add_google("Electrolize", "electrolize")
font_add_google("Quicksand", "quicksand")
showtext_auto()
library(ggplot2)
library(glue)

# Import data ------------------------------------------------------------------

source_data <- tidytuesdayR::tt_load('2020-07-14')

# Clean data -------------------------------------------------------------------

source_data_frame <- source_data$astronauts

# Men and women astronauts per decade ------------------------------------------

source_data_frame %>%
  select(
    id, number, name, sex, occupation, year_of_mission, hours_mission,
    eva_hrs_mission) %>%
  mutate(
    decade = case_when(
      year_of_mission >= 1960 & year_of_mission <= 1969 ~ "1960-1969",
      year_of_mission >= 1970 & year_of_mission <= 1979 ~ "1970-1979",
      year_of_mission >= 1980 & year_of_mission <= 1989 ~ "1980-1989",
      year_of_mission >= 1990 & year_of_mission <= 1999 ~ "1990-1999",
      year_of_mission >= 2000 & year_of_mission <= 2009 ~ "2000-2009",
      year_of_mission >= 2010 & year_of_mission <= 2019 ~ "2010-2019"),
    decade = factor(decade,
                    levels = c("2010-2019",
                               "2000-2009",
                               "1990-1999",
                               "1980-1989",
                               "1970-1979",
                               "1960-1969")),
    sex = factor(sex,
                 levels = c("female", "male"))
    ) -> clean_data

# Number of unique astronauts by decade
clean_data %>%
  group_by(decade) %>%
  summarise(
    n_total = unique(length(number)),
    missions_total = length(id),
    hours_mission_total = sum(hours_mission)) %>%
  ungroup(
  ) -> astronauts_by_decade

# Number of unique astronauts by decade and sex
clean_data %>%
  group_by(decade, sex) %>%
  summarise(
    n = unique(length(number)),
    missions_subtotal = length(id),
    hours_mission_subtotal = sum(hours_mission)) %>%
  complete(sex) %>%
  replace_na(list(n = 0,
                  missions_subtotal = 0,
                  hours_mission_subtotal = 0)) %>%
  ungroup(
    ) -> astronauts_by_decade_and_sex

# Merge
astronauts_by_decade_and_sex_merged <- left_join(
  astronauts_by_decade_and_sex, astronauts_by_decade)

# Tidy up merged data frame
astronauts_by_decade_and_sex_merged %>%
  mutate(
    pct_of_n_total = round(
      n / n_total * 100, 0),
    pct_of_missions_total = round(
      missions_subtotal / missions_total * 100, 0),
    pct_of_hours_mission_total = round(
      hours_mission_subtotal / hours_mission_total * 100, 0)) %>%
  select(
    decade, sex, n, pct_of_n_total, n_total, missions_total, missions_subtotal,
    pct_of_missions_total, hours_mission_subtotal, pct_of_hours_mission_total,
    hours_mission_total
    ) -> astronauts_by_decade_and_sex_tidy

# Pre-plot prep ----------------------------------------------------------------

# Define custom colour palette
sex_colours <- c(
  "male" = paletteer_d("wesanderson::Rushmore")[2],
  "female" = paletteer_d("wesanderson::Rushmore")[3])

# Set common plot features
plot_features <- list(
  geom_bar(
    stat = "identity",
    aes(fill = sex),
    width = 0.75),
  scale_fill_manual(
    values = sex_colours),
  theme_minimal(),
  theme(
    plot.background = element_rect(fill = "#000F0F"),
    text = element_text(
      size = 32, colour = paletteer_d("wesanderson::Royal1")[3],
      family = "dosis"),
    legend.position = "none",
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      colour = paletteer_d("wesanderson::Royal1")[3],
      family = "dosis"))
)

# Plot: Astronauts by decade and sex -------------------------------------------

# Define bar labels
astronauts_by_decade_and_sex_tidy %>%
  select(decade, sex, n, n_total) %>%
  mutate(
    sex_rephrase = case_when(
      sex == "female" ~ "women",
      sex == "male"   ~ "men"),
    sex_rephrase = case_when(
      sex == "female" & n == 1 ~ "woman",
      TRUE                     ~ sex_rephrase),
    label = glue("{n} {sex_rephrase}"),
    x_pos = case_when(
      sex == "male"   ~ n - 28,
      sex == "female" ~ n_total + 28)
    ) -> n_label

# Build plot
p <- ggplot(
  astronauts_by_decade_and_sex_tidy,
  aes(x = n, y = decade, fill = sex))
p <- p + plot_features
p <- p + scale_x_continuous(
  limits = c(0, 500),
  breaks = seq(0, 500,by = 100))
p <- p + geom_text(
  data = n_label,
  aes(label = label,
      x = x_pos, y = decade,
      colour = sex),
  fontface = "bold", size = 7,
  family = "dosis")
p <- p + scale_colour_manual(
  values = c("male" = "#000F0F",
             "female" = paletteer_d("wesanderson::Royal2")[5]))

# Export to PNG
ggsave(
  here("2020-07-14/plot_astro_by_decades_and_sex.png"),
  last_plot(),
  device = "png",
  type = "cairo",
  width = 18, height = 12, units = "cm", dpi = 300)

# Plot: Missions by decade and sex ---------------------------------------------

p <- ggplot(
  astronauts_by_decade_and_sex_tidy,
  aes(x = missions_subtotal, y = decade))
p <- p + plot_features

# Plot: Mission hours by decade and sex ----------------------------------------

p <- ggplot(
  astronauts_by_decade_and_sex_tidy,
  aes(x = hours_mission_subtotal, y = decade))
p <- p + plot_features

# Close graphics device
showtext_auto(FALSE)