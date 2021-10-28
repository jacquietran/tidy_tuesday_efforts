# Load libraries ---------------------------------------------------------------

library(dplyr)
library(stringr)
library(ggplot2)
library(ggtext)

# Read data into R -------------------------------------------------------------

ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

# Tidy up ----------------------------------------------------------------------

race %>%
  select(race_year_id, event, race, participation, distance,
         contains("elevation"), aid_stations, participants) %>%
  left_join(., ultra_rankings) %>%
  filter(race_year_id == "68575") %>%
  filter(!is.na(time_in_seconds)) %>%
  mutate(
    participation = str_to_lower(participation),
    time_in_hours = time_in_seconds / 3600,
    age_category = case_when(
        age <= 24           ~ "20-24 years",
        age > 24 & age <=29 ~ "25-29 years",
        age > 29 & age <=34 ~ "30-34 years",
        age > 34 & age <=39 ~ "35-39 years",
        age > 39 & age <=44 ~ "40-44 years",
        age > 44 & age <=49 ~ "45-49 years",
        age > 49 & age <=54 ~ "50-54 years",
        age > 54 & age <=59 ~ "55-59 years",
        age > 59 & age <=64 ~ "60-64 years",
        age > 64 & age <=69 ~ "65-69 years")) -> monviso

# Plot -------------------------------------------------------------------------

p <- ggplot(
  monviso, aes(x = age_category, y = time_in_hours))
p <- p + geom_boxplot(outlier.shape = NA)
p <- p + geom_point(alpha = 0.5, size = 8, aes(colour = gender))
p <- p + scale_colour_discrete(
  labels = c("Men", "Women"))
p <- p + labs(
  title = "Where are all the women in Monviso???",
  subtitle = "Finishing times of competitors in the 100 Miglia Monviso, 2021 (160 km race)",
  x = NULL, y = "Race time (hours)",
  caption = "**Source:** Benjamin Nowak & ITRA // **Plot:** @jacquietran")
p <- p + theme_minimal()
p <- p + theme(
  text = element_text(size = 18),
  plot.caption = element_markdown(size = NULL, margin=margin(15,0,0,0)),
  legend.position = "top",
  legend.title = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank())

ggsave(
  here::here("2021-10-28/monviso.png"),
  last_plot(), device = "png", dpi = 300,
  width = 12, height = 9, units = "in", bg = "#FFFFFF")