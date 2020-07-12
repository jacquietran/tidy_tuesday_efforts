# Load libraries ---------------------------------------------------------------

library(here)
library(dplyr)
library(tidyr)
library(paletteer)
library(showtext)
library(ggplot2)
library(corrr)
library(caret)
library(ggalt)

# Import data ------------------------------------------------------------------

source_data <- tidytuesdayR::tt_load('2020-07-07')

# Wrangle data -----------------------------------------------------------------

source_data_df <- source_data$coffee_ratings

# Isolate only the necessary variables
source_data_df %>%
  select(
    cupper_points, aroma, flavor, aftertaste, acidity, body, balance, uniformity,
    clean_cup, sweetness) %>%
  filter(
    cupper_points != 0
    ) -> grades_by_cupper_points

# Create a long format data set for plotting
grades_by_cupper_points %>%
  pivot_longer(
    cols = c("aroma", "flavor", "aftertaste", "acidity", "body", "balance",
             "uniformity", "clean_cup", "sweetness"),
    names_to = "criteria",
    values_to = "value"
  ) -> grades_by_cupper_points_long

# Bivariate correlations -------------------------------------------------------

# Calculate correlation coefficients
cor_coef_matrix <- correlate(grades_by_cupper_points)

# Plot: Grades x cupper points -------------------------------------------------

# Load font
font_add_google("Special Elite", "specialelite")

# Custom facet labels with correlation coefficients
criteria_labels <- c(
  "aroma" = "Aroma (r = 0.60)",
  "flavor" = "Flavour (r = 0.74)",
  "aftertaste" = "Aftertaste (r = 0.73)",
  "acidity" = "Acidity (r = 0.61)",
  "body" = "Body (r = 0.52)",
  "balance" = "Balance (r = 0.65)",
  "uniformity" = "Uniformity (r = 0.19)",
  "clean_cup" = "Clean cup (r = 0.24)",
  "sweetness" = "Sweetness (r = 0.03)")

# Build plot
p <- ggplot(
  grades_by_cupper_points_long,
  aes(x = value,
      y = cupper_points))
p <- p + facet_wrap(
  ~criteria,
  nrow = 3, labeller = labeller(criteria = criteria_labels))
p <- p + geom_point(
  alpha = 0.15,
  size = 3,
  colour = paletteer_d("ochRe::olsen_seq")[4])
p <- p + geom_smooth(
  method = "lm",
  colour = "#FFFFFF",
  linetype = "dashed",
  se = FALSE)
p <- p + scale_x_continuous(
  limits = c(0, 10),
  breaks = seq(0, 10, by = 2))
p <- p + labs(
  title = "Coffee ratings: Overall cupper points & bean attributes",
  subtitle = "r = Pearson's correlation coefficient",
  x = "Grade", y = "Overall points")
p <- p + theme_minimal()
p <- p + theme(
  text = element_text(size = 32, colour = "#FFFFFF", family = "specialelite"),
  legend.position = "none",
  plot.background = element_rect(
    fill = paletteer_d("NineteenEightyR::cobra")[3],
    colour = NA),
  axis.text = element_text(colour = "#999999"),
  panel.grid.major = element_line(colour = "#333333"),
  panel.grid.minor = element_blank(),
  strip.text = element_text(colour = "#FFFFFF"),
  strip.background = element_rect(
    fill = paletteer_d("ochRe::emu_woman_paired")[6],
    colour = NA))

showtext_auto()

# Export to PNG
ggsave(
  here("2020-07-07/plot_cupper_points_x_grades.png"),
  last_plot(),
  device = "png",
  type = "cairo",
  width = 18, height = 12, units = "cm", dpi = 300)

showtext_auto(FALSE)

# Linear regression ------------------------------------------------------------

# Fit linear model using variables with r > 0.50
initial_model <- lm(
  cupper_points ~ aroma + flavor + aftertaste + acidity + body + balance,
  data = grades_by_cupper_points)

# Check summary of initial model
summary(initial_model)

# Re-fit linear model using only statistically significant parameters
reduced_model <- lm(
  cupper_points ~ flavor + aftertaste + acidity + balance,
  data = grades_by_cupper_points)

# Check summary of reduced model
summary(reduced_model)

# Check variable importance
reduced_model_var_imp <- varImp(reduced_model)
reduced_model_var_imp %>%
  mutate(
    criteria = rownames(reduced_model_var_imp),
    importance_base = 0) %>%
  rename(
    importance = Overall) %>%
  select(
    criteria, importance_base, importance
    ) -> reduced_model_var_imp_tidy

# Plot: Reduced model - Variable importance ------------------------------------

# Build plot
p <- ggplot(
  reduced_model_var_imp_tidy,
  aes(x = importance, y = reorder(criteria, importance)))
p <- p + geom_segment(
  x = reduced_model_var_imp_tidy$importance_base,
  xend = reduced_model_var_imp_tidy$importance,
  y = reduced_model_var_imp_tidy$criteria,
  yend = reduced_model_var_imp_tidy$criteria,
  colour = "#CCCCCC",
  size = 2)
p <- p + geom_point(
  shape = 21, size = 5, fill = paletteer_d("ochRe::galah")[2],
  stroke = 3, colour = "#CCCCCC")
p <- p + scale_x_continuous(
  limits = c(0, 10),
  breaks = seq(0, 10, by = 2))
p <- p + scale_y_discrete(
  labels = c(
    "flavor" = "Flavour",
    "aftertaste" = "Aftertaste",
    "balance" = "Balance",
    "acidity" = "Acidity"))
p <- p + labs(
  title = "Variable importance for estimating overall cupper points",
  x = "Importance", y = NULL)
p <- p + theme_minimal()
p <- p + theme(
  text = element_text(size = 32, colour = "#FFFFFF", family = "specialelite"),
  legend.position = "none",
  plot.background = element_rect(
    fill = paletteer_d("NineteenEightyR::cobra")[3],
    colour = NA),
  axis.text = element_text(colour = "#999999"),
  panel.grid.major = element_line(colour = "#333333"),
  panel.grid.major.y = element_blank(),
  panel.grid.minor = element_blank())
  
showtext_auto()

# Export to PNG
ggsave(
  here("2020-07-07/plot_reduced_model_var_importance.png"),
  last_plot(),
  device = "png",
  type = "cairo",
  width = 16, height = 6, units = "cm", dpi = 300)

showtext_auto(FALSE)