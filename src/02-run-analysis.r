################################################################################
#
# Project:  Data Management and Cleaning Workshop
# Purpose:  Run analysis
# Author:   Patrick Lavallee Delgado
# Created:  2 February 2024
#
# Notes:    Do not overwrite raw data!
#
# To do:
#
################################################################################


# Load packages.
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(openxlsx)

# Identify inputs and outputs.
PWD <- getwd()
DTA <- file.path(PWD, "out", "data.rda")
FIG <- file.path(PWD, "out", "plot.png")
OUT <- file.path(PWD, "out", "analysis.xlsx")

# Read long and wide analysis files.
# This loads the two datasets we created into the same namespaces.
load(DTA)
head(stu_sch_long)
head(stu_sch_wide)

# Plot test scores by race.
# Transform data so that each point on the plot is a separate row.
stu_sch_long |>
  select(stuid, grade, gender, race, math, ela) |>
  filter(!is.na(gender)) |>
  # Reshape long on test subject.
  pivot_longer(
    cols = c(math, ela),
    names_to = "test_subj",
    values_to = "test_val"
  ) |>
  # Reshape wide on grade.
  pivot_wider(
    names_from = grade,
    names_prefix = "grade_",
    values_from = test_val
  ) |>
  # Draw plot.
  ggplot(aes(x = grade_6, y = grade_7, color = race)) +
  geom_point() +
  geom_abline() +
  facet_wrap(
    facets = vars(gender, test_subj),
    labeller = labeller(test_subj = as_labeller(c(
      "math" = "Math",
      "ela" = "English/Language Arts"
    )))
  ) +
  labs(
    title = "Math and ELA test score growth by gender and race",
    x = "6th grade score",
    y = "7th grade score",
    color = "Race"
  ) +
  theme_minimal()

# Save the figure.
ggsave(FIG)

# Calculate unadjusted means by grade and gender.
tab1 <- stu_sch_long |>
  group_by(grade, female) |>
  summarize(across(c(math, ela, sci, soc), ~ mean(.x, na.rm = TRUE)))
tab1

# Estimate differences in math scores by gender, conditional on prior scores.
# Growth among students classified as female decreased by 0.5 points on average.
# Results are statistically different from zero at the 5% level.
reg1 <- lm(math_7 ~ female + math_6, data = stu_sch_wide)
summary(reg1)

# Set the reference group in the race category to White.
stu_sch_wide$race <- relevel(stu_sch_wide$race, ref = "White")

# Estimate differences in ELA scores by race, conditional on prior scores.
# Disparities in growth are not statistically different from zero.
reg2 <- lm(ela_7 ~ race + ela_6, data = stu_sch_wide)
summary(reg2)

# Collect regression output.
regs <- list("math_female" = reg1, "ela_race" = reg2) |>
  lapply(tidy) |>
  bind_rows(.id = "model")

# Save tabulation and regression output to Excel.
ws <- list("summaries" = tab1, "models" = regs)
write.xlsx(ws, OUT)
