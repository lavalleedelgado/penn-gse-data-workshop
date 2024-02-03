################################################################################
#
# Project:  Data Management and Cleaning Workshop
# Purpose:  Clean data
# Author:   Patrick Lavallee Delgado
# Created:  2 February 2024
#
# Notes:    Do not overwrite raw data!
#
# To do:
#
################################################################################


# Install packages.
req <- c("tidyverse", "fastDummies", "openxlsx")
new <- req[!(req %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new)

# Load packages.
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(fastDummies)
library(openxlsx)

# Identify inputs and outputs.
PWD <- getwd()
STU <- file.path(PWD, "in", "stu.csv")
SCH <- file.path(PWD, "in", "sch.xlsx")
DTA <- file.path(PWD, "out", "data.rda")
OUT <- file.path(PWD, "out", "data.xlsx")

# Load and peek at student data.
# Need to use the function that corresponds to the file type, e.g. csv.
stu <- read.csv(STU)
head(stu)

# These are some helpful functions for accessing information about the dataset.
print(paste("Number of rows:", nrow(stu)))
print(paste("Number of columns:", ncol(stu)))

################################################################################
# Convert text to numeric data.
################################################################################

# Race is string data.
table(stu$race)

# Break if we get unexpected values.
# This forces a revisit of the code if the data change.
race_vals <- c("Asian", "Black", "Hispanic", "White")
stopifnot(all(stu$race %in% race_vals))

# Cast to factor.
# Each value is now a labeled integer.
stu$race <- as.factor(stu$race)
table(stu$race, as.numeric(stu$race))

# Cast to indicators.
# Each category now has its own variable.
stu <- dummy_cols(stu, "race") |>
  rename_with(~ tolower(gsub("race_", "", .x)))

# Check correspondence.
# We can use an existing list to let the code adapt to the data.
stu |>
  group_by(race) |>
  summarize(across(tolower(race_vals), sum))

# Date of birth is also string data.
table(stu$dob)

# Cast to date.
# Need to use the function that corresponds to the MM/DD/YYYY pattern.
stu$dob <- mdy(stu$dob)

# Plot the distribution of birthdays.
# Bin dates into months.
stu |>
  mutate(mob = floor_date(dob, "month")) |>
  ggplot(aes(x = mob)) +
  geom_bar() +
  labs(
    title = "Birthdays",
    x = "Month of birth",
    y = "Count"
  ) +
  theme_minimal()

################################################################################
# Handle missing data.
################################################################################

# Individualized education plan is an indicator.
summary(stu$iep)
table(stu$iep)

# Break if we get unexpected values.
# What does "-9" mean? This will throw off any summary of IEP status.
iep_vals <- c(-9, 0, 1)
stopifnot(all(stu$iep %in% iep_vals))

# Set missing values.
# Assume the client gave us this definition.
stu$iep[stu$iep == -9] <- NA

# Check the summary again.
# We can now interpret the mean as the share of students with an IEP.
summary(stu$iep)
stopifnot(all(stu$iep[!is.na(stu$iep)] %in% c(0, 1)))

# Subject test scores are continuous.
# All subjects have some "-9" values. Social studies has a lot of zeros.
subjs <- c("math", "ela", "sci", "soc")
summary(stu[, subjs])

# Set missing values.
# Assume the client gave us this definition.
stu <- mutate(stu, across(all_of(subjs), ~ replace(.x, .x == -9, NA)))

# Note the missingness pattern.
# Students have either all test scores or no test scores.
for (i in seq_along(subjs)) {
  stopifnot(all(is.na(stu[, subjs[1]]) == is.na(stu[, subjs[i]])))
}

# Break if we get unexpected values.
# This only checks that non-missing values are in [0, 100].
for (subj in subjs) {
  stopifnot(min(stu[, subj], na.rm = TRUE) >= 0)
  stopifnot(max(stu[, subj], na.rm = TRUE) <= 100)
}

# Plot the distribution of test scores in social studies.
# Zero is perhaps not impossible, but every student received a zero in 2019.
stu |>
  mutate(sy = factor(sy)) |>
  ggplot(aes(x = soc, fill = sy)) +
  geom_histogram(position = "identity") +
  labs(
    title = "Social studies test scores by school year",
    x = "Social studies score",
    y = "Count",
    fill = "School year"
  ) +
  theme_minimal()

# Set missing values.
# Assume we verified that the district awarded no social studies scores in 2019.
stopifnot(all(stu$soc[stu$sy == 2019 & !is.na(stu$soc)] == 0))
stu <- mutate(stu, soc = replace(soc, sy == 2019, NA))

################################################################################
# Handle mixed data types.
################################################################################

# Gender has a mix of numeric and string data.
summary(stu$gender)
table(stu$gender)

# Break if we get unexpected values.
# Need to check what these mean against a codebook!
gender_vals <- c("1", "2", "X")
stopifnot(all(stu$gender %in% gender_vals))

# Cast to numeric.
# Assume the codebook defines "X" as missing.
# This coerces "X" and all other non-numeric characters to "NA".
stu <- stu |>
  rename(gender_old = gender) |>
  mutate(gender = as.numeric(gender_old))
table(stu$gender_old, stu$gender, useNA = "ifany")

# Cast to factor.
# Assume the codebook provides this mapping of values to labels.
gender_labs <- c("Male" = 1, "Female" = 2)
stu <- stu |>
  mutate(gender = factor(
    x = gender,
    levels = gender_labs,
    labels = names(gender_labs)
  ))
table(stu$gender, as.numeric(stu$gender), useNA = "ifany")

# Cast to indicators.
# Assume we decided to label anyone categorized as "F" or "X" as "not male".
# Fill "NA" values in the new indicators with zero to complete coverage.
stu <- dummy_cols(stu, "gender") |>
  rename_with(tolower) |>
  rename(c(male = gender_male, female = gender_female)) |>
  mutate(across(c(male, female), ~ if_else(is.na(.x), 0, .x)))

# Check correspondence.
stu |>
  group_by(gender_old, gender) |>
  summarize(across(c(male, female, gender_na), sum))

################################################################################
# Handle duplicate data.
################################################################################

# Write a function to check uniqueness of identifiers.
# Use this in a chain of tidyverse functions to check your work.
isid <- function(.data, ...) {
  if(any(duplicated(dplyr::select(.data, ...)))) {
    stop("indexers do not uniquely identify the observations")
  }
  return(.data)
}

# Drop perfect duplicates.
# Check, but it is probably safe to assume no loss of information.
nobs <- nrow(stu)
stu <- unique(stu)
dups <- nobs - nrow(stu)
print(paste("Perfect duplicates:", dups))

# Mark remaining duplicates.
# This checks whether the number of rows within a student-year exceeds 1.
stu <- stu |>
  group_by(stuid, sy) |>
  mutate(is_dup = n() > 1) |>
  ungroup()
table(stu$is_dup)

# Show duplicates.
# Looks like some have multiple records for suspensions. Which is right?
stu |>
  filter(is_dup) |>
  arrange(stuid, sy) |>
  select(-is_dup)

# Deduplicate.
# Assume we have no preference between records, so we take the average.
# Student ID and school year now uniquely identify rows in the dataset.
stu <- stu |>
  group_by(stuid, sy) |>
  mutate(oss = if_else(is_dup, mean(oss), oss)) |>
  ungroup() |>
  unique() |>
  isid(stuid, sy)

################################################################################
# Reshape and merge data.
################################################################################

# Load and peek at school data.
# Need to use the function that corresponds to the file type, e.g. Excel.
sch <- read.xlsx(SCH, sheet = "sch")
head(sch)

# Transform to school-year level.
# Note each row describes one school over multiple years. These are "wide" data.
# We can construct a school year variable from the information encoded in the
# column names to make additional rows for each school and school year.
sch <- sch |>
  isid(schid) |>
  pivot_longer(
    # Columns to pivot.
    cols = starts_with(c("frpl", "enrl")),
    # Regular expression to parse data encoded in pivot column names.
    names_pattern = "^([a-z]+)([0-9]+)$",
    # New columns to create; ".value" gets corresponding capture group above.
    names_to = c(".value", "sy"),
    # Cast school year as numeric.
    names_transform = list(sy = as.integer)
  ) |>
  mutate(sy = 2000 + sy) |>
  isid(schid, sy)

# Merge student and school data.
# This adds columns from the second data frame to the first by matching rows on
# shared keys. Note the relationship between the keys of both data frames.
stu_sch_long <- left_join(
  stu,
  sch,
  by = c("schid", "sy"),
  relationship = "many-to-one"
) |>
isid(stuid, sy)

# Check what did not merge.
# Filters the first data frame for rows that do not match in the second. Note
# that all students match, but not all schools do.
check <- anti_join(stu, sch, by = c("schid", "sy"))
stopifnot(nrow(check) == 0)
anti_join(sch, stu, by = c("schid", "sy"))

# Make analytic dataset.
# Transform data so that each student in the regression has their own row.
stu_sch_wide <- stu_sch_long |>
  # Ensure data are unique by student-year and student-grade.
  isid(stuid, sy) |>
  isid(stuid, grade) |>
  # Drop unnecessary columns that vary by student.
  select(-c(sy, is_dup)) |>
  # Reshape wide on grade.
  pivot_wider(
    names_from = grade,
    values_from = c(iep, math, ela, sci, soc, oss, frpl, enrl)
  ) |>
  isid(stuid)

################################################################################
# Save clean file.
################################################################################

# Save to load into another R program.
# This saves the long and wide datasets just as they are.
save(stu_sch_long, stu_sch_wide, file = DTA)

# Save to Excel workbook.
# Provide a named list to write multiple datasets to different worksheets.
ws <- list("long" = stu_sch_long, "wide" = stu_sch_wide)
write.xlsx(ws, OUT)
