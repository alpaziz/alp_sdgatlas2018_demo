library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(forcats)
library(gtable)
library(countrycode)
library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)

# Prepare data for fig_sdg1_income_histogram_poverty
years = c(1990, 2013)
region = "WLD"

income_histogram_poverty_df <- read_excel("inputs/sdg1/SDG1_f1_global_dist_histogram.xlsx")

income_histogram_poverty_df <- income_histogram_poverty_df %>%
  filter(regioncode == region) %>%
  select(year, povertyline, people, p_label) %>%
  filter(year %in% years) %>%
  mutate(people = people * 1e6) %>%         # people cols are in millions
  mutate(p_label = recode(p_label,
                          ".5-1"  = "0.5-1",
                          "1-1.9" = "1-1.90",
                          "1.9-4" = "1.90-4"
  ))


# Save df
save(income_histogram_poverty_df, file = "inputs/data/income_histogram_poverty_df.rda")





