rm(list=ls())
setwd("/Users/daisymitchell/MyRCoursework/Code")

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(vegan)
library(stats)
library(lme4)
library(broom)
library(openxlsx)
library(sampling)


totalbirdnet_df <- read.delim("/Users/daisymitchell/Desktop/combined_filtered_data.txt")



###### Create a table for all species occurring <20 times ######
under_twenty <- totalbirdnet_df %>%
  group_by(Common.Name) %>%
  filter(n() < 20) %>%
  ungroup()

print(unique(under_twenty$Common.Name))

under_twenty_unique <- under_twenty %>%
  distinct(Common.Name, .keep_all = TRUE)

under_twenty_species <- under_twenty_unique %>%
  select(Common.Name)

write.xlsx(under_twenty_species, file = "/Users/daisymitchell/Desktop/under_twenty_species.xlsx")






#### REMOVE SPECIES OCCURING <20 TIMES (PRE-VALIDATION STEP) ####
over_twenty <- totalbirdnet_df %>%
  group_by(Common.Name) %>%
  filter(n() >= 20) %>%
  ungroup()

print(unique(over_twenty$Common.Name))
#### 10141 obs across 49 species (after filtering for occur >20 times) ####
## Export this
write.csv(over_twenty, file = "/Users/daisymitchell/Desktop/species_over_twenty.csv")


confidence_summary <- over_twenty %>%
  group_by(Common.Name) %>%
  summarise(mean_confidence = mean(Confidence, na.rm = TRUE),
            median_confidence = median(Confidence, na.rm = TRUE),
            sd_confidence = sd(Confidence, na.rm = TRUE),
            min_confidence = min(Confidence, na.rm = TRUE),
            max_confidence = max(Confidence, na.rm = TRUE),
            n = n()) %>%
  arrange(desc(mean_confidence))








#### SELECT OBS TO VALIDATE USING STRATIFIED RANDOM SAMPLING ####
# STRATA ARE BASED ON CONFIDENCE SCORE INTERVALS - VALIDATE MORE FROM HIGHER STRATA #

# Create function for stratified random sampling
stratified_sampling <- function(df, species, n_low = 4, n_mid = 6, n_high = 10) {
  df %>%
    arrange(Confidence) %>%
    mutate(
      Stratum = ntile(Confidence, 3),
      Stratum = case_when(
        Stratum == 1 ~ "Low",
        Stratum == 2 ~ "Mid",
        Stratum == 3 ~ "High"
      )
    ) %>%
    group_by(Stratum) %>%
    group_modify(~ sample_n(.x, size = min(n(), if_else(.y$Stratum == "Low", n_low, if_else(.y$Stratum == "Mid", n_mid, n_high)))))
}

# Apply to df
for_validation <- over_twenty %>%
  group_by(Common.Name) %>%
  group_modify(~ stratified_sampling(.x, .y$Common.Name)) %>%
  ungroup()


#### Add a column for filename to allow extraction from HP Desktop folder of wav files ####
for_validation <- for_validation %>%
  mutate(filename = str_extract(Begin.Path, "[^\\\\]+$"))

table(for_validation$Common.Name)
table(for_validation$Stratum)


## Export file 
write.xlsx(for_validation, file = "/Users/daisymitchell/Desktop/files_to_validate.xlsx", rowNames = FALSE)






##### ADD COLUMN OF STRATUM MIN AND MAX CONFIDENCE VALUES #####

## editing stratified sampling to add columns in for_validation that have min and max
## confidence value intervals associated with each species' high mid and low strata
stratified_sampling <- function(df, species, n_low = 4, n_mid = 6, n_high = 10) {
  df <- df %>%
    arrange(Confidence) %>%
    mutate(
      Stratum = ntile(Confidence, 3),
      Stratum = case_when(
        Stratum == 1 ~ "Low",
        Stratum == 2 ~ "Mid",
        Stratum == 3 ~ "High"
      )
    )
  
  intervals <- df %>%
    group_by(Stratum) %>%
    summarize(
      Min_Confidence = min(Confidence),
      Max_Confidence = max(Confidence),
      .groups = 'drop'
    )
  
  df <- df %>%
    left_join(intervals, by = "Stratum")
  
  sampled_df <- df %>%
    group_by(Stratum) %>%
    group_modify(~ sample_n(.x, size = min(n(), if_else(.y$Stratum == "Low", n_low, if_else(.y$Stratum == "Mid", n_mid, n_high)))))
  
  return(sampled_df)
}

# Apply to df
for_validation <- over_twenty %>%
  group_by(Common.Name) %>%
  group_modify(~ stratified_sampling(.x, .y$Common.Name)) %>%
  ungroup()

## Export file - overwriting one without min and max confidence interval columns
write.xlsx(for_validation, file = "/Users/daisymitchell/Desktop/files_to_validate.xlsx", rowNames = FALSE)





#### CREATE DF CONTAINING THE MIN CONFIDENCE VALUES ASSOCIATED WITH EACH SPECIES' HIGH MID LOW STRATA ####

# Define the stratified sampling function
stratified_sampling <- function(df) {
  df %>%
    arrange(Confidence) %>%
    mutate(
      Stratum = ntile(Confidence, 3),
      Stratum = case_when(
        Stratum == 1 ~ "Low",
        Stratum == 2 ~ "Mid",
        Stratum == 3 ~ "High"
      )
    )
}

# Apply the stratified sampling function to assign strata
stratified_df <- over_twenty %>%
  group_by(Common.Name) %>%
  group_modify(~ stratified_sampling(.x)) %>%
  ungroup()

# Calculate the minimum confidence values for each stratum and species
strata_min_df <- stratified_df %>%
  group_by(Common.Name, Stratum) %>%
  summarize(Min_Confidence = min(Confidence), .groups = 'drop') %>%
  pivot_wider(names_from = Stratum, values_from = Min_Confidence, names_prefix = "min_") %>%
  rename(high_min = min_High, mid_min = min_Mid, low_min = min_Low)

# Export as csv
write.csv(strata_min_df, file = "/Users/daisymitchell/Desktop/min_conf_per_strata.csv")

