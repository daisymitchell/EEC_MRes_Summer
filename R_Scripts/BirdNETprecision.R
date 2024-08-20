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

## Read in the validated files csv
validated <- read.csv("/Users/daisymitchell/Desktop/validatedfiles.csv")
table(validated$TrueFalseUnsure)

## Conservative approach - convert all unsure into false
class(validated$TrueFalseUnsure)

conservative <- validated
conservative$TrueFalseUnsure[conservative$TrueFalseUnsure == "Unsure"] <- "FALSE"
table(conservative$TrueFalseUnsure)


## Find any species that were 100% inaccurate during validation
species_with_all_false <- conservative %>%
  group_by(Common.Name) %>%
  summarize(all_false = all(TrueFalseUnsure == FALSE)) %>%
  filter(all_false) %>%
  pull(Common.Name)
print(species_with_all_false)
# Great spotted woodpecker - this always construction noise mistaken for them drumming
# Northern fulmar - always construction noise mistaken for their calls


## Also removing certain species based on ecology - very unlikely to be in CoL/in June
# Eurasian curlew
# Short-toed Treecreeper
# Redwing


#### Final list of species to be removed from main dataset: ####
# Great Spotted Woodpecker, Northern Fulmar, Eurasian Curlew, Short-toed Treecreeper, Redwing 


## Read in dataset
birds_df <- read.csv("/Users/daisymitchell/Desktop/species_over_twenty.csv")

## Remove above species
species_to_remove <- c("Great Spotted Woodpecker", "Northern Fulmar", "Eurasian Curlew", "Short-toed Treecreeper", "Redwing")

# Remove all occurrences of the specified species from the dataframe
birds_filtered <- birds_df %>%
  filter(!(Common.Name %in% species_to_remove))
print(unique(birds_filtered$Common.Name))
#### This leaves 9915 obs. across 44 species ####

## Swap these names to the more likely sub-species:
# 'Green-winged Teal' to 'Teal'
# 'White Wagtail' to 'Pied Wagtail'
birds_filtered$Common.Name <- recode(birds_filtered$Common.Name, 
                                     "Green-winged Teal" = "Eurasian Teal", 
                                     "White Wagtail" = "Pied Wagtail")

print(unique(birds_filtered$Common.Name))



#### Now perform precision calculations - and then filter remaining species ####

## Calculate precision as: p = Tp / (Tp + Fp)
## Using 10 BirdNET confidence score thresholds placed evenly between 0.5-0.95

# Remove species from conservative df
species_to_remove <- c("Great Spotted Woodpecker", "Northern Fulmar", "Eurasian Curlew", "Short-toed Treecreeper", "Redwing")
conservative <- conservative %>%
  filter(!(Common.Name %in% species_to_remove))
print(unique(conservative$Common.Name))
# Re-name these two species
conservative$Common.Name <- recode(conservative$Common.Name, 
                            "Green-winged Teal" = "Eurasian Teal", 
                            "White Wagtail" = "Pied Wagtail")
print(unique(conservative$Common.Name))


# Define the confidence thresholds
confidence_thresholds <- seq(0.5, 0.95, by = 0.05)

# Function to calculate precision for a given species and confidence threshold
calculate_precision <- function(df, species, threshold) {
  filtered_df <- df %>%
    filter(Common.Name == species & Confidence >= threshold)
  
  if(nrow(filtered_df) == 0) {
    return(NA)
  }
  
  true_positives <- sum(filtered_df$TrueFalseUnsure == "TRUE")
  total_positives <- nrow(filtered_df)
  
  precision <- true_positives / total_positives
  return(precision)
}

# Get unique species names
species_list <- unique(conservative$Common.Name)

# Create empty list to store results
results <- list()

# Loop through each species and confidence threshold to calculate precision
for(species in species_list) {
  for(threshold in confidence_thresholds) {
    precision_value <- calculate_precision(conservative, species, threshold)
    results <- append(results, list(data.frame(
      Species = species,
      ConfidenceThreshold = threshold,
      Precision = precision_value
    )))
  }
}

# Combine the results into a df
precision <- do.call(rbind, results)
view(precision)
print(unique(precision$Species))

# Re-name Species column
colnames(precision)[colnames(precision) == "Species"] <- "Common.Name"



#### Find all species' minimum confidence threshold required for 90% precision ####
species_list <- unique(precision$Common.Name)

# Create empty dataframe to store results
min_confidence_thresholds <- data.frame(Common.Name = character(), MinThreshold = numeric(), stringsAsFactors = FALSE)

# Loop through each species and confidence threshold to calculate precision and find minimum threshold for 90% precision
for(species in species_list) {
  min_threshold <- NA
  for(threshold in confidence_thresholds) {
    precision_value <- calculate_precision(conservative, species, threshold)
    if (!is.na(precision_value) && precision_value >= 0.9) {
      min_threshold <- threshold
      break
    }
  }
  min_confidence_thresholds <- rbind(min_confidence_thresholds, data.frame(Common.Name = species, MinThreshold = min_threshold))
}

view(min_confidence_thresholds)





####### FILTER BIRD DATA BASED ON MINIMUM CONFIDENCE THRESHOLDS PER SPECIES #######
# join the MinThreshold column to birds_filtered based on Common.Name
birds_filtered_with_thresholds <- birds_filtered %>%
  left_join(min_confidence_thresholds, by = "Common.Name")

# Filter rows in birds_filtered_with_thresholds where Confidence is above the MinThreshold
filtered_birds <- birds_filtered_with_thresholds %>%
  filter(Confidence >= MinThreshold)

view(filtered_birds)
table(filtered_birds$Common.Name)


#### EXPORT FINAL DATASET TO BE USED ####
write.csv(filtered_birds, file = "/Users/daisymitchell/Desktop/final_bird_dataset.csv")


# Create table of total species counts
species_count <- as.data.frame(table(filtered_birds$Common.Name))
species_count <- species_count[order(-species_count$Freq), ]
# Export
write_xlsx(species_count, "/Users/daisymitchell/Desktop/species_count_final.xlsx")







