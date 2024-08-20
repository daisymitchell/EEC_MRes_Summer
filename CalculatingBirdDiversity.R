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
library(ape)
library(ggplot2)
library(reshape2)
library(tibble)


totalbirdnet_df <- read.csv("/Users/daisymitchell/Desktop/final_bird_dataset.csv")

sites <- read.csv("/Users/daisymitchell/Desktop/SiteParameters.csv")


#### Calculate species richness and total occurrences per site ####
site_diversities <- totalbirdnet_df %>%
  group_by(Site) %>%
  summarise(
    species_richness = n_distinct(Common.Name),
    total_occurrences = n()
  )


##### Create abundance presence-absence matrix for biodiveristy calcs #####
bird_abundance <- select(totalbirdnet_df, Common.Name, Site)
# count occurrences of each species at each site
abundance_df <- bird_abundance %>%
  group_by(Common.Name, Site) %>%
  summarise(Count = n(), .groups = 'drop')
# make abundance matrix
abundance_matrix <- abundance_df %>%
  pivot_wider(names_from = Site, values_from = Count, values_fill = list(Count = 0))
# transpose so sites are rows and species columns
abundance_matrix <- t(abundance_matrix)

# Convert to df to set rownames as sites
abundance_df <- as.data.frame(abundance_matrix, stringsAsFactors = FALSE)
colnames(abundance_df) <- abundance_df[1, ]
abundance_df <- abundance_df[-1, ]
row_names <- rownames(abundance_df)

# Convert all values to numeric
abundance_df <- lapply(abundance_df, function(x) as.numeric(as.character(x)))
# Restore row names
rownames(abundance_df) <- row_names
# Convert back to a matrix
abundance_matrix <- as.matrix(abundance_df)


#### Calculate Shannon diversity per site ####
shannondiv <- diversity(abundance_matrix, index = "shannon")
view(shannondiv)
shannondf <- as.data.frame(shannondiv)
# convert sites from rownames back to a column
shannondf <- rownames_to_column(shannondf, var = "Site")

## Add Shannon values to site_diversities 
site_diversities <- left_join(site_diversities, shannondf, by = "Site")


## Export site_diversities table
write.csv(site_diversities, file = "/Users/daisymitchell/Desktop/site_diversities.csv")








-----------------------------------------------------------------------------------------------------------
# old ideas below




######### Find any species that only occur in one site ###############
# Create the new dataframe
single_site_occurrences <- totalbirdnet_df %>%
  group_by(Common.Name, Site) %>%          # Group by species and site
  summarise(Occurrences = n()) %>%         # Count occurrences at each site
  ungroup() %>%
  group_by(Common.Name) %>%                # Group by species
  filter(n() == 1) %>%                     # Keep only species that occur at one site
  ungroup()

# View the resulting dataframe
print(single_site_occurrences)





############ Find any species that only occur in one site type ##################
# Merge totalbirdnet_df with sites to add the Site_type column
totalbirdnet_df <- totalbirdnet_df %>%
  left_join(sites, by = "Site")  

# Identify species that occur in only one site type
species_single_site_type <- totalbirdnet_df %>%
  group_by(Common.Name, Site_type) %>%    
  summarise(Occurrences = n()) %>%        
  ungroup() %>%
  group_by(Common.Name) %>%               
  filter(n_distinct(Site_type) == 1) %>%  
  ungroup()

print(species_single_site_type)









############ Find any species for which 90% or more occurrences are within one site type ##################
# Calculate total occurrences of each species across all site types
species_total_occurrences <- totalbirdnet_df %>%
  group_by(Common.Name) %>%
  summarise(Total_Occurrences = n())

# Calculate occurrences of each species within each site type
species_site_type_occurrences <- totalbirdnet_df %>%
  group_by(Common.Name, Site_type) %>%
  summarise(Site_Type_Occurrences = n()) %>%
  ungroup()

# Calculate the percentage of occurrences within each site type
species_percentage <- species_site_type_occurrences %>%
  left_join(species_total_occurrences, by = "Common.Name") %>%
  mutate(Percentage = (Site_Type_Occurrences / Total_Occurrences) * 100)

# Filter for species where 90% or more occurrences are in one site type
species_90_percent <- species_percentage %>%
  filter(Percentage >= 90) %>%
  select(Common.Name, Site_type, Percentage)

print(species_90_percent)




