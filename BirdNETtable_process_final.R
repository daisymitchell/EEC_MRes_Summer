rm(list=ls())
setwd("/Users/daisymitchell/MyRCoursework/Code")

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(vegan)
library(openxlsx)




#### Read in all BirdNET tables #####
folder_path <- "/Users/daisymitchell/Desktop/BirdNETtables"
file_list <- list.files(path = folder_path, pattern = "*.txt", full.names = TRUE)

for (file_path in file_list) {
  # Extract the file name without the extension
  file_name <- tools::file_path_sans_ext(basename(file_path))
  # Read the file into a data frame
  assign(file_name, read.delim(file_path))
}


#### Combine the dfs to determine total obs. and no. species #####
df_names <- ls(pattern = "BirdNETtable$")
total_table <- bind_rows(lapply(df_names, get))
print(unique(total_table$Common.Name))
# Total obs. = 56698 and Total no. species = 130






##### If same species occurs within same WAV file, only keep one occurrence (with highest confidence) #####
folder_path <- "/Users/daisymitchell/Desktop/BirdNETtables"
file_list <- list.files(path = folder_path, pattern = "*.txt", full.names = TRUE)

for (file_path in file_list) {
  # Extract file name without extension
  file_name <- tools::file_path_sans_ext(basename(file_path))
  # Remove 'BirdNETtable' part
  site_name <- sub("BirdNETtable", "", file_name)
  # Read file into data frame
  df <- read.delim(file_path)
  # Extract only the part of the 'Begin.Path' starting from '2024' onwards
  df$EndofBegin.Path <- sub(".*(2024.*)", "\\1", df$Begin.Path)
  # Filter so that if species is occurring multiple times within the same wav file (same site, day, and minute)
  # only the occurrence with the highest Confidence is kept
  df_filtered <- df %>%
    group_by(EndofBegin.Path, Species.Code) %>%
    slice_max(order_by = Confidence, n = 1) %>%
    ungroup()
  # Assign the filtered data frame to a variable with the file name
  assign(paste0(site_name, "_filtered"), df_filtered)
}


## Add a Site column to each df
filtered_df_names <- grep("_filtered$", ls(), value = TRUE)

for (df_name in filtered_df_names) {
  # Extract base name before '_filtered'
  base_name <- sub("_filtered$", "", df_name)
  # Retrieve df by name
  df <- get(df_name)
  # Add 'Site' column with the base name value
  df <- df %>%
    mutate(Site = base_name)
  # Assign df back to its original name
  assign(df_name, df)
}

# It made a dataframe called df but its just Weils so remove it
rm(df_filtered)





#### Combine filtered dfs #####
filtered_df_names <- grep("_filtered$", ls(), value = TRUE)
# Retrieve dfs
filtered_dfs <- mget(filtered_df_names)
# Combine
combined_df <- bind_rows(filtered_dfs)
# Export
write.table(combined_df, file = "/Users/daisymitchell/Desktop/combined_filtered_data.txt", sep = "\t", row.names = FALSE)


