# This respository contains the following data and R scripts:

# Data:
BirdNET_tables - folder containing text files of initial BirdNET output tables for each study site.

SiteCharacteristics - csv file containing the site characteristics data for every study site.

# R scripts:
BirdNET_table_process_final - combining BirdNET tables and filtering out multiple occurrences of the same species in the same minute.

ValidationBirdNET - stratified sampling to select observations for manual validation. 

BirdNETprecision - calculating precision based on manual validation results to determine per-species confidence score thresholds, and filtering out observations below these thresholds.

CalculatingBirdDiversity - calculating the species richness of each site.

BirdSpeciesRichness - all further species richness analyses.

BirdDissimilarity - all community composition analyses.

CoL_Sites_Map - code to create a map of the City of London.
