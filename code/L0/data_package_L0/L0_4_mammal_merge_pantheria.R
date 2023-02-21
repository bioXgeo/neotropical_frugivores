#Title: Mammal merge pantheria

#Project: Frugivoria

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske, Patrick Bills

#Overview: This script merges PanTHERIA traits into the mammal database in the same fashion as the "L0_2_external_database_merge.R" script, except for the smaller subset of mammal species.

#Data Input: PanTHERIA dataset (Jones et al. 2009), mam_frug.csv

#Requires: "L0_3_frugivore_subset.R" should be run first; mam_frug.csv

#Data Output: list of species that did not merge correctly - mamm_pantheria_na_original.csv; final mammal dataset with PanTHERIA inclided - final_mammal_dataset.csv

#Date: Oct 10th, 2020

#Modified: Aug 27th, 2022


# Load library
library(dplyr)


# Read in PanTHERIA dataset (can be found here: https://esapubs.org/archive/ecol/E090/184/)
pantheria <- read.csv("INSERT PATH HERE")

# Change column name to match that of mammal database species names (IUCN_species_names)
colnames(pantheria)[which(names(pantheria) == "MSW05_Binomial")] <- "IUCN_species_name"

# Read in database from previous step (L0:3_frugivore_subset; mam_frug)
mam_frug <- read.csv("INSERT PATH HERE")

# Merge the dataset together
mamm_pantheria_original <- merge.data.frame(mam_frug, pantheria, by= "IUCN_species_name", all.x=TRUE)

# Write to file
write.csv(mamm_pantheria, "mamm_pantheria.csv")

# Find species that did not match correctly
mamm_pantheria_na_original <-mamm_pantheria_original[is.na(mamm_pantheria_original$MSW05_Order),]
write.csv(mamm_pantheria_na_original, "pantheria_added_mammals_mamm_pantheria_na.csv")

# Remove the species with NA's from the dataset. Will merge those species back in later. Pantheria merge without any NAs
mamm_pantheria_original_no_NA <-mamm_pantheria_original[!is.na(mamm_pantheria_original$MSW05_Order),]

# Find alternate names (Elton Names [scientific_name]) present in PanTHERIA database
mamm_pantheria_alt_names_original <- pantheria %>%
  filter(IUCN_species_name %in% mamm_pantheria_na_original$elton_species_name)

# Remove Pantheria columns from the NA dataset so we do not get repeats
pantheria_rm_na_original <-mamm_pantheria_na_original[,-c(141:194)]

# Change the species name to "elton_species_name"
colnames(mamm_pantheria_alt_names_original)[which(names(mamm_pantheria_alt_names_original) == "IUCN_species_name")] <- "elton_species_name"

# Merge the original dataset (pantheria removed) that failed to pair with PanTHERIA with the alternate/Elton names PanTHERIA dataset
full_subset_NA_original <- merge.data.frame(pantheria_rm_na_original, mamm_pantheria_alt_names_original, by="elton_species_name", all=TRUE)

# Merge original dataset with NA's removed with this final dataset
final_mammal_dataset <- rbind(full_subset_NA_original, mamm_pantheria_original_no_NA)
write.csv(final_mammal_dataset, "final_mammal_dataset.csv")

