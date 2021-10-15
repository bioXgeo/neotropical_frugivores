#Project: Montane Frugivoria

#Code reference: mammal_merge_pantheria

#Purpose: Merge in the PanTHERIA traits for the final mammal database 

#Date: Oct 10th, 2020

#Modified: Oct 15th, 2021

#By: Beth E. Gerstner

#load library
library(dplyr)

#Read in PanTHERIA spreadsheet. PanTHERIA Dataset found here:

pantheria <- read.csv("INSERT PATH HERE")

#Change column name to match that of mammal database species names (IUCN_species_names)
colnames(pantheria)[which(names(pantheria) == "MSW05_Binomial")] <- "IUCN_species_name"

#read in database from previous step (montane_frugivore_subset)
mam_frug_montane <- read.csv("INSERT PATH HERE")

#merge the dataset together
mamm_pantheria_original <- merge.data.frame(mam_frug_montane, pantheria, by= "IUCN_species_name", all.x=TRUE)

#write to file
write.csv(mamm_pantheria, "mamm_pantheria.csv")


#Find species that did not match correctly
mamm_pantheria_na_original_t <-mamm_pantheria_original[is.na(mamm_pantheria_original$MSW05_Order),]
write.csv(mamm_pantheria_na_original, "/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/L1/working_databases/database_csv_2_9_21/pantheria_added_mammals_mamm_pantheria_na.csv")

#remove the species with NA's from the dataset. Will merge those species back in later.
mamm_pantheria_original_no_NA <-mamm_pantheria_original[!is.na(mamm_pantheria_original$MSW05_Order),]

#Find alternate names (Elton Names [scientific_name]) present in PanTHERIA database
mamm_pantheria_alt_names_original <- pantheria %>%
  filter(IUCN_species_name %in% mamm_pantheria_na_original$scientific_name)

#remove Pantheria columns from the NA dataset so we don't get repeats
pantheria_rm_na_original <-mamm_pantheria_na_original[,-c(77:130)]

#change the species name to "scientific_name"
colnames(mamm_pantheria_alt_names_original)[which(names(mamm_pantheria_alt_names_original) == "IUCN_species_name")] <- "scientific_name"

#merge the original dataset (pantheria removed) that failed to pair with PanTHERIA with the alternate/Elton names PanTHERIA dataset
full_subset_NA_original <- merge.data.frame(pantheria_rm_na_original, mamm_pantheria_alt_names_original, by="scientific_name", all=TRUE)

#merge original dataset with NA's removed with this final dataset
final_mammal_dataset <- rbind(full_subset_NA_original, mamm_pantheria_original_no_NA)
write.csv(final_mammal_dataset, "final_mammal_dataset.csv")

