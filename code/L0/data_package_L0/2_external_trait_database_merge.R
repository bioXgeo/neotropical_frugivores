#Title: External trait database merge

#Project: Montane Frugivoria

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske, Patrick Bills

#Data input: EltonTraits dataset (Wilman et al. 2014); IUCN species lists from code "1_IUCN_species_list_subset": mam_IUCN.csv, bird_IUCN.csv, manually generated lookup table

#Data output: fully-harmonized merge with EltonTraits - bird_trait_all_final.csv, mamm_trait_all_final.csv

#Overview: Merge IUCN species list subset with two external trait databases, Elton Traits (feeding and activity traits for birds and mammals) and PanTHERIA (morphological, life history and geographic traits for mammals only). To do this, trait databases had to be harmonized, meaning that differences in nomenclature had to be corrected for there to be successful merging. This is both a manual and automated process and will be time consuming for any set of taxa. This process allowed us to create "lookup tables" for the dataset where we list the correct names as assigned in each database. 

#Requires: "1_IUCN_species_list_subset" to be run first

#Notes: Harmonization process was done manually and is noted in the code.

#Date: Oct 10th, 2020

#Modified: Oct 15th, 2021


#Trait Information

## Now that we have habitat information for all mammals and birds in Central and South American countries of interest, we need to figure out which are frugivores. We do this by using the Elton Traits Database (Wilman et al. 2014)

#Read in Elton Traits dataset. Download available here: https://figshare.com/articles/dataset/Data_Paper_Data_Paper/3559887?backTo=/collections/EltonTraits_1_0_Species-level_foraging_attributes_of_the_world_s_birds_and_mammals/3306933
#Convert from .txt to .csv
birds <- read.csv("INSERT BIRD ELTON PATH HERE")
mamm<- read.csv("INSERT MAMMAL ELTON PATH HERE")

#read in final outputs from previous code "1_IUCN_species_list_subset"
bird_IUCN <- read.csv("insert path to output here")
mammal_IUCN <- read.csv("insert path to output here")

#Combine bird EltonTraits dataset (full) and IUCN bird dataset for Latin America together
colnames(birds)[which(names(birds) == "Scientific")] <- "scientific_name"
bird_trait_IUCN<- merge(bird_IUCN, birds, by= "scientific_name", all.x=TRUE)

#Combine mammal EltonTraits dataset and IUCN bird data together
colnames(mamm)[which(names(mamm) == "Scientific")] <- "scientific_name"
mamm_trait_IUCN <- merge.data.frame(mammal_IUCN, mamm, by= "scientific_name", all.x=TRUE)

setwd("INSERT PATH HERE")
write.csv(bird_trait_IUCN, 'bird_trait_IUCN.csv')
write.csv(mamm_trait_IUCN, 'mamm_trait_IUCN.csv')

# Some of these rows had no corresponding species in the EltonTraits dataset (NAs in these rows) but cam be filled in manually. This could be due to spelling errors or changes in taxonomic nomenclature.
# Will eventually remove species that have lower than a 10% frugivorous diet after resolving the issues with naming conventions. Necessary to figure out alternate names for species with NA for Elton Traits. 

## Resolving Taxonomies
# Mammals

# Go through the merges that did not work correctly (kept all IUCN species that were not found in the Elton Traits database) and compare against synonym lists.
# Correcting issues with species not merging correctly between databases
# Find species that did not match correctly
mamm_trait_na <-mamm_trait_IUCN[is.na(mamm_trait_IUCN$Diet.Vunk),]  
mamm_trait_na <- mamm_trait_na[!duplicated(mamm_trait_na[,c('scientific_name')]),] #391 IUCN mammal species didn't merge correctly

#Manually look up species with NA for EltonTrait synonyms. Find alternate names for species and create an alternate name list, or "lookup table". First column labeled "IUCN_species_name", second column for "Elton_name". Pull EltonTraits for those species and append to the original merged dataset.


#Download lookup table for mammals (saved here:)
mam_lookup_final <- read.csv("INSERT PATH HERE")

# Find species names present in the lookup table that didn't merge correctly 
alternate_match <-unique(as.data.frame(mamm_trait_na$scientific_name[mamm_trait_na$scientific_name %in% mam_lookup_final$IUCN_species_name])) #200 species names found 

# Change column name in alternate_match
colnames(alternate_match)[which(names(alternate_match) == "mamm_trait_na$scientific_name[mamm_trait_na$scientific_name %in% mam_lookup_final$IUCN_species_name]")] <- "IUCN_name_with_alternate"

#choose row in lookup table with the alternate match and get the IUCN name
mamm_lookup_subset <-mam_lookup_final %>%
  filter(IUCN_species_name %in% alternate_match$IUCN_name_with_alternate)

#add column to the first merge so that we can keep track of IUCN names vs. alternate names 
mamm_trait_IUCN$IUCN_species_name <- ""

#there are potentially some white spaces trailing some species name in the the Elton_name column
mamm_lookup_subset$elton_name <- trimws(mamm_lookup_subset$elton_name, which = c("right"))

#Merge list of alternate names with elton_traits to append them to the original merge 
mamm_trait_alternates <- merge(mamm, mamm_lookup_subset, by.x=c("scientific_name"), by.y=c("elton_name"))

## Remove duplicates with NAs

#transform the alternate names so you can subset the merged dataset and remove those names (will re-attach edited names).
#Need to remove the scientific_names (which are the IUCN_name) in the original dataset that have NA's for EltonTraits. This way the alternate names can be merged back into the scientific_names. 
mam_alternates_matrix <- as.matrix(mamm_trait_alternates$IUCN_species_name)

alternates_df <- as.data.frame(mam_alternates_matrix)

#Remove the duplicate species names (wrong names and correct names) because we'll be merging them back in. 
mam_trait_alt_subset <-mamm_trait_IUCN %>%
  filter(!scientific_name %in% alternates_df$V1)

#Merge IUCN information back into the mamm_trait_alternates object
mamm_trait_alternates_IUCN_info <- merge(mamm_trait_alternates, mammal_IUCN, by.x="IUCN_species_name",by.y="scientific_name")

#Bind the alternate name object to the original edited merge. The scientific name column are the elton_names.
#Fully harmonized dataset for EltonTraits
mam_trait_all_final <-rbind(mamm_trait_alternates_IUCN_info, mam_trait_alt_subset)
write.csv(mam_trait_all_final, "mam_trait_all_final.csv")


# Birds

#Find bird names that didn't merge correctly
bird_trait_na <-bird_trait_IUCN[is.na(bird_trait_IUCN$Diet.Vunk),]  
bird_trait_na <- bird_trait_na[!duplicated(bird_trait_na[,c('scientific_name')]),] # 873 IUCN bird species didn't merge correctly

# Change column name in alternate_match
colnames(alternate_match_b)[which(names(alternate_match_b) == "bird_trait_na$scientific_name[bird_trait_na$scientific_name %in% bird_lookup_final$IUCN_species_name]")] <- "IUCN_name_with_alternate"

#download lookup tables
bird_lookup_final <- read.csv("INSERT BIRD LOOKUP TABLE PATH HERE")

#choose row in lookup table with the alternate match and get the IUCN name
bird_lookup_subset <-bird_lookup_final %>%
  filter(IUCN_species_name %in% alternate_match_b$IUCN_name_with_alternate)

#add column to the first merge so that we can keep track of IUCN names vs. alternate names 
bird_trait_IUCN$IUCN_species_name <- ""

#there are potentially some white spaces trailing some species name in the the Elton_name column
bird_lookup_subset$elton_name <- trimws(bird_lookup_subset$elton_name, which = c("right"))

#Merge list of alternate names with elton_traits to append them to the original merge 
bird_trait_alternates <- merge(birds, bird_lookup_subset, by.x=c("scientific_name"), by.y=c("elton_name"))

## Remove duplicates with NAs

#transform the alternate names so I can subset the merged dataset and remove those names (will re-attach edited names)
bird_alternates_matrix <- as.matrix(bird_trait_alternates$IUCN_species_name)

alternates_df_b <- as.data.frame(bird_alternates_matrix)

#Remove the duplicate species names (wrong names and correct names) because we'll be merging them back in. We want the correct  and 

bird_trait_alt_subset <-bird_trait_IUCN %>%
  filter(!scientific_name %in% alternates_df_b$V1)

#Merge IUCN information back into the mamm_trait_alternates object
bird_trait_alternates_IUCN_info <- merge(bird_trait_alternates, bird_IUCN, by.x="IUCN_species_name",by.y="scientific_name")

#Bind the alternate name object to the original edited merge. The scientific name column is the elton_trait names.
bird_trait_all_final <-rbind(bird_trait_alternates_IUCN_info, bird_trait_alt_subset)

#write to a file
write.csv(bird_trait_all_final, "bird_trait_all_final.csv")
