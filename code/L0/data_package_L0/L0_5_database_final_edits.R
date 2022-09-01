#Title: Database final edits

#Project:Frugivoria 

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske, Patrick Bills

#Overview: Merge in IUCN range calculations (as of 2021) and add column showing taxonomic disparities for final database. Takes database output of scripts "frugivore_subset" for birds and "mammal_merge_PanTHERIA" for mammals.

#Data Input: bird_frug.csv, final_mammal_dataset.csv, full_mammal_ranges_2021.csv, full_bird_ranges_2021.csv

#Data Output: final_mammal_database.csv, final_bird_database.csv

#Requires: "L0_3_frugivore_subset.R" and "L0_4_mammal_merge_pantheria.R" should be run first.

#Date: Aug 27th, 2022


# Read in mammal database
mam <- read.csv("INSERT DATABASE PATH")
# Read in bird database
bird<- read.csv("INSERT DATABASE PATH")

# Read in mammal range data 
mam_range <- read.csv("INSERT RANGE DATA PATH HERE")

colnames(mam_range)[which(names(mam_range) == "iucn_species_name")] <- "IUCN_species_name"
mam_range$X <- NULL

# Read in bird range data
bird_range <- read.csv("INSERT RANGE DATA PATH HERE")
bird_range$X <-NULL
colnames(bird_range)[which(names(bird_range) == "iucn_species_name")] <- "IUCN_species_name"

# Merge mammal database and mammal ranges

mam_db_ranges <- merge(mam, mam_range, by="IUCN_species_name", all.x=T)

# Merge bird database and bird ranges
  
bird_db_ranges <- merge(bird, bird_range, by="IUCN_species_name", all.x=T)

# Add column for taxonomic disparities
mam_db_ranges$taxonomic_disparity <- ifelse(mam_db_ranges$IUCN_species_name == mam_db_ranges$elton_species_name, '0',
                                  ifelse(mam_db_ranges$IUCN_species_name != mam_db_ranges$elton_species_name, '1','NA'))

# How many species names are mismatched?
length(mam_db_ranges$taxonomic_disparity[mam_db_ranges$taxonomic_disparity==1]) #90

# Birds
bird_db_ranges$taxonomic_disparity <- ifelse(bird_db_ranges$IUCN_species_name == bird_db_ranges$elton_species_name, '0',
                                   ifelse(bird_db_ranges$IUCN_species_name != bird_db_ranges$elton_species_name, '1','NA'))

# Save as new database
setwd("INSERT PATH HERE")
write.csv(mam_db_ranges, "final_mammal_database.csv")
write.csv(bird_db_ranges, "final_bird_database.csv")




