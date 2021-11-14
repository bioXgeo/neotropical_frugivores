#Project: Montane Frugivoria 

#Purpose: Merge in IUCN range calculations and add column showing taxonomic disparities

#Date: Oct 11th, 2021

#By: Beth E. Gerstner

#read in mammal database
mam <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/data/frugivore/L1/complete_database/Frugivoria_montane_mammal_database.csv")
#read in bird database
bird<- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/data/frugivore/L1/complete_database/Frugivoria_montane_bird_database.csv")

#read in mammal range data
mam_range <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/data/frugivore/L0/frugivore_range_data/montane_mammal_ranges.csv")

colnames(mam_range)[which(names(mam_range) == "iucn_species_name")] <- "IUCN_species_name"
mam_range$X <- NULL

#read in bird range data
bird_range <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/data/frugivore/L0/frugivore_range_data/montane_bird_ranges.csv")
bird_range$X <-NULL
colnames(bird_range)[which(names(bird_range) == "iucn_species_name")] <- "IUCN_species_name"

#merge mammal database and mammal ranges

mam_db_ranges <- merge(mam, mam_range, by="IUCN_species_name", all.x=T)

#merge bird database and bird ranges
  
bird_db_ranges <- merge(bird, bird_range, by="IUCN_species_name", all.x=T)

#add column for taxonomic disparities
mam_db_ranges$taxonomic_disparity <- ifelse(mam_db_ranges$IUCN_species_name == mam_db_ranges$elton_species_name, '0',
                                  ifelse(mam_db_ranges$IUCN_species_name != mam_db_ranges$elton_species_name, '1','NA'))

#how many species names are mismatched?
length(mam_db_ranges$taxonomic_disparity[mam_db_ranges$taxonomic_disparity==1]) #90

#birds
bird_db_ranges$taxonomic_disparity <- ifelse(bird_db_ranges$IUCN_species_name == bird_db_ranges$elton_species_name, '0',
                                   ifelse(bird_db_ranges$IUCN_species_name != bird_db_ranges$elton_species_name, '1','NA'))


#save as new database
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/data/frugivore/L1")
write.csv(mam_db_ranges, "montane_mam_database_11_10_21_range_tax.csv")
write.csv(bird_db_ranges, "montane_bird_database_10_11_21_range_tax.csv")




