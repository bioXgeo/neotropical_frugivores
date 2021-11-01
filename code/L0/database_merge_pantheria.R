
#load library
library(dplyr)

#Read in PanTHERIA spreadsheet
pantheria <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Mammals_Birds_all_data/Trait_Database_2019/PanTHERIA_traits.csv")

#Change column name to match that of mammal database species names (IUCN_species_names)
colnames(pantheria)[which(names(pantheria) == "MSW05_Binomial")] <- "IUCN_species_name"

#remerge pantheria and original montane database so we have all of the columns (deleted old merge)
original_mam <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/data/frugivore/L1/montane_mam_database_10_11_21_range_tax.csv")

original_mam <- original_mam[,1:77]

mamm_pantheria_original <- merge.data.frame(original_mam, pantheria, by= "IUCN_species_name", all.x=TRUE)

write.csv(mamm_pantheria_original, "/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/L1/working_databases/database_csv_2_9_21/pantheria_added_mammals/mamm_original_pantheria.csvmamm_pantheria <- merge.data.frame(mont_mam_new, pantheria, by= "IUCN_species_name", all.x=TRUE)")


#find species that did not match correctly
mamm_pantheria_na_original_t <-mamm_pantheria_original[is.na(mamm_pantheria_original$MSW05_Order),]
write.csv(mamm_pantheria_na_original, "/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/L1/working_databases/database_csv_2_9_21/pantheria_added_mammals_mamm_pantheria_na.csv")

#remove the species with NA's from the dataset. Will merge those species back in later.
mamm_pantheria_original_no_NA <-mamm_pantheria_original[!is.na(mamm_pantheria_original$MSW05_Order),]

#Find alternate names (Elton Names [scientific_name]) present in PanTHERIA database
mamm_pantheria_alt_names_original <- pantheria %>%
  filter(!IUCN_species_name %in% mamm_pantheria_na_original_t$elton_species_name) #81 found

#remove Pantheria columns from the NA dataset so we don't get repeats
pantheria_rm_na_original <-mamm_pantheria_na_original[,-c(77:130)]

#change the species name to "scientific_name"
colnames(mamm_pantheria_alt_names_original)[which(names(mamm_pantheria_alt_names_original) == "IUCN_species_name")] <- "scientific_name"
colnames(pantheria_rm_na_original)[which(names(pantheria_rm_na_original) == "IUCN_species_name")] <- "scientific_name"

#merge the original dataset (pantheria removed) that failed to pair with PanTHERIA with the alternate/Elton names PanTHERIA dataset
full_subset_NA_original <- merge.data.frame(pantheria_rm_na_original, mamm_pantheria_alt_names_original, by="scientific_name", all=TRUE)

#merge original dataset with NA's removed with this final dataset
final_original_dataset <- rbind(full_subset_NA_original, mamm_pantheria_original_no_NA)
write.csv(final_original_dataset, "/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/L1/working_databases/database_csv_2_9_21/pantheria_added_mammals/final_datasets_tofix/final_original_dataset_fixed_3_29_21.csv")

#issue... need to add species back in that are NA's for PanTHERIA that aren't in the species list
#How many species with NAs are not found in  alternate names dataset

#mamm_pantheria_NAs_add_back <- mamm_pantheria_na_original %>%
  filter(!scientific_name %in% mamm_pantheria_alt_names_original$scientific_name)

#edited csv in excel
original_species <-read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/L1/working_databases/database_csv_2_9_21/pantheria_added_mammals/final_datasets_tofix/final_original_dataset_fixed_3_29_21.csv")

#random 10% check 
#sample <-original_species[sample(nrow(original_species), 19), ]
#original_species$check <- sample$taxonid[match(original_species$IUCN_species_name, sample$IUCN_species_name)]
#length(unique(original_species$genus))

#read in edited csv
#group by "filled_by" column and check 10% of each of these groups. Equals a total of 30 test observations. 
full_montane_dataset <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/L1/working_databases/database_csv_2_9_21/pantheria_added_mammals/final_datasets_tofix/montane_mammal_database_checks - complete_montane_mammal_databas.csv")
#Stratified random sampling based on genus or student
library(dplyr)
set.seed(1)
Test2 <- full_montane_dataset %>%
  group_by(Filled_by) %>%
  slice_sample(prop = .1)

#Create a check column in the final dataset
full_montane_dataset$check <- ""

#Subset the check list to only include species names and a check column for easy merging with final dataset
species_check <-as.data.frame(Test2$IUCN_species_name)
colnames(species_check)[which(names(species_check) == "Test2$IUCN_species_name")] <- "IUCN_species_name"
species_check$check <-"check"

#merge checklist and final dataset and remove extraneous columns 
final_check_list <- merge(species_check,full_montane_dataset, by="IUCN_species_name", all.y = T)
colnames(final_check_list)[which(names(final_check_list) == "check.x")] <- "check"
final_check_list <-final_check_list[,-c(134)]

write.csv(Test2, "/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/L1/working_databases/database_csv_2_9_21/pantheria_added_mammals/final_datasets_tofix/checked_species_mammals.csv")

#_______________________________________________________________________________________________________________________
#Bird final dataset 

bird_1 <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/L1/working_databases/database_csv_2_9_21/pantheria_added_mammals/final_datasets_tofix/bird_montane_original.csv")
bird_2 <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/L1/working_databases/database_csv_2_9_21/pantheria_added_mammals/final_datasets_tofix/bird_montane_new.csv")

full_montane_bird_dataset <- rbind(bird_1,bird_2)

#Write full montane bird dataset to a file
write.csv(full_montane_bird_dataset, "/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/L1/working_databases/database_csv_2_9_21/pantheria_added_mammals/final_datasets_tofix/complete_montane_bird_database_4_13_21.csv")

no_match_1 <- mamm_pantheria_na_original_t %>%
  filter(elton_species_name %in% pantheria$IUCN_species_name) #81 found

