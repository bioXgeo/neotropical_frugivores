#Title: Database final edits

#Project:Frugivoria 

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske, Patrick Bills

#Overview: This code adds a column showing taxonomic naming disparities between IUCN and EltonTraits for final database. This column references differences in naming conventions between the IUCN_species_name column and the Elton_species_name column. These differences may be due to changes in taxonomy, inclusion of new species, or potential misspellings. This script also creates a subset of the final mammal and bird databases to include only select shared cross-taxa traits. This subset is meant to enhance ease of use for individuals looking to conduct cross-taxa analyses. 

#Data Input: Frugivoria_bird_database_2023_full.csv, Frugivoria_mammal_database_2023_full.csv

#Data Output: Frugivoria_bird_database_2023_full.csv, Frugivoria_mammal_database_2023_full.csv, Frugivoria_mammal_database_2023_subset.csv, Frugivoria_bird_database_2023_subset.csv

#Requires: All LO code should be run first.

#Date:  February 21st, 2023

library(dplyr)


# Read in mammal database
mam <- read.csv("INSERT DATABASE PATH HERE") #final_mammal_dataset.csv
# Read in bird database
bird<- read.csv("INSERT DATABASE PATH HERE") #bird_frug.csv

## Taxonomic disparities

# Add column for taxonomic disparities
mam$taxonomic_disparity <- ifelse(mam$IUCN_species_name == mam$elton_species_name, '0',
                                  ifelse(mam$IUCN_species_name != mam$elton_species_name, '1','NA'))

# Calculate how many species names are mismatched
length(mam$taxonomic_disparity[mam$taxonomic_disparity==1]) #174

# Birds
bird$taxonomic_disparity <- ifelse(bird$IUCN_species_name == bird$elton_species_name, '0',
                                   ifelse(bird$IUCN_species_name != bird$elton_species_name, '1','NA'))

length(bird$taxonomic_disparity[bird$taxonomic_disparity==1]) #195

# Save as new database
setwd("INSERT PATH HERE")
write.csv(mam, "Frugivoria_mammal_database_2023_full.csv")
write.csv(bird, "Frugivoria_bird_database_2023_full.csv")

## Create database subset of cross-taxa traits
mam_subset <- mam %>% select(IUCN_species_name, common_name, family, genus, species, subspecies, elton_species_name, diet_cat, diet_source_e, diet_breadth, diet_level, activity_nocturnal_e, activity_source_e, activity_level, for_strat_value_e,	for_strat_certainty_e, body_mass_e, body_mass_level_e, body_mass_source_e, body_size_mm, body_size_notes, body_size_level, body_size_source, sexual_dimorphism, sexual_dimorphism_notes, sexual_dimorphism_level, sexual_dimorphism_source, longevity, longevity_notes, longevity_level, longevity_source, home_range_size, home_range_notes, home_range_level, home_range_source, habitat_specialization, habitat_specialization_source, generation_time, generation_time_notes, generation_time_level, generation_time_source, IUCN_category	,habitat, habitat_breadth, habitat_breadth_source, habitat_level, mean_CHELSA_bio1_1981.2010_V.2.1, mean_CHELSA_bio12_1981.2010_V.2.1, mean_human_fp_range_2010, mean_human_fp_range_2020, percent_change_hf_2010_2020, inferred_range_sqkm, date_data_obtained, filled_by) 


# Bird dataset does not have the same source and level columns for activity patterns as the mammal dataset (EltonTraits does not contain these columns for birds). Also included forest strata since the user can categorize similarly to mammals "for_strat_value" if they choose their own thresholds.
bird_subset <- bird %>% select(IUCN_species_name, common_name, family_e, genus, species, elton_species_name, diet_cat_e, diet_source_e, diet_breadth, diet_level, activity_nocturnal_e, for_strat_ground_e, for_strat_understory_e, for_strat_midhigh_e, for_strat_canopy_e, for_strat_aerial_e, for_strat_spec_level, for_strat_source_e, body_mass_e, body_mass_level_e, body_mass_source_e, body_size_mm, body_size_notes, body_size_level, body_size_source, sexual_dimorphism, sexual_dimorphism_notes, sexual_dimorphism_level, sexual_dimorphism_source, longevity, longevity_notes, longevity_level, longevity_source, home_range_size, home_range_notes, home_range_level, home_range_source, habitat_specialization, habitat_specialization_source, generation_time, generation_time_notes, generation_time_level, generation_time_source, IUCN_category	,habitat, habitat_breadth, habitat_breadth_source, habitat_level, mean_CHELSA_bio1_1981.2010_V.2.1, mean_CHELSA_bio12_1981.2010_V.2.1, mean_human_fp_range_2010, mean_human_fp_range_2020, percent_change_hf_2010_2020, inferred_range_sqkm, date_data_obtained, filled_by) 


#write subsets to file
setwd("INSERT PATH HERE")
write_excel_csv(mam_subset, "Frugivoria_mammal_database_2023_subset.csv")
write_excel_csv(bird_subset, "Frugivoria_bird_database_2023_subset.csv")






