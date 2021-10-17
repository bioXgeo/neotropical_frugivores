# Frugivoria
Frugivoria is a trait database containing dietary, life-history, morphological traits as well as IUCN conservation status, for birds and mammals exhibiting frugivory, which are important for seed dispersal, an essential ecosystem service. This version of Frugivoria encompasses species in moist montane forests of Central and South America. Frugivoria and its workflow enables researchers to quantify relationships between traits and the environment, as well as spatial trends in functional diversity, contributing to basic knowledge and applied conservation of frugivores in this region. By harmonizing trait information from disparate sources and providing code to access species occurrence data, this open-access database fills a major knowledge gap and enables more comprehensive trait-based studies of species exhibiting frugivory in this ecologically important region.


## Funding
NASA FINESST

## Collaborators
Beth E. Gerstner (MSU)
Patrick Bills (MSU)
Phoebe L. Zarnetske (MSU)

## L0

The L0 subfolder contains scripts for Level-0 analysis, mainly pulling and compiling data. This contains the following scripts: 

- 1_IUCN_species_list_subset: IUCN species list & habitat download

- 2_external_trait_database_merge: merge with EltonTraits and resolve taxonomic disparities

- 3_montane_frugivore_subset: subset database by consumption of fruit

- 4_mammal_merge_pantheria: merge final dataset for mammals with PanTHERIA dataset

- 5_database_final_edits: final additions to the database. Merge in range size calculations an add column for presence of taxonomic disparities

## L1
The L1 subfolder contains scripts for Level-1 analysis, mainly calculating database statistics. Specifically:
- database_analyses - statistical anaylses of database for birds and mammals

## L2
The L2 subfolder contains scripts for Level-2 analysis, mainly visualization of the database. Specifically:

- 1_database_stacked_barplot: stacked barplot showing contribution of external databases to Frugivoria

- 2_trait_count_barplot: relative counts of newly added traits to dataset for birds and mammals

- 3_downloading_gbif_records: code demonstrating how to obtain GBIF records using the database

- 4_study_region_map_fig: map of study region

- 5_trait_mapping: map of two traits, mass and generation time for birds and mammals













*This readme last modified by BEG... *
