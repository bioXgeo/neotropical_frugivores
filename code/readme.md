# Frugivoria
Frugivoria is a trait database containing dietary, life-history, morphological traits as well as IUCN conservation status, for birds and mammals exhibiting frugivory, which are important for seed dispersal, an essential ecosystem service. This version of Frugivoria encompasses species in moist montane forests of Central and South America. Frugivoria and its workflow enables researchers to quantify relationships between traits and the environment, as well as spatial trends in functional diversity, contributing to basic knowledge and applied conservation of frugivores in this region. By harmonizing trait information from disparate sources and providing code to access species occurrence data, this open-access database fills a major knowledge gap and enables more comprehensive trait-based studies of species exhibiting frugivory in this ecologically important region.


## Funding
NASA FINESST Grant #80NSSC19K1332

## Collaborators
- Beth E. Gerstner: PhD Candidate, Michigan State University (MSU)
- Patrick Bills: Data Scientist, MSU
- Phoebe L. Zarnetske: PI, [MSU Spatial & Community Ecology Lab (SpaCE Lab)](http://www.communityecologylab.com)

## Directories

All directories are named for the data level, in accordance with guidelines from the [Environmental Data Initiative](http://www.environmentaldatainitiative.org) where Level 0 (L0) raw data are read in and cleaned, then output as Level-1 (L1) data, which are subsequently evaluated and summarized as Level-2 (L2) data.

## L0

The L0 subfolder contains scripts for Level-0 (raw data) analysis, mainly pulling and compiling data. This contains the following scripts: 

- 1_IUCN_species_list_subset: IUCN species list & habitat download

- 2_external_trait_database_merge: merge with EltonTraits and resolve taxonomic disparities

- 3_montane_frugivore_subset: subset database by consumption of fruit

- 4_mammal_merge_pantheria: merge final dataset for mammals with PanTHERIA dataset

- 5_database_final_edits: final additions to the database. Merge in range size calculations and add column for presence of taxonomic disparities

- workflow_diagram: diagram demonstrating all steps of building the Frugivoria database

## L1
The L1 subfolder contains scripts for Level-1 analysis, mainly calculating database statistics. Specifically:
- frugivoria_database_analyses_demo: statistical anaylses of database for birds and mammals

## L2
The L2 subfolder contains scripts for Level-2 analysis, mainly visualization of the database. Specifically:

- database_stacked_barplot: stacked barplot showing contribution of external databases to Frugivoria

- trait_count_barplot: relative counts of newly added traits to dataset for birds and mammals

- downloading_gbif_records: code demonstrating how to obtain GBIF records using the database

- study_region_map_fig: map of study region for birds and mammals overlaid on probability of cloud forest

- trait_mapping: map of two traits, mass and generation time for birds and mammals





*This readme last modified by PLZ & BEG 1 Dec 2021*
