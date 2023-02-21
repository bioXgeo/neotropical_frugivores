#Title: BOTW Processing

#Project: Frugivoria

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske, Patrick Bills

#Overview: Loads and processes the Birds of the World (BOTW) geodatabase from BirdLife International. This dataset has to be requested.

#Data Input: BOTW.gdb; final_bird_dataset.csv

#Data Output: BOTW_subset.shp

#Date: 2/05/23


# Load packages
library(sf)
library(dplyr)

# Set working directory to folder where the BOTW geodatabase is stored
setwd("INSERT PATH HERE")

# Read in the proper layer (this takes considerable time; approximately an hour or more)
all_botw <- st_read("BOTW.gdb", layer = "All_Species")   

# Read in completed bird database (or any species list to use for subsetting)
bird <-read.csv("INSERT PATH HERE") #final_bird_dataset.csv from L0_3_frugivore_subset.R

# Extract species name column from bird database
scientific_name_b<- bird %>%
  select(IUCN_species_name)

# Rename species name column to match that of species list of interest
colnames(all_botw)[2] <- "IUCN_species_name"

# Subset BOTW to species list
frug_bird <- all_botw %>% filter(IUCN_species_name %in% scientific_name_b$IUCN_species_name)

# Format will be in "GEOMETRY" and need this to be a "MULTIPOLYGON".
frug_bird_mp <- st_cast(frug_bird, "MULTIPOLYGON")

# Write spatial subset to a shapefile
st_write(frug_bird_mp, "BOTW_subset.shp")
