#Project: Frugivoria - Extracting Birds of the World shapfiles of interest for calculating range size categories of both birds and mammals

#Purpose: This script converts from Geodatabase to shapefiles for the BOTW dataset and calculates range size for both BOTW (bird) and IUCN (mammal) datasets. It then places the range size for each species into designated size categories. These categories are based on range size quantiles for the whole range map dataset.

#Date: Feb 9th, 2021
#Author: Beth E. Gerstner
#   Edited by Patrick Bills

library(sf)
library(rgeos)
library(rgdal)
library(dplyr)
library(geosphere)


#Birds of the World Dataset - In GeoDatabase format, which needs to be converted to a spatial polygons dataframe
# - BOTW Geodatabase saved here: https://drive.google.com/file/d/1W8qDTLaC1IxV8ogqf6OUHEsFs8dVctSk/view?usp=sharing

#To do this, you need to use the command line. Code here: 

#Geodatabase to shapefile
#Set working directory to wherever you save the BOTW dataset
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Data/BOTW_11_20")

#IN TERMINAL
# set directory where the geodatabase is in the terminal
cd /Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Data/BOTW_11_20

#In terminal
ogr2ogr -f 'ESRI SHAPEFILE' BOTW BOTW.gdb

#set working directory to wherever the newly converted spatial dataset is
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Data/BOTW_11_20/BOTW")

# Pull in newly converted bird dataset using sf package (way faster than using OGR function)
BOTW_bird <-read_sf("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Data/BOTW_11_20/BOTW.shp")

#convert this to spatial dataframe
BOTW_spatial<- as(BOTW_bird,"Spatial")

#Subset this shapefile by species list of interest
#Set working directory to databases on the google drive so we can pull out of the IUCN_species_names

montane_birds_old <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/L1/working_databases/database_csv_2_9_21/original_montane_birds.csv")
montane_birds_new <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/L1/working_databases/database_csv_2_9_21/newly_added_montane_birds.csv")

#Pull out the scientific name column 
birds_1 <- as.data.frame(montane_birds_old$IUCN_species_name)
birds_2 <- as.data.frame(montane_birds_new$IUCN_species_name)

#Full list of species names for montane birds. We will use this list to subset the spatial dataframe of all bird ranges
all_montane_bird_names <- rbind(birds_1, birds_2)

#Subset spatial dataframe by species list. Pretty sure the species name in the BOTW data is under the column SCINAME, but may be different now. This is worth double checking... if only I could open this file on my computer :(

BOTW_montane_subset <BOTW_spatial %>
  filter(SCINAME %in% all_montane_bird_names$IUCN_species_name)
