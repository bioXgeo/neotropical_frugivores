#Project: Frugivoria - Calculating IUCN range size categories
#Purpose: This script checks calculates range size based on IUCN range maps for birds and mammals. It then places the range size for each species into size categories. These categories are based on range size quantiles for the whole range map dataset.
#Date: November 2nd, 2020
#Author: Beth E. Gerstner

library(sf)
library(rgeos)
library(rgdal)
library(dplyr)
library(geosphere)

##Mammals

setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Data/TERRESTRIAL_MAMMALS_2")
IUCN_mam <-read_sf("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Data/MAMMALS_TERRESTRIAL_Nov/MAMMALS_TERRESTRIAL_ONLY.shp")
IUCN_mam_spatial<- as(IUCN_mam,"Spatial")

#Use geosphere package to calculate area over polygons in Lat/Long projection
testing <-areaPolygon(IUCN_mam_spatial)

#add this as a column in the dataset
IUCN_mam$range_size <-testing
# #Convert to meters (change projection for that of south America)
# IUCN_mam_utm <-st_transform(IUCN_mam,
#           crs("+proj=utm +zone=18 +south +datum=WGS84 
#               +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# 
# #Create an area column
# IUCN_mam_utm$area <- st_area(IUCN_mam_utm)
# 
# #Convert m to km
# 

#Have more than one range per species. Need to add up areas by ID# and calculate quantiles on a dataset where each species has only ONE range value. 

IUCN_mam_sum <- aggregate(x = IUCN_mam$range_size, by = list(IUCN_mam$id_no),FUN = "sum")

            
# Compare the range size of each species to the quantiles of the whole dataset and put them into a size category category.

# First, convert the aggregate area column to numeric (it is currently in "units")
#IUCN_mam_sum$x<-as.numeric(IUCN_mam_sum$x)

# Calculate percentiles
# Percentile_00  = min(IUCN_mam_sum$x)
# Percentile_25  = quantile(IUCN_mam_sum$x, 0.25)
# Percentile_50  = quantile(IUCN_mam_sum$x, 0.50)
# Percentile_75  = quantile(IUCN_mam_sum$x, 0.75)
# Percentile_100 = max(IUCN_mam_sum$x)
            
#Turn the percentile values into a dataframe for easy viewing            
RB = rbind(Percentile_00, Percentile_25, Percentile_50, Percentile_75, Percentile_100)
dimnames(RB)[[2]] = "Value"
RB
# Percentile_00  1.861527e+02
# Percentile_25  1.643067e+10
# Percentile_50  1.693378e+11
# Percentile_75  1.083732e+12
# Percentile_100 8.833135e+13
            
#Create new range size column in the summed area dataset            
IUCN_mam_sum$range_size <-""
            
#extremely small range
IUCN_mam_sum$range_size[IUCN_mam_sum$x >= Percentile_00 & IUCN_mam_sum$x <= Percentile_25]  = "0"

#small range            
IUCN_mam_sum$range_size[IUCN_mam_sum$x >= Percentile_25 & IUCN_mam_sum$x <=  Percentile_50]  = "1"
            
#medium            
IUCN_mam_sum$range_size[IUCN_mam_sum$x >= Percentile_50 & IUCN_mam_sum$x <=  Percentile_75]  = "2"

#large            
IUCN_mam_sum$range_size[IUCN_mam_sum$x >= Percentile_75 & IUCN_mam_sum$x <=  Percentile_100]  = "3"

#Add this as a column in the final mammal database for those species with spatial data

#__________________________________________________________________________________________________
##Birds
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Data/BOTW/BOTW_shapefile")

# Pull in bird dataset. Converted from geodatabase using this code (/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/R_code/IUCN_data/botw_shapefile_conversion.R)
IUCN_bird <-read_sf("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Data/BOTW_11_20/BOTW/All_Species.shp")

IUCN_bird_geo <- st_read("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Data/BOTW_11_20/BOTW.gdb")
#y <- st_drop_zm(x)
test <- as(IUCN_bird_geo, "Spatial")

#Convert to meters (change projection for that of south America)
IUCN_bird_utm <-st_transform(IUCN_bird,
                            crs("+proj=utm +zone=18 +south +datum=WGS84 
              +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Create an area column
IUCN_bird_utm$area <- st_area(IUCN_bird_utm)

#Convert m to km
IUCN_bird_utm$area_km <- (IUCN_bird_utm$area)/1000

#Have more than one range per species. Need to add up areas by ID# and calculate quantiles on a dataset where each species has only ONE range value. 

IUCN_bird_utm_sum <- aggregate(x = IUCN_bird_utm$area, by = list(IUCN_bird_utm$SISID),FUN = "sum")


# Compare the range size of each species to the quantiles of the whole dataset and put them into a size category category.

# First, convert the aggregate area column to numeric (it is currently in "units")
IUCN_bird_utm_sum$x<-as.numeric(IUCN_bird_utm_sum$x)

# Calculate percentiles
Percentile_00  = min(IUCN_bird_utm_sum$x)
Percentile_25  = quantile(IUCN_bird_utm_sum$x, 0.25)
Percentile_50  = quantile(IUCN_bird_utm_sum$x, 0.50)
Percentile_75  = quantile(IUCN_bird_utm_sum$x, 0.75)
Percentile_100 = max(IUCN_bird_utm_sum$x)

#Turn the pernctile values into a dataframe for easy viewing            
RB_bird = rbind(Percentile_00, Percentile_25, Percentile_50, Percentile_75, Percentile_100)
dimnames(RB_bird)[[2]] = "Value"
RB_bird
# Value
# Percentile_00  0.000000e+00
# Percentile_25  4.360404e+10
# Percentile_50  5.353469e+11
# Percentile_75  5.044436e+12
# Percentile_100 5.023983e+15

#Create new range size column in the summed area dataset            
IUCN_bird_utm_sum$range_size <-""

#extremely small range
IUCN_bird_utm_sum$range_size[IUCN_bird_utm_sum$x >= Percentile_00 & IUCN_bird_utm_sum$x <= Percentile_25]  = "0"

#small range            
IUCN_bird_utm_sum$range_size[IUCN_bird_utm_sum$x >= Percentile_25 & IUCN_bird_utm_sum$x <=  Percentile_50]  = "1"

#medium            
IUCN_bird_utm_sum$range_size[IUCN_bird_utm_sum$x >= Percentile_50 & IUCN_bird_utm_sum$x <=  Percentile_75]  = "2"

#large            
IUCN_bird_utm_sum$range_size[IUCN_bird_utm_sum$x >= Percentile_75 & IUCN_bird_utm_sum$x <=  Percentile_100]  = "3"

#testing how this worked
test <-IUCN_bird_utm[IUCN_bird_utm$SISID=="22679763",]
test$SCINAME
#Add this as a column in the final mammal database for those species with spatial data
            
            
library(rredlist)
            
rl_narrative(name="88109476", key = token )
