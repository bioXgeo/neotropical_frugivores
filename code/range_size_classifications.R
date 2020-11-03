#Project: Frugivoria - Calculating IUCN range size categories
#Purpose: This script checks calculates range size based on IUCN range maps for birds and mammals. It then places the range size for each species into size categories. These categories are based on range size quantiles for the whole range map dataset.
#Date: November 2nd, 2020
#Author: Beth E. Gerstner

library(sf)
library(rgeos)
library(rgdal)
library(dplyr)

##Mammals

setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Data/TERRESTRIAL_MAMMALS_2")
IUCN_mam <-read_sf("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Data/MAMMALS_TERRESTRIAL_Nov/MAMMALS_TERRESTRIAL_ONLY.shp")

#Convert to meters (change projection for that of south America)
IUCN_mam_utm <-st_transform(IUCN_mam,
          crs("+proj=utm +zone=18 +south +datum=WGS84 
              +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Create an area column
IUCN_mam_utm$area <- st_area(IUCN_mam_utm)

#Convert m to km
IUCN_mam_utm$area_km <- (IUCN_mam_utm$area)/1000

#Have more than one range per species. Need to add up areas by ID# and calculate quantiles on a dataset where each species has only ONE range value. 

IUCN_mam_utm_sum <- aggregate(x = IUCN_mam_utm$area, by = list(IUCN_mam_utm$id_no),FUN = "sum")

            
# Compare the range size of each species to the quantiles of the whole dataset and put them into a size category category.

# First, convert the aggregate area column to numeric (it is currently in "units")
IUCN_mam_utm_sum$x<-as.numeric(IUCN_mam_utm_sum$x)

# Calculate percentiles
Percentile_00  = min(IUCN_mam_utm_sum$x)
Percentile_25  = quantile(IUCN_mam_utm_sum$x, 0.25)
Percentile_50  = quantile(IUCN_mam_utm_sum$x, 0.50)
Percentile_75  = quantile(IUCN_mam_utm_sum$x, 0.75)
Percentile_100 = max(IUCN_mam_utm_sum$x)
            
#Turn the pernctile values into a dataframe for easy viewing            
RB = rbind(Percentile_00, Percentile_25, Percentile_50, Percentile_75, Percentile_100)
dimnames(RB)[[2]] = "Value"
RB
            # Percentile_00  0.000000e+00
            # Percentile_25  2.037098e+10
            # Percentile_50  2.597598e+11
            # Percentile_75  2.188229e+12
            # Percentile_100 1.358420e+17
            
#Create new range size column in the summed area dataset            
IUCN_mam_utm_sum$range_size <-""
            
#extremely small range
IUCN_mam_utm_sum$range_size[IUCN_mam_utm_sum$x >= Percentile_00 & IUCN_mam_utm_sum$x <= Percentile_25]  = "0"

#small range            
IUCN_mam_utm_sum$range_size[IUCN_mam_utm_sum$x >= Percentile_25 & IUCN_mam_utm_sum$x <=  Percentile_50]  = "1"
            
#medium            
IUCN_mam_utm_sum$range_size[IUCN_mam_utm_sum$x >= Percentile_50 & IUCN_mam_utm_sum$x <=  Percentile_75]  = "2"

#large            
IUCN_mam_utm_sum$range_size[IUCN_mam_utm_sum$x >= Percentile_75 & IUCN_mam_utm_sum$x <=  Percentile_100]  = "3"

#Add this as a column in the final mammal database for those species with spatial data
#__________________________________________________________________________________________________
##Birds
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Data/BOTW/BOTW_shapefile")

# Pull in bird dataset
IUCN_bird <-read_sf("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Data/BOTW/BOTW_shapefile/All_Species.shp")

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
Percentile_00  = min(0)
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
            
            

            