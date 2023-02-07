#Title: Calculating range based traits and appending to databases

#Project: Frugivoria

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske, Patrick Bills

#Overview: Derives range based traits from both IUCN and BirdLife International spatial databases. Here we calculate range size for all species and, in a addition, calculate mean annual temperature over range, mean annual precipitation over range, and average human footprint index. Both temperature and precipitation data are based on the ERA Interim Reanalysis (CHELSA dataset; https://chelsa-climate.org/). The human footprint index was sourced from the Wildlife Conservation Society (https://wcshumanfootprint.org/)

#Data Input: Frugivoria_bird_database_2023.csv, Frugivoria_mammal_database_2023.csv, CHELSA Bioclim 1, CHELSA Bioclim 12, Human Footprint Index years 2010 & 2020.

#Data Output: Frugivoria_bird_database_2023.csv, Frugivoria_mammal_database_2023.csv with added range based traits.

#Date: 1/31/23

#Load libraries
library(sf)
library(raster)
library(dplyr)
library(exactextractr)

# Read in all range maps from IUCN (2022)
mam_shp <- st_read("C:/Users/bgers/Desktop/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")

# Read in all range maps for BirdLife International (2022) - subsetted to species of interest in MSU's HPC
bird_shp <-st_read("C:/Users/bgers/Desktop/frugivoria_range/BOTW_subset.shp")


## Read in Frugivoria databases
# Read in bird database
bird <- read.csv("G:/Shared drives/SpaCE_Lab_neotropical_frugivores/Manuscripts/Database_Manuscript/Database_paper/EDI_resubmission_2023/databases_2023/Frugivoria_bird_database_2023.csv")

# Read in mammal database
mam <-read.csv("G:/Shared drives/SpaCE_Lab_neotropical_frugivores/Manuscripts/Database_Manuscript/Database_paper/EDI_resubmission_2023/databases_2023/Frugivoria_mammal_database_2023_complete.csv")


# Subset mammal and bird databases so that we have a list of their scientific names
scientific_name_m<- mam %>%
  select(IUCN_species_name)

scientific_name_b<- bird %>%
  select(IUCN_species_name)

# Rename sci_name column to match 
colnames(mam_shp)[2] <- "IUCN_species_name"
colnames(bird_shp)[2] <- "IUCN_species_name"


# Filter the mammal shapefile so that only species in the Frugivoria database remain; this was already done for birds on the HPC
frug_mam <- mam_shp %>% filter(IUCN_species_name %in% scientific_name_m$IUCN_species_name)
frug_bird <- bird_shp

# Remove all shapefiles that have a presence code above 3 (this removes parts of the range where the species is extinct or likely extinct)
frug_mam_rm <- frug_mam[!frug_mam$presence >3,]
frug_bird_rm <- frug_bird[!frug_bird$presenc >3,]

# Remove Ursus americanus because it's causing issues
all_mam_polygon <-all_mam_polygon %>%  filter(!IUCN_species_name=='Ursus americanus')

# Separates the multiple polygons per species into individual polygons, which will help calculations down the line
all_mam_polygon <-st_cast(frug_mam_rm, "MULTIPOLYGON") %>% st_cast("POLYGON")
all_bird_polygon <-st_cast(frug_bird_rm, "MULTIPOLYGON") %>% st_cast("POLYGON")

# Turn into SpatialPolygon for further calculations
frug_mam_spat <- as(all_mam_polygon, 'Spatial')
frug_bird_spat <- as(all_bird_polygon, 'Spatial')

# Load Bio1 (mean annual temp) and Bio12 (mean annual precipitation) (Bioclim variables from CHELSA dataset)
mean_temp_range <- raster("C:/Users/bgers/Desktop/frugivoria_range/CHELSA_bio1_1981-2010_V.2.1.tif")
mean_precip_range <-  raster("C:/Users/bgers/Desktop/frugivoria_range/CHELSA_bio12_1981-2010_V.2.1.tif")

#Load human footprint data from WCS (already resampled to match CHELSA; see commented out code below to run this part)
human_fp_range_2020 <- raster("C:/Users/bgers/Desktop/frugivoria_range/human_fp_range_2020_p.tif")
human_fp_range_2010 <- raster("C:/Users/bgers/Desktop/frugivoria_range/human_fp_range_2010_p.tif")

#Resample human_fp to match that of climate data
#human_fp_range_2020 <- resample(human_fp_range_2020,mean_temp_range,method="bilinear")
#writeRaster(human_fp_range_2020, filename="human_fp_range_2020_p.tif", format="GTiff", overwrite=T)

#human_fp_range_2010 <- resample(human_fp_range_2010,mean_temp_range,method="bilinear")
#setwd("/mnt/ufs18/home-048/gerstn11/IUCN_shape")
#writeRaster(human_fp_range_2010, filename = "human_fp_range_2010_p.tif", format="GTiff", overwrite=T )

# Stack all environmental variables
env <- stack(mean_temp_range,mean_precip_range, human_fp_range_2010,human_fp_range_2020)

# Extract values of each variable over the range. Here, we are summing all the values within the species range and counting the number of cells in each range for later calculation of mean. This helps overcome the issue where some species have multiple polygons. Will later add values of each individual polygon together. Do not use "extract" function in raster package here because it runs too slow.
mean_variables_m <- exact_extract(env, frug_mam_spat, fun=c("sum","count"), append_cols="IUCN_species_name", coverage_area=T)
mean_variables_b <- exact_extract(env, frug_bird_spat, fun=c("sum","count"), append_cols="IUCN_species_name", coverage_area=T)

#Group all polygons by IUCN_species_name, which allows sums to be calculated for each species; only want one value for each species)
#mammals
variable_sums_m <- mean_variables_m %>% group_by(IUCN_species_name) %>% 
  summarise(mean_CHELSA_bio1_1981.2010_V.2.1=sum(sum.CHELSA_bio1_1981.2010_V.2.1),
            mean_CHELSA_bio12_1981.2010_V.2.1= sum(sum.CHELSA_bio12_1981.2010_V.2.1),
            mean_human_fp_range_2010_p= sum(sum.human_fp_range_2010_p),
            mean_human_fp_range_2020_p= sum(sum.human_fp_range_2020_p),
            count.CHELSA_bio1_1981.2010_V.2.1=sum(count.CHELSA_bio1_1981.2010_V.2.1),
            count.CHELSA_bio12_1981.2010_V.2.1=sum(count.CHELSA_bio1_1981.2010_V.2.1),
            count.human_fp_range_2010_p=sum(count.human_fp_range_2010_p),
            count.human_fp_range_2020_p=sum(count.human_fp_range_2010_p)) %>% as.data.frame()

#birds
variable_sums_b <- mean_variables_b %>% group_by(IUCN_species_name) %>% 
  summarise(mean_CHELSA_bio1_1981.2010_V.2.1=sum(sum.CHELSA_bio1_1981.2010_V.2.1),
            mean_CHELSA_bio12_1981.2010_V.2.1= sum(sum.CHELSA_bio12_1981.2010_V.2.1),
            mean_human_fp_range_2010_p= sum(sum.human_fp_range_2010_p),
            mean_human_fp_range_2020_p= sum(sum.human_fp_range_2020_p),
            count.CHELSA_bio1_1981.2010_V.2.1=sum(count.CHELSA_bio1_1981.2010_V.2.1),
            count.CHELSA_bio12_1981.2010_V.2.1=sum(count.CHELSA_bio1_1981.2010_V.2.1),
            count.human_fp_range_2010_p=sum(count.human_fp_range_2010_p),
            count.human_fp_range_2020_p=sum(count.human_fp_range_2010_p)) %>% as.data.frame()


# Takes two climate variables and divides them by the number of cells in the range (# cells are variable specific due to NAs)
clim_means_m <-variable_sums_m %>%
  mutate_at(
    c("mean_CHELSA_bio1_1981.2010_V.2.1", "mean_CHELSA_bio12_1981.2010_V.2.1"),
    funs(. / count.CHELSA_bio1_1981.2010_V.2.1)
  )

clim_means_b <-variable_sums_b %>%
  mutate_at(
    c("mean_CHELSA_bio1_1981.2010_V.2.1", "mean_CHELSA_bio12_1981.2010_V.2.1"),
    funs(. / count.CHELSA_bio1_1981.2010_V.2.1)
  )

# Remove unnecessary columns
clim_means_m <- clim_means_m[,c("IUCN_species_name","mean_CHELSA_bio1_1981.2010_V.2.1","mean_CHELSA_bio12_1981.2010_V.2.1")]
clim_means_b <- clim_means_b[,c("IUCN_species_name","mean_CHELSA_bio1_1981.2010_V.2.1","mean_CHELSA_bio12_1981.2010_V.2.1")]

# Takes two human footprint variables and divides them by the number of cells in the range (# cells are variable specific due to NAs)
# mammals
human_footprint_means_m <-variable_sums_m %>%
  mutate_at(
    c("mean_human_fp_range_2010_p", "mean_human_fp_range_2020_p"),
    funs(. / count.human_fp_range_2010_p)
  )

# birds
human_footprint_means_b <-variable_sums_b %>%
  mutate_at(
    c("mean_human_fp_range_2010_p", "mean_human_fp_range_2020_p"),
    funs(. / count.human_fp_range_2010_p)
  )

#remove unnecessary columns
human_footprint_means_m <- human_footprint_means_m[,c("IUCN_species_name","mean_human_fp_range_2010_p", "mean_human_fp_range_2020_p")]
human_footprint_means_b <- human_footprint_means_b[,c("IUCN_species_name","mean_human_fp_range_2010_p", "mean_human_fp_range_2020_p")]


#merge climate and footprint data together
env_calculation_means_m <- merge(clim_means_m, human_footprint_means_m, by="IUCN_species_name")
env_calculation_means_b <- merge(clim_means_b, human_footprint_means_b, by="IUCN_species_name")

# Write to file so do not have to run again (takes a while)
setwd("C:/Users/bgers/Desktop/frugivoria_range")
write.csv(env_calculation_means_m, file="mean_env_mammal.csv")
write.csv(env_calculation_means_b, file="mean_env_bird.csv")

# Percent change in human impact since 2010
env_calculation_means_m$percent_change_hf_2010_2020 <- apply(env_calculation_means_m[,c('mean_human_fp_range_2020_p', 'mean_human_fp_range_2010_p')], 1, function(x) { (x[1]-x[2])/x[2] * 100 } )
write.csv(env_calculation_means_m, file="mean_env_mammal.csv")

env_calculation_means_b$percent_change_hf_2010_2020 <- apply(env_calculation_means_b[,c('mean_human_fp_range_2020_p', 'mean_human_fp_range_2010_p')], 1, function(x) { (x[1]-x[2])/x[2] * 100 } )
write.csv(env_calculation_means_b, file="mean_env_bird.csv")

##Calculating range sizes
#known and inferred presence

#calculate inferred range size (is in km2)
all_mam_polygon$inferred_range_sqkm <- st_area(st_transform(all_mam_polygon, 4326))/(1000*1000) #Take care of units

table(sf::st_is_valid(all_bird_polygon))

# Some bird ranges are not valid and spatial metrics can't be calculated. Remove those ranges
all_bird_polygon_e <-all_bird_polygon[sf::st_is_valid(all_bird_polygon),] #137 not valid

# Calculate inferred range for birds
all_bird_polygon_e$inferred_range_sqkm <- st_area(st_transform(all_bird_polygon_e, 4326))/(1000*1000) #Take care of units

# Group by species name and add up range sizes for each species
all_mam_inferred_range <-all_mam_polygon %>% group_by(IUCN_species_name) %>% 
  summarise(inferred_range_sqkm=sum(inferred_range_sqkm)) %>% as.data.frame()

all_bird_inferred_range <-all_bird_polygon_e %>% group_by(IUCN_species_name) %>% 
  summarise(inferred_range_sqkm=sum(inferred_range_sqkm)) %>% as.data.frame()

# Observed presence
# Remove all shapefiles that have a presence code above 1 (this removes parts of the range where the species is inferred to be)
# mammal
frug_mam_pres_only <- frug_mam[!frug_mam$presence >1,]

# Remove Ursus americanus because it's causing issues
frug_mam_pres_only_1 <-frug_mam_pres_only %>%  filter(!IUCN_species_name=='Ursus americanus')

# birds
frug_bird_pres_only <- frug_bird[!frug_bird$presenc >1,]

# Separates multipolygons into polygons, which will help calculations down the line
all_mam_polygon_po <-st_cast(frug_mam_pres_only_1, "MULTIPOLYGON") %>% st_cast("POLYGON")
all_bird_polygon_po <-st_cast(frug_bird_pres_only, "MULTIPOLYGON") %>% st_cast("POLYGON")

# Some bird ranges are not valid and spatial metrics can't be calculated. Remove those ranges
all_bird_polygon_po_e <-all_bird_polygon_po[sf::st_is_valid(all_bird_polygon_po),]

#Calculate range size of observed range
all_mam_polygon_po$observed_range_sqkm <- st_area(st_transform(all_mam_polygon_po, 4326))/(1000*1000) #Take care of units)
all_bird_polygon_po_e$observed_range_sqkm <- st_area(st_transform(all_bird_polygon_po_e, 4326))/(1000*1000) #Take care of units)

# Group by species and add up range sizes (this takes a while to run with large datasets. Grab a few cups of coffee)
all_mam_observed_range <-all_mam_polygon_po %>% group_by(IUCN_species_name) %>% 
  summarise(observed_range_sqkm=sum(observed_range_sqkm)) %>% as.data.frame()

all_bird_observed_range <-all_bird_polygon_po_e %>% group_by(IUCN_species_name) %>% 
  summarise(observed_range_sqkm=sum(observed_range_sqkm)) %>% as.data.frame()

# Merge range data and remove irrelevant columns
final_range_data_m <- merge(all_mam_observed_range,all_mam_inferred_range, by="IUCN_species_name")
final_range_data$geometry.x <- NULL
final_range_data$geometry.y<- NULL

final_range_data_b <- merge(all_bird_observed_range,all_bird_inferred_range, by="IUCN_species_name")
final_range_data_b$geometry.x <- NULL
final_range_data_b$geometry.y<- NULL

# Create final dataframe with all spatial calculations
final_spatial_calcs_m <- merge(env_calculation_means_m, final_range_data_m, by="IUCN_species_name", all=T)
write.csv(final_spatial_calcs_m, file="final_spatial_calculations_mammals.csv")

final_spatial_calcs_b <- merge(env_calculation_means_b, final_range_data_b, by="IUCN_species_name", all=T)
write.csv(final_spatial_calcs_b, file="final_spatial_calculations_birds.csv")

#how many species missing spatial data
missing_mam <- mam %>% filter(!IUCN_species_name %in% final_spatial_calcs_m$IUCN_species_name)

missing_bird <- bird %>% filter(!IUCN_species_name %in% final_spatial_calcs_b$IUCN_species_name) #5 birds without spatial data
