#Title: Calculating range based traits and appending to databases

#Project: Frugivoria

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske, Patrick Bills

#Overview: Derives range based traits from both IUCN and BirdLife International. Here we calculate range size for all species and, in a addition, calculate mean annual temperature over range, mean annual precipitation over range, and average human footprint index. Both temperature and precipitation data are based on the ERA Interim Reanalysis (CHELSA dataset; https://chelsa-climate.org/). The human footprint index was sources from the Wildlife Conservation Society (https://wcshumanfootprint.org/)

#Data Input: Frugivoria_bird_database_2023.csv, Frugivoria_mammal_database_2023.csv

#Data Output: Frugivoria_bird_database_2023.csv, Frugivoria_mammal_database_2023.csv with added range based traits.

#Date: 1/31/23

#Load libraries
library(sf)
library(raster)

#Read in all range maps from IUCN (2022)
mam_shp <- st_read("C:/Users/bgers/Desktop/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")

#Read in  Frugivoria databases
bird <- read.csv("G:/Shared drives/SpaCE_Lab_neotropical_frugivores/Manuscripts/Database_Manuscript/Database_paper/EDI_resubmission_2023/databases_2023/Frugivoria_bird_database_2023.csv")

#read in bird database
mam <-read.csv("G:/Shared drives/SpaCE_Lab_neotropical_frugivores/Manuscripts/Database_Manuscript/Database_paper/EDI_resubmission_2023/databases_2023/Frugivoria_mammal_database_2023_complete.csv")


#subset mammal and bird databases so that we have a list of their scientific names
scientific_name_m<- mam %>%
  select(IUCN_species_name)

scientific_name_b<- bird %>%
  select(IUCN_species_name)

## MAMMALS
# Rename sci_name column to match 
colnames(mam_shp)[2] <- "IUCN_species_name"


# Filter the mammal shapefile so that only species in the Frugivoria database remain
frug_mam <- mam_shp %>% filter(IUCN_species_name %in% scientific_name_m$IUCN_species_name)

# Remove all shapefiles that have a presence code above 3 (this removes parts of the range where the species is extinct or likely extinct)
frug_mam_rm <- frug_mam[!frug_mam$presence >3,]

# Remove Ursus americanus because it's causing issues
#frug_mam_rm_1 <-frug_mam_rm %>%  filter(!IUCN_species_name=='Ursus americanus')

# Separates the multiple polygons per species into indivual polygons, which will help calculations down the line
all_mam_polygon <-st_cast(frug_mam_rm, "MULTIPOLYGON") %>% st_cast("POLYGON")

# Turn into SpatialPolygon for further calculations
frug_mam_spat <- as(all_mam_polygon, 'Spatial')

# Load Bio1 (mean annual temp) and Bio12 (mean annual preciptation) (Bioclim variables from CHELSA dataset)
mean_temp_range <- raster("C:/Users/bgers/Desktop/frugivoria_range/CHELSA_bio1_1981-2010_V.2.1.tif")
mean_precip_range <-  raster("C:/Users/bgers/Desktop/frugivoria_range/CHELSA_bio12_1981-2010_V.2.1.tif")

#Load human footprint data from WCS (already resampled to match CHELSA; see commented out code below to run)
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

# Extract values of each variable over the range. Here we are summing all the values within the species range and counting the number of cells in each range for later calculations of mean. This helps overcome the issue where some species have multiple polygons. Will later add each individual polygon together
mean_variables <- exact_extract(env, frug_mam_spat, fun=c("sum","count"), append_cols="IUCN_species_name", coverage_area=T)

#Group all polygons by IUCN_species_name, which allows sums  to be calculated for each species; only want one value for each column)
variable_sums <- mean_variables %>% group_by(IUCN_species_name) %>% 
  summarise(mean_CHELSA_bio1_1981.2010_V.2.1=sum(sum.CHELSA_bio1_1981.2010_V.2.1),
            mean_CHELSA_bio12_1981.2010_V.2.1= sum(sum.CHELSA_bio12_1981.2010_V.2.1),
            mean_human_fp_range_2010_p= sum(sum.human_fp_range_2010_p),
            mean_human_fp_range_2020_p= sum(sum.human_fp_range_2020_p),
            count.CHELSA_bio1_1981.2010_V.2.1=sum(count.CHELSA_bio1_1981.2010_V.2.1),
            count.CHELSA_bio12_1981.2010_V.2.1=sum(count.CHELSA_bio1_1981.2010_V.2.1),
            count.human_fp_range_2010_p=sum(count.human_fp_range_2010_p),
            count.human_fp_range_2020_p=sum(count.human_fp_range_2010_p)) %>% as.data.frame()

# Takes two climate variables and divides them by the number of cells in the range (# cells are variable specific due to NAs)
clim_means <-variable_sums %>%
  mutate_at(
    c("mean_CHELSA_bio1_1981.2010_V.2.1", "mean_CHELSA_bio12_1981.2010_V.2.1"),
    funs(. / count.CHELSA_bio1_1981.2010_V.2.1)
  )

#remove unnecessary columns
clim_means <- clim_means[,c("IUCN_species_name","mean_CHELSA_bio1_1981.2010_V.2.1","mean_CHELSA_bio12_1981.2010_V.2.1")]

# Takes two human footprint variables and divides them by the number of cells in the range (# cells are variable specific due to NAs)
human_footprint_means <-variable_sums %>%
  mutate_at(
    c("mean_human_fp_range_2010_p", "mean_human_fp_range_2020_p"),
    funs(. / count.human_fp_range_2010_p)
  )

#remove unnecessary columns
human_footprint_means <- human_footprint_means[,c("IUCN_species_name","mean_human_fp_range_2010_p", "mean_human_fp_range_2020_p")]

#merge climate and footprint data together
env_calculation_means <- merge(clim_means, human_footprint_means, by="IUCN_species_name")

# Write to file so do not have to run again (takes a while)
setwd("C:/Users/bgers/Desktop/frugivoria_range")
write.csv(env_calculation_means, file="mean_env_mammal.csv")

# Percent change in human impact since 2010
env_calculation_means$percent_change_hf_2010_2020 <- apply(env_calculation_means[,c('mean_human_fp_range_2020_p', 'mean_human_fp_range_2010_p')], 1, function(x) { (x[1]-x[2])/x[2] * 100 } )
write.csv(env_calculation_means, file="mean_env_mammal.csv")

##Calculating range sizes
#known and inferred presence
library(sf)

#calculate inferred range size
all_mam_polygon$inferred_range_sqkm <- st_area(st_transform(all_mam_polygon, 4326))/(1000*1000) #Take care of units

#group by species name and add up range sizes for each species
all_mam_inferred_range <-all_mam_polygon %>% group_by(IUCN_species_name) %>% 
  summarise(inferred_range_sqkm=sum(inferred_range_sqkm)) %>% as.data.frame()

# Known presence
# Remove all shapefiles that have a presence code above 1 (this removes parts of the range where the species is inferred to be)
frug_mam_pres_only <- frug_mam[!frug_mam$presence >1,]
# Remove Ursus americanus because it's causing issues
#frug_mam_pres_only_1 <-frug_mam_pres_only %>%  filter(!IUCN_species_name=='Ursus americanus')

# Joins the separates multipolygons into polygons, which will help calculations down the line
all_mam_polygon_po <-st_cast(frug_mam_pres_only_1, "MULTIPOLYGON") %>% st_cast("POLYGON")

#Calculate range size of observed range
all_mam_polygon_po$observed_range_sqkm <- st_area(st_transform(all_mam_polygon_po, 4326))/(1000*1000) #Take care of units)

#Group by species and add up range sizes
all_mam_observed_range <-all_mam_polygon_po %>% group_by(IUCN_species_name) %>% 
  summarise(observed_range_sqkm=sum(observed_range_sqkm)) %>% as.data.frame()

#Merge range data and remove irrelevant columns
final_range_data <- merge(all_mam_observed_range,all_mam_inferred_range, by="IUCN_species_name")
final_range_data$geometry.x <- NULL
final_range_data$geometry.y<- NULL

#create final dataframe will all spatial calculations
final_spatial_calcs <- merge(env_calculation_means, final_range_data, by="IUCN_species_name")
write.csv(final_spatial_calcs, file="final_spatial_calculations_mammals.csv")

#how many species missing spatial data
missing_mam <- mam %>% filter(!IUCN_species_name %in% final_spatial_calcs$IUCN_species_name)
missing_shp <-as.data.frame(mam_shp)
Amphinectomys savamis