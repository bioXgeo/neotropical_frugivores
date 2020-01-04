#Pulling mexican species information
### TOKEN FOR ACCESSING IUCN API ###########################################
### This is only authorized to be used by people from MSU ##################
token <- '3b3db1c753a7cad0616e16146363ec88eead97d185cb3304070d7343123516fd'
############################################################################

library(rredlist)

#Pull all species from Mexico
mexico_species <- rl_sp_country("MX", key = token, parse = TRUE)
mexico_species_df <- as.data.frame(mexico_species)

#mammals
#Can do the same thing as for birds for now and use the subset the Mexican list by the IUCN mammal shapefile. Not all species have shapefile so this won't be a comprehensive list. For what we need this for it will work.

all_mammals <-as.data.fnrame(rl_comp_groups(group = c('mammals'), key = token))


# Merge mexican species and all_mammals to get a subset of all mammals in Mexico
mx_mammals <- merge(all_mammals, mexico_species, by="result.taxonid")

#birds
# This is not working over the IUCN API. For now, will have to subset the Mexican list by the list obtained from the IUCN bird shapefile. 
#Read in fixed IUCN bird table. This file was created in the IUCN_Shapefile R script. It is the BOTW shapefile subsetted to species occurring in Central and South America. 
IUCN_bird_CSA<-read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Data/BOTW/BOTW_shapefile/IUCN_birds_attributes_CSA_pre_frug_1.csv")

#Change column for species name
colnames(IUCN_bird_CSA)[which(names(IUCN_bird_CSA) == "SCINAME")] <- "scientific_name"

# fix column names so the spatial data and mexican species list match 
colnames(mexico_species_df)[which(names(mexico_species_df) == "result.scientific_name")] <- "scientific_name"
colnames(mexico_species_df)[which(names(mexico_species_df) == "result.taxonid")] <- "taxonid"
colnames(IUCN_bird_CSA)[which(names(IUCN_bird_CSA) == "SISID")] <- "taxonid"

#Merge species list for mexico and bird list with spatial data (will be missing species without spatial data)
mx_birds <- merge(IUCN_bird_CSA, mexico_species_df, by="taxonid")

#mexican bird and mammal subsets (all mammals and only birds with SPATIAL DATA for now)
mx_birds
mx_mammals

# Merge with elton traits by species name and then use the lookup table to correct mismatches. 
# read in elton traits data
birds <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/BirdFuncDat.csv")
mamm<- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/MamFuncDat.csv")
colnames(mamm)[which(names(mamm) == "Scientific")] <- "scientific_name"
colnames(mx_mammals)[which(names(mx_mammals) == "result.scientific_name.x")] <- "scientific_name"

#mammal/elton trait merge
mx_mamm_trait <- merge(mamm, mx_mammals, by="scientific_name", all.y=TRUE)

#birds
colnames(birds)[which(names(birds) == "Scientific")] <- "scientific_name"
colnames(mx_birds)[which(names(mx_birds) == "scientific_name.x")] <- "scientific_name"

#bird/elton trait merge
mx_bird_trait <- merge(birds, mx_birds, by="scientific_name", all.y=TRUE)
mx_bird_trait_distinct <- distinct(mx_bird_trait,scientific_name, .keep_all= TRUE )

#_______________________________________________________________________
# Correcting issues with species not merging correctly between databases
#find species that did not match correctly
mamm_trait_na <-mx_mamm_trait[is.na(mx_mamm_trait$Diet.Vunk),] #74 IUCN mammal species didn't merge

#Find species that match alternate names. Lookup table for species is found in code 'scientific_name_lookup_table'
alternate_match <-as.data.frame(mamm_trait_na$scientific_name[mamm_trait_na$scientific_name %in% mam_lookup_final$IUCN_species_name])
colnames(alternate_match)[which(names(alternate_match) == "mamm_trait_na$scientific_name[mamm_trait_na$scientific_name %in% mam_lookup_final$IUCN_species_name]")] <- "IUCN_name_with_alternate"

#choose row in lookup table with the alternate match and get the IUCN name
mamm_lookup_subset <-mam_lookup_final %>%
  filter(IUCN_species_name %in% alternate_match$IUCN_name_with_alternate)

#add column to the first merge so that we can keep track of IUCN names vs. alternate names 
mx_mamm_trait$IUCN_name <- ""

#Merge list of alternate names with elton_traits and append them to the orginal merge 
mx_mamm_trait_alternates <- merge.data.frame(mamm, mamm_lookup_subset, by.x=c("scientific_name"), by.y=c("elton_name"))

## Remove duplicates with NAs

#transform the alternate names so I can subset the merged dataset and remove those names (will re-attach edited names)
alternates_matrix <-rbind(as.matrix(mx_mamm_trait_alternates$scientific_name), as.matrix(mx_mamm_trait_alternates$IUCN_species_name))

alternates_df <- as.data.frame(alternates_matrix)

#Remove the duplicate species names (wrong names and correct names) because we'll be merging them back in

mam_trait_alt_subset <-mx_mamm_trait %>%
  filter(!scientific_name %in% alternates_df$V1)

# Add IUCN information back into the subset
mx_mamm_trait_alternates_IUCN_info <- merge(mx_mamm_trait_alternates, mx_mammals, by.x="IUCN_species_name",by.y="scientific_name")

colnames(mam_trait_alt_subset)[which(names(mam_trait_alt_subset) == "IUCN_name")] <- "IUCN_species_name"
#Bind the alternate name object to the orginal edited merge
mam_trait_all <-rbind(mam_trait_alt_subset, mx_mamm_trait_alternates_IUCN_info)
#_________________________________________________________________________________________

#Find species that still haven't merged correctly and fix by hand

mamm_trait_na_manual <-mam_trait_all[is.na(mam_trait_all$Diet.Vunk),] # 26 species still have NAs
write.csv(mamm_trait_na_manual,"mam_trait_manual_lookups.csv")

#read in lookup table for species with synonyms manually looked up
mx_mamm_trait_alt_2 <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/lookup_table/mammal_lookup_2.csv")

#Remove species missing matches from the full trait dataset so we can remerge later
mam_trait_alt_subset_2 <-mam_trait_all %>%
  filter(!scientific_name %in% mx_mamm_trait_alt_2$IUCN_name)

#Merge list of alternate names with elton_traits 
mx_mamm_trait_alternates_2 <- merge.data.frame(mamm, mx_mamm_trait_alt_2, by.x=c("scientific_name"), by.y=c("elton_name"), all.y = TRUE)

#Bind the alternate name object to the orginal edited merge
colnames(mam_trait_alt_subset_2)[which(names(mam_trait_alt_subset_2) == "IUCN_name")] <- "IUCN_species_name"

#remaining species with IUCN information
mx_mamm_trait_alternates_IUCN_info_2 <- merge(mx_mamm_trait_alternates_2, mx_mammals, by.x="IUCN_name",by.y="scientific_name")

#remove unwanted columns that came from the lookup table 2
mx_mamm_trait_alternates_IUCN_info_2$X <- NULL 
mx_mamm_trait_alternates_IUCN_info_2$level <- NULL 

#change column name to match the subsetted mammal trait df
colnames(mx_mamm_trait_alternates_IUCN_info_2)[which(names(mx_mamm_trait_alternates_IUCN_info_2) == "IUCN_name")] <- "IUCN_species_name"

#bind subsetted mammal trait df with df of missing species names/IUCN info
mam_trait_all_final <-rbind(mam_trait_alt_subset_2, mx_mamm_trait_alternates_IUCN_info_2)

#Remove duplicates (due to using genus level data which led to repeats)
mam_trait_all_final_distinct <- distinct(mam_trait_all_final,scientific_name, .keep_all= TRUE)

#Subset by frugivorous species
mx_mam_frug <- mam_trait_all_final_distinct[mam_trait_all_final_distinct$Diet.Fruit>=10,] #191 mammal frugivores
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/mx_species")
write.csv(mx_mam_frug, "mx_mam_frug.csv")
#_________________________________________________________________________________________
# Correcting issues with species not merging correctly between databases
#find species that did not match correctly
bird_trait_na <-mx_bird_trait_distinct[is.na(mx_bird_trait_distinct$Diet.Vunk),] #201 IUCN bird species didn't merge

#Find species that match alternate names. Lookup table for species is found in code 'scientific_name_lookup_table'
alternate_match_b <-as.data.frame(bird_trait_na$scientific_name.y[bird_trait_na$scientific_name.y %in% bird_lookup_final$IUCN_species_name]) 
colnames(alternate_match_b)[which(names(alternate_match_b) == "bird_trait_na$scientific_name.y[bird_trait_na$scientific_name.y %in% bird_lookup_final$IUCN_species_name]")] <- "IUCN_name_with_alternate"

#choose row in lookup table with the alternate match and get the IUCN name
bird_lookup_subset <-bird_lookup_final %>% #27 species found in lookup table
  filter(IUCN_species_name %in% alternate_match_b$IUCN_name_with_alternate)

#add column to the first merge so that we can keep track of IUCN names vs. alternate names 
mx_bird_trait$IUCN_name <- ""

#Merge list of alternate names with elton_traits and append them to the orginal merge 
mx_bird_trait_alternates <- merge.data.frame(birds, bird_lookup_subset, by.x=c("scientific_name"), by.y=c("elton_name"))

## Remove duplicates with NAs

bird_trait_alt_subset <-mx_bird_trait_distinct %>%
  filter(!scientific_name.y %in% bird_lookup_subset$IUCN_species_name)

colnames(bird_trait_alt_subset)[which(names(bird_trait_alt_subset) == "IUCN_name")] <- "IUCN_species_name"

# Add IUCN information back into the subset
mx_bird_trait_alternates_IUCN_info <- merge(mx_bird_trait_alternates, mx_birds, by.x="IUCN_species_name",by.y="scientific_name")

#Bind the alternate name object to the orginal edited merge
bird_trait_all <-rbind(bird_trait_alt_subset, mx_bird_trait_alternates_IUCN_info)
#_________________________________________________________________________________________

#Find species that still haven't merged correctly and fix by hand

bird_trait_na_manual <-bird_trait_all[is.na(bird_trait_all$Diet.Vunk),] # 174 species still have NAs
write.csv(bird_trait_na_manual,"bird_trait_manual_lookups.csv")

#read in lookup table for species with synonyms manually looked up
mx_bird_trait_alt_2 <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/lookup_table/bird_lookup_table_2.csv")

#Remove species missing matches from the full trait dataset so we can remerge later
bird_trait_alt_subset_2 <-bird_trait_all %>%
  filter(!scientific_name %in% mx_bird_trait_alt_2$IUCN_species_name)

#Merge list of alternate names with elton_traits. Two species didn't merge, but they are insectivores.
mx_bird_trait_alternates_2 <- merge.data.frame(birds, mx_bird_trait_alt_2, by.x=c("scientific_name"), by.y=c("elton_name"))

#Bind the alternate name object to the orginal edited merge
colnames(bird_trait_alt_subset_2)[which(names(bird_trait_alt_subset_2) == "IUCN_name")] <- "IUCN_species_name"

#remaining species with IUCN information
mx_bird_trait_alternates_IUCN_info_2 <- merge(mx_bird_trait_alternates_2, mx_birds, by.x="IUCN_species_name",by.y="scientific_name")

#remove unwanted columns that came from the lookup table 2
mx_bird_trait_alternates_IUCN_info_2$X <- NULL 
mx_bird_trait_alternates_IUCN_info_2$level <- NULL 

#change column name to match the subsetted bird trait df
colnames(mx_bird_trait_alternates_IUCN_info_2)[which(names(mx_bird_trait_alternates_IUCN_info_2) == "IUCN_name")] <- "IUCN_species_name"

#remove columns that don't match so we can merge
setdiff(bird_trait_alt_subset_2, mx_bird_trait_alternates_IUCN_info_2)
mx_bird_trait_alternates_IUCN_info_2$X.1.x <- NULL 
mx_bird_trait_alternates_IUCN_info_2$X.1.y <- NULL 
mx_bird_trait_alternates_IUCN_info_2$X.x <- NULL 
mx_bird_trait_alternates_IUCN_info_2$X.y <- NULL 
mx_bird_trait_alternates_IUCN_info_2$X <- NULL 
bird_trait_alt_subset_2$X.1 <- NULL
bird_trait_alt_subset_2$X <- NULL

#bind subsetted mammal trait df with df of missing species names/IUCN info
bird_trait_all_final <-rbind(bird_trait_alt_subset_2, mx_bird_trait_alternates_IUCN_info_2)

#Remove duplicates (due to using genus level data which led to repeats)
bird_trait_all_final_distinct <- distinct(bird_trait_all_final,scientific_name, .keep_all= TRUE)

#Subset by frugivorous species
mx_bird_frug <- bird_trait_all_final_distinct[bird_trait_all_final_distinct$Diet.Fruit>=10,] #306 species

# Write csv to a file 
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/mx_species")
write.csv(mx_bird_frug, "mx_bird_frug.csv")
#________________________________________________________________________________________________
##Pull shapefiles for frugivorous species
library(sf)
library(rgeos)
library(rgdal)

##Mammals

setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Data/TERRESTRIAL_MAMMALS/TERRESTRIAL_MAMMALS_1")
IUCN_mam <-read_sf("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Data/TERRESTRIAL_MAMMALS_2/TERRESTRIAL_MAMMALS.shp")

# #Create new column in the shapefile so that each shapefile has a unique identifier 
IUCN_mam$new_id = 1:nrow(IUCN_mam)

# list of unique species names for frugivorous mammals in Mexico made above

mam_frug_sn <-as.vector(unique(mx_mam_frug$result.scientific_name.y))

# Subset the new IDs made above by those that have species names matching the frugivorous subset
shapes  = IUCN_mam$new_id[IUCN_mam$binomial %in% mam_frug_sn] # shapes = ID numbers for subset

# Subsets the IUCN shapefiles to those with IDs matching the frugivorous subset
IUCN_mam_df = IUCN_mam[IUCN_mam$new_id %in% shapes,] #188 unique mammal species (three species do not have shapefiles)

#write the shapefile to a file
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/mx_species")
st_write(IUCN_mam_df, "mx_mammals.shp")

##Birds

setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Data/BOTW/BOTW_shapefile")

# Pull in bird dataset
IUCN_bird <-read_sf("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Data/BOTW/BOTW_shapefile/All_Species.shp")

IUCN_bird$new_id = 1:nrow(IUCN_bird)

# list of unique species names for frugivorous mammals in central and south america
bird_frug_sn <-as.vector(unique(mx_bird_frug$scientific_name.y))

# Subset the new IDs made above by those that have species names matching the frugivorous subset
shapes_1 = IUCN_bird$new_id[IUCN_bird$SCINAME %in% bird_frug_sn] # shapes = ID numbers for subset

# Subsets the IUCN shapefiles to those with IDs matching the frugivorous subset
IUCN_bird_df = IUCN_bird[IUCN_bird$new_id %in% shapes_1,] #

#write the shapefile to a file
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/mx_species")
st_write(IUCN_bird_df, "mx_birds.shp")

#________________________________________________________________________________________________
# Centroid analaysis - removing bird and mammal species where the centroid of their ranges are above the northern latitude of Mexico.
IUCN_mam_df_sp <-as(IUCN_mam_df, 'Spatial')
mam_centr <- gCentroid(IUCN_mam_df_sp, byid = TRUE, id=IUCN_mam_df_sp$new_id) #want this to be the IUCN ID number

plot(mam_centr)

#turn into a dataframe for easier subsetting by latitude
mam_centr_df <-as.data.frame(mam_centr)

#subset above a certain latitude, remove anything above Mexico (lat >=23.6345)
subset_mam_mx <- mam_centr_df[mam_centr_df$y<=23.6345,]

#Want to remove species (not just their polygons) that have ranges above that latitude.
#Returns a dataframe of species with ranges above Northern Mexico
subset_mam_removed <- mam_centr_df[!mam_centr_df$y<=23.6345,]

#Fix names so that the ID number is a column instead of a rowname
subset_mam_removed <-setNames(cbind(rownames(subset_mam_removed), subset_mam_removed, row.names = NULL), c("new_id", "x", "y"))

#Create object of species IDs that were removed
mam_id_rm <- subset_mam_removed$new_id

#Subset the full IUCN dataframe to what we want to remove so we can get the names of species we want to remove entirely
mam_sp_to_remove <-IUCN_mam_df[IUCN_mam_df$new_id %in% mam_id_rm,]

#Find distinct species names in this dataframe
mam_sp_to_remove_distinct <-distinct(mam_sp_to_remove,binomial, .keep_all= TRUE )

#Subset the full IUCN dataframe species without ranges above Mexico
mx_IUCN_mam_final <- IUCN_mam_df %>%
  filter(!binomial %in% mam_sp_to_remove_distinct$binomial)

#plot

mx_IUCN_mam_final <- mx_IUCN_mam_final[!mx_IUCN_mam_final$binomial=="Boselaphus tragocamelus",]

#Write this to a file
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/mx_species")
st_write(mx_IUCN_mam_final, "mx_mammals_final_rm.shp")

## Birds

IUCN_bird_df_sp <-as(IUCN_bird_df, 'Spatial')
bird_centr <- gCentroid(IUCN_bird_df_sp, byid = TRUE, id=IUCN_bird_df_sp$new_id) #want this to be the IUCN ID number

plot(bird_centr)

#turn into a dataframe for easier subsetting by latitude
bird_centr_df <-as.data.frame(bird_centr)


#subset above a certain latitude, remove anything above Mexico (lat >=23.6345)
subset_bird_mx <- bird_centr_df[bird_centr_df$y<=23.6345,]

#Want to remove species (not just their polygons) that have ranges above that latitude.
#Returns a dataframe of species with ranges above Northern Mexico
subset_bird_removed <- bird_centr_df[bird_centr_df$y>23.6345,]

#Fix names so that the ID number is a column instead of a rowname
subset_bird_removed <-setNames(cbind(rownames(subset_bird_removed), subset_bird_removed, row.names = NULL), c("new_id", "x", "y"))

#Create object of species IDs that were removed
bird_id_rm <- subset_bird_removed$new_id

#Subset the full IUCN dataframe to what we want to remove so we can get the names of species we want to remove entirely
bird_sp_to_remove <-IUCN_bird_df[IUCN_bird_df$new_id %in% bird_id_rm,]

#Find distinct species names in this dataframe
bird_sp_to_remove_distinct <-distinct(bird_sp_to_remove,SCINAME, .keep_all= TRUE )

#Subset the full IUCN dataframe species without ranges above Mexico
mx_IUCN_bird_final <- IUCN_bird_df %>%
  filter(!SCINAME %in% bird_sp_to_remove_distinct$SCINAME)

#Write this to a file
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/mx_species")
st_write(mx_IUCN_bird_final, "mx_birds_final_2.shp")

#Removed some extraneous points in QGIS for species Elanoides forficatus. For some reason this was not removed from the dataset with the centroid analysis. 

# Take final species subsets and dissolve with shapefile of central and South America. This will be where we pull GBIF records from.

#_________________________________________________________________________________________
#Dissolve
#mammals

library(dplyr)

#back to spatial
mx_IUCN_mam_final_sp <-as(mx_IUCN_mam_final, 'Spatial')

#Union of all shapefiles; this returns a unionized shapefile, but there are geographic issues (boxes). Compared with unioned shapefile made in QGIS.
mammal_union <- gUnaryUnion(mx_IUCN_mam_final_sp, id= mx_IUCN_mam_final_sp@data$kingdom)

mam_spatial_df<- data.frame(id = getSpPPolygonsIDSlots(mammal_union))
row.names(mam_spatial_df) <- getSpPPolygonsIDSlots(mammal_union)

# Make spatial polygon data frame
mammal_union_spatial_df <- SpatialPolygonsDataFrame(mammal_union, data =mam_spatial_df)
writeOGR(mammal_union_spatial_df,"mammal_union.shp",".",driver="ESRI Shapefile")

mammal_sf <- st_as_sf(mammal_union)
##Birds

#Union of all shapefiles; this returns a unionized shapefile, but there are geographic issues (boxes). Compared with unioned shapefile made in QGIS.

#upload QGIS edited file (removed strange points for a single species)
birds_edited <-readOGR("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/mx_species/mx_birds_final_2_edited.shp")
bird_buff <- gBuffer(birds_edited, byid=TRUE, width=0)
bird_union <- gUnaryUnion(bird_buff, id= bird_buff@data$kingdom)

bird_spatial_df<- data.frame(id = getSpPPolygonsIDSlots(bird_union))
row.names(bird_spatial_df) <- getSpPPolygonsIDSlots(bird_union)

# Make spatial polygon data frame
bird_union_spatial_df <- SpatialPolygonsDataFrame(bird_union, data =bird_spatial_df)
writeOGR(bird_union_spatial_df,"bird_union.shp",".",driver="ESRI Shapefile")

# Unionize both birds and mammals

#Gets rid of self intersection issue
bird_buff <- gBuffer(bird_union, byid=TRUE, width=0)
#Gets rid of self intersection issue
mam_buff <- gBuffer(mammal_union, byid=TRUE, width=0)

#Change back to SF 
bird_sf <-st_as_sf(bird_buff)
mammal_sf <-st_as_sf(mam_buff)

frugivore_union_mx <- st_union(bird_sf,mammal_sf)
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/mx_species")
st_write(frugivore_union_mx, "mx_frugivore_union.shp")

#Unionize with Central and South America
setwd("/Volumes/GoogleDrive/My Drive/neotropical_frugivores/Data/World_shapefile/World_Continents")
CS_america <- read_sf(".","CS_americas")
CSA_america_sp <-as(CS_america, 'Spatial')

#Gets rid of self intersection issue
CSA_buff <- gBuffer(CSA_america_sp, byid=TRUE, width=0)

#Unionize Central and SA by region (19)
CS_union <- gUnaryUnion(CSA_buff, id= CSA_buff@data$REGION)
CSA_sf <-st_as_sf(CS_union)

#Unionize frugivore shapefile and Central and SA shapefile to get full study area
mx_northern_lim <- st_union(frugivore_union_mx, CSA_sf)

#write to file
st_write(mx_northern_lim, "CSA_frugivore_union.shp")

#Think about birds that migrate
#_____________________________________________________________________________________________________

# Pull all species from Central and South America (have this as a cropped IUCN shapefile) that have a shapefile

# Subset out frugivores

# Remove species with ranges above mexico (this object already exists)

# Pull GBIF records for all of these species with an IUCN shapefile - This will likely need to be done on the HPC at least for birds. Ask for help later this week once I get to that point.







