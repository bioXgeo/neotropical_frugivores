library(taxize)
?taxon

### TOKEN FOR ACCESSING IUCN API ###########################################
### This is only authorized to be used by people from MSU ##################
token <- '3b3db1c753a7cad0616e16146363ec88eead97d185cb3304070d7343123516fd'
############################################################################


# Names of all threatened species in CO/EC ------------------------------

library(httr)
library(jsonlite)
library(dplyr)
library(rredlist)

api_url <- 'https://apiv3.iucnredlist.org/api/v3/'


#____________________________________________________________________________________________________________________________
# get all country codes
all_countries <- GET(url = paste0(api_url, 'country/list?token=',token))
all_countries_1 <- fromJSON(content(all_countries, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)$result

#____________________________________________________________________________________________________________________________

#extract species by country
#South America

#Colombia
colombia_species <- rl_sp_country("CO", key = token, parse = TRUE)
c1 <- colombia_species$result[1]
#Ecuador
ecuador_species <- rl_sp_country("EC", key = token, parse = TRUE)
c2 <- ecuador_species$result[1]
#Peru
peru_species <- rl_sp_country("PE", key = token, parse = TRUE)
c3 <- peru_species$result[1]
#Bolivia
bolivia_species <- rl_sp_country("BO", key = token, parse = TRUE)
c4 <-  bolivia_species$result[1]
#Argentina
argentina_species <- rl_sp_country("AR", key = token, parse = TRUE)
c5 <- argentina_species$result[1]
#Venezuela
venezuela_species <- rl_sp_country("VE", key = token, parse = TRUE)
c6 <- venezuela_species$result[1]

# Would add these if was doing all of Central and South America, but am actually doing buffered region around montane areas.

#ADD BRAZIL
#brazil_species <- rl_sp_country("BR", key = token, parse = TRUE)
#c7 <- brazil_species$result[1]

#ADD CHILE
Chile_species <- rl_sp_country("CL", key = token, parse = TRUE)
c7 <- Chile_species$result[1]
#ADD Guyana
#guyana_species <- rl_sp_country("GY", key = token, parse = TRUE)
#c9 <- guyana_species$result[1]

#ADD French Guiana
#french_species <- rl_sp_country("GF", key = token, parse = TRUE)
#c10 <- french_species$result[1]

#ADD Uruguay
#uraguay_species <- rl_sp_country("UY", key = token, parse = TRUE)
#c11 <- uraguay_species$result[1]
#____________________________________________________________________________________________________________________________
#Central America 

#Mexico
mexico_species <- rl_sp_country("MX", key = token, parse = TRUE)
c8 <- mexico_species$result[1]
#Guatemala
guatemala_species <- rl_sp_country("GT", key = token, parse = TRUE)
c9 <- guatemala_species$result[1]
#Costa Rica
costa_rica_species <- rl_sp_country("CR", key = token, parse = TRUE)
c10 <- costa_rica_species$result[1]
#Panama
panama_species <- rl_sp_country("PA", key = token, parse = TRUE)
c11 <- panama_species$result[1]
#El Salvador
el_salvador_species <-rl_sp_country("SV", key = token, parse = TRUE)
c12 <- el_salvador_species$result[1]
#Belize
belize_species <- rl_sp_country("BZ", key = token, parse = TRUE)
c13 <- belize_species$result[1]
#Nicaragua
nicaragua_species <- rl_sp_country("NI", key = token, parse = TRUE)
c14 <- nicaragua_species$result[1]

#Generate a list of all species in South america (near montane regions) and Central america to extract mammmals from
#There will be repeats within these objects because the same species may be present in multiple countries. We will remove these later
all_species_south_america <- rbind(c1, c2, c3, c4, c5, c6, c7)
all_species_central_america <- rbind(c8, c9, c10, c11, c12, c13, c14)

#just Mexico to test Northern Boundary
c8
#__________________________________________________________________________________________________________________________
# Break down by species group if possible 

# Get codes for the different species groups
grp_codes <- GET(url = paste0(api_url, 'comp-group/list?token=', token))
code <-fromJSON(content(grp_codes, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)$result


# Get species list of all birds across all countries. 
all_birds <-rl_comp_groups(group = c('birds'), key = token)

# Get species list of all mammals across all countries
all_mammals <-rl_comp_groups(group = c('mammals'), key = token)
#test <-rl_threats(name='Bassaricyon neblina', key=token)


#___________________________________________________________________________________________________________________________
# Mammals and birds occurring in central and south america
#South America

#Mammals

#turn the list into a dataframe so the merge can be completed
all_mammals <- as.data.frame(all_mammals$result)
SA_mammals <- merge(all_mammals, all_species_south_america, by="taxonid")
SA_mammals <- distinct(SA_mammals, scientific_name, .keep_all = TRUE)


#Birds
all_birds<- as.data.frame(all_birds$result)
SA_birds <- merge(all_birds, all_species_south_america, by="taxonid")
SA_birds <- distinct(SA_birds, scientific_name, .keep_all = TRUE)

#Central America
#Mammals
CA_mammals <- merge(all_mammals, all_species_central_america, by="taxonid")
CA_mammals <- distinct(CA_mammals, scientific_name, .keep_all = TRUE)

#Birds
CA_birds <- merge(all_birds, all_species_central_america, by="taxonid")
CA_birds <- distinct(CA_birds, scientific_name, .keep_all = TRUE)

#All Latin American Mammals (removed duplicates due to species occurring in more than one country)
latin_american_mammals <- merge(SA_mammals, CA_mammals, all=TRUE)


#All latin American Birds
latin_american_birds <- merge(SA_birds, CA_birds, all=TRUE)
#___________________________________________________________________________________________________________________________________
# Habitat information

# Include error-checking code in the function so that it will return NA for everything if there's no data.
get_habitat <- function(ID) {
  habitat_by_sp<- rl_habitats(id=ID,key=token)$result
  if (class(habitat_by_sp) == 'data.frame') {
    data.frame(taxonid=ID, habitat_by_sp)
  } else {
    data.frame(taxonid=ID)
  }
}

# Apply get_habitat to each row of latin_american_mammals
all_habitat_mammals <- latin_american_mammals %>%
  rowwise %>%
  do(get_habitat(.$taxonid))

#write to a file so don't have to keep re-running (can cause API issues)
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/database_run_2020/habitat")
write.csv(all_habitat_mammals, 'all_habitat_mammals.csv')

# Apply get_habitat to each row of latin_american_birds
# This is a large dataset so break it up into chunks for shorter calls over the IUCN API. Each call will take ~30 min and this minimizes the chances of a crash. The common error is "Bad Gateway (error 502)". Emailed IUCN about this and this is the best solution.

bird_1 <- latin_american_birds[1:500,]
bird_2 <- latin_american_birds[501:1000,]
bird_3 <-latin_american_birds[1001:1500,]
bird_4 <- latin_american_birds[1501:2000,]
bird_5 <-latin_american_birds[2001:2500,]
bird_6 <- latin_american_birds[2501:2750,]
bird_7 <- latin_american_birds[2751:3000,]
bird_8 <- latin_american_birds[3001:3250,]
bird_9 <- latin_american_birds[3251:3400,]
bird_9.5 <- latin_american_birds[3401:3500,]
bird_10 <- latin_american_birds[3501:3750,]
bird_11 <- latin_american_birds[3750:3902,]

bird_hab_1 <- bird_1 %>%
  rowwise %>%
  do(get_habitat(.$taxonid))

bird_hab_2 <- bird_2 %>%
  rowwise %>%
  do(get_habitat(.$taxonid))

bird_hab_3 <- bird_3 %>%
  rowwise %>%
  do(get_habitat(.$taxonid))

bird_hab_4 <- bird_4 %>%
  rowwise %>%
  do(get_habitat(.$taxonid))

bird_hab_5 <- bird_5 %>%
  rowwise %>%
  do(get_habitat(.$taxonid))

bird_hab_6 <- bird_6 %>%
  rowwise %>%
  do(get_habitat(.$taxonid))

bird_hab_7 <- bird_7 %>%
  rowwise %>%
  do(get_habitat(.$taxonid))

bird_hab_8 <- bird_8 %>%
  rowwise %>%
  do(get_habitat(.$taxonid))

bird_hab_9 <- bird_9 %>%
  rowwise %>%
  do(get_habitat(.$taxonid))

bird_hab_9.5 <- bird_9.5 %>%
  rowwise %>%
  do(get_habitat(.$taxonid))

bird_hab_10 <- bird_10 %>%
  rowwise %>%
  do(get_habitat(.$taxonid))

bird_hab_11 <- bird_11 %>%
  rowwise %>%
  do(get_habitat(.$taxonid))

#Bind all of the bird habitat data together
all_habitat_birds <- rbind(bird_hab_1, bird_hab_2, bird_hab_3, bird_hab_4, bird_hab_5, bird_hab_6, bird_hab_7,bird_hab_8,bird_hab_9, bird_hab_9.5,bird_hab_10)

#write to a file so don't have to keep re-running
write.csv(all_habitat_birds, 'all_habitat_birds.csv')

#Subset the habitat information for all species by those that are montane tropical and lowland tropical
#subset by lowland
bird_habitat_lowland <- all_habitat_birds[all_habitat_birds$habitat =="Forest - Subtropical/Tropical Moist Lowland",]
mam_habitat_lowland <- all_habitat_mammals[all_habitat_mammals$habitat =="Forest - Subtropical/Tropical Moist Lowland",]

#subset by montane
bird_habitat_montane <- all_habitat_birds[all_habitat_birds$habitat =="Forest - Subtropical/Tropical Moist Montane",]
mam_habitat_montane <- all_habitat_mammals[all_habitat_mammals$habitat =="Forest - Subtropical/Tropical Moist Montane",]

#merge Latin American animal datasets with Central and South American species lists for montane and lowland species
#join lowland and montane together
#We can parse them back out later on

bird_habitat_join <- rbind(bird_habitat_lowland, bird_habitat_montane)

mammal_habitat_join <- rbind(mam_habitat_lowland, mam_habitat_montane)

#Merge the habitat data with the IUCN lists so that we have the statuses and habitats combined with species names (not just taxonid). Only keep species with habitat info.

mammal_IUCN<-merge(latin_american_mammals, mammal_habitat_join, by= "taxonid", all.y=TRUE)


bird_IUCN<- merge(latin_american_birds, bird_habitat_join, by= "taxonid", all.y=TRUE)
#_____________________________________________________________________________________________________________________________________
#Trait Information

## Now that we have habitat information for all mammals and birds in Central and South American countries of interest, we need to figure out which are frugivores. We do this by using the Elton Traits Database (insert citation here)

#Read in Elton Traits dataset 
birds <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/BirdFuncDat.csv")
mamm<- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/MamFuncDat.csv")

#Combine bird elton traits dataset (full) and IUCN bird dataset for Latin America together
colnames(birds)[which(names(birds) == "Scientific")] <- "scientific_name"
bird_trait_IUCN<- merge(bird_IUCN, birds, by= "scientific_name", all.x=TRUE)
tail(bird_trait_IUCN)

#Combine mammal elton traits dataset and IUCN bird data together
colnames(mamm)[which(names(mamm) == "Scientific")] <- "scientific_name"
mamm_trait_IUCN <- merge.data.frame(mammal_IUCN, mamm, by= "scientific_name", all.x=TRUE)

#setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Mammals_Birds_all_data/Trait_database_2019")
#write.csv(bird_trait_IUCN, 'bird_trait_IUCN.csv')
#write.csv(mamm_trait_IUCN, 'mamm_trait_IUCN.csv')

# Some of these rows had no elton traits data but will be filled in manually (NAs in these rows). This could be due to spelling errors
# or changes in taxonomic status.
# Will remove species that have lower than 10% frugivorous diet after resolving the issues with naming conventions (figuring out alternate names species with NA for Elton Traits). Interface with code for building Lookup_tables.
_____________________________________________________________________________________________________________________________________
# Resolving Taxonomies (lookup tables were created ) - GITHUB CODE
# Mammals

#Go through the merges that did not work correctly (kept all IUCN species that were not found in the Elton Traits database) and compare against synonym lists
# Correcting issues with species not merging correctly between databases
#find species that did not match correctly
mamm_trait_na <-mamm_trait_IUCN[is.na(mamm_trait_IUCN$Diet.Vunk),]  
mamm_trait_na <- mamm_trait_na[!duplicated(mamm_trait_na[,c('scientific_name')]),] #212 IUCN mammal species didn't merge correctly

#Read in the lookup table for mammals (this was created over the course of database building after many iterations)
#mam_lookup <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/lookup_table/full_mammal_lookup_9_20.csv")
#mam_lookup <- distinct(mam_lookup, IUCN_name, .keep_all = TRUE)

#Find species that match alternate names. Lookup table for species is found in code 'scientific_name_lookup_table'
#read in lookup table made previously
mam_lookup_original<- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/lookup_table/mammal_lookup.csv")
# Change column name in alternate_match
colnames(mam_lookup_original)[which(names(mam_lookup_original) == "IUCN_name")] <- "IUCN_species_name"

# Read in most recent lookup table
old_frug_species_list_lookup <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/lookup_table/CSA_mam_trait_lookups_final_1_7.csv")

#Remove erroneous columns from the old lookup table
old_frug_species_list_lookup <- old_frug_species_list_lookup[ -c(3,4,5,6)]

#bind both tables together
mam_lookup_final <- rbind(mam_lookup_original, old_frug_species_list_lookup)

#remove any species name repeats that occurred after the rbind
mam_lookup_final <- mam_lookup_final %>% distinct(IUCN_species_name, .keep_all = TRUE)

# Change column name in alternate_match
colnames(mam_lookup_final)[which(names(mam_lookup_final) == "IUCN_name")] <- "IUCN_species_name"

# Find species names present in the lookup table that didn't merge correctly 
alternate_match <-unique(as.data.frame(mamm_trait_na$scientific_name[mamm_trait_na$scientific_name %in% mam_lookup_final$IUCN_species_name])) #200 species names found 

# Change column name in alternate_match
colnames(alternate_match)[which(names(alternate_match) == "mamm_trait_na$scientific_name[mamm_trait_na$scientific_name %in% mam_lookup_final$IUCN_species_name]")] <- "IUCN_name_with_alternate"

#choose row in lookup table with the alternate match and get the IUCN name
mamm_lookup_subset <-mam_lookup_final %>%
  filter(IUCN_species_name %in% alternate_match$IUCN_name_with_alternate)

#add column to the first merge so that we can keep track of IUCN names vs. alternate names 
mamm_trait_IUCN$IUCN_species_name <- ""

#there are potentially some white spaces trailing some species name in the the Elton_name column
mamm_lookup_subset$elton_name <- trimws(mamm_lookup_subset$elton_name, which = c("right"))

#Merge list of alternate names with elton_traits to append them to the original merge 
mamm_trait_alternates <- merge(mamm, mamm_lookup_subset, by.x=c("scientific_name"), by.y=c("elton_name"))

## Remove duplicates with NAs

#transform the alternate names so I can subset the merged dataset and remove those names (will re-attach edited names).
#Need to remove the scientific_names (which are the IUCN_name) in the original dataset that have NA's for the elton traits. This way the alternate names can be merged back into the scientific_names. 
mam_alternates_matrix <- as.matrix(mamm_trait_alternates$IUCN_species_name)

alternates_df <- as.data.frame(mam_alternates_matrix)

#Remove the duplicate species names (wrong names and correct names) because we'll be merging them back in. We want the correct  and 

mam_trait_alt_subset <-mamm_trait_IUCN %>%
  filter(!scientific_name %in% alternates_df$V1)

#Merge IUCN information back into the mamm_trait_alternates object
mamm_trait_alternates_IUCN_info <- merge(mamm_trait_alternates, mammal_IUCN, by.x="IUCN_species_name",by.y="scientific_name")

mam_trait_alt_subset <- mam_trait_alt_subset[ -c(38,39,40,41)]

#Bind the alternate name object to the orginal edited merge. The scientific name column is the elton_trait names.
mam_trait_all <-rbind(mamm_trait_alternates_IUCN_info, mam_trait_alt_subset)

#Must now lookup species names that were not in the look_up table. This may be because of new additions to the IUCN database.
#____________________________________________________________________________________________________________________
## Manual Lookups
# Mammals

#Find species that still haven't merged correctly and fix by hand
mamm_trait_na_manual <-mam_trait_all[is.na(mam_trait_all$Diet.Vunk),] 
mamm_trait_na_manual <- mamm_trait_na_manual$scientific_name 

#Remove NAs and repeats
mamm_trait_na_manual <-unique(mamm_trait_na_manual[!is.na(mamm_trait_na_manual)]) #40 non-matching species (many of which are subspecies)

setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/database_run_2020")
write.csv(mamm_trait_na_manual,"mam_trait_manual_lookups.csv")

# manually looked up remaining species names. Pull in the edited file.
mam_trait_alt_2 <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/database_run_2020/mam_trait_manual_lookups.csv")

#Remove species missing matches from the full trait dataset so we can remerge later
mam_trait_alt_subset_2 <-mam_trait_all %>%
  filter(!scientific_name %in% mam_trait_alt_2$IUCN_name)

#Merge list of alternate names with elton_traits. Two species didn't merge, but they are insectivores.
mam_trait_alternates_2 <- merge.data.frame(mamm, mam_trait_alt_2, by.x=c("scientific_name"), by.y=c("elton_name"))

#Bind the alternate name object to the orginal edited merge
colnames(mam_trait_alt_subset_2)[which(names(mam_trait_alt_subset_2) == "IUCN_name")] <- "IUCN_species_name"

#remaining species with IUCN information
mam_trait_alternates_IUCN_info_2 <- merge(mam_trait_alternates_2, mammal_IUCN, by.x="IUCN_name",by.y="scientific_name")
mam_trait_alternates_IUCN_info_2$X <- NULL
#change column name to match the subsetted bird trait df
colnames(mam_trait_alternates_IUCN_info_2)[which(names(mam_trait_alternates_IUCN_info_2) == "IUCN_name")] <- "IUCN_species_name"

#bind subsetted mammal trait df with df of missing species names/IUCN info
mam_trait_all_final <-rbind(mam_trait_alt_subset_2, mam_trait_alternates_IUCN_info_2)

#Remove duplicates (due to using genus level data which led to repeats)
#mam_trait_all_final_distinct <- distinct(mam_trait_all_final,scientific_name, .keep_all= TRUE)

#Subset by frugivorous species
mam_frug <- mam_trait_all_final[mam_trait_all_final$Diet.Fruit>=10,] #827 species

#fill in missing values in the IUCN_species_name_final column with those matching the elton traits name
#Gives full list of IUCN species names for dataset
mam_frug_final <-with(mam_frug, ifelse(IUCN_species_name=="", as.character(scientific_name), IUCN_species_name))
mam_frug$IUCN_species_name <- mam_frug_final

#Remove NAs
mam_frug <- mam_frug[!is.na(mam_frug$IUCN_species_name),]

#Parse out montane and lowland mammal species for use as individual databases
#montane
setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/database_run_2020")
mam_frug_montane <- mam_frug[mam_frug$habitat=="Forest - Subtropical/Tropical Moist Montane",]
write.csv(mam_frug_montane, "mammal_montane_database_2020.csv")

#lowland
mam_frug_lowland <- mam_frug[mam_frug$habitat=="Forest - Subtropical/Tropical Moist Lowland",]
write.csv(mam_frug_lowland, "mammal_lowland_database_2020.csv")

#write the final lookup table for mammals
mam_trait_alt_2$X <- NULL
colnames(mam_trait_alt_2)[which(names(mam_trait_alt_2) == "IUCN_name")] <- "IUCN_species_name"
mammal_lookup_2020 <- rbind(mam_lookup_final,mam_trait_alt_2)
write.csv(mammal_lookup_2020,"mammal_lookups_final_2020.csv")

#____________________________________________________________________________________________________________________
Resolving Taxonomies (lookup tables were created ) - GITHUB CODE
# Birds

#Go through the merges that did not work correctly (kept all IUCN species that were not found in the Elton Traits database) and compare against synonym lists
# Correcting issues with species not merging correctly between databases
#find species that did not match correctly
bird_trait_na <-bird_trait_IUCN[is.na(bird_trait_IUCN$Diet.Vunk),]  
bird_trait_na <- bird_trait_na[!duplicated(bird_trait_na[,c('scientific_name')]),] #464 IUCN bird species didn't merge correctly

#Find species that match alternate names. Lookup table for species is found in code 'scientific_name_lookup_table'
bird_lookup_original <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/lookup_table/complete_bird_lookup_7_15.csv")
bird_lookup_2 <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/lookup_table/bird_lookup_table_2.csv")
bird_lookup_original$X <- NULL
bird_lookup_2$X <- NULL
bird_lookup_2$X.1 <- NULL
bird_lookup_final <- rbind(bird_lookup_original, bird_lookup_2)

#write the lookup table to a file for future use
write.csv(bird_lookup_final,"bird_lookups_final_2020.csv")

# Remove any unwanted spaces
bird_lookup_final$elton_name <- trimws(bird_lookup_final$elton_name, which = c("right"))

# Find species names present in the lookup table that didn't merge correctly 
alternate_match_b <-unique(as.data.frame(bird_trait_na$scientific_name[bird_trait_na$scientific_name %in% bird_lookup_final$IUCN_species_name])) # 414 species names found 

# Change column name in alternate_match
colnames(alternate_match_b)[which(names(alternate_match_b) == "bird_trait_na$scientific_name[bird_trait_na$scientific_name %in% bird_lookup_final$IUCN_species_name]")] <- "IUCN_name_with_alternate"

#choose row in lookup table with the alternate match and get the IUCN name
bird_lookup_subset <-bird_lookup_final %>%
  filter(IUCN_species_name %in% alternate_match_b$IUCN_name_with_alternate)

#add column to the first merge so that we can keep track of IUCN names vs. alternate names 
bird_trait_IUCN$IUCN_species_name <- ""

#there are potentially some white spaces trailing some species name in the the Elton_name column
bird_lookup_subset$elton_name <- trimws(bird_lookup_subset$elton_name, which = c("right"))

#Merge list of alternate names with elton_traits to append them to the original merge 
bird_trait_alternates <- merge(birds, bird_lookup_subset, by.x=c("scientific_name"), by.y=c("elton_name"))

## Remove duplicates with NAs

#transform the alternate names so I can subset the merged dataset and remove those names (will re-attach edited names)
bird_alternates_matrix <- as.matrix(bird_trait_alternates$IUCN_species_name)

alternates_df <- as.data.frame(bird_alternates_matrix)

#Remove the duplicate species names (wrong names and correct names) because we'll be merging them back in. We want the correct  and 

bird_trait_alt_subset <-bird_trait_IUCN %>%
  filter(!scientific_name %in% alternates_df$V1)

#Merge IUCN information back into the mamm_trait_alternates object
bird_trait_alternates_IUCN_info <- merge(bird_trait_alternates, bird_IUCN, by.x="IUCN_species_name",by.y="scientific_name")

#Bind the alternate name object to the orginal edited merge. The scientific name column is the elton_trait names.
bird_trait_all <-rbind(bird_trait_alternates_IUCN_info, bird_trait_alt_subset)

#Must now lookup species names that were not in the look_up table. This may be because of new additions to the IUCN database.
#____________________________________________________________________________________________________________________
## Manual Lookups
# bird

#Find species that still haven't merged correctly and fix by hand
bird_trait_na_manual <-bird_trait_all[is.na(bird_trait_all$Diet.Vunk),] 
bird_trait_na_manual <- bird_trait_na_manual$scientific_name 

#Remove NAs and repeats
bird_trait_na_manual <-unique(bird_trait_na_manual[!is.na(bird_trait_na_manual)]) #53 non-matching species 

setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/database_run_2020")
write.csv(bird_trait_na_manual,"bird_trait_manual_lookups.csv")

# manually looked up remaining species names. Pull in the edited file.
bird_trait_alt_2 <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/database_run_2020/bird_trait_manual_lookups.csv")

#Remove species missing matches from the full trait dataset so we can remerge later
bird_trait_alt_subset_2 <-bird_trait_all %>%
  filter(!scientific_name %in% bird_trait_alt_2$IUCN_name)

#Merge list of alternate names with elton_traits. Two species didn't merge, but they are insectivores.
bird_trait_alternates_2 <- merge.data.frame(birds, bird_trait_alt_2, by.x=c("scientific_name"), by.y=c("elton_name"))

#Bind the alternate name object to the orginal edited merge
colnames(bird_trait_alt_subset_2)[which(names(bird_trait_alt_subset_2) == "IUCN_name")] <- "IUCN_species_name"

#remaining species with IUCN information
bird_trait_alternates_IUCN_info_2 <- merge(bird_trait_alternates_2, bird_IUCN, by.x="IUCN_name",by.y="scientific_name")
#change column name to match the subsetted bird trait df
colnames(bird_trait_alternates_IUCN_info_2)[which(names(bird_trait_alternates_IUCN_info_2) == "IUCN_name")] <- "IUCN_species_name"

#bind subsetted mammal trait df with df of missing species names/IUCN info
bird_trait_all_final <-rbind(bird_trait_alt_subset_2, bird_trait_alternates_IUCN_info_2)

#Subset by frugivorous species
bird_frug <- bird_trait_all_final[bird_trait_all_final$Diet.Fruit>=10,] # 1605 species

#fill in missing values in the IUCN_species_name_final column with those matching the elton traits name
#Gives full list of IUCN species names for dataset
bird_frug_final <-with(bird_frug, ifelse(IUCN_species_name=="", as.character(scientific_name), IUCN_species_name))
bird_frug$IUCN_species_name <- bird_frug_final

#Parse out montane and lowland mammal species for use as individual databases
#montane
bird_frug_montane <- bird_frug[bird_frug$habitat=="Forest - Subtropical/Tropical Moist Montane",]
write.csv(bird_frug_montane, "bird_montane_database_2020.csv")

#lowland
bird_frug_lowland <- bird_frug[bird_frug$habitat=="Forest - Subtropical/Tropical Moist Lowland",]
write.csv(bird_frug_lowland, "bird_lowland_database_2020.csv")

#write the final lookup table for mammals
colnames(bird_trait_alt_2)[which(names(bird_trait_alt_2) == "IUCN_name")] <- "IUCN_species_name"
bird_lookup_2020 <- rbind(bird_lookup_final,bird_trait_alt_2)
write.csv(bird_lookup_2020,"bird_lookups_final_2020.csv")

