#Title: IUCN species list subset

#Project: Frugivoria

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske, Patrick Bills

#Overview: This script obtains the species list of birds and mammals for relevant countries from the IUCN and subsets by species habitat.

#Data output: IUCN species lists for countries of interest in tropical/subtroptical moist montane and tropical/subtropical moist lowland forest - mam_IUCN.csv, bird_IUCN.csv

#Date: Oct 10th, 2020

#Modified: Aug 27th, 2022


# Libraries
library(taxize)
library(httr)
library(jsonlite)
library(dplyr)
library(rredlist)

## Token for accessing IUCN data through their API
## User must obtain a token (https://apiv3.iucnredlist.org/api/v3/token) before using the 'rredlist package'.
token <- 'INSERT TOKEN HERE'
api_url <- 'https://apiv3.iucnredlist.org/api/v3/'

# To obtain country codes of interest:
all_countries <- GET(url = paste0(api_url, 'country/list?token=',token))
all_countries_1 <- fromJSON(content(all_countries, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)$result

# Extract all species in the IUCN database by country
# In this example, we are downloading species lists for countries with significant mountain ranges in Central and South America

## South America

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
#Chile
Chile_species <- rl_sp_country("CL", key = token, parse = TRUE)
c7 <- Chile_species$result[1]

##Central America 

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

# Generate a list of all species in South America and Central America
#There will be repeats within these objects because the same species may be present in multiple countries. These should be removed later.
all_species_south_america <- rbind(c1, c2, c3, c4, c5, c6, c7)
all_species_central_america <- rbind(c8, c9, c10, c11, c12, c13, c14)

## Extract taxa of interest

# Get codes for the different species groups
grp_codes <- GET(url = paste0(api_url, 'comp-group/list?token=', token))
code <-fromJSON(content(grp_codes, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)$result

# Get species list of all birds across all countries. 
all_birds <-rl_comp_groups(group = c('birds'), key = token)

# Get species list of all mammals across all countries
all_mammals <-rl_comp_groups(group = c('mammals'), key = token)
#test <-rl_threats(name='Bassaricyon neblina', key=token)


## Extract mammals and birds occurring in Central and South america
# South America

# Mammals

# Turn the list into a dataframe so the merge can be completed
# This subsets the entire species list to only the mammals included in our country list of species
all_mammals <- as.data.frame(all_mammals$result)
SA_mammals <- merge(all_mammals, all_species_south_america, by="taxonid")
SA_mammals <- distinct(SA_mammals, scientific_name, .keep_all = TRUE)


# Birds
all_birds<- as.data.frame(all_birds$result)
SA_birds <- merge(all_birds, all_species_south_america, by="taxonid")
SA_birds <- distinct(SA_birds, scientific_name, .keep_all = TRUE)

# Central America
#Mammals
CA_mammals <- merge(all_mammals, all_species_central_america, by="taxonid")
CA_mammals <- distinct(CA_mammals, scientific_name, .keep_all = TRUE)

# Birds
CA_birds <- merge(all_birds, all_species_central_america, by="taxonid")
CA_birds <- distinct(CA_birds, scientific_name, .keep_all = TRUE)

# All Latin American mammals (removed duplicates due to species occurring in more than one country)
latin_american_mammals <- merge(SA_mammals, CA_mammals, all=TRUE)


# All latin American birds
latin_american_birds <- merge(SA_birds, CA_birds, all=TRUE)

## Subset the species lists to only those found in "Tropical Moist Montane Habitat"

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

# Write to a file so don't have to keep re-running (can cause API issues)
setwd("INSERT DIRECTORY HERE")
write.csv(all_habitat_mammals, 'all_habitat_mammals.csv')

# Apply get_habitat to each row of latin_american_birds
# This is a large dataset so break it up into chunks for shorter calls over the IUCN API. Each call will take ~30 min and this minimizes the chances of a crash. The common error is "Bad Gateway (error 502)". Emailed IUCN about this and this is the best solution. To avoid this error, a user can download straight off of the IUCN website. For reproducibility, the code to do this is included here. This is how the final dataset was obtained for publication.

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

# Bind all of the bird habitat data together
all_habitat_birds <- rbind(bird_hab_1, bird_hab_2, bird_hab_3, bird_hab_4, bird_hab_5, bird_hab_6, bird_hab_7,bird_hab_8,bird_hab_9, bird_hab_9.5,bird_hab_10)

# write to a file to avoid re-running
write.csv(all_habitat_birds, 'all_habitat_birds.csv')

# Subset the habitat information for all species by those that are tropical montane
# There will be species that overlap between the two habitat types; can condense those later on with a "habitat" column for each species where the categories are montane, lowland, and both.
bird_habitat_montane <- all_habitat_birds[all_habitat_birds$habitat =="Forest - Subtropical/Tropical Moist Montane",]
mam_habitat_montane <- all_habitat_mammals[all_habitat_mammals$habitat =="Forest - Subtropical/Tropical Moist Montane",]
bird_habitat_lowland <- all_habitat_birds[all_habitat_birds$habitat =="Forest - Subtropical/Tropical Moist Lowland",]
mam_habitat_lowland <- all_habitat_mammals[all_habitat_mammals$habitat =="Forest - Subtropical/Tropical Moist Lowland",]

# Merge Latin American animal datasets with Central and South American species lists for montane and lowland species
#join lowland and montane together
#We can parse them back out later on

bird_habitat_join <- rbind(bird_habitat_lowland, bird_habitat_montane)

mammal_habitat_join <- rbind(mam_habitat_lowland, mam_habitat_montane)

# Merge the habitat data with the IUCN lists so that we have the statuses and habitats combined with species names (not just taxonid). Only keep species with habitat info.

mammal_IUCN<-merge(latin_american_mammals, mam_habitat_join, by= "taxonid", all.y=TRUE)
write.csv(mam_IUCN, "mam_IUCN.csv")


bird_IUCN<- merge(latin_american_birds, bird_habitat_join, by= "taxonid", all.y=TRUE)
write.csv(bird_IUCN, "bird_IUCN.csv")
