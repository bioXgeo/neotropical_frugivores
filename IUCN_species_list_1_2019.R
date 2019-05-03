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

api_url <- 'apiv3.iucnredlist.org/api/v3'

#____________________________________________________________________________________________________________________________
# get all country codes
all_countries <- GET(url = paste0(api_url, 'country/list?token=', token))
all_countries_1 <- fromJSON(content(all_countries, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)$result

#____________________________________________________________________________________________________________________________
#extract species by country
#South America

#colombia
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

#____________________________________________________________________________________________________________________________
#Central America 

#Mexico
mexico_species <- rl_sp_country("MX", key = token, parse = TRUE)
c7 <- mexico_species$result[1]
#Guatemala
guatemala_species <- rl_sp_country("GT", key = token, parse = TRUE)
c8 <- guatemala_species$result[1]
#Costa Rica
costa_rica_species <- rl_sp_country("CR", key = token, parse = TRUE)
c9 <- costa_rica_species$result[1]
#Panama
panama_species <- rl_sp_country("PA", key = token, parse = TRUE)
c10 <- panama_species$result[1]
#El Salvador
el_salvador_species <-rl_sp_country("SV", key = token, parse = TRUE)
c11 <- el_salvador_species$result[1]
#Belize
belize_species <- rl_sp_country("BZ", key = token, parse = TRUE)
c12 <- belize_species$result[1]
#Nicaragua
nicaragua_species <- rl_sp_country("NI", key = token, parse = TRUE)
c13 <- nicaragua_species$result[1]

#Generate a list of all species in South america and Central america to extract mammmals from
all_species_south_america <- rbind(c1, c2, c3, c4, c5, c6)
all_species_central_america <- rbind(c7, c8, c9, c10, c11, c12, c13)
#__________________________________________________________________________________________________________________________
# Break down by species group if possible 

# Get codes for the different species groups
grp_codes <- GET(url = paste0(api_url, '/comp-group/list?token=', token))
fromJSON(content(grp_codes, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)

# Get species list of all birds across all countries
all_birds <- GET(url = paste0(api_url, '/comp-group/getspecies/birds?token=', token))
all_birds <- fromJSON(content(all_birds, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)$result

# Get species list of all mammals across all countries
all_mammals <- GET(url = paste0(api_url, '/comp-group/getspecies/mammals?token=', token))
all_mammals <- fromJSON(content(all_mammals, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)$result

#___________________________________________________________________________________________________________________________
# Mammals and birds occurring in central and south america
#South America

#Mammals
SA_mammals <- merge(all_mammals, all_species_south_america, by="taxonid")


#Birds
SA_birds <- merge(all_birds, all_species_south_america, by="taxonid")

#Central America
#Mammals
CA_mammals <- merge(all_mammals, all_species_central_america, by="taxonid")

#Birds
CA_birds <- merge(all_birds, all_species_central_america, by="taxonid")

#Latin American Mammals
latin_american_mammals <- unique(merge(SA_mammals, CA_mammals, all.x=TRUE))


#Latin American Birds
latin_american_birds <- unique(merge(SA_birds, CA_birds, all.x=TRUE))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Attach genus and species to the ID
library(rgbif)


#_________________________________________________________________________________________________________________________
# Get habitat for each species --------------------------------------------

# Include error-checking code in the function so that it will return NA for everything if there's no data.

get_habitat <- function(ID) {
  habitat_by_sp <- GET(url = paste0(api_url, '/habitats/species/id/', ID,'?token=', token))
  habitat_by_sp <- fromJSON(content(habitat_by_sp, as = 'text'), simplifyDataFrame = TRUE, flatten = TRUE)$result
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


# Apply get_habitat to each row of latin_american_birds
all_habitat_birds <- latin_american_birds %>%
  rowwise %>%
  do(get_habitat(.$taxonid))


#Read in Elton Traits dataset 
birds <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/BirdFuncDat.csv")
mamm<- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/MamFuncDat.csv")

#Combine bird elton traits dataset (full) and IUCN bird dataset for Latin America together
colnames(birds)[which(names(birds) == "Scientific")] <- "scientific_name"
bird_trait_IUCN<- merge.data.frame(birds, latin_american_birds, by= "scientific_name", all=TRUE)
tail(bird_trait_IUCN)

#Combine mammal elton traits dataset and IUCN bird data together
colnames(mamm)[which(names(mamm) == "Scientific")] <- "scientific_name"
mamm_trait_IUCN <- merge.data.frame(mamm, latin_american_mammals, by= "scientific_name", all=TRUE)

setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Mammals_Birds_all_data/Trait_database_2019")
write.csv(bird_trait_IUCN, 'bird_trait_IUCN.csv')
write.csv(mamm_trait_IUCN, 'mamm_trait_IUCN.csv')

# Read in cleaned data (removed all birds and mammals outside of Latin America. Only rows with IUCN statuses were retained)
# Some of these rows had no elton traits data but will be filled in manually. This could be due to spelling errors
# or changes in taxonomy.
# Also removed species that have lower than 10% frugivorous diet. Retained those species that did not have elton trait data.

bird_frug_cleaned <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Mammals_Birds_all_data/Trait_database_2019/bird_trait_IUCN_NA_rm_pos_frugivore.csv")
mamm_frug_cleaned <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Mammals_Birds_all_data/Trait_database_2019/mamm_trait_IUCN_NA_rm_pos_frugivory.csv")

#Merge each taxon dataset by taxonid to have a comprehensive list of functional traits, IUCN statuses, and habitat types
bird_trait_IUCN_habitat <- merge.data.frame(bird_frug_cleaned, all_habitat_birds, by= "taxonid")
write.csv(bird_trait_IUCN_habitat, 'IUCN_trait_birds_habitat.csv')
length(unique(bird_trait_IUCN_habitat$scientific_name))
#Merge mammals by taxonid
mamm_trait_IUCN_habitat <- merge.data.frame(mamm_frug_cleaned, all_habitat_mammals, by= "taxonid")
write.csv(mamm_trait_IUCN_habitat, 'complete_IUCN_trait_mammals_habitat.csv')

#check to make sure this actually worked
length(unique(mamm_trait_IUCN_habitat$scientific_name))

# Obtain species that have a habitat designation of tropical montane and/or tropical moist lowland
# Figure out how many species overlap between the two categories (both tropical moist lowland and montane)
#Birds montane (greater than 1200m)
bird_species_montane <- bird_trait_IUCN_habitat[bird_trait_IUCN_habitat$habitat=="Forest - Subtropical/Tropical Moist Montane",]
length(unique(bird_species_montane$scientific_name))
write.csv(bird_species_montane, 'IUCN_trait_montane_birds.csv')

#Birds lowland (less than 1200m)
bird_species_lowland <- bird_trait_IUCN_habitat[bird_trait_IUCN_habitat$habitat=="Forest - Subtropical/Tropical Moist Lowland",]
length(unique(bird_species_lowland$scientific_name))
write.csv(bird_species_lowland, 'IUCN_trait_lowland_birds.csv')

# Species in the lowland dataset not in the highland dataset
bird_species_montane_unique <-unique(bird_species_montane$scientific_name)
bird_species_lowland_unique <-unique(bird_species_lowland$scientific_name)
no_match_bird_low <- as.data.frame(bird_species_lowland_unique[!bird_species_lowland_unique %in% bird_species_montane_unique])
nrow(no_match_bird_low) #528 new species
#Check that this worked

#mammals montane
mamm_species_montane <- mamm_trait_IUCN_habitat[mamm_trait_IUCN_habitat$habitat=="Forest - Subtropical/Tropical Moist Montane",]
write.csv(mamm_species_montane, 'IUCN_trait_montane_mammals.csv')
length(unique(mamm_species_montane$scientific_name))

#mammals lowland
mamm_species_lowland <- mamm_trait_IUCN_habitat[mamm_trait_IUCN_habitat$habitat=="Forest - Subtropical/Tropical Moist Lowland",]
write.csv(mamm_species_lowland, 'IUCN_trait_lowland_mammals.csv')
length(unique(mamm_species_lowland$scientific_name))

# Species in the lowland dataset not in the highland dataset
mamm_species_montane_unique <-unique(mamm_species_montane$scientific_name)
mamm_species_lowland_unique <-unique(mamm_species_lowland$scientific_name)
no_match_mam_low <- as.data.frame(mamm_species_lowland_unique[!mamm_species_lowland_unique %in% mamm_species_montane_unique])
nrow(no_match_mam_low) #253 new species
#Check that this worked
write.csv(no_match_mam_low, 'no_match_mam_low.csv')
write.csv(mamm_species_lowland_unique, 'low_unique.csv')
write.csv(mamm_species_montane_unique, 'montane_unique.csv')
#check to make sure this works 
length(unique(mamm_species_montane$scientific_name))
#[6] "Forest - Subtropical/Tropical Moist Lowland"                                           
#[7] "Forest - Subtropical/Tropical Moist Montane" #maybe use this? 

