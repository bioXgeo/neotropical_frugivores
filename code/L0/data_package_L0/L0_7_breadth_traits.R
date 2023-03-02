#Title: Calculating breadth traits and appending to databases

#Project: Frugivoria

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske, Patrick Bills

#Overview: Derives breadth traits from different data sources and appends to full database for mammals and birds. 
# The diet breadth trait was calculated using the Shannon Diversity index and was derived from % contribution of species diets (e.g., "diet_inv_e", "diet_vend_e", "diet_vect_e", etc.) based on traits from the EltonTraits dataset. 
# Diet breadth here is defined as the number of diet categories consumed, with any category with a % value greater than 0 being counted. 
# Habitat breadth was derived from the number of habitat types listed for each species in their formal IUCN assessment.
# Note: Need to generate an IUCN Redlist token to be able to use this code (https://apiv3.iucnredlist.org/api/v3/token)

#Data Input: Frugivoria_bird_database_2023.csv, Frugivoria_mammal_database_2023.csv

#Data Output: Frugivoria_bird_database_2023_full.csv, Frugivoria_mammal_database_2023_full.csv with added traits.

#Date: January 24th, 2023

#Load library
library(vegan)

#read in completed mammal database
bird <- read.csv("INSERT DATABASE FILE PATH")

#read in bird database
mam <-read.csv("INSERT DATABASE FILE PATH")

#subset mammal database to % diet traits
new_mam <-mam[,12:21]

#calculate Shannon Diversity index on % diet traits. Low values indicate specialization in diet. High values indicate diversity of the diet and are likely more omnivorous species.
shannon_index_m <- diversity((new_mam), index="shannon")

#insert diet breadth as a column
mam$diet_breadth <- shannon_index_m


#subset bird database to % diet traits
new_bird <- bird[,12:21]

#calculate Shannon Diversity index on % diet traits. Low values indicate specialization in diet. High values indicate diversity of the diet and are likely more omnivorous species.
shannon_index_b <- diversity((new_bird), index="shannon")

#insert diet breadth as a column
bird$diet_breadth <- shannon_index_b

#___________________________________________________________________________________________________
# Calculate habitat breadth based on IUCN habitat data
library(rredlist)
library(dplyr)


#subset mammal and bird databases so that we have a list of their scientific names

scientific_name_m<- mam %>%
  select(IUCN_species_name)

scientific_name_b<- bird %>%
  select(IUCN_species_name)


scientific_names <- rbind(scientific_name_m, scientific_name_b)


#Divide into groups of 600. IUCN's API times out after too many species - noted in package description under "rate limiting". Need to obtain a redlist API key (https://apiv3.iucnredlist.org/api/v3/token).

all.species.habitat <- data.frame()
for(i in 1:691){
  species.i <- scientific_names[i,"IUCN_species_name"]
  IUCN_habitat.n <- rl_habitats(species.i, key='INSERT KEY')
  IUCN_habitat <- as.data.frame(IUCN_habitat.n$result)
  IUCN_habitat$species <- IUCN_habitat.n$name
  all.species.habitat= rbind(all.species.habitat, IUCN_habitat)
}

all.species.habitat.2 <- data.frame()
for(i in 692:1292){
  species.i <- scientific_names[i,"IUCN_species_name"]
  IUCN_habitat.n <- rl_habitats(species.i, key='INSERT KEY')
  IUCN_habitat <- as.data.frame(IUCN_habitat.n$result)
  IUCN_habitat$species <- IUCN_habitat.n$name
  all.species.habitat.2= rbind(all.species.habitat.2, IUCN_habitat)
}

all.species.habitat.3 <- data.frame()
for(i in 1293:1746){
  species.i <- scientific_names[i,"IUCN_species_name"]
  IUCN_habitat.n <- rl_habitats(species.i, key='INSERT KEY')
  IUCN_habitat <- as.data.frame(IUCN_habitat.n$result)
  IUCN_habitat$species <- IUCN_habitat.n$name
  all.species.habitat.3= rbind(all.species.habitat.3, IUCN_habitat)
}

# join all habitat subsets
all.species.habitat.full <- rbind(all.species.habitat, all.species.habitat.2, all.species.habitat.3)

# remove duplicated habitat types (these are because of certain habitats being listed multiple times for a species since they can be for both "breeding" and "resident").

all.species.habitat.full.drm.d <- all.species.habitat.full[!duplicated(all.species.habitat.full[c(2,6)]),]
write.csv(all.species.habitat.full.drm.d, "all.habitats.no.dupes.csv")

#Count number of repeats per species, which should be unique habitat
habitat_breadth <-all.species.habitat.full.drm.d%>% group_by(species)%>% 
  count(species)

#1730 species have habitat data. 
habitat_breadth_df <-as.data.frame(habitat_breadth)
#write.csv(habitat_breadth_df, "C:/Users/bgers/Desktop/habitat_breadth_final.csv")

#change column names of habitat breadth dataframe for merging
colnames(habitat_breadth_df)[1] <- "IUCN_species_name"
colnames(habitat_breadth_df)[2] <- "habitat_breadth"

#merge full mammal database with habitat breadth 
full_mam <- merge(mam, habitat_breadth_df, by= "IUCN_species_name", all.x=T)

#merge full bird database with habitat breadth
full_bird <- merge(bird, habitat_breadth_df, by= "IUCN_species_name", all.x=T)

setwd("INSERT FILE PATH")

# Write full database with breadth traits to a file
write.csv(full_mam, "Frugivoria_mammal_database_2023_full.csv")
write.csv(full_bird, "Frugivoria_bird_database_2023_full.csv")
