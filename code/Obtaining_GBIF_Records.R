#GBIF Occurrence Data Retrieval 

#Download data by genus 
library(dismo)

#read in mammal database
data1_mammals<- read.csv("/Volumes/GoogleDrive/My Drive/neotropical_frugivores/Database2019/Databases/L1/IUCN_trait_montane_mammals_IUCN_trait_montane_mammals_cleaned_6_19.csv")
head(data1_mammals)
scientific_names <- data1_mammals$scientific_name
data_name_df <- as.data.frame(scientific_names)

#split scientific names into genus and species
library(tidyr)
scientific_names_mammals <-separate(data_name_df, scientific_names, into = c("genus", "species"), sep= " (?=[^ ]+$)")
scientific_names_mammals=na.omit(scientific_names_mammals)
# Downloading GBIF data
# Only grabs 300 at a time. Need to automate this process for all mammal and bird species in the final database. 
# Ideally this code will put all of the records for each species in a single dataframe, but because this will be so big, maybe it is 
# better to break the original species list up into batches.
all.mammals <- data.frame()
for(i in 1:nrow(scientific_names_mammals)){
  genus.i <- scientific_names_mammals[i,"genus"]
  species.i <- scientific_names_mammals[i,"species"]
  gbif_mammals <- gbif(genus.i, species=species.i, args=NULL, geo=TRUE, sp=FALSE, 
               removeZeros=FALSE, download=TRUE, ntries=5, nrecs=300, start=1, end=Inf)
  gbif_mammals <- gbif_mammals[,c("scientificName","lat","lon")]
  all.mammals= rbind(all.mammals, gbif_mammals)
}

nrow(scientific_names_mammals)
