#GBIF Occurrence Data Retrieval 
#Project: Neotropical Frugivores
#By: Beth Gerstner 6/25/19

#Download data by genus 
library(dismo)

#read in mammal database
data1_mammals<- read.csv("/Volumes/GoogleDrive/My Drive/neotropical_frugivores/Database2019/Databases/L1/IUCN_trait_montane_mammals_IUCN_trait_montane_mammals_cleaned_6_19.csv")
head(data1_mammals)
scientific_names <- data1_mammals$scientific_name
data_name_df <- as.data.frame(scientific_names)

#split scientific names into genus and species for the mammal database
library(tidyr)
scientific_names_mammals <-separate(data_name_df, scientific_names, into = c("genus", "species"), sep= " (?=[^ ]+$)")
scientific_names_mammals=na.omit(scientific_names_mammals)
# Downloading GBIF data
# Only grabs 300 at a time. Need to automate this process for all mammal and bird species in the final database. 
# Ideally this code will put all of the records for each species in a single dataframe, but because this will be so big, maybe it is 
# better to break the original species list up into batches.
all.mammals <- data.frame()
for(i in 1:nrow(scientific_names_mammals)) {
  tryCatch({
  genus.i <- scientific_names_mammals[i,"genus"]
  species.i <- scientific_names_mammals[i,"species"]
  gbif_mammals <- gbif(genus.i, species=species.i, args=NULL, geo=TRUE, sp=FALSE, 
               removeZeros=FALSE, download=TRUE, ntries=5, nrecs=300, start=1, end=Inf)
  gbif_mammals <- gbif_mammals[,c("scientificName","lat","lon")]
  all.mammals= rbind(all.mammals, gbif_mammals)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# tryCatch allows the code to continue without getting stuck at errors.
# Errors are due to species that have empty fields (for ex. no occurrence records)

write.csv(all.mammals, file="/Volumes/GoogleDrive/My Drive/neotropical_frugivores/Database2019/all.mammals.csv")
nrow(scientific_names_mammals)

#read in bird database
data1_bird<- read.csv("/Volumes/GoogleDrive/My Drive/neotropical_frugivores/Database2019/Databases/L1/IUCN_trait_montane_birds_IUCN_trait_montane_birds.csv")
head(data1_bird)
scientific_names_b <- data1_bird$scientific_name
data_name_df_b <- as.data.frame(scientific_names_b)


#split scientific names into genus and species for the bird database
library(tidyr)
scientific_names_birds <-separate(data_name_df_b, scientific_names_b, into = c("genus", "species"), sep= " (?=[^ ]+$)")
scientific_names_birds=na.omit(scientific_names_birds)
# Downloading GBIF data
# Only grabs 300 at a time. Need to automate this process for all mammal and bird species in the final database. 
# Ideally this code will put all of the records for each species in a single dataframe, but because this will be so big, maybe it is 
# better to break the original species list up into batches.
all.birds <- data.frame()
for(i in 1:nrow(scientific_names_birds)) {
  tryCatch({
    genus.i <- scientific_names_birds[i,"genus"]
    species.i <- scientific_names_birds[i,"species"]
    gbif_birds <- gbif(genus.i, species=species.i, args=NULL, geo=TRUE, sp=FALSE, 
                         removeZeros=FALSE, download=TRUE, ntries=5, nrecs=300, start=1, end=Inf)
    gbif_birds <- gbif_birds[,c("scientificName","lat","lon")]
    all.birds= rbind(all.birds, gbif_birds)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

write.csv(all.birds, file="/Volumes/GoogleDrive/My Drive/neotropical_frugivores/Database2019/all.birds.csv")
nrow(scientific_names_birds)
