#Project: Montane Frugivoria

#Code reference: montane_frugivore_subset

#Purpose: Subset IUCN/EltonTraits dataset to only those species eating 10% fruit or greater. Further divide the dataset by those existing in Lowland and Montane moist forest.

#Date: Oct 10th, 2020

#Modified: Oct 15th, 2021

#By: Beth E. Gerstner

# Read in output of previous script "external_trait_database_merge".
mam_trait_all_final <- read.csv("INSERT PATH HERE")
bird_trait_all_final <-read.csv("INSERT PATH HERE")

## Subset by frugivorous species
mam_frug <- mam_trait_all_final[mam_trait_all_final$Diet.Fruit>=10,] 

#Parse out montane and lowland mammal species for use as individual databases
#montane
mam_frug_montane <- mam_frug[mam_frug$habitat=="Forest - Subtropical/Tropical Moist Montane",]
write.csv(mam_frug_montane, "mammal_montane_database.csv")

#lowland
mam_frug_lowland <- mam_frug[mam_frug$habitat=="Forest - Subtropical/Tropical Moist Lowland",]
write.csv(mam_frug_lowland, "mammal_lowland_database.csv")


