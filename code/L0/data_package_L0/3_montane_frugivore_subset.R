#Title: Montane frugivore subset

#Project: Montane Frugivoria

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske, Patrick Bills

#Data input: outputs of code "2_external_trait_database_merge.R" - mam_trait_all_final.csv; bird_trait_all_final.csv

#Data output: bird_frug_montane.csv, mam_frug_montane.csv

#Overview: Subset IUCN/EltonTraits dataset to only those species eating 10% fruit or greater. Further subset the dataset by those existing in tropical moist montane  forest only.

#Date: Oct 10th, 2020

#Modified: Oct 15th, 2021


# Read in output of previous script "external_trait_database_merge".
mam_trait_all_final <- read.csv("INSERT PATH HERE")
bird_trait_all_final <-read.csv("INSERT PATH HERE")

## Subset by frugivorous species
mam_frug <- mam_trait_all_final[mam_trait_all_final$Diet.Fruit>=10,] 

#Parse out montane and lowland mammal species for use as individual databases
#montane
mam_frug_montane <- mam_frug[mam_frug$habitat=="Forest - Subtropical/Tropical Moist Montane",]
write.csv(mam_frug_montane, "mammal_montane_database.csv")

## Subset by frugivorous species
bird_frug <- bird_trait_all_final[bird_trait_all_final$Diet.Fruit>=10,] 

#Parse out montane and lowland mammal species for use as individual databases
#montane
bird_frug_montane <- bird_frug[bird_frug$habitat=="Forest - Subtropical/Tropical Moist Montane",]
write.csv(bird_frug_montane, "bird_montane_database.csv")


