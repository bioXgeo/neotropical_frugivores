#Title: Montane frugivore subset

#Project: Frugivoria

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske, Patrick Bills

#Data input: outputs of code "L0_2_external_trait_database_merge.R" - mam_trait_all_final.csv; bird_trait_all_final.csv

#Data output: bird_frug.csv, mam_frug.csv

#Overview: Subset IUCN/EltonTraits dataset to only those species eating 10% fruit or greater. Further subset the dataset by those existing in tropical moist montaneforest only.

#Date: Oct 10th, 2020

#Modified: Aug 27th, 2022


# Read in output of previous script "external_trait_database_merge".
mam_trait_all_final <- read.csv("INSERT PATH HERE")
bird_trait_all_final <-read.csv("INSERT PATH HERE")

# Subset by frugivorous mammal species
mam_frug <- mam_trait_all_final[mam_trait_all_final$Diet.Fruit>=10,] 


# Subset by frugivorous bird species
bird_frug <- bird_trait_all_final[bird_trait_all_final$Diet.Fruit>=10,] 

# Write frugivore subsets to a file
setwd("INSERT PATH HERE")
write.csv(mam_frug, "mam_frug.csv")
write.csv(bird_frug, "final_bird_dataset.csv")

