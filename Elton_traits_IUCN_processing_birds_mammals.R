#Read in Elton Traits dataset 
birds <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/BirdFuncDat.csv")
mamm<- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/MamFuncDat.csv")

#Combine bird elton traits dataset and IUCN bird data together
colnames(birds)[which(names(birds) == "Scientific")] <- "scientific_name"
bird_trait_IUCN_2<- merge.data.frame(birds, all_birds, by= "scientific_name", all=TRUE)

write.csv(bird_trait_IUCN, 'trait_iucn_bird.csv')

#Combine mammal elton traits dataset and IUCN bird data together
all_mammals <-read.csv("/Users/bethgerstner/Desktop/mammals_IUCN_testing.csv")
colnames(mamm)[which(names(mamm) == "Scientific")] <- "scientific_name"
mamm_trait_IUCN_3 <- merge.data.frame(mamm, all_mammals, by= "scientific_name", all=TRUE)

setwd("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/IUCN_Elton_Traits")
write.csv(mamm_trait_IUCN, 'trait_iucn_mammal_2.csv')

#Merge each taxon dataset by taxonid to have a comprehensive list of functional traits, IUCN statuses, and habitat types
bird_trait_IUCN_habitat <- merge.data.frame(bird_trait_IUCN, all_habitat, by= "taxonid")
write.csv(bird_trait_IUCN_habitat, 'complete_IUCN_trait_birds.csv')

mamm_trait_IUCN_habitat <- merge.data.frame(mamm_trait_IUCN, all_habitat, by= "taxonid")
write.csv(mamm_trait_IUCN_habitat, 'complete_IUCN_trait_mammals.csv')











