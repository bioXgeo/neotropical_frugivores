#Retrieving synonyms for IUCN names using taxize
#This can be used to automate the process of finding species in other (older) existing databases where the names might not be up to date

#Find all names for a given taxa
syn <- synonyms(scientific_names, db=c('itis'))

test <- synonyms("Puma yagouaroundi", db="", accepted=TRUE)

#Put into a dataframe format 
mammal_syn <- synonyms_df(syn)
mammal_syn_df <-as.data.frame(mammal_syn$syn_name)
na.omit(mammal_syn_df)

names(mammal_syn)

#If accepted name not found in dataset, look for synonyms
#Put the name found in the database into it's own column labeled "Elton traits" "Generation Length" "Pantheria'.

IUCN_names <-as.data.frame(scientific_names)
E_names <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/Elton_Traits_birds_mammals/MamFuncDat.csv")
Elton_names <- as.data.frame(E_names[c("Scientific")])
colnames(Elton_names)[colnames(Elton_names)=="Scientific"] <- "scientific_names"
match_IUCN_elton <-IUCN_names[match(Elton_names$scientific_names, IUCN_names$scientific_names), ]
final_match_IUCN_elton <-as.data.frame(na.omit(match_IUCN_elton))
colnames(final_match_IUCN_elton)[colnames(final_match_IUCN_elton)=="na.omit(match_IUCN_elton)"] <- "scientific_names"

#These are the species_names that are in IUCN and Elton Databases
match_IUCN_elton <-na.omit(as.data.frame(IUCN_names[match(Elton_names$scientific_names, IUCN_names$scientific_names), ]))

#Species that are in IUCN, but not in Elton Traits database
#These are the species that needed to be looked up manually.
no_match_IUCN_elton <- as.data.frame(subset(IUCN_names$scientific_names, !(IUCN_names$scientific_names %in% Elton_names$scientific_names)))

nrow(no_match_IUCN_elton) + nrow(match_IUCN_elton)
colnames(no_match_IUCN_elton)[colnames(no_match_IUCN_elton)=="subset(IUCN_names$scientific_names, !(IUCN_names$scientific_names %in% Elton_names$scientific_names))"] <- "scientific_names"

# Species names that are synonyms found in Elton_Traits database
# This is not finding all synonyms... taxize is not finding all possibilities -- this makes automating this process hard.
# Had to look species up manually for obscure synonyms.
IUCN_elton_syn <- na.omit(as.data.frame(no_match_IUCN_elton[match(mammal_syn_df$scientific_names, no_match_IUCN_elton$scientific_names),]))



