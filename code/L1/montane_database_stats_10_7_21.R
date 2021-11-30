#Project: Montane Frugivoria - analyzing the montane database 

#Purpose: To analyze and assess what is in the montane frugivore database for the data paper.

#Date: Oct 11th, 2021

#By: Beth E. Gerstner

#read in mammal database
mam <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/data/frugivore/L1/complete_database/Frugivoria_montane_mammal_database.csv")
#read in bird database
bird<- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/data/frugivore/L1/complete_database/Frugivoria_montane_bird_database.csv")


#Needed stats
#1) how many new traits in total
#a) for birds

#These are from previous databases
body_mass_b<- length(which(!is.na(bird$body_mass_value_g_e)))
diet_categ_b <- length(which(!is.na(bird$diet_cat))) 
nocturnal_b <- length(which(!is.na(bird$activity_nocturnal_e)))
for_strat_b <- length(which(!is.na(bird$for_strat_aerial_e)))
diet_inv_b <- length(which(!is.na(bird$diet_inv_e)))
#diet_vend_b <- length(which(!is.na(bird$diet_vend_e)))  
#diet_vect_b <- length(which(!is.na(bird$diet_vect_e)))   
#diet_fish_b <- length(which(!is.na(bird$diet_vfish_e)))  
#diet_vunk_b <- length(which(!is.na(bird$diet_vunk_e))) 
#diet_scav_b <- length(which(!is.na(bird$diet_scav_e)))  
#diet_fruit_b <-length(which(!is.na(bird$diet_fruit_e)))  
#diet_nect_b <-length(which(!is.na(bird$diet_nect_e)))                 
#diet_seed_b <-length(which(!is.na(bird$diet_seed_e)))                 
#diet_plant_b <-length(which(!is.na(bird$diet_plant_e)))                    

elton_traits_b <- body_mass_b + diet_categ_b + nocturnal_b + for_strat_b + diet_inv_b # 691 is %diet trait & for strat. Did not count each of the % columns seperately as this is part of a single trait (% diet makeup and % time in strata)
#3410 from Elton Traits

# count new bird traits that we have added
longevity_b <- length(which(!is.na(bird$longevity)))
home_range_b <- length(which(!is.na(bird$home_range_size)))
habitat_special_b <- length(which(!is.na(bird$habitat_specialization)))
gen_time_b <- length(which(!is.na(bird$generation_time)))
body_size_b <- length(which(!is.na(bird$body_size_cm)))
sexual_dim_b <- length(which(!is.na(bird$sexual_dimorphism)))
range_1_b <- length(which(!is.na(bird$observed_range_sqkm)))
range_2_b <-  length(which(!is.na(bird$inferred_range_sqkm)))

new_bird_traits <-  longevity_b + habitat_special_b + gen_time_b + body_size_b + sexual_dim_b + range_1_b + range_2_b # 4021 new traits

all_bird_traits <-  elton_traits_b + new_bird_traits #7431 traits

#b) for mammals

#These are from previous databases (need to add Pantheria in here), try and sum NAs for x:x for Pantheria traits

diurnal_m <-length(which(!is.na(mam$activity_diurnal_e)))
crepuscular_m <-length(which(!is.na(mam$activity_crepuscular_e)))
nocturnal_m <- length(which(!is.na(mam$activity_nocturnal_e)))
body_mass_m<- length(which(!is.na(mam$body_mass_value_g_e)))
for_strat_m <- length(which(!is.na(mam$for_strat_value_e))) 
diet_inv_m <- length(which(!is.na(mam$diet_inv_e)))  #% diet will count as one trait

elton_traits_m <- diurnal_m + crepuscular_m + nocturnal_m + body_mass_m + for_strat_m + diet_inv_m #1872

#count Pantheria traits from within the database by looking at the citations saying "PanTHERIA".
in_db_pantheria <- length(which(mam=="PanTHERIA")) #6 in database. Should remove these from the new count.

#count the number of traits in PanTHERIA but remove the reference column
pantheria <- mam[,80:129]
pantheria$references_p <- NULL
pantheria_traits <- length(which(!is.na(pantheria))) #6,106

#count the number of newly added traits
diet_categ_m <- length(which(!is.na(mam$diet_cat)))
longevity_m <- length(which(!is.na(mam$longevity)))
home_range_m <- length(which(!is.na(mam$home_range_size)))
habitat_special_m <- length(which(!is.na(mam$habitat_specialization)))
gen_time_m <- length(which(!is.na(mam$generation_time)))
body_size_m<- length(which(!is.na(mam$body_size_cm)))
range_1_m <- length(which(!is.na(mam$observed_range_sqkm)))
range_2_m <-  length(which(!is.na(mam$inferred_range_sqkm)))

#all new traits added minus those within database from PanTHERIA
#add 3 ranges to be included
new_mam_traits <- diet_categ_m + longevity_m +home_range_m +range_1_m +range_2_m + habitat_special_m +gen_time_m +body_size_m -6  # 2045

#Total number of traits in the database
all_mam_traits <- new_mam_traits + pantheria_traits + elton_traits_m #10,023

#Total number of traits in the entire dataset
all_mam_traits + all_bird_traits #17,454
#2) # of genera
#a) birds 
#b) mammals

#Unique genera
#99 for mammals
#226 for birds

library(tidyr)

#unique genera birds
length(unique(bird$genus)) #226 genera

#unique genera mammals
length(unique(mam$genus)) #99 genera

#Unique mammal species 
length(unique(mam$IUCN_species_name)) #306

#Unique Bird species 
length(unique(bird$IUCN_species_name)) #682

#3) #New species/name changes
#New species - have either been reclassified or recently discovered <-- these are the original numbers. I need to write code that can quantify name changes. It should look at mismatches between the IUCN_name and the scientific_name (EltonTraits). These species can be new species that we've then imputed to a close relative, species that have been reclassified, species with misspellings that we've standardized to the IUCN name.

#23 new mammal species
#67 mammal species reclassified

# 2 new bird species
# 126 birds reclassified
# 6 Spelling errors

#Need to quantify mismatches between the elton_species_name and IUCN_species_name. 
#mammals

#how many mammal species names are mismatched?
length(mam$taxonomic_disparity[mam$taxonomic_disparity==1]) #90

#how many bird species names are mismatched?
length(bird$taxonomic_disparity[bird$taxonomic_disparity==1]) #134

#names mismatched
134+90 # 224 - 


#5) How many traits were imputed to family? (not including EltonTraits, will have to add this later on)
library (tidyr)
mam_genus_level_impute<-mam%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==0)

#imputed to genus: new traits
#home_range 27
#longevity 40
#generation_time 45
#body_size 39
#sexual dimorphism 61
#diet_cat #23 new species. need to know how many were reclassified to figure out what's imputed to genus

#total imputed for newly added traits
impute_mam_genus_new <-27+40+45+39+61+23 # 235


#percent imputed in total for genus
genus_impute_m <-impute_mam_genus_new/new_mam_traits #11.49% to genus for mammals

##imputed to family: new traits

#longevity 50
#generation_time 91
#body_size 3
#sexual dimorphism 31
#home range 1

mam_fam_level_impute<-mam%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==-1)

impute_mam_fam_new <-50+91+3+31+1 # 176

#total traits imputed
total_mam_impute <-impute_mam_genus_new+impute_mam_fam_new #411

#%imputed for mammals
total_mam_impute/new_mam_traits # 20.10% new traits imputed to family or genus


#Bird imputation
bird_genus_level_impute<-bird%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==0)

#imputed for birds
#home_range 4
#longevity 6
#sexual dim 1
#generation_time 1

#Total imputed for newly added traits to genus
impute_bird_genus_new <-4+6+1+1 # 12

#total traits imputed
genus_impute_b <-impute_bird_genus_new/new_bird_traits #.30%

#Bird imputation to family
bird_family_level_impute<-bird%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==-1)

#imputed for birds
#home_range 
#longevity 
#sexual dim 1
#generation_time 

#Total imputed for newly added traits to genus
impute_bird_family_new <-1 # 1

#total traits imputed
family_impute_b <-impute_bird_family_new/new_bird_traits #.025%

#total imputed for birds
total_bird_impute <-impute_bird_genus_new+impute_bird_family_new #13

#%imputed for mammals
total_bird_impute/new_bird_traits # .32% imputed to family or genus

#all database imputations
full_impute_b_m <- impute_bird_fam_new + impute_bird_genus_new + impute_mam_fam_new + impute_mam_genus_new #435

#all new traits for birds and mammals
all_new_traits_b_m <-new_mam_traits + new_bird_traits #6066

#percent imputed in total for new traits (birds and mammals)
full_impute_b_m/all_new_traits_b_m #= 7.17% imputed for newly added traits

#Total genus impute for new traits
genus_impute <- impute_bird_genus_new + impute_mam_genus_new #247

#total family impute for new traits 
family_impute <- impute_mam_fam_new + impute_bird_fam_new #188

#impute for EltonTraits - they include phylogenetically imputed data with a value of 2.
bird_phylo_level_impute <- bird%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==2)

#impute for bird diet
length(bird$diet_certainty_e[bird$diet_certainty_e=="D1"]) #genus level diet info, 112
length(bird$diet_certainty_e[bird$diet_certainty_e=="D2"]) #family level diet info 24


mam_phylo_level_impute <- mam%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==2)

#impute for diet
length(mam$diet_certainty_e[mam$diet_certainty_e=="D1"]) #genus level diet info, 70
length(mam$diet_certainty_e[mam$diet_certainty_e=="D2"]) #family level diet info, 22

#impute for activity
length(mam$activity_certainty_e[mam$activity_certainty_e=="D1"]) #genus level diet info, 9
length(mam$activity_certainty_e[mam$activity_certainty_e=="D2"]) #family level diet info, 68

##Total family impute for Elton Traits. They do not distinguish between genus and family level imputations
##birds
#body mass: 37 genus or fam
#for strat: 29 genus or fam
#bird diet: 136
##mam
#body mass: 58 genus or fam
#mam_diet: 92
#activity pattern: 77
#           4 phylogenetic imputations

all_elton_impute <-37 + 29 + 136 + 58 + 4 +92 +77 #433

# All new traits for birds and mammals
all_new_traits_b_m <-new_mam_traits + new_bird_traits #6,066

# Percent imputed in total for new traits (birds and mammals)
full_impute_b_m/all_new_traits_b_m #= 7.17% imputed for newly added traits

# Total # of imputations in database 
total_impute <- all_elton_impute + full_impute_b_m #868


#How many species in the database are data deficient
length(mam[mam$IUCN_category=="DD",]) #129
length(bird[bird$IUCN_category=="DD",]) #75

#mammals with data deficiency
129/312 #41.35%

#birds with data deficiency
75/682 #11%

#_____________________________________________________________________

#Figures:
#stacked barplot
library(ggplot2)

#Mammals
#pantheria %
pantheria_traits/all_mam_traits #60.92
#elton %
elton_traits_m/all_mam_traits #18.68
#new %
new_mam_traits/all_mam_traits #20.40

#birds
#elton
elton_traits_b/all_bird_traits #45.89
#new
new_bird_traits/all_bird_traits #54.11

category<- c("Mammals","Mammals","Mammals","Birds","Birds","Birds")
condition<-c("PanTHERIA","EltonTraits","New traits","PanTHERIA","EltonTraits","New traits")
percent <- c(60.92,18.68,20.40,0,45.89,54.11) # change these to the new values 
data <- data.frame(category,condition,percent)

# Stacked + percent
plot <-ggplot(data, aes(fill=condition, y=percent, x=Category)) + 
  geom_bar(position="stack", stat="identity") + geom_text(aes(label = percent),size = 3, hjust = 0.6, vjust = 3, position =  "stack")  + scale_fill_manual(values=c("#999999", "#99CC00", "#488A99"), 
                                                                                                                                                           name="Data Source",
                                                                                                                                                           breaks=c("EltonTraits","New traits", "PanTHERIA"), labels=c("EltonTraits","New Traits in Frugivoria","PanTHERIA"))+ labs(x="Taxa")  + labs(y="Percent contribution") + theme(plot.background = element_rect(fill = "white"))

plot + theme(panel.background = element_rect(fill = "white"), axis.line = element_line(colour = "dark gray", 
                                                                                       size = .5, linetype = "solid"))
