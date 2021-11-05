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

new_bird_traits <-  longevity_b + habitat_special_b + gen_time_b + body_size_b + sexual_dim_b + range_1_b + range_2_b # 4021 new traits + 1 bird range to be fixed = 4022

all_bird_traits <-  elton_traits_b + new_bird_traits #7431 traits

  #b) for mammals

#These are from previous databases (need to add Pantheria in here), try and sum NAs for x:x for Pantheria traits

diurnal_m <-length(which(!is.na(mam$activity_diurnal_e)))
crepuscular_m <-length(which(!is.na(mam$activity_crepuscular_e)))
nocturnal_m <- length(which(!is.na(mam$activity_nocturnal_e)))
body_mass_m<- length(which(!is.na(mam$body_mass_value_g_e)))
for_strat_m <- length(which(!is.na(mam$for_strat_value_e))) 
diet_inv_m <- length(which(!is.na(mam$diet_inv_e)))  #% diet will count as one trait

elton_traits_m <- diurnal_m + crepuscular_m + nocturnal_m + body_mass_m + for_strat_m + diet_inv_m #1878

#count Pantheria traits from within the database by looking at the citations saying "PanTHERIA".
in_db_pantheria <- length(which(mam=="PanTHERIA")) #6 in database. Should remove these from the new count.

#count the number of traits in PanTHERIA but remove the reference column
pantheria <- mam[,79:128]
pantheria$references_p <- NULL
pantheria_traits <- length(which(!is.na(pantheria))) #6076

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
new_mam_traits <- diet_categ_m + longevity_m +home_range_m +range_1_m +range_2_m + habitat_special_m +gen_time_m +body_size_m -6 + 3 #2058-6+3= 2055

#Total number of traits in the database
all_mam_traits <- new_mam_traits + pantheria_traits + elton_traits_m #10,009

#Total number of traits in the entire dataset
all_mam_traits + all_bird_traits #17,440
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

  
#5) How many traits were imputed to family? (not including EltonTraits, will have to add this later on)
library (tidyr)
mam_genus_level_impute<-mam%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==0)

#imputed to genus: new traits
#home_range 27
#longevity 41
#generation_time 45
#body_size 40
#sexual dimorphism 62
#diet_cat #24 new species. need to know how many were reclassified to figure out what's imputed to genus

#total imputed for newly added traits
impute_mam_genus_new <-27+41+45+40+62+24 # 239 


#percent imputed in total for genus
genus_impute_m <-impute_mam_genus_new/new_mam_traits #11.63% to genus for mammals

##imputed to family: new traits

#longevity 50
#generation_time 91
#body_size 3
#sexual dimorphism 31

mam_fam_level_impute<-mam%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==-1)

impute_mam_fam_new <-50+91+3+31 # 175

#total traits imputed
total_mam_impute <-impute_mam_genus_new+impute_mam_fam_new #414

#%imputed for mammals
total_mam_impute/new_mam_traits # 20.15% new traits imputed to family or genus


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
full_impute_b_m <- impute_bird_fam_new + impute_bird_genus_new + impute_mam_fam_new + impute_mam_genus_new

#all new traits for birds and mammals
all_new_traits_b_m <-new_mam_traits + new_bird_traits

#percent imputed in total for new traits (birds and mammals)
full_impute_b_m/all_new_traits_b_m #= 7.16% imputed for newly added traits


#imputed Pantheria and Elton Traits?
  
#Results (all of these will be mock ups for now (IBS figures):
#1) #stacked barplot of newly added traits (those on top of Elton Traits/PanTheria)
#2) #show a single mapped trait as an example of something you can do
  
  
#How many species in the database are data deficient
length(mam[mam$IUCN_category=="DD",]) #131
length(bird[bird$IUCN_category=="DD",]) #75

#mammals with data deficiency
131/313 #41.85%

#birds with data deficiency
75/682 #11%

#_____________________________________________________________________
  
#Figures:
#stacked barplot
library(ggplot2)

#Mammals
#pantheria %
pantheria_traits/all_mam_traits #60.7
#elton %
elton_traits_m/all_mam_traits #18.76
#new %
new_mam_traits/all_mam_traits #20.53

#birds
#elton
elton_traits_b/all_bird_traits #45.89
#new
new_bird_traits/all_bird_traits #54.11

category<- c("Mammals","Mammals","Mammals","Birds","Birds","Birds")
condition<-c("PanTHERIA","EltonTraits","New traits","PanTHERIA","EltonTraits","New traits")
percent <- c(60.7,18.76,20.53,0,45.89,54.11) # change these to the new values 
data <- data.frame(category,condition,percent)

# Stacked + percent
plot <-ggplot(data, aes(fill=condition, y=percent, x=Category)) + 
  geom_bar(position="stack", stat="identity") + geom_text(aes(label = percent),size = 3, hjust = 0.6, vjust = 3, position =  "stack")  + scale_fill_manual(values=c("#999999", "#99CC00", "#488A99"), 
                                                                                                                                                           name="Data Source",
                                                                                                                                                           breaks=c("EltonTraits","New traits", "PanTHERIA"), labels=c("EltonTraits","New Traits","PanTHERIA"))+ labs(x="Taxa")  + labs(y="Percent contribution") + theme(plot.background = element_rect(fill = "white"))

plot + theme(panel.background = element_rect(fill = "white"), axis.line = element_line(colour = "dark gray", 
                                                                                       size = .5, linetype = "solid"))

#Trait summary
traits_test <- read.csv("/Users/bethgerstner/Desktop/graph_test.csv")


ggplot(traits_test, aes(x=trait, y=species_., fill=taxa)) + 
  geom_bar(stat="identity", position=position_dodge())


#Bird Traits 
library(dplyr)

bird_df <- data.frame(longevity_b,home_range_b,range_1_b, habitat_special_b, gen_time_b, body_size_b,sexual_dim_b)

oldnames = c("longevity_b", "home_range_b","range_1_b","habitat_special_b", "gen_time_b", "body_size_b", "sexual_dim_b")
newnames = c("longevity", "home_range","range_size","habitat_special", "gen_time", "body_size", "sexual_dim")

bird_df_wide <-bird_df %>% rename_at(vars(oldnames), ~ newnames)

#Mammals wide

mam_df <- data.frame(longevity_m,home_range_m,range_1_m, habitat_special_m, gen_time_m, body_size_m,sexual_dim_m)


oldnames.m = c("longevity_m", "home_range_m","range_1_m","habitat_special_m", "gen_time_m", "body_size_m", "sexual_dim_m")
newnames.m = c("longevity", "home_range","range_size","habitat_special", "gen_time", "body_size", "sexual_dim")

mam_df_wide <-mam_df %>% rename_at(vars(oldnames.m), ~ newnames.m)

full_wide_trait_db <- rbind(mam_df_wide, bird_df_wide)

#convert this from wide to long format so we can put into ggplot2
library(tidyr)
full_long_trait_db <-gather(full_wide_trait_db,trait, species)
full_long_trait_db$taxa <-c("mammals", "birds", "mammals", "birds","mammals", "birds","mammals", "birds","mammals", "birds","mammals", "birds","mammals", "birds")

full_long_trait_db$trait <-as.factor(full_long_trait_db$trait)
library(ggplot2)
all_new_traits <-ggplot(full_long_trait_db, aes(x=trait, y=species, fill=taxa)) + labs(x = "New Frugivoria traits", y = "# species with trait") +
  geom_bar(stat="identity", position=position_dodge()) +geom_hline(yintercept=313, color="lightseagreen") + geom_hline(yintercept=682, color="lightcoral")

all_new_traits + scale_x_discrete(labels = c("body size", "gen time", "habitat special","home range","longevity", "range size", "sexual dim"))
