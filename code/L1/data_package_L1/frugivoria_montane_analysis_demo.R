#Project: Montane Frugivoria - analyzing the montane database 

#Purpose: To analyze and assess what is in the montane frugivore database for the data paper. Uses final outputs of script "final_database_edits"

#Code reference: database_analyses

#Date: Oct 11th, 2021

#By: Beth E. Gerstner

#read in completed mammal database
mam <- read.csv("INSERT PATH HERE")
#read in completed bird database
bird<- read.csv("INSERT PATH HERE")


## Assessing composition of database (Elton Traits, newly added traits, PanTHERIA)
##Birds

# These are from EltonTraits. Commented out repetitive traits.
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

#Total # traits from EltonTraits
elton_traits_b <- body_mass_b + diet_categ_b + nocturnal_b + for_strat_b + diet_inv_b #. Did not count each of the % columns seperately as this is part of a single trait (% diet makeup and % time in strata)

# New bird traits that we have added
longevity_b <- length(which(!is.na(bird$longevity)))
home_range_b <- length(which(!is.na(bird$home_range_size)))
habitat_special_b <- length(which(!is.na(bird$habitat_specialization)))
gen_time_b <- length(which(!is.na(bird$generation_time)))
body_size_b <- length(which(!is.na(bird$body_size_cm)))
sexual_dim_b <- length(which(!is.na(bird$sexual_dimorphism)))
range_1_b <- length(which(!is.na(bird$observed_range_sqkm)))
range_2_b <-  length(which(!is.na(bird$inferred_range_sqkm)))

# Total # of newly added traits
new_bird_traits <-  longevity_b + habitat_special_b + gen_time_b + body_size_b + sexual_dim_b + range_1_b + range_2_b


# Total # of bird traits
all_bird_traits <-  elton_traits_b + new_bird_traits

## Mammals

# Traits from EltonTrait dataset

diurnal_m <-length(which(!is.na(mam$activity_diurnal_e)))
crepuscular_m <-length(which(!is.na(mam$activity_crepuscular_e)))
nocturnal_m <- length(which(!is.na(mam$activity_nocturnal_e)))
body_mass_m<- length(which(!is.na(mam$body_mass_value_g_e)))
for_strat_m <- length(which(!is.na(mam$for_strat_value_e))) 
diet_inv_m <- length(which(!is.na(mam$diet_inv_e)))  #% diet will count as one trait

# Count of traits from EltonTraits
elton_traits_m <- diurnal_m + crepuscular_m + nocturnal_m + body_mass_m + for_strat_m + diet_inv_m

# Count PanTHERIA traits from within the database by looking at the citations saying "PanTHERIA".
in_db_pantheria <- length(which(mam=="PanTHERIA")) #6 in database. Will remove these from the new count as they are from another included database.

#count the number of traits in PanTHERIA 
#subsetted the PanTHERIA traits from the mammal database
pantheria <- mam[,79:128]

# Removed reference column from count
pantheria$references_p <- NULL

# Count number of traits from PanTHERIA
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

# All new traits added minus those within database from PanTHERIA
new_mam_traits <- diet_categ_m + longevity_m +home_range_m +range_1_m +range_2_m + habitat_special_m +gen_time_m +body_size_m -6

#Total number of traits in the database
all_mam_traits <- new_mam_traits + pantheria_traits + elton_traits_m

#Total number of traits in the entire dataset
all_mam_traits + all_bird_traits

#Genera count

library(tidyr)

# count unique genera birds
length(unique(bird$genus)) 

# count unique genera mammals
length(unique(mam$genus))

# Unique mammal species 
length(unique(mam$IUCN_species_name))

# Unique Bird species 
length(unique(bird$IUCN_species_name))

#New species/name changes
#These are species that have either been reclassified or recently discovered. These species can be new species that we've then imputed to a close relative, species that have been reclassified, species with misspellings that we've standardized to the IUCN name. This was assessed manually.

# Quantify mismatches between the elton_species_name and IUCN_species_name.
# Mammals

# Mismatched mammal species names
length(mam$taxonomic_disparity[mam$taxonomic_disparity==1])

# Mismatched bird species names
length(bird$taxonomic_disparity[bird$taxonomic_disparity==1])

  
# Traits were imputed to genus (not including EltonTraits)
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
#diet_cat #24 new species. 

# Total imputed for newly added traits
impute_mam_genus_new <-27+41+45+40+62+24 # 239 


# Percent imputed in total for genus
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

# Total imputed for newly added traits to genus
impute_bird_genus_new <-4+6+1+1 # 12

# Percent traits imputed to genus
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

# Total imputed for newly added traits to genus
impute_bird_family_new <-1 # 1

# Percent traits imputed to family
family_impute_b <-impute_bird_family_new/new_bird_traits #.025%

# Total imputed for birds
total_bird_impute <-impute_bird_genus_new+impute_bird_family_new #13

# Percent imputed for birds
total_bird_impute/new_bird_traits # .32% imputed to family and genus

# All database imputations
full_impute_b_m <- impute_bird_fam_new + impute_bird_genus_new + impute_mam_fam_new + impute_mam_genus_new

# All new traits for birds and mammals
all_new_traits_b_m <-new_mam_traits + new_bird_traits

# Percent imputed in total for new traits (birds and mammals)
full_impute_b_m/all_new_traits_b_m #= 7.16% imputed for newly added traits


# Species in the database that are data deficient (DD) in IUCN
length(mam[mam$IUCN_category=="DD",]) #131
length(bird[bird$IUCN_category=="DD",]) #75

#mammals with data deficiency
131/313 #41.85%

#birds with data deficiency
75/682 #11%

#Total % makeup of trait database
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

