#Title: Frugivoria montane analysis demo

#Project: Montane Frugivoria

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske, Patrick Bills

#Overview: To demonstrate ways to analyze and assess what is contained within the Frugivoria database. Uses final trait datasets that include those entered through exhaustive search of the literature and online sources. 

#Data Input: Frugivoria_mammal_database_2023_full.csv, Frugivoria_bird_database_2023_full.csv

#Data Output: trait counts, imputation counts, taxonomic stats, IUCN data deficient counts

#Date: Oct 11th, 2021

#By: Beth E. Gerstner

# Load in libraries 
library(dplyr)
library(tidyr)

# Read in completed mammal database
mam <- read.csv("G:/Shared drives/SpaCE_Lab_neotropical_frugivores/Manuscripts/Database_Manuscript/Database_paper/EDI_resubmission_2023/databases_2023/Frugivoria_mammal_database_2023_full.csv")

bird<- read.csv("G:/Shared drives/SpaCE_Lab_neotropical_frugivores/Manuscripts/Database_Manuscript/Database_paper/EDI_resubmission_2023/databases_2023/Frugivoria_bird_database_2023_full.csv")

## Assessing composition of database (Elton Traits, newly added traits, PanTHERIA)
##Birds

# These are from EltonTraits. Commented out repetitive traits and kept overarching trait categories.
body_mass_b<- length(which(!is.na(bird$body_mass_e)))
diet_categ_b <- length(which(!is.na(bird$diet_cat))) 
nocturnal_b <- length(which(!is.na(bird$activity_nocturnal_e)))
for_strat_b <- length(which(!is.na(bird$for_strat_aerial_e)))
diet_inv_b <- length(which(!is.na(bird$diet_inv_e))) # % diet
#diet_vend_b <- length(which(!is.na(bird$diet_vend_e)))  
#diet_vect_b <- length(which(!is.na(bird$diet_vect_e)))   
#diet_fish_b <- length(which(!is.na(bird$diet_vfish_e)))  
#diet_vunk_b <- length(which(!is.na(bird$diet_vunk_e))) 
#diet_scav_b <- length(which(!is.na(bird$diet_scav_e)))  
#diet_fruit_b <-length(which(!is.na(bird$diet_fruit_e)))  
#diet_nect_b <-length(which(!is.na(bird$diet_nect_e)))                 
#diet_seed_b <-length(which(!is.na(bird$diet_seed_e)))                 
#diet_plant_b <-length(which(!is.na(bird$diet_plant_e)))                    

# Total # traits groups from EltonTraits
elton_traits_b <- body_mass_b + diet_categ_b + nocturnal_b + for_strat_b + diet_inv_b #5723 Did not count each of the % columns separately as this is part of a single trait (% diet makeup and % time in strata) 

# New bird traits that we have added (trait groups; excludes min and max columns for a single trait category)
longevity_b <- length(which(!is.na(bird$longevity)))
home_range_b <- length(which(!is.na(bird$home_range_size)))
habitat_special_b <- length(which(!is.na(bird$habitat_specialization)))
gen_time_b <- length(which(!is.na(bird$generation_time)))
body_size_b <- length(which(!is.na(bird$body_size_mm)))
sexual_dim_b <- length(which(!is.na(bird$sexual_dimorphism)))
range_1_b <- length(which(!is.na(bird$observed_range_sqkm)))
range_2_b <-  length(which(!is.na(bird$inferred_range_sqkm)))
habitat_breadth_b <- length(which(!is.na(bird$habitat_breadth)))
diet_breadth_b <- length(which(!is.na(bird$diet_breadth)))
avg_precip_b <- length(which(!is.na(bird$mean_CHELSA_bio12_1981.2010_V.2.1)))
avg_temp_b <- length(which(!is.na(bird$mean_CHELSA_bio1_1981.2010_V.2.1)))
avg_hf_2010_b <- length(which(!is.na(bird$mean_human_fp_range_2010)))
avg_hf_2020_b <- length(which(!is.na(bird$mean_human_fp_range_2020)))
hf_perc_change_b <- length(which(!is.na(bird$percent_change_hf_2010_2020)))

# Total # of newly added traits
new_bird_traits <-  longevity_b + habitat_special_b + gen_time_b + body_size_b + sexual_dim_b + range_1_b + range_2_b + habitat_breadth_b + diet_breadth_b +avg_precip_b + avg_temp_b + avg_hf_2010_b + avg_hf_2020_b + hf_perc_change_b #14998

# Newly added geographic traits
new_geo_traits_b <- habitat_breadth_b +avg_precip_b + avg_temp_b + avg_hf_2010_b + avg_hf_2020_b + hf_perc_change_b #6868

# Total # of bird traits
all_bird_traits <-  elton_traits_b + new_bird_traits #20,721

## Mammals
# Traits from EltonTrait dataset

diurnal_m <-length(which(!is.na(mam$activity_diurnal_e)))
crepuscular_m <-length(which(!is.na(mam$activity_crepuscular_e)))
nocturnal_m <- length(which(!is.na(mam$activity_nocturnal_e)))
body_mass_m<- length(which(!is.na(mam$body_mass_e)))
for_strat_m <- length(which(!is.na(mam$for_strat_value_e))) 
diet_inv_m <- length(which(!is.na(mam$diet_inv_e)))  #% diet will count as one trait

# Count of traits from EltonTraits
elton_traits_m <- diurnal_m + crepuscular_m + nocturnal_m + body_mass_m + for_strat_m + diet_inv_m #3630

# Count PanTHERIA traits from within the database by looking at the citations saying "PanTHERIA".
in_db_pantheria <- length(which(mam=="PanTHERIA")) #6 in database. Will remove these from the new count as they are from another included database.

#count the number of traits in PanTHERIA 
#subsetted the PanTHERIA traits from the mammal database
pantheria <- mam[,90:144]

# Removed reference column from count
pantheria$references_p <- NULL

# Count number of traits from PanTHERIA
pantheria_traits <- length(which(!is.na(pantheria))) #15,290

#count the number of newly added traits
diet_categ_m <- length(which(!is.na(mam$diet_cat)))
longevity_m <- length(which(!is.na(mam$longevity)))
home_range_m <- length(which(!is.na(mam$home_range_size)))
habitat_special_m <- length(which(!is.na(mam$habitat_specialization)))
gen_time_m <- length(which(!is.na(mam$generation_time)))
body_size_m<- length(which(!is.na(mam$body_size_mm)))
range_1_m <- length(which(!is.na(mam$observed_range_sqkm)))
range_2_m <-  length(which(!is.na(mam$inferred_range_sqkm)))
sexual_dim_m <- length(which(!is.na(mam$sexual_dimorphism)))
habitat_breadth_m <- length(which(!is.na(mam$habitat_breadth)))
diet_breadth_m<- length(which(!is.na(mam$diet_breadth)))
avg_precip_m <- length(which(!is.na(mam$mean_CHELSA_bio12_1981.2010_V.2.1)))
avg_temp_m <- length(which(!is.na(mam$mean_CHELSA_bio1_1981.2010_V.2.1)))
avg_hf_2010_m <- length(which(!is.na(mam$mean_human_fp_range_2010)))
avg_hf_2020_m <- length(which(!is.na(mam$mean_human_fp_range_2020)))
hf_perc_change_m <- length(which(!is.na(mam$percent_change_hf_2010_2020)))

# All new traits added minus those within database from PanTHERIA
new_mam_traits <- diet_categ_m + sexual_dim_m + longevity_m +home_range_m +range_1_m +range_2_m + habitat_special_m +gen_time_m +body_size_m + habitat_breadth_m + diet_breadth_m +avg_precip_m + avg_temp_m + avg_hf_2010_m + avg_hf_2020_m + hf_perc_change_m #8,709

# Newly added geographic traits
new_geo_traits_m <- habitat_breadth_m +avg_precip_m + avg_temp_m + avg_hf_2010_m + avg_hf_2020_m + hf_perc_change_m #3, 8585

# Total number of traits in the database
all_mam_traits <- new_mam_traits + pantheria_traits + elton_traits_m #27,629

# Total number of traits in the entire dataset
all_traits_b_m <-all_mam_traits + all_bird_traits #48,350

## Genera count

# count unique genera birds
length(unique(bird$genus)) #329 genera

# unique genera mammals
length(unique(mam$genus)) #160 genera

# unique mammal species 
length(unique(mam$IUCN_species_name)) #586

# unique Bird species 
length(unique(bird$IUCN_species_name)) #1147

# New species/name changes
# These are species that have either been reclassified or recently discovered. These species can be new species that we've then imputed to a close relative, species that have been reclassified, species with misspellings that we've standardized to the IUCN name. This was assessed manually and is shown in lookup_table_frugivore_mammal_species.csv & lookup_table_frugivore_mammal_species.csv

# 42 new mammal species
# 132 mammal species reclassified

# 2 new bird species
# 182 birds reclassified
# 8 Spelling errors

## Quantify mismatches between the elton_species_name and IUCN_species_name.
# Mammals

# Mismatched mammal species names
length(mam$taxonomic_disparity[mam$taxonomic_disparity==1]) #174

# Mismatched bird species names
length(bird$taxonomic_disparity[bird$taxonomic_disparity==1]) #195

  
# Traits were imputed to genus (not including EltonTraits)
mam_levels <- mam %>% select(diet_level, activity_level, body_mass_level_e, body_size_level, body_size_source, sexual_dimorphism_level, longevity_level, home_range_level, generation_time_level, habitat_level) 

mam_genus_level_impute<-mam_levels%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==0)

#imputed to genus: new traits
#home_range 77
#longevity 78
#generation_time 101
#body_size 97
#sexual dimorphism 78
#diet_level 179 #this specific level value indicates the number of species for which all diet traits were imputed. Takes into account EltonTraits imputes and those that we further imputed to close relatives. There are two new diet traits based on EltonTraits (i.e., diet_cat, diet_breadth). For 179 species, both of these new diet traits were imputed and therefore we need to multiply this value by two (358).

# total imputed for newly added traits
impute_mam_genus_new <-77+78+101+97+78+358 #789


# percent imputed in total for genus
genus_impute_m <-impute_mam_genus_new/new_mam_traits #9.05% to genus for mammals

# imputed to family: new traits
mam_family_level_impute<-mam_levels%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==-1)

#diet 32
#longevity 111
#generation_time 181
#body_size 5
#sexual dimorphism 34
#home range 55

impute_mam_fam_new <-32+111+181+5+34+55  #418

family_impute_m <-impute_mam_fam_new/new_mam_traits #4.8
# total traits imputed
total_mam_impute <-impute_mam_genus_new+impute_mam_fam_new #1207

# %imputed for mammals
total_mam_impute/new_mam_traits # 13.85% new traits imputed to family or genus

# Bird imputations
bird_levels <-  bird  %>% select(diet_level, for_strat_spec_level, body_mass_level_e, body_size_level, sexual_dimorphism_level, longevity_level, home_range_level, generation_time_level, habitat_level) 

bird_genus_level_impute<-bird_levels%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==0)

#imputed for birds
#home_range 47
#longevity 37
#sexual dim 1
#generation_time 1
#body_size 1
#diet_level 237  #this specific level value indicates the number of species for which all diet traits were imputed. Takes into account EltonTraits imputes and those that we further imputed to close relatives. There is one new diet trait based on EltonTraits (i.e., diet_breadth). For 237 species, this new diet trait was imputed.

#Total imputed for newly added traits to genus
impute_bird_genus_new <-47+37+1+1+1+237 # 324

#total traits imputed
genus_impute_b <-impute_bird_genus_new/new_bird_traits #2.16%

#Bird imputation to family
bird_family_level_impute<-bird_levels%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==-1)

#imputed for birds
#home_range 3
#longevity 75
#sexual dim 1
#diet_level 33

#Total imputed for newly added traits to family
impute_bird_family_new <-1+75+3+1+33 # 113

family_impute_b <-impute_bird_family_new/new_bird_traits # 1.12%

#total imputed for birds
total_bird_impute <-impute_bird_genus_new+impute_bird_family_new #437

#%imputed for birds
total_bird_impute/new_bird_traits # 2.91% imputed to family or genus

#all database imputations
full_impute_b_m <- impute_bird_family_new + impute_bird_genus_new + impute_mam_fam_new + impute_mam_genus_new # 1,644

#all new traits for birds and mammals
all_new_traits_b_m <-new_mam_traits + new_bird_traits #23,707

#all new imputations
new_imputations_b_m <-full_impute_b_m/all_new_traits_b_m #6.93%

##EltonTraits Imputations
#impute for EltonTraits - they include phylogenetically imputed data with a value of 2.
bird_phylo_level_impute <- bird_levels%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==2) #0

#impute for bird body mass
#imputation values
length(bird$body_mass_level_e[bird$body_mass_level_e==0]) #134 
# Total body mass imputes for birds 134

#impute for bird diet
#original imputations
length(bird$diet_certainty_e[bird$diet_certainty_e=="D1"]) #genus level diet info, 170
length(bird$diet_certainty_e[bird$diet_certainty_e=="D2"]) #family level diet info 33 

# Total imputation values, taking into account further imputes due to taxonomic changes
length(bird$diet_level[bird$diet_level==0]) #237 
length(bird$diet_level[bird$diet_level==-1]) #33
# Total diet imputes for birds 270 (multiply by two for diet_cat and  %diet)

#impute for forest strata
length(bird$for_strat_spec_level[bird$for_strat_spec_level==0]) #140 genus or family level
# Total forest strat imputes to genus or family 140

## Mammals
# impute for EltonTraits - they include phylogenetically imputed data with a value of 2
mam_phylo_level_impute <- mam_levels%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==2) #9 body mass

#impute for diet
#original imputations
length(mam$diet_certainty_e[mam$diet_certainty_e=="D1"]) #genus level diet info, 100
length(mam$diet_certainty_e[mam$diet_certainty_e=="D2"]) #family level diet info, 32
# Total imputation values, taking into account further imputes due to taxonomic changes
length(mam$diet_level[mam$diet_level==0]) #179
length(mam$diet_level[mam$diet_level==-1]) #32
# Total diet imputes for mammals 211


#impute for activity
length(mam$activity_certainty_e[mam$activity_certainty_e=="D1"]) #genus level activity, 33
length(mam$activity_certainty_e[mam$activity_certainty_e=="D2"]) #family level activity, 74
# Total imputation values, taking into account further imputes due to taxonomic changes
length(mam$activity_level[mam$activity_level==0]) #120
length(mam$activity_level[mam$activity_level==-1]) #74
# Total activity imputes for mammals 194


#impute for body mass, does not distinguish between family or genus level for mammals
length(mam$body_mass_level_e[mam$body_mass_level_e==0]) #144
length(mam$body_mass_level_e[mam$body_mass_level_e==2]) #9
# Total body mass imputes 153

# All imputations from EltonTraits (sums of totals above)
all_elton_impute <-134 + (270*2) + 140 + 179 + 211 + 194 + 153 + 9 #total EltonTrait imputes 

## PanTHERIA imputations

# Use PanTHERIA level column to subset to only species imputed to genus level 
mam_tax_disparity <-mam[mam$PanTHERIA_level==0,] #92

# subset to just PanTHERIA traits
mam_pantheria_impute_subset <- mam_tax_disparity[,95:144]

# remove reference column
mam_pantheria_impute_subset$references_p <-NULL

pantheria_impute <- length(which(!is.na(mam_pantheria_impute_subset))) #2,126 imputations

# total imputes in Frugivoria
total_impute <- all_elton_impute + full_impute_b_m + pantheria_impute #5,418

# % imputed full database
perc_impute_full_database <- total_impute/all_traits_b_m # 11.2%


## IUCN Metrics
# Data Deficiency 
#How many species in the database are data deficient
dd_m_total <-nrow(mam[mam$IUCN_category=="DD",]) #88
dd_b_total <-nrow(bird[bird$IUCN_category=="DD",]) #0

# mammals with data deficiency
dd_m_total/nrow(mam) #14.5%

# birds with data deficiency
0

# montane species data deficiency
DD_m <-nrow(mam[mam$habitat==1 & mam$IUCN_category=="DD" | mam$habitat==3 & mam$IUCN_category=="DD",]) #44
DD_b <-nrow(bird[bird$habitat==1 & bird$IUCN_category=="DD" | bird$habitat==3 & bird$IUCN_category=="DD",]) #0

# montane species critically endangered
CR_m <-nrow(mam[mam$habitat==1 & mam$IUCN_category=="CR" | mam$habitat==3 & mam$IUCN_category=="CR",]) #15
CR_b <-nrow(bird[bird$habitat==1 & bird$IUCN_category=="CR" | bird$habitat==3 & bird$IUCN_category=="CR",]) #9

# montane species endangered
EN_m <-nrow(mam[mam$habitat==1 & mam$IUCN_category=="EN" | mam$habitat==3 & mam$IUCN_category=="EN",]) #26
EN_b <-nrow(bird[bird$habitat==1 & bird$IUCN_category=="EN" | bird$habitat==3 & bird$IUCN_category=="EN",]) #26

# montane species vulnerable
VU_m <-nrow(mam[mam$habitat==1 & mam$IUCN_category=="VU" | mam$habitat==3 & mam$IUCN_category=="VU",]) #33
VU_b <-nrow(bird[bird$habitat==1 & bird$IUCN_category=="VU" | bird$habitat==3 & bird$IUCN_category=="VU",]) #58

# all threatened mammals
threatened_m <-CR_m + CR_b + EN_m + EN_b + VU_m + VU_b #167 species threatened

# total number of montane species in the database
mont_m <-nrow(mam[mam$habitat==1 | mam$habitat==3,]) #320
mont_b<-nrow(bird[bird$habitat==1 | bird$habitat==3,]) #682
all_montane <- mont_m + mont_b

# % montane species threatened
threatened_mont_perc <- threatened_m/all_montane #16.7% montane species threatened

# % montane species data deficient
DD_mont_perc<- DD_m/all_montane #4.4 of montane species data deficient


# lowland species data deficiency
DD_l_m <-nrow(mam[mam$habitat==2 & mam$IUCN_category=="DD" | mam$habitat==3 & mam$IUCN_category=="DD",]) #57
DD_l_b <-nrow(bird[bird$habitat==2 & bird$IUCN_category=="DD" | bird$habitat==3 & bird$IUCN_category=="DD",]) #0

# lowland species critically endangered
CR_l_m <-nrow(mam[mam$habitat==2 & mam$IUCN_category=="CR" | mam$habitat==3 & mam$IUCN_category=="CR",]) #13
CR_l_b <-nrow(bird[bird$habitat==2 & bird$IUCN_category=="CR" | bird$habitat==3 & bird$IUCN_category=="CR",]) #8

# lowland species endangered
EN_l_m <-nrow(mam[mam$habitat==2 & mam$IUCN_category=="EN" | mam$habitat==3 & mam$IUCN_category=="EN",]) #14
EN_l_b <-nrow(bird[bird$habitat==2 & bird$IUCN_category=="EN" | bird$habitat==3 & bird$IUCN_category=="EN",]) #24

# montane species vulnerable
VU_l_m <-nrow(mam[mam$habitat==2 & mam$IUCN_category=="VU" | mam$habitat==3 & mam$IUCN_category=="VU",]) #28
VU_l_b <-nrow(bird[bird$habitat==2 & bird$IUCN_category=="VU" | bird$habitat==3 & bird$IUCN_category=="VU",]) #47


# total number of montane species in the database
low_m <-nrow(mam[mam$habitat==2 | mam$habitat==3,]) #453
low_b<-nrow(bird[bird$habitat==2 | bird$habitat==3,]) #846
all_lowland <- low_m + low_b
threatened_low <-CR_l_m + CR_l_b + EN_l_m + EN_l_b + VU_l_m + VU_l_b #134 species threatened

# % lowland species threatened
threatened_low_perc <- threatened_low/all_lowland #10.3% lowland species threatened

# % lowland species data deficient
DD_low_perc<- DD_l_m/all_lowland #4.38 of lowland species data deficient

# %threatened for all species
all_threatened_perc <-(threatened_low + threatened_m)/(nrow(mam)+nrow(bird)) #17.2%


#_____________________________________________________________________

#% composition of Frugivoria

#Mammals
#pantheria %
pantheria_traits/all_mam_traits #55.34
#elton %
elton_traits_m/all_mam_traits #13.13
#new %
new_mam_traits/all_mam_traits #31.52

#birds
#elton
elton_traits_b/all_bird_traits #27.62
#new
new_bird_traits/all_bird_traits #72.38
