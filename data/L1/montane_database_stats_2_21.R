#Project: Montane Frugivoria - analyzing the montane database 

#Purpose: To analyze and assess what is in the montane frugivore database for the data paper.

#Date: Feb 10th, 2021, Updated: April 2021

#By: Beth E. Gerstner

#read in mammal database
mam  <- read.csv("/Users/bethgerstner/Desktop/montane_mammal_database_checks - complete_montane_mammal_databas.csv")

#read in bird database
#need to join first
bird <-read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/L1/working_databases/database_csv_2_9_21/pantheria_added_mammals/final_datasets_tofix/complete_montane_bird_database_4_13_21.csv")


#inset NAs for missing values
mam[mam==" "]<-NA
bird[bird==""] <-NA


#Needed stats
#1) how many new traits in total
  #a) for birds

#These are from previous databases

#Elton Traits database
body_mass_b<- length(which(!is.na(bird$body_mass_value)))
diet_categ_b <- length(which(!is.na(bird$diet_cat))) 
nocturnal_b <- length(which(!is.na(bird$activity_nocturnal)))
for_strat_b <-  length(which(!is.na(mam[,25:29])))
per_diet_b <- length(which(!is.na(mam[, 12:21])))

elton_traits_birds <- body_mass_b + diet_categ_b + nocturnal_b + for_strat_b + per_diet_b # 6746

longevity_b <- length(which(!is.na(bird$longevity)))
home_range_b <- length(which(!is.na(bird$home_range_size)))
range_size_b <-length(which(!is.na(bird$range_size)))
habitat_special_b <- length(which(!is.na(bird$habitat_specialization)))
gen_time_b <- length(which(!is.na(bird$generation_time)))
body_size_b<- length(which(!is.na(bird$body_size)))
longevity_min_b <- length(which(!is.na(bird$longevity_min)))
home_range_min_b <- length(which(!is.na(bird$home_range_size_min)))
body_size_min_b<- length(which(!is.na(bird$min_body_size)))
sexual_dim_b  <- length(which(!is.na(bird$sexual_dimorphism)))


new_bird_traits <-  longevity_b + home_range_b +range_size_b + habitat_special_b + gen_time_b +body_size_b + longevity_min_b + home_range_min_b + body_size_min_b +sexual_dim_b #3029 new traits

all_bird_traits <-  new_bird_traits + elton_traits_birds #9775 traits

  #b) for mammals

#Elton Traits
diurnal_m <-length(which(!is.na(mam$activity_diurnal)))
crepuscular_m <-length(which(!is.na(mam$activity_crepuscular)))
nocturnal_m <- length(which(!is.na(mam$activity_nocturnal)))
body_mass_m<- length(which(!is.na(mam$body_mass_value_g_elton)))
ForStrat_m <- length(which(!is.na(mam$for_strat_value))) 
perc_diet_m <-length(which(!is.na(mam[,10:19])))

elton_traits_mammals <- diurnal_m + crepuscular_m +nocturnal_m + body_mass_m + ForStrat_m + perc_diet_m #4770 Elton Traits


#sum all pantheria columns 
is.na(mam) <- mam == "-999"
mam <-mam[,-c(114)]

pantheria_traits <- length(which(!is.na(mam[,84:132]))) # 5078 Pantheria traits

diet_categ_m <- length(which(!is.na(mam$diet_cat)))
longevity_m <- length(which(!is.na(mam$longevity)))
longevity_min_m <- length(which(!is.na(mam$min_longevity)))
home_range_m <- length(which(!is.na(mam$home_range_size)))
home_range_min_m <- length(which(!is.na(mam$home_range_min)))
range_size_m <-length(which(!is.na(mam$range_size_level.))) #this will change in the future
habitat_special_m <- length(which(!is.na(mam$habitat_specialization)))
gen_time_m <- length(which(!is.na(mam$generation_time)))
gen_time_min_m <- length(which(!is.na(mam$min_generation_time)))
body_size_m<- length(which(!is.na(mam$body_size_cm)))
body_size_min_m<- length(which(!is.na(mam$min_body_size_cm)))
sexual_dim_m <-length(which(!is.na(mam$sexual_dimorphism)))


new_mam_traits <- diet_categ_m + longevity_m + longevity_min_m +home_range_m +range_size_m + habitat_special_m +gen_time_m + gen_time_min_m +body_size_m + body_size_min_m + sexual_dim_m #2297

#Total number of traits in the mammal database thus far (will change)
all_mammal_traits <- elton_traits_mammals + pantheria_traits + new_mam_traits # 12145 traits in total (need to figure out what to do with overlapping trait values; some are included in all of these external databases)

#Traits in entire database
all_mammal_traits + all_bird_traits

#2) # of genera
  #a) birds 
  #b) mammals
#Unique generay

library(tidyr)

length(unique(mam$genus)) #96 genera for mammals 

# 227 genera for birds

length(unique(bird$genus)) #223 genera for birds

#Unique mammal species 
length(unique(mam$IUCN_species_name)) #318

#Unique Bird species 
length(unique(bird$IUCN_species_name)) #686

#3) #New species/name changes
#New species (15 mammals, 230 birds) - have either been reclassified or recently discovered <-- these are the original numbers. I need to write code that can quantify name changes. It should look at mismatches between the IUCN_name and the scientific_name (elton traits). These species can be new species that we've then imputed to a close relative, species that have been reclassified, species with mispellings that we've standardized to the IUCN name. This was done in Excel due to each species needing to be manually looked up.

#Birds
#2 new birds
#261 reclassified
#9 alternate spellings

#mammals
#24 new mammals
#61 reclassified

# #Need to quanitify mismatches between the scientific and IUCN_name. Need to update this when I fix the scientific name column. Students didn't fill this in completely.
# #mammals
# mam$new <- ifelse(mam$IUCN_species_name == mam$P_scientific_name, '0',
#                      ifelse(mam$IUCN_species_name != mam$P_scientific_name, '1','NA'))
# 
# #how many species names are mismatched?
# length(mam$new[mam$new==1]) #230
# 
# #birds
# bird$new <- ifelse(bird$IUCN_species_name == bird$scientific_name, '0',
#                   ifelse(bird$IUCN_species_name != bird$scientific_name, '1','NA'))
# 
# #how many species names are mismatched?
# length(bird$new[bird$new==1]) #307

  
#5) How many traits were imputed to family? (not including EltonTraits, will have to add this later on)
library(dplyr)
mam_genus_level_impute<-mam%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==0)

#imputed
#home_range 34
#longevity 35
#generation_time 58
#body_size 54
#body mass 53
#sexual_dimorphism 15

#total imputed for newly added traits
34+35+58+54+15 = 196

#total with values

longevity_l <- length(which(!is.na(mam$longevity_level))) #135
home_range_l <- length(which(!is.na(mam$home_range_level))) #88
gen_time_l <- length(which(!is.na(mam$generation_time_level))) #192
body_size_l<- length(which(!is.na(mam$body_size_level))) #300
body_mass_l <- length(which(!is.na(mam$body_mass_spec_level_elton))) #317
sexual_dim_l <- length(which(!is.na(mam$sexual_dimorphism_level))) #100

#added traits with values not NA
total_species_level_traits_mam <-longevity_l +home_range_l +gen_time_l +body_size_l +body_mass_l +sexual_dim_l #1132

#percent imputed in total
196/total_species_level_traits_mam #= 17.3% for mammals

#Bird imputation
bird_genus_level_impute<-bird%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==0)

#imputed for birds
#home_range 4
#longevity 5
#generation_time 1
#body mass 32



#total imputed for newly added traits birds
32+1+5+4 #42

#total with values

longevity_lb <- length(which(!is.na(bird$longevity))) #52
home_range_lb <- length(which(!is.na(bird$home_range_level))) #78
gen_time_lb <- length(which(!is.na(bird$generation_time_level))) #529
body_size_lb<- length(which(!is.na(bird$body_size))) #263
body_mass_lb <- length(which(!is.na(bird$body_mass_spec_level))) #673
sexual_dim_lb <-length(which(!is.na(bird$sexual_dimorphism))) #163

#added traits with values not NA (this value will change. Some species are missing level values)
52+78+529+263+673+163# = 1748

#percent imputed in total
42/1748# = 2.4% imputed

#Total newly added traits at species level
1748 + 1132 # = 2880 newly added traits without NA values

#Imputed in total (will change)
196+42 #= 238(total imputed)
238/2880 #(imputed/total new trait) = 8.2% 


#imputed Pantheria and Elton Traits?
  
#Results (all of these will be mock ups for now (IBS figures):
#1) #stacked barplot of newly added traits (those on top of Elton Traits/PanTheria)
#2) #show a single mapped trait as an example of something you can do

#test : stacked barplot
library(ggplot2)

category<- c("Mammals","Mammals","Mammals","Birds","Birds","Birds")
condition<-c("Pantheria","Elton Traits","New traits","Pantheria","Elton Traits","New traits")
percent <- c(41.8,39.2,18.9,0,69,31.0) # change these to the new values 
data <- data.frame(category,condition,percent)

# Stacked + percent
plot <-ggplot(data, aes(fill=condition, y=percent, x=Category)) + 
  geom_bar(position="stack", stat="identity") + geom_text(aes(label = percent),size = 3, hjust = 0.6, vjust = 3, position =  "stack")  + scale_fill_manual(values=c("#999999", "#99CC00", "#488A99"), 
                                                                                                                                                              name="Data Source",
                                                                                                                                                              breaks=c("Elton Traits","New traits", "Pantheria"), labels=c("Elton Traits","New Traits","Pantheria"))+ labs(x="Taxa")  + labs(y="Percent contribution") + theme(plot.background = element_rect(fill = "white"))

plot + theme(panel.background = element_rect(fill = "white"), axis.line = element_line(colour = "dark gray", 
                                                                                      size = .5, linetype = "solid"))
  
#Trait summary
traits_test <- read.csv("/Users/bethgerstner/Desktop/graph_test.csv")


ggplot(traits_test, aes(x=trait, y=species_., fill=taxa)) + 
  geom_bar(stat="identity", position=position_dodge())


#Bird Traits 
library(dplyr)
home_range_b_full <-home_range_b+home_range_min_b
longevity_b_full <-longevity_b + longevity_min_b
body_size_b_full <- body_size_b + body_size_min_b
bird_df <- data.frame(longevity_b_full,home_range_b,range_size_b, habitat_special_b, gen_time_b, body_size_b_full,sexual_dim_b)

oldnames = c("longevity_b_full", "home_range_b","range_size_b","habitat_special_b", "gen_time_b", "body_size_b_full", "sexual_dim_b")
newnames = c("longevity", "home_range","range_size","habitat_special", "gen_time", "body_size", "sexual_dim")

bird_df_wide <-bird_df %>% rename_at(vars(oldnames), ~ newnames)

#Mammals wide
home_range_m_full <-home_range_m+home_range_min_m
longevity_m_full <-longevity_m + longevity_min_m
body_size_m_full <- body_size_m
mam_df <- data.frame(longevity_m_full,home_range_m,range_size_m, habitat_special_m, gen_time_m, body_size_m_full,sexual_dim_m)


oldnames.m = c("longevity_m_full", "home_range_m","range_size_m","habitat_special_m", "gen_time_m", "body_size_m_full", "sexual_dim_m")
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
  geom_bar(stat="identity", position=position_dodge()) +geom_hline(yintercept=318) + geom_hline(yintercept=686)

all_new_traits + scale_x_discrete(labels = c("body size", "gen time", "habitat special","home range","longevity", "range size", "sexual dim"))



#_____________________________________________________________________
  

