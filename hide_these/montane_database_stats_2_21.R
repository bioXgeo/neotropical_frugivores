#Project: Montane Frugivoria - analyzing the montane database 

#Purpose: To analyze and assess what is in the montane frugivore database for the data paper.

#Date: Feb 10th, 2021

#By: Beth E. Gerstner

#read in mammal database
mam <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/L1/working_databases/database_csv_2_9_21/pantheria_added_mammals/FINAL_mammal_dataset_edited.csv")
#read in bird database

#need to join first (newly added species with the original dataset)
bird1 <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/L1/working_databases/database_csv_2_9_21/newly_added_montane_birds.csv")
bird2 <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/L1/working_databases/database_csv_2_9_21/original_montane_birds_2_10_2021.csv")
library(gtools) # For the smartbind function (was having strange issue where I couldn't use rbind for the two bird datasets I was trying to combine. This seemed to work fine. It transformed the data types so that they matched and could be combined.)
bird <- smartbind(bird1, bird2)
write.csv(bird, "/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/L1/working_databases/database_csv_2_9_21/pantheria_added_mammals/FINAL_bird_dataset_edited.csv")

#bird <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/L1/working_databases/database_csv_2_9_21/pantheria_added_mammals/FINAL_bird_dataset_edited.csv")

#inset NAs for missing values
mam[mam==" "]<-NA
bird[bird==""] <-NA

#Needed stats
#1) how many new traits in total
  #a) for birds

#These are from previous databases (need to add Pantheria in here)
body_mass_b<- length(which(!is.na(bird$Body_mass_value)))
diet_categ_b <- length(which(!is.na(bird$Diet_cat))) 
nocturnal_b <- length(which(!is.na(bird$Activity_nocturnal)))


longevity_b <- length(which(!is.na(bird$longevity)))
home_range_b <- length(which(!is.na(bird$home_range_size)))
range_size_b <-length(which(!is.na(bird$range_size)))
habitat_special_b <- length(which(!is.na(bird$habitat_specialization)))
gen_time_b <- length(which(!is.na(bird$generation_time)))
body_size_b<- length(which(!is.na(bird$body_size)))

new_bird_traits <-  longevity_b + home_range_b +range_size_b + habitat_special_b + gen_time_b +body_size_b #4235 new traits

all_bird_traits <-  body_mass_b+ diet_categ_b + nocturnal_b +longevity_b + home_range_b +range_size_b + habitat_special_b + gen_time_b +body_size_b #6623 traits

  #b) for mammals

#These are from previous databases (need to add Pantheria in here)
#diurnal_m <-length(which(!is.na(mam$activity_diurnal)))
#crepuscular_m <-length(which(!is.na(mam$activity_crepuscular)))
#nocturnal_m <- length(which(!is.na(mam$nocturnal)))
#body_mass_m<- length(which(!is.na(mam$body_mass_value..g.)))
#ForStrat_m <- length(which(!is.na(mam$For_strat_value))) # 2414 in total without Pantheria traits

diet_categ_m <- length(which(!is.na(mam$Diet_cat)))
longevity_m <- length(which(!is.na(mam$longevity)))
home_range_m <- length(which(!is.na(mam$home_range_size)))
range_size_m <-length(which(!is.na(mam$home_range_size.1)))
habitat_special_m <- length(which(!is.na(mam$habitat_specialization)))
gen_time_m <- length(which(!is.na(mam$generation_time)))
body_size_m<- length(which(!is.na(mam$body_size_cm)))

new_mam_traits <- diet_categ_m + longevity_m +home_range_m +range_size_m + habitat_special_m +gen_time_m +body_size_m  #1134

#Total number of traits in the database thus far (will change)
2414+6623
  
#2) # of genera
  a) birds 
  b) mammals
#Unique genera
#71 genera for mammals

library(tidyr)
#code to separate one column into many
scientific_names_mammals <-separate(mam, IUCN_species_name, into = c("genus", "species"), sep= " (?=[^ ]+$)")

length(unique(scientific_names_mammals$genus)) #103 genera

# genera for birds
scientific_names_birds <-separate(bird, IUCN_species_name, into = c("genus", "species"), sep= " (?=[^ ]+$)")

length(unique(scientific_names_birds$genus)) #295 genera

#Unique mammal species 
length(unique(mam$IUCN_species_name)) #316

#Unique Bird species 
length(unique(bird$IUCN_species_name)) #850

#3) #New species/name changes
#New species (15 mammals, 230 birds) - have either been reclassified or recently discovered <-- these are the original numbers. I need to write code that can quantify name changes. It should look at mismatches between the IUCN_name and the scientific_name (EltonTraits). These species can be new species that we've then imputed to a close relative, species that have been reclassified, species with misspellings that we've standardized to the IUCN name.

#Need to quantify mismatches between the scientific and IUCN_name. Need to update this when I fix the scientific name column. Students didn't fill this in completely.
#mammals
mam$new <- ifelse(mam$IUCN_species_name == mam$P_scientific_name, '0',
                     ifelse(mam$IUCN_species_name != mam$P_scientific_name, '1','NA'))

#how many species names are mismatched?
length(mam$new[mam$new==1]) #110

#birds
bird$new <- ifelse(bird$IUCN_species_name == bird$scientific_name, '0',
                  ifelse(bird$IUCN_species_name != bird$scientific_name, '1','NA'))

#how many species names are mismatched?
length(bird$new[bird$new==1]) #307

  
#5) How many traits were imputed to family? (not including EltonTraits, will have to add this later on)
library (tidyr)
mam_genus_level_impute<-mam%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==0)

#imputed
#home_range 20
#longevity 26
#generation_time 55
#body_size 43
#body mass 53

#total imputed for newly added traits
#20+26+55+43+53 = 197

#total with values

longevity_l <- length(which(!is.na(mam$longevity_level))) #115
home_range_l <- length(which(!is.na(mam$home_range_level))) #70
gen_time_l <- length(which(!is.na(mam$generationtime_level))) #168
body_size_l<- length(which(!is.na(mam$body_size_level))) #275
body_mass_l <- length(which(!is.na(mam$body_mass_spec_level))) #299

#added traits with values not NA
#115+70+168+275+299 = 927

#percent imputed in total
#197/927 = 21.3% for mammals

#Bird imputation
bird_genus_level_impute<-bird%>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value==0)

#imputed for birds
#home_range 3
#longevity 1
#generation_time 2
#body mass 32


#total imputed for newly added traits
#38

#total with values

longevity_lb <- length(which(!is.na(bird$long_level))) #186
home_range_lb <- length(which(!is.na(bird$home_range_level))) #71
gen_time_lb <- length(which(!is.na(bird$generation_time_level))) #679
body_size_lb<- length(which(!is.na(bird$body_size))) #859
body_mass_lb <- length(which(!is.na(mam$body_mass_spec_level))) #299

#added traits with values not NA (this value will change. Some species are missing level values)
#186+71+679+859+299 = 2094

#percent imputed in total
#38/2094 = 2% imputed

#Total database imputed
#2094 + 927 = 3021 newly added traits without NA values

#Imputed in total (will change)
#197+38 = 235 (total imputed)
#235/3021 (imputed/total new trait) = 7.8% 


#imputed Pantheria and Elton Traits?
  
#Results (all of these will be mock ups for now (IBS figures):
#1) #stacked barplot of newly added traits (those on top of Elton Traits/PanTheria)
#2) #show a single mapped trait as an example of something you can do
  
  
#How many species in the database are data deficient
length(mam[mam$IUCN_category=="DD",]) #104
length(bird[bird$IUCN_category=="DD",]) #71

#mammals with data deficiency
104/316 #33%

#birds with data deficiency
71/850 #8.4%

#_____________________________________________________________________
  
  
  
  
  
  
  
  
# Mammals
# Read in old database
old_df_m <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Mammals_Birds_all_data/Trait_Database_2019/IUCN_trait_montane_mammals_original.csv")
# check the length of each trait of interest for mammals 
#old database
#diurnal_m <-length(which(!is.na(old_df_m$Activity.Diurnal)))
#crepuscular_m <-length(which(!is.na(old_df_m$Activity.Crepuscular)))
body_mass_m<- length(which(!is.na(old_df_m$BodyMass.Value)))
ForStrat_m <- length(which(!is.na(old_df_m$ForStrat.Value)))
diet_categ_m <- c(0)
longevity_m <- c(0)
home_range_m <- c(0)
range_size_m <- c(0)
habitat_special_m <- c(0)
gen_time_m <- c(0)

#new database
diurnal_mn <-length(which(!is.na(dat$Activity.Diurnal)))
crepuscular_mn <-length(which(!is.na(dat$Activity.Crepuscular)))
body_mass_mn<- length(which(!is.na(dat$BodyMass.Value)))
ForStrat_mn <- length(which(!is.na(dat$ForStrat.Value)))
diet_categ_mn <- length(which(!is.na(dat$Diet.Cat)))
longevity_mn <- length(which(!is.na(dat$longevity)))
home_range_mn <- length(which(!is.na(dat$home_range_size)))
range_size_mn <-length(which(!is.na(dat$range_size)))
habitat_special_mn <- length(which(!is.na(dat$habitat_specialization)))
gen_time_mn <- length(which(!is.na(dat$generation_time)))
body_size_mn<- length(which(!is.na(dat$body_size)))

traits_m_old<-c(diurnal_m, crepuscular_m, body_mass_m, ForStrat_m, diet_categ_m,longevity_m, home_range_m, range_size_m, habitat_special_m,gen_time_m)
traits_m_recent<-c(diurnal_mn, crepuscular_mn, body_mass_mn, ForStrat_mn, diet_categ_mn,longevity_mn, home_range_mn, range_size_mn, habitat_special_mn,gen_time_mn)
difference <- traits_m_recent - traits_m_old

# Birds
# Read in old database


old_df_b <- read.csv("/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Mammals_Birds_all_data/Trait_Database_2019/IUCN_trait_montane_birds_final.csv")
new_df_b <- read.csv("/Volumes/GoogleDrive/My Drive/neotropical_frugivores/Database2019/Databases/L1/IUCN_trait_montane_birds_species_list.csv")
# check the length of each treat of interest for mammals 
#old database
diurnal_b <-length(which(!is.na(new_df_b$Activity.Diurnal)))
crepuscular_b <-length(which(!is.na(new_df_b$Activity.Crepuscular)))
body_mass_b<- length(which(!is.na(old_df_b$BodyMass.Value)))
ForStrat_b <- length(which(!is.na(old_df_b$ForStrat.canopy)))
diet_categ_b <- length(which(!is.na(old_df_b$Diet.5Cat)))
diet_categ_b <- length(which(!is.na(old_df_b$Diet.5Cat)))
longevity_b <- c(0)
home_range_b <- c(0)
range_size_b <- c(0)
habitat_special_b <- c(0)
gen_time_b <- c(0)

#new database
diurnal_bn <-length(which(!is.na(new_df_b$Activity.Diurnal)))
crepuscular_bn <-length(which(!is.na(new_df_b$Activity.Crepuscular)))
body_mass_bn<- length(which(!is.na(new_df_b$BodyMass.Value)))
ForStrat_bn <- length(which(!is.na(new_df_b$ForStrat.Value)))
diet_categ_bn <- length(which(!is.na(new_df_b$Diet.Cat)))
longevity_bn <- length(which(!is.na(new_df_b$longevity)))
home_range_bn <- length(which(!is.na(new_df_b$home_range_size)))
range_size_bn <-length(which(!is.na(new_df_b$range_size)))
habitat_special_bn <- length(which(!is.na(new_df_b$habitat_specialization)))
gen_time_bn <- length(which(!is.na(new_df_b$generation_time)))
body_size_bn<- length(which(!is.na(new_df_b$body_size)))

traits_b_old<-c(diurnal_b, crepuscular_b, body_mass_b, ForStrat_b, diet_categ_b,longevity_b, home_range_b, range_size_b, habitat_special_b,gen_time_b)
traits_b_recent<-c(diurnal_bn, crepuscular_bn, body_mass_bn, ForStrat_bn, diet_categ_bn,longevity_bn, home_range_bn, range_size_bn, habitat_special_bn,gen_time_bn)
difference_b <- traits_b_recent - traits_b_old

trait_matrix_m <-matrix(c(traits_m_old,difference),nrow=2, byrow=T)
trait_matrix_m <-matrix(c(traits_m_old,traits_m_recent),nrow=2, byrow=T)
barplot(trait_matrix_m, beside=T,main="Number of species/diet category", ylab="Species Count", cex.lab=1,cex.names=1, col=c("red","blue"), legend = c("Frugivore","Frug/Herb","Frug/Insect","Frug/Nect","Frug/Omni","Herbivore","Herb/Frug","Insectivore","Insect/Frug","Insect/Herb","Insect/Omni","Invertebrate","Nectar/Frug","Omnivore"), args.legend=list(title="Diet Categories", cex=.8),ylim=c(0,200),xlim=c(0,23))

#Make a pie chart birds

ratios <- c(3126,2094)
names(ratios) <-c("EltonTraits (59.8%)","Traits Added (40.1%)")
Bird_ratio <- pie(ratios, col=c("cornsilk", "cyan"),  main="Bird trait origin ratios")

3126/(3126+2094)

2094/(3126+2094)

#Make a pie chart mammals
ratios <- c(499, 1467,1347)
names(ratios) <-c("Pantheria (15.1%)","EltonTraits (44.3%)","Traits Added (40.6%)")
mam_ratio <- pie(ratios, col=c("light green", "cornsilk", "cyan"),  main="Mammal trait origin ratios")

499/(499+1467+1347)
1467/(499+1467+1347)
1347/(499+1467+1347)

#Unique genera
#71 genera for mammals

scientific_names_mammals <-separate(dat, scientific_name, into = c("genus", "species"), sep= " (?=[^ ]+$)")

length(unique(scientific_names_mammals$GENUS))

# 190 genera for birds
scientific_names_birds <-separate(new_df_b, scientific_name, into = c("GENUS", "species"), sep= " (?=[^ ]+$)")

length(unique(scientific_names_birds$GENUS))

#Unique mammal species (182)
length(unique(dat$scientific_name))

#Unique Bird species (512)
count_diet_cats <-tapply(dat$scientific_name, dat$Diet.Cat, length)
length(unique(new_df_b$scientific_name))

#New species (15 mammals, 230 birds) - have either been reclassified or recently discovered

#Diet composition of dataset
dat_1 <-read.csv("/Volumes/GoogleDrive/My Drive/neotropical_frugivores/Database2019/Databases/L1/diet_cats.csv")
count_diet_cats <-as.data.frame(tapply(dat_1$scientific_name, dat_1$Diet.Cat, length))
df <- as.data.frame(count_diet_cats)
df <- tibble::rownames_to_column(df, "Category")

barplot(df$`tapply(dat_1$scientific_name, dat_1$Diet.Cat, length)`, main="Frequency of Diets Present", xlab= "Diet", ylab="Species Count", cex.lab=1,cex.names=1, col=rainbow(16), legend = c("Carn", "Carn/Frug", "Nect/Frug","Frug","Frug/Herb","Frug/Insect","Frug/Omn","Herb/Frug","Insect/Frug","Nect/Invert","Omnivore"))

#small ranged
barplot(df$`tapply(dat_1$scientific_name, dat_1$Diet.Cat, length)`, main="Number of species/diet category", ylab="Species Count", cex.lab=1,cex.names=1, col=rainbow(14), legend = c("Frugivore","Frug/Herb","Frug/Insect","Frug/Nect","Frug/Omni","Herbivore","Herb/Frug","Insectivore","Insect/Frug","Insect/Herb","Insect/Omni","Invertebrate","Nectar/Frug","Omnivore"), args.legend=list(title="Diet Categories",cex=.8),ylim=c(0,400),xlim=c(0,23))


#test : stacked barplot

category<- c("Mammals","Mammals","Mammals","Birds","Birds","Birds")
condition<-c("Pantheria","Elton Traits","New traits","Pantheria","Elton Traits","New traits")
percent <- c(.151,.443,.406,0,.599,.401) # change these to the new values 
data <- data.frame(category,condition,percent)

# Stacked + percent
ggplot(data, aes(fill=condition, y=percent, x=Category)) + 
  geom_bar(position="fill", stat="identity") + geom_text(aes(label = percent*100),size = 4, hjust = 0.6, vjust = 3, position =  "stack")  + scale_fill_manual(values=c("#999999", "#99CC00", "#56B4E9"), 
                       name="Data Source",
                       breaks=c("Elton Traits","New traits", "Pantheria"), labels=c("New traits","Elton Traits","Pantheria"))+ labs(x="Taxa")  + labs(y="Percent contribution")
