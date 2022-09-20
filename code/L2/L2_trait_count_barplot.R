#Title: Trait count barplot

#Project: Montane Frugivoria

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske, Patrick Bills

#Data input: Frugivoria_bird_database.csv, Frugivoria_mammal_database.csv

#Data output: trait_count_barplot.pdf

#Overview: This script analyzes the trait composition of the database for newly added traits and creates a barplot. Also includes PanTHERIA to show how many of those overlapping traits we have filled for mammals.


library(dplyr)
library(tidyr)
library (ggplot2)



#read in completed mammal database
mam <- read.csv("INSERT PATH HERE")
#read in bird database
bird<- read.csv("INSERT PATH HERE")

# New bird traits
longevity_b <- length(which(!is.na(bird$longevity)))
home_range_b <- length(which(!is.na(bird$home_range_size)))
habitat_special_b <- length(which(!is.na(bird$habitat_specialization)))
gen_time_b <- length(which(!is.na(bird$generation_time)))
body_size_b <- length(which(!is.na(bird$body_size_mm)))
sexual_dim_b <- length(which(!is.na(bird$sexual_dimorphism)))
range_1_b <- length(which(!is.na(bird$observed_range_sqkm)))
range_2_b <-  length(which(!is.na(bird$inferred_range_sqkm)))

# New mammal traits

#count the number of newly added traits
diet_categ_m <- length(which(!is.na(mam$diet_cat)))
longevity_m <- length(which(!is.na(mam$longevity)))
home_range_m <- length(which(!is.na(mam$home_range_size)))
habitat_special_m <- length(which(!is.na(mam$habitat_specialization)))
gen_time_m <- length(which(!is.na(mam$generation_time)))
body_size_m<- length(which(!is.na(mam$body_size_mm)))
sexual_dim_m <- length(which(!is.na(mam$sexual_dimorphism)))
range_1_m <- length(which(!is.na(mam$observed_range_sqkm)))
range_2_m <-  length(which(!is.na(mam$inferred_range_sqkm)))

# PanTHERIA traits
diet_categ_m_p <- length(which(!is.na(mam$diet_cat_p)))
longevity_m_p <- length(which(!is.na(mam$max_longevity_m_p)))
home_range_m_p <- length(which(!is.na(mam$home_range_km2_p)))
habitat_special_m_p <- length(which(!is.na(mam$habitat_specialization_p)))
gen_time_m_p <- length(which(!is.na(mam$generation_time_p)))
body_size_m_p<- length(which(!is.na(mam$body_size_mm_p)))
sexual_dim_m_p <- length(which(!is.na(mam$sexual_dimorphism_p)))
range_1_m_p <- length(which(!is.na(mam$GR_area_km2_p)))
range_2_m_p <-  length(which(!is.na(mam$inferred_range_sqkm_p)))


#Bird Traits 

#Birds change names of traits
bird_df <- data.frame(longevity_b,home_range_b,range_1_b, habitat_special_b, gen_time_b, body_size_b,sexual_dim_b) #left out range_2 for visualization purposes because it will be the same values at range_1.

#change names of traits
oldnames = c("longevity_b", "home_range_b","range_1_b","habitat_special_b", "gen_time_b", "body_size_b", "sexual_dim_b")
newnames = c("longevity", "home_range","range_size","habitat_special", "gen_time", "body_size", "sexual_dim")

bird_df_wide <-bird_df %>% rename_at(vars(oldnames), ~ newnames)

#Mammals change names of traits
mam_df <- data.frame(longevity_m,home_range_m,range_1_m, habitat_special_m, gen_time_m, body_size_m,sexual_dim_m)

#change names of traits
oldnames.m = c("longevity_m", "home_range_m","range_1_m","habitat_special_m", "gen_time_m", "body_size_m", "sexual_dim_m")
newnames.m = c("longevity", "home_range","range_size","habitat_special", "gen_time", "body_size", "sexual_dim")

mam_df_wide <-mam_df %>% rename_at(vars(oldnames.m), ~ newnames.m)

# Pantheria change names of traits
#Mammals change names of traits
mam_df_p <- data.frame(longevity_m_p,home_range_m_p,range_1_m_p,habitat_special_m_p, gen_time_m_p, body_size_m_p, sexual_dim_m_p)

#change names of traits
oldnames.m.p = c("longevity_m_p", "home_range_m_p","range_1_m_p","habitat_special_m_p", "gen_time_m_p", "body_size_m_p", "sexual_dim_m_p")
newnames.m.p = c("longevity", "home_range","range_size", "habitat_special", "gen_time", "body_size", "sexual_dim")

mam_df_wide.p <-mam_df_p %>% rename_at(vars(oldnames.m.p), ~ newnames.m.p)

#joining bird and mammal data
full_wide_trait_db <- rbind(mam_df_wide, bird_df_wide, mam_df_wide.p)

#convert this from wide to long format so we can put into ggplot2
full_long_trait_db <-gather(full_wide_trait_db,trait, species)
full_long_trait_db$taxa <-c("mammals", "birds", "PanTHERIA", "mammals", "birds","PanTHERIA", "mammals", "birds", "PanTHERIA","mammals", "birds","PanTHERIA","mammals", "birds", "PanTHERIA","mammals","birds","PanTHERIA","mammals", "birds","PanTHERIA")

#need the traits to be a factor
full_long_trait_db$trait <-as.factor(full_long_trait_db$trait)

#plot traits
all_new_traits <-ggplot(full_long_trait_db, aes(x=trait, y=species, fill=taxa, color)) + scale_fill_manual(values = c("birds" = "lightcoral", "mammals" = "lightseagreen","PanTHERIA" = "grey51"))+ labs(x = "New Frugivoria traits", y = "# species with trait") +
  geom_bar(stat="identity", position=position_dodge()) +geom_hline(yintercept=599, color="lightseagreen") + geom_hline(yintercept=1148, color="lightcoral") + theme(panel.background = element_rect(fill = "white"))

#add trait labels
all_new_traits + scale_x_discrete(labels = c("body size", "gen time", "habitat special","home range","longevity", "range size", "sexual dim")) + theme(axis.line = element_line(color="black", size = .2))


setwd("INSERT PATH TO SAVE")
dev.off()
