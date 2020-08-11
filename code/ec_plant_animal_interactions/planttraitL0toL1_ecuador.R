# title: "planttraitL0toL1_ecuador.R"
# author: "Hazel Anderson, Pat Bills"
# date: "6/25/2020"x
# function : : downloads plant traits for Ecuador and saved them in to data/L0 folder

# this sets options for country and folders, then calls the function in fruiting_plant_traits_by_country.R 
# note this is an R svcript rather than Rmarkdown as BIEN functions use a cache folder and that has a problem
# when using "knit" due to difference in the current working directory (root directory of project version the folder of rhe script)
source('code/ec_plant_animal_interactions/fruiting_plant_traits_by_country.R')

# Lcation of the L0 folder where downloaded BIEN data will be saved and reused. 
# if you are using Rmarkdown and kintting, use '../../data/L0'
options("cache_folder" = 'data/L0')
country = "Ecuador"

# get just the species data (not the occurrence data yet)
species_list <- BIEN_list_country(country)

# filter using BIEN functions to get just fruiting species
species <- fruiting_species(species)

# FOR TESTING get just the first 100 sp
# REMOVE THIS LINE WHEN RUNNING ON THE FULL DATASET
species_list[1:100,]

all_fruiting_traits <- BIEN_trait_species(species_list)


# filter out fruiting species and get traits for those
trait_data <- fruiting_species_traits() 

L1_folder <- 'data/L1'
L1_filename <- "ecuador_plant_traits.csv"
print(paste("writing traits to ", file.path(L1_folder,L1_filename)))
write.csv(trait_data,file.path(L1_folder,L1_filename)  )

