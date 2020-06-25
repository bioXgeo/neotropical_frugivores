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

occurrence_data <- get_species_occurrence_data(country)
print(paste("There are ",  nrow(occurrence_data),  "occurrence records"))
      
# FOR TESTING get just the first 10000 lines of occurrence data
# REMOVE THIS LINE WHEN RUNNING ON THE FULL DATASET
occurrence_data <- occurrence_data[1:1000,]

trait_data <- fruiting_species_traits(occurrence_data) 

L1_folder <- 'data/L1'
L1_filename <- "ecuador_plant_traits.csv"
print(paste("writing traits to ", file.path(L1_folder,L1_filename)))
write.csv(trait_data,file.path(L1_folder,L1_filename)  )

