# title: "fruiting_Plant_traits_by_country.R    "
# author: "Hazel Anderson, Pat Bills"
# date: "6/16/2020"
# function : functions to pulls trait data from BIEN for all fruiting species for a country 
# using R library BIEN.  It saves data downloaded from BIEN into a cache folder
# if the data has already been downloaded into the cache folder then it will read that instead.  
# you can set the cache folder using the option() function eg. option('cache_folder', 'data/L0')

# Load required packages
library(BIEN)
library(ape)
library(maps)
library(sp)
library(dplyr)


library(rgeos)
library(rgdal)
library(sf)
library(dplyr)
library(ggplot2)
# library(scico)
library(rnaturalearth)
# library(purrr)
# library(smoothr)
# library(rgbif)
# library(lwgeom)


#' Read ocurrences from BIEN for a country into a dataframe using BIEN::BIEN_occurrence_country
#' 
#' Note that the occurrence data is not necessary to get a list of species for a country.  Use BIEN_list_country()
#' This will take a while to download from BIEN if it's not be downloaded before.  

#' @param country Country name matching BIEN database.  Case-sensitive. Default 'Ecuador'
#' @return a data frame in BIEN format.   
#' @examples
#' occurrence_data <- get_species_occurrence_data('Colombia')
#' str_to_upper(dog)# check for local cached copy of species list for a country
get_species_occurrence_data <- function(country = "Ecuador", cache_folder= getOption("cache_folder", 'cache')){
    # TODO check that parameter is a valid BIEN country
    # TODO figure out cache folder because the default will probably not work
    message(paste('using cache folder ', cache_folder))
    
    # create the full path to the saved file using the country name
    occurrence_cache_file <- file.path(cache_folder, paste0("bien_",country,".csv"))
    
    # IF the cache file is here, read that, otherwise pull plant species list from BIEN
    # and cache that
    if(file.exists(occurrence_cache_file)){
        occurrence_data <- read.csv(occurrence_cache_file)
    } else {
        print(paste("please wait while species occurence data is downloading for ", country))
        occurrence_data <- BIEN_occurrence_country(country)
        # cache these data now
        write.csv(occurrence_data,occurrence_cache_file)
    }
    
    return(occurrence_data)
}


# full list of BIEN traits
# this is not currently used, but if we only want a subset of traits, then edit this vector and edit the 
# functions to use this trait_vector (currently they get all traits)
trait_vector <- function(){
    c( "diameter at breast height (1.3 m)", 
       "flower color", 
       "flower pollination syndrome", 
       "fruit type", 
       "inflorescence length", 
       "leaf area", 
       "leaf area per leaf dry mass", 
       "leaf carbon content per leaf dry mass", 
       "leaf carbon content per leaf nitrogen content", 
       "leaf compoundness", 
       "leaf dry mass", 
       "leaf dry mass per leaf fresh mass", 
       "leaf fresh mass", 
       "Leaf lamina fracture toughness", 
       "leaf life span", 
       "leaf nitrogen content per leaf area", 
       "leaf nitrogen content per leaf dry mass", 
       "leaf phosphorus content per leaf area", 
       "leaf phosphorus content per leaf dry mass", 
       "leaf photosynthetic rate per leaf area", 
       "leaf photosynthetic rate per leaf dry mass", 
       "leaf relative growth rate", 
       "leaf stomatal conductance for H2O per leaf area", 
       "leaf thickness", 
       "longest whole plant longevity", 
       "maximum fruit length", 
       "maximum leaf length", 
       "maximum leaf width", 
       "maximum whole plant height", 
       "maximum whole plant longevity", 
       "minimum fruit length", 
       "minimum leaf length", 
       "minimum leaf width", 
       "minimum whole plant height", 
       "plant flowering begin", 
       "plant flowering duration", 
       "plant fruiting duration", 
       "root dry mass", 
       "seed length", 
       "seed mass", 
       "stem dry mass", 
       "stem relative growth rate", 
       "stem wood density", 
       "vessel lumen area", 
       "vessel number", 
       "whole plant dispersal syndrome", 
       "whole plant growth form", 
       "whole plant growth form diversity", 
       "whole plant height", 
       "whole plant primary juvenile period length", 
       "whole plant sexual system", 
       "whole plant vegetative phenology", 
       "whole plant woodiness"
    )
    
}


# given a set of species, output only those that bear fruit (as a new vector of species)

fruiting_species <- function(species){
    proxy_trait_to_identify_fruiting <- "fruit type"
    # get a list of all species that have this trait, starting with species from our occurrence data
    message(paste('Downloading list for trait', proxy_trait_to_identify_fruiting, 'for', length(species), 'species') )
    BIEN_fruiting_species_data <- BIEN_trait_traitbyspecies( species, c(proxy_trait_to_identify_fruiting))
    # BIEN data has more than species, extract only the species names as a vector for use in other BIEN functions
    s <- as.vector(unique( BIEN_fruiting_species_data$scrubbed_species_binomial  ) )
    return( s )
}

# give all traits for those species that have fruit
# this is a little redundant because fruiting_species downloads and then throws away the fruit_type trait 
# and then downloads all the trait info for every trait

fruiting_species_traits <- function(species){
    
    # filter using BIEN functions to get just fruiting species
    species <- fruiting_species(species)
    
    # get all traits for those species
    message(paste('Downloading trait data for', length(species), 'species') )
    all_fruiting_traits <- BIEN_trait_species(species)
    return(all_fruiting_traits)
}

# given some occurence, pull out the unique species present
# note this shouldn't be necessary as BIEN has several functions to pull species by country, trait, etc
# e.g.  BIEN_list_country --> a list of species
species_in_occurrence <- function(occurence_data){
    return(as.vector(unique(occurrence_data$scrubbed_species_binomial)))
}


# test function looking at just the first 100 occurrence records
country_fruiting_species <- function(country="Ecuador"){
    s <- fruiting_species(BIEN_list_country(country)$scrubbed_species_binomial)
    return(s)
}

# given a country name and a country shape, pull the fruiting plant species, 
# get the ranges and crop those ranges to the country shape file
# Country shape could be from a custom function (see ecuador_shp() function) 
country_fruiting_rangemaps <-function(country_name="Ecuador", country_shape){
    species_list <- country_fruiting_species(country_name)
    ranges.sf <- BIEN_ranges_load_species(species_list)
    
    cropped_ranges <- sf::st_intersection(ranges.sf, country_shape)
    return(cropped_ranges)
    
}


ecuador_shp <- function(){
    worldMap <- rnaturalearth::ne_countries(scale = "medium", type = "countries", returnclass = "sf")
    
    # country subset. In this case we are removing the Galapagos by defining the bounding box around the Ecuador polygon.
    CRpoly <- worldMap %>% filter(sovereignt == "Ecuador")
    return(st_crop(CRpoly, c(xmin=-84, xmax=-75.24961, ymin=-4.990625, ymax=1.455371)))
}

ecuador_maps <- function() {
    country_name <- "Ecuador"
    country_shape <- ecuador_shp()
    
    ecuador_fruit_maps <- country_fruiting_rangemaps(country_name,country_shape)
    plot(ecuador_fruit_maps)
    return(ecuador_fruit_maps)
}
