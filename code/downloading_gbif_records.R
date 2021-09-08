# ADD SUMMARY AND MORE INFORMATION
# fill in your gbif.org credentials 
user <- "bgerstner90" # your gbif.org username 
pwd <- "hermione1" # your gbif.org password
email <- "bgerstner90@gmail.com" # your email 

library(dplyr)
library(purrr)
library(readr)  
library(magrittr) # for %T>% pipe
library(rgbif) # for occ_download
library(taxize) # for get_gbifid_
# All frugivorous mammals in Central and South America (excluding those way Centroids above Northern Mexico)
mammal_frug <- "/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/databases/montane_mammal_database_checks.csv"
# match the names 
gbif_taxon_keys_mammals <- 
  readr::read_csv(mammal_frug) %>% 
  pull("IUCN_species_name") %>% # use fewer names if you want to just test 
  taxize::get_gbifid_(method="backbone") %>% # match names to the GBIF backbone to get taxonkeys
  imap(~ .x %>% mutate(original_sciname = .y)) %>% # add original name back into data.frame
  bind_rows() %T>% # combine all data.frames into one
  readr::write_tsv(path = "all_matches.tsv") %>% # save as side effect for you to inspect if you want
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% # get only accepted and matched names
  filter(class == "Mammalia") %>% # remove anything that might have matched to a non-plant
  pull(usagekey) # get the gbif taxonkeys
# !!very important here to use "type=in"!!
# make the download request to GBIF 

occ_download(
  pred_in("taxonKey", gbif_taxon_keys_mammals),
  pred_in("basisOfRecord", c('PRESERVED_SPECIMEN','HUMAN_OBSERVATION','OBSERVATION','MACHINE_OBSERVATION')),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred_gte("year", 2000),
  format = "SIMPLE_CSV",
  pred_within("POLYGON((-75.16354 0.03958,-73.81367 -0.59409,-75.05862 -1.39921,-72.57811 -0.18179,-70.73781 5.72789,-70.79976 6.84574,-70.16267 13.9572,-76.9901 9.97052,-79.08197 7.60193,-77.5219 5.75079,-77.73298 2.38444,-78.49547 1.19354,-77.59132 0.74138,-75.16354 0.03958))"),
  user=user,pwd=pwd,email=email
)

occ_download(
  pred_in("taxonKey", gbif_taxon_keys_mammals),
  pred_in("basisOfRecord", c('PRESERVED_SPECIMEN','HUMAN_OBSERVATION','OBSERVATION','MACHINE_OBSERVATION')),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred_gte("year", 2016),
  format = "SIMPLE_CSV",
  pred_within("POLYGON((-80.76316 -0.44306,-81.65989 -3.00162,-80.57975 -5.28092,-78.78417 -5.30492,-78.1156 -3.60958,-74.96513 -1.40285,-75.35168 0.22257,-76.61101 0.50276,-79.01926 2.02797,-80.76316 -0.44306))"),
  user=user,pwd=pwd,email=email)



# occ_download(
#   sprintf("taxonKey = %s", paste0(gbif_taxon_keys_mammals, collapse = ",")),
#   type="in", 
#   format = "SIMPLE_CSV", 
#   user=user,pwd=pwd,email=email)

#_______________________________________________________________________________________
#Birds

bird_frug <- "/Users/bethgerstner/Desktop/MSU/Zarnetske_Lab/Data/databases/montane_birds_database_checks.csv"
# match the names 
gbif_taxon_keys_birds <- 
  readr::read_csv(bird_frug) %>% 
  pull("IUCN_species_name") %>% # use fewer names if you want to just test 
  taxize::get_gbifid_(method="backbone") %>% # match names to the GBIF backbone to get taxonkeys
  imap(~ .x %>% mutate(original_sciname = .y)) %>% # add original name back into data.frame
  bind_rows() %T>% # combine all data.frames into one
  readr::write_tsv(path = "all_matches.tsv") %>% # save as side effect for you to inspect if you want
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% # get only accepted and matched names
  # remove anything that might have matched to a non-plant
  filter(class == "Aves") %>%
  pull(usagekey) # get the gbif taxonkeys
# !!very important here to use "type=in"!!
# make the download request to GBIF 

occ_download(
  pred_in("taxonKey", gbif_taxon_keys_birds),
  pred_in("basisOfRecord", c('PRESERVED_SPECIMEN','HUMAN_OBSERVATION','OBSERVATION','MACHINE_OBSERVATION')),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred_gte("year", 2016),
  format = "SIMPLE_CSV",
  pred_within("POLYGON((-75.16354 0.03958,-73.81367 -0.59409,-75.05862 -1.39921,-72.57811 -0.18179,-70.73781 5.72789,-70.79976 6.84574,-70.16267 13.9572,-76.9901 9.97052,-79.08197 7.60193,-77.5219 5.75079,-77.73298 2.38444,-78.49547 1.19354,-77.59132 0.74138,-75.16354 0.03958))"),
  user=user,pwd=pwd,email=email)

#need Ecuador as it's own file
occ_download(
  pred_in("taxonKey", gbif_taxon_keys_birds),
  pred_in("basisOfRecord", c('PRESERVED_SPECIMEN','HUMAN_OBSERVATION','OBSERVATION','MACHINE_OBSERVATION')),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred_gte("year", 2016),
  format = "SIMPLE_CSV",
  pred_within("POLYGON((-80.76316 -0.44306,-81.65989 -3.00162,-80.57975 -5.28092,-78.78417 -5.30492,-78.1156 -3.60958,-74.96513 -1.40285,-75.35168 0.22257,-76.61101 0.50276,-79.01926 2.02797,-80.76316 -0.44306))"),
  user=user,pwd=pwd,email=email)




