#Title: Download GBIF records

#Project: Montane Frugivoria

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske, Patrick Bills

#Data inputs: bird_frug_montane.csv, mam_frug_montane.csv

#Data outputs: GBIF occurrence record download on the GBIF website

#Overview: Downloading occurrence records for species in a small spatial subset (Colombia and Ecuador). WKT boundaries were extract from roughly drawn polygons around Colombia and Ecuador through the GBIF website "location" tab for searching occurrence records.  This code is modified from that provided by the GBIF blog, which overcomes the issue of pulling large numbers of records through GBIF for multiple species at once (Waller & Grosjean, 2019)

#Code modified from: John Waller and Marie Grosjean's blog post: https://data-blog.gbif.org/post/downloading-long-species-lists-on-gbif/

#Requires: Uses output of "3_montane_frugivore_subset", which must be run first to obtain species subset.


# fill in your gbif.org credentials 
user <- " " # your gbif.org username 
pwd <- " " # your gbif.org password
email <- " " # your email 

library(dplyr)
library(purrr)
library(readr)  
library(magrittr) # for %T>% pipe
library(rgbif) # for occ_download
library(taxize) # for get_gbifid_

# Set path to mammal database.
mammal_frug <- "INSERT PATH HERE"
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

# Make the download request to GBIF. Will appear in your GBIF account under downloads.
occ_download(
  pred_in("taxonKey", gbif_taxon_keys_mammals),
  pred_in("basisOfRecord", c('PRESERVED_SPECIMEN','HUMAN_OBSERVATION','OBSERVATION','MACHINE_OBSERVATION')),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred_gte("year", 2000),
  format = "SIMPLE_CSV",
  pred_within("POLYGON((-80.18398 0.10451,-79.83526 -1.606,-80.57896 -4.17046,-78.5749 -5.38276,-78.21263 -3.1561,-76.57226 -2.32892,-75.96933 -1.58091,-74.93352 -0.29529,-69.90707 -4.23138,-69.03622 0.26892,-67.03952 1.88776,-67.59548 6.54709,-70.32262 8.66511,-71.33897 12.40357,-75.11241 11.33829,-84.1098 9.04529,-80.18398 0.10451))"),
  user=user,pwd=pwd,email=email
)

#_______________________________________________________________________________________
#Birds

#Set path of bird database
bird_frug <- "INSERT PATH HERE"
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
  pred_within("POLYGON((-80.18398 0.10451,-79.83526 -1.606,-80.57896 -4.17046,-78.5749 -5.38276,-78.21263 -3.1561,-76.57226 -2.32892,-75.96933 -1.58091,-74.93352 -0.29529,-69.90707 -4.23138,-69.03622 0.26892,-67.03952 1.88776,-67.59548 6.54709,-70.32262 8.66511,-71.33897 12.40357,-75.11241 11.33829,-84.1098 9.04529,-80.18398 0.10451))"),
  user=user,pwd=pwd,email=email)

