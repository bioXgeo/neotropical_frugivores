## Code to download massive species lists from GBIF
## By: Beth Gerstner (Modified from https://data-blog.gbif.org/post/downloading-long-species-lists-on-gbif/)

# fill in your gbif.org credentials 
user <- "bgerstner90" # your gbif.org username 
pwd <- "####" # your gbif.org password
email <- "bgerstner90@gmail.com" # your email 

library(dplyr)
library(purrr)
library(readr)  
library(magrittr) # for %T>% pipe
library(rgbif) # for occ_download
library(taxize) # for get_gbifid_
# All frugivorous mammals in Central and South America (excluding those way Centroids above Northern Mexico)
mammals <- "/Volumes/GoogleDrive/My Drive/neotropical_frugivores/frugivore_database/Databases/L0/CSA_mam_frug.csv"
# match the names 
gbif_taxon_keys <- 
  readr::read_csv(mammals) %>% 
  pull("IUCN_species_name_all") %>% # use fewer names if you want to just test 
  taxize::get_gbifid_(method="backbone") %>% # match names to the GBIF backbone to get taxonkeys
  imap(~ .x %>% mutate(original_sciname = .y)) %>% # add original name back into data.frame
  bind_rows() %T>% # combine all data.frames into one
  readr::write_tsv(path = "all_matches.tsv") %>% # save as side effect for you to inspect if you want
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% # get only accepted and matched names
  filter(class == "Mammalia") %>% # remove anything that might have matched to a non-plant
  pull(usagekey) # get the gbif taxonkeys
# !!very important here to use "type=in"!!
# make the download request to GBIF 
# Only want records in a given region (unionized shapefile of species ranges)
#wkt <- read.csv("/Users/bethgerstner/Desktop/wkt.csv")

#geometry=wkt
test <- occ_download(
  sprintf("taxonKey = %s", paste0(gbif_taxon_keys, collapse = ",")), type = "in",
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email)
