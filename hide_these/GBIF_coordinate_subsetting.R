
library(rgeos)
library(rgdal)

#Read in all occurrence points and thin them by 10km
occ <- read.csv("/Volumes/GoogleDrive/My Drive/neotropical_frugivores/frugivore_database/Databases/L0/CSA_mammals_GBIF_data_unedited.csv")
occ <- occ[,c("verbatimScientificName","family","lat","long")]

##File used for analysis: occ <- read.csv("/Volumes/BETH'S DRIV/Anderson_Lab_Archive/georef_occur_summer15/10km_thin/thinned_data_thin1.csv") 
occ_na_rm <- na.omit(occ)
occ.sp <- SpatialPointsDataFrame(occ_na_rm[c(4,3)], as.data.frame(occ_na_rm[,1])) #Makes into spatial object

setwd("/Volumes/GoogleDrive/My Drive/neotropical_frugivores/Data/World_shapefile/World_Continents")
world <-readOGR(".", "TM_WORLD_BORDERS-0.3")

#subset the world shapefile to Central and South America 
CS_americas <-subset(world, SUBREGION %in% c('5','13'))
plot(CS_americas)

#plot points to inspect
points(occ.sp) 
writeOGR(occ.sp, ".","mammal_points.shp", driver="ESRI Shapefile")

#plot points over the IUCN Mexico northern limit file 
setwd("/Volumes/GoogleDrive/My Drive/neotropical_frugivores/Data/MX_northern_limit_shapefile")
mx_northern_lim <-readOGR(".", "CSA_frugivore_union")
plot(mx_northern_lim)
points(occ.sp)

#There are some rat species that are very widely distributed and I will have to take them out of the analysis - didn't get filtered out of the centroid analysis--- need to check this. 
#1) check to see what the IUCN range map for those rat species looks like... is the GBIF data very different from this map? If so, this is why it passed through the IUCN centroid test and wasn't incorporated into the final unionized shapefile. Check for the species at each step of the process.




#subset points to retain those within mx_northern_lim2


