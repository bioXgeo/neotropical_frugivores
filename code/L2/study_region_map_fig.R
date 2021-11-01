#Title: Study region map fig

#Project: Montane Frugivoria

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske, Patrick Bills

#Overview: Creates a study area/occurrence map for birds and mammals within the Frugivoria database. The occurrence data was obtained from GBIF for all species within the database. The occurrence records for birds and mammals is overlaid on top of probability of cloud forest.

#Data input: GBIF dataset, cloud forest probability layer (MODCF_CloudForestPrediction.tif) from  Wilson & Jetz 2016.

#Data outputs: ggplot maps of study region and occurrence records available for bird and mammal species in the database. 

#Requires: Uses final outputs of script "downloading_gbif_records.R". To retrieve the GBIF data obtained through that script, it must first be downloaded from the GBIF website.

#Date: Oct 11th, 2021



library(dplyr)
library(maps)
library(ggplot2)
library(tmap)
library(rnaturalearth)
library(leaflet)
library(sf)
library(ggsn)
library(rgeos)
library(cowplot)
library(BAMMtools)


# Read in GBIF data for birds
birds <- read.csv("INSERT PATH")

# Select the spatial columns from the GBIF dataset
bird_occs <- birds %>%
  select(species, decimalLatitude, decimalLongitude)

# Retain only unique records. This helps prevent overloading R when mapping. There's no reason to have more than one occurrence for a species at the same location unless we are mapping richness.
full_bird_occ  <-unique(bird_occs[c("species","decimalLatitude","decimalLongitude")])


# Pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Subset world map. In this case we are removing the Galapagos by defining the bounding box around the Ecuador polygon.
study_region <- worldMap %>% filter(sovereignt == "Ecuador" | sovereignt == "Colombia"| sovereignt == "Panama"| sovereignt == "Venezuela" | sovereignt == "Peru" | sovereignt == "Bolivia" | sovereignt == "Brazil")

# Convert the body mass column to numeric
bird_dataset$Body_mass_value <-as.numeric(bird_dataset$Body_mass_value)

# Crop the study region to the bounding box of interest
study_region_crop <-st_crop(study_region, xmin = -83, xmax = -70, ymin = -7, ymax = 13)


library(raster)

# We included a cloud forest probability layer (MODCF_CloudForestPrediction.tif) from  Wilson & Jetz 2016.
# Citation: Wilson, A.M. & Jetz, W. (2016) Remotely Sensed High-Resolution Global Cloud Dynamics for Predicting Ecosystem and Biodiversity Distributions. PLOS Biology, 14, e1002415.
cloud_forest <- raster("INSERT PATH HERE") 

# Crop cloud forest layer to that of the study region
cloud_forest_clip <- crop(cloud_forest, study_region_crop)

# Need the raster to be a dataframe for mapping. Convert to points to do this.
cloud_forest_p <- rasterToPoints(cloud_forest_clip, spatial = TRUE)

# Then convert to a 'conventional' dataframe
cloud_forest_df  <- data.frame(cloud_forest_p)

bird_occs <- ggplot() + theme_bw() + geom_raster(data=cloud_forest_df, aes(x=x, y=y, fill=MODCF_CloudForestPrediction)) + scale_fill_gradient(low="white", high="black") + geom_sf(data = study_region_crop, fill = NA) +geom_point(data = full_bird_occ, aes(x = decimalLongitude, y = decimalLatitude), color="lightcoral", size=.01, alpha=.1)

#Add scale
final_bird_occs <-bird_occs + theme(legend.position="none", panel.background = element_rect(fill = "aliceblue"))+ scalebar(x.min = -84, x.max = -81.7, y.min =-3, y.max = -5,dist = 100, st.dist=.1, st.size=1.9, height=.19, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop, location="bottomleft", scale=.07, symbol=1) + ylab("Latitude") + xlab("Longitude") 


#Create an inset map

#Load in South America
#Load in study area
#create ggplot map and can remove the zoom in part below

#Load in Latin American study region
SA_study_region <- worldMap %>% filter(region_wb == "Latin America & Caribbean")

#Full study region of dataset. This was a drawn polgyon to highlight areas containing moist montane regions.
wkt_full_study_region <-readWKT("POLYGON((-117.90586 32.40823,-114.93283 24.02867,-103.38519 17.57394,-95.69147 14.92501,-93.74959 16.49881,-92.64833 13.26132,-86.8181 11.91952,-85.36732 7.62409,-78.95541 7.29366,-82.2415 -6.75735,-73.62066 -17.43199,-69.74556 -20.27371,-74.26679 -41.31821,-75.77755 -51.68231,-72.9972 -52.59989,-72.9972 -52.59989,-72.9972 -52.59989,-72.9972 -52.59989,-66.35996 -29.99383,-64.2351 -12.64059,-73.18461 -6.82992,-69.07763 1.34059,-67.37915 2.79301,-67.6658 5.65767,-69.44627 6.92719,-70.93479 7.4764,-72.13081 10.06589,-72.54342 11.79581,-75.00572 11.32275,-77.1206 9.48178,-78.31286 10.38244,-81.55377 9.55725,-83.40351 10.96224,-81.82526 12.13639,-85.4989 16.5353,-88.11664 20.69613,-94.66028 18.37092,-95.47524 23.39009,-98.81918 27.37902,-104.5994 31.71723,-117.90586 32.40823))")

#Change CRRS
crs(wkt_full_study_region) <-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


plot(SA_study_region[1])
plot(wkt_full_study_region)

#Change to spatial dataframe
wkt_full_study_region_sf <-st_as_sf(wkt_full_study_region)

SA_study_region_df <- fortify(SA_study_region)
wkt_full_study_region_df <- fortify(wkt_full_study_region_sf)

#check the way the inset looks
test_inset <- ggplot() + geom_sf(data = SA_study_region_df) + theme_bw() + geom_sf(data=wkt_full_study_region_df, alpha=.30, fill="blue")


# Get zoom box and outline study region used for GBIF example
inset_map_box <- 
  test_inset +
  geom_rect(aes(
    xmin = -83, 
    xmax = -70, 
    ymin = -7, 
    ymax = 13),
    fill = NA, 
    colour = "red",
    size = 0.6,
    alpha=.8
  )


# Add inset map to bird occurrence map
final_bird_occs_inset <-ggdraw(final_bird_occs) +
  draw_plot(
    {
      inset_map_box +
        theme(legend.position = "none", axis.title.x = element_blank(),
              axis.title.y = element_blank(), axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              plot.background = element_blank(),
              panel.grid.major = element_blank())
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.15, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.73,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.40, 
    height = 0.25)

final_bird_occs_inset


#Same for mammals as above

#download mammal occurrence records from GBIF
mam <- read.csv("/Users/bethgerstner/Desktop/mam_GBIF/mam_GBIF_2021.csv")

#select the spatial columns 
mam_occs <- mam_full %>%
  select(species, decimalLatitude, decimalLongitude)

#retain only unique records
full_mam_occ <-unique(mam_occs[c("species","decimalLatitude","decimalLongitude")])


#merge in traits
full_mam_occ$IUCN_species_name <- full_mam_occ$species
full_mam_occ <- full_mam_occ[,-c(1)]

# Create map of occurrence records on top of cloud forest
mam_occs <- ggplot() + theme_bw() + geom_raster(data=cloud_forest_df, aes(x=x, y=y, fill=MODCF_CloudForestPrediction)) + scale_fill_gradient(low="white", high="black") + geom_sf(data = study_region_crop, fill = NA) + geom_point(data = full_mam_occ, aes(x = decimalLongitude, y = decimalLatitude), size=.01, color="lightseagreen")


#add map features
final_mam_occs <-mam_occs + theme(legend.position = "none", panel.background = element_rect(fill = "aliceblue"))+ scalebar(x.min = -84, x.max = -81.7, y.min =-3, y.max = -5,dist = 100, st.dist=.1, st.size=1.9, height=.19, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop, location="bottomleft", scale=.07, symbol=1) + ylab("") + xlab("") 

final_mam_occs

# Create multipanel plot
grid.arrange(final_bird_occs_inset, final_mam_occs, nrow = 1)



