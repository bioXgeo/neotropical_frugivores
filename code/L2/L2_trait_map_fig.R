#Title: Trait map figure

#Project: Montane Frugivoria

#Author: Beth E. Gerstner

#Collaborators: Phoebe L. Zarnetske, Patrick Bills

#Data Input: GBIF data for birds and mammals, Frugivoriae_bird_database.csv, Frugivoria_mammal_database.csv

#Data Output: density plots for two traits, trait distribution maps for those traits made in ggplot, final multipanel plot combining this information.

#Overview: Trait maps (mass and generation time) for birds and mammals. The final maps have a study region inset map and also a density plot showing the distribution of the trait in the dataset. 

#Requires: Uses final outputs of script "L2_downloading_gbif_records.R". To retrieve the GBIF data obtained through that script, it must first be downloaded from the GBIF website.

#Date: Oct 11th, 2021

# Libraries
library(dplyr)
library(maps)
library(ggplot2)
library(ggspatial)
library(tmap)
library(rnaturalearth)
library(leaflet)
library(sf)
library(ggsn)
library(rgeos)
library(cowplot)
library(BAMMtools)
library(MASS)
library(scales)

## Trait: Body Mass

# Insert path to where bird GBIF data is saved
birds <- read.csv("INSERT PATH HERE")

# Read in final bird trait database 
bird_trait_data <- read.csv("INSERT PATH HERE")

#select the spatial columns 
bird_occs <- birds %>%
  select(species, decimalLatitude, decimalLongitude)

# Retain only unique records
full_bird_occ <-unique(bird_occs[c("species","decimalLatitude","decimalLongitude")])

# Merge in traits
full_bird_occ$IUCN_species_name <- full_bird_occ$species
full_bird_occ <- full_bird_occ[,-c(1)]
bird_dataset <- merge(full_bird_occ, bird_trait_data, by="IUCN_species_name")


# Pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Country subset. In this case we are removing the Galapagos by defining the bounding box around the Ecuador polygon.
study_region <- worldMap %>% filter(sovereignt == "Ecuador" | sovereignt == "Colombia"| sovereignt == "Panama"| sovereignt == "Venezuela" | sovereignt == "Peru" | sovereignt == "Bolivia" | sovereignt == "Brazil")

# Convert the body mass column to numeric
bird_dataset$body_mass_value_g_e <-as.numeric(bird_dataset$body_mass_value_g_e)

# Crop study region to bounding box of interest
study_region_crop <-st_crop(study_region, xmin = -83, xmax = -70, ymin = -7, ymax = 13)

# Test how this looks
test <- ggplot() + geom_sf(data = study_region_crop) + theme_bw() + geom_point(data = bird_dataset, aes(x = decimalLongitude, y = decimalLatitude, color=body_mass_value_g_e), size=.1) 

#where should the breaks be so we can see color differences
getJenksBreaks(bird_dataset$body_mass_value_g_e, 10, subset = NULL)

#Chose to break at lower values to help with viewing. All values higher than the last break are then included in that range.
bird_mass <-test + scale_fill_binned(
  alpha=1,
  begin=0,
  end=1,
  limits = c(5,350), 
  breaks = c(441, 103, 181, 322,532),
  type="viridis",
  na.value = "grey50",
  direction = -1,
  guide = guide_colorsteps(draw.ulim = F, draw.llim = F, even.steps = TRUE), aesthetics = "colour", guide_legend("Body mass (g)")) 


# add map features
final_bird_mass_1 <-bird_mass + theme(legend.title = element_text(size=12, color = "black", face="bold"), legend.justification=c(0,1),legend.position=c(0.05, 0.7), legend.background = element_blank(), legend.key = element_blank())+  ylab("Latitude") + xlab("Longitude") 

final_bird_mass <- final_bird_mass_1 + ggspatial::annotation_scale(
  location = "bl", 
  pad_x = unit(.06, "in"), pad_y = unit(.15,"in"),
  bar_cols = c("grey60", "white"),
  text_family = "Arial"
) +
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(2, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "Arial"
    )
  )

#Inset map

#Load in SA
#Load in study area
#create ggplot map and can remove the zoom in part below
SA_study_region <- worldMap %>% filter(region_wb == "Latin America & Caribbean")

#Polygon of study region drawn for trait database including moist montane regions.
wkt_full_study_region <-readWKT("POLYGON((-116.39878 32.14502,-116.18439 25.589,-103.35463 18.51935,-78.19944 5.42044,-82.02377 -5.17495,-75.35233 -17.47622,-70.61516 -17.66095,-73.69595 -39.73744,-76.89099 -49.35018,-71.89115 -55.00772,-64.31938 -57.53321,-68.33512 -51.38147,-63.74197 -48.20784,-65.71778 -44.50498,-56.70514 -36.93942,-57.44673 -33.15855,-57.44673 -26.14211,-62.84294 -21.53527,-57.83652 -18.17212,-61.04452 -13.37745,-65.16414 -7.68073,-72.90818 -8.70505,-70.65325 -4.52629,-66.15564 0.14301,-60.45847 3.35677,-59.58068 8.57429,-68.03483 12.85263,-76.02387 9.6308,-81.75551 9.26437,-82.18541 13.87265,-85.9107 17.83896,-87.64715 21.66655,-95.89125 19.41326,-96.31007 26.3313,-106.27585 31.97955,-116.39878 32.14502))")

# Change CRS
crs(wkt_full_study_region) <-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


plot(SA_study_region[1])
plot(wkt_full_study_region)

#Change to sf
wkt_full_study_region_sf <-st_as_sf(wkt_full_study_region)

SA_study_region_df <- fortify(SA_study_region)
wkt_full_study_region_df <- fortify(wkt_full_study_region_sf)

# Test the inset
test_inset <- ggplot() + geom_sf(data = SA_study_region_df) + theme_bw() + geom_sf(data=wkt_full_study_region_df, alpha=.30, fill="blue")

# Get zoom box and outline for inset map
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


#density plot to understand the distribution of traits
# Use semi-transparent fill
m_birds <-ggplot(bird_trait_data, aes(x=Body_mass_value_e), alpha=.4) + geom_density(fill="lightcoral", alpha=.7) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x))) + xlab("Body mass (g)") +  geom_vline(aes(xintercept = median(Body_mass_value, na.rm = T)), colour = "red", linetype ="longdash", size = .8)

# Calculate mean and median to put on graph
summary(bird_trait_data$Body_mass_value)

# Add inset map
final_bird_mass_inset <-ggdraw(final_bird_mass) +
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
    x = 0.11, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.73,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.40, 
    height = 0.25)

final_bird_mass

#add inset density plot

final_bird_mass_inset_density_f <-ggdraw(final_bird_mass_inset) +
  draw_plot(
    {
      m_birds +
        theme(legend.position = "none",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=.5))
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.58, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.12,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.28, 
    height = 0.22)



#Same for mammals

# Read in GBIF data for mammals
mam <- read.csv("INSERT PATH HERE")

# Read in final trait database for mammals
mam_trait_data <- read.csv("INSERT PATH HERE")


# Select the spatial columns 
mam_occs <- mam_full %>%
  select(species, decimalLatitude, decimalLongitude)

# Retain only unique records
full_mam_occ <-unique(mam_occs[c("species","decimalLatitude","decimalLongitude")])

#merge in traits
full_mam_occ$IUCN_species_name <- full_mam_occ$species
full_mam_occ <- full_mam_occ[,-c(1)]
mam_dataset <- merge(full_mam_occ, mam_trait_data, by="IUCN_species_name")

mam_dataset$body_mass_value_g_elton <-as.numeric(mam_dataset$body_mass_value_g_e)


mam_test_new <- ggplot() + geom_sf(data = study_region_crop) + theme_bw() + geom_point(data = mam_dataset, aes(x = decimalLongitude, y = decimalLatitude, color=body_mass_value_g_e), size=.01)

#where should the breaks be so we can see color differences
mam_jenks <-mam_dataset$body_mass_value_g_e
getJenksBreaks(mam_jenks, 10, subset = NULL)

#Chose to show jenks in the densist part of the dataset for viewability
mam_mass <-mam_test_new + scale_fill_binned(
  alpha=1,
  begin=.1,
  end=1,
  limits = c(0,7500), 
  breaks = c(5.6,   1537.52,   3250,   5000,   7274.95),
  type="viridis",
  na.value = "grey50",
  direction=-1,
  guide = guide_colorsteps(draw.ulim = F, draw.llim = T, even.steps = T), aesthetics = "colour", guide_legend("Body mass (g)"))


final_mam_mass <-mam_mass + theme(legend.title = element_text(size=12, color = "black", face="bold"), legend.justification=c(0,1),legend.position=c(0.05, 0.7), legend.background = element_blank(), legend.key = element_blank()) + ylab("") + xlab("") 
library(MASS)
library(scales)

#mammal density plot to understand distribution of mass in dataset
m_mam <-ggplot(mam_trait_data, aes(x=body_mass_value_g_e), alpha=.4) + geom_density(fill="lightseagreen", alpha=.7) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x))) + xlab("Body mass (g)") +  geom_vline(aes(xintercept = median(body_mass_value_g_elton, na.rm = T)),  colour = "red", linetype ="longdash", size = .8)

#calculate mean and median to put on graph
summary(mam_trait_data$body_mass_value_g_elton)

final_mam_mass_inset_density <-ggdraw(final_mam_mass) +
  draw_plot(
    {
      m_mam +
        theme(legend.position = "none",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=.5))
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.58, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.12,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.28, 
    height = 0.22)

final_mam_mass_inset_density

## Trait: Generation Time

bird_dataset$generation_time
bird_dataset$generation_time <-as.numeric(bird_dataset$generation_time)

# Create plot of trait
bird_gen_time <- ggplot() + geom_sf(data = study_region_crop) + theme_bw() + geom_point(data = bird_dataset, aes(x = decimalLongitude, y = decimalLatitude, color=generation_time), size=.1) 

# Find where should the breaks be so we can see color differences
getJenksBreaks(bird_dataset$generation_time, 5, subset = NULL)

# Build map
bird_gen <-bird_gen_time + scale_fill_binned(
  low= "mistyrose1",
  high="magenta4",
  limits = c(0,14), 
  breaks = c(1.9, 4.1,  5.8, 8.8, 15.2),
  na.value = "grey50",
  guide = guide_colorsteps(draw.ulim = T, draw.llim = T, even.steps = TRUE), aesthetics = "colour", guide_legend("Generation time (y)")) 

#Add map features
final_bird_gen <-bird_gen + theme(legend.title = element_text(size=12, color = "black", face="bold"), legend.justification=c(0,1),legend.position=c(0.05, 0.7), legend.background = element_blank(), legend.key = element_blank())+ ylab("Latitude") + xlab("Longitude") 


#density plot
# Use semi-transparent fill
m_birds_gen <-ggplot(bird_trait_data, aes(x=generation_time), alpha=.4) + geom_density(fill="lightcoral", alpha=.7) + xlab("Generation time (y)") +  geom_vline(aes(xintercept = median(generation_time, na.rm = T)), colour = "red", linetype ="longdash", size = .8)

#calculate mean and median to put on graph
summary(bird_trait_data$generation_time)

# Final trait map of generation time for birds
final_bird_gen_inset <-ggdraw(final_bird_gen) +
  draw_plot(
    {
      m_birds_gen +
        theme(legend.position = "none",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=.5))
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.58, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.12,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.28, 
    height = 0.22)

#mammal gentime
mam_dataset$generation_time <-as.numeric(mam_dataset$generation_time)


mam_gen <- ggplot() + geom_sf(data = study_region_crop) + theme_bw() + geom_point(data = mam_dataset, aes(x = decimalLongitude, y = decimalLatitude, color=generation_time), size=.01)

# Where should the breaks be so we can see color differences
mam_jenks_gen <-mam_dataset$generation_time
getJenksBreaks(mam_jenks_gen, 5, subset = NULL)

mam_gen_final <-mam_gen + scale_fill_binned(
  low= "mistyrose1",
  high="magenta4",
  limits = c(0,14), 
  breaks = c(1.0,  3.8,  6.8,  11.4, 16.0),
  na.value = "grey50",
  guide = guide_colorsteps(draw.ulim = F, draw.llim = T, even.steps = T), aesthetics = "colour", guide_legend("Generation time (y)"))

#Add map features
final_mam_gen <-mam_gen_final + theme(legend.title = element_text(size=12, color = "black", face="bold"), legend.justification=c(0,1),legend.position=c(0.05, 0.7), legend.background = element_blank(), legend.key = element_blank())+ scalebar(x.min = -84, x.max = -81.7, y.min =-3, y.max = -5,dist = 100, st.dist=.1, st.size=1.9, height=.19, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop, location="bottomleft", scale=.07, symbol=1) + ylab("") + xlab("") 

mam_trait_data$generation_time <- as.numeric(mam_trait_data$generation_time)

# Calculate mammal density to show distribution of generation times in dataset
m_mam_gen <-ggplot(mam_trait_data, aes(x=generation_time), alpha=.4) + geom_density(fill="lightseagreen", alpha=.7) + xlab("Generation time (y)") +  geom_vline(aes(xintercept = median(generation_time, na.rm = T)), colour = "red", linetype ="longdash", size = .8)

#calculate mean and median to put on graph
summary(mam_trait_data$generation_time)

# # Final trait map of generation time for birds
final_mam_gen_inset_density <-ggdraw(final_mam_gen) +
  draw_plot(
    {
      m_mam_gen +
        theme(legend.position = "none",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=.5))
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.58, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.12,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.28, 
    height = 0.22) +
  coord_fixed(ratio=1)

final_mam_gen_inset_density

# Create multipanel plots
mass_plot <-plot_grid(final_bird_mass_inset_density_f, NULL, final_mam_mass_inset_density, rel_widths = c(1, -0.1, 1), align = "hv", nrow = 1)

gen_time_plot <- plot_grid(final_bird_gen_inset, NULL, final_mam_gen_inset_density, rel_widths = c(1, -0.1, 1), align = "hv",  nrow = 1)

full_trait_plot <- plot_grid(mass_plot, gen_time_plot)