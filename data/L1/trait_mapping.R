birds <- read.csv("/Users/bethgerstner/Desktop/birds_GBIF/bird_small_gbif_2021.csv", stringsAsFactors = FALSE)

birds_2 <- read.csv("/Users/bethgerstner/Desktop/birds_GBIF/bird_small_gbif_2.csv")

bird_trait_data <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_FRUGIVORIA/data/frugivore/L1/working_databases/montane_edits/montane_birds_database_checks.csv")


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

#select the spatial columns 
bird_occs <- birds %>%
  select(species, decimalLatitude, decimalLongitude)

#retain only unique records
bird_occs_unique <-unique(bird_occs[c("species","decimalLatitude","decimalLongitude")])

#pull in Ecuador occurrences - these couldn't load correctly in Excel
bird_occs_2 <- birds_2 %>%
  select(species, decimalLatitude, decimalLongitude)

#unique occurrences in Ecuador
bird_occs_unique_2 <-unique(bird_occs_2[c("species","decimalLatitude","decimalLongitude")])


#join bird occurrences

full_bird_occ <- rbind(bird_occs_unique, bird_occs_unique_2)

#merge in traits
full_bird_occ$IUCN_species_name <- full_bird_occ$species
full_bird_occ <- full_bird_occ[,-c(1)]
bird_dataset <- merge(full_bird_occ, bird_trait_data, by="IUCN_species_name")


#pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# country subset. In this case we are removing the Galapagos by defining the bounding box around the Ecuador polygon.
study_region <- worldMap %>% filter(sovereignt == "Ecuador" | sovereignt == "Colombia"| sovereignt == "Panama"| sovereignt == "Venezuela" | sovereignt == "Peru" | sovereignt == "Bolivia" | sovereignt == "Brazil")


#scalebar(neighbours, location= "bottomright",dist = 200, st.dist=.1, st.size=2, height=0.1, transform = TRUE, dist_unit = "km", model = 'WGS84') +
  #north(study_region, location="topleft", scale=0.5, symbol=1) +

bird_dataset$Body_mass_value <-as.numeric(bird_dataset$Body_mass_value)

study_region_crop <-st_crop(study_region, xmin = -83, xmax = -70, ymin = -7, ymax = 13)


test <- ggplot() + geom_sf(data = study_region_crop) + theme_bw() + geom_point(data = bird_dataset, aes(x = decimalLongitude, y = decimalLatitude, color=Body_mass_value), size=.1) 

#where should the breaks be so we can see color differences
getJenksBreaks(bird_dataset$Body_mass_value, 10, subset = NULL)

bins <- c(4.24,   41.00,  106.00,  200.00,  314.06,  485.00,  806.71, 1600.10, 2872.00, 4133.00)

bird_mass <-test + scale_fill_binned(
  alpha=1,
  begin=.1,
  end=1,
  limits = c(4.24,350), 
  breaks = c(4, 41, 106, 200, 314),
  type="viridis",
  na.value = "grey50",
  direction = -1,
  guide = guide_colorsteps(draw.ulim = F, draw.llim = T, even.steps = TRUE), aesthetics = "colour", guide_legend("Body mass (g)")) 


?scale_color_stepsn

#bird_mass_test <-test + scale_color_stepsn(colours = c("purple4","lightblue", "blue", "darkblue", "lightgreen", "darkgreen","red","green","yellow"), breaks = c(4, 41, 106, 200, 314, 485, 807, 1600, 2872, 4133), na.value = "grey50", guide_legend("Body mass (g)"))


final_bird_mass <-bird_mass + theme(legend.title = element_text(size=12, color = "black", face="bold"), legend.justification=c(0,1),legend.position=c(0.05, 0.7), legend.background = element_blank(), legend.key = element_blank())+ scalebar(x.min = -84, x.max = -81.7, y.min =-3, y.max = -5,dist = 100, st.dist=.1, st.size=1.9, height=.19, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop, location="bottomleft", scale=.07, symbol=1) + ylab("Latitude") + xlab("Longitude") 

#Inset map

#Load in SA
#Load in study area
#create ggplot map and can remove the zoom in part below

SA_study_region <- worldMap %>% filter(region_wb == "Latin America & Caribbean")


wkt_full_study_region <-readWKT("POLYGON((-117.90586 32.40823,-114.93283 24.02867,-103.38519 17.57394,-95.69147 14.92501,-93.74959 16.49881,-92.64833 13.26132,-86.8181 11.91952,-85.36732 7.62409,-78.95541 7.29366,-82.2415 -6.75735,-73.62066 -17.43199,-69.74556 -20.27371,-74.26679 -41.31821,-75.77755 -51.68231,-72.9972 -52.59989,-72.9972 -52.59989,-72.9972 -52.59989,-72.9972 -52.59989,-66.35996 -29.99383,-64.2351 -12.64059,-73.18461 -6.82992,-69.07763 1.34059,-67.37915 2.79301,-67.6658 5.65767,-69.44627 6.92719,-70.93479 7.4764,-72.13081 10.06589,-72.54342 11.79581,-75.00572 11.32275,-77.1206 9.48178,-78.31286 10.38244,-81.55377 9.55725,-83.40351 10.96224,-81.82526 12.13639,-85.4989 16.5353,-88.11664 20.69613,-94.66028 18.37092,-95.47524 23.39009,-98.81918 27.37902,-104.5994 31.71723,-117.90586 32.40823))")

crs(wkt_full_study_region) <-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


plot(SA_study_region[1])
plot(wkt_full_study_region)

wkt_full_study_region_sf <-st_as_sf(wkt_full_study_region)

SA_study_region_df <- fortify(SA_study_region)
wkt_full_study_region_df <- fortify(wkt_full_study_region_sf)

test_inset <- ggplot() + geom_sf(data = SA_study_region_df) + theme_bw() + geom_sf(data=wkt_full_study_region_df, alpha=.30, fill="blue")

#+ geom_polygon(data = wkt_full_study_region, aes(x = decimalLongitude, y = decimalLatitude), size=.0001)


#get zoom box and outline
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


#density plot
# Use semi-transparent fill


m_birds <-ggplot(bird_trait_data, aes(x=Body_mass_value), alpha=.4) + geom_density(fill="lightcoral", alpha=.7) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x))) + xlab("Body mass (g)") +  geom_vline(aes(xintercept = median(Body_mass_value, na.rm = T)), colour = "red", linetype ="longdash", size = .8)

#calculate mean and median to put on graph
summary(bird_trait_data$Body_mass_value)

#add inset map
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

mam <- read.csv("/Users/bethgerstner/Desktop/mam_GBIF/mam_GBIF_2021.csv")
mam2 <- read.csv("/Users/bethgerstner/Desktop/mam_GBIF/mammal_GBIF_2021_correct.csv")

mam_full <-rbind(mam, mam2)
mam_trait_data <- read.csv("/Users/bethgerstner/Desktop/mam_GBIF/montane_mammal_database_checks.csv")


#select the spatial columns 
mam_occs <- mam_full %>%
  select(species, decimalLatitude, decimalLongitude)

#retain only unique records
full_mam_occ <-unique(mam_occs[c("species","decimalLatitude","decimalLongitude")])


#merge in traits
full_mam_occ$IUCN_species_name <- full_mam_occ$species
full_mam_occ <- full_mam_occ[,-c(1)]
mam_dataset <- merge(full_mam_occ, mam_trait_data, by="IUCN_species_name")

mam_dataset$body_mass_value_g_elton <-as.numeric(mam_dataset$body_mass_value_g_elton)


mam_test_new <- ggplot() + geom_sf(data = study_region_crop) + theme_bw() + geom_point(data = mam_dataset, aes(x = decimalLongitude, y = decimalLatitude, color=body_mass_value_g_elton), size=.01)

#where should the breaks be so we can see color differences
mam_jenks <-mam_dataset$body_mass_value_g_elton
getJenksBreaks(mam_jenks, 10, subset = NULL)

#bins_mam <- c( 5.60,   1339.99,   3249.97,   5000.00,   7274.95,   9599.97,  12500.0,  21266.69,  32233.69, 140000.63)

mam_mass <-mam_test_new + scale_fill_binned(
  alpha=1,
  begin=.1,
  end=1,
  limits = c(0,7500), 
  breaks = c(5,   1340,   3250,   5000,   7275),
  type="viridis",
  na.value = "grey50",
  direction=-1,
  guide = guide_colorsteps(draw.ulim = F, draw.llim = T, even.steps = T), aesthetics = "colour", guide_legend("Body mass (g)"))


final_mam_mass <-mam_mass + theme(legend.title = element_text(size=12, color = "black", face="bold"), legend.justification=c(0,1),legend.position=c(0.05, 0.7), legend.background = element_blank(), legend.key = element_blank())+ scalebar(x.min = -84, x.max = -81.7, y.min =-3, y.max = -5,dist = 100, st.dist=.1, st.size=1.9, height=.19, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop, location="bottomleft", scale=.07, symbol=1) + ylab("") + xlab("") 
library(MASS)
library(scales)

#mammal density 
m_mam <-ggplot(mam_trait_data, aes(x=body_mass_value_g_elton), alpha=.4) + geom_density(fill="lightseagreen", alpha=.7) +
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





#Inset map

#Load in SA
#Load in study area
#create ggplot map and can remove the zoom in part below

SA_study_region <- worldMap %>% filter(region_wb == "Latin America & Caribbean")


wkt_full_study_region <-readWKT("POLYGON((-117.90586 32.40823,-114.93283 24.02867,-103.38519 17.57394,-95.69147 14.92501,-93.74959 16.49881,-92.64833 13.26132,-86.8181 11.91952,-85.36732 7.62409,-78.95541 7.29366,-82.2415 -6.75735,-73.62066 -17.43199,-69.74556 -20.27371,-74.26679 -41.31821,-75.77755 -51.68231,-72.9972 -52.59989,-72.9972 -52.59989,-72.9972 -52.59989,-72.9972 -52.59989,-66.35996 -29.99383,-64.2351 -12.64059,-73.18461 -6.82992,-69.07763 1.34059,-67.37915 2.79301,-67.6658 5.65767,-69.44627 6.92719,-70.93479 7.4764,-72.13081 10.06589,-72.54342 11.79581,-75.00572 11.32275,-77.1206 9.48178,-78.31286 10.38244,-81.55377 9.55725,-83.40351 10.96224,-81.82526 12.13639,-85.4989 16.5353,-88.11664 20.69613,-94.66028 18.37092,-95.47524 23.39009,-98.81918 27.37902,-104.5994 31.71723,-117.90586 32.40823))")

crs(wkt_full_study_region) <-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


plot(SA_study_region[1])
plot(wkt_full_study_region)

wkt_full_study_region_sf <-st_as_sf(wkt_full_study_region)

SA_study_region_df <- fortify(SA_study_region)
wkt_full_study_region_df <- fortify(wkt_full_study_region_sf)

test_inset <- ggplot() + geom_sf(data = SA_study_region_df) + theme_bw() + geom_sf(data=wkt_full_study_region_df, alpha=.30, fill="blue")

#+ geom_polygon(data = wkt_full_study_region, aes(x = decimalLongitude, y = decimalLatitude), size=.0001)


#get zoom box and outline
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

# another trait for birds
bird_dataset$generation_time
bird_dataset$generation_time <-as.numeric(bird_dataset$generation_time)

#create plot of trait
bird_gen_time <- ggplot() + geom_sf(data = study_region_crop) + theme_bw() + geom_point(data = bird_dataset, aes(x = decimalLongitude, y = decimalLatitude, color=generation_time), size=.1) 

#where should the breaks be so we can see color differences
getJenksBreaks(bird_dataset$generation_time, 5, subset = NULL)

#bins <- c( 2.9,  3.8,  4.4,  5.3,  6.1,  7.0,  7.7,  8.5,  9.4, 13.5)

bird_gen <-bird_gen_time + scale_fill_binned(
  low= "mistyrose1",
  high="magenta4",
  limits = c(0,14), 
  breaks = c(2.9, 3.8, 4.2,  5.7, 8.5, 13.5),
  na.value = "grey50",
  guide = guide_colorsteps(draw.ulim = T, draw.llim = T, even.steps = TRUE), aesthetics = "colour", guide_legend("Generation time (y)")) 


?scale_color_stepsn

#bird_mass_test <-test + scale_color_stepsn(colours = c("purple4","lightblue", "blue", "darkblue", "lightgreen", "darkgreen","red","green","yellow"), breaks = c(4, 41, 106, 200, 314, 485, 807, 1600, 2872, 4133), na.value = "grey50", guide_legend("Body mass (g)"))


final_bird_gen <-bird_gen + theme(legend.title = element_text(size=12, color = "black", face="bold"), legend.justification=c(0,1),legend.position=c(0.05, 0.7), legend.background = element_blank(), legend.key = element_blank())+ scalebar(x.min = -84, x.max = -81.7, y.min =-3, y.max = -5,dist = 100, st.dist=.1, st.size=1.9, height=.19, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop, location="bottomleft", scale=.07, symbol=1) + ylab("Latitude") + xlab("Longitude") 


#density plot
# Use semi-transparent fill


m_birds_gen <-ggplot(bird_trait_data, aes(x=generation_time), alpha=.4) + geom_density(fill="lightcoral", alpha=.7) + xlab("Generation time (y)") +  geom_vline(aes(xintercept = median(generation_time, na.rm = T)), colour = "red", linetype ="longdash", size = .8)

#calculate mean and median to put on graph
summary(bird_trait_data$generation_time)

#add inset density plot

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

#where should the breaks be so we can see color differences
mam_jenks_gen <-mam_dataset$generation_time
getJenksBreaks(mam_jenks_gen, 6, subset = NULL)

#bins_mam <- c(0.29,  3.00,  6.00,  8.40, 12.00, 16.00)

mam_gen_final <-mam_gen + scale_fill_binned(
  low= "mistyrose1",
  high="magenta4",
  limits = c(0,16), 
  breaks = c(0.29,  3.00,  6.00,  8.40, 12.00, 16.00),
  na.value = "grey50",
  guide = guide_colorsteps(draw.ulim = F, draw.llim = T, even.steps = T), aesthetics = "colour", guide_legend("Generation time (y)"))


final_mam_gen <-mam_gen_final + theme(legend.title = element_text(size=12, color = "black", face="bold"), legend.justification=c(0,1),legend.position=c(0.05, 0.7), legend.background = element_blank(), legend.key = element_blank())+ scalebar(x.min = -84, x.max = -81.7, y.min =-3, y.max = -5,dist = 100, st.dist=.1, st.size=1.9, height=.19, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop, location="bottomleft", scale=.07, symbol=1) + ylab("") + xlab("") 

library(MASS)
library(scales)

mam_trait_data$generation_time <- as.numeric(mam_trait_data$generation_time)

#mammal density 
m_mam_gen <-ggplot(mam_trait_data, aes(x=generation_time), alpha=.4) + geom_density(fill="lightseagreen", alpha=.7) + xlab("Generation time (y)") +  geom_vline(aes(xintercept = median(generation_time, na.rm = T)), colour = "red", linetype ="longdash", size = .8)

#calculate mean and median to put on graph
summary(mam_trait_data$generation_time)

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

# Same for mammals

#multipanel
mass_plot <-plot_grid(final_bird_mass_inset_density_f, NULL, final_mam_mass_inset_density, rel_widths = c(1, -0.1, 1), align = "hv", nrow = 1)

gen_time_plot <- plot_grid(final_bird_gen_inset, NULL, final_mam_gen_inset_density, rel_widths = c(1, -0.1, 1), align = "hv",  nrow = 1)

full_trait_plot <- plot_grid(mass_plot, gen_time_plot)
















#how to zoom in
#coord_sf(xlim = c(-83, -70), ylim = c(-7, 13), expand = FALSE)
test+ scale_colour_discrete(breaks = 1:5) + theme(legend.position = c("left"))
                          
                          
factor <- factor(bird_dataset$Body_mass_value)
getJenksBreaks(factor, 10, subset = NULL)
b <- breaks(factor, 1:5)
 


