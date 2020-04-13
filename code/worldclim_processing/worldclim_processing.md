# Necessary steps and code for processing WorldClim datasets #

*authors: Beth Gerstner & Patrick Bills*

*date: 4/13/20*


### 1. Download both 30 arc second Wordclim datasets from the website [worldclim.org](www.worldclim.org) to HPC folder *XXX* (where should we save this?)
  - old version: link addresses to data broken into two datasets [bioclim 1-9](http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/bio1-9_30s_bil.zip), [bioclim 10-19](https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_bio.zip)
  - new version: [bioclim2.1](https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_bio.zip)

### 2. Code to clip both datasets by a given shapefile

Load necessary libraries 

```R
library(rgdal) #reads in shapefile
library(rgeos) #allows the creation of buffered regions
library(raster) #allows you to create raster stacks and masks
```

Read in the species occurrence records and turn into a SpatialPointsDataFrame

```R
occ <- read.csv("Path to csv with all occurrence records") 
occ.sp <- SpatialPointsDataFrame(occ[c(1,2)], as.data.frame(occ[,1])) #Makes it into spatial object 
```

**Old worldclim data**

Set working directory to folder where environmental data is stored 

```R
setwd("WHEREVER OLD WORLDCLIM IS STORED")
```

Making stack of all 19 bioclimatic variables (both downloads for the old version need to be in the same folder). The stack links all of the individual rasters in the folder together so that they can each be processed at the same time

```R
worldclim_old <- list.files(pattern='bil', full.names=TRUE)
env_old <- stack(worldclim_old)
```

One thing that makes this data easier to work with is to first crop the full dataset to a smaller extent so we're not working with 1km x 1km grid cells for the whole world. 

5 degree buffer around occurrence records (should be a SpatialPointsDataFrame or SpatialPoints). 

```R
extentlarge <- gBuffer(occ.sp, width=5)
setwd("path to where you want to save large shapefile")
writeOGR(extentlarge, ".", "extentlarge_5degbuf", driver="ESRI Shapefile")
   
```

Crop and mask environmental variables by the larger extent. We crop first because masking first would take too long.

```R
env_crop_old <- crop(env_old, extentlarge) # will be a box
env_msk_old <- mask(env_crop_old, extentlarge) # will cut the environmental variables to the buffered shape around occurrence records
```

Data quality check

```R
plot(env_crop_old[[1]])
plot(env_msk_old[[1]])
points(occ.sp, col="red", cex=0.6)
```

Write this raster to the same folder the other spatial data is saved

```R
writeRaster(env_msk_old, filename= "bioclim_old_msk.tif", format="GTiff", overwrite=T)
writeRaster(env_crop_old, filename= "bioclim_old_crop.tif", format="GTiff", overwrite=T) # This file can be used to project SDMs later 
```

**New wordclim data**

Set working directory to folder where environmental data is stored 

```R
setwd("WHEREVER NEW WORLDCLIM IS STORED")
```

Making stack of all new 19 bioclimatic variables. The stack links all of the individual rasters in the folder together so that they can each be processed at the same time

```R
worldclim_new <- list.files(pattern='bil', full.names=TRUE)
env_new <- stack(worldclim_new)
```

Crop and mask environmental variables by the larger extent. We crop first because masking first would take too long.
```R
env_crop_new <- crop(env_new, extentlarge)
env_msk_new <- mask(env_crop_new, extentlarge)
```

Data quality check
```R
plot(env_crop_new[[1]])
plot(env_msk_new[[1]])
points(occ.sp, col="red", cex=0.6)
```

Write this raster to the same folder the other spatial data is saved
```R
writeRaster(env_msk_new, filename= "bioclim_new_msk.tif", format="GTiff", overwrite=T)
writeRaster(env_crop_new, filename= "bioclim_new_crop.tif", format="GTiff", overwrite=T) # can be used to project SDMs later on
```

**At this point the rasters should be small enough to process further on personal computers. **








