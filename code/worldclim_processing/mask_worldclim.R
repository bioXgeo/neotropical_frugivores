#' mask_worldclim.R
#' author: "Beth Gerstner and Pat Bills"
#' date: "4/21/2020"
#' crop/mask WorldClim rasters to set of lat/long coordinates (e.g. occurence data) #
#' 
#' ##  NOTES raster options are set to use 20gb memory and SCRATCH disk for tmp files.  This is HPCC only code. 
#' To make this more portable, instead of encoding options here, put in options files in this folder or in home dir
#' 
#' 
#' ## WORK IN PROGRESS
#'  masking doesn't work; in transition from two code chunks for old/new into one function
#'  need occurrence data CRS and wordclim crs to match (or leave as NA?)
#' 
#' ## Goal
#' 
#' The object is to compare the analyses version 1.4 'old' and version 2.1 'new' climate data 
#' from Worldclim using a single species range.   
#' 
#' ### Download both 30 arc second Wordclim datasets from the website [worldclim.org](www.worldclim.org) 
#' 
#' Download these files and unzip into an HPCC folder using shell commands.  The exact folder will be be determined by the project later, but for these instructions we'll use your  ```$SCRATCH``` directory  which should have plent of space (note these will deleted in 45d).   Also note this could be done in R 
#'
#'   - old version: link addresses to data broken into two datasets 
#'      - [bioclim 1-9](https://data.biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/bio1-9_30s_bil.zip) 
#'      - [bioclim 10-19](https://data.biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/bio10-19_30s_bil.zip) 
#'   - new version: [bioclim2.1](https://data.biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_bio.zip)
#' 
#' 
#' ## TO DO
#' 1. build a generic function that takes one worldclim data set that takes parameters, then call it from a 'main' function
#' when called by the command line
#' 2. allow two different folders for worldclim data instead of 'old' and 'new'
#' Load necessary libraries 
#' 3. create  abstract functions to 1) mask to points 2) compare any two masked rasters
# --------------------------------------------------------------------------------------------------------------


library(rgdal) #reads in shapefile
library(rgeos) #allows the creation of buffered regions
library(raster) #allows you to create raster stacks and masks
library(sf) #alternative to sp for building spatial data frame from occurence coords
library(future) # parallel framework
library(future.apply) # parallel functions


# ------------------------------------------------------------------
### Options
# the rasters produced are large and may fill up the /tmp folder on the local computer
# this will not be as fast, but use the same disk as the output_path folder
# TODO create a parameter for this.  On the HPCC use the $SCRATCH disk

# rasterOptions(format, overwrite, datatype, tmpdir, tmptime, progress,
#     timer, chunksize, maxmemory, memfrac, todisk, setfileext, tolerance,
#     standardnames, depracatedwarnings, addheader,
#     default = FALSE)
# see 
#  https://discuss.ropensci.org/t/how-to-avoid-space-hogging-raster-tempfiles/864
#  https://www.gis-blog.com/increasing-the-speed-of-raster-processing-with-r-part-23-parallelisation/
#  https://strimas.com/post/processing-large-rasters-in-r/

rasterOptions(tmpdir = file.path(Sys.getenv("SCRATCH"), "raster_tmp"), 
              maxmemory=2e10)

# ------------------------------------------------------------------
### Parameters
#' This script assumes worldclim files have been downloaded and unzipped into subfolders called 'new' and 'old' 
#' under a single folder which is probably a convenient assumption.  Parameters are the locations of those files


#' path to parent folder of old and new worldclim folders
#' TODO make this platform generic
default_worldclim_path<-paste0(Sys.getenv('SCRATCH'), '/worldclim')
default_output_path <- "."

#' location of occurence record file
#' use a command line parameter 
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
    stop("requires parameter : path to csv occurence files", call.=FALSE)
} else if(length(args) < 2){
    # only 1 arg given, use default for second arg
    # default worldclim path output file
    args[2] <- default_worldclim_path
} else if (length(args) < 3) {
    args[3] <- default_output_path
}

# note, for development/testing on HPCC, use  
# csv_occ_file<- "/mnt/research/plz-lab/DATA/neotropical_frugivores/pogeo_corrected.csv"

csv_occ_file   <- args[1]
worldclim_path <- args[2]
output_path    <- args[3]


#' 
#' One thing that makes this data easier to work with is to first crop the full dataset
#'  to a smaller extent so we're not working with 1km x 1km grid cells for the whole world. 
#' 

#' Read in the species occurrence records and turn into a SpatialPointsDataFrame
#' and create a 5 degree buffer around occurrence records (should be a SpatialPointsDataFrame or SpatialPoints). 
#' 
## --------------------------------------------------------------------------------------------------------------
occ <- read.csv(csv_occ_file) 
occ.sp <- SpatialPointsDataFrame(occ[c(1,2)], as.data.frame(occ[,1])) #Makes it into spatial object 
occ.extent <- gBuffer(occ.sp, width=5)

# this crashes: 
writeOGR(occ.extent, dsn=output_path, layer="extentlarge_5degbuf", driver="ESRI Shapefile")
#'   Error in writeOGR(occ.extent, dsn = output_path, layer = "extentlarge_5degbuf",  :
#'   obj must be a SpatialPointsDataFrame, SpatialLinesDataFrame or
#'   SpatialPolygonsDataFrame


# sf version
# this works to write a file and for cropping, but it doesn't work for masking
# WHAT IS THE CRS of these points?
# occ.sf <- st_read(csv_occ_file, options=c("X_POSSIBLE_NAMES=lon", "Y_POSSIBLE_NAMES=lat"))
# occ.sf.extent <- st_buffer(occ.sf, dist=5)
# occ.sf.extent.sp <- as(occ.extent, 'Spatial') # convert to spatial 
# st_write(occ.sf.extent, output_path, layer="extentlarge_5degbuf",driver="ESRI Shapefile")
   
#' 
#' **Old worldclim data**

#' Making stack of all 19 bioclimatic variables (both downloads for the old version need to be in the same folder). 
#' The stack links all of the individual rasters in the folder together so that they can each be processed at the same time
#' 
## --------------------------------------------------------------------------------------------------------------
worldclim_path_old = file.path(worldclim_path, 'old') # assumes v1.4 data in a folder called 'old'
worldclim_files <- list.files(path = worldclim_path_old, pattern='bil$', full.names=TRUE) # pattern ensures files end with bil
climvars_list <- lapply(worldclim_files, raster)  # make list of rasters.  equiv to climvars_list[[i] <- raster(worldclimvars[i])

#' 
#' Crop and mask environmental variables by the larger extent. 
#' We crop first because masking first would take too long.
#' Using the parallel library 'future' to run all 19 in parallel otherwise crop takes a very long time
## --------------------------------------------------------------------------------------------------------------

# parallel methods for cropping and masking 
# https://cran.r-project.org/web/packages/future.apply/future.apply.pdf

future::plan(multiprocess) # use all the cores available.  should manually set num of cores based on job
climvars_cropped <- future_lapply(climvars_list,   raster::crop, y=occ.extent)
climvars_masked  <- future_lapply(climvars_cropped , raster::mask, mask=occ.extent) # this line does not complete
climvars_stacked <- stack(climvars_masked)

 
#' Data quality check
#' 
## --------------------------------------------------------------------------------------------------------------
plot(climvars_cropped[[1]])
plot(climvars_masked[[1]])
points(occ.sp, col="red", cex=0.6)

#' 
#' Write this raster to the output folder 
#' 
## --------------------------------------------------------------------------------------------------------------
writeRaster(climvars_stacked, filename = file.path(output_path,"bioclim_old_msk.tif"), format="GTiff", overwrite=T)



#' 
#' **New wordclim data**

#' 
#' Making stack of all new 19 bioclimatic variables. The stack links all of the individual rasters in the folder together so that they can each be processed at the same time
#' 
## --------------------------------------------------------------------------------------------------------------
setwd('new')
worldclim_path_new = file.path(worldclim_path, 'new')
worldclim_new <- list.files(path=worldclim_path_new, pattern='bil', full.names=TRUE)
env_new <- stack(worldclim_new)

#' 
#' Crop and mask environmental variables by the larger extent. We crop first because masking first would take too long.
#' 
## --------------------------------------------------------------------------------------------------------------
env_crop_new <- crop(env_new, extentlarge)
env_msk_new <- mask(env_crop_new, extentlarge)

#' 
#' Data quality check
## --------------------------------------------------------------------------------------------------------------
plot(env_crop_new[[1]])
plot(env_msk_new[[1]])
points(occ.sp, col="red", cex=0.6)

#' 
#' Write this raster to the same folder the other spatial data is saved
## --------------------------------------------------------------------------------------------------------------
writeRaster(env_msk_new, 
            filename= file.path(output_path,"bioclim_new_msk.tif"), 
            format="GTiff", 
            overwrite=T)
writeRaster(env_crop_new, 
            filename= file.path(output_path,"bioclim_new_crop.tif"), 
            format="GTiff", 
            overwrite=T) # can be used to project SDMs later on

#' 
#' **At this point the rasters should be small enough to process further on personal computers. **
#' 
