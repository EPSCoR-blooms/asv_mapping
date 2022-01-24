# Sabattus robot quick vis to check loiters

library(tidyverse)
library(sf)
library(tmap)

# determine OS for automatic filepath determination
os <- osVersion
path_pat = NULL

if (grepl('win', os, ignore.case = T) == T ){
  path_pat = 'Z:/'
  message('Windows OS detected')
} else if (grepl('mac', os, ignore.case = T) == T ){
  path_pat = '/Volumes/EpscorBlooms/'
  message('Mac OS detected')
} else {
  message('OS path pattern not detected. Please store OS path pattern manually.')
}

# point to directories
data_dir = paste0(path_pat, 'project_data/ASV_data/analysis_ready/SAB/')
layers_dir = paste0(path_pat, 'lake_spatial_data/NHD_shapefiles/')
map_dir = paste0(path_pat, 'project_data/ASV_data/intermediary/map_reality_check/')

# Load shapefiles of lakes ----

#get the layers inside the gdb
st_layers(file.path(layers_dir, 'NHDLakeShapefiles.gdb'))

#read in the GDB
ME <- st_read('C:/Users/steeleb/Dropbox/EPSCoR_shapefiles/NHDLakeShapefiles.gdb', 'me_lakes')

#subset for SAB
SAB <- ME[ME$GNIS_Name == 'Custer Pond',]
# make into a simple feature
SAB <- st_as_sf(SAB)
#get the crs
st_crs(SAB)

#reporject into WGS84, UTM 19N
SAB_reproj <- st_transform(SAB, crs = 'EPSG:32619')
#check projection
st_crs(SAB_reproj)

# run loop over data to do first pass of processing, looking at loiter flag ----

#get list of robot runs
robo_runs <- list.files(data_dir)

for(i in 1:length(robo_runs)) {
  #read in robot data
  SAB_robot_data <- read.csv(file.path(data_dir, robo_runs[i]))
  #make into simple feature; crs is WGS84
  SAB_robot_georef <- st_as_sf(SAB_robot_data, coords = c('longitude_gps_deg', 'latitude_gps_deg'), crs = 'EPSG:4326')
  #reproject to WGS UTM19N
  SAB_robot_georef_reproj <- st_transform(SAB_robot_georef, 'EPSG:32619')
  #check projection
  st_crs(SAB_robot_georef_reproj)

  #get the extent of the robot run for focus
  bbox_robot = st_bbox(SAB_robot_georef_reproj)

  #make SAB basemeap from robot extent
  SAB_map = tm_shape(SAB_reproj, bbox = bbox_robot) +
    tm_borders()

  #add a layer to check projection and loiter locs
  SAB_map_robot = SAB_map +
    tm_shape(SAB_robot_georef_reproj) +
    tm_dots(col = 'loiter_flag',
            palette = c('black', 'orange')) +
    tm_layout(inner.margins = c(0.2, 0.1, 0.2, 0.1),
      title = paste0(robo_runs[i], ' quick vis'),
      legend.position = c('left', 'bottom'))
  filename = paste0(SAB_robot_data$lake[1], '_', SAB_robot_data$date[1], '_', SAB_robot_data$deployment_instance[1], '.png')
  tmap_save(SAB_map_robot, file.path(map_dir, filename))
}
