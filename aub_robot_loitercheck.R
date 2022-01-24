# Auburn robot quick vis to check loiters

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
data_dir = paste0(path_pat, 'project_data/ASV_data/analysis_ready/AUB/')
layers_dir = paste0(path_pat, 'lake_spatial_data/NHD_shapefiles/')
map_dir = paste0(path_pat, 'project_data/ASV_data/intermediary/map_reality_check/')

# Load shapefiles of lakes ----

#get the layers inside the gdb
st_layers(file.path(layers_dir, 'NHDLakeShapefiles.gdb'))

#read in the GDB
ME <- st_read('C:/Users/steeleb/Dropbox/EPSCoR_shapefiles/NHDLakeShapefiles.gdb', 'me_lakes')

#subset for AUB
AUB <- ME[ME$GNIS_Name == 'The Basin',]
# make into a simple feature
AUB <- st_as_sf(AUB)
#get the crs
st_crs(AUB)

#reporject into WGS84, UTM 19N
AUB_reproj <- st_transform(AUB, crs = 'EPSG:32619')
#check projection
st_crs(AUB_reproj)

# run loop over data to do first pass of processing, looking at loiter flag ----

#get list of robot runs
robo_runs <- list.files(data_dir)

for(i in 1:length(robo_runs)) {
  #read in robot data
  AUB_robot_data <- read.csv(file.path(data_dir, robo_runs[i]))
  #make into simple feature; crs is WGS84
  AUB_robot_georef <- st_as_sf(AUB_robot_data, coords = c('longitude_gps_deg', 'latitude_gps_deg'), crs = 'EPSG:4326')
  #reproject to WGS UTM19N
  AUB_robot_georef_reproj <- st_transform(AUB_robot_georef, 'EPSG:32619')
  #check projection
  st_crs(AUB_robot_georef_reproj)

  #get the extent of the robot run for focus
  bbox_robot = st_bbox(AUB_robot_georef_reproj)

  #make AUB basemeap from robot extent
  AUB_map = tm_shape(AUB_reproj, bbox = bbox_robot) +
    tm_borders()

  #add a layer to check projection and loiter locs
  AUB_map_robot = AUB_map +
    tm_shape(AUB_robot_georef_reproj) +
    tm_dots(col = 'loiter_flag',
            palette = c('black', 'orange')) +
    tm_layout(inner.margins = c(0.2, 0.1, 0.2, 0.1),
      title = paste0(robo_runs[i], ' quick vis'),
      legend.position = c('left', 'bottom'))
  filename = paste0(AUB_robot_data$lake[1], '_', AUB_robot_data$date[1], '_', AUB_robot_data$deployment_instance[1], '.png')
  tmap_save(AUB_map_robot, file.path(map_dir, filename))
}
