# Sunapee robot quick vis

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
data_dir = paste0(path_pat, 'project_data/ASV_data/analysis_ready/SUN/')
layers_dir = paste0(path_pat, 'lake_spatial_data/NHD_shapefiles/')
map_dir = paste0(path_pat, 'project_data/ASV_data/intermediary/map_reality_check/')

# Load shapefiles of lakes ----

#get the layers inside the gdb
st_layers(file.path(layers_dir, 'NHDLakeShapefiles.gdb'))

#read in the GDB
NH <- st_read('C:/Users/steeleb/Dropbox/EPSCoR_shapefiles/NHDLakeShapefiles.gdb', 'nh_lakes')

#subset for SUNattus
SUN <- NH[NH$GNIS_Name == 'Sunapee Lake',]
# make into a simple feature
SUN <- st_as_sf(SUN)
#get the crs
st_crs(SUN)

#reporject into WGS84, UTM 19N
SUN_reproj <- st_transform(SUN, crs = 'EPSG:32619')
#check projection
st_crs(SUN_reproj)

# run loop over data to do first pass of processing, looking at loiter flag ----

#get list of robot runs
robo_runs <- list.files(data_dir)

for(i in 1:length(robo_runs)) {
  #read in robot data
  SUN_robot_data <- read.csv(file.path(data_dir, robo_runs[i]))
  #make into simple feature; crs is WGS84
  SUN_robot_georef <- st_as_sf(SUN_robot_data, coords = c('longitude_gps_deg', 'latitude_gps_deg'), crs = 'EPSG:4326')
  #reproject to WGS UTM19N
  SUN_robot_georef_reproj <- st_transform(SUN_robot_georef, 'EPSG:32619')
  #check projection
  st_crs(SUN_robot_georef_reproj)

  #get the extent of the robot run for focus
  bbox_robot = st_bbox(SUN_robot_georef_reproj)

  #make SUN basemeap from robot extent
  SUN_map = tm_shape(SUN_reproj, bbox = bbox_robot) +
    tm_borders()

  #add a layer to check projection and loiter locs
  SUN_map_robot = SUN_map +
    tm_shape(SUN_robot_georef_reproj) +
    tm_dots(col = 'loiter_flag',
            palette = c('black', 'orange')) +
    tm_layout(inner.margins = c(0.2, 0.1, 0.2, 0.1),
      title = paste0(robo_runs[i], ' quick vis'),
      legend.position = c('left', 'bottom'))
  filename = paste0(SUN_robot_data$lake[1], '_', SUN_robot_data$date[1], '_', SUN_robot_data$deployment_instance[1], '.png')
  tmap_save(SUN_map_robot, file.path(map_dir, filename))
}



# 
# 
# 
# 
# #list of variables from sonde
# variables = c('temperatureWater_degC',
#               'electricalConductivity_uscm',
#               'specificConductance_mscm',
#               'specificConductance_uscm',
#               'pH',
#               'chlorophyll_a_RFU',
#               'chlorophyll_a_ugl',
#               'blue_GreenAlgae_Cyanobacteria_Phycocyanin_ugl',
#               'oxygenDissolved_perc',
#               'oxygenDissolved_mgl',
#               'solidsTotalSuspended_mgl',
#               'turbidity_NTU')
# 
# #function to iteratively add variables over list
# make_map = function(variable) {
#   SUN_var_map = SUN_map +
#     tm_shape(SUN_robot_georef_reproj) +
#     tm_dots(col = variable)
#   print(SUN_var_map)
# }
# 
# make_map(variables)
