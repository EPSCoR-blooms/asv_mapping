library(tidyverse)
library(sf)
library(tmap)

# determine OS for automatic filepath determination
os <- osVersion
path_pat = NULL

if (grepl('win', os, ignore.case = T) == T ){
  path_pat = 'Z:/'
} else if (grepl('mac', os, ignore.case = T) == T ){
  path_pat = '/Volumes/EpscorBlooms/'
} else {
  message('OS path pattern not detected. Please store OS path pattern manually.')
}

# point to directories
data_dir = paste0(path_pat, 'project_data/ASV_data/analysis_ready/SAB/')
layers_dir = paste0(path_pat, 'lake_spatial_data/NHD_shapefiles/')

#get the layers inside the gdb
st_layers(file.path(layers_dir, 'NHDLakeShapefiles.gdb'))

#read in the GDB
ME <- st_read('C:/Users/steeleb/Dropbox/EPSCoR_shapefiles/NHDLakeShapefiles.gdb', 'me_lakes')

#subset for Sabattus
SAB <- ME[ME$GNIS_Name == 'Custer Pond',]
# make into a simple feature
SAB <- st_as_sf(SAB)
#get the crs
st_crs(SAB)

#reporject into WGS84, UTM 18N
SAB_reproj <- st_transform(SAB, crs = 'EPSG:32618')
#check projection
st_crs(SAB_reproj)

#read in robot data
SAB_robot_data <- read.csv(file.path(data_dir, 'SAB_2021-08-20_asv_processed.csv'))
#make into simple feature; crs is WGS84
SAB_robot_georef <- st_as_sf(SAB_robot_data, coords = c('longitude_deg', 'latitude_deg'), crs = 'EPSG:4326')
#reproject to WGS UTM18N
SAB_robot_georef_reproj <- st_transform(SAB_robot_georef, 'EPSG:32618')
#check projection
st_crs(SAB_robot_georef_reproj)

#make sabattus basemeap
SAB_map = tm_shape(SAB_reproj) +
  tm_borders()

#add a layer to check projection
SAB_map_robot = SAB_map +
  tm_shape(SAB_robot_georef_reproj) +
  tm_dots(col = 'pH')

SAB_map_robot

#list of variables from sonde
variables = c('temperatureWater_degC',
              'electricalConductivity_uscm',
              'specificConductance_mscm',
              'specificConductance_uscm',
              'pH',
              'chlorophyll_a_RFU',
              'chlorophyll_a_ugl',
              'blue_GreenAlgae_Cyanobacteria_Phycocyanin_ugl',
              'oxygenDissolved_perc',
              'oxygenDissolved_mgl',
              'solidsTotalSuspended_mgl',
              'turbidity_NTU')

#function to iteratively add variables over list
make_map = function(variable) {
  SAB_var_map = SAB_map +
    tm_shape(SAB_robot_georef_reproj) +
    tm_dots(col = variable)
  print(SAB_var_map)
}

make_map(variables)


         