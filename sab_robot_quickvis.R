library(tidyverse)
library(sf)
library(tmap)
library(rgdal)

## this script needs to reference DartFS file locs; increase reproducibility once robot files have CV.

#get the layers inside the gdb
ogrListLayers('C:/Users/steeleb/Dropbox/EPSCoR_shapefiles/NHDLakeShapefiles.gdb')

#read in the GDB
ME <- readOGR('C:/Users/steeleb/Dropbox/EPSCoR_shapefiles/NHDLakeShapefiles.gdb', 'me_lakes')

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
SAB_robot_data <- read.csv('C:/Users/steeleb/Downloads/2021-08-26-Sabattus.csv')
#make into simple feature; crs is WGS84
SAB_robot_georef <- st_as_sf(SAB_robot_data, coords = c('longitude', 'latitude'), crs = 'EPSG:4326')
#reproject to WGS UTM18N
SAB_robot_georef_reproj <- st_transform(SAB_robot_georef, 'EPSG:32618')
#check projection
st_crs(SAB_robot_goeref)

#make sabattus basemeap
SAB_map = tm_shape(SAB_reproj) +
  tm_borders()

#add a layer to check projection
SAB_map_robot = SAB_map +
  tm_shape(SAB_robot_georef_reproj) +
  tm_dots(col = 'pH')

SAB_map_robot

#list of variables from sonde
variables = colnames(SAB_robot_georef_reproj)[29:38]

#function to iteratively add variables over list
make_map = function(variable) {
  SAB_var_map = SAB_map +
    tm_shape(SAB_robot_georef_reproj) +
    tm_dots(col = variable)
  print(SAB_var_map)
}

make_map(variables)


         