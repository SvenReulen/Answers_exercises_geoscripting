# Author: Sven Reulen
# Date: 12-11-2013
# Dependencies: Set direction of data in line 12 
# Description: Fourth exercise for the course Applied Geoscripting.  
# Variables:

library(sp)
library(rgdal)
library(rgeos)
library(rasta)

##### Assignment1: Describe in your own words the function CreateHarvestTracks
# CreateHarvestTracks is a function that takes as input a data frame and an object of class CRS (Coordinate Reference System) to
# create a SpatialLinesDataFrame which consists of the tracks that a harvester has driven, these tracks include dates which are 
# ordered before input in the data frame.

# Preparation: Download file, set coordinate system, set date, create the lines,
download.file("http://rasta.r-forge.r-project.org/kroonven.csv", "kroonven.csv")
borne_data = read.table("kroonven.csv", sep = ",", header = TRUE)
coordinates(borne_data) <- c("lon.degr.","lat.degr.")
prj_string_WGS <- CRS("+proj=longlat +datum=WGS84")
borne_data@proj4string <- prj_string_WGS
prj_string_RD <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889
                      +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.2369,50.0087,465.658,
                      -0.406857330322398,0.350732676542563,-1.8703473836068,4.0812 +units=m +no_defs")
all_rd <- spTransform(borne_data, prj_string_RD)
dimnames(all_rd@coords)[[2]] <- c("x", "y")
all_rd$datetime <- as.POSIXct(paste(paste(all_rd$year, all_rd$month, all_rd$day,
                              sep="-"), paste(all_rd$hr, all_rd$min, all_rd$sec,
                              sep=":")), tz="Europe/Andorra")
all_rd <- as.data.frame(all_rd)
all_rd <- all_rd[order(all_rd$datetime),]
tracksaslines <- CreateHarvestTracks(all_rd, prj_string_RD)

#1. buffer the cleaned lines to create harvest blocks, i.e. blocks where a particular load came from;
Harvested_areas <- gBuffer(tracksaslines,byid=T, width=0.5*tracksaslines$width,capStyle="Round")

#2. Fill small holes within these blocks by swelling and shrinking;
# Swelling
Harvested_areas <- gBuffer(Harvested_areas, byid=T,id=rownames(Harvested_areas), width = 2.0)
# Shrinking
Harvested_areas <- gBuffer(Harvested_areas, byid=T,id=rownames(Harvested_areas), width = -2.0)

#3. compute for each block the yield per hectare and add this attribute to the spatial polygons data frame;
areas <- gArea(Harvested_areas, byid=T)
areas_in_ha <- areas/10000
yieldsperhectar<-Harvested_areas$loads/areas_in_ha
Harvested_areas$yieldsperhectare <-yieldsperhectar 

#4. make a map showing the the yield per hectare of each block, using spplot;
spplot(Harvested_areas, zcol="yieldsperhectare", lwd=1.5, col = "orange")

#5. export the spatial polygons data frame to display polygon boundaries in Google Earth using writeOGR;
Harvested_areas_WGS <- spTransform(Harvested_areas, prj_string_WGS)
setwd("~/Wageningen/2.2 Geoscripting")
writeOGR(Harvested_areas_WGS, file.path("data", "bordersareas.kml"), "bordersareas", driver="KML", overwrite_layer=TRUE)

