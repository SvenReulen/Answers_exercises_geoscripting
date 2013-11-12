# Author: Sven Reulen
# Date: 07-11-2013
# Dependencies: Set working directory, and define path by changing dirdat
# Description: First exercise for the course Applied Geoscripting, plotting a map of a country using a function
# Variables: dirdat = direction of data

getwd()
setwd("~/Wageningen/2.2 Geoscripting")
#download.file("http://rasta.r-forge.r-project.org/rasta.zip", file.path(dirdat, "rasta_0.7.zip"))
library(rasta)
library(raster)
dirdat <- "~/Wageningen/2.2 Geoscripting"
createmap <- function(cou, lvl){
  map <- raster::getData("GADM", country = cou, level = lvl, path = dirdat)
  plot(map, bg = "grey", axes=T,col="red")
  plot(map, add=TRUE, main = 'test')
  title(paste('Map of the following country:', cou))
}

#Testing
createmap('NLD',2)
#Test different country and level
createmap('BE',1)
#Test false input
createmap(BE,2)
createmap(BE,3)

