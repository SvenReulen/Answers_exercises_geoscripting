# Author: Sven Reulen
# Date: 07-11-2013
# Dependencies: 
# Description: Third exercise for the course Applied Geoscripting. 
#              Sample 30 points, retrieve the underlying temperature values, 
#              calculate the mean and plot the samplepoints and mean. 
# Variables: 

library(rasta)
library(raster)
library(spatstat)

# From lesson create the Anomaly.
filepath <- system.file("extdata", "anom.2000.03.tiff", package ="rasta")
g <- raster(filepath)
plot(g)

# Randomly sample 30 pixels
samplepoints <- runifpoint(30, win = as.vector(t(bbox(g))))
xy <- data.frame(x= samplepoints$x, y=samplepoints$y)

# Visualize the points on top of raster
plot(samplepoints, add=TRUE, col='red')

# Derive the median or st deviation of all the temperature values of the samples (hint see ?extract within the raster package)
pointswithvalues <- extract(g, xy)
mean = mean(pointswithvalues)

# add the derived, e.g. median to the plot as a text label
title(main = paste('mean = ',mean))
