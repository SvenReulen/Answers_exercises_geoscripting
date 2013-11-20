# Author: Sven Reulen
# Date: 20-11-2013
# Dependencies
# Description: Seventh exercise of the course geoscripting
# Variables

# Libraries
library(rasta)
library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(rasterVis)
library(zoo)
library(reshape)
# Import data
data(tura)
tura <- tura/10000

# Calculate three new rasters for years 2000, 2005, 2010
sceneID <- names(tura)
sceneinfo <- getSceneinfo(sceneID)
sceneinfo$year <- format(sceneinfo$date, "%Y")
# 2000
a <- sceneinfo$year == 2000
year2000 <- subset(tura, which(a))
raster2000 <- calc(year2000, fun = mean, na.rm='TRUE')
plot(raster2000)
# 2005
b <- sceneinfo$year == 2005
year2005 <- subset(tura, which(b))
raster2005 <- calc(year2005, fun = mean, na.rm='TRUE')
plot(raster2005)
# 2010
c <- sceneinfo$year == 2010
year2010 <- subset(tura, which(c))
raster2010 <- calc(year2010, fun = mean, na.rm='TRUE')
plot(raster2010)
# Put these into a new rasterbrick with nlayers
threelayerbrick <- brick(raster2000,raster2005,raster2010)
cols <- c("yellow",  "green", "red")
?brewer.pal
rtheme <- rasterTheme(region=cols)
# plot them next to each other with good colors
levelplot(threelayerbrick[[1:3]], names.attr=c('2000', '2005',  '2010'), par.settings=rtheme)
# Produce an RGB composite with these three layers such that NDVI2000 = RED, NDVI2005=GREEN, NDVI2010 = BLUE
plotRGB(threelayerbrick, 1,2,3, stretch='hist')
# Testing which colour has the highest difference: Finding: Dark blue = big difference, a lot of colours together with dark blue in the center
##x <- click(threelayerbrick, n=1)
# Get two location extents
##e1 <- drawExtent()
##e2 <- drawExtent()
##print(e1)
##print(e2)
# Define extents
e1 <- extent(c(820300.9, 820618.6, 828258.2, 828592.3))
e2 <- extent(c(822679.8, 823038.2, 831932.4, 832176.9))
plot(e1, add=TRUE)
plot(e2, add=TRUE)
# Extract means
meanNDVIe1 <- extract(x=tura, y=e1, fun='mean')
meanNDVIe2 <- extract(x=tura, y=e2, fun='mean')
# prepare the ggplot
ts <- data.frame(sensor = getSceneinfo(names(tura))$sensor,
                 date = getSceneinfo(names(tura))$date,
                  valuese1 = meanNDVIe1,
                  valuese2 = meanNDVIe2)
with(ts, plot(date, valuese1, xlab="date", ylab="Values"))
ts$date <- as.character(ts$date)
tsmelt <- melt(ts)
names(tsmelt)[3] <- "mean"
tsmelt$date <- as.Date(tsmelt$date)
# plot the time series
ggplot(data = tsmelt, aes(x = date, y = value)) +
   geom_point() +
   scale_x_date() +
   labs(y = "NDVI") +
   facet_wrap(~ mean, nrow=2) +
   theme_bw()


