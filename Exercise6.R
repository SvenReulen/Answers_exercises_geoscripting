# Author: Sven Reulen
# Date: 18-11-2013
# Dependencies: Set direction of data in line 12 
# Description: Sixth exercise for the course Applied Geoscripting.  
# Variables:

# libraries
library(rasta)
library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(rasterVis)
library(randomForest)

### 1. a plot of the original lulcGewata raster with a meaningful legend (ie. classes as characters)
# load data
data(lulcGewata)
data(LUTGewata)
# Assign right colours, plot the map with these colors and then assign a legend 
cols <- c("orange", "light green", "brown", "light pink", "dark green", "light blue")
plot(lulcGewata, main = 'Gewata', col=cols, legend=FALSE)
legend("topright", legend=LUTGewata$Class, fill=cols)
### 2. a plot of your training polygons
# Create polygons in the right areas for each class
# Cropland
cropland <- drawPoly(sp=TRUE)
cropland <- gUnion(cropland, drawPoly(sp=TRUE))
projection(cropland) <- projection(lulcGewata)
cropland <- SpatialPolygonsDataFrame(cropland, data=data.frame(class="cropland"), match.ID=FALSE)
# Bamboo
bamboo <- drawPoly(sp=TRUE)
projection(bamboo) <- projection(lulcGewata)
bamboo <- SpatialPolygonsDataFrame(bamboo, data=data.frame(class="bamboo"), match.ID=FALSE)
# Bare soil
e <- drawExtent() 
plot(lulcGewata, col=cols, legend=FALSE, ext=e)
bare_soil <- drawPoly(sp=TRUE)
projection(bare_soil) <- projection(lulcGewata)
bare_soil <- SpatialPolygonsDataFrame(bare_soil, data=data.frame(class="bare soil"), match.ID=FALSE)
# Coffee
e <- drawExtent() 
plot(lulcGewata, col=cols, legend=FALSE, ext=e)
coffee <- drawPoly(sp=TRUE)
projection(coffee) <- projection(lulcGewata)
coffee <- SpatialPolygonsDataFrame(coffee, data=data.frame(class="coffee"), match.ID=FALSE)
# Forest
forest <- drawPoly(sp=TRUE)
forest <- gUnion(forest, drawPoly(sp=TRUE))
projection(forest) <- projection(lulcGewata)
forest <- SpatialPolygonsDataFrame(forest, data=data.frame(class="forest"), match.ID=FALSE)
# Wetlands
wetlands <- drawPoly(sp=TRUE)
projection(wetlands) <- projection(lulcGewata)
wetlands <- SpatialPolygonsDataFrame(wetlands, data=data.frame(class="wetlands"), match.ID=FALSE)
# Fusing polygons  
cropland <- spChFIDs(cropland, "cropland")
bamboo <- spChFIDs(bamboo, "bamboo")
bare_soil <- spChFIDs(bare_soil, "bare soil")
coffee <- spChFIDs(coffee, "coffee")
forest <- spChFIDs(forest, "forest")
wetlands <- spChFIDs(wetlands, "wetlands")
trainingPoly <- spRbind(cropland, bamboo)
trainingPoly <- spRbind(trainingPoly, bare_soil)
trainingPoly <- spRbind(trainingPoly, coffee)
trainingPoly <- spRbind(trainingPoly, forest)
trainingPoly <- spRbind(trainingPoly, wetlands)
# Write trainingPoly away for usage next time
writeOGR(trainingPoly,dsn= 'trainingPoly', driver = 'ESRI Shapefile', layer= 'types of landuse')
# plot
plot(lulcGewata, main = 'Gewata', col=cols, legend=FALSE)
plot(trainingPoly, add=TRUE)
### 3. a summary of the resulting randomForest model object
## use the Gewata covariates as done earlier in the exercise
# load data
data(GewataB2)
data(GewataB3)
data(GewataB4)
data(vcfGewata)
# create covariates
gewata <- brick(GewataB2, GewataB3, GewataB4)
ndvi <- overlay(GewataB4, GewataB3, fun=function(x,y){(x-y)/(x+y)})
vcfGewata[vcfGewata > 100] <- NA
ndvi <- calc(ndvi, fun = function(x) floor(x*10000))
dataType(ndvi) <- "INT2U"
names(ndvi) <- "NDVI"
covs <- addLayer(gewata, ndvi, vcfGewata)
# Train the raster data
reclass <- function(x){
  which(x==levels(trainingPoly@data$class))
  }
trainingPoly@data$Code <- sapply(trainingPoly@data$class, FUN=reclass)
classes <- rasterize(trainingPoly, ndvi, field='Code')
dataType(classes) <- "INT1U"
# table of values representing all layers with known values
covmasked <- mask(covs, classes)
names(classes) <- "class"
trainingbrick <- addLayer(covmasked, classes)
# add all of these values to a data.frame representing all training data
valuetable <- getValues(trainingbrick)
valuetable <- as.data.frame(valuetable)
valuetable <- valuetable[!is.na(valuetable$class),]
valuetable$class <- factor(valuetable$class, levels = c(1:6))
valuetable$label <- with(valuetable, ifelse(class==1, "cropland",
                        ifelse(class==2, "bamboo",
                        ifelse(class==3, "bare_soil",
                        ifelse(class==4, "coffee",
                        ifelse(class==5, "forest","wetland"))))))
valuetable <- na.omit(valuetable)
modelRF <- randomForest(x=valuetable[,c(1:3)], y=valuetable$class,
                        importance = TRUE)
summary(modelRF)
### 4. the resulting thematic map with meaningful legend (as above)
predLC <- predict(covs, model=modelRF, na.rm=TRUE)
plot(predLC, main = 'Predicted landuse', col=cols, legend=FALSE)
legend("bottomright", legend=c("cropland", "bamboo", "bare soil", "coffee", "forest", "wetland"), fill=cols, bg="white")
### 5. the output OOB confusion matrix with accuracy per class (with correct row and column headings)
modelRF$confusion
colnames(modelRF$confusion) <- c("cropland", "bamboo", "bare soil", "coffee", "forest", "wetland", "class.error")
rownames(modelRF$confusion) <- c("cropland", "bamboo", "bare soil", "coffee", "forest", "wetland")
modelRF$confusion
###? 6.the variable importance ranks (mean accuracy decrease and mean Gii coefficient decrease)
varImpPlot(modelRF)

