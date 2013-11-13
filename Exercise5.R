# Author: Sven Reulen
# Date: 13-11-2013
# Dependencies: Set direction of data in line 12 
# Description: Fourth exercise for the course Applied Geoscripting.  
# Variables:

# Load library
library(rasta)
# load data
data(taravao)
data(taravao2)
# Generate clouds
cloud1 <- calc(x=taravao[[9]], fun= QA2cloud)
cloud2 <- calc(x=taravao2[[9]], fun= QA2cloud)
# Give all not cloud values NA
cloud1[cloud1 == 0] <- NA
cloud2[cloud2 == 0] <- NA
# Drop the clouds from the data
data_date1 <- dropLayer(x=taravao, i=9)
data_date2 <- dropLayer(x=taravao2, i=9)
# GIve all clouds the value NA
data_date1[cloud1==1] <- NA
data_date2[cloud2==1] <- NA

# Function to average the two images
Mergetwoimages <- function(x, y){
    if(is.na(x)){
      return(y)
    } else{
      list = c(x,y)
        return(mean(list, na.rm=TRUE))   
    }
}

# Function to vectorize the result of the average function
VRandomFunction <- function(a, b) {
  out <- mapply(FUN=Mergetwoimages, a, b)
  return(out)
}

# Create new image
averagedimage <- overlay(data_date1, data_date2, na.rm=TRUE, fun=VRandomFunction)
plotRGB(averagedimage, 3,5,4)

