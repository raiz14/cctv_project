# csv - to spatial


library(readr)

a <- read.csv("data-2019-06-27_Changed.csv")
dir()


# points from scratch
coords = cbind(a$lon, a$lat)
sp = SpatialPoints(coords)
# make spatial data frame
spdf = SpatialPointsDataFrame(coords, a)
spdf = SpatialPointsDataFrame(sp, a)
# promote data frame to spatial
coordinates(spdf) = cbind(a$lon, a$lat)
coordinates(spdf) = ~lon + lat
# back to data
spdf= as.data.frame(spdf)
##      lon   lat Z
## 1 11.515 24.52 d
## 2  7.056 27.11 a
## 3 12.945 30.09 c
## 4 12.793 24.72 e
## 5 12.888 28.24 b
a@a
##   Z
## 1 d
## 2 a
## 3 c
## 4 e
## 5 b
bbox(spdf)
##      min   max
## x  7.056 12.94
## y 24.520 30.09



