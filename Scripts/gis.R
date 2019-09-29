# MAPS _ SPB
# 




library("readxl")

getwd()
dir()

median(df$amount)


library(ggplot2)


ggplot(data) + geom_point(data, mapping = aes(x = amount, y = district))

for (col in colnames(data){
  Encoding(data[col]) <- "UTF-8"}

summary(brr$district)

write_csv(df, "utf8.csv")

names(brr$district)
sum(brr$district)

table(brr$district,brr$amount)

sum(brr$amount by brr$district)






summary(cctv["OVDAddress"])

###
###

#Достаем набор через АПИ

library(httr)
library(jsonlite)

request <- GET(
  'https://apidata.mos.ru/v1/datasets/2386/features',
  query = list(
    api_key = 'fdefc2d7a8433ce30de0ce3e48d6a634'
  ),
  verbose()
)
response <- content(request, "text")
write_json(response, "cams.geojson")

###

install.packages("spatstat")
install.packages("maptools")
library(sp)
library(rgdal)
library(rgeos)
library(spatstat)
library(maptools)



###


library(ggplot2)
data <- data.frame(murder = USArrests$Murder, state = tolower(rownames(USArrests)))
map <- map_data("state")
l <- ggplot(data, aes(fill = murder))
l + geom_map(aes(map_id = state), map = map) + expand_limits(x = map$long, y = map$lat)


###

library(sf)
library(rgdal)

dir()
boundary_poly <- st_read("boundary-polygon-lvl8.shp")
building_p <- st_read("building-point.shp")
airlways_p <- st_read("aerialway-point.shp")
airports_poly <- st_read("airport-polygon.shp")
bulding_pnt <- st_read("building-point.shp")
bulding_poly <- st_read("building-polygon.shp")
highway_l <- st_read("highway-line.shp")
highway_crossing_p <- st_read("highway-crossing-point.shp")
land <- st_read("land.shp")
landuse_poly <- st_read("landuse-polygon.shp")
nature_reserve_poly <- st_read("nature_reserve-polygon.shp")
paking_poly <- st_read("parking-polygon.shp")
poi_point <- st_read("poi-point.shp")
power_l <- st_read("power-line.shp")
public_transp <- st_read("public-transport-line.shp")
railway_l <- st_read("railway-line.shp")
railway_station <- st_read("railway-line.shp")
railway_platforms <- st_read("railway-platform-polygon.shp")
settlement_poly <- st_read("settlement-polygon.shp")
vegetation_poly <- st_read("vegetation-polygon.shp")
water_l <- st_read("water-line.shp")
public_transport-l <- st_read("public-transport-line.shp")
surface_poly <- st_read("surface-polygon.shp")

 

# Строим графики
# library("viridis") - красоты

plot(boundary_poly %>% st_geometry, lwd = 0.5, border = 'gray')
plot(nature_reserve_poly %>% st_geometry, col = 'darkgreen', border = 'steelblue', add = TRUE)

plot(paking_poly %>% st_geometry, col = 'steelblue', border = 'steelblue', add = TRUE)

plot(cctv %>% st_geometry, col = 'steelblue1', border = 'steelblue', add = TRUE)






class(boundary)
plot(boundary)
plot(power_l, col = 'red')

###
"CCTV"
###

# Way 1
library(geojsonsf)
url <- "https://apidata.mos.ru/v1/datasets/2386/features?api_key=fdefc2d7a8433ce30de0ce3e48d6a634"
cctv <- geojson_sf(url)
cctv$Attributes <- iconv(cctv$Attributes, from = "utf-8", to = "cp1251")
plot(cctv['Attributes'], graticule = TRUE, axes = TRUE)


cctv <- st_read("data1.geojson")

# Way 2
library(readxl)
cams <- read_xlsx("data-8174-2019-06-27.xlsx")

cctv <- 

# Way 3
library(jsonlite)


CCTV <- st_read("cams1224.csv")

#recode!
CCTV$ADDRESS <- iconv(CCTV$ADDRESS, from = "utf-8", to = "cp1251")
CCTV$ADMAREA <- iconv(CCTV$ADMAREA, from = "utf-8", to = "cp1251")
CCTV$ADMAREA <- iconv(CCTV$ADMAREA, from = "utf-8", to = "cp1251")
CCTV$DISTRICT <- iconv(CCTV$DISTRICT, from = "utf-8", to = "cp1251")
CCTV$OVD_ADDRES <- iconv(CCTV$OVD_ADDRES, from = "utf-8", to = "cp1251")


# ?
library(maptools)
rm(cams)
cams <- as_Spatial(CCTV)
plot(CCTV)





library(rgdal)


## Not run:
file <- system.file("examples", "norway_maple.kml", package = "geojsonio")
# KML type file - using the web method
file_to_geojson(input=file, method='web', output='kml_web')
## read into memory
file_to_geojson(input=file, method='web', output = ":memory:")
file_to_geojson(input=file, method='local', output = ":memory:")

###

