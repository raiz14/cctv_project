# Code to create all OSM objects dataset (part 1 of all)
# By: Dmitriy Serebrennikov

# Install the required packages
pkgs <- c("sf", "dplyr", "data.table", "mapview", "geojsonsf", "geojsonio", "geojson", "lubridate", "units", "stringi", "mapview", "stringr")

if (length(setdiff(pkgs, rownames(installed.packages()))) > 0) {
  
  install.packages(setdiff(pkgs, rownames(installed.packages())))
  
}

# Load packages
lapply(pkgs, library, character.only = T)

setwd("D:/serebrennikov/serebrennikov_cctv_project/data")

# Parameters
poi_cutoff <- 1 # metre cutoff around POI for join them to buildings
units(poi_cutoff) <- with(ud_units, m)
projection_used <- 3857 # projection to use — EPSG:3857, Web-Mercator
subset_data_district <- "" # Subset the data (leave empty if not)

# OpenStreetMap GeoJSONs data load
# Data purchased from https://data.nextgis.com/ru/region/RU-MOW/
# For all datasets add a column with type of spatial objects and select only the necessary column
standard_columns <- c("NAME", "NAME_RU","NAME_EN", "OSM_ID", "OSM_TYPE", "geometry")

poi <- st_read("poi-point.shp", stringsAsFactors = F) %>%
  mutate(geom_type = "poi_point") 

poi_polygon <- st_read("poi-polygon.shp", stringsAsFactors = F, promote_to_multi = F) %>%
  st_cast(., "POLYGON") %>% # st_cast is necessary. Without this, the type of geometry would not change
  mutate(geom_type = "poi_polygon") 

public_transport_point <- st_read("public-transport-point.shp", stringsAsFactors = F) # Use column: RAILWAY, HIGHWAY and standard columns
public_transport_point <- public_transport_point %>%
  select(standard_columns, RAILWAY, HIGHWAY) %>% 
  mutate(geom_type = "public_transport_point")

building_polygon <- st_read("building-polygon.shp", stringsAsFactors = F, promote_to_multi = F) # Use column: BUILDING and standard columns
building_polygon <- building_polygon %>% 
  select(standard_columns, BUILDING)  %>% 
  st_cast(., "POLYGON") %>% # st_cast is necessary. Without this, the type of geometry would not change
  mutate(geom_type = "building_polygon") 
# Fix one orphaned category
building_polygon[building_polygon$BUILDING == "garage", "BUILDING"] <- "garages"

building_point <- st_read("building-point.shp") # BUILDING and standard columns
# Make correction for a category "Toilets"
building_point$BUILDING <- as.character(building_point$BUILDING)
building_point$BUILDING <- replace(building_point$BUILDING, building_point$NAME == "Городской туалет", "toilets") 
building_point <- building_point %>% 
  select(standard_columns, BUILDING)  %>% 
  mutate(geom_type = "building_point") %>% 
  rename("BUILDING_POINT" = "BUILDING")

parking_polygon <- st_read("parking-polygon.shp") # # Use column: PARKING and standard columns
# Use only open-space parking
parking_polygon <- parking_polygon %>% 
  filter(PARKING == "surface") %>%  
  select(standard_columns,PARKING) %>% 
  st_cast(., "POLYGON") %>% # st_cast is necessary. Without this, the type of geometry would not change
  mutate(geom_type = "parking_polygon") 

railway_station_point <- st_read("railway-station-point.shp") #  RAILWAY and standard columns
railway_station_point <- railway_station_point  %>%  
  select(standard_columns,RAILWAY) %>% 
  mutate(geom_type = "railway_station_point")

# District boundaries data load
district_boundaries <- st_read("boundary-polygon-lvl8.shp", stringsAsFactors = F)

# Set projection of choice for spatial objects
spatial_objects <- c("district_boundaries","poi", "poi_polygon", "public_transport_point", "building_polygon", "building_point", "parking_polygon", "railway_station_point")

transform_spat <- function(x){
  x <- st_as_sf(x)
  x <- st_transform(x, crs = projection_used, proj4string = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")
}

for(v in spatial_objects) {
  
  assign(v, transform_spat(get(v)))
  
}

#####################

# Spatial subset if this option is specified in parameters

if( nchar(subset_data_district) > 0 ) {
  
  ## Data restriction: consider only Northwestern district of Moscow for debugging purposes
  north_west_district_boundaries <- district_boundaries[ district_boundaries$ADMIN_L5 == "Северо-Западный административный округ",] %>%
    st_combine
  
  # Extend the boundary by the cctv_cutoff size to make sure that we capture all the objects
  # neighbouring the district boundary
  north_west_district_boundaries <- st_buffer(north_west_district_boundaries, cctv_cutoff)
  
  for(v in spatial_objects) {
    
    assign(v, get(v)[st_intersects(get(v), north_west_district_boundaries, sparse = F),])
    
  }
  
}

#####################

# Select all SHOPs from the dataset by POI. This is necessary for correct analysis. At the end of this code, we will attach SHOPs as points to a ready- made dataset using rbind. Without this in the current design the SHOPs will be spoil the results of the regression and make them irrelevant. There are also SHOPs values that we need to leave - "art", "mall", " fuel" and "supermarket" in poi_polygon
setDT(poi)
poi_point_shop <- poi[!is.na(poi$SHOP) & !(poi$SHOP == "mall")& !(poi$SHOP == "art")& !(poi$SHOP == "fuel"), ]   # Errors without hardcoding!
poi <- transform_spat(poi[!(OSM_ID %in% poi_point_shop$OSM_ID)])
poi_point_shop <- poi_point_shop[poi_point_shop$SHOP == "supermarket", ]

# The same whith POI polygons
setDT(poi_polygon)
poi_polygon_shops <- poi_polygon[!is.na(poi_polygon$SHOP) & !(poi_polygon$SHOP == "mall")& !(poi_polygon$SHOP == "art") & !(poi_polygon$SHOP == "fuel") & !(poi_polygon$SHOP == "supermarket"), ]  # Errors without hardcoding!
poi_polygon <- transform_spat(poi_polygon[!(OSM_ID %in% poi_polygon_shops$OSM_ID)]) 
# The same whith "POI polygons"waste" categories

poi_point_waste <- poi %>% filter(AMENITY == "recycling" |  AMENITY == "waste_basket" | AMENITY == "bench" | AMENITY == "waste_disposal")
poi <- transform_spat(as.data.table(poi)[!(OSM_ID %in% poi_point_waste$OSM_ID)])

#
poi_polygon_waste <- poi_polygon %>% filter(AMENITY == "recycling" |  AMENITY == "waste_basket" | AMENITY == "bench" | AMENITY == "waste_disposal")
poi_polygon <- transform_spat(as.data.table(poi_polygon)[!(OSM_ID %in% poi_polygon_waste$OSM_ID)]) 



# Rbind to all poi_shop
poi_shops <- poi_point_shop

#####################
#####################
#####################

# Combine POIs and building polygons
# 
## Row-bind POI points, public transport points and other "point" data. We need to convert to data.table
poi_public_and_transport_points <-
  st_sf(rbind(
    as.data.table(poi),
    as.data.table(public_transport_point),
    as.data.table(building_point),
    as.data.table(railway_station_point),
    fill = T
  ))

## Remove duplication in data sets in terms of the same OSM_ID
## (about 60 points)
poi_public_and_transport_points <- unique(poi_public_and_transport_points, by = "OSM_ID")

# Row-bind building and parking polygons
building_and_parking_polygon <-
  st_sf(rbind(
    as.data.table(building_polygon),
    as.data.table(parking_polygon), # Add parking polygon
    fill = T
  ))

## Inner join POI/transport points with POI polygons
## Cutoff for poi = 1 m.
poi_points_matched_to_building_poly <-
  st_join(
    building_and_parking_polygon,
    st_buffer(poi_public_and_transport_points, poi_cutoff),
    join = st_intersects,
    left = F
  )
## In this object we have all the points intersecting polygons


# Make a dataset only with geometries
setDT(poi_points_matched_to_building_poly)
match_to_b_geom <-
  unique(poi_points_matched_to_building_poly[, c("OSM_ID.x", "geometry")], by = "OSM_ID.x")

# Aggregate all pois to building
# Some columns have equal values and if aggregate them, they would repeat one name a lot of times. In this case, make a unchangeable columns and changeable columns. Aggregate differently
# 
# Without this, we get a repeat of geometries, because now the relationship of the lines is "one polygon" - "one point". If for one polygon there is an intersection (for example) with three points,  this will be written as three rows with repeating geometries. We bring the data to the relation "one unique polygon" - "all points inside it"

# 
unchangeable_cols <- c("NAME.x", "NAME_RU.x", "NAME_EN.x", "OSM_ID.x", "OSM_TYPE.x", "BUILDING" , "geom_type.x", "OSM_TYPE.y", "geom_type.y","PARKING")
changeable_cols <- c("OSM_ID.x","NAME.y","NAME_EN.y","NAME_RU.y","MAN_MADE","LEISURE","AMENITY","OFFICE", "SHOP","TOURISM","SPORT","OSM_ID.y", "RAILWAY", "HIGHWAY","BUILDING_POINT")

poi_points_matched_to_building_poly[,] <-
  lapply(poi_points_matched_to_building_poly[,], as.character)
setDT(poi_points_matched_to_building_poly)
poi_b_p_1 <-
  unique(poi_points_matched_to_building_poly[, ..unchangeable_cols], by = "OSM_ID.x")


poi_points_matched_to_building_poly <-poi_points_matched_to_building_poly[,..changeable_cols] %>% 
  group_by(OSM_ID.x) %>%
  summarise_all(funs(paste(na.omit(.), collapse = "|"))) %>% 
  merge(.,poi_b_p_1, by="OSM_ID.x") %>% 
  merge(.,match_to_b_geom, by="OSM_ID.x") 

# 

poi_points_matched_to_building_poly <- transform_spat(poi_points_matched_to_building_poly)

### Concatenate non-missing column content from points and poly for matched attributes
### We concatenate them by "|"
column_names_to_concat_for_build <-
  names(poi_points_matched_to_building_poly)[grepl("\\.y$", names(poi_points_matched_to_building_poly))]
column_names_to_concat_for_build <-
  gsub(".y", "", column_names_to_concat_for_build, fixed = T)

### Since it is a destructive operation, store the results first in a temporary object
temp_b <- copy(poi_points_matched_to_building_poly)
setDT(temp_b)


for(v in column_names_to_concat_for_build) {
  
  set(temp_b, j = v, value = ifelse(is.na(temp_b[[paste0(v, ".y")]]), temp_b[[paste0(v, ".x")]], paste(temp_b[[paste0(v, ".x")]], temp_b[[paste0(v, ".y")]] , sep = "|") ) )
  # Remove NAs
  set(temp_b, j = v, value = gsub("|NA|", "", temp_b[[v]], fixed = T))
  set(temp_b, j = v, value = gsub("|NA", "", temp_b[[v]], fixed = T))
  set(temp_b, j = v, value = gsub("NA|", "", temp_b[[v]], fixed = T))
  # Remove redundant columns
  set(temp_b, j = paste0(v, ".x"), value = NULL)
  set(temp_b, j = paste0(v, ".y"), value = NULL)
  
}

### Back to the matched object
temp_b <- st_sf(temp_b[, c("geometry", column_names_to_concat_for_build), with = F]) 

tag_column_b <- c("MAN_MADE","LEISURE","AMENITY","OFFICE","SHOP","TOURISM","SPORT", "RAILWAY", "HIGHWAY", "BUILDING_POINT","BUILDING", "PARKING")
setDT(temp_b)
setDT(poi_points_matched_to_building_poly)

poi_points_matched_to_building_poly <- st_sf(cbind(temp_b,poi_points_matched_to_building_poly[,..tag_column_b]))


# Delete "yes" in tag column.
setDT(poi_points_matched_to_building_poly)

for(v in tag_column_b) {
  
  set(poi_points_matched_to_building_poly, j = v, value = gsub("|yes", "", poi_points_matched_to_building_poly[[v]], fixed = T))
  set(poi_points_matched_to_building_poly, j = v, value = gsub("|yes|", "", poi_points_matched_to_building_poly[[v]], fixed = T))
  set(poi_points_matched_to_building_poly, j = v, value = gsub("yes|", "", poi_points_matched_to_building_poly[[v]], fixed = T))
  set(poi_points_matched_to_building_poly, j = v, value = gsub("yes", "", poi_points_matched_to_building_poly[[v]], fixed = T))

}

# Empty strings to NA
for(j in names(poi_points_matched_to_building_poly) ) {
  
  set(poi_points_matched_to_building_poly, i = grep("^$|^ $|^NA$", poi_points_matched_to_building_poly[[j]]), j = j, value = NA_character_)
  
}

### OSM_IDs for objects in POI points and building polygons that were intersecting
intersecting_osm_ids_poi_and_buildings <-
  unique(c(
    stri_split_fixed(poi_points_matched_to_building_poly$OSM_ID, "|", simplify = T)
  ))

## Row-bind of POI/transport points and polygons

poi_buildings_parking <-
  rbind(
    as.data.table(poi_public_and_transport_points)[!(OSM_ID %in% intersecting_osm_ids_poi_and_buildings)],
    # all POI points not intersecting with POI polygons
    as.data.table(building_and_parking_polygon)[!(OSM_ID %in% intersecting_osm_ids_poi_and_buildings)],
    # all building and parking polygons not intersecting with POI points
    as.data.table(poi_points_matched_to_building_poly), # all POI points and polygons that are intersecting within cctv_other_cutoff
#    as.data.table(poi_shops), # POI shops to use them as separate objects from intersect POIs. Now we mute it
    fill = T
  )

### This object contains incompatible geometry types:
### table(droplevels(st_geometry_type(all_osm_objects$geometry)))
### and direct operations can be performed only on per geometry type basis  
# Split poi_buildings_parking into two data frames with Points and Polygon geometry type

# FYI: how to select by geometry type:
# setDT(poi_buildings_parking)
# poi_buildings_parking_POINT <- st_sf(poi_buildings_parking[st_geometry_type(poi_buildings_parking$geometry) == "POINT"])
# poi_buildings_parking_POLYGON <- st_sf(poi_buildings_parking[st_geometry_type(poi_buildings_parking$geometry) == "POLYGON"])
# 
# Good, but further there are errors on this way and I don't know how to solve them.
# Another and working variant

poi_buildings_parking <- transform_spat(poi_buildings_parking)
poi_buildings_parking$st_type <- st_geometry_type(poi_buildings_parking)
poi_buildings_parking_POINT <- poi_buildings_parking %>% 
  filter(st_type == "POINT") %>% 
  select(.,-st_type)
poi_buildings_parking_POLYGON <- poi_buildings_parking %>% 
  filter(st_type == "POLYGON") %>% 
  select(.,-st_type)
poi_buildings_parking <- select(poi_buildings_parking,-st_type)


# 
# Combine poi_buildings_parking and poi_polygons
# 
## Join poi_buildings_parking data frame with POI geometry polygons
all_matched_to_poi_poly <- st_sf(rbind(
  as.data.table(st_join(poi_buildings_parking_POINT,poi_polygon, join = st_intersects, left = F)),
  as.data.table(st_join(poi_buildings_parking_POLYGON,poi_polygon, join = st_intersects, left = F)),
  fill = T
))
setDT(all_matched_to_poi_poly)

### Concatenate non-missing column content from points and poly for matched attributes
### We concatenate them by "|"
column_names_to_concat <- names(all_matched_to_poi_poly)[grepl("\\.y$", names(all_matched_to_poi_poly))]
column_names_to_concat <- gsub(".y", "", column_names_to_concat, fixed = T)

### Since it is a destructive operation, store the results first in a temporary object
temp <- copy(all_matched_to_poi_poly)
setDT(all_matched_to_poi_poly)

for(v in column_names_to_concat) {
  
  set(temp, j = v, value = ifelse(is.na(temp[[paste0(v, ".y")]]), temp[[paste0(v, ".x")]], paste(temp[[paste0(v, ".x")]], temp[[paste0(v, ".y")]] , sep = "|") ) )
  set(temp, j = v, value = gsub("|NA|", "", temp[[v]], fixed = T))
  set(temp, j = v, value = gsub("|NA", "", temp[[v]], fixed = T))
  set(temp, j = v, value = gsub("NA|", "", temp[[v]], fixed = T))
  # Remove redundant columns
  set(temp, j = paste0(v, ".x"), value = NULL)
  set(temp, j = paste0(v, ".y"), value = NULL)
  
}

### Back to the matched object
temp <- st_sf(temp[, c("geometry", column_names_to_concat), with = F]) # minus column to concat in temp and cbind other columns!!!
tag_column <- c("RAILWAY", "HIGHWAY", "BUILDING_POINT","BUILDING", "PARKING")
setDT(temp) #!
all_matched_to_poi_poly <- cbind(temp,all_matched_to_poi_poly[,..tag_column])
# all_matched_to_poi_poly <- copy(temp)


# Delete "yes" in tag column.
setDT(all_matched_to_poi_poly)

# Delete yes in BUILDING and BUILDING_POINT column.
# Account for |-separated "yes", for instance: "yes|school"
# all_osm_objects[, BUILDING := gsub("^yes$|\\|yes\\||^yes\\||\\|yes$", "", BUILDING, perl = T)]
# all_osm_objects[, BUILDING_POINT := gsub("^yes$|\\|yes\\||^yes\\||\\|yes$", "", BUILDING_POINT, perl = T)]

for(v in tag_column_b) {
  
  set(all_matched_to_poi_poly, j = v, value = gsub("|yes", "", all_matched_to_poi_poly[[v]], fixed = T))
  set(all_matched_to_poi_poly, j = v, value = gsub("|yes|", "", all_matched_to_poi_poly[[v]], fixed = T))
  set(all_matched_to_poi_poly, j = v, value = gsub("yes|", "", all_matched_to_poi_poly[[v]], fixed = T))
  set(all_matched_to_poi_poly, j = v, value = gsub("yes", "", all_matched_to_poi_poly[[v]], fixed = T))
  
}

# Empty strings to NA
for(j in names(all_matched_to_poi_poly) ) {
  
  set(all_matched_to_poi_poly, i = grep("^$|^ $|^NA$", all_matched_to_poi_poly[[j]]), j = j, value = NA_character_)
  
}

### OSM_IDs for objects in POI points and POI polygons that were intersecting
intersecting_osm_ids_poi_points_polygons <- unique(c(stri_split_fixed(all_matched_to_poi_poly$OSM_ID, "|", simplify = T)))

### Create park data frame for rbind it. Also add parks OSM_IDs to vector above
setDT(poi_polygon)
park_polygon <- poi_polygon[LEISURE == "park"] %>% 
  mutate(geom_type = "park_polygon") 
Intersecting_osm_ids_poi_and_park_points_polygons <- c(intersecting_osm_ids_poi_points_polygons, park_polygon$OSM_ID)


## Final row-bind of POI/transport points and polygons
all_OSM_objects <-
  rbind(
    as.data.table(poi_buildings_parking)[!(OSM_ID %in% intersecting_osm_ids_poi_points_polygons)],
    # all POI points not intersecting with POI polygons
    as.data.table(poi_polygon)[!(OSM_ID %in% Intersecting_osm_ids_poi_and_park_points_polygons)],
    # all POI polygons not intersecting with POI points
    as.data.table(all_matched_to_poi_poly), # all POI points and polygons that are intersecting within cctv_other_cutoff
    as.data.table(park_polygon), # all parks 
    fill = T
  )

# Final corrections and creation shp files
all_OSM_objects$local_id <- seq(1:nrow(all_OSM_objects))
all_OSM_objects <- all_OSM_objects[,c("local_id", "NAME", "NAME_EN", "NAME_RU", "MAN_MADE", "LEISURE", "AMENITY", "OFFICE", "SHOP", "TOURISM", "SPORT", "RAILWAY","HIGHWAY","BUILDING_POINT", "BUILDING", "PARKING", "OSM_TYPE", "OSM_ID", "geom_type", "geometry")] 


### NB: this object contains incompatible geometry types:
### table(droplevels(st_geometry_type(all_osm_objects$geometry)))
### and direct operations can be performed only on per geometry type basis

## OSM objects
save(all_OSM_objects, file = paste0("Moscow_osm_objects_without_bench", "_", poi_cutoff, "_", subset_data_district, ".rdata"), compress = "xz")

