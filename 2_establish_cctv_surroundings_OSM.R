# Code to establish cctv surroundings OSM objects (part 2 of all)
# By: Dmitriy Serebrennikov

# Install the required packages
pkgs <- c("sf", "dplyr", "data.table", "mapview", "geojsonsf", "geojsonio", "geojson", "lubridate", "units", "stringi", "mapview", "stringr")

if (length(setdiff(pkgs, rownames(installed.packages()))) > 0) {
  
  install.packages(setdiff(pkgs, rownames(installed.packages())))
  
}

# Load packages
lapply(pkgs, library, character.only = T)

setwd("D:/serebrennikov/serebrennikov_cctv_project/data")

cctv_cutoff <- 50 # metre cutoff around camera for POI to be considered in the vicinity
units(cctv_cutoff) <- with(ud_units, m)
OSM_objects_cutoff <- 50 # metre cutoff around OSM objects for other OSM objects to be considered in the vicinity
units(OSM_objects_cutoff) <- with(ud_units, m)


projection_used <- 3857 # projection to use — EPSG:3857, Web-Mercator
subset_data_district <- "All_data" # Subset the data (leave empty if not)

# Load combined to one dataset OpenStreetMap GeoJSONs data
load("Moscow_osm_objects_without_bench_1_.rdata")
all_OSM_objects$PARKING <- as.character(all_OSM_objects$PARKING)
setDT(all_OSM_objects)
all_OSM_objects$PARKING <- gsub("surface", "parking", all_OSM_objects$PARKING)
table(all_OSM_objects$PARKING)

# Split dataset into two according to two types of geometry that it contains

transform_spat <- function(x){
  x <- st_as_sf(x)
  x <- st_transform(x, crs = projection_used, proj4string = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")
}

all_OSM_objects <- transform_spat(all_OSM_objects)
all_OSM_objects$st_type <- st_geometry_type(all_OSM_objects)
all_OSM_objects_POINT <- all_OSM_objects %>% 
  filter(st_type == "POINT")
all_OSM_objects_POLYGON <- all_OSM_objects %>% 
  filter(st_type == "POLYGON")

# District boundaries data load
district_boundaries <- st_read("boundary-polygon-lvl8.shp", stringsAsFactors = F)

district_boundaries <- district_boundaries %>% select(ADMIN_L5, geometry) %>% group_by(ADMIN_L5) %>% summarise()
district_boundaries <- district_boundaries[-13,]
district_boundaries$AO <- c("Eastern_AO", "Western_AO", "Zelenograd_AO", "Novomoskovsky_AO", "Northern _AO", "NorthEastern_AO", "NorthWestern_AO", "Troitsk_AO", "Central_AO", "SouthEastern_AO", "SouthWestern_AO", "Southern_AO")
district_boundaries <- select(district_boundaries, -ADMIN_L5)

# Public spaces

load("public_spaces_point.rdata")
public_spaces_point <- public_spaces_point %>% transform_spat() %>% select(PUBLIC, geometry) %>% st_buffer(., 100)

# Camera locations data load
# Data collect from the open city government source "Open data" by Moscow city government.
# 
# Link to datasets:
# Surveillance cameras in places of mass congestion of people (in the further code - "Street cameras") (10.01.2020 version): https://data.mos.ru/opendata/7710878000-registry-of-surveillance-cameras-in-places-of-mass-congestion-of-people
# 
# Load changed cctv datasets
## Street cameras
 cctv_street <- st_read("D:/serebrennikov/serebrennikov_cctv_project/data/cctv_2020_01_10.geojson", stringsAsFactors = F) 

# Keep only columns of interest
 columns_of_interest <- c("ID", "AdmArea", "District", "OVDAddress", "geometry")
 cctv_street <- cctv_street[, columns_of_interest]
# Add camera types variable
 cctv_street$cctv_type <- "cctv_street"
# Read in previously created camera ID-time emerged in data information
# TODO: write proper code that creates this file (will be later)
# put it in a separate .R file, and reference in here
 cctv_date_appeared_in_data <- fread("cctv_time.csv", encoding = "UTF-8")
 cctv_date_appeared_in_data$earliest_version <- ymd(cctv_date_appeared_in_data$earliest_version)
# Add the column with information on when the camera appeared in the data
 cctv_street$earliest_version <- cctv_date_appeared_in_data[match(cctv_street$ID, cctv_date_appeared_in_data$ID)]$earliest_version
# Unit test: every camera has its earliest date assigned
 all(nrow(cctv_street[is.na(cctv_street$earliest_version),]) == 0)
# Rename ID variable in camera data to avoid ambiguities with OSM data
 cctv_street <- cctv_street %>% rename("cctv_ID" = "ID")

# Set projection of choice for spatial objects
spatial_objects <- c("cctv_street", "all_OSM_objects_POINT", "all_OSM_objects_POLYGON", "district_boundaries", "public_spaces_point")

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
#####################
#####################

# Row-bind all camera data sets
cctv_all <- cctv_street

# If you want to work with subset data, then:
# all_OSM_objects <- rbind(all_OSM_objects_POINT,all_OSM_objects_POLYGON)

#####################

### Add intersection of OSM objects with cctv

# Transform to sf all needed spatial datasets
spatial_objects_for_cctv_intersection <- c("cctv_street", "all_OSM_objects_POLYGON", "all_OSM_objects_POINT")

for(v in spatial_objects_for_cctv_intersection) {
  
  assign(v, transform_spat(get(v)))
  
}

###
# Intersect points and polygons OSM objects with all types of cctv differently. Then use rbind for them.
# Cutoff for sreet cameras = 50 m.

intersects_cctv_osm_objects <- rbind(
  as.data.table(st_join(st_buffer(cctv_street, cctv_cutoff), all_OSM_objects_POLYGON, join = st_intersects, left = F)),
  as.data.table(st_join(st_buffer(cctv_street, cctv_cutoff), all_OSM_objects_POINT, join = st_intersects, left = F)),
  as.data.table(st_join(st_buffer(cctv_street, cctv_cutoff), public_spaces_point, join = st_intersects, left = F)),
  as.data.table(st_join(st_buffer(cctv_street, cctv_cutoff), district_boundaries, join = st_intersects, left = T)),
  fill = T
) %>% 
  select(., -c(geometry)) %>% # Remove geometry, because we will not use it further.
  mutate(st_type = "POINT", geom_type = "cctv_point", OSM_TYPE = NA_character_) # Make a spetial values of cctv-OSM_objects intersection for this column.

# Aggregate dataframe by the "cctv_ID". 
# Now we also have changeable and unchangeable columns for which we use different aggregation (by unique value and by "paste" all the value separated by "|") 
setDT(intersects_cctv_osm_objects)
intersects_cctv_osm_objects[,] <-
  lapply(intersects_cctv_osm_objects[,], as.character)

unchangeable_cctv_osm_objects_agregate_cols <- c("cctv_ID", "AdmArea", "District", "OVDAddress", "cctv_type", "earliest_version", "st_type", "geom_type")
changeable_cctv_osm_objects_agregate_cols <- c("cctv_ID", "local_id", "NAME", "NAME_EN", "NAME_RU", "MAN_MADE", "LEISURE", "AMENITY", "OFFICE", "SHOP", "TOURISM", "SPORT", "RAILWAY", "HIGHWAY", "BUILDING_POINT", "BUILDING", "PARKING", "OSM_ID", "PUBLIC","AO")

# Aggregation
intersects_cctv_osm_objects <- unique(intersects_cctv_osm_objects[, ..unchangeable_cctv_osm_objects_agregate_cols], by = "cctv_ID") %>% 
  merge(.,intersects_cctv_osm_objects[,..changeable_cctv_osm_objects_agregate_cols] %>%           group_by(cctv_ID) %>%
          summarise_all(funs(paste(na.omit(.), collapse = "|"))) , by="cctv_ID") 

# Check it
# length(unique(intersects_cctv_osm_objects$cctv_ID))
# View(head(intersects_cctv_osm_objects))

# Empty strings to NA
for(j in names(intersects_cctv_osm_objects) ) {
  
  set(intersects_cctv_osm_objects, i = grep("^$|^ $|^NA$", intersects_cctv_osm_objects[[j]]), j = j, value = NA_character_)
  
}

#####################

# Add intersections OSM-objects without cctv with all OSM-objects to get a spatial context of these OSM-objects

### local_id-s for objects in OSM-objects that were intersecting
intersects_cctv_osm_objects_LOCAL_ids <- unique(c(stri_split_fixed(intersects_cctv_osm_objects$local_id, "|", simplify = T)))

### cctv_id-s for cameras in cctv_all that were intersecting
intersects_cctv_osm_objects_CCTV_ids <- intersects_cctv_osm_objects$cctv_ID

# Make a data frame of OSM-objects without cctv around
all_OSM_objects_without_CCTV <- transform_spat(as.data.table(all_OSM_objects)[!(local_id %in% intersects_cctv_osm_objects_LOCAL_ids)])

# Split into POINT and POLYGON types of geomtery
all_OSM_objects_without_CCTV$st_type <- st_geometry_type(all_OSM_objects_without_CCTV)
all_OSM_objects_without_CCTV_POINT <- all_OSM_objects_without_CCTV %>% 
  filter(st_type == "POINT") %>% 
  select(., -st_type)
all_OSM_objects_without_CCTV_POLYGON <- all_OSM_objects_without_CCTV %>% 
  filter(st_type == "POLYGON") %>% 
  select(., -st_type)

# Geometre data frame for merge it with cctv geometry finally
all_OSM_objects_without_CCTV_GEOM <- rbind(
  as.data.table(all_OSM_objects_without_CCTV_POINT)[,c("local_id", "geometry")],
  as.data.table(st_centroid(all_OSM_objects_without_CCTV_POLYGON))[,c("local_id", "geometry")],
  fill = T
)

# Create data frames with nessessary column for st_join
columns_for_intersection <- c("MAN_MADE", "LEISURE", "AMENITY", "OFFICE", "SHOP", "TOURISM", "SPORT", "RAILWAY", "HIGHWAY", "BUILDING_POINT", "BUILDING", "PARKING", "geometry")
all_OSM_objects_POINT_for_OSM_OSM_join <- transform_spat(as.data.table(all_OSM_objects_POINT)[,..columns_for_intersection])
all_OSM_objects_POLYGON_for_OSM_OSM_join <- transform_spat(as.data.table(all_OSM_objects_POLYGON)[,..columns_for_intersection])

# Intersect OSM-objects without cctv around with all OSM objects
intersects_osm_osm_objects <- rbind(
  as.data.table(st_join(
    st_buffer(all_OSM_objects_without_CCTV_POINT, OSM_objects_cutoff),
    all_OSM_objects_POINT_for_OSM_OSM_join,
    join = st_intersects, left = T)),
  as.data.table(st_join(
    st_buffer(all_OSM_objects_without_CCTV_POINT, OSM_objects_cutoff),
    all_OSM_objects_POLYGON_for_OSM_OSM_join,
    join = st_intersects, left = T)),  
  as.data.table(st_join(
    st_buffer(all_OSM_objects_without_CCTV_POINT, OSM_objects_cutoff), 
    public_spaces_point, join = st_intersects, left = T)),
  as.data.table(st_join(
    all_OSM_objects_without_CCTV_POINT, 
    district_boundaries, join = st_intersects, left = T)),
  as.data.table(st_join(
    st_buffer(all_OSM_objects_without_CCTV_POLYGON, OSM_objects_cutoff),
    all_OSM_objects_POINT_for_OSM_OSM_join,
    join = st_intersects, left = T)),
  as.data.table(st_join(
    st_buffer(all_OSM_objects_without_CCTV_POLYGON, OSM_objects_cutoff),
    all_OSM_objects_POLYGON_for_OSM_OSM_join,
    join = st_intersects, left = T)),
  as.data.table(st_join(
    st_buffer(all_OSM_objects_without_CCTV_POLYGON, OSM_objects_cutoff), 
    public_spaces_point, join = st_intersects, left = T)),
  as.data.table(st_join(
    all_OSM_objects_without_CCTV_POLYGON, 
    district_boundaries, join = st_intersects, left = T)),
  fill = T) %>% 
  select(., -c(geometry)) # Remove geometry, because we will not use it further.

# Aggregate dataframe by the "local_id". 
# Now we also have changeable and unchangeable columns for which we use different aggregation (by unique value and by "paste" all the value separated by "|") 
intersects_osm_osm_objects[,] <-
  lapply(intersects_osm_osm_objects[,], as.character)

unchangeable_osm_osm_objects_cols <- c("local_id", "NAME", "NAME_EN", "NAME_RU", "MAN_MADE.x", "LEISURE.x", "AMENITY.x", "OFFICE.x", "SHOP.x", "TOURISM.x", "SPORT.x", "RAILWAY.x", "HIGHWAY.x", "BUILDING_POINT.x", "BUILDING.x", "PARKING.x", "OSM_TYPE", "OSM_ID", "geom_type")
changeable_osm_osm_objects_cols <- c("local_id", "MAN_MADE.y", "LEISURE.y", "AMENITY.y", "OFFICE.y", "SHOP.y", "TOURISM.y", "SPORT.y", "RAILWAY.y", "HIGHWAY.y", "BUILDING_POINT.y", "BUILDING.y", "PARKING.y", "AO", "PUBLIC")


# Aggregate
intersects_osm_osm_objects <- unique(intersects_osm_osm_objects[, ..unchangeable_osm_osm_objects_cols], by = "local_id") %>% 
  merge(.,intersects_osm_osm_objects[,..changeable_osm_osm_objects_cols] %>% 
          group_by(local_id) %>%
          summarise_all(funs(paste(na.omit(.), collapse = "|"))) , by="local_id") 


### Concatenate non-missing column content from OSM objects for matched attributes
### We concatenate them by "|"
column_names_to_concat_for_osm_osm <-
  names(intersects_osm_osm_objects)[grepl("\\.y$", names(intersects_osm_osm_objects))]
column_names_to_concat_for_osm_osm <-
  gsub(".y", "", column_names_to_concat_for_osm_osm, fixed = T)

### Since it is a destructive operation, store the results first in a temporary object
temp_osm_osm <- as.data.table(copy(intersects_osm_osm_objects))

for(v in column_names_to_concat_for_osm_osm) {
  
  set(temp_osm_osm, j = v, value = ifelse(is.na(temp_osm_osm[[paste0(v, ".y")]]), temp_osm_osm[[paste0(v, ".x")]], paste(temp_osm_osm[[paste0(v, ".x")]], temp_osm_osm[[paste0(v, ".y")]] , sep = "|") ) )
  # Remove NAs
  set(temp_osm_osm, j = v, value = gsub("|NA|", "", temp_osm_osm[[v]], fixed = T))
  set(temp_osm_osm, j = v, value = gsub("|NA", "", temp_osm_osm[[v]], fixed = T))
  set(temp_osm_osm, j = v, value = gsub("NA|", "", temp_osm_osm[[v]], fixed = T))
  # Remove redundant columns
  set(temp_osm_osm, j = paste0(v, ".x"), value = NULL)
  set(temp_osm_osm, j = paste0(v, ".y"), value = NULL)
  
}

# Empty strings to NA
for(j in names(temp_osm_osm) ) {
  
  set(temp_osm_osm, i = grep("^$|^ $|^NA$", temp_osm_osm[[j]]), j = j, value = NA_character_)
  
}

# Come back to intersects_osm_osm_objects
intersects_osm_osm_objects <- copy(temp_osm_osm)

##########################

## Row-bind of OSM-objects and cctv cameras
all_cctv_and_OSM_objects <-
  rbind(
    as.data.table(intersects_osm_osm_objects),
    # all POI points not intersecting with POI polygons
    as.data.table(cctv_all)[!(cctv_ID %in% intersects_cctv_osm_objects_CCTV_ids)],
    # all POI polygons not intersecting with POI points
    as.data.table(intersects_cctv_osm_objects), # all POI points and polygons that are intersecting within cctv_other_cutoff
    fill = T
  ) %>% 
  select(.,-c("st_type", "geometry"))  # Remove geometry, because we will not use it further.

# Checks
# length(unique(all_cctv_and_OSM_objects$local_id))
# length(unique(all_cctv_and_OSM_objects$cctv_ID))

#####################

# Final corrections
# Add new IDs for new relaton - presence or absence of a camera. Also make a better order of columns.

all_cctv_and_OSM_objects$cctv_OSM_ID <- seq(1:nrow(all_cctv_and_OSM_objects))
all_cctv_and_OSM_objects[,] <-
  lapply(all_cctv_and_OSM_objects[,], as.character)

all_cctv_and_OSM_objects <- all_cctv_and_OSM_objects[,c("cctv_OSM_ID", "local_id", "cctv_ID", "AdmArea", "District", "OVDAddress", "cctv_type", "earliest_version", "OSM_ID", "NAME", "NAME_EN", "NAME_RU", "geom_type", "OSM_TYPE", "MAN_MADE", "LEISURE", "AMENITY", "OFFICE", "SHOP", "TOURISM", "RAILWAY", "HIGHWAY", "BUILDING_POINT", "BUILDING", "PARKING", "PUBLIC", "AO")] 

# Empty or pseudo-NA strings to NA
all_cctv_and_OSM_objects[,1:ncol(all_cctv_and_OSM_objects)][all_cctv_and_OSM_objects[,1:ncol(all_cctv_and_OSM_objects)] == ""] <- NA
all_cctv_and_OSM_objects[,1:ncol(all_cctv_and_OSM_objects)][all_cctv_and_OSM_objects[,1:ncol(all_cctv_and_OSM_objects)] == "NA"]<- NA


# Save the dataset 
fwrite(all_cctv_and_OSM_objects, file = paste0("street_cctv_and_osm_objects_LONG_bin_design_by_CCTV_after_intersection_without_bench", "_", cctv_cutoff, "_", OSM_objects_cutoff, ".csv"), row.names = F, na = NA)

# all_cctv_and_OSM_objects <- data.table::fread("street_cctv_and_osm_objects_LONG_bin_design_by_CCTV_after_intersection_without_bench_50_50.csv", encoding = "UTF-8")



#####################
#####################
#####################

# Make wide table from long one

# Create a new object for work and tag-columns of interest
objects_after_intersection <- setDT(all_cctv_and_OSM_objects)
tag_types <- c("MAN_MADE", "LEISURE", "AMENITY", "OFFICE", "SHOP", "TOURISM", "RAILWAY", "HIGHWAY", "BUILDING_POINT", "BUILDING", "PARKING", "PUBLIC", "AO")

# Melt all strings in tag columns to long vector
objects_tags_long <-
  melt(
    objects_after_intersection,
    id.vars = "cctv_OSM_ID",
    measure.vars = tag_types,
    na_rm = T,
    value.name = "tag"
  )
objects_tags_long <- objects_tags_long[!is.na(tag)]

# Data have a lot of "multiple tag strings". They are strings which contain two o more tags separated by spetial symbols. There are three characters in the data: c(";", "|", ",")
# Change the separated symbols to ";". Thus, only one separator character (";") will remain in the data with which it is convenient to work
objects_tags_long$tag <- gsub('\\,', ';', objects_tags_long$tag )
objects_tags_long$tag <- gsub('\\|', ';', objects_tags_long$tag )

# Split all multiple tags in the strings 
objects_tags_long_split <-
  as.data.table(stringi::stri_split_fixed(objects_tags_long$tag, c(";"), simplify = T))
objects_tags_long_split[, cctv_OSM_ID := objects_tags_long$cctv_OSM_ID]

# Split multiple tags to columns
objects_tags_long_split <-
  objects_tags_long_split[nzchar(ncol(objects_tags_long_split))]
objects_tags_long_split_long <-
  melt(objects_tags_long_split,
       id.vars = "cctv_OSM_ID",
       value.name = "tag")
objects_tags_long_split_long <-
  objects_tags_long_split_long[nzchar(tag)]

# Add the information to the data
objects_tags_long <-
  rbind(objects_tags_long[!grepl(";", tag, fixed = T)], objects_tags_long_split_long)
objects_tags_long[, variable := NULL]
objects_tags_long[, tag := str_trim(gsub("\\s+", " ", tag))]

# Make a wide table of presense of CCTV with tags as columns. The table is given in absolute values.  
objects_tags_count <-
  dcast(
    objects_tags_long,
    cctv_OSM_ID ~ tag,
    fun.aggregate = function(x) {
      length(x)
    }
  )

# View(head(objects_tags_count))

# Add OSM-objects tags
objects_and_cameras <-
  merge(
    as.data.table(all_cctv_and_OSM_objects[, -c(..tag_types)]),
    as.data.table(objects_tags_count),
    by = "cctv_OSM_ID",
    all.x = T,
    all.y = F,
    sort = F
  )

# From absolute-values table to binary-values table.

col_binary <- names(objects_and_cameras)[15:ncol(objects_and_cameras)] 
objects_and_cameras_binary <- objects_and_cameras[, (col_binary) := lapply(.SD, function(x) {ifelse(x > 0, 1, 0)}), .SDcols = col_binary]
objects_and_cameras_binary <- objects_and_cameras_binary[, (col_binary) := lapply(.SD, function(x) {ifelse(is.na(x), 0, x)}), .SDcols = col_binary]

# unique(objects_and_cameras_binary$school)
# View(head(objects_and_cameras_binary))

# Save the dataset. (fwrite - because this is the fastest way) 
fwrite(objects_and_cameras_binary, file = "street_cctv_and_osm_objects_WIDE_bin_cutoff_50.csv", row.names = F, na = NA)

# objects_and_cameras_binary <- data.table::fread("street_cctv_and_osm_objects_WIDE_bin_cutoff_50.csv", encoding = "UTF-8")
# View(objects_and_cameras_binary %>% filter(crossing == 1))