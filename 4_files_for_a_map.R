###################################
###### Make files for a map #######
###################################

# Code for making a final shp files (part 4 and last)
# By: Dmitriy Serebrennikov

library(sf)

# setwd("D:/serebrennikov/serebrennikov_cctv_project/data")

prob_table_full$local_id <-  as.character(prob_table_full$local_id)
prob_table_full$diff_str2 <- prob_table_full$diff_street
prob_table_full$cctv_exist <- as.character(prob_table_full$cctv_exist)

# Load all_OSM_object data 
# load("Moscow_osm_objects_1_.rdata")
all_OSM_objects$local_id <-  as.character(all_OSM_objects$local_id)

transform_spat <- function(x){
  x <- st_as_sf(x)
  x <- st_transform(x, crs = 3857, proj4string = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")
}

#
all_OSM_objects <- transform_spat(all_OSM_objects)
all_OSM_objects$st_type <- st_geometry_type(all_OSM_objects)
all_OSM_objects_POINT <- all_OSM_objects %>% 
  filter(st_type == "POINT")
all_OSM_objects_POLYGON <- all_OSM_objects %>% 
  filter(st_type == "POLYGON")

# Park polygon to point geometry
all_OSM_objects_POINT <- transform_spat(rbind(
  as.data.table(all_OSM_objects_POINT),
  as.data.table(all_OSM_objects_POLYGON %>% filter(geom_type == "park_polygon") %>% st_centroid()),
  fill = T
))
all_OSM_objects_POLYGON <- transform_spat(as.data.table(all_OSM_objects_POLYGON[!(all_OSM_objects_POLYGON$geom_type == "park_polygon"),]))

# Merge geometry with other data

map_diff_POINT <-
  transform_spat(merge(
    as.data.table(prob_table_full),
    as.data.table(all_OSM_objects_POINT)[, c("local_id", "geometry")],
    by = "local_id",
    all.x = F,
    all.y = T
  ))
map_diff_POLYGON <-
  transform_spat(merge(
    as.data.table(prob_table_full),
    as.data.table(all_OSM_objects_POLYGON)[, c("local_id", "geometry")],
    by = "local_id",
    all.x = F,
    all.y = T
  ))

# Add column's reordering (later)

map_diff_POINT$cctv_exist <- ifelse(map_diff_POINT$cctv_exist == 1, "TRUE", "FALSE")
map_diff_POLYGON$cctv_exist <- ifelse(map_diff_POLYGON$cctv_exist == 1, "TRUE", "FALSE")

# Save
write_sf(map_diff_POINT, "map_street_bin_1se_POINT_cutoff_50.shp", update = T)
write_sf(map_diff_POLYGON, "map_street_bin_1se_POLYGON_cutoff_50.shp", update = T)



