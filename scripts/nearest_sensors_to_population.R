# Population versus the sensors....

library(tidyverse)
library(sf)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)


sensor_network <- read_sf("data/combined_sensor_network.gpkg")

uk_outline <- ne_countries(country = "United Kingdom", returnclass = "sf", scale = "large") 


# Now lets get the population per LSOA......

population_per_lsoa <- read_csv("data/population_data/population_per_lsoa.csv") 

population_per_lsoa <- population_per_lsoa[-c(1,4),] |> 
  rename(total_pop = `...5`, lsoa_code = `...3`) |> 
  dplyr::select(lsoa_code, total_pop)

lsoa_shapefile <- read_sf("data/Lower_layer_Super_Output_Areas_EW_BFC.gpkg") |> 
  st_transform(crs = 4326) # Make into longs and lats 


sf_use_s2(FALSE)


# Compute centroids of grid cells
lsoa_centroids <-st_centroid(lsoa_shapefile)

# Find nearest station for each centroid
nearest_station_index <- st_nearest_feature(lsoa_centroids, sensor_network)

# Attach the station info to lsoas
lsoa_centroids$nearest_station <- sensor_network$id[nearest_station_index]
lsoa_centroids$nearest_station_name <- sensor_network$sensor_id[nearest_station_index]
lsoa_centroids$nearest_station_type <- sensor_network$source[nearest_station_index]
lsoa_centroids$nearest_station_location <- sensor_network$geom[nearest_station_index]



nearest_idx <- nngeo::st_nn(lsoa_centroids, sensor_network, k = 1, returnDist = TRUE)


lsoa_centroids$distance_to_monitor <- unlist(nearest_idx$dist)


write_sf(lsoa_centroids, "data/lsoa_centroids_with_aq_sites.gpkg")


lsoa_centroids_with_aq_sites <- lsoa_centroids

lsoa_centroids_with_aq_sites <- read_sf("data/lsoa_centroids_with_aq_sites.gpkg")

# And just a quick plot to see what this looks like......
ggplot(lsoa_centroids_with_aq_sites)+
  geom_sf(data )
  geom_sf(aes(colour = nearest_station_type, geometry = geom)) 



# How about do with a grid rather than LSOAs? 

# Convert to British National Grid

sensor_network_m <- st_transform(sensor_network, crs = 27700) 


uk_outline_m <- st_transform(uk_outline, crs = 27700)

# Create a 10 km x 10 km grid over the UK
grid_10km <- st_make_grid(uk_outline_m, cellsize = 2500, square = TRUE) |> 
  st_intersection(uk_outline_m)  # keep only cells within the UK


grid_10km_sf <- st_sf(
  grid_id = 1:length(grid_10km),
  geometry = grid_10km
)


# Compute centroids of grid cells
grid_centroids <- st_centroid(grid_10km_sf)


# Find nearest station for each centroid
nearest_station_index <- st_nearest_feature(grid_centroids, sensor_network_m)



# Attach the station info to grids
grid_10km_sf$nearest_station <- sensor_network_m$sensor_id[nearest_station_index]

# And just a quick plot to see what this looks like......
ggplot(grid_10km_sf)+
  geom_sf(aes(fill = nearest_station, geometry = geometry), colour = NA) +
  geom_sf(data = sensor_network_m, aes(geometry = geom), colour = "black") +
  theme(legend.position = "none")


total_areas <- grid_10km_sf |> 
  group_by(nearest_station) |> 
  summarise(total_area = sum(st_area(geometry)), col = NA)


grid_10km_sf$nearest_station_type <- sensor_network_m$source[nearest_station_index]


grid_merged <- grid_10km_sf |>
  group_by(nearest_station, nearest_station_type) |>
  summarise(geometry = st_union(geometry)) |>
  ungroup() |> 
  mutate(area = units::drop_units(st_area(geometry))) 





pal <- colorFactor(
  palette = RColorBrewer::brewer.pal(3, "Set1"),       # or another palette like viridis::viridis_pal()(n)
  domain = grid_merged$nearest_station_type
)




grid_merged |>
  st_transform(4326) |>
  leaflet() |>
  addTiles() |>
  addMarkers(data = st_transform(sensor_network_m, 4326), 
             group = "Site markers") |>
  addPolygons(
    fillColor =  ~ pal(nearest_station_type),
    fillOpacity = 0.7,
    color = "white",
    weight = 1
  ) |>
  addLayersControl(overlayGroups = c("Site markers"),
                   options = layersControlOptions(collapsed = FALSE)) |> 
  addLegend("bottomright", pal = pal, values = ~nearest_station_type,
            opacity = 1
  )

