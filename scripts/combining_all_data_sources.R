# Working out contenders for comparison......
library(openair)
library(tidyverse)
library(sf)
library(leaflet)


# If we just strip out all the information for now so can make into one point layer....

# 1. AURN

aurn_locations <- importMeta(source = "aurn")


aurn_locations_sf <- aurn_locations |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  rename(sensor_id = code) |> 
  select(source, geometry, sensor_id)


#2. PurpleAir


purple_air_locations <- read_rds(file = "data/purple_air_data.rds")



purple_air_locations_sf <- purple_air_locations |> 
  select(sensor_index, longitude, latitude) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  mutate(source = "purpleair") |> 
  rename(sensor_id = sensor_index)



#3. Sensor Community 

sensor_community_uk_data_sf <- read_sf("data/sensor_community_uk_data_sf.gpkg")



sensor_community_locations_sf <- sensor_community_uk_data_sf |> 
  select(geom, id...1) |> 
  mutate(source = "sensorcommunity") |> 
  rename(geometry = geom) |> 
  distinct() |> 
  rename(sensor_id = id...1)
  


sensor_network <- rbind(aurn_locations_sf, 
                        purple_air_locations_sf, 
                        sensor_community_locations_sf)



# We have some extra date that seems to be right in the middle of the atlantic 

# Filter by the longitude - it is likely this just has an error/missing the latitude 


uk_boundingbox <- rnaturalearth::ne_countries(country = "United Kingdom") 

sensor_network <- sensor_network |> 
  st_crop(uk_boundingbox) |> 
  mutate(id = row_number())


write_sf(sensor_network, "data/combined_sensor_network.gpkg")



sensor_network <- read_sf("data/combined_sensor_network.gpkg")

# Finding out which is the nearest sensor to each of the locations.... 

sensor_network_nearest_neighbour <- sensor_network |> 
  mutate(nearest_neighbour_id = st_nearest_feature(sensor_network)) 


# And then the distance between each point 

sensor_network_with_dist <- sensor_network_nearest_neighbour |> 
  mutate(distances_between_points = st_distance(sensor_network, sensor_network[nearest_neighbour_id,], by_element = T))

# Now for each get the nearest feature....

sensor_network_with_dist |> 
  mutate(distances_between_points = units::drop_units(distances_between_points)) |> 
  #filter(distances_between_points <= 100) |> 
  ggplot() +
  geom_sf(data = uk_boundingbox) +
  geom_sf(aes(geometry = geom, fill = distances_between_points, colour = distances_between_points)) +
  scale_colour_continuous(trans = "log1p") +
  scale_fill_continuous(trans = "log1p")


# Now to get the types of sensor at the nearest neighbour locations.... 

sensor_network_without_geom <- sensor_network |> 
  st_drop_geometry()

sensor_network_sources_assigned <- sensor_network_with_dist |> 
  left_join(sensor_network_without_geom, join_by(nearest_neighbour_id == id)) |> 
  rename(nearest_neighbour_source = source.y) |> 
  arrange(distances_between_points) |> 
  mutate(same_source_marker = source.x == nearest_neighbour_source)




# What are the top 50 closest different sensor types that it might be worth looking into? 


sites_of_interest <- sensor_network_sources_assigned |> 
  filter(same_source_marker == F) |> 
  mutate(source.x = as_factor(source.x)) |> 
  head(300) 



# What is the max distance between stations now? 

max(sites_of_interest$distances_between_points) # 84130m



pal <- colorFactor(
  palette = "Set1",    
  domain = sites_of_interest$source.x
)


leaflet(sites_of_interest) |> 
  addTiles()|> 
  addCircleMarkers(popup = ~source.x, 
             color = ~pal(source.x)) |> 
  addPolylines()





sensor_network <- sensor_network |> 
  mutate(source = as_factor(source)) |> 
  mutate(marker_colour = case_when(
    source == "aurn" ~ "red", 
    source == "purpleair" ~ "purple" , 
    source == "sensorcommunity" ~ "yellow"
  ))


# And just a plot of all the sensor locations.... 

pal <- colorFactor(
  palette = "magma",    
  domain = unique(as.factor(sensor_network$source))
)

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = sensor_network$marker_colour
)


sensor_network |> 
  leaflet() |> 
  addTiles() |> 
  addAwesomeMarkers(icon = icons, 
                    popup = ~ as.character(source))




leaflet() |>
  addTiles() |>
  addCircleMarkers(
    data = all_aurn_data_lastest_measurement_with_location,
    lat = ~latitude,
    lng = ~longitude,
    stroke = NA,
    fillOpacity = 1,
    fillColor = ~pm_palette(pm2.5), 
    popup = ~paste0("<b>", site, "</b><br/>",
                    "PM2.5 (monthly average): ", round(pm2.5, 2) , "<br/>",
                    "Last monthly measuremnt: ", as.POSIXct(date, origin = "1970-01-01", tz = "UTC")),
    layerId = ~code, 
    #clusterOptions = markerClusterOptions()
  ) |> 
  addCircleMarkers(data = uk_sensors_sf, 
                   #clusterOptions = T, 
                   stroke = FALSE,
                   fillOpacity = 1,
                   fillColor = ~pm_palette(pm2.5_10minute), 
                   popup = ~paste0(
                     "<b>", name, "</b><br/>",
                     "PM2.5 (10min average): ", pm2.5_10minute, "<br/>",
                     "Last seen: ", as.POSIXct(last_seen, origin = "1970-01-01", tz = "UTC")
                   ))