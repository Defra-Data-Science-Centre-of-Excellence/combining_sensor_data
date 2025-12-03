# Making a map to show the nearest AURN site that has been active in the past year (2024-205)

library(openair)
library(tidyverse)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(nngeo)


# Step 1: Load all the AURN sites using openair


aurn_locations <- importMeta(source = "aurn") 

aurn_code_sites <- aurn_locations$code 

all_aurn_data <- importAURN(site = c(aurn_code_sites), year = 2024:2025,  pollutant = c("no", "no2", "nox", "o3", "pm2.5", "pm10"), verbose = T, data_type = "monthly") |> 
  dplyr::select(-c("o3_capture", "no_capture", "no2_capture", "nox_capture", "pm10_capture", "pm2.5_capture")) |> 
  filter(if_any(c(no, no2, nox, o3, pm2.5, pm10), ~ !is.na(.)))  # Getting rid of ones where it is all NA 


aurn_locations_active_sites <- aurn_locations |> 
  filter(code %in% all_aurn_data$code)


# Plot these locations.... 


leaflet(aurn_locations_active_sites) |>
  addTiles() |>
  addMarkers(
    lat = ~latitude,
    lng = ~longitude,
    popup = ~site,
    layerId = ~code, 
    clusterOptions = markerClusterOptions()
  )


# Step 2: make a grid for the uk....

uk_outline <- ne_countries(country = "United Kingdom", returnclass = "sf", scale = "large") 

# conver to british national grid.... 
uk_outline_m <- st_transform(uk_outline, crs = 27700)

# Create a 10 km x 10 km grid over the UK
grid_1km <- st_make_grid(uk_outline_m, cellsize = 1000, square = TRUE) |> # 1km2 cells may take a while to make
  st_intersection(uk_outline_m) |> # keep only cells within the UK 
  st_transform(crs = 4326) # And now change back to longs and lats so can plot in leaflet



# Make into an sf 

grid_1km_sf <- st_sf(
  grid_id = 1:length(grid_1km),
  geometry = grid_1km
)





# Compute centroids of grid cells
grid_centroids <- st_centroid(grid_1km_sf)


# Need to set the crs for the aurn sites 

aurn_locations_active_sites <- aurn_locations_active_sites |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


# Find nearest station for each centroid
nearest_station_index <- st_nearest_feature(grid_centroids, aurn_locations_active_sites)



# Attach the station info to grids
grid_1km_sf$nearest_station <- aurn_locations_active_sites$site[nearest_station_index]

# Now merge the grid to make the maps quicker to plot.... 

grid_merged <- grid_1km_sf |>
  group_by(nearest_station) |>
  summarise(geometry = st_union(geometry)) |>
  ungroup() |> 
  mutate(area = units::drop_units(st_area(geometry))) 



# And just a quick plot to see what this looks like......
ggplot(grid_merged)+
  geom_sf(aes(fill = area, geometry = geometry), colour = NA, linewidth = 0.2,alpha = 0.8) +
  scale_fill_viridis_c(trans = "log10") +
  #geom_sf(data = aurn_locations_active_sites, aes(geometry = geometry), colour = "black") +
  scale_x_continuous(limits = c(-9,3)) +
  theme(legend.position = "none") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#ADEDFF", colour = NA))


# Creating a colour palette for leaflet map

pal <- colorFactor(
  palette = viridis::viridis_pal()(204),       # or another palette like viridis::viridis_pal()(n)
  domain = grid_merged$area
)



grid_merged |> 
  leaflet() |> 
  addTiles() |> 
  addPolygons(fillColor =  ~pal(area), 
              fillOpacity = 0.7,
              color = "white",
              weight = 1) |> 
  addMarkers(
    data = aurn_locations_active_sites,
    popup = ~site,
    group = "AURN site markers"
  ) |> 
  addLayersControl(
    overlayGroups = c("AURN site markers"),
    options = layersControlOptions(collapsed = FALSE)
  )





# Lets see if we can also add in the population_df 


popdf <- read_sf("data/population_data/popdf_sf.gpkg")


# Now lets try and get the two grids lined up 



polygon_intersects <- st_intersects(popdf,grid_merged) |> 
  data.frame()


population_per_polygon <- polygon_intersects |> 
  mutate(population_in_grid = popdf$pop_aw[row.id], 
         polygon = grid_merged$geometry[col.id])




population_per_polygon |> 
  st_as_sf(crs = 4326) |> 
  group_by(polygon) |> 
  summarise(total_pop = sum(population_in_grid, na.rm = T)) |> 
  ggplot() +
  geom_sf(aes(fill = total_pop, geometry = polygon), colour = NA) +
  scale_fill_viridis_c()







# What I need to do now is take the population points (grid centroids) and work out the nearest for each of these points 
# Use the package nngeo


# Find nearest AURN site (returns index list)
nearest_idx <- nngeo::st_nn(popdf, aurn_locations_active_sites, k = 1, returnDist = TRUE)

# Extract nearest index and distance
popdf$aurn_site_index <- unlist(nearest_idx$nn)
popdf$distance_to_aurn <- unlist(nearest_idx$dist)



aurn_locations_active_sites_info <- aurn_locations_active_sites |> 
  mutate(aurn_site_index = 1:nrow(aurn_locations_active_sites)) |> 
  select(aurn_site_index, site_type, site) |> 
  st_drop_geometry()



popdf <- popdf |> 
  left_join(aurn_locations_active_sites_info)



popdf |> 
  ggplot() +
  geom_sf(aes(colour = distance_to_aurn/1000, geometry = geom), size = 0.6) +
  scale_colour_viridis_c()




popdf |> 
  ggplot(aes(x = distance_to_aurn/1000, weight = pop_aw, fill = site_type)) +
  geom_histogram(aes(y = after_stat(100 * count / sum(count))), binwidth = 1) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = seq(0,100, by = 10), limits = c(0,100), expand = c(0,0))



# Making nicer bins for the 

breaks <- seq(0, 50, by = 5)


pop_by_distance <- popdf |> 
  st_drop_geometry() |> 
  mutate(dist_km = distance_to_aurn/1000 ) |> 
  mutate(
    bin = cut(
      dist_km,
      breaks = c(breaks, Inf),             # Inf = captures all values above 60
      labels = c(paste0(head(breaks, -1), "-", tail(breaks, -1), " km"), "50+ km"),
      include.lowest = TRUE,
      right = FALSE
    )
  )|> 
  group_by(bin, site_type) |>
  summarise(pop = sum(pop_aw, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(percentage = 100 * pop / sum(pop)) |> 
  mutate(site_type = factor(site_type, levels = c("Rural Background","Suburban Industrial","Suburban Background" ,"Urban Industrial",
                                                  "Urban Traffic", "Urban Background")))




site_colours <- c(
  "Rural Background"    = "#33a02c",
  "Suburban Background" = "#ff7f00",
  "Suburban Industrial" = "#FDDC9B",
  "Urban Background"    = "#1f78b4",
  "Urban Industrial"    = "#a6cee3",
  "Urban Traffic"       = "#6baed6"
)



whole_group_bin_percentages <- pop_by_distance |> 
  group_by(bin) |> 
  summarise(percent_in_bin = sum(percentage))


ggplot(pop_by_distance) +
  geom_col(aes(x = bin, y = percentage, fill = site_type)) +
  geom_text(data = whole_group_bin_percentages, aes(x = bin, y = percent_in_bin  + 1, label = scales::percent(percent_in_bin/100, accuracy = 0.1))) +
  scale_x_discrete(name = "Distance to closest AURN", expand = c(0,0)) +
  scale_y_continuous(name = "Percentage of Population (%)", expand = c(0,0), limits = c(0,40)) +
  scale_fill_manual(values = site_colours)+
  theme_minimal()



ggsave("plots/proportion_population_closest_to_sensor_aurn_by_type.png")




# How could we make a contour map to show these areas? 
library(stars)
library(raster)
library(gstat)

# Need to first make into a raster...
# Reproject to British National Grid
# And do some interpolation to fill in the population deadspots....

# Actually this doesnt actually even need to be done with this population dataframe it couold jsut be the original km

popdf_bng <- st_transform(popdf, 27700)
pop_sp <- as(popdf_bng, "Spatial")



r <- raster(pop_sp, res = 1000)   # 1000 m = 1 km grid


idw_model <- gstat(formula = distance_to_aurn ~ 1, data = pop_sp, nmax = 15)
idw_raster <- interpolate(r, idw_model)




stars_obj <- st_as_stars(idw_raster)


# Create contour lines at 5 km (5000 m) bands

breaks <- seq(0, max(stars_obj[[1]], na.rm = TRUE), by = 1000)

contours <- st_contour(stars_obj, breaks = breaks, na.rm = TRUE)




contours_filled <- contours |>
  st_make_valid() |>        # ensures no topology errors
  group_by(var1.pred) |>
  summarise(do_union = TRUE) |> 
  st_intersection(uk_outline_m)




ggplot(contours_filled) +
  geom_sf(aes(geometry = geometry, fill = var1.pred), colour = NA) +
  scale_fill_viridis_d(
    direction = -1,
  ) +
  theme(legend.position = "none")

  

# And make a leaflet edition.... 


contours_filled <- contours_filled |> 
  mutate(distance_bin = factor(var1.pred)) |> 
  st_transform(4326)


pal <- colorFactor(
  palette = viridis::viridis(length(unique(contours_filled$distance_bin)), option = "viridis", direction = -1),
  domain = contours_filled$distance_bin
)


leaflet(contours_filled) |> 
  addProviderTiles("CartoDB.Positron") |>   # nice clean basemap
  addPolygons(   
    weight = 0.2,
    color = "lightgrey",
    fillColor = ~pal(distance_bin),  # fill by distance bin
    fillOpacity = 0.6,
    popup = ~paste("Distance:", var1.pred)
  ) |> 
  addMarkers(data = aurn_locations_active_sites, popup = ~ paste0(site,"<br>" ,site_type), 
             group = "AURN site markers") |> 
  addLayersControl(
    overlayGroups = c("AURN site markers"),
    options = layersControlOptions(collapsed = FALSE)
  )

