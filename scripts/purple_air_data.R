# Investigating the Purple Air API 

library(PurpleAir)
library(sf)
library(tidyverse)
library(rnaturalearth)
library(leaflet)



PURPLE_AIR_API_KEY = "14291C85-A363-11F0-BDE5-4201AC1DC121"

Sys.setenv("PURPLE_AIR_API_KEY" = PURPLE_AIR_API_KEY)

check_api_key(Sys.getenv("PURPLE_AIR_API_KEY"))


uk_boundingbox <- rnaturalearth::ne_countries(country = "United Kingdom") 


uk_bbox <- c(
  swlat = 49.823,
  swlon = -8.649,
  nelat = 60.860,
  nelon = 1.768
)



# Get outdoor sensors only
uk_sensors <- get_sensors_data(
  x =st_bbox(uk_boundingbox),
  fields = c("name", "latitude", "longitude", "pm2.5_10minute", "last_seen"),
  location_type = "outside",
  max_age = as.integer(3600* 24),
  purple_air_api_key = Sys.getenv("PURPLE_AIR_API_KEY")
)


write_rds(uk_sensors, file = "data/purple_air_data.rds")



uk_sensors_sf <- st_as_sf(uk_sensors,
                          coords = c("longitude", "latitude"),
                          crs = 4326)



pm_bins <- c(0, 12, 35.4, 55.4, 150.4, 250.4, Inf)
pm_colors <- c("lightgreen", "yellow", "orange", "red", "purple", "maroon")



pm_palette <- colorBin(
  palette = pm_colors,
  domain = uk_sensors_sf$pm2.5_10minute,
  bins = pm_bins,
  na.color = "grey"
)



leaflet::leaflet() |> 
  addTiles() |> 
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
