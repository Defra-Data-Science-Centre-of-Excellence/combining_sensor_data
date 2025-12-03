# Now looking at all sensors....


leaflet::leaflet(aurn_locations_urban) |> 
  addTiles() |> 
  addCircleMarkers(data = uk_sensors_sf, 
                  # clusterOptions = T, 
                   stroke = FALSE,
                   fillOpacity = 0.3,
                   fillColor = "purple", 
                   popup = ~paste0(
                     "<b>", name, "</b><br/>",
                     "PM2.5 (10min average): ", pm2.5_10minute, "<br/>",
                     "Last seen: ", as.POSIXct(last_seen, origin = "1970-01-01", tz = "UTC")
                   )) |> 
  addCircleMarkers(
    fillOpacity = 0.3,
    stroke = F,
    lat = ~latitude,
    lng = ~longitude,
    popup = ~site,
    layerId = ~code, 
   # clusterOptions = markerClusterOptions()
  ) |> 
  addCircleMarkers(data = uk_data_sf, 
                   # clusterOptions = T, 
                   stroke = FALSE,
                   fillOpacity = 0.3,
                   fillColor = "lightgreen"
                   )
  


aurn_locations_urban_sf <- st_as_sf(aurn_locations_urban, coords = c("longitude", "latitude"),
         crs = 4326)



