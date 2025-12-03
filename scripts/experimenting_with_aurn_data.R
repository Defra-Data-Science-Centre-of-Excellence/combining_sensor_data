library(openair)
library(tidyverse)
library(leaflet)



aurn_locations <- importMeta(source = "aurn") 

aurn_code_sites <- aurn_locations$code 

all_aurn_data_lastest_measurement <- importAURN(site = c(aurn_code_sites), year = 2024:2025,  pollutant = c("no", "no2", "nox", "o3", "pm2.5", "pm10"), verbose = T, data_type = "monthly") |> 
  select(-c("o3_capture", "no_capture", "no2_capture", "nox_capture", "pm10_capture", "pm2.5_capture")) |> 
  filter(if_any(c(no, no2, nox, o3, pm2.5, pm10), ~ !is.na(.))) |> 
  group_by(site) |> 
  filter(date == max(date)) |> 
  filter(is.na(pm2.5) == F)


aurn_locations_active_sites <- aurn_locations |> 
  filter(code %in% all_aurn_data$code)


all_aurn_data_lastest_measurement_with_location <- all_aurn_data_lastest_measurement |> 
  left_join(aurn_locations)



pm_bins <- c(0, 12, 35.4, 55.4, 150.4, 250.4, Inf)
pm_colors <- c("lightgreen", "yellow", "orange", "red", "purple", "maroon")



pm_palette <- colorBin(
  palette = pm_colors,
  domain = all_aurn_data_lastest_measurement_with_location$pm2.5,
  bins = pm_bins,
  na.color = "grey"
)




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
                    "PM2.5 (monthly average): ", round(pm2.5, 2) , "ug/m3 <br/>",
                    "Last monthly measuremnt: ", as.POSIXct(date, origin = "1970-01-01", tz = "UTC"),"<br/>",
                    "Site Type: AURN"),
    
    layerId = ~code, 
    group = "AURN"
    #clusterOptions = markerClusterOptions()
  ) |> 
  addCircleMarkers(data = uk_sensors_sf, 
                   #clusterOptions = T, 
                   stroke = FALSE,
                   fillOpacity = 1,
                   fillColor = ~pm_palette(pm2.5_10minute), 
                   group = "PurpleAir",
                   popup = ~paste0(
                     "<b>", name, "</b><br/>",
                     "PM2.5 (10min average): ", pm2.5_10minute, "ug/m3 <br/>",
                     "Last seen: ", as.POSIXct(last_seen, origin = "1970-01-01", tz = "UTC"), "<br/>",
                     "Site Type: PurpleAir"
                   )) |> 
  addCircleMarkers(data = filter(sensor_community_uk_data_leaflet, is.na(P2) == F), 
                   stroke = FALSE,
                   fillOpacity = 1,
                   fillColor = ~pm_palette(P2),  
                   group = "SensorCommunity",
                   popup =  ~paste0(
                  "<b>Last measurement:</b> ", timestamp, "<br>",
                  "<b>PM2.5:</b> ", round(P2, 1), " ug/m3<br>", 
                  "Site Type: Sensor Community"
  )) |> 
  addLayersControl(
    overlayGroups = c("AURN", "PurpleAir", "SensorCommunity"),
    options = layersControlOptions(collapsed = FALSE)
  ) |> 
  addLegend(
    "bottomright",
    pal = pm_palette,
    values = c(0, 250),        # or use range(all_data$pm2.5, na.rm=TRUE)
    title = "PM<sub>2.5</sub> (µg/m³)",
    opacity = 1,
    labFormat = labelFormat(digits = 1)
  )


