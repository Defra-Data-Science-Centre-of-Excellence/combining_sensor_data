library(httr)
library(jsonlite)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)

# Set the API endpoint
url <- "https://data.sensor.community/static/v2/data.1h.json"  



# Make GET request
response <- GET(url)



data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  
  
  
# Unnest the location data
flat_data <- bind_cols(
  data,
  data$sensor,
  data$location, 
) |> select(-location, -sensor)  |> 
  rename("sensor_id" = `id...7`) |> 
  bind_cols(data$sensor_type) |> 
select(-sensor_type) |> 
  rename("sensor_type_id" = `id...8`) |> 
  mutate(longitude = as.numeric(longitude), 
         latitude = as.numeric(latitude)) # Drop nested column after unnesting

# Filter for sensors in the UK
uk_data <- flat_data |> 
  filter(country == "GB")


# Just keeping the location for now..... 


sensor_community_uk_data_sf <-  st_as_sf(uk_data,
                        coords = c("longitude", "latitude"),
                        crs = 4326) |>  
  unnest(cols = c(sensordatavalues))


st_write(sensor_community_uk_data_sf, "data/sensor_community_uk_data_sf.gpkg")


sensor_community_uk_data_sf <- st_read("data/sensor_community_uk_data_sf.gpkg")


# P1 = PM10 
# P2 = PM2.5 

sensor_community_uk_data_sf_cropped <- sensor_community_uk_data_sf |> 
  st_crop(c(
    ymin = 49.823,
    xmin = -8.649,
    ymax = 60.860,
    xmax = 1.768
  )) |> 
  filter(value_type %in% c("P1", "P2", "humidty", "pressure", "temperature", "humidity"))



# Lets get the rough UK outline too.... 


uk_outline <- rnaturalearth::ne_countries(country = "United Kingdom")


sensor_community_uk_data_sf_cropped |> 
ggplot() +
  geom_sf(data = uk_outline, aes(geometry = geometry)) +
  geom_sf(aes(fill = value, colour = value, geometry = geom)) +
  facet_wrap(~value_type, nrow = 2) +
  theme(legend.position = "none")



# Or lets put on leaflet and add a popup with the data values.....will need to pivot wider first


sensor_community_uk_data_leaflet <- sensor_community_uk_data_sf_cropped |> 
  mutate(value = as.numeric(value)) |> 
  pivot_wider(id_cols = c(timestamp, geom), names_from = value_type, values_from = value, values_fn = mean) |> 
  mutate(
    popup_text = paste0(
      "<b>Timestamp:</b> ", timestamp, "<br>",
      "<b>PM10:</b> ", round(P1, 1), " ug/m3<br>",
      "<b>PM2.5:</b> ", round(P2, 1), " ug/m3<br>",
      "<b>Temperature:</b> ", round(temperature, 2), " °C<br>",
      "<b>Pressure:</b> ", round(pressure, 2), " Pa<br>", 
      "<b>Relative Humidity:</b> ", round(humidity,2), " %"
    )
  )







pm_bins <- c(0, 12, 35.4, 55.4, 150.4, 250.4, Inf)
pm_colors <- c("green", "yellow", "orange", "red", "purple", "maroon")



pm_palette <- colorBin(
  palette = pm_colors,
  domain = round(sensor_community_uk_data_leaflet$P2, 1),
  bins = pm_bins,
  na.color = "grey"
)





sensor_community_uk_data_leaflet |> 
  filter(is.na(P1) == F) |> 
  leaflet() |> 
  addTiles() |> 
  addCircleMarkers(popup =  ~popup_text, color = ~pm_palette(sensor_community_uk_data_leaflet$P2))




sensor_community_uk_data_leaflet |> 
  filter(is.na(P1) == F)

