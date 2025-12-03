# Background AQ maps 
library(raster)
library(sf)
library(terra)



pm_conc <-read_csv("data/ambient_air_quality/mappm252024g.csv")


v <- vect(pm_conc, geom = c("x", "y"), crs = "EPSG:27700")

# Creating a grid that is the same size and dimensions as the vector
r <- rast(v, resolution = 1000)

# Rasterise using the PM2.5 field
pm25_rast <- rasterize(v, r, field = "pm252024g")


pm25_rast_epsg <- pm25_rast |> 
  project("EPSG:4326")


pm25_poly <- as.polygons(pm25_rast_epsg, dissolve = T)
pm25_sf <- st_as_sf(pm25_poly) |> 
  st_transform(crs = 4326)



ggplot(pm25_sf) +
  geom_sf(aes(fill = last), colour = NA)


pal <- colorNumeric(
  palette = "viridis",     # or "magma", "inferno", "RdYlBu", etc.
  domain = pm25_sf$last     # numeric PM2.5 column
)





leaflet(pm25_sf) |> 
  addTiles() |> 
  addPolygons(
    fillColor = ~pal(last),
   popup = ~paste0("<b>PM2.5: </b>", last, "µg/m³"),
    color = NA,
    weight = 0,
    fillOpacity = 0.8
  ) |>
  addLegend(
    pal = pal,
    values = pm25_sf$last,
    title = "Annual Mean PM2.5 2024"
  )



# lets save the pm2.5 annual avergaes as a sf layer....


write_sf(pm25_sf, "data/ambient_air_quality/pm25_sf_2024_annual_mean.gpkg")
