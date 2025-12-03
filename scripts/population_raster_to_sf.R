# Now looking at the population per km2 according to UKCEH - https://catalogue.ceh.ac.uk/documents/7beefde9-c520-4ddf-897a-0167e8918595#top


library(raster)
library(tidyverse)
library(sf)


population <- raster("data/population_data/data/uk_residential_population_2021.tif")


crs(population) = 27700



population_in_lat_long <- projectRaster(population, crs = 4326)

plot(population_in_lat_long)


pop_df <- as.data.frame(population_in_lat_long, xy = T) |> 
  filter(is.na(pop_aw) == F)


pop_df_sf <- pop_df |> 
  st_as_sf(coords = c("x","y"), crs = 4326)



ggplot(pop_df_sf) +
  geom_sf(aes(fill = pop_aw, geometry = geometry, colour = pop_aw)) +
  scale_colour_continuous(trans = "log10") +
  scale_fill_continuous(trans = "log10")


write_sf(pop_df_sf, "data/population_data/popdf_sf.gpkg")
