# Now looking at the diffusion tube data....


library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)



diffusion_tube_data_raw <- read_csv("data/diffusion_tube_data/2023 DT Data.csv") |> 
  mutate(decS13 = as.numeric(decS13)) # we only have 4 observations so R is interpreting this coloumn as bolean


diffusion_tube_data <- diffusion_tube_data_raw |> 
  st_as_sf(crs = 27700,  coords = c("intX_OS_Easting", "intY_OS_Northing")) |> 
  st_transform(4326)


diffusion_tube_data_subset <- diffusion_tube_data |> 
    sample_n(1000)





leaflet(diffusion_tube_data) |> 
  addTiles() |> 
  #addHeatmap(radius = 20, blur = 4)
  addMarkers(clusterOptions = T, 
             popup = ~decS6) 




diffusion_tube_data_subset_long <- diffusion_tube_data_subset |> 
  pivot_longer(cols = c(decS1:decS12), names_to = "observation_number", values_to = "nox_value")


ggplot(diffusion_tube_data_subset_long) +
  geom_histogram(aes(x = nox_value, colour = observation_number, fill = observation_number)) +
  facet_wrap(~observation_number)
