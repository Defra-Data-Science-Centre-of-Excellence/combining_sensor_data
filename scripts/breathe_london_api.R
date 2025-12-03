# Breathe London API 

library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(leaflet)


# Set the API endpoint
url <- "https://api.breathelondon-communities.org/api/ListSensors?key=a6f24186-b753-11ec-b909-0242ac120002"  

# Make GET request
response <- GET(url)



data <- fromJSON(content(response, as = "text", encoding = "UTF-8")) |> 
  data.frame()


site_locations <- data |> 
  select(SiteName, Latitude, Longitude, LatestIPM25Value) |> 
  st_as_sf(coords = c( "Longitude","Latitude"))



site_locations |> 
  filter(is.na(LatestIPM25Value) == F) |> 
leaflet() |> 
  addTiles() |> 
  addCircleMarkers(popup =~ paste0(SiteName,"<br> <b> PM2.5:</b> " , as.character(LatestIPM25Value)))





pm_data <- data |> 
  select("LatestIPM25Value")




ggplot(pm_data) +
  geom_bar(aes(x= LatestIPM25Value))




url <- "https://api.breathelondon-communities.org/api/getClarityData/CLDP001/IPM25/Mon 11 Apr 2022 11:00:00 GMT/Mon 11 Apr 2022 13:00:00 GMT/Hourly?key=a6f24186-b753-11ec-b909-0242ac120002"
