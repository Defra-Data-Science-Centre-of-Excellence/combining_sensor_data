# Combining Sensor Data
A project for integrating and visualising air‑quality data from multiple sensor networks. It includes: 

- AURN (Automatic Urban and Rural Network) – Defra's reference‑grade monitoring stations
- PurpleAir – low‑cost sensor network
- Sensor.Community – low‑cost sensor network
- Diffusion Tubes
- Breathe London


| Script | Description |
|--------|-------------|
| `DT_analysis.R` | Diffusion Tube analysis|
| `background_pm_25_data.R` | Load and process background PM2.5 datasets - changing the csv files to shapefiles |
| `breathe_london_api.R` | Access and download data from the Breathe London API |
| `closest_aurn_sensor.R` | Identify the closest AURN sensor for each 1km grid cell in the UK|
| `combining_all_data_sources.R` | Merge all sensor datasets (AURN, PurpleAir, Sensor Community) into one dataset (all as point sources). Then working out the closest sites that may be used as counterfactuals for comparison.  |
| `comparing_close_to_london_marylebone_site.R` | Compare sensors near the Marylebone Road AURN site - initial comparison |
| `experimenting_with_aurn_data.R` | Exploratory work with AURN datasets - Produces leaflet map with DAQI indexes|
| `nearest_sensors_to_population.R` | Match population raster cells to nearest sensors |
| `population_raster_to_sf.R` | Convert population raster data into sf polygons |
| `purple_air_data.R` | Load and process PurpleAir sensor data |
| `removing_spiel_function.R` | Utility function for cleaning or removing unwanted text from gov stats files |
| `sensorcommunity_data.R` | Load and process Sensor.Community data |
| `sites_of_interest_initial_data.R` | Prepare initial list of sites of interest |
