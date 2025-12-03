# Comparing one site to another.... 
library(openair)

pm_marlyebone_road <- importUKAQ(site = "my1", year = 2024:2025) 


# closest purple air data = 197637

PURPLE_AIR_API_KEY = "14291C85-A363-11F0-BDE5-4201AC1DC121"

Sys.setenv(PURPLE_AIR_API_KEY = PURPLE_AIR_API_KEY)


broadley_terrace <- PurpleAir::get_sensor_history(sensor_index = "197637",  average = "60min", fields = c( "pm2.5_atm"),
                                                  start_timestamp = as.POSIXct("2025-07-01"),   end_timestamp = as.POSIXct("2025-10-31"))

pm_marlyebone_road <- pm_marlyebone_road |> 
  dplyr::select(date, pm2.5)

# Now try and join the AURN and Purple air data 

combined_data <- broadley_terrace |> 
  left_join(pm_marlyebone_road, join_by(time_stamp == date)) |> 
  pivot_longer(cols = c(pm2.5_atm, pm2.5), names_to = "pm_measurement", values_to = "pm") |> 
  mutate(pm_measurement = ifelse(pm_measurement == "pm2.5", "AURN", "PurpleAir"))
  
  
  
ggplot(combined_data) +
  geom_line(aes(x = time_stamp, y = pm, colour = pm_measurement)) +
  facet_wrap(~pm_measurement, ncol = 1)



# Needs to be wide data for dygraphs 

combined_data_wide <- combined_data |> 
  pivot_wider(names_from = "pm_measurement", values_from = "pm") |> 
  mutate(PurpleAir = round(PurpleAir, digits = 0)) 


dygraphs::dygraph(combined_data_wide) |> 
  dygraphs::dySeries("AURN", label = "AURN") |> 
  dygraphs::dySeries("PurpleAir", label = "PurpleAir")  |> 
  dygraphs::dyRangeSelector()
  


# Comparing one datset to the other..... 

combined_data_wide |> 
  #mutate(PurpleAir = round(PurpleAir, digits = 0)) |> 
  ggplot() +
  geom_point(aes(x=PurpleAir, y = AURN, colour = time_stamp)) +
  geom_smooth(aes(x=PurpleAir, y = AURN)) +
  scale_x_continuous(limits = c(0,50))+
  scale_y_continuous(limits = c(0,50))+
    theme(legend.position = "none")

# Now lets turn one of these into a percentage of the other.... PurpleAir as a percentage of AURN 

relative_pct <- combined_data_wide |> 
  mutate(percentage_of_AURN = PurpleAir/AURN * 100) 


# Get lots of Inf and NaN values due to the dividing by 0 problem. 
