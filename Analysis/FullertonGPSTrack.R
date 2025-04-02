df <- st_read("Data/Fullerton Transmitter Track.gpx", layer = "track_points") %>% 
  mutate(
  time = rawData$time, 
  lon = st_coordinates(rawData)[,1], 
  lat = st_coordinates(rawData)[,2]
) %>% select(time, lon, lat)

