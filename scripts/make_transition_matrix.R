library(tidyverse)
library(lubridate)
full_bikes <- read.csv("data/full_bikes_named.csv")
stations <- read.csv("data/stations.csv")

#### MORNING

station_to_station <- full_bikes %>%
  mutate(StartTime = hms(StartTime)) %>%
  filter(StartTime > hms("6:00:00") & StartTime < hms("12:00:00") 
         &  !is.na(StartLatitude) & !is.na(EndLatitude)) %>%
  group_by(StartHub, EndHub) %>%
  summarise(trip_count = n())

count_matrix <- station_to_station %>%
  spread(EndHub, trip_count) 
  
count_matrix[is.na(count_matrix)] <- 0

colnames(count_matrix)[2:175] == count_matrix$StartHub

bike_matrix <- as.matrix(count_matrix[1:174, 2:175])
colnames(bike_matrix) <- NULL

for (i in 1:174) {
  bike_matrix[i,] <- bike_matrix[i,] / sum(bike_matrix[i,])
}

stat_dist_morning <- (bike_matrix %^% 1000)[1,]
#### REPEAT FOR AFTERNOON



station_to_station <- full_bikes %>%
  mutate(StartTime = hms(StartTime)) %>%
  filter(StartTime > hms("14:00:00") & StartTime < hms("20:00:00") 
         &  !is.na(StartLatitude) & !is.na(EndLatitude)) %>%
  group_by(StartHub, EndHub) %>%
  summarise(trip_count = n())

count_matrix <- station_to_station %>%
  spread(EndHub, trip_count) 

count_matrix[is.na(count_matrix)] <- 0

count_matrix <- count_matrix %>%
  select(EndHub, count_matrix$StartHub)

colnames(count_matrix)[2:175] == count_matrix$StartHub

bike_matrix <- as.matrix(count_matrix[1:174, 2:175])
colnames(bike_matrix) <- NULL

for (i in 1:174) {
  bike_matrix[i,] <- bike_matrix[i,] / sum(bike_matrix[i,])
}

stat_dist_evening <- (bike_matrix %^% 1000)[1,]

stat_dists <- data.frame(station = colnames(count_matrix)[2:175], 
                         morning = stat_dist_morning,
                         evening = stat_dist_evening)

stat_dists_lat_lon <- stat_dists %>%
  inner_join(stations, by = c("station" = "StartHub"))

leaflet() %>%
  addCircles(lng = stat_dists_lat_lon$StartLongitude,
             lat = stat_dists_lat_lon$StartLatitude, 
             radius = 1000* sqrt(stat_dists_lat_lon$morning)+1, 
             color = "red", opacity = .85) %>%
  addProviderTiles(providers$Stamen.Toner)

leaflet() %>%
  addCircles(lng = stat_dists_lat_lon$StartLongitude,
             lat = stat_dists_lat_lon$StartLatitude, 
             radius = 1000* sqrt(stat_dists_lat_lon$evening)+1, 
             color = "red", opacity = .85) %>%
  addProviderTiles(providers$Stamen.Toner)


leaflet() %>%
  addCircles(lng = stat_dists_lat_lon$StartLongitude,
             lat = stat_dists_lat_lon$StartLatitude, 
             radius = 5000* abs(sqrt(stat_dists_lat_lon$morning) - sqrt(stat_dists_lat_lon$evening)), 
             color = if_else(sqrt(stat_dists_lat_lon$morning) - sqrt(stat_dists_lat_lon$evening) > 0, "green", "red"), opacity = .85) %>%
  addProviderTiles(providers$Stamen.Toner)








