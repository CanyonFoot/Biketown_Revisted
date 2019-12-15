X2018_06 <- read_csv("data/2018_06.csv")
X2018_07 <- read_csv("data/2018_07.csv")
X2018_08 <- read_csv("data/2018_08.csv")
X2019_06 <- read_csv("data/2019_06.csv")
X2019_07 <- read_csv("data/2019_07.csv")
X2019_08 <- read_csv("data/2019_08.csv")

colnames(X2019_06) <- colnames(X2018_06)
colnames(X2019_07) <- colnames(X2018_06)
colnames(X2019_08) <- colnames(X2018_06)

full_bikes <- rbind(X2018_06, X2018_07, X2018_08, X2019_06, X2019_07,X2019_08)
library(geosphere)
library(tidyverse)

stations <- full_bikes %>%
  filter(!is.na(StartHub)) %>%
  select(StartHub, StartLongitude, StartLatitude) %>%
  unique()

bikes_no_start_station <- full_bikes %>%
  filter(is.na(StartHub) &  !is.na(StartLatitude) & !is.na(StartLongitude)) %>% 
  select(StartHub, StartLongitude, StartLatitude, RouteID)

bikes_no_end_station <- full_bikes %>%
  filter(is.na(EndHub) & !is.na(EndLatitude) & !is.na(EndLongitude)) %>% 
  select(EndHub, EndLongitude, EndLatitude, RouteID)

stations_dist_stations <- distm(stations[2:3], stations[2:3])

stations_dist_start <- distm(bikes_no_start_station[2:3], stations[2:3]) 
stations_dist_end <- distm(bikes_no_end_station[2:3], stations[2:3]) 


closest_start_station <- matrix(rep(NA, 2*length(stations_dist_start[,1])), ncol = 2) 
for (i in 1:length(stations_dist_start[,1])) {
  closest_start_station[i,1] <- min(stations_dist_start[i,])
  closest_start_station[i,2] <- which.min(stations_dist_start[i,])
}


closest_end_station <- matrix(rep(NA, 2*length(stations_dist_end[,1])), ncol = 2) 
for (i in 1:length(stations_dist_end[,1])) {
  closest_end_station[i,1] <- min(stations_dist_end[i,])
  closest_end_station[i,2] <- which.min(stations_dist_end[i,])
}

bikes_no_end2 <- cbind(bikes_no_end_station, closest_end_station)
colnames(bikes_no_end2) <- c(colnames(bikes_no_end_station), "min_dist", "closest_station")


bikes_no_start2 <- cbind(bikes_no_start_station, closest_start_station)
colnames(bikes_no_start2) <- c(colnames(bikes_no_start_station), "min_dist", "closest_station")

bikes_no_end_names <- bikes_no_end2 %>%
  mutate(EndHub2 = if_else(min_dist < 300, stations$StartHub[closest_station], "No Nearby Hub")) %>%
  select(RouteID, EndHub2)

bikes_no_start_names <- bikes_no_start2 %>%
  mutate(StartHub2 = if_else(min_dist < 300, stations$StartHub[closest_station], "No Nearby Hub")) %>%
  select(RouteID, StartHub2)

full_bikes_named <- full_bikes %>%
  full_join(bikes_no_start_names) %>%
  full_join(bikes_no_end_names) %>%
  mutate(EndHub = if_else(is.na(EndHub2), EndHub, EndHub2)) %>%
  mutate(StartHub = if_else(is.na(StartHub2), StartHub, StartHub2))


write.csv(full_bikes_named, "data/full_bikes_named.csv")

  


















