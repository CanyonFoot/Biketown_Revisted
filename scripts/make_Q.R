library(tidyverse)
library(lubridate)
full_bikes <- read.csv("data/full_bikes_named.csv")
stations <- read.csv("data/stations.csv")

lambda <- station_to_station <- full_bikes %>%
  mutate(StartTime = hms(StartTime)) %>%
  filter(StartTime >= hms("7:00:00") & StartTime <= hms("12:00:00"))  %>%
  group_by(StartHub) %>%
  summarise(trip_count = n() / (5 * length(unique(full_bikes$StartDate))))
  

 station_to_station <- full_bikes %>%
  mutate(StartTime = hms(StartTime)) %>%
  filter(StartTime > hms("7:00:00") & StartTime < hms("12:00:00") 
         &  !is.na(StartLatitude) & !is.na(EndLatitude)) %>%
  group_by(StartHub, EndHub) %>%
  summarise(trip_count = n())


count_matrix <- station_to_station %>%
  spread(EndHub, trip_count) 

count_matrix[is.na(count_matrix)] <- 0

hubs <- as.character(count_matrix$StartHub)
colnames(count_matrix) <- as.character(colnames(count_matrix))

count_matrix <- count_matrix %>%
  ungroup() %>%
  select(hubs)

colnames(count_matrix)[1:174] == hubs


for (i in 1:174) {
  count_matrix[i,i] <- 0
  count_matrix[i,] <- count_matrix[i,] / sum(count_matrix[i,])
}

for (i in 1:174) {
  count_matrix[i,] <- count_matrix[i,] * lambda$trip_count[i]
  count_matrix[i,i] <-   - sum(count_matrix[i,])
}

Q <- as.matrix(count_matrix)
colnames(Q) <- NULL

library(expm)
P_function <- function(t) {
  expm(t * Q)
}

embedded_chain_matrix <- Q
for (i in 1:174) {
  
}

limit <- expm(100000 * Q)
image(t(limit), col = viridis(10))

