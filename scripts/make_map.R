full_bikes <- read.csv("data/full_bikes_named.csv")
library(tidyverse)

X2018_06 <- read_csv("data/2018_06.csv")
X2018_07 <- read_csv("data/2018_07.csv")
X2018_08 <- read_csv("data/2018_08.csv")
X2019_06 <- read_csv("data/2019_06.csv")
X2019_07 <- read_csv("data/2019_07.csv")
X2019_08 <- read_csv("data/2019_08.csv")

colnames(X2019_06) <- colnames(X2018_06)
colnames(X2019_07) <- colnames(X2018_06)
colnames(X2019_08) <- colnames(X2018_06)

pre_bikes <- rbind(X2018_06, X2018_07, X2018_08, X2019_06, X2019_07,X2019_08)


stations <- pre_bikes %>%
  filter(!is.na(StartHub)) %>%
  group_by(StartHub, StartLatitude, StartLongitude) %>%
  summarise(count = n()) %>%
  arrange(count) %>% 
  select(StartLatitude, StartLongitude, count)
library(leaflet)

leaflet() %>%
  addCircles(lng = stations$StartLongitude,
             lat = stations$StartLatitude, 
             radius = sqrt(stations$count), 
             color = "orange", opacity = .85) %>%
  addProviderTiles(providers$Stamen.Toner)

















