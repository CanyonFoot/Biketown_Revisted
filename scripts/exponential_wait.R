library(tidyverse)
library(lubridate)

full_bikes <- read.csv("data/full_bikes_named.csv")
lambda <- read.csv("data/lambda.csv")
stations_most_pop <- full_bikes %>%
  group_by(StartHub) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
  

graph_function_start <- function(start_station) {
  bike_times <- full_bikes %>%
    filter(StartHub == start_station) %>%
    mutate(full_date_time = paste(StartDate, StartTime, sep = " "),
           StartTime = hms(StartTime),
           full_date_time = mdy_hms(full_date_time)) %>%
    filter(StartTime > hms("8:00:00") & StartTime < hms("18:00:00")) 
    

  lambda_df <- lambda %>%
    filter(StartHub == start_station)
  
  lambda_val <- lambda_df$trip_count
  
  time_vec <- rep(NA, length(bike_times$X))
  for (i in 1:(length(bike_times$X) - 1)) {
    if (bike_times$EndDate[i] == bike_times$EndDate[i+1]) {
      time_vec[i] <- as.numeric(as.duration(bike_times$StartTime[i+1] -  bike_times$StartTime[i])) 
    }
  }
  

  mean_time <- mean(time_vec[time_vec >= 0], na.rm =T) / 3600
  g <- data.frame(time = time_vec / 3600) %>%
    filter(time >= 0) %>%
    ggplot(aes(x = time, color = "Actual")) +
    geom_density() +
    stat_function(fun = dexp, args = c(1/mean_time), aes(color = "Theory")) + 
  return(list(mean_time, lambda_val, g))
}













##  END FUNCTION 


graph_function_end <- function(End_station) {
  bike_times <- full_bikes %>%
    filter(EndHub == End_station) %>%
    mutate(full_date_time = paste(EndDate, EndTime, sep = " "),
           EndTime = hms(EndTime),
           full_date_time = mdy_hms(full_date_time)) %>%
    filter(EndTime > hms("8:00:00") & EndTime < hms("18:00:00")) %>%
    arrange(full_date_time)
  
  
  time_vec <- rep(NA, length(bike_times$X))
  for (i in 1:(length(bike_times$X) - 1)) {
    if (bike_times$EndDate[i] == bike_times$EndDate[i+1]) {
      time_vec[i] <- as.duration(bike_times$EndTime[i+1] -  bike_times$EndTime[i]) 
    }
  }
  
  
  # Mean is 24.34
  mean_time <- mean(time_vec, na.rm =T) / 60
  
  g1 <- data.frame(time = time_vec / 60) %>%
    ggplot(aes(x = time, color = "Actual")) +
    geom_density() +
    stat_function(fun = dexp, args = c(1/mean_time), aes(color = "Theory")) 
  
  g2 <- plot(ecdf(time_vec/60))
  
  return(list(mean_time, g1))
}


lambda_vec <- rep(NA, 174) 
graph_vec <- rep(NA, 174) 
for (i in 1:174) {
  g <- graph_function_start(stations_most_pop$StartHub[i])
  lambda_vec[i] <- g[[1]]
  graph_vec <- g[[2]]
}



