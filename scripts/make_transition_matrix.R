full_bikes <- read.csv("data/full_bikes_named.csv")

tst <- full_bikes %>%
  group_by(StartHub) %>%
  summarise(count = n()) %>%
  arrange(count)

tst <- full_bikes %>%
  group_by(StartHub, EndHub) %>%
  summarise(n())
  select(StartHub, EndHub)