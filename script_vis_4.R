library(ggplot2)
library(ggthemes)
library(ggrepel)
library(readr)
library(dplyr)
library(stringr)
library(tools)
library(magrittr)

full_trains <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/full_trains.csv")

# Visualisation 4 ####
full_trains_agg <- full_trains %>% 
  filter(str_detect(departure_station, "PARIS")) %>%
  mutate(departure_station = toTitleCase(tolower(departure_station))) %>%
  mutate(month = factor(month, levels = 1:12, labels = month.name)) %>%
  group_by(month, year, departure_station) %>%
  summarise(mean_num_of_canceled_trains = mean(num_of_canceled_trains, na.rm = TRUE)) 

full_trains_agg %>%  
  ggplot(aes(x = departure_station, 
             y = mean_num_of_canceled_trains)) +
  geom_jitter(aes(colour = departure_station), width = .2, 
              alpha = .6, size = 5, stroke = 1) +
  geom_label_repel(data = filter(full_trains_agg, 
                                 (year == 2018) & (month == "May" | 
                                                     month == "April" | 
                                                     month == "June")), 
                   aes(x = departure_station, 
                       y = mean_num_of_canceled_trains,
                       label = month)) +
  guides(colour = FALSE) +
  coord_flip() +
  labs(x = NULL, y = NULL, 
       title = "Cancelled Trains from Paris Railway Stations by Year",
       subtitle = "Average Number of Cancellations per Month - Strike Months in 2018 highlighted") +
  theme(text = element_text(size = 12)) +
  scale_color_tableau() +
  facet_wrap(~ year) 
