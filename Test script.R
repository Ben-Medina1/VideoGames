library(tidyverse)
library(readr)
library(caret)

Data<-read_csv("Kaggle_Video_Game_Sales_Data/Video_Games_Sales_as_at_22_Dec_2016.csv")

#EDA 1: Year of Release##
Data %>% select(Year_of_Release) %>% na.omit() %>% ggplot(aes(Year_of_Release)) +
  geom_bar() 

#EDA 2: Genre
Data %>% select(Genre) %>% na.omit() %>% ggplot(aes(Genre)) +
  geom_bar()

#EDA 3: Publisher
Data %>% select(Publisher) %>% 
  ggplot(aes(Publisher)) +
  geom_bar()