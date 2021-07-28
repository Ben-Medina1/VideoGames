library(tidyverse)
library(readr)
library(caret)
library(broom)
Orig_Data<-read_csv("Kaggle_Video_Game_Sales_Data/Video_Games_Sales_as_at_22_Dec_2016.csv")

#EDA 1: Year of Release##
Data %>% select(Year_of_Release) %>% na.omit() %>% ggplot(aes(Year_of_Release)) +
  geom_bar() 

#EDA 2: Genre
Orig_Data %>% select(Genre) %>% na.omit() %>% ggplot(aes(Genre)) +
  geom_bar(size =2)

#EDA 3: Publisher
Data %>% select(Publisher) %>% 
  ggplot(aes(Publisher)) +
  geom_bar()

#EDA 4: Rating##
Data %>% select(Rating) %>% na.omit() %>%
  ggplot(aes(Rating)) +
  geom_bar()

##Create training/test sets##
library(caret)
set.seed(0623)
test_index <- createDataPartition(Orig_Data$Global_Sales, times = 1, p = 0.8, list = FALSE)
train_set <-  Orig_Data[test_index, ] 
test_set <- Orig_Data[-test_index, ]

##EDA Does Rating have effect on Global_Sales##
train_set %>% select(Rating, Global_Sales) %>% filter(!is.na(Rating)) %>% 
  ggplot(aes(Rating,Global_Sales)) +
  geom_boxplot()
#Answer: somewhat hard to see but there is an effect of some sort.
Rating_vs_Sales<-lm(Global_Sales ~ Rating, data=train_set)
glance(Rating_vs_Sales)
tidy(Rating_vs_Sales)
#P-values say nope!
#####

##EDA reviewing Global_Sales vs Genre##
train_set %>% select(Genre, Global_Sales) %>% filter(!is.na(Genre)) %>% 
  ggplot(aes(Genre,Global_Sales)) +
  geom_boxplot()

train_set %>% select(Genre, Global_Sales) %>% filter(!is.na(Genre)) %>% 
  ggplot(aes(Genre,Global_Sales)) +
  geom_point()
#Probably not a good variable to predict global sales??
#Let's see
Genre_vs_Sales<-lm(Global_Sales~Genre,data = train_set)
tidy(Genre_vs_Sales)
#Answer: genre can work. 
#####

##EDA Global_Sales##
train_set %>% select(Global_Sales) %>% filter(!is.na(Global_Sales)) %>% 
  ggplot(aes(Global_Sales)) +
  geom_density()

train_set %>% select(Global_Sales) %>% filter(!is.na(Global_Sales)) %>% 
  ggplot(aes(Global_Sales)) +
  geom_histogram()

train_set %>% select(Global_Sales) %>% filter(!is.na(Global_Sales)) %>% 
  ggplot(aes(sample = scale(Global_Sales)) +
  geom_qq()
#Clearly this is not a normal distribution by any means
#####

##EDA User_Score##
train_set %>% select(User_Score) %>% filter(!is.na(User_Score)) %>%
  ggplot(aes(User_Score)) +
  geom_bar()
#####

##EDA Critic_Score##
train_set %>% select(Critic_Score) %>% filter(!is.na(Critic_Score)) %>%
  ggplot(aes(Critic_Score)) +
  geom_histogram(binwidth = 10)

#QQ Plot to verify normal distribution
train_set %>% select(Critic_Score) %>% filter(!is.na(Critic_Score)) %>%
  ggplot(aes(sample = Critic_Score)) +
  geom_qq()
#Normal distribution
#####

##EDA Critic_Count##
train_set %>% select(Critic_Count) %>% filter(!is.na(Critic_Count)) %>%
  ggplot(aes(Critic_Count)) +
  geom_histogram(binwidth = 1)
#QQ Plot
train_set %>% select(Critic_Count) %>% filter(!is.na(Critic_Count)) %>%
  ggplot(aes(sample = Critic_Count)) +
  geom_qq()
#Not normal, maybe log?
#####

##EDA Critic_Score##
train_set %>% select(Critic_Score) %>% filter(!is.na(Critic_Score)) %>%
  ggplot(aes(Critic_Score)) +
  geom_histogram(binwidth = 2)
#Much more normal distribution
#####

##EDA Platform
train_set %>% select(Platform) %>% filter(!is.na(Platform)) %>%
  ggplot(aes(Platform))+
  geom_bar()
#Relationship with Global_Sales?
train_set %>% select(Platform, Global_Sales) %>% filter(!is.na(Platform)) %>% 
  ggplot(aes(Platform,Global_Sales, limit=40)) +
  geom_boxplot()
#small relationship? Some box plots are higher than others. 
Platform_vs_Sales<-lm(Global_Sales ~ Platform, data=train_set)
glance(Platform_vs_Sales)
tidy(Platform_vs_Sales)
#P-values are low enough to say there is a relationship. 
#####
##EDA Testing whether there's correlation between variables Critic_Score & Critic_Count ##
train_set %>% glimpse(

##RMSE formula for checking model##
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
#####


