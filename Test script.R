library(tidyverse)
library(readr)
library(caret)

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
test_index <- createDataPartition(Data$Global_Sales, times = 1, p = 0.8, list = FALSE)
train_set <-  Orig_Data[test_index, ] 
test_set <- Orig_Data[-test_index, ]

##EDA Does Rating have effect on Global_Sales##
train_set %>% select(Rating, Global_Sales) %>% filter(!is.na(Rating)) %>% 
  ggplot(aes(Rating,Global_Sales)) +
  geom_boxplot()
#Answer: somewhat hard to see but there is an effect of some sort.

##EDA reviewing Global_Sales vs Genre##
train_set %>% select(Genre, Global_Sales) %>% filter(!is.na(Genre)) %>% 
  ggplot(aes(Genre,Global_Sales)) +
  geom_boxplot()

train_set %>% select(Genre, Global_Sales) %>% filter(!is.na(Genre)) %>% 
  ggplot(aes(Genre,Global_Sales)) +
  geom_point()
#Probably not a good variable to predict global sales??
#Let's see

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

##EDA User_Score##
train_set %>% select(User_Score) %>% filter(!is.na(User_Score)) %>%
  ggplot(aes(User_Score)) +
  geom_bar()


##EDA Critic_Score##
train_set %>% select(Critic_Score) %>% filter(!is.na(Critic_Score)) %>%
  ggplot(aes(Critic_Score)) +
  geom_histogram(binwidth = 10)

#QQ Plot to verify normal distribution
train_set %>% select(Critic_Score) %>% filter(!is.na(Critic_Score)) %>%
  ggplot(aes(sample = Critic_Score)) +
  geom_qq()
#Normal distribution

##EDA Critic_Count##
train_set %>% select(Critic_Count) %>% filter(!is.na(Critic_Count)) %>%
  ggplot(aes(Critic_Count)) +
  geom_histogram(binwidth = 1)
#QQ Plot
train_set %>% select(Critic_Count) %>% filter(!is.na(Critic_Count)) %>%
  ggplot(aes(sample = Critic_Count)) +
  geom_qq()
#Not normal, maybe log?
train_set %>% select(Critic_Count) %>% filter(!is.na(Critic_Count)) %>%
  ggplot(aes(sample = Critic_Count)) +
  geom_qq()

##EDA Critic_Score##
train_set %>% select(Critic_Score) %>% filter(!is.na(Critic_Score)) %>%
  ggplot(aes(Critic_Score)) +
  geom_histogram(binwidth = 2)
#Much more normal distribution

##EDA remove excess near zero variance variables
library(caret)
nzv <- nearZeroVar(x)

##EDA Platform
train_set %>% select(Platform) %>% filter(!is.na(Platform)) %>%
  ggplot(aes(Platform))+
  geom_bar()
#Relationship with Global_Sales?
train_set %>% select(Platform, Global_Sales) %>% filter(!is.na(Platform)) %>% 
  ggplot(aes(Platform,Global_Sales, limit=40)) +
  geom_boxplot()
  
#small relationship? Some box plots are higher than others. 
Platform_vs_Sales<-lm()




