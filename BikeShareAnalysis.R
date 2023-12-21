setwd("/Users/jse/Documents/STAT348")
library(plotly)
library(readr)
library(vroom)

trainV<-vroom("train.csv")
testV<- vroom("test.csv")

library(tidymodels)
library(recipes)
library(skimr)



ggplot(trainV, aes(x = humidity)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
  labs(title = "Histogram of Windspeed", x = "Windspeed", y = "Frequency")

cleanTrain <- trainV %>% 
  select(-casual, -registered)

skim(trainV)


train_recipe <- recipe(count ~., data = cleanTrain) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>% 
  step_date(datetime, features = "dow") %>% 
  step_time(datetime,features = c("hour", "minute")) %>% 
  step_mutate(day_night = ifelse(datetime_hour >= 0 & datetime_hour <= 5 | 
                                   datetime_hour >= 18 & datetime_hour <= 23,
                                 0,1)) %>% 
  step_select(-datetime_dowFri, -datetime_minute)
  

processed_data <- prep(train_recipe) %>% bake(new_data = cleanTrain)


glimpse(processed_data)
skim(processed_data)

my_mod <- linear_reg() %>% 
  set_engine("lm")

bike_workflow <- workflow() %>% 
  add_recipe(train_recipe) %>% 
  add_model(my_mod) %>% 
  fit(data = processed_data)


## I can look at the fitted LM model this way
extract_fit_engine(bike_workflow) %>% 
  tidy

extract_fit_engine(bike_workflow) %>% 
  summary()

#Get Predictions for test set AND format for Kaggle
lin_preds<-predict(bike_workflow, new_data=testV) %>% 
  bind_cols(., testV) %>% 
  select(datetime, .pred) %>% 
  rename(count =.pred) %>% 
  mutate(count=pmax(0,count)) %>% 
  mutate(datetime=as.character(format(datetime())))

bike_predictions <- predict(bike_workflow, new_data = testV) 

bike_predictions[bike_predictions < 0] <- 0

result_df <- tibble(predictions = bike_predictions)

write_csv(bike_predictions, "bikeShareDemand.csv")
;            