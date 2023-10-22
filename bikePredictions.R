library(tidyverse)
library(tidymodels)
library(vroom)

test <- vroom("testbike.csv")
train <- vroom("trainbike.csv")

train <- train %>% 
  select(-c(casual, registered))

bike_recipe <- recipe(count ~ ., data=train) %>% # Set model formula and dataset
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>%
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_poly(humidity, degree=2) %>% #Create polynomial expansion of var
  step_date(datetime, features="dow") %>% # gets day of week
  step_time(datetime, features=c("hour", "minute")) #create time variable
  
# Apply the recipe to the training data
train_preprocessed <- prep(bike_recipe, data = train)

# Preview the preprocessed data
head(bake(train_preprocessed, new_data = test))

bake(train_preprocessed, new_data = train)
bake(train_preprocessed, new_data = test) 


