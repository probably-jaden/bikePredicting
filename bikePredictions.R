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
head(bake(train_preprocessed, new_data = train))
head(bake(train_preprocessed, new_data = test))

## Define the model


## Set up the whole workflow
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(model) %>%
  fit(data=train)

## Look at the fitted LM model this way
extract_fit_engine(bike_workflow) %>%
  summary()

## Get Predictions for test set AND format for Kaggle
test_preds <- predict(bike_workflow, new_data = test) %>%
  bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write prediction file to CSV
vroom_write(x=test_preds, file="./BikeRentals/TestPreds.csv", delim=",")



