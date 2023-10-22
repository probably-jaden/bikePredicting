library(tidyverse)
library(tidymodels)
library(vroom)

test <- vroom("testbike.csv")
train <- vroom("trainbike.csv")

train <- train %>% 
  mutate(logCount = log(count)) %>% 
  select(-c(casual, registered, count))

#train$logCount <- ifelse(train$logCount == 0, 0.001, train$logCount)

bike_recipe <- recipe(logCount ~ ., data=train) %>% # Set model formula and dataset
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>%
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_poly(humidity, degree=2) %>% #Create polynomial expansion of var
  step_date(datetime, features="dow") %>% # gets day of week
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%  # dummy variables for all categorical features
  step_normalize(all_numeric_predictors()) # data must be normalized to enact penalty's
  
# Apply the recipe to the training data
train_preprocessed <- prep(bike_recipe, data = train)

# Preview the preprocessed data
head(bake(train_preprocessed, new_data = train))
head(bake(train_preprocessed, new_data = test))

## Define the model
model <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(),
                        min_n=tune()) %>% #Type of model
  set_engine("rpart") %>% # What R function to use
  set_mode("regression")

## Set up the whole workflow
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(model)

## Grid of values to tune over
tuning_grid <- grid_regular(tree_depth(),
                            cost_complexity(),
                            min_n(),
                            levels = 5)

# Split data for CV
folds <- vfold_cv(train, v = 5, repeats=1)

# Run the cross-validation
CV_results <- bike_workflow %>%
  tune_grid(resamples=folds,
          grid=tuning_grid,
          metrics=metric_set(rmse, mae, rsq))

# Find Best Tuning Parameters
bestTune <- CV_results %>%
  select_best("rmse")


# Finalize the Workflow & fit it
final_wf <- bike_workflow %>%
  finalize_workflow(bestTune) %>%
  fit(data=train)

## Look at the fitted LM model this way
extract_fit_engine(final_wf) %>%
  summary()

## Get Predictions for test set AND format for Kaggle
test_preds <- predict(final_wf, new_data = test) %>%
  bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle) as well as undo the log
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(count = exp(count)) %>% 
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write prediction file to CSV
vroom_write(x=test_preds, file="./BikeRentals/TestPreds.csv", delim=",")



