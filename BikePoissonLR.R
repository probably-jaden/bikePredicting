setwd("/Users/jse/Documents/STAT348")
library(plotly)
library(readr)
library(vroom)

trainV<-vroom("train.csv")
testV<- vroom("test.csv")

library(tidymodels)
library(recipes)
library(skimr)
library(poissonreg)

cleanTrain <- trainV %>% 
  mutate(logCount = log(count)) %>% 
  select(-c(casual, registered))

skim(trainV)

train_recipe <- recipe(logCount ~., data = cleanTrain) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>% 
  step_rm(-datetime) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())

processed_data <- prep(train_recipe) %>% bake(new_data = cleanTrain)


glimpse(processed_data)
skim(processed_data)

############

preg_model <- linear_reg(penalty=tune(), mixture=tune()) %>% #Set model and tuning6
                         set_engine("glmnet") # Function to fit in R7

## Set Workflow
preg_wf <- workflow() %>%
add_recipe(train_recipe) %>%
add_model(preg_model)

## Grid of values to tune over14
tuning_grid <- grid_regular(penalty(), mixture(), levels = 3) ## L^2 total tuning possibilities17

## Split data for CV19
folds <- vfold_cv(processed_data, v = 10, repeats=1)


CV_results <- preg_wf %>%
tune_grid(resamples=folds,
          grid=tuning_grid,
          metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL5

## Plot Results (example)
collect_metrics(CV_results) %>% # Gathers metrics into DF
  filter(.metric=="rmse") %>%
ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
geom_line()

## Find Best Tuning Parameters
bestTune <- CV_results %>%
select_best("rmse")


final_wf <-preg_wf %>%
finalize_workflow(bestTune) %>%
fit(data=myDataSet)

## Predict7
final_wf %>%
predict(new_data = myNewData)



################
# Model types

my_mod <- linear_reg() %>% 
  set_engine("lm")

pois_mod <- poisson_reg() %>% #Type of model3
  set_engine("glm")

#Penalized regression model
preg_model <- linear_reg(penalty = .5, mixture = .5) %>% 
  set_engine("glmnet")


##########
# Work Flows

bike_pois_workflow <- workflow() %>%
  add_recipe(train_recipe) %>%
  add_model(pois_mod) %>%
  fit(data = processed_data)

bike_workflow <- workflow() %>% 
  add_recipe(train_recipe) %>% 
  add_model(my_mod) %>% 
  fit(data = processed_data)

preg_wf <- workflow() %>% 
  add_recipe(train_recipe) %>% 
  add_model(preg_model) %>% 
  fit(data = processed_data)


## I can look at the fitted LM model this way
extract_fit_engine(bike_workflow) %>% 
  tidy

extract_fit_engine(bike_workflow) %>% 
  summary()

extract_fit_engine(bike_pois_workflow) %>% 
  tidy

extract_fit_engine(bike_pois_workflow) %>% 
  summary()


######
# predictions

bike_predictions_penalizedLM <- predict(preg_wf,
                                        new_data=testV) %>% 
  bind_cols(., testV) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=exp(count)) %>% 
  mutate(datetime=as.character(format(datetime)))


bike_predictions_pois <- predict(bike_pois_workflow,
                            new_data=testV) %>% 
  bind_cols(., testV) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=exp(count)) %>% 
  mutate(datetime=as.character(format(datetime)))


test_preds <- predict(bike_workflow, new_data = testV) %>%
  bind_cols(., testV) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and predictions
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle


#### Submit to CSV for kaggle 

write_csv(bike_predictions_penalizedLM, "bikeShareDemandPenal2.csv")
#Get Predictions for test set AND format for Kaggle


