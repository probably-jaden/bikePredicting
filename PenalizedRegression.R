setwd("/Users/jse/Documents/STAT348")
library(plotly)
library(readr)
library(vroom)
library(tune)
library(ranger)
library(tidymodels)
library(recipes)
library(skimr)
library(poissonreg)

trainV<-vroom("train.csv")
testV<- vroom("test.csv")

cleanTrain <- trainV %>% 
  mutate(logCount = log(count)) %>% 
  select(-c(casual, registered))

skim(trainV)

train_recipe <- recipe(logCount ~., data = cleanTrain) %>%
  step_mutate(datetime = as.numeric(datetime)) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())
