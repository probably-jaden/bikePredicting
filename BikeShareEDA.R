

setwd("/Users/jse/Documents/STAT348")
library(plotly)
library(readr)
library(vroom)

data <-read_csv("train.csv")
summary(data)

plot_ly(data, x = ~temp, y = ~registered, z = ~windspeed, type = "scatter3d", mode = "markers", size = 0.1)

trainV<-vroom("train.csv")

## 4 big options ot import data
#   RStudio's GUI import
#   read.csv()
#   read_csv() tidyverse 
#   readr()   also tidyverse
#   vroom()   highly recommended Speed, specific column types, name repair, combine cols

# What is an Exploratory Data Analysis
  # Data visualizations


#R tools for performing an EDA
#   dplyr::glimpse(dataset) - lists the variable type of each column
dplyr::glimpse(trainV)

#   skimr::skim(dataset)- nice overview of the dataset
skimr::skim(trainV)

#   DataExplorer::plot_intro(dataset) - visualization of glimpse
DataExplorer::plot_intro(trainV)

#   DataExplorer::plot_correlation(dataset) - correlation heat map between variables
heatMapCorr<-DataExplorer::plot_correlation(trainV)

tempS <- ggplot(data=trainV, aes(x=temp, y=count, color = workingday))+
  geom_point(size=0.1)+
  geom_smooth()+
  ggtitle("Temperature vs Bike Rentals")+
  ylab("Bike Rental")+
  xlab("Temperature in Celcsius")

humdS <- ggplot(data=trainV, aes(x=humidity, y=count, color = workingday))+
  geom_point(size=0.1)+
  geom_smooth()+
  ggtitle("Humidity vs Bike Rentals")+
  ylab("Bike Rental")+
  xlab("relative humidity")

seasS <- ggplot(data=trainV, aes(x=season, y=count, color = workingday))+
  geom_point(size=1)+
  geom_smooth()+
  ggtitle("Season vs Bike Rentals")+
  ylab("Bike Rental")+
  xlab("relative humidity")



#   DataExplorer::plot_bar(dataset) - bar charts of all discrete variables
DataExplorer::plot_bar(trainV)

#   DataExplorer::plot_histograms(dataset) - histograms of all numerical variables
DataExplorer::plot_histograms(trainV) 

#   DataExplorer::plot_missing(dataset) - percent missing in each column
DataExplorer::plot_missing(trainV) 

#   GGally::ggpairs(dataset) - 1/2 scatterplot and 1/2 correlation heat map
#GGally::ggpairs(trainV) # dangerously big

# library(patchwork) puts plots right besides each other
library(patchwork)

heatMapCorr+tempS+humdS+seasS

library(ggplot2)
library(ggcorrplot)


# Assuming `trainV` is a dataframe containing your data
correlation_matrix <- cor(trainV)
ggcorrplot(correlation_matrix, type = "lower", lab = TRUE)

library(ComplexHeatmap)

# Assuming `trainV` is a dataframe containing your data
correlation_matrix <- cor(trainV)
Heatmap(correlation_matrix)

library(heatmaply)

# Assuming `trainV` is a dataframe containing your data
correlation_matrix <- cor(trainV)
heatmaply(correlation_matrix)



### Wed Sept 12
library(tidymodels)


my_recipe <- recipe(rFormula, data=trainV) %>% # Set model formula and d2
  step_mutate(newVar=var1*var2) %>% #Create a new variable3
  step_poly(var, degree=2) %>% #Create polynomial expansion of var4
  step_date(timestamp, features="dow") %>% # gets day of week5
  step_time(timestamp, features=c("hour", "minute")) %>%
  step_dummy(all_nominal_predictors()) %>% #create dummy variables7
  step_zv(all_predictors()) %>% #removes zero-variance predictors8
  step_corr(variables, threshold=0.5) %>% # removes > than .5 corr9
  step_rm(var) %>% #removes a variables10
  step_select(var, -var2) #selects columns11
prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using myDataS12
bake(prepped_recipe, new_data=AnotherDataSet)







