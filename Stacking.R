# Reading the csv files
bike_share_train <- read.csv("../input/train.csv", header=T)
bike_share_test <- read.csv("../input/test.csv", header=T)

str(bike_share_train)
colnames(bike_share_train)
#head(bike_share_train)

# Ignore the casual, registered fields as this sum is equal to count field
bike_share_train <- bike_share_train[,-c(10,11)]

# Converting integer to factor on training set
bike_share_train$season <- as.factor(bike_share_train$season)
bike_share_train$holiday <- as.factor(bike_share_train$holiday)
bike_share_train$workingday <- as.factor(bike_share_train$workingday)
bike_share_train$weather <- as.factor(bike_share_train$weather)


# Converting int to factor on test set
bike_share_test$season <- as.factor(bike_share_test$season)
bike_share_test$holiday <- as.factor(bike_share_test$holiday)
bike_share_test$workingday <- as.factor(bike_share_test$workingday)
bike_share_test$weather <- as.factor(bike_share_test$weather)


#Deriving day, hour from datetime field Train & Test
library(lubridate)
bike_share_train$datetime <- ymd_hms(bike_share_train$datetime)
bike_share_train$hour <- hour(bike_share_train$date)
bike_share_train$day <- wday(bike_share_train$date)
bike_share_train$month <- month(bike_share_train$date, label=T)
str(bike_share_train) 
names(bike_share_train)
bike_share_train[,11:13]<-lapply(bike_share_train[,11:13], factor) #converting derived variables into factors

bike_share_test$datetime <- ymd_hms(bike_share_test$datetime)
bike_share_test$hour <- hour(bike_share_test$date)
bike_share_test$day <- wday(bike_share_test$date)
bike_share_test$month <- month(bike_share_test$date, label=T)
str(bike_share_test)
names(bike_share_test)
bike_share_test[,10:12]<-lapply(bike_share_test[,10:12], factor) #converting derived variables into factors


# Removing datetime field 
bike_share_train$datetime <- NULL
colnames(bike_share_train)

#Exploratory Data Analysis
library(sqldf)
library(ggplot2)
# Get the average count of bikes rent by season, hour
season_summary_by_hour <- sqldf('select season, hour, avg(count) as count from bike_share_train group by season, hour')

# From this plot it shows, 
# There are more rental in morning(from 7-9th hour) and evening(16-19th hour)
# People rent bikes more in Fall, and much less in Spring
p1<-ggplot(bike_share_train, aes(x=hour, y=count, color=season))+
  geom_point(data = season_summary_by_hour, aes(group = season))+
  geom_line(data = season_summary_by_hour, aes(group = season))+
  ggtitle("Bikes Rent By Season")+ theme_minimal()+
  scale_colour_hue('Season',breaks = levels(bike_share_train$season), 
                   labels=c('spring', 'summer', 'fall', 'winter'))
p1

# Get the average count of bikes rent by weather, hour
weather_summary_by_hour <- sqldf('select weather, hour, avg(count) as count from bike_share_train group by weather, hour')

# From this plot it shows, 
# People rent bikes more when weather is good
# We see bike rent only at 18th hour when weather is very bad
p2<-ggplot(bike_share_train, aes(x=hour, y=count, color=weather))+
  geom_point(data = weather_summary_by_hour, aes(group = weather))+
  geom_line(data = weather_summary_by_hour, aes(group = weather))+
  ggtitle("Bikes Rent By Weather")+ scale_colour_hue('Weather',breaks = levels(bike_share_train$weather), 
                                                     labels=c('Good', 'Normal', 'Bad', 'Very Bad'))
p2

# Get the average count of bikes rent by day, hour
day_summary_by_hour <- sqldf('select day, hour, avg(count) as count from bike_share_train group by day, hour')

# From this plot it shows, 
# There are more bikes rent on weekdays during morining and evening
# There are more bikes rent on weekends during daytime
p3<-ggplot(bike_share_train, aes(x=hour, y=count, color=day))+
  geom_point(data = day_summary_by_hour, aes(group = day))+
  geom_line(data = day_summary_by_hour, aes(group = day))+
  ggtitle("Bikes Rent By Weekday")+ scale_colour_hue('Weekday',breaks = levels(bike_share_train$day),
                                                     labels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
p3


# Splitting the Train dataset
library(caTools)
set.seed(123)
split <- sample.split(bike_share_train$count, SplitRatio = 0.75)
training_set <- subset(bike_share_train, split == TRUE)
validation_set <- subset(bike_share_train, split == FALSE)

# Applying Linear Regression model
lmBikeRent <- lm(count~., data = training_set)
summary(lmBikeRent)

#Residual Plots
# Change the panel layout to 2 x 2
par(mfrow = c(2, 2))
# Diagnostic Plots
# Residuals vs Fitted: This plot shows if residuals have non-linear patterns.If you find equally spread residuals around a horizontal line without distinct patterns, that is a good indication you don't have non-linear relationships. 
# Normal Q-Q: This plot shows if residuals are normally distributed.Do residuals follow a straight line well or do they deviate severely? It's good if residuals are lined well on the straight dashed line.
# Scale-Location: It's also called Spread-Location plot. This plot shows if residuals are spread equally along the ranges of predictors. This is how you can check the assumption of equal variance (homoscedasticity). It's good if you see a horizontal line with equally (randomly) spread points.
# Residuals vs Leverage: This plot helps us to find influential cases(outliers) if any. Check if any points fall outside of a dashed line, Cook's distance(meaning they have high Cook's distance score).Those outside points are influential to the regression results.The regression results will be altered if we exclude those cases.
# From plots, it shows there is a pattern at one location as highlighted in plots with green color.
plot(lmBikeRent)

#Stepwise Model Selection
# Now performs stepwise model selection by AIC with both directions(Forward, Backward)
library(MASS)
library(car)
lmBikeRentAIC<-stepAIC(lmBikeRent, direction="both")
# lmBikeRentVIF<-vif(lmBikeRent)
summary(lmBikeRentAIC)

# Apply prediction on validation set
lm_predict_validation <- predict(lmBikeRentAIC, newdata = validation_set)

#Lets compute the root-mean-square error value between actual and predicted
library(Metrics)
validation_rmse<-rmse(validation_set$count,lm_predict_validation)
print("root-mean-square error between actual and predicted")
print(validation_rmse)

# Let's check the summary of predicted count values
cat("\n")
print("summary of predicted count values")
summary(lm_predict_validation)

# summary of actual count values
print("summary of actual count values")
summary(validation_set$count)

#From above summary we saw negative values of predicted count.
# We don't want negative values as forecast for bike count. Replace all negative numbers with 1 
Output2Mod <- lm_predict_validation
Output2Mod[lm_predict_validation<=0] <-1

# Check again the summary of predicted count values
print("summary of predicted count values after replaced the negative values")
summary(Output2Mod)

# As we replaced the negative values, the rmse value got reduced
print("root-mean-square error value after replaced the negative values")
print(rmse(validation_set$count,Output2Mod))

cat("\n")
#If we want to penalize under-prediction of demand, rmsle might be a better metric
validaion_rmsle<-rmsle(validation_set$count,Output2Mod)
print("root-mean-square-log error value after replaced the negative values")
print(validaion_rmsle)

#Log Transformation
# Since we got negative predicted values, let's do log transformation and run regression model again
lmBikeRentLog <- lm(log(count)~., data = training_set)

# Now performs stepwise model selection on log model
lmBikeRentLogAIC <- stepAIC(lmBikeRentLog, direction="both")

lm_predict_validation_log <- predict(lmBikeRentLogAIC,newdata=validation_set)

# As the predicted values are in log format, use exponential(exp) to convert from log to non-log values
lm_predict_validation_nonlog <- exp(lm_predict_validation_log)

# Let's check the summary of predicted count values, it shows there are no negative values
print("summary of predicted count values after log transformation")
summary(lm_predict_validation_nonlog)

# Check rmsle value again, it got reduced from 0.9549201 to 0.6278527
validaion_nonlog_rmsle<-rmsle(validation_set$count,lm_predict_validation_nonlog)
print("root-mean-square-log error value after log transformation")
print(validaion_nonlog_rmsle)

Residual vs Fitted plot
# Let's check the Residual vs Fitted plot
# It shows some points forms a straight lines
# If you select bottom straight line points using "identify", you will find that the bike rent count is 1
# and next straight line points will have bike rent count of 2. 
plot(lmBikeRentLog$fitted.values, lmBikeRentLog$residuals)

# Run model on test data
lm_predict_test_log <- predict(lmBikeRentLogAIC,newdata=bike_share_test)

# As the predicted values are in log format, use exponential(exp) to convert from log to non-log values
lm_predict_test_nonlog <- exp(lm_predict_test_log)

final_df <- cbind(as.data.frame(lm_predict_test_nonlog), bike_share_test$datetime)
colnames(final_df) <- c("count", "datetime")
final_df

