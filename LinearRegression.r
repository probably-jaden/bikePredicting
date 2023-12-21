# Predict scoring set with model and saved to relative file_name.
predict_scoring_set <- function(model_to_score, file_name, newdata = biketest.df) {
  # file_name e.g. "output/nn_defaults_5_variables.csv"
  pred <- predict(model_to_score, newdata = newdata, na.action = na.pass)
  pred <- negative_to_zero(pred)
  # write submission in kaggle format
  # datetime,count
  # 2011-01-20 00:00:00,0
  write.csv(data.frame(datetime = newdata$datetime, count = pred),
            file = file_name, row.names = FALSE)
}

# courtesy of tutorial: http://uc-r.github.io/regression_trees
# function to get optimal cp
get_cp <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}


# -- Keep only variables we would use for predictions. ----------

# Skipping stuff like casual/registered counts, individual sporting events/calendars.
# Put these in a data frame.
keeps <- c("count", "hour", "dayofweek", "month", "is_daylight", "peak", 'peak_alt', "season",
           "holiday", "workingday", "weather", "temp", "temp_squared", "atemp", "humidity", 
           "windspeed", "house", "senate", "congress_both", "sporting_event", "session_any",
           "scaled_hour", "scaled_dayofweek", "scaled_month", "scaled_season", "scaled_weather", 
           "scaled_temp", "scaled_temp_squared", "scaled_atemp", "scaled_humidity", "scaled_windspeed",
           "house_num", "senate_num", "session_any_num")
biketrain.df <- subset(bikeall.df, train == 1)
biketrain.df <- subset(biketrain.df, select = keeps)
rm(keeps)


# Take out weather = 4. There are only 3 of these observations, which wreaks havoc with modeling.
# Many times, I get "Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
# factor weather has new levels 4"
# biketrain.df$weather <- as.character(biketrain.df$weather)
# biketrain.df$weather[biketrain.df$weather =="4"] <- "3"
# biketrain.df$weather <- as.factor(biketrain.df$weather)


# ++ Partition training | validation ----------

# randomly generate training and validation sets
set.seed(7)  # set seed for reproducing the partition

ss <- sample(1:2, size = nrow(biketrain.df), replace = TRUE, prob = c(0.6, 0.4))
# training.df = biketrain.df[ss==1,]
training.df = biketrain.df
validation.df = biketrain.df[ss==2,]# -- Polynomial regression -------------------------------
# polynomial.temp.lm <- lm(data = training.df, count ~ poly(temp, 2, raw = TRUE))
polynomial.temp.lm <- lm(data = training.df, count ~ poly(temp, 2, raw = TRUE))
summary(polynomial.temp.lm)

pred.train <- predict(polynomial.temp.lm, training.df)

# training: RMSLE 1.43911
pred.train <- negative_to_zero(pred.train)
rmsle(training.df$count, pred.train)

# predict validation data
pred.valid <- predict(polynomial.temp.lm, validation.df)

# validation: RMSLE 1.42811
pred.valid <- negative_to_zero(pred.valid)
rmsle(validation.df$count, pred.valid)

# remove variables
rm(list = c('pred.train', 'pred.valid'))
