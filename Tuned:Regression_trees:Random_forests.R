##### Load packages

library(tidyverse)
library(Amelia)
library(caTools)
library(corrplot)
library(caret)
library(forecast)

##### Load dataframe
train <- read.csv('../input/bike-sharing-demand/train.csv')
test <- read.csv('../input/bike-sharing-demand/test.csv')

##### Inspect data frames

head(train)
str(train)
summary(train)

head(test)
str(test)
summary(test)

##### Changing season, workingday, weather colums to factor

train$season <- as.factor(train$season)
train$workingday <- as.factor(train$workingday)
train$weather <- as.factor(train$weather)

test$season <- as.factor(test$season)
test$workingday <- as.factor(test$workingday)
test$weather <- as.factor(test$weather)

num.col <- sapply(train, is.numeric)
cor <- cor(train[,num.col])
cor <- cor[-c(2,5,6), -c(2,5,6)] ## removing atemp, casual and registered
cor
corrplot(cor, addCoef.col =1, tl.cex = 1, cl.cex = 1)

train$hour <- format(as.POSIXct(train$datetime),format = '%H')
test$hour <- format(as.POSIXct(test$datetime),format = '%H')

train$date <- as.POSIXlt(train$datetime, format='%Y-%m-%d')
test$date <- as.POSIXlt(test$datetime, format='%Y-%m-%d')

theme_set(theme_bw())

ggplot(train, aes(count, temp))+
  geom_point(aes(color = season), alpha = 0.5)

cor(train[,c('count', 'temp')])

ggplot(train, aes(count, humidity))+
  geom_point(aes(col = humidity), alpha = 0.5)+
  scale_y_continuous(n.breaks =10)+
  scale_color_gradientn(colours = c('darkblue', 'lightblue','yellow', 'darkorange', 'red'))

cor(train[,c('count', 'humidity')])

###### bikeshare by date and season
ggplot(train, aes(as.POSIXct(date), count))+
  geom_point(aes(color = season), alpha = 0.4)

ggplot(train, aes(hour, count))+
  geom_point(position = position_jitter(w=1,h=0), aes(color = temp))+
  scale_color_gradientn(colours = c('darkblue', 'lightblue','yellow', 'darkorange', 'darkred'))

###### Creating a model
train$hour <- as.numeric(train$hour)
test$hour <- as.numeric(test$hour)


poly.count <- lm(count ~ poly(hour, 6, raw=T)+
                   poly(windspeed,2, raw=T)+
                   poly(temp,2, raw=T)+
                   poly(humidity,4, raw=T), train)

summary(poly.count)

plot(poly.count)

### Creating histogram for residual distribution

ggplot(poly.count,aes(poly.count$residuals))+
  geom_histogram()

p.count <- predict(poly.count, test)

final.count <- cbind(test, p.count)
head(final.count)

final.count <-as.data.frame(final.count)
final.count$p.count <- ifelse(final.count$p.count <=0,0,final.count$p.count)

head(final.count,10)


ggplot(train, aes(hour, count))+
  geom_point(position = position_jitter(w=1,h=0), aes(color = temp))+
  scale_color_gradientn(colours = c('darkblue', 'lightblue','yellow', 'darkorange', 'darkred'))+
  stat_smooth(formula = y~poly(x,6, raw = TRUE), lty = 'dotted')

set.seed(101)

sample.t <- sample.split(train$count, 0.70)
pm.train <- filter(train, sample.t == TRUE)
pm.test <- filter(train, sample.t == FALSE)

test.model <- lm(count ~ poly(hour, 6, raw=T)+
                   poly(windspeed,2, raw=T)+
                   poly(temp,2, raw=T)+
                   poly(humidity,4, raw=T), pm.train)


pred.train <- predict(test.model, pm.test)

head(cbind(pm.test$count, pred.train),15)

accuracy(pred.train, pm.test$count)

library(rpart)
library(rpart.plot)


### Pre-process data
tree.train <- train %>% 
  select(-c('datetime', 'date', 'registered', 'casual', 'atemp'))


### Create Split
tree.sample <- sample.split(tree.train$count, SplitRatio = 0.70)

train.rt <- tree.train %>%
  filter(tree.sample == TRUE)

test.rt <- tree.train %>%
  filter(tree.sample == FALSE)

### Create regression tree base model
tree.model <- rpart(count ~., method ='anova', data = train.rt)


### Preict base model
tree.pred <-predict(tree.model, test.rt, method ='anova')

### bind base prediction to 
tree.result <- cbind(test.rt$count, tree.pred)
colnames(tree.result) <- c('count', 'pred')

tree.result <- as.data.frame(tree.result)

### Result for base model
head(tree.result)

### Accuracy test
accuracy(tree.pred, tree.result$count)

### Plot base model tree
prp(tree.model,fallen.leaves = FALSE, branch = 0, compress = FALSE, space = 5)

#### Creating a full tree to prune it back

full.tree <- rpart(count ~., method ='anova', data = train.rt,control = rpart.control(cp =.0002, min.split =5, minbucket = 5, maxdepth = 5, xval = 10))

prp(full.tree,fallen.leaves = FALSE, branch = 0, compress = FALSE, space = 5)

printcp(full.tree)

'min.xerror + minxstd'
0.40466+0.010618  ## Lowest xerror and xstd

prune.tree <-prune(full.tree, cp=0.00204043)
prp(prune.tree,fallen.leaves = FALSE, branch = 0, compress = FALSE, space = 5)

prune.pred <- predict(prune.tree,test.rt)

prune.result <- cbind(test.rt$count, prune.pred)

colnames(prune.result) <- c('count', 'pred')

head(prune.result,6)

prune.result <- as.data.frame(prune.result)

"Prune RMSE"
accuracy(prune.pred,prune.result$count)

library(randomForest)

### Pre process train

rf <- train %>% 
  select(-c('date', 'registered', 'casual', 'atemp'))

### Split Train data to train and test
set.seed(101)
sample.rf <- sample.split(rf$count, SplitRatio = 0.70)
train.rf <- filter(rf, sample.rf == TRUE)
test.rf <- filter(rf, sample.rf == FALSE)

rf.model <- randomForest(count~., train.rf, importance = TRUE, ntree = 1000)

### print model
rf.model

### Show importance of the model
importance(rf.model)

### Predict rf
pred.rf <- predict(rf.model, test.rf)

### accuracy of the model
accuracy(pred.rf,test.rf$count)

rf.result <- cbind(test.rf$count, pred.rf)
colnames(rf.result) <-c('act', 'pred')

rf.result <- as.data.frame(rf.result)


### Comparison vs rf.test count data
head(rf.result, 10)

sub.pred <- round(predict(rf.model, test), digits = 0)

sub <- cbind(test$datetime, sub.pred)
colnames(sub) <- c('datetime', 'count')

sub <- as.data.frame(sub)

write.csv(file = 'submission.csv', x = sub, row.names = F)