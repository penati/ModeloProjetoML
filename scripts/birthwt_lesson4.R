## CARREGANDO PACKAGES ####
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(readr)

## LEITURA E PREPARACAO DOS DADOS ####
### Leitura

bw <- read.csv("data/birthwt.csv",stringsAsFactors=FALSE)

## Vizualização dos dados ####
str(bw)
hist(bw$bwt)

# convert the categorical variables to factor
cols <- c('low', 'race', 'smoke', 'ht', 'ui')
bw[cols] <- lapply(bw[cols], as.factor)

# Split data (training and test groups)
set.seed(123)

train.index <- sample((nrow(bw)),0.7*nrow(bw))

train <- bw[train.index,]
test  <- bw[-train.index,]

## Build a tree ####

## Buildind a tree ----
fit_1 <- rpart(low ~ age + lwt + race + smoke + ptl + ht + ui + ftv,
             data=train,
             method="class")
summary(fit_1)
#ptl 
#100 
parameters.fit_1 <- list(minsplit=fit_1$control$minsplit, 
                         minbucket=fit_1$control$minbucket, 
                         cp=fit_1$control$cp,
                         variable.importance=fit_1$variable.importance)
parameters.fit_1 # minsplit = 20, minbucket = 7, cp=0.01,

fancyRpartPlot(fit_1)

## check with test set
prediction_test1 <- predict(fit_1, test, type = "class")
prop.table(table(prediction_1,test$low))


## Trying with a big tree (2) ----
fit_2 <- rpart(low ~ age + lwt + race + smoke + ptl + ht + ui + ftv,
             data=train, method="class",
             minsplit = 10, minbucket = 3, cp=-1)
summary(fit_2)
parameters.fit_2 <- list(minsplit=fit_2$control$minsplit, 
                         minbucket=fit_2$control$minbucket, 
                         cp=fit_2$control$cp,
                         variable.importance=fit_2$variable.importance)
parameters.fit_2
fancyRpartPlot(fit_2)

## checking accuracy with test set
prediction_2 <- predict(fit_2, test, type = "class")
prop.table(table(prediction_2,test$low))

## Trying with a big tree (3) ----
fit_3 <- rpart(low ~ age + lwt + race + smoke + ptl + ht + ui + ftv,
               data=train, method="class",
               minsplit = 5, minbucket = 3, cp =-1)

parameters.fit_3 <- list(minsplit=fit_3$control$minsplit, 
                         minbucket=fit_3$control$minbucket, 
                         cp=fit_3$control$cp,
                         variable.importance=fit_3$variable.importance)
parameters.fit_3
summary(fit_3)
fancyRpartPlot(fit_3)

## checking accuracy with test set
prediction_3 <- predict(fit_3, test, type = "class")
prop.table(table(prediction_3,test$low))

## Trying with a big tree (4) ----
fit_4 <- rpart(low ~ age + lwt + race + smoke + ptl + ht + ui + ftv,
               data=train, method="class",
               minsplit = 5, minbucket = 2, cp =-1)
summary(fit_4)
fancyRpartPlot(fit_4)

## checking accuracy with test set
prediction_4 <- predict(fit_4, test, type = "class")
prop.table(table(prediction_4,test$low))

## Trying with a big tree (5) ----
fit_5 <- rpart(low ~ age + lwt + race + smoke + ptl + ht + ui + ftv,
               data=train, method="class",
               minsplit = 2, minbucket = 2, cp =-1)
summary(fit_5)
fancyRpartPlot(fit_5)

## checking accuracy with test set
prediction_5 <- predict(fit_5, test, type = "class")
prop.table(table(prediction_5,test$low))
