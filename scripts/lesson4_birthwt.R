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
prediction_1 <- predict(fit_1, test, type = "class")
table1 <- prop.table(table(prediction_1,test$low))
accuracy1 <- table1[1,1]+table1[2,2]
accuracy1

## Trying with a big tree (2) ----
fit_2 <- rpart(low ~ age + lwt + race + smoke + ptl + ht + ui + ftv,
             data=train, method="class",
             minsplit = 10, minbucket = 3, cp=-1)
summary(fit_2)
fancyRpartPlot(fit_2)

## checking accuracy with test set
prediction_2 <- predict(fit_2, test, type = "class")
table2 <- prop.table(table(prediction_2,test$low))
table2

accuracy2 <- table2[1,1]+table2[2,2]
accuracy2

## Trying with a big tree (3) ----
fit_3 <- rpart(low ~ age + lwt + race + smoke + ptl + ht + ui + ftv,
               data=train, method="class",
               minsplit = 5, minbucket = 3, cp =-1)

summary(fit_3)
fancyRpartPlot(fit_3)

## checking accuracy with test set
prediction_3 <- predict(fit_3, test, type = "class")
table3 <- prop.table(table(prediction_3,test$low))
table3

accuracy3 <- table3[1,1]+table3[2,2]
accuracy3

## Trying with a big tree (4) ----
fit_4 <- rpart(low ~ age + lwt + race + smoke + ptl + ht + ui + ftv,
               data=train, method="class",
               minsplit = 5, minbucket = 2, cp =-1)
summary(fit_4)
fancyRpartPlot(fit_4)

## checking accuracy with test set
prediction_4 <- predict(fit_4, test, type = "class")
table4 <- prop.table(table(prediction_4,test$low))
table4

accuracy4 <- table4[1,1]+table4[2,2]
accuracy4

## Trying with a big tree (5) ----
fit_5 <- rpart(low ~ age + lwt + race + smoke + ptl + ht + ui + ftv,
               data=train, method="class",
               minsplit = 2, minbucket = 2, cp =-1)
summary(fit_5)
fancyRpartPlot(fit_5)

## checking accuracy with test set
prediction_5 <- predict(fit_5, test, type = "class")
table5 <- prop.table(table(prediction_5,test$low))
table5

accuracy5 <- table5[1,1]+table5[2,2]
accuracy5
