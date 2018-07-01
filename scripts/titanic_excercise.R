###birth weight

library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(readr)

#rpart(formula, data=, method=) |
#   formula	outcome ~ predictor1 + predictor2 + predictor3
#   data=	specifies the data frame
#   method=	"class" for a classification tree
#           "anova" for a regression tree
#minsplit is the minimum number of observations that must exist in a node in order for a split to be attempted
#minbucket is the minimum number of observations in any terminal node.
#mytree <- rpart(Fraud ~ RearEnd, data = train, method = "class", minsplit = 2, minbucket = 1)
#cp=complexity parameter | If the next best split in growing a tree does not reduce the tree’s overall complexity by a certain amount, rpart will terminate the growing process
#Setting cp to a negative amount ensures that the tree will be fully grown.
#mytree <- rpart(Fraud ~ RearEnd, data = train, method = "class", minsplit = 2, minbucket = 1, cp= -1)
#fancyRpartPlot(mytree)
#weights argument | p.e. you may want more weight on dectection of TRUE frauds
#mytree <- rpart(Fraud ~ RearEnd, data = train, method = "class",
#                minsplit = 2, minbucket = 1,
#                weights = c(.4, .2, .2))


## LEITURA E PREPARACAO DOS DADOS ####
### Leitura

titanic <- read.csv("data/titanic.csv",stringsAsFactors=FALSE)

## Vizualização dos dados ####
##Look at #people who survived
table(titanic$Survived)

prop.table(table(titanic$Survived))

##Let's explore Survived x Sex
prop.table(table(titanic$Sex,titanic$Survived))

prop.table(table(titanic$Sex,titanic$Survived),1)

## Split data (training and test groups) ####

set.seed(123)
train.index <- sample((nrow(titanic)),0.7*nrow(titanic))
train <- titanic[train.index,]
test  <- titanic[-train.index,]

## Buildind a tree ----
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")
summary(fit)

## check with test set ----
prediction <- predict(fit, test, type = "class")
table(prediction,test$Survived)
prop.table(table(prediction,test$Survived))

## Trying with a big tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train, method="class",
             minsplit = 2, cp = 0)
fancyRpartPlot(fit)

## checking accuracy with training set
prediction <- predict(fit, train, type = "class")
table(prediction,train$Survived)

prop.table(table(prediction,train$Survived)) # We reach an accuracy of 0.979

## check with test set
prediction <- predict(fit, test, type = "class")
table(prediction,test$Survived)
prop.table(table(prediction,test$Survived)) # We reach an accuracy of 0.769
