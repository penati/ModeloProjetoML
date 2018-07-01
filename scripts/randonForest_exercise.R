## CARREGANDO PACKAGES ####
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(readr)
library(tidyverse)
library(Amelia) #visual way to check for missing data

# randomForest(formula, data=, method=, ntree=, importance =  )
  #formula	outcome ~ predictor1 + predictor2 + predictor3
  #data	specifies the data frame
  #method	"class" for classification/ "anova" regression
  #impotance	TRUE or FALSE. Allow inspect variable imporatance
  #ntree	Number of trees

## LEITURA E PREPARACAO DOS DADOS ####
### Leitura

titanic <- read.csv("data/titanic.csv",stringsAsFactors=FALSE)

## Vizualização dos dados ####

## Cleaning NA´s
summary(titanic)

  #A visual way to check for missing data
missmap(titanic, main = "Missing values vs observed")

  #use Regression tree to fix age
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                data=titanic[!is.na(titanic$Age),], 
                method="anova")

titanic$Age[is.na(titanic$Age)] <-
  predict(Agefit, titanic[is.na(titanic$Age),])

  #Is there any other NA´s?
missmap(titanic, main = "Missing values vs observed")

## look at Embarked
summary(titanic$Embarked)
titanic$Embarked <- as.factor(titanic$Embarked)
summary(titanic$Embarked) #duas em branco

  #a large majority boarded in Southampton, let’s just replace those two with “S”
which(titanic$Embarked == "")
titanic$Embarked[c(62,830)] = "S"
#Make sure "S" is entered as a factor
titanic$Embarked <- factor(titanic$Embarked)

str(titanic)
#Sex is not a factor
titanic$Sex <- as.factor(titanic$Sex)
summary(titanic$Sex)

## Split data (training and test groups) ----
set.seed(123)
train.index <- sample((nrow(titanic)),0.7*nrow(titanic))
train <- titanic[train.index,]
test  <- titanic[-train.index,]

# factor in “y” avoid used of method =“class”
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                    data=train,
                    importance=TRUE, #for plot variable importance
                    ntree=2000)

varImpPlot(fit)
summary(fit)

prediction.train <- predict(fit, train, type = "class")
prop.table(table(prediction.train,train$Survived))


prediction.test <- predict(fit, test, type = "class")
prop.table(table(prediction.test,test$Survived))


