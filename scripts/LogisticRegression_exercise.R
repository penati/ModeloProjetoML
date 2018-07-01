## CARREGANDO PACKAGES ####
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(readr)
library(tidyverse)
library(Amelia) #visual way to check for missing data

#Generalized Linear Models
#glm(formula , data = ___, family = ___)
  #formula	outcome ~ predictor1 + predictor2 + predictor3
  #data=	specifies the data frame
  #family=	binomial(link = "logit")

## LEITURA E PREPARACAO DOS DADOS ####
### Leitura
titanic <- read.csv("data/titanic.csv",stringsAsFactors=FALSE, na.strings = c(""))

#LEITURA DIRETO DA URL
#library(RCurl)
#titanic <- read.csv(
#  text=getURL("https://raw.githubusercontent.com/Efsilvaa/EPSMLRepo/master/Data/titanic.csv"),
#  stringsAsFactors=FALSE,
#  na.strings = c(""))


## Vizualização dos dados ####

## Cleaning NA´s
summary(titanic)

titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

contrasts(titanic$Sex) #view how R encoded the factors
contrasts(titanic$Embarked)

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

## Split data (training and test groups) ----
set.seed(123)
train.index <- sample((nrow(titanic)),0.7*nrow(titanic))
train <- titanic[train.index,]
test  <- titanic[-train.index,]

## Buildind a model ####
fit <- glm(Survived ~ Pclass + Age + Sex + SibSp + Parch + Fare + Embarked,
           data = train,
           family = binomial(link = "logit"))
summary(fit)

# Using Analysis Of Variance (ANOVA) to analyze the table of deviance
anova(fit, test="Chisq")

## Function predict() in Logistic Regression ----
  #predict(model , newdata = ___, type = ___)**
  #  model	The variable that contains the trained model
  #newdata=	specifies the newdata to be tested
  #type=	"response": predicted probabilities
  #      "terms": fitted values

#check with test set
#newdata needs the same preprocessing as train 
prediction_prob <- predict(fit,newdata=test,type='response')
prediction <- ifelse(prediction_prob > 0.5,1,0)
prop.table(table(prediction,test$Survived))

## Trying with small model ----
fit2 <- glm(Survived ~ Pclass + Sex + Age + SibSp,
           data=train, 
           family=binomial(link="logit"))

prediction_prob2 <- predict(fit2, test, type='response')
prediction2 <- ifelse(prediction_prob2 > 0.5, 1, 0)
prop.table(table(prediction2,test$Survived))
