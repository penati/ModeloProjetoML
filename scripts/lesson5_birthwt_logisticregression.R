## CARREGANDO PACKAGES ####
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(readr)
library(readr)
library(tidyverse)
library(Amelia) #visual way to check for missing data

## LEITURA E PREPARACAO DOS DADOS ####
## Leitura

bw <- read.csv("data/birthwt.csv",stringsAsFactors=FALSE)

## Vizualização dos dados
str(bw)
summary(bw)
hist(bw$bwt)

# convert the categorical variables to factor
cols <- c('low', 'race', 'smoke', 'ht', 'ui')
bw[cols] <- lapply(bw[cols], as.factor)

# A visual way to check for missing data
missmap(bw, main = "Missing values vs observed")

# Split data (training and test groups)
set.seed(123)
train.index <- sample((nrow(bw)),0.7*nrow(bw))
train <- bw[train.index,]
test  <- bw[-train.index,]

## Buildind a model ####
fit <- glm(low ~ age + lwt + race + smoke + ptl + ht + ui + ftv,
           data = train,
           family = binomial(link = "logit"))
summary(fit)

# Using Analysis Of Variance (ANOVA) to analyze the table of deviance
anova(fit, test="Chisq")

# Check with test set ----
#newdata needs the same preprocessing as train 
prediction_prob1 <- predict(fit,newdata=test,type='response')
prediction1 <- ifelse(prediction_prob1 > 0.5,1,0)

table1 <- prop.table(table(prediction1,test$low))
table1

accuracy1 <- table1[1,1]+table1[2,2]
accuracy1

## Trying with small model ----
fit2 <- glm(low ~ lwt + smoke + ptl + ht + ui,
            data=train, 
            family=binomial(link="logit"))

prediction_prob2 <- predict(fit2,newdata=test,type='response')
prediction2 <- ifelse(prediction_prob2 > 0.5,1,0)

table2 <- prop.table(table(prediction2,test$low))
table2

accuracy2 <- table2[1,1]+table2[2,2]
accuracy2