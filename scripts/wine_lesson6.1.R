## CARREGANDO PACKAGES ####
library(ggplot2)
library(caret)

## LEITURA E PREPARACAO DOS DADOS ####
### Leitura
load("data/wine_data.Rdata")

### Vizualização
head(wine_df)
str(wine_df)
summary(wine_df)

wine_df$classe <- as.factor(wine_df$V14)


## Dividindo os dados em Treinamento e Test ----
set.seed(300)
indiceTrain <- createDataPartition(y = wine_df$classe,p = 0.75,list = FALSE)
training <- wine_df[indiceTrain,]
testing <- wine_df[-indiceTrain,]

## Checando a distribuição dos dados de treinamento e teste ----
## A função createDataPartition() procura sempre manter a distribuição da amostra
wine_df$grupo = "Treinamento"
wine_df$grupo[-indiceTrain]="Teste"  

m <- ggplot(data = wine_df, aes(x=V14,fill=grupo)) 
m + geom_density(alpha=0.4) 

## Preprocessamento dos dados ----
## kNN é sensivel a distribuição dos dados por variável.
## Será necessário normallizá-los. O pacote caret permite incluir o preprocessamento. 
##    No caso, será usado center e scale

#A coluna classe não será considerada por ser uma categoria (fator)
trainX <- training[,names(training) != "V14"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues


## Treinamento ----
set.seed(400)
ctrl <- trainControl(method = "repeatedcv",repeats = 3) 

knnFit <- train(V14 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13,
                data = training, method = "knn", 
                trControl = ctrl, 
                preProcess = c("center","scale"), 
                tuneLength = 20)

## Output de kNN fit ----
knnFit

## Plotando knnFit ----
plot(knnFit)
