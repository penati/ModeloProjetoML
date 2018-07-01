## CARREGANDO PACKAGES ####
library(ggplot2)
library(caret)

## LEITURA E PREPARACAO DOS DADOS ####
### Leitura
load("data/regressaoKNNcv_pp.Rdata")

### Vizualização
head(pp)
str(pp)
summary(pp)


## Dividindo os dados em Treinamento e Test ----
set.seed(300)
indiceTrain <- createDataPartition(y = pp$valor,p = 0.75,list = FALSE)
training <- pp[indiceTrain,]
testing <- pp[-indiceTrain,]

## Checando a distribuição dos dados de treinamento e teste ----
## A função createDataPartition() procura sempre manter a distribuição da amostra
pp$grupo = "Treinamento"
pp$grupo[-indiceTrain]="Teste"  

m <- ggplot(data = pp, aes(x=valor,fill=grupo)) 
m + geom_density(alpha=0.4) 

## Preprocessamento dos dados ----
## kNN é sensivel a distribuição dos dados por variável.
## Será necessário normallizá-los. O pacote caret permite incluir o preprocessamento. 
##    No caso, será usado center e scale

#A coluna classe não será considerada por ser uma categoria (fator)
trainX <- training[,names(training) != "valor"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues


## Treinamento ----
## Cada modelo possui um conjunto de parâmetros que podem ser alterados para o ajuste. 
## Para saber quais são os parâmetros de cada modelo, u
## ma opção é ir na página do caret https://topepo.github.io/caret/available-models.html 
## e realizar a busca pelo modelo

set.seed(400)
ctrl <- trainControl(method = "repeatedcv",repeats = 3) 

gridK <- expand.grid( k = c(3:5) )

knnFit <- train(valor ~ PredictorA + PredictorB,
                data = training, method = "knn", 
                trControl = ctrl, 
                preProcess = c("center","scale"),
                #Definindo a faixa de parametros a ser usada
                tuneGrid = gridK )

knnFit


## Variando os parâmetros automaticamente usando tuneLenght ----
## Existe a opção de se usar tuneLenght para definir o tamanho da faixa dos parâmtros

knnFit <- train(valor ~ PredictorA + PredictorB, 
                data = training, method = "knn", 
                trControl = ctrl, 
                preProcess = c("center","scale"), 
                tuneLength = 20)

## Output de kNN fit ----
knnFit

## Plotando knnFit ----
plot(knnFit)
