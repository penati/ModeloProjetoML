## CARREGANDO PACKAGES ####
library(tidyverse)
library(readxl)
library(ggplot2)
library(caret)

## LEITURA E PREPARACAO DOS DADOS ####
### Leitura
hr <- read_excel("data/housingSmall.xlsx", 
                 range = "A2:E16")

### Ajustando os nomes das colunas
colnames(hr)[2] = "area"
colnames(hr)[3] = "garagem"
colnames(hr)[4] = "quartos"
colnames(hr)[5] = "preco"

## Usando LOOCV ####
### Defina training control
train_control <- trainControl(method = "LOOCV")

# modelo baseado no treinamento usando train_control
model <- train(preco~area, data=hr, trControl=train_control, method="lm")

# summarize results
print(model)

mse <- model$results$RMSE^2
mse

## Usando CARET ####
# define training control folders = 10
train_control <- trainControl(method="cv", number=10)
# train the model
model_CV <- train(preco~area, data=hr, trControl=train_control, method="lm")
# summarize results
# considerando agora somente RMSE
print(model_CV)

## Usando CARET com repeatedCV ####
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model_CVR <- train(preco~area, data=hr, trControl=train_control, method="lm")
# summarize result
print(model_CVR)


#### Comparando Modelos #####
## Montando manualmente ----
# define "training control"
set.seed(200)
train_control <- trainControl(method="repeatedcv", number=10, repeats=3) # treinamento do modelo 
modelos <- NULL
for (i in (1:8)){
  m_formula <- paste("preco ~ poly(area,",i,")",sep="")
  model <- train(as.formula(m_formula), data = hr, trControl=train_control,
                 method="lm")
  modelos <- rbind(modelos,data.frame(i, model$results))
}
modelos
class(modelos) #Veremos que se trata de um “data frame” 
str(modelos) #Qual a estrutura (colunas)

## Montando o gráfico manualmente
g <- ggplot(data = modelos,aes(x = i))+ 
  geom_line(aes(y=RMSE,colour = "RMSE"))+
  geom_line(aes(y=RMSESD, colour = "RMSESD"))+
  labs(title = "Comparando Modelos", x = "Modelos de Regressão Preço ~ Poly(area, x)")

#Melhorando a escala 
g + scale_x_continuous(breaks = seq(1:8))
g + scale_y_continuous(limits = c(0,120))
g + scale_y_continuous(limits = c(0,75))

## usando caret (cv e resamples) ----
# define o treinamento
train_control <- trainControl(method="repeatedcv", number=10, repeats=3) 
N <- 6 #Número de modelos 
modelos_list <- list() #Armazena a lista de modelos
for (i in (1:N)){
  #artifício, pois não funcionou com a fórmula em train()
  m_formula <- paste("preco ~ poly(area,",i,")",sep="") 
  model <- train(as.formula(m_formula), data = hr, 
                 trControl=train_control, method="lm")
  modelos_list[[paste("lmPoly_",i,sep = "")]] <- model
}

resamps <- resamples(modelos_list) 
resamps
summary(resamps)

# BOXPLOT comparando os resultados
bwplot(resamps , metric="RMSE")
