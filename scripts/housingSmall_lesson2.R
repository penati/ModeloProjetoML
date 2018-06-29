## CARREGANDO PACKAGES ####
library(tidyverse)
library(readxl)
library(ggplot2)

## LEITURA E PREPARACAO DOS DADOS ####
### Leitura
hr <- read_excel("data/housingSmall.xlsx", 
                           range = "A2:E16")
View(hr)

## Preparando os dados
### Ajustando os nomes das colunas

#### Verificando
colnames(hr)
colnames(hr)[2]

#### Alterando
colnames(hr)[2] = "area"
colnames(hr)[3] = "garagem"
colnames(hr)[4] = "quartos"
colnames(hr)[5] = "preco"

#### Checando
#colnames(hr)

### Visualizando os dados
# Duavariáveis	quantitativas -> usar scatter plot
g <- ggplot(data = hr, aes(x=area, y=preco))
g <- g + geom_point() +
  labs(title = "Preços de Venda de imóveis", y = "Preço (x R$1000,00)", x = "Área (x 100m2)")
g

### Quanto custa casa em relação aos vizinhos?
#### Inserindo retas verticais
g + geom_vline(xintercept = 2.0, colour = "green", linetype = "longdash" , size = 2) +
    geom_vline(xintercept = c(1.93,2.07), colour = "red", linetype = "dotdash", size = 1) +
    geom_point(data = hr[10:11,],aes(x = area,y=preco),size=3,colour="red")

## MODELOS PREDITIVO ####

### Regressão Linear -> preço~area ----
#### considerando apenas a area
lm_area <- lm(data = hr, preco~area)

### Vizualizando regressão linear
precoXarea.plot <- g + geom_abline(intercept = lm_area$coef[1], slope = lm_area$coef[2])
precoXarea.plot
# Regresão Linear (direto no ggplot)
#g <- g + stat_smooth(method = lm, se = FALSE, formula = y~x, colour = "Black",linetype = "solid")

### Regressão Linar Polinomial quadrática ----
#### direto no ggplot
#### g + stat_smooth(method = lm, se = FALSE, formula = y~poly(x,2, raw=TRUE), colour="Red")

### Regressão Linear -> preço~areaEgaragem ----
#### considerando a area e o numero de garagens
lm_areaEgaragem <- lm(data=hr, preco~area+garagem)

### Regressão Linear -> preço~areaEquartos ----
#### considerando a area e o numero de quartos
lm_areaEquartos <- lm(data=hr, preco~area+quartos)

### Regressão Linear -> preço~areaEgaragemEquartos ----
#### considerando a area, o numero de garens e quartos
lm_areaEgaragemEquartos <- lm(data=hr, preco~area+garagem+quartos)

#### VIZUALIZANDO RESULTADOS
#layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
#plot(lm_area)

erro_std1 <- summary(lm_area)$sigma
r_square1 <- summary(lm_area)$adj.r.squared

erro_std2 <- summary(lm_areaEgaragem)$sigma
r_square2 <- summary(lm_areaEgaragem)$adj.r.squared

erro_std3 <- summary(lm_areaEquartos)$sigma
r_square3 <- summary(lm_areaEquartos)$adj.r.squared

erro_std4 <- summary(lm_areaEgaragemEquartos)$sigma
r_square4 <- summary(lm_areaEgaragemEquartos)$adj.r.squared

#menor erro e maior r2 é do lm_areaEquartos