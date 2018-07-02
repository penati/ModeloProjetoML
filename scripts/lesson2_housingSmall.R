## CARREGANDO PACKAGES ####
library(tidyverse)
library(readxl)
library(ggplot2)

#### LEITURA E PREPARACAO DOS DADOS ####
### Leitura
hr <- read_excel("data/housingSmall.xlsx", 
                           range = "A2:E16")
#View(hr)

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
  labs(title = "Preços de Venda de imóveis", y = "Preço (x R$1000,00)", x = "Área (x 100m2)")+
  theme_gray() +
  theme(text=element_text(size=16), title=element_text(size=16), legend.position="none")
g

### Quanto custa casa em relação aos vizinhos?
#### Inserindo retas verticais
#g + geom_vline(xintercept = 2.0, colour = "green", linetype = "longdash" , size = 2) +
#    geom_vline(xintercept = c(1.93,2.07), colour = "red", linetype = "dotdash", size = 1) +
#    geom_point(data = hr[10:11,],aes(x = area,y=preco),size=3,colour="red")

#### MODELOS PREDITIVO ####

### Regressão Linear -> preço~area ----
## considerando apenas a area
#lm_area <- lm(data = hr, preco~area)

## Vizualizando regressão linear
#precoXarea.plot <- g + geom_abline(intercept = lm_area$coef[1], slope = lm_area$coef[2])
#precoXarea.plot
# Regresão Linear (direto no ggplot)
#g <- g + stat_smooth(method = lm, se = FALSE, formula = y~x, colour = "Black",linetype = "solid")

## Regressão Linar Polinomial quadrática ----
## direto no ggplot
## g + stat_smooth(method = lm, se = FALSE, formula = y~poly(x,2, raw=TRUE), colour="Red")

### Regressão Linear -> preço~areaEgaragem ----
## considerando a area e o numero de garagens
#lm_areaEgaragem <- lm(data=hr, preco~area+garagem)

### Regressão Linear -> preço~areaEquartos ----
## considerando a area e o numero de quartos
#lm_areaEquartos <- lm(data=hr, preco~area+quartos)



### Regressão Linear -> preço~areaEgaragemEquartos ----
lm_linear <- lm(data=hr, preco~area+garagem+quartos)
summary(lm_linear)

### Regressão Quadrádica ----
lm_quadra <- lm(data=hr, preco~poly(area+garagem+quartos,2,raw=TRUE))
summary(lm_quadra)

### Regressão Polinomial ----
lm_poli <- lm(data=hr, preco~poly(area+garagem+quartos,17,raw=TRUE))
summary(lm_poli)

#### VIZUALIZANDO RESULTADOS
#layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
#plot(lm_area)

erros_residuais <- c(lm_linear$residuals,lm_quadra$residuals,lm_poli$residuals)
erros_squared <- sapply(erros_residuais, function(x) x^2)

residuais.df <- data.frame("regressao" = c(rep("linear", times=14), 
                                            rep("quadratica", time=14), 
                                            rep("polinomial",times=14)), 
                        "erros_residuais" = erros_residuais,
                        "erros_squared" = erros_squared,
                        "MSE" = c(rep(mean(sapply(lm_linear$residuals, function(x) x^2)),times=14),
                                  rep(mean(sapply(lm_quadra$residuals, function(x) x^2)),times=14),
                                  rep(mean(sapply(lm_poli$residuals, function(x) x^2)),times=14))
                        )


residuais.plot = ggplot(residuais.df, aes(x=regressao, y=erros_squared)) +
  geom_boxplot(aes(color=regressao)) +
  geom_point(aes(color=regressao), position = position_jitter(width = 0.05)) +
  geom_point(aes(y=MSE, color=regressao), size=4, shape=17) +
  labs(title="Distribuição de erro quadrático", 
       subtitle="Boxplot e média",
       x="Modelo de regressão", y="Erro quadrático residual") +
  theme_classic() +
  theme(text=element_text(size=16), title=element_text(size=16), legend.position="none")
residuais.plot

erros_e_r2 <- data.frame("modelo" = c("linear","quadrático","polinomial"), 
                         "Residual standard_error" = c(summary(lm_linear)$sigma, summary(lm_quadra)$sigma, summary(lm_poli)$sigma),
                         "Adjusted-R-squared" = c(summary(lm_linear)$adj.r.squared, summary(lm_quadra)$adj.r.squared, summary(lm_poli)$adj.r.squared))
erros_e_r2
summary(lm_linear)