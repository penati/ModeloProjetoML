## CARREGANDO PACKAGES ####
library(readxl)
library(tidyverse)

## LEITURA E PREPARACAO DOS DADOS ####
### Leitura

dados <- read_excel("data/BlueSky_Single-Leg_demand.xls",
                    range = "A6:B371", col_types = c("date","numeric"))

### Ajustando nomes das colunas
colnames(dados)[1] <- "date"
colnames(dados)[2] <- "demand"

## VIZUALIZANDO OS DADOS ####
head(dados)
#Plots
g <- ggplot(dados, aes(x=date,y=demand)) +
  geom_point(color="grey") + 
  labs(title="Evolução da demanda ao longo do ano", x="Data", y="Demanda de passageiros") +
  theme_classic() +
  theme(text=element_text(size=16), title=element_text(size=16))

first.plot <- g + geom_line(color="grey")

## ANALISES E MODELOS ####
# Definicao do Valor Crítico
tarifa.full <- 174
tarifa.low <- 114

valor.critico <- (tarifa.full-tarifa.low)/tarifa.full

### Modelo 1 - Normal ----
## Considerando que a demanda segue uma distribuição normal
#sem discrimar o dia da semana
media_1 <- mean(dados$demand)
media_1_int <-round(media_1,0) 
# sd e RMSE (considerando o valor fracionário)
rmse_1 <- sd(dados$demand)
prot_level_1 <- qnorm(p=valor.critico, mean = media_1, sd = rmse_1 )
prot_level_1 <- round(prot_level_1,0)

prot_level_1.plot <- g + 
  geom_hline(yintercept =  media_1, color="blue", linetype = "dashed", size = 1) +
  geom_hline(yintercept =  prot_level_1, color="red", linetype = "dotdash", size = 1) + 
  labs(title="Comparação da demanda média (blue) e o nível de proteção (red)", 
       subtitle="Considerando uma demanda normalizada", x="Data", y="Demanda de passageiros") +
  theme_classic() +
  theme(text=element_text(size=16), title=element_text(size=16))

prot_level_1.plot

### Modelo 2 - Empírico ----
## Considerando que a demanda segue uma distribuição empírica
#sem discrimar o dia da semana
q.model_2 <- quantile(dados$demand, c(valor.critico,0.5)) 
prot_level_2 <-  q.model_2[[1]]
mediana_2 <-  q.model_2[[2]]
# RMSE
rmse_2 <- sqrt(mean((dados$demand - mediana_2)^2))

prot_level_2.plot <- g + 
  geom_hline(yintercept =  mediana_2, color="blue", linetype = "dashed", size = 1) +
  geom_hline(yintercept =  prot_level_2, color="red", linetype = "dotdash", size = 1) + 
  labs(title="Comparação da demanda mediana (blue) e o nível de proteção (red)", 
       subtitle="Considerando uma demanda empírica", x="Data", y="Demanda de passageiros") +
  theme_classic() +
  theme(text=element_text(size=16), title=element_text(size=16))
prot_level_2.plot

### Modelo 3 - Normal semanal ----
## Considerando que a demanda segue uma distribuição normal
## Considerando os dias da semana
dados$day <- weekdays(dados$date)

mean.days.normal <- dados %>%
  group_by(day) %>%
  mutate(media.N = mean(demand),
            sd.N = sd(demand),
            prot_level_3 = round(qnorm(p=valor.critico ,mean = media.N, sd = sd.N),0))

# RMSE
rmse_3 <- sqrt(mean((mean.days.normal$demand - mean.days.normal$media.N)^2))

prot_level_3.plot <- ggplot(mean.days.normal, aes(x=date,y=demand)) +
  geom_point() + 
  geom_line(aes(x=date, y=media.N), color="blue") +
  geom_line(aes(x=date, y=prot_level_3), color="red") + 
  labs(title="Comparação da demanda média (blue) e o nível de proteção (red)", 
       subtitle="Considerando os dias da semana euma demanda normalizada", x="Data", y="Demanda de passageiros") +
  theme_classic() +
  theme(text=element_text(size=16), title=element_text(size=16))
prot_level_3.plot

### Modelo 4 - Empírica semanal ----
## Considerando que a demanda segue uma distribuição empírica
## Considerando os dias da semana
median.days.empirica <- dados %>%
  group_by(day) %>%
  mutate(mediana = median(demand),
         prot_level_4 = as.integer(quantile(demand,valor.critico))
  )

rmse_4 <- sqrt(mean((median.days.empirica$demand - median.days.empirica$mediana)^2))

prot_level_4.plot <- ggplot(median.days.empirica, aes(x=date,y=demand)) +
  geom_point() + 
  geom_line(aes(x=date, y=mediana), color="blue") +
  geom_line(aes(x=date, y=prot_level_4), color="red") + 
  labs(title="Comparação da demanda média (blue) e o nível de proteção (red)", 
       subtitle="Considerando os dias da semana e uma demanda empírica", x="Data", y="Demanda de passageiros") +
  theme_classic() +
  theme(text=element_text(size=16), title=element_text(size=16))
prot_level_4.plot

### Modelo 5 - Regressão ----
dados$month <- months(dados$date)
dados$day <- weekdays(dados$date)

# Para regressão: FATORES
dados$day <- factor(dados$day)
dados$month <- factor(dados$month)

#summary(dados$day)
#is.factor(dados$day)
#levels(dados$day)

#### Buscando o modelo com menor erro ----
#---- Regressão - considerando o dia da semana
dados.fit.1 <- lm(data=dados, demand ~  day)
rmse_5.1 <- summary(dados.fit.1)$sigma

#---- Regressão - considerando o mes
dados.fit.2 <- lm(data=dados, demand ~  month)
rmse_5.2 <- summary(dados.fit.2)$sigma

#---- Regressão - considerando dia da semana e o mes
dados.fit.3 <- lm(data=dados, demand ~  day + month)
rmse_5.3 <- summary(dados.fit.3)$sigma

#---- Regressão - considerando a interação do dia da semana e mês
dados.fit.4 <- lm(data=dados, demand ~ day*month)
rmse_5.4 <- summary(dados.fit.4)$sigma

erros <- data.frame("modelo" = c("1", "2", "3", "4", "5.1", "5.2", "5.3", "5.4"),
                    "rmse"= c(rmse_1, rmse_2, rmse_3, rmse_4, rmse_5.1, rmse_5.2, rmse_5.3, rmse_5.4))

#### plotando os valores previstos ----
prediction <- predict(dados.fit.3, newdata = dados)

prot_level_5.plot <- g +
  geom_point(aes(x=dados$date,y=dados.fit.3$fitted.values), color = "blue") + 
  geom_line(aes(x=dados$date,y=prediction), linetype="dotdash", color = "blue") +
  labs(title="Comparação entre os dados (black) e previsão do modelo (blue)", 
       subtitle="Regressão linear considerando dia da semana e mês", x="Data", y="Demanda de passageiros") +
  theme_classic() +
  theme(text=element_text(size=16), title=element_text(size=16))
prot_level_5.plot

#### Buscando pontos com grandes erros ----
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(dados.fit.3)

dados$date[263] #2006-11-22 Thanksgiving Day
dados$date[272] #2006-12-01
dados$date[300] #2006-12-29 Newyear

#### Modificando o dataframe para considerar datas exepcionais ----
dados <- dados %>%
  mutate (holydays=0)

dados[263, "holydays"] <- "1"
dados[272, "holydays"] <- "-1" #eh um dia que apresenta queda e não aumento como em feriado
dados[300, "holydays"] <- "1"

dados$holydays <- factor(dados$holydays)
levels(dados$holydays)

#### Modificando o modelo considerando os feriados ----

#---- Regressão - considerando a interação do dia da semana, mês e feriado
dados.fit.6 <- lm(data=dados, demand ~ day + month + holydays)
rmse_6 <- summary(dados.fit.6)$sigma #20.69231  #19.21393 #19.41123

add_row(erros, "modelo" = "6", "rmse"=rmse_6)

plot(dados.fit.6) #menos overfit

# Melhorarndo mais um pouco o modelo ----

dados$date[42] #2006-04-15
dados$date[302] #2006-12-31
dados$date[306] #2007-01-04

dados[42, "holydays"] <- "1"
dados[302, "holydays"] <- "1"
dados[306, "holydays"] <- "1"

dados.fit.7 <- lm(data=dados, demand ~ day + month + holydays)
rmse_7 <- summary(dados.fit.7)$sigma
add_row(erros, "modelo" = "7", "rmse"=rmse_7)

plot(dados.fit.7) # daqui pra frente pouco seria melhorado


#plotando os valores previstos segundo o novo modelo----

prediction2 <- predict(dados.fit.7, newdata = dados)

prot_level_7.plot <- g +
  geom_point(aes(x=dados$date,y=dados.fit.7$fitted.values), color = "blue") + 
  geom_line(aes(x=dados$date,y=prediction2), linetype="dotdash", color = "blue") +
  geom_point(aes(x=dados$date[263], y=dados$demand[263],color="2006-11-22"))+
  geom_point(aes(x=dados$date[272], y=dados$demand[272],color="2006-12-01"))+
  geom_point(aes(x=dados$date[300], y=dados$demand[300],color="2006-12-29"))+
  geom_point(aes(x=dados$date[42], y=dados$demand[42],color="2006-04-15"))+
  geom_point(aes(x=dados$date[302], y=dados$demand[302],color="2006-12-31"))+
  geom_point(aes(x=dados$date[306], y=dados$demand[306],color="2007-01-04"))+
  labs(title="Comparação entre os dados (grey) e previsão do modelo (blue)", 
       subtitle="Regressão linear considerando também feriados", x="Data", y="Demanda de passageiros") +
  theme_classic() +
  theme(text=element_text(size=16), title=element_text(size=16))
prot_level_7.plot

## Definindo pares de proteção e limites ####

# semana alvo da decisao
week.df <- dados %>%
  filter(date > "2006-03-18 00:00:00" & date < "2006-03-25 00:00:00")

prot_lv_e_limits.df <- week.df %>%
  mutate( prot_lv = predict(dados.fit.7, newdata = week.df),
          reserve_limit = 140-predict(dados.fit.7, newdata = week.df))
prot_lv_e_limits.df

### Plotando proteção e limites
prot_lv_e_limits.plot <- 
  ggplot(prot_lv_e_limits.df, aes(x=date)) +
  geom_point(aes(y=demand)) + 
  geom_area(aes(y=demand), alpha=0.1) +
  geom_point(aes(y=prot_lv), color = "blue") + 
  geom_line(aes(y=prot_lv), color = "blue", linetype = "solid") +
  geom_point(aes(y=reserve_limit), color = "red") +
  geom_line(aes(y=reserve_limit), color = "red", linetype = "solid") +
  labs(title="Nível de proteção (blue) e limites de reservas (red)", 
       subtitle="Determinado a partir dos dados do ano anterior (black)", x="Data", y="Nº de passagens") +
  theme_classic() +
  theme(text=element_text(size=16), title=element_text(size=16))
prot_lv_e_limits.plot
