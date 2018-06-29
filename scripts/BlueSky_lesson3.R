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
#Plots
g <- ggplot(dados, aes(x=date,y=demand)) +
  geom_point() + 
  labs(title="Evolução da demanda ao longo do ano", x="Data", y="Demanda de passageiros") +
  theme_classic() +
  theme(text=element_text(size=18), title=element_text(size=18))
g + geom_line()

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
  labs(title="Comparação da demanda média (azul) e o nível de proteção (vermelho)", 
       subtitle="Considerando uma demanda normalizada", x="Data", y="Demanda de passageiros") +
  theme_classic() +
  theme(text=element_text(size=18), title=element_text(size=18))

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
  labs(title="Comparação da demanda mediana (azul) e o nível de proteção (vermelho)", 
       subtitle="Considerando uma demanda empírica", x="Data", y="Demanda de passageiros") +
  theme_classic() +
  theme(text=element_text(size=18), title=element_text(size=18))
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
  labs(title="Comparação da demanda média (azul) e o nível de proteção (vermelho)", 
       subtitle="Considerando uma demanda normalizada e os dias da semana", x="Data", y="Demanda de passageiros") +
  theme_classic() +
  theme(text=element_text(size=18), title=element_text(size=18))
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
  labs(title="Comparação da demanda média (azul) e o nível de proteção (vermelho)", 
       subtitle="Considerando uma demanda empírica e os dias da semana", x="Data", y="Demanda de passageiros") +
  theme_classic() +
  theme(text=element_text(size=18), title=element_text(size=18))
prot_level_4.plot

### Modelo 5 - Regressão ----
dados$month <- months(dados$date)

# Para regressão: FATORES
dados$day <- factor(dados$day)
dados$month <- factor(dados$month)

#summary(dados$day)
#is.factor(dados$day)
#levels(dados$day)

#---- Regressão - considerando o dia da semana
dados.fit.1 <- lm(data=dados, demand ~  day)
rsme_5.1 <- summary(dados.fit.1)$sigma

#---- Regressão - considerando o mes
dados.fit.2 <- lm(data=dados, demand ~  month)
rsme_5.2 <- summary(dados.fit.2)$sigma

#---- Regressão - considerando dia da semana e o mes
dados.fit.3 <- lm(data=dados, demand ~  day + month)
rsme_5.3 <- summary(dados.fit.3)$sigma

#---- Regressão - considerando a interação do dia da semana e mês
dados.fit.4 <- lm(data=dados, demand ~ day*month)
rsme_5.4 <- summary(dados.fit.4)$sigma

#plotando os valores previstos
prediction <- predict(dados.fit.3, newdata = dados)

prot_level_5.plot <- g +
  geom_point(aes(x=dados$date,y=dados.fit.3$fitted.values), color = "red") + 
  geom_line(aes(x=dados$date,y=prediction), linetype="dotdash", color = "blue") +
  #geom_line(aes(x=date, y=mediana), color="blue") +
  #geom_line(aes(x=date, y=prot_level_4), color="red") + 
  labs(title="Comparação da demanda média (azul) e o nível de proteção (vermelho)", 
       subtitle="Considerando uma demanda empírica e os dias da semana", x="Data", y="Demanda de passageiros") +
  theme_classic() +
  theme(text=element_text(size=18), title=element_text(size=18))
prot_level_5.plot

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(dados.fit.3)


dados$date[263] #Thanksgiving Day
dados$date[272]
dados$date[300] #Newyear

##considerando datas exepcionais
dados <- dados %>%
  mutate (holydays=0)

dados[257, "holydays"] <- "1"
dados[258, "holydays"] <- "1"
dados[259, "holydays"] <- "1"
dados[260, "holydays"] <- "1"
dados[261, "holydays"] <- "1"
dados[262, "holydays"] <- "1"
dados[263, "holydays"] <- "1"
dados[264, "holydays"] <- "1"
dados[265, "holydays"] <- "1"
dados[266, "holydays"] <- "1"
dados[267, "holydays"] <- "1"
dados[268, "holydays"] <- "1"

dados[272, "holydays"] <- "2"
dados[297, "holydays"] <- "2"
dados[298, "holydays"] <- "2"
dados[299, "holydays"] <- "2"
dados[300, "holydays"] <- "1"
dados[301, "holydays"] <- "1"
dados[302, "holydays"] <- "1"
dados$holydays <- factor(dados$holydays)
levels(dados$holydays)

#---- Regressão - considerando a interação do dia da semana e mês
dados.fit.5 <- lm(data=dados, demand ~ day*month + holydays)
rsme_5.5 <- summary(dados.fit.5)$sigma #20.69231  #19.21393 #19.41123

dados.fit.6 <- lm(data=dados, demand ~ day*month*holydays)
rsme_5.6 <- summary(dados.fit.6)$sigma #19.98660  #19.19865 #18.22783

plot(dados.fit.5) #menos overfit


#plotando os valores previstos

prediction_b <- predict(dados.fit.5, newdata = dados)

prot_level_5b.plot <- g +
  geom_point(aes(x=dados$date,y=dados.fit.5$fitted.values), color = "red") + 
  geom_line(aes(x=dados$date,y=prediction_b), linetype="dotdash", color = "blue") +
  #geom_line(aes(x=date, y=mediana), color="blue") +
  #geom_line(aes(x=date, y=prot_level_4), color="red") + 
  labs(title="Comparação da demanda média (azul) e o nível de proteção (vermelho)", 
       subtitle="Considerando uma demanda empírica e os dias da semana", x="Data", y="Demanda de passageiros") +
  theme_classic() +
  theme(text=element_text(size=18), title=element_text(size=18))
prot_level_5b.plot

## Definindo pares de proteção e limites ####

week.df <- dados %>%
  filter(date > "2006-03-18 00:00:00" & date < "2006-03-25 00:00:00")

prot_lv_e_limits.df <- week.df %>%
  mutate( prot_lv = predict(dados.fit.5, newdata = week.df),
          reserve_limit = 140-predict(dados.fit.5, newdata = week.df))
prot_lv_e_limits.df

### Plotando proteção e limites
prot_lv_e_limits.plot <- 
  ggplot(prot_lv_e_limits.df, aes(x=date)) +
  geom_point(aes(y=demand)) + 
  geom_area(aes(y=demand), alpha=0.1) +
  geom_point(aes(y=prot_lv), color = "blue") + 
  geom_area(aes(y=prot_lv), color = "blue", fill="blue", alpha = 0.2, linetype = "solid") +
  geom_point(aes(y=reserve_limit), color = "red") +
  geom_area(aes(y=reserve_limit), color = "red", fill="red", alpha = 0.2, linetype = "solid") +
  labs(title="Nível de proteção (azul) e limites de reservas (vermelho)", 
       subtitle="Determinado a partir dos dados do ano anterior (preto)", x="Data", y="Nº de passagens") +
  theme_classic() +
  theme(text=element_text(size=18), title=element_text(size=18))
prot_lv_e_limits.plot