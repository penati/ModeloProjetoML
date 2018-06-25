## CARREGANDO PACKAGES ####
library(tidyverse)


## LEITURA E PREPARACAO DOS DADOS ####
### Leitura

#ChickWeight.df <- ChickWeight
#save(ChickWeight.df, file="data/ChickWeight.Rdata")

load("data/ChickWeight.Rdata")
data <- ChickWeight.df

# dimensoes do dataframe
dim(data)
# primeiros dados
head(data)
# ultimos dados
tail(data)
# numero de obersavacoes para cada "Chick"
xtabs(~Chick, data)
# numero de obersavacoes para cada "Diet"
xtabs(~Diet, data)
# numero de obersavacoes de Chicks por time
xtabs(~Time, data)

# Subset out Time
time_21 = data %>%
  # Filter to only include Time 21
  filter(Time == 21)

time_21
dim(time_21)
head(time_21)
tail(time_21)


## MAKE FIGURES ####

data.plot = ggplot(data, aes(x=Time, y=weight)) +
  geom_point(aes(color=Diet)) +
  geom_smooth(aes(color=Diet), se = FALSE) +
  labs(title="Aumento de peso do pintinho conforme a dieta", x="Dias de vida", y="Peso (g)") +
  theme_classic() +
  theme(text=element_text(size=18), title=element_text(size=18))
data.plot

# Write figure to a pdf in the 'figures' folder
pdf("figures/aumento-peso-pintinhos.pdf")
# Call plot
data.plot
# Close pdf call
dev.off()

time_21.plot = ggplot(time_21, aes(x=Diet, y=weight)) +
  geom_boxplot(aes(color=Diet)) +
  geom_point(aes(color=Diet)) +
  labs(title="Peso dos pintinhos aos 21 dias de vida", x="Dieta", y="Peso (g)") +
  theme_classic() +
  theme(text=element_text(size=18), title=element_text(size=18), legend.position="none")
time_21.plot

# Write figure to a pdf in the 'figures' folder
pdf("figures/peso-21dias-pintinhos.pdf")
# Call plot
time_21.plot
# Close pdf call
dev.off()

## RUN DESCRIPTIVE STATISTICS ####
### Summarise data

weight_by_time <- data %>%
  # agrupando por "Diet" e pelo "Time"
  group_by(Diet, Time) %>%
  # obtendo a média, desvio padrao, maximo e minimo pesos para cada "Time", de acordo com a dieta
  summarise(weight_mean = mean(weight),
            weight_sd = sd(weight),
            weight_min = min(weight),
            weight_max = max(weight))

weight_by_time

weight_21days_by_diet <- time_21 %>%
  # agrupando por "Diet"
  group_by(Diet) %>%
  # obtendo a média, desvio padrao, maximo e minimo peso aos 21 dias para cada dieta
  summarise(weight_mean = mean(weight),
            weight_sd = sd(weight),
            weight_min = min(weight),
            weight_max = max(weight))

weight_21days_by_diet