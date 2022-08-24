## Estadistica para la toma de decisiones en R ##

## Removemos notacion cientifica
options(scipen = 999)

## Caracteres usados en el idioma español
Sys.setlocale("LC_ALL", 'es_ES.UTF-8')

## Cargar librerias

library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)

## Abrir archivo de Excel
pokemon <- read_excel("Pokemon.xlsx")

# Si estamos interesados en algunos tipos de pokemones
# Primero vemos los tipos de pokemones
table(pokemon$`Type 1`)

# Podemos usar tapply para ver la media por tipo de pokemon
tapply(pokemon$Attack, pokemon$`Type 1`, mean)

# Para conocer la defensa de los pokemones
tapply(pokemon$Defense, pokemon$`Type 1`, mean)

# Prop table para ver porcentajes de tipo por generacion
round(prop.table(table(pokemon$`Type 1`, pokemon$Generation), 1), 2)

# Prop table para ver porcentajes de generacion y tipos
round(prop.table(table(pokemon$`Type 1`, pokemon$Generation), 2), 2)

# Graficos de distribucion
## Hiatograma con distribucion
ggplot(pokemon, aes(x = Attack)) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(pokemon$Attack), sd = sd(pokemon$Attack)))

# QQPLOT
qqnorm(pokemon$Attack, pch = 1, frame = FALSE)
qqline(pokemon$Attack, col = "steelblue", lwd = 2)

# Parece que hay algunos datos atipicos
# con pokemones muy fuertes
pokemones_normal <- pokemon %>%
  filter(Attack %in% c(10:150))
## Hiatograma con distribucion
ggplot(pokemones_normal, aes(x = Attack)) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(pokemones_normal$Attack), sd = sd(pokemones_normal$Attack)))

# QQPLOT
qqnorm(pokemones_normal$Attack, pch = 1, frame = FALSE)
qqline(pokemones_normal$Attack, col = "steelblue", lwd = 2)


# Filtrar datos para pokemones tipo fuego
# Quiero una nueva base con los pokemones de fuego
pokemones_fuego <- pokemon %>%    
  filter (`Type 1` == "Fire")

## Estadistica descriptiva
summary(pokemones_fuego$Attack)


## Hiatograma con distribucion
ggplot(pokemones_fuego, aes(x = Attack)) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(pokemones_fuego$Attack), sd = sd(pokemones_fuego$Attack)))

# QQPLOT
qqnorm(pokemones_fuego$Attack, pch = 1, frame = FALSE)
qqline(pokemones_fuego$Attack, col = "steelblue", lwd = 2)

