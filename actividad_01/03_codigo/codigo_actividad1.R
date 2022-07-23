## Removemos notacion cientifica
options(scipen = 999)

## Caracteres usados en el idioma español
Sys.setlocale("LC_ALL", 'es_ES.UTF-8')

## Instalar librerias
# install.packages("readxl") # Para importar datos o archivos desde excel
# install.packages("foreign") # Para importar datos de otros formatos
# install.packages("haven") # Para importar archivos desde stata o spss 
# install.packages("tidyverse") # Para manipular datos
# install.packages("dplyr") # Para manipular datos
# install.packages("ggplot2") # Para graficas
# install.packages("survey") # Para trabajar con encuestas
# install.packages("sjmisc") 


## Cargar librerias
library(foreign)
library(readxl)
library(haven)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(survey)
library(sjmisc)

## Ahora si, empezamos
bd <- read.csv("jugadores_fifa_2022.csv")

# Organiza los datos en una tabla de frecuencia. 
# Debe contar con 5 clases (determina la amplitud de la clase), 
# frecuencia relativa y frecuencia absoluta.

# Tabla
table(bd$gender)
# Estadisticos descriptivos
summary(bd$height_cm)
summary(bd$weight_kg)

# Porcentajes de datos
bd %>% frq(gender)

# Vamos a generar intervalos de estatura
bd$intervalo_cm<-cut(bd$height_cm,breaks=c(0,160,171,180,190,206))
bd$intervalo_cm<-factor(bd$intervalo_cm,
                     levels=c("(0,160]","(160,171]","(171,180]","(180,190]","(190,206]")
                     ,labels=c("160-","161 a 170","171 a 180","181 a 189","190 a 206")
)

table(bd$intervalo_cm)

# Vamos a generar intervalos de peso
bd$intervalo_kg<-cut(bd$weight_kg,breaks=c(0,50,60,80,100,110))
bd$intervalo_kg<-factor(bd$intervalo_kg,
                        levels=c("(0,50]","(50,60]","(60,80]","(80,100]","(100,110]")
                        ,labels=c("50-","51 a 60","61 a 79","80 a 100","100+")
)

table(bd$intervalo_kg)

## Frecuencias absolutas y relativas

## Frecuencias
bd %>% frq(gender, intervalo_cm)
bd %>% frq(gender, intervalo_kg)

# N significa frecuencia absoluta
# Valid es frecuencia relativa
# Cum es frecuencia acumulada

# Calcula lo siguiente por género: media, 
# mediana, moda, desviación estándar, rango y varianza.

tapply(bd$height_cm, bd$gender, summary) # media y mediana
tapply(bd$height_cm, bd$gender, sd) # desviacion estandar
tapply(bd$height_cm, bd$gender, var) # varianza

#calcular rango 
max (bd$height_cm, na.rm = TRUE ) - min (bd$height_cm, na.rm = TRUE )
max (bd$weight_kg, na.rm = TRUE ) - min (bd$weight_kg, na.rm = TRUE )

# Realiza los histogramas correspondientes para la variable de peso, 
# considerando los géneros (histograma de peso-mujeres, 
# histograma de peso-hombres).

## Histograma mujeres

bd_female <- bd %>%    
  filter (gender == "female")

ggplot(data = bd_female,
       mapping = aes(x = weight_kg)) +
  geom_histogram(bins = 9,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Peso',
       fill = 'vs genero',
       x = 'peso',
       y = 'conteos',
       subtitle = 'Histograma',
       caption = 'Fuente: Datos de mujeres en el FIFA')

## Histograma hombres

bd_male <- bd %>%    
  filter (gender == "male")

ggplot(data = bd_male,
       mapping = aes(x = weight_kg)) +
  geom_histogram(bins = 9,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Peso',
       fill = 'vs genero',
       x = 'peso',
       y = 'conteos',
       subtitle = 'Histograma',
       caption = 'Fuente: Datos de hombres en el FIFA')

## Diagrama de dispersión
ggplot(bd, aes(x=weight_kg, y=height_cm)) + 
  geom_point(aes(color=gender))





