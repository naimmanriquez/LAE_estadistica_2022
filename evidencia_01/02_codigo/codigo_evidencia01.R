## Analisis de datos de la endutih

# 1. Removemos notacion cient?fica
options(scipen = 999)

# Instalamos

install.packages("sjmisc")
install.packages("sjlabelled")
install.packages("survey")
install.packages("dplyr")
install.packages("pacman")

## Cargamos librerias 

library(pacman)
p_load(sjmisc, sjlabelled, tidyverse, haven, survey, dplyr, ggplot2)

## Carga de base de datos
base_endutih <- readRDS(url("https://github.com/naimmanriquez/LAE_estadistica_2022/blob/main/evidencia_01/01_datos/base_endutih.rds?raw=true"))

# Nos quedamos solo con los mayores de 18 anios
endutih_mayores <- base_endutih %>%
  filter(EDAD %in% c(18:80))

# Convertir a numerico por si no esta
endutih_mayores <- endutih_mayores %>%
  mutate_at("EDAD", ~as.numeric(.)) 

endutih_mayores <- endutih_mayores %>%
  mutate_at("P7_4", ~as.numeric(.)) 

## CONOCIENDO LA ESTRUCTURA DE LOS DATOS ##

# Estructura de la base de datos y variables
str(endutih_mayores)
glimpse(endutih_mayores)

# Para una sola variable: Generalmente cuantas horas al dia utiliza Internet?
str(endutih_mayores$P7_4)

## Estadisticos descriptivos del uso de internet
summary(endutih_mayores$P7_4)

# Tabla de frecuencias
endutih_mayores %>% frq(P7_4)


## PREGUNTA 1
## Para los datos en general, determina el promedio de tiempo dedicado a Internet.
summary(endutih_mayores$P7_4)

## PREGUNTA 2
## Para el total de datos, determina la varianza y la desviacion estandar del tiempo que dedican al uso de Internet.
# varianza
var(endutih_mayores$P7_4, na.rm = TRUE)
# desviacion estandar
sd(endutih_mayores$P7_4, na.rm = TRUE)


# Grafica
# Boxplot
ggplot(endutih_mayores, aes(y = P7_4)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.15,
               color = 1) +  # Color barras 
  geom_boxplot(fill = 3,           # Color caja
               alpha = 0.5,        # Transparencia
               color = 1,          # Color del borde
               outlier.colour = 2) + # Color atípicos
  labs(title = 'Horas de uso del internet al día',
       y = 'Horas de uso',
       subtitle = 'Diagrama de cajas',
       caption = 'Fuente: ENDUTIH 2021. INEGI')

# Distribucion
ggplot(endutih_mayores, aes(x = P7_4)) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white") +
  stat_function(aes(P7_4),fun=dexp) +
  labs(title = 'Horas de uso del internet al día',
       y = 'Horas de uso',
       subtitle = 'Curvas de distribución',
       caption = 'Fuente: ENDUTIH, 2021. INEGI')

## PREGUNTA 3
# Para los datos por genero, determina en promedio quien dedica mas tiempo a Internet: 
## hombres o mujeres.
endutih_mayores$genero <- endutih_mayores$SEXO
endutih_mayores$genero <-factor(endutih_mayores$genero, labels = c("Masculino","Femenino"))
tapply(endutih_mayores$P7_4, endutih_mayores$genero, summary)

# Grafica
# Boxplot
ggplot(endutih_mayores, aes(y = P7_4)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.15,
               color = 1) +  # Color barras 
  geom_boxplot(fill = 3,           # Color caja
               alpha = 0.5,        # Transparencia
               color = 1,          # Color del borde
               outlier.colour = 2) + # Color atípicos
  facet_wrap(~ genero) +
  labs(title = 'Horas de uso del internet al día por género',
       y = 'Horas de uso',
       subtitle = 'Diagrama de cajas',
       caption = 'Fuente: ENDUTIH 2021. INEGI')

# Distribucion
ggplot(endutih_mayores, aes(x = P7_4)) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white") +
  facet_wrap(~ genero) +
  stat_function(aes(P7_4),fun=dexp) +
  labs(title = 'Horas de uso del internet al día por género',
       y = 'Horas de uso',
       subtitle = 'Curvas de distribución',
       caption = 'Fuente: ENDUTIH, 2021. INEGI')

## PREGUNTA 4 y 5
## Para los hombres y mujeres, calcula el coeficiente de correlacion lineal 
## entre la edad y el tiempo dedicado al uso de Internet.
## ¿Como se interpreta esa correlacion?

# Dado que es factor, la convertimos a numerica
r <- by(endutih_mayores, endutih_mayores$genero, FUN = function(X) cor(endutih_mayores$EDAD, endutih_mayores$P7_4, method = "spearman", use="complete.obs"))

r

## PREGUNTA 6
## Para los datos por genero: determina la mediana de la edad y del tiempo dedicado a Internet.

tapply(endutih_mayores$P7_4, endutih_mayores$genero, summary)

## PARTE 2 - PRUEBA DE HIPOTESIS
## Imagina que el promedio que dedica una persona a Internet 
## (sin importar su genero) es de 7 horas diarias. 
## Con los datos anteriores, prueba las siguientes hipotesis:

## H0: miu = 7 contra la alternativa de que Ha : miu =/ 7 
## con un nivel de significancia de 0.05. 
## Realiza todas las etapas de una prueba de hipotesis 
## y concluye sobre el contexto del problema. 
## ¿Es el tiempo promedio dedicado a Internet diferente a 7?

mu0=7

mu0

sol.test=t.test(endutih_mayores$P7_4,mu=7,alternative="two.sided",conf.level=0.95)

#Resumen del test
sol.test

## Intervalo de confianza
install.packages("Rmisc")
library(Rmisc)

CIvector <- CI(na.omit(endutih_mayores$P7_4))

CIvector

