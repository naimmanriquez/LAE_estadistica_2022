## EJERCICIO 12 A ##

## Se llevo a cabo un conjunto de ensayos experimentales para determinar 
## una forma de predecir el tiempo de cocimiento en minutos (Y) 
## a varios niveles de amplitud del horno, (pies, X1) 
## y temperatura de cocción (grados Celsius, X2). 
## Los datos obtenidos fueron registrados como se muestra a continuación:

library(readxl)

ejercicio12a <- read_excel("ejercicio12a.xlsx")

attach(ejercicio12a)

# Definir variables

Y <- cbind(Y)
X1 <- cbind(X1, X2)

# PARTE A #

## Estima la ecuación de regresión múltiple.

olsreg1 <- lm(Y ~ X1)
summary(olsreg1)
confint(olsreg1, level=0.95)

# PARTE B #

## Interpreta los coeficientes individuales de la ecuación de regresión lineal 
## múltiple considerando el contexto del problema.

## respuesta: interpretar

## PARTE C ##

## Pronostica el tiempo de cocimiento cuando el nivel de amplitud 
## del horno es de 5 pies y la temperatura de cocción es de 20 grados Celsius.

## respuesta: calcular con la formula, sustituir valores x1 y x2


rm(list = ls())


### EJERCICIO 12 B ###

#El supervisor de una empresa está examinando la relación existente 
#entre la calificación que obtiene un empleado en una prueba de aptitud, 
#su experiencia previa y el éxito en el trabajo. 
#Se estudia y se pondera la experiencia de un empleado en trabajos anteriores 
#y se obtiene una calificación entre 2 y 12. 
#La medida del éxito en el empleo se basa en un sistema de puntuación 
#que incluye producción total y eficiencia, 
#con valor máximo posible de 50. El supervisor tomó una muestra de seis empleados
#con menos de un año de antigüedad y obtuvo lo siguiente:

library(readxl)

ejercicio12b <- read_excel("ejercicio12b.xlsx")

attach(ejercicio12b)

# Definir variables

Y <- cbind(Y)
X1 <- cbind(X1, X2)

# PARTE A #

## Estima la ecuación de regresión múltiple.

olsreg1 <- lm(Y ~ X1)
summary(olsreg1)
confint(olsreg1, level=0.95)

# PARTE B #

## Interpreta los coeficientes individuales de la ecuación de regresión lineal 
## múltiple; considerando el contexto del problema.

# PARTE C #

## Si un empleado obtuvo 83 puntos en la prueba de aptitud 
## y tenía una experiencia en trabajos anteriores de 7 años, 
## ¿qué evaluación de desempeño puede esperar?