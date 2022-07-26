## 1. Removemos notaci�n cientifica
options(scipen = 999)

# Cargar librerias
library(readxl)
library(lmtest)
library(olsrr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)

# Leer archivo #
ejercicio14a <- read_excel("ejercicio14a.xlsx")

# Problema 1 #
## Una cadena de comida rapida ha experimentado un cambio importante en sus ventas 
## como resultado de una campa�a de publicidad exitosa. 
## En consecuencia, la gerencia ahora necesita un nuevo modelo de regresi�n para sus ventas.

attach(ejercicio14a)

# Punto A #
## Determinar la ecuaci�n que mejor se ajuste a sus ventas.

Y <- cbind(ventas)
X1 <- cbind(semanas) 
olsreg1 <- lm(Y ~ X1)
summary(olsreg1)

## Punto B #
## Encuentra el coeficiente de determinaci�n e interpretalo en el contexto del problema.

## Punto C #
## �Est�s satisfecho con el modelo como pronosticador de ventas (Y)? 
## Explica. Realiza todas las etapas de una prueba de hipotesis con ?? = 0.05.

## Homocedasticidad ##

bptest(olsreg1)

# Residuos
e1hat <- resid(olsreg1)
summary(e1hat)

## Punto D #
## Transforma la variable independiente (X2) y ahora corre de nuevo el modelo con X y X2 

ejercicio14a$semanas2 ='^'(ejercicio14a$semanas,2)

attach(ejercicio14a)

Y <- cbind(ventas)
X1 <- cbind(semanas, semanas2) 
olsreg1 <- lm(Y ~ X1)
summary(olsreg1)

## Punto E #
## Encuentra el coeficiente de determinaci�n e interpretalo en el contexto del problema. 
## Comp�ralo con el obtenido en el inciso b

# Problema 2 #
## Un editor de libros de texto universitarios realiz� un estudio para relacionar 
## la ganancia por libro (Y) con el costo de venta (X) para un periodo de seis a�os. 
## Se obtuvieron los siguientes datos (en miles de d�lares, ajustados por la inflaci�n):

## Leer datos
ejercicio14b <- read_excel("ejercicio14b.xlsx")

# Punto A # 
# Ajusta un modelo cuadratico

ejercicio14b$X2 ='^'(ejercicio14b$X,2)

attach(ejercicio14b)

Y <- cbind(Y)
X1 <- cbind(X, X2) 
olsreg1 <- lm(Y ~ X1)
summary(olsreg1)

## Punto B ##
# �Proporcionan los datos suficiente evidencia para indicar una curvatura entre (Y) y (X)? 

## Punto C ##
# Encuentra el coeficiente de determinaci�n en la salida de Excel e interpreta su valor en el contexto del problema.

## Punto D ##
## Utiliza la ecuaci�n de predicci�n para estimar la utilidad media del libro 
## cuando el costo de venta por libro es de $6500 (expresa dicho costo 
## en miles de d�lares antes de sustituirlo en la ecuaci�n).

# Problema 3 #
## Un agr�nomo est� interesado en la producci�n de algod�n recopilada 
## en los siguientes datos referentes al n�mero de bellotas por planta durante 
## la estaci�n de crecimiento. Aqu� Y es la media del n�mero de bellotas por planta 
## y X es el tiempo medido en semanas.

# Leer archivo #
ejercicio14c <- read_excel("ejercicio14c.xlsx")

## Punto A #
## Elabora un diagrama de dispersion ##

ggplot(ejercicio14c, aes(x=X, y=Y)) +
  geom_point(size=2, shape=22) +
  theme_stata()

## Punto B #
## Ajusta un modelo cuadratico

ejercicio14c$X2 ='^'(ejercicio14c$X,2)

attach(ejercicio14c)

Y <- cbind(Y)
X1 <- cbind(X, X2) 
olsreg1 <- lm(Y ~ X1)
summary(olsreg1)

## Punto C #
# Encuentra el coeficiente de determinaci�n en la salida del software 
## e interpreta su valor

## Punto D #
## Utiliza la ecuacion de prediccion para estimar el n�mero de bellotas 
## cuando el n�mero de semanas es de 8, 10 y 14.

