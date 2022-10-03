### PROBLEMA 1 ###

## 1. La energía eléctrica consumida (Y) 
## cada mes por una planta química se considera relacionada 
## con la temperatura ambiente promedio, grados Fahrenheit (X1), 
## número de días al mes (X2), 
## la pureza promedio del producto, en porciento (X3) 
## y las toneladas obtenidas del producto (X4). 
## Se dispone de los datos históricos del año anterior. 

library(readxl)

ejercicio13a <- read_excel("ejercicio13a.xlsx")

attach(ejercicio13a)

# Definir variables

Y <- cbind(Y)
X1 <- cbind(X1, X2, X3, X4) 

## PREGUNTA 1 ##

## Estima e interpreta los coeficientes de la ecuación de regresión lineal 
## múltiple.

olsreg1 <- lm(Y ~ X1)

summary(olsreg1)

## PREGUNTA 2 ##

## Interpreta los coeficientes de regresión en el contexto del problema.

## PREGUNTA 3 ##

## Prueba la significancia global del modelo de regresión múltiple; 
## realiza todas las etapas de una prueba de hipótesis.

## supuetos del modelo ##

## Homocedasticidad ##

library(lmtest)

bptest(olsreg1)

# Residuos
e1hat <- resid(olsreg1)
summary(e1hat)

## Multicolinealidad ##

library(olsrr)

ols_vif_tol(olsreg1)

## Prueba la significancia de los coeficientes de regresión individuales. 
## Realiza todas las etapas de una prueba de hipótesis para cada uno 
## de los coeficientes.

confint(olsreg1, level=0.99)

## Calcula e interpreta R2 en el contexto del problema y el error estandar.

summary(olsreg1)

rm(list = ls())



## PROBLEMA 2 ##

## Un negocio de ventas por catálogo de computadoras personales, 
## software y hardware mantiene un almacén centralizado 
## para la distribución de los productos ordenados. 
## La administración examina el proceso de distribución 
## y está interesada en examinar los factores que afectan 
## los costos. En la actualidad, se cobra una pequeña cuota por manejo, 
## independiente del monto de la orden. Se recolectaron datos de 
## los últimos 24 meses que indican los costos de distribución (Y), 
## las ventas (X1) y el número de órdenes recibidas (X2). 

library(readxl)

ejercicio13b <- read_excel("ejercicio13b.xlsx")

attach(ejercicio13b)

# Definir variables

Y <- cbind(Y)
X1 <- cbind(X1, X2) 


## PREGUNTA 1 ##

## Estima e interpreta los coeficientes de la ecuación de regresión lineal 
## múltiple.

olsreg1 <- lm(Y ~ X1)

summary(olsreg1)

## PREGUNTA 2 ##

## Interpreta los coeficientes de regresión en el contexto del problema.

## PREGUNTA 3 ##

## Prueba la significancia global del modelo de regresión múltiple; 
## realiza todas las etapas de una prueba de hipótesis.

## supuetos del modelo ##

## Homocedasticidad ##

library(lmtest)

bptest(olsreg1)

# Residuos
e1hat <- resid(olsreg1)
summary(e1hat)

## Multicolinealidad ##

library(olsrr)

ols_vif_tol(olsreg1)

## Prueba la significancia de los coeficientes de regresión individuales. 
## Realiza todas las etapas de una prueba de hipótesis para cada uno 
## de los coeficientes.

confint(olsreg1, level=0.99)

## Calcula e interpreta R2 en el contexto del problema y el error estandar.

summary(olsreg1)