# Removemos notacion cientifica
options(scipen = 999)

## PROBLEMA 1 ##
# Si el coeficiente de determinación entre dos variables independientes es 0.20, 
# ¿cuál es el valor del factor de inflación de varianza VIF?

## PROBLEMA 2 ##
# Si el coeficiente de determinación entre dos variables independientes es 0.50, 
# ¿cuál es el valor del factor de inflación de varianza VIF?

## PROBLEMA 3 ##
# Un negocio de ventas por catAlogo de computadoras personales, 
# software y hardware mantiene un almacén centralizado para la distribución 
# de los productos ordenados. La administración examina el proceso de distribución 
# y está interesada en examinar los factores que afectan los costos. 
# En la actualidad, se cobra una pequeña cuota por manejo, 
# independiente del monto de la orden. 
# Se recolectaron datos de los últimos 24 meses que indican 
# los costos de distribucion (Y), las ventas (X1) y el número de ordenes recibidas (X2).

library(readxl)
library(olsrr)

# Cargar datos
ejercicio_15_3 <- read_excel("ejercicio_15_3.xlsx")

# Declarar datos
attach(ejercicio_15_3)

# Definir variables
Y <- cbind(Y)
X1 <- cbind(X1, X2)

# a) Realiza una regresión múltiple y determina 
# el VIF para cada variable explicativa en el modelo

olsreg1 <- lm(Y ~ X1)
summary(olsreg1)
confint(olsreg1, level=0.95)

## Multicolinealidad ##

ols_vif_tol(olsreg1)

# Eliminamos el data frame
rm(list = ls())


## PROBLEMA 4 ##
# Una organizacion de consumidores desea desarrollar un modelo para predecir 
# el rendimiento de gasolina de automovil, medido en cantidad de millas recorridas 
# [millas por galón (mpg)] de acuerdo a los caballos de fuerza del motor 
# y el peso del auto Kg.

# Cargar datos
ejercicio_15_4 <- read_excel("ejercicio_15_4.xlsx")

# Declarar datos
attach(ejercicio_15_4)

# Definir variables
Y <- cbind(Y)
X1 <- cbind(X1, X2)

# a) Realiza una regresión múltiple

olsreg1 <- lm(Y ~ X1)
summary(olsreg1)
confint(olsreg1, level=0.95)

## Determina el factor de inflación de varianza VIF para 
# cada variable explicativa en el modelo. 
# ¿Existe alguna razón para sospechar que existe multicolinealidad?

ols_vif_tol(olsreg1)

# Eliminamos el data frame
rm(list = ls())


## PROBLEMA 5 ##
# El director de operaciones de transmisión de una estacion de television 
## desea estudiar el aspecto de las “horas de espera” 
# en la que los artistas gráficos sindicalizados 
# se les paga por no realizar actividades. 
# Las variables a considerar son las siguientes:

# Cargar datos
ejercicio_15_5 <- read_excel("ejercicio_15_5.xlsx")

# Declarar datos
attach(ejercicio_15_5)

# Definir variables
Y <- cbind(Y)
X1 <- cbind(X1, X2)

# a) Realiza una regresión múltiple

olsreg1 <- lm(Y ~ X1)
summary(olsreg1)
confint(olsreg1, level=0.95)

## Determina el factor de inflación de varianza VIF para 
# cada variable explicativa en el modelo. 
# ¿Existe alguna razón para sospechar que existe multicolinealidad?

ols_vif_tol(olsreg1)

# Eliminamos el data frame
rm(list = ls())
