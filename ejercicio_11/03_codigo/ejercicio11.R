#### EJERCICIO 11 ####

## En un experimento con conejos se tomaron en cuenta las siguientes variables:
##  Y: Proporción del peso final al peso inicial.
##  X: Gramos diarios de alimento por kg de peso inicial.

library(readxl)

ejercicio11 <- read_excel("ejercicio11.xlsx")

attach(ejercicio11)

# Definir variables

Y <- cbind(peso)
X1 <- cbind(alimentos)

## Realiza un diagrama de dispersión de los datos para Y contra X.

# Grafico de dispersion 
plot(Y ~ X1, data = ejercicio11)

## Calcula la recta de mínimos cuadrados para Y contra X.

# Regresión lineal 
olsreg1 <- lm(Y ~ X1)
summary(olsreg1)
confint(olsreg1, level=0.95)

# Grafico de regresion
abline(olsreg1)

## Prueba la hipótesis de que la pendiente es cero. 
## Realiza todas las etapas de la prueba de hipótesis (?? = 0.01).

confint(olsreg1, level=0.99)

## Calcula las predicciones Y para los siguientes valores de 
## X0: 0, 5, 15, 25, 30, 35.5

Y1hat <- fitted(olsreg1)
summary(Y1hat)
plot(Y1hat ~ X1)
