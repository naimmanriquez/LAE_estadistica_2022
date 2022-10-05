## Actividad 6 opcion 1 ##
# Remover notacion cientifica
options(scipen = 999)

# Cargar datos
actividad6 <- read_excel("actividad6.xlsx")

## Librerias
library(ggplot2)
library(olsrr)
library(tidyverse)
library(lmtest)
library(ggthemes)
library(GGally)

# Tomamos datos con la funcion attach()
attach(actividad6)

## Punto A ##
# Estima e interpreta en el contexto del problema 
# los coeficientes de la ecuacion de regresión multiple.

Y <- cbind(caloria)
X <- cbind(grasa, carbohidrato, proteina) 
olsreg1 <- lm(Y ~ X)
summary(olsreg1)

## Punto B ##
# Prueba la significancia del modelo de regresion multiple; 
## realiza todas las etapas de una prueba de hipotesis.

## Matriz de correlaciones ##
cor(Y, X)
C<-actividad6[,2:5]
ggpairs(C)

## Homocedasticidad ##
bptest(olsreg1)

# Residuos
e1hat <- resid(olsreg1)
summary(e1hat)
par(mfrow=c(2, 2))
plot(olsreg1, las=1, col='deepskyblue4', which=1:3)

## Multicolinealidad ##
ols_vif_tol(olsreg1)

## Punto C ##
## Prueba la significancia de los coeficientes de regresion individuales. 
## Realiza todas las etapas de una prueba de hipotesis para cada uno de los coeficientes.

## Intervalos de confianza
confint(olsreg1, level=0.95)

## Punto D ##
# Calcula e interpreta R2 en el contexto del problema.

## Punto E ##
## Calcula el error estándar de estimacion.

## Punto F ##
## Estima la cantidad promedio de calorías cuando el contenido de grasa es de 50 g, 
## la cantidad de carbohidratos es de 10 g y la cantidad de proteinas es de 8 g.

## Punto G ##
# Calcula R2 ajustada.

## Punto E ##
# Construye un intervalo de confianza para las pendientes de la poblacion
confint(olsreg1, level=0.95)



