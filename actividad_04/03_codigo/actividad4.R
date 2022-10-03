## ACTIVIDAD 4 ##
options(scipen = 999)
## Parte 1 ##

## Define los siguientes términos:
## Criterios de estimación de la precisión de un pronóstico 
## (desviación absoluta media, error cuadrático medio, 
## error porcentual absoluto medio y error porcentual medio)
## Análisis de la regresión simple
## Estimadores de mínimos cuadrados
## Intervalo de confianza
## Coeficiente de regresión
## Coeficiente de correlación
## Coeficiente de determinación

## Parte 2 ##

library(readxl)

aguascalientes <- read_excel("aguascalientes.xlsx")

## Busca información de 20 casas en venta en donde las variables son 
## Y (metros de construcción) y X (metros de terreno); 
## y realiza lo que se indica:

## PUNTO A ##

## Realiza el diagrama de dispersión 
## y describe el comportamiento de ambas variables.

library(ggplot2)

ggplot(aguascalientes, aes(x=MT2, y=MC2)) +
  geom_point(alpha=0.6) +
  theme_minimal()

## PUNTO B ## 
## Calcula la recta de regresión de mínimos cuadrados.

p1 <- ggplot(aguascalientes, aes(x = MT2, y = MC2)) + geom_point()

p3 <- p1 + geom_point(color="red") + geom_smooth(method = "lm", se = TRUE)

m <- lm(formula = MC2 ~ MT2, data = aguascalientes)

summary(m)

a <- signif(coef(m)[1], digits = 2)
b <- signif(coef(m)[2], digits = 2)
textlab <- paste("y = ",b,"x + ",a, sep="")

r1 <- p3 + geom_text(aes(x = 80, y = 200, label = textlab), color="black", size=3, parse = FALSE)

r2 <- p3 + annotate("text", x = 80, y = 200, label = textlab, color="black", size = 3, parse=FALSE)

r2

## PUNTO C ##

## ¿Existe evidencia que indique que a mayor cantidad de metros de construcción, 
## mayor es el precio de venta? 
## Prueba la significancia de la recta de regresión 
## con un nivel de significancia ?? = 0.01. ¿Es significativa esta regresión?

confint(m)

## PUNTO D ##

## Pronostica el precio de la vivienda si la cantidad de 
## metros de construcción es de 90, 105 y 120 metros de construcción.

## PUNTO E ## 

## Calcula el coeficiente de correlación y determinación

cor.test(aguascalientes$MC2, aguascalientes$MT2)

summary(m)

## PUNTO F ##
## En un terreno urbano, ¿a mayor cantidad en metros de construcción, 
## mayor es el precio de la vivienda?


rm(list=ls())


## PARTE 3 ##

## Se utilizaron dos modelos de pronóstico para producir 
## los valores futuros de una serie de tiempo; estos valores (Yt) 
## se muestran en la tabla siguiente, 
## junto con los valores reales observados (Yt).

library(readxl)

actividad4p3 <- read_excel("actividad4p3.xlsx")

View(actividad4p3)

## Calcula la DAM, el ECM, el EPAM y el EPM. 
## ¿Alguno de los dos métodos parece superior? Explica.

library(forecast)

fit1 <- rwf(actividad4p3$Modelo1[1:10], h = 10)

accuracy(fit1)

fit2 <- rwf(actividad4p3$Modelo2[1:10], h = 10)

accuracy(fit2)

rm(list = ls())

## PARTE 4 ##

## En una compañía fabricante de helados se sospecha que almacenar 
## el helado a temperaturas bajas durante largos periodos 
## tiene un efecto lineal en la pérdida de peso del producto. 
## En la planta de almacenamiento de la compañía se obtuvieron 
## los siguientes datos:

library(readxl)

actividad4p4 <- read_excel("actividad4p4.xlsx")

View(actividad4p4)

## A ##
## Ajusta e interpreta a los datos un modelo de regresión lineal simple.

m <- lm(formula = peso ~ semanas, data = actividad4p4)

summary(m)

## B ##
## Prueba la significancia de la pendiente ??1.

confint(m)

## C ##

## Calcula e interpreta R2.

summary(m)

## D ##
## Pronostica la pérdida cuando el tiempo es de 33 semanas. ##


## PARTE 5 ##

## Con los conceptos vistos y puestos en práctica, 
# da una respuesta justificada a las siguientes cuestiones:

## ¿Para qué utilizarías la regresión lineal simple en un problema de tu especialidad?
## ¿Qué relación tiene con la correlación?
## ¿Cómo medirías el ajuste del modelo de regresión lineal obtenido?
## ¿Qué es el coeficiente de determinación?
## ¿Por qué crees que se llama regresión lineal?
## ¿Cuál es la relación de la prueba de hipótesis con el intervalo de confianza en la regresión?
