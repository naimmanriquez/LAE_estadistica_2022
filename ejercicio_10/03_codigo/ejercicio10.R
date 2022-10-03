library(ggplot2)
library(ggthemes)
library(readxl)
library(dplyr)
library(scales)
library(readxl)

## EJERCICIO 10 ##
## PROBLEMA 1 ##

## Las ventas de línea blanca varían según el estado del mercado de casas nuevas: 
## cuando las ventas de casas nuevas son buenas, 
## también se reflejan éstas en las cifras de lavaplatos, 
## lavadoras de ropa, secadoras y refrigeradores. 
## Una asociación de comercio compiló los siguientes datos históricos 
## (en miles de unidades) de las ventas de línea blanca y construcción de casas.

ejercicio10a <- read_excel("ejercicio10a.xlsx")

## PUNTO A ##
## Realiza un diagrama de dispersión para estos datos.

ggplot(ejercicio10a, aes(x=casas, y=ventas)) +
  geom_point(alpha=0.6) +
  theme_minimal()

## PUNTO B ##
## Desarrolla una ecuación para la relación entre las ventas de línea blanca 
## (en miles) y la construcción de casas (miles).

regresion <- lm(formula = ventas ~ casas, data = ejercicio10a)

summary(regresion)

## PUNTO C ##
## Interpreta la pendiente de la recta de regresión.

## PUNTO D ##

## Calcula e interpreta el coeficiente de determinación de la muestra, 
## r2, para estos datos.

### PUNTO E ##
## Interpreta el error estándar de estimación.


rm(list= ls())


## PROBLEMA 3 ##

## Una compañía de productos químicos desea estudiar los efectos 
## que el tiempo de extracción tiene en la eficiencia de una operación 
## de extracción, obteniéndose los datos que aparecen en la siguiente tabla:

library(readxl)

ejercicio10c <- read_excel("ejercicio10c.xlsx")

View(ejercicio10c)

## PUNTO A ##
## Realiza un diagrama de dispersión para verificar 
## que una línea recta se ajustará bien a los datos.

library(ggplot2)
library(ggthemes)

ggplot(ejercicio10c, aes(x=tiempo, y=eficiencia)) +
  geom_point(alpha=0.6) +
  theme_minimal()

## PUNTO B ##
## Obtén una línea de regresión estimada.

p1 <- ggplot(ejercicio10c, aes(x = tiempo, y = eficiencia)) + geom_point()

p3 <- p1 + geom_point(color="red") + geom_smooth(method = "lm", se = TRUE)

m <- lm(formula = eficiencia ~ tiempo, data = ejercicio10c)

summary(m)


a <- signif(coef(m)[1], digits = 2)
b <- signif(coef(m)[2], digits = 2)
textlab <- paste("y = ",b,"x + ",a, sep="")

r1 <- p3 + geom_text(aes(x = 20, y = 70, label = textlab), color="black", size=3, parse = FALSE)

r2 <- p3 + annotate("text", x = 20, y = 70, label = textlab, color="black", size = 3, parse=FALSE)

r2

## PUNTO C ##
## Utiliza la ecuación estimada de regresión para predecir 
## la eficiencia de extracción cuando el tiempo de extracción es de 35 minutos.

## PUNTO D ##
## Prueba la hipótesis de que:
## H0 : b1 = 0 en oposición a Ha: b1 difernete de 0. 

summary(m)
confint(m)