library(ggplot2)
library(ggthemes)
library(readxl)
library(dplyr)
library(scales)

### EJERCICIO 9 ###

### PARTE 1 ### 
## Realizar un resumen sobre el método de mínimos cuadrados, 
## donde se explique la razón por la cual se le denomina así.

### PARTE 2 ## 
## Resuelve el siguiente ejercicio:

## Una empresa de bienes raíces ha recopilado datos para ayudar a determinar 
## cómo el número de ventas de viviendas en la región está relacionado 
## con los niveles de tasas de interés de la hipoteca. 
## En la siguiente tabla se muestran el número de viviendas vendidas 
## en la región y las tasas de interés de la hipoteca para 12 meses, 
## seleccionados al azar.

ejercicio9 <- read_excel("ejercicio9.xlsx")


### PUNTO A ###
## Realiza un diagrama de dispersión para estos datos con 
## el número de casas vendidas en el eje vertical.

ggplot(ejercicio9, aes(x=interes, y=viviendas, size = interes, color=zona)) +
  geom_point(alpha=0.6) +
  scale_size(range = c(.1, 12), name="Tasa de interes") +
  theme_solarized_2()


## PUNTO B ##
## Describe la relación entre el interés de la hipoteca 
## y el número de viviendas vendidas.


## PUNTO C ##
## Determina la recta de regresión que describa como las tasas de interés 
## (X) afectan el número de viviendas vendidas (Y). 
## ¿Qué indica el coeficiente de regresión acerca de esta relación?



p1 <- ggplot(ejercicio9, aes(x = interes, y = viviendas))
p1 + geom_point()
p3 <- p1 + geom_point(color="red") + geom_smooth(method = "lm", se = TRUE)

m <- lm(formula = viviendas ~ interes, data = ejercicio9)

summary(m)


a <- signif(coef(m)[1], digits = 2)
b <- signif(coef(m)[2], digits = 2)
textlab <- paste("y = ",b,"x + ",a, sep="")

r1 <- p3 + geom_text(aes(x = 11, y = 207, label = textlab), color="black", size=3, parse = FALSE)

r2 <- p3 + annotate("text", x = 11, y = 207, label = textlab, color="black", size = 3, parse=FALSE)

r2

## PUNTO D
## Pronostica el número de viviendas vendidas si la tasa de interés es del 10%


## PUNTO E

## Determina el coeficiente de correlación.

library(ggpubr)

ggscatter(ejercicio9, x = "interes", y = "viviendas", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Interes", ylab = "Viviendas vendidas")