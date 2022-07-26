# Codigo caso practico 1
options(scipen = 999)
library(readxl)

## a)
# De acuerdo con los datos, la edad media de las personas fallecidas es:
## media de las personas fallecidad
summary(fallecidos_h1n1$edad)

## b)	
# Si se calcula la desviaci�n est�ndar, obtenemos este valor:
## desviacion estandar
sd(fallecidos_h1n1$edad)

## c) 
## Contraste de hip�tesis:
  t.test(fallecidos_h1n1$edad,
         mu = 70, 
         alternative = "two.sided" ) # contraste bilateral
  
# df = grados de libertad
# t = t calculada: 4.4647
# t de tablas con 0.05: 1.7207
  
#  d)	
# De acuerdo con los resultados obtenidos, 
# la decisi�n correcta con respecto a la hip�tesis es? 