
## EJERCICIO 4

## PROBLEMA 1 ### 

## Las puntuaciones en un test que mide la variable creatividad siguen, 
## en la población general de adolescentes, una distribución normal 
## con media de 11.5 puntos. En un centro escolar que ha implantado 
## un programa de estimulación de la creatividad, 
## se tomó una muestra de 30 alumnos que ha proporcionado 
## las siguientes puntuaciones:

library(readxl)
creatividad <- read_excel("C:/Users/naim_/Downloads/creatividad.xlsx")
View(creatividad)

## A un nivel de confianza de 95%, 
## ¿puede afirmarse que el programa es efectivo? 
## Realiza el planteamiento de la prueba de hipótesis 
## y pruébala estadísticamente.

summary(creatividad)

mean(creatividad$creatividad)

sd(creatividad$creatividad)

## Hipotesis nula

mu0=11.5

mu0

sol.test=t.test(creatividad,mu=11.5,alternative="two.sided",conf.level=0.95)

#Resumen del test
sol.test

## No hay evidencia que nos permita rechazar la hipótesis nula
## Ho = 11.5 contra la alternativa diferente de 11.5


### PROBLEMA 2

## Se somete a prueba a todos los integrantes del magisterio 
## de enseñanza básica (primaria) de un país. 
## Un experto en educación afirma que el promedio de la calificación, 
## sobre una base de 100, fue de 76. 
## Un representante del alto gobierno pone en duda dicha afirmación, 
## por lo cual se toma una muestra aleatoria de 400 maestros 
## cuya media fue de 74 con desviación estándar de 16. 
## Comprueba dicha afirmación con una prueba de hipótesis 
## y un nivel de significancia del 1%.

xbarra <- 74  # Datos del problema
desvia <- 16   # Datos del problema
n <- 400         # Datos del problema
mu <- 76      # Media de referencia

est <- (xbarra - mu) / (desvia / sqrt(n))

est  # Para obtener el valor del estadístico

pnorm(est)  # Para obtener el valor-P

## Como el P valor es menor a 0.01, se rechaza la hipotesis nula.



### PROBLEMA 3

## Un fabricante de pintura de secado rápido afirma 
## que el tiempo de secado de la misma es de 20 minutos. 
## El comprador diseña el siguiente experimento: pinta 36 tableros 
## y decide rechazar el producto si el promedio de tiempo de secado supera 
## la afirmación del fabricante. La media resultante de la muestra 
## fue de 20.75 min. Si por experiencia s = 2.4 min, 
## se pregunta cuál es la probabilidad de rechazar la afirmación 
## del fabricante suponiendo que la población tiene una media 
## de secado de 20 min. Realiza la prueba de hipótesis con 
## un nivel de significancia del 10%. 
## ¿Existe evidencia suficiente para afirmar la hipótesis del fabricante?


xbarra <- 20.75  # Datos del problema
desvia <- 2.4   # Datos del problema
n <- 36         # Datos del problema
mu <- 20      # Media de referencia

est <- (xbarra - mu) / (desvia / sqrt(n))

est  # Para obtener el valor del estadístico

pnorm(est)  # Para obtener el valor-P

## Como el P valor es mayor a 0.10, no se rechaza la hipotesis nula.



### PROBLEMA 4

## El índice de resistencia a la rotura de un tipo de cuerda, 
## expresado en kg, sigue una distribución normal con desviación 
## típica 15.6 kg. Con una muestra de 5 de estas cuerdas 
## seleccionadas al azar, se obtuvieron los siguientes índices: 
## 280, 240, 270, 285, 270. Obtén un intervalo de confianza 
## para la media del índice de resistencia a la rotura utilizando 
## un nivel de confianza del 95%. Realiza 
## una interpretación del intervalo de confianza obtenido.

install.packages("distributions3")

library(distributions3)

x <- c(280, 240, 270, 285, 270)
n <- length(x)

Z <- Normal(0, 1)

mean(x) + quantile(Z, 0.05 / 2) * 15.6 / sqrt(n)

mean(x) - quantile(Z, 0.05 / 2) * 15.6 / sqrt(n)

