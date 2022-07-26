## Removemos notacion cientifica
options(scipen = 999)

## Cargar librerias
library(readxl)
library(ggplot2)
library(ggthemes)

## Cargar datos

temp = tempfile(fileext = ".xlsx")
dataURL <- "https://github.com/naimmanriquez/LAE_estadistica_2022/blob/main/ejercicio6/02_datos/ejercicio6a.xlsx?raw=true"
download.file(dataURL, destfile=temp, mode='wb')
ejercicio6a <- readxl::read_excel(temp, sheet =1)

## Parte 1

## El gerente de un banco est� interesado en reducir el tiempo que las personas 
## esperan para ver a su asesor financiero. Tambi�n le interesa la relaci�n 
## entre el tiempo de espera (Y) en minutos y el n�mero de asesores atendiendo (X). 
## Se registraron los siguientes datos:

head(ejercicio6a)


## a) Calculen el coeficiente de correlaci�n.

cor(ejercicio6a$x, ejercicio6a$y)

## b) Interpreta tus resultados.

## Hay una correlaci�n negativa: entre mas asesores, menos minutos de espera

ggplot(data = ejercicio6a, aes(x=x, 
                               y=y)) +
  geom_point(size=3) +
  geom_smooth(aes(x = x, y = y)) +
  labs(x = "N�mero de asesores atendiendo",
       y = "Minutos de espera") + theme_get()

## c) Calcula la media, varianza y desviaci�n est�ndar de cada variable 
## e interpreta tus resultados.

summary(ejercicio6a$x)
var(ejercicio6a$x)
sd(ejercicio6a$x)

summary(ejercicio6a$y)
var(ejercicio6a$y)
sd(ejercicio6a$y)

rm(list=ls())

### PARTE 2

## Cargar datos

temp = tempfile(fileext = ".xlsx")
dataURL <- "https://github.com/naimmanriquez/LAE_estadistica_2022/blob/main/ejercicio6/02_datos/ejercicio6b.xlsx?raw=true"
download.file(dataURL, destfile=temp, mode='wb')
ejercicio6b <- readxl::read_excel(temp, sheet =1)

## Una empresa refresquera est� estudiando el efecto de su �ltima 
## campa�a publicitaria. Se eligieron personas al azar y se les llam� 
## para preguntarles cuantas latas de su refresco hab�an comprado 
## la semana anterior y cu�ntos anuncios de su refresco hab�an 
## le�do o visto durante el periodo. Los datos se presentan a continuaci�n:

head(ejercicio6b)

## a) Determina el coeficiente de correlaci�n.

cor(ejercicio6b$x, ejercicio6b$y)

## b) Interpreta los resultados del inciso a).

## c) �Est�s de acuerdo con el planteamiento de las variables del problema? 
## �Qu� suceder�a con la interpretaci�n si X es el n�mero de latas compradas 
## y Y el n�mero de anuncios? 
## �Tendr�a sentido este �ltimo planteamiento?, �s� o no?, �por qu�?

## d) Realiza un gr�fico de dispersi�n para el problema inicial 
## e interpreta el mismo de manera detallada.

ggplot(data = ejercicio6b, aes(x=x, 
                               y=y)) +
  geom_point(size=3) +
  ggtitle("Gr�fico variables x y") +
  ylab("Latas") +
  geom_smooth(aes(x = x, y = y)) +
  xlab("Anuncios") + theme_get() 

rm(list=ls())

### PARTE 3

## Cargar datos

temp = tempfile(fileext = ".xlsx")
dataURL <- "https://github.com/naimmanriquez/LAE_estadistica_2022/blob/main/ejercicio6/02_datos/ejercicio6c.xlsx?raw=true"
download.file(dataURL, destfile=temp, mode='wb')
ejercicio6c <- readxl::read_excel(temp, sheet =1)

## El siguiente conjunto de datos son las ventas semanales de un art�culo 
## de comida (en miles). Determinen el coeficiente de autocorrelaci�n 
## y prueben la hip�tesis de que
## Hip�tesis nula: no hay aytocorrelacion
## Hip�tesis alternativa: hay autocorrelacion

## Utilicen alpha = 0.05 y un alpha = 0.01.

## a) Compara ambos resultados y realiza una conclusi�n de los mismos.
## b) �Es relevante emplear esta serie de tiempo para realizar pron�sticos?

head(ejercicio6c)

ts <- ts(ejercicio6c, frequency=52, start=c(2020,1))

plot(ts)

print(ts)

autocorrelacion<-acf(ts, type ="correlation", plot = FALSE)

plot(autocorrelacion)

Box.test(ts, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 1)


# Pronostico
library(forecast)
modelo<-auto.arima(ejercicio6c[,1])
summary(modelo)
pronostico<- forecast(modelo,1,level=95)
plot(pronostico)

matriz.pronosticos <-data.frame(pronostico$mean,pronostico$lower,pronostico$upper)
matriz.pronosticos
