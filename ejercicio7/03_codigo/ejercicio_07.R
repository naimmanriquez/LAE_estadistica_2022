## Removemos notacion cientifica
options(scipen = 999)

## Cargar librerias
library(readxl)
library(ggplot2)
library(ggthemes)
library(forecast)

## Cargar datos

temp = tempfile(fileext = ".xlsx")
dataURL <- "https://github.com/naimmanriquez/LAE_estadistica_2022/blob/main/ejercicio7/02_datos/ejercicio7a.xlsx?raw=true"
download.file(dataURL, destfile=temp, mode='wb')
ejercicio7a <- readxl::read_excel(temp, sheet =1)

# PRIMER PROBLEMA

## Los datos de la demanda anual de bolsas de fertilizante de una empresa agrícola se muestran en la siguiente tabla.

head(ejercicio7a)

ejerciciotimeseries <- ts(ejercicio7a$demanda)

ejerciciotimeseries

## Grafica la serie de tiempo. 

plot(ejerciciotimeseries)

## Encuentra el valor de pronóstico para la demanda de fertilizante 
## para cada año, comenzando por el año 4 por medio de un promedio móvil 
## de k=3 años y realiza el pronóstico para el año 12. 

moving_average = forecast(ma(ejerciciotimeseries[1:11], order=3), h=2)

plot(moving_average)

matriz.pronosticos <-data.frame(moving_average$mean,moving_average$lower,moving_average$upper)

matriz.pronosticos

rm(list = ls())

### PROBLEMA 2

## Aplica el suavizamiento exponencial y un valor inicial 
## de 38 para realizar lo que se solicita en los incisos del ejercicio.

## Cargar datos

temp = tempfile(fileext = ".xlsx")
dataURL <- "https://github.com/naimmanriquez/LAE_estadistica_2022/blob/main/ejercicio7/02_datos/ejercicio7b.xlsx?raw=true"
download.file(dataURL, destfile=temp, mode='wb')
ejercicio7b <- readxl::read_excel(temp, sheet =1)


ejerciciotimeseries <- ts(ejercicio7b$yt)

ejerciciotimeseries

## Grafica la serie de tiempo. 

plot(ejerciciotimeseries)

## Encuentra el valor de pronóstico para cada periodo t

fcast_ses <- ses(ejerciciotimeseries, h = 4)

plot(fcast_ses)

summary(fcast_ses)

rm(list = ls())

### PROBLEMA 3

## Las ventas de equipos de cocina han aumentado durante los últimos cinco años. 
## El gerente había pronosticado, antes de iniciar el negocio, 
## que las ventas del primer año serían de 360 equipos de cocina. 

## Cargar datos

temp = tempfile(fileext = ".xlsx")
dataURL <- "https://github.com/naimmanriquez/LAE_estadistica_2022/blob/main/ejercicio7/02_datos/ejercicio7c.xlsx?raw=true"
download.file(dataURL, destfile=temp, mode='wb')
ejercicio7c <- readxl::read_excel(temp, sheet =1)

ejerciciotimeseries <- ts(ejercicio7c$ventas)

ejerciciotimeseries

## Grafica la serie de tiempo.

autoplot(ejerciciotimeseries, xlab='Tiempo', ylab='Ventas') 

fcast_ses <- ses(ejerciciotimeseries, h = 3)

plot(fcast_ses)

summary(fcast_ses)