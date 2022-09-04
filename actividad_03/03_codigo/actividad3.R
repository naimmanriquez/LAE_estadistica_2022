## ACTIVIDAD 3 Opcion 1 ##
options(scipen = 999)

# Cargar librerias
library(readxl)
library(ggplot2)
library(ggpubr)

## EJERCICIO 1 ##

## Define lo que significan los siguientes términos:
## Correlación
## Autocorrelación
## Promedio móvil
## Suavizamiento exponencial


## EJERCICIO 2 ##

## Busca información de 20 casas en venta en donde las variables sean
## Y (metros de construcción) y X (metros de terreno); 
## lleva a cabo lo que se indica:

# Cargar datos
temp = tempfile(fileext = ".xlsx")
dataURL <- "https://github.com/naimmanriquez/LAE_estadistica_2022/blob/main/actividad_03/02_datos/aguascalientes.xlsx?raw=true"
download.file(dataURL, destfile=temp, mode='wb')
aguascalientes <- readxl::read_excel(temp, sheet =1)


## Realiza y describe el diagrama de dispersión

ggplot(data = aguascalientes, aes(x=MT2, 
                                  y=MC2)) +
  geom_point(size=3) +
  geom_smooth(aes(x = MT2, y = MC2)) +
  ggtitle("Gráfico entre metros de construcción y terreno") +
  labs(x = "Metros de terreno",
       y = "Metros de construcción") +
  theme_bw()

## Calcula e interpreta el coeficiente de correlación muestral r.

cor(aguascalientes$MC2, aguascalientes$MT2)    

ggscatter(aguascalientes, x = "MT2", y = "MC2",
          color = "black", shape = 21, size = 3,
          add = "reg.line",  
          add.params = list(color = "blue", fill = "lightgray"),
          conf.int = TRUE, 
          cor.coef = TRUE, 
          cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n"))

## Responde a la siguiente cuestión en un terreno urbano. 
## ¿A mayor cantidad en metros de construcción, mayor es el precio de la vivienda?

ggplot(data=aguascalientes, aes(MC2, PrecioMXN)) + geom_point() + stat_smooth() +
  ggtitle("Correlacion entre metros de construcción y precio") +
  ylab("Precio") +
  xlab("MC2") + 
  theme(plot.title = element_text(hjust = 0.5))


ggscatter(aguascalientes, x = "MC2", y = "PrecioMXN",
          color = "black", shape = 21, size = 3,
          add = "reg.line",  
          add.params = list(color = "blue", fill = "lightgray"),
          conf.int = TRUE, 
          cor.coef = TRUE, 
          cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n"))

rm(list=ls())

### EJERCICIO 3 ##

## Busca información de los cetes a 28 días- semanal, 
## periodicidad diaria y datos del Banco de México. 
## Considera las últimas 20 cotizaciones de los cetes y realiza lo que se indica:

## Determina el coeficiente de autocorrelación r1
## Determina la prueba de hipótesis de lo siguiente:
## Hipótesis nula: H0 : ??1 = 0 (La autocorrelación es igual a cero).
## Hipótesis alternativa: Ha: ??1??? 0 (La  autocorrelación es diferente de cero).
## Donde pk es el coeficiente de autocorrelación poblacional en el lapso k.
## ¿Existe autocorrelación entre los rendimientos de los CETES  a 28 días?

## EJERCICIO 4 ##

## Las llamadas de emergencia a un teléfono 
## durante las últimas 24 semanas son éstas:

# Cargar datos
temp = tempfile(fileext = ".xlsx")
dataURL <- "https://github.com/naimmanriquez/LAE_estadistica_2022/blob/main/actividad_03/02_datos/llamadas.xlsx?raw=true"
download.file(dataURL, destfile=temp, mode='wb')
llamadas <- readxl::read_excel(temp, sheet =1)

## Realiza y describe un diagrama de dispersión.

ggplot(data = llamadas, aes(x=semana, y=llamadas)) +
  geom_point(size=3) +
  ggtitle("Gráfico de dispersión") +
  labs(x = "Semana",
       y = "Llamadas") +
  theme_bw()

## Determina un promedio móvil con k=3 periodos y pronostica el valor para la semana 25.

library(forecast)

llamadastimeseries <- ts(llamadas$llamadas)

moving_average = forecast(ma(llamadastimeseries[4:24], order=3), h=2)

plot(moving_average)

matriz.pronosticos <-data.frame(moving_average$mean,moving_average$lower,moving_average$upper)
matriz.pronosticos

## ULTIMA PARTE ACTIVIDAD

## Con los conceptos vistos y puestos en práctica, 
## brinda una respuesta justificada a cada una de las siguientes cuestiones

## ¿Qué significa el coeficiente de correlación?
## ¿Cómo se interpreta el coeficiente de correlación? 
## ¿Para qué sirve el coeficiente de autocorrelación?
## ¿Cuándo utilizarías el método de promedios móviles?
## ¿Cómo elegirías la constante suavizamiento en el método de suavización exponencial?