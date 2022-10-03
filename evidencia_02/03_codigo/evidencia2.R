### CASO 1 ###

## Desarrollo de tres modelos de pronósticos para calcular 
## el número posible de clientes de la CCC para el resto de 1993. 
## Se pueden emplear los métodos de promedio simple,
## promedio móvil y suavizamiento exponencial.

library(readxl)

evidencia2a <- read_excel("evidencia2a.xlsx")

library(forecast)

ts <- ts(evidencia2a$clientes, frequency=12, start=c(1985,1))

plot(ts)

print(ts)

moving_average = forecast(ma(ts[1:99], order=3), h=9)

plot(moving_average)

summary(moving_average)

fcast_ses <- ses(ts, h = 9)

plot(fcast_ses)

summary(fcast_ses)


### CASO 2 ###

## ¿Cuál de los modelos de pronóstico estudiados puede funcionar 
## mejor para las ventas mensuales nacionales? 
## Justifica tu respuesta apoyándola con gráficos 
## y explicando la razón de tu selección del método de pronóstico.

## MODELO ARIMA PARA PRONOSTICO DE VENTAS ##

library(readxl)

evidencia2b <- read_excel("evidencia2b.xlsx")

ventas.ts <- ts(evidencia2b$ventas, start = c(1983,1), frequency = 12)

print(ventas.ts)

#Trazamos la serie de tiempo 

plot(ventas.ts)

library(ggplot2)
library(plotly)
library(ggthemes)
library(scales)

ggplot(data=evidencia2b, aes(Periodo, ventas)) + geom_line() +
  ggtitle("Ventas Murphy Brothers") +
  ylab("Ventas Mensuales)") +
  xlab("Periodo") + 
  theme(plot.title = element_text(hjust = 0.4)) 

autocorrelacion<-acf(ventas.ts, type ="correlation", plot = FALSE)

plot(autocorrelacion)

componentes.ts = decompose(ventas.ts)

plot(componentes.ts)

library(forecast)

ndiffs(ventas.ts)

nsdiffs(ventas.ts)

diff.ventas.ts<-autoplot(diff(ventas.ts), ts.linetype = "dashed", ts.colour = "blue")

diff.ventas.ts

autoplot(acf(diff(ventas.ts), plot = FALSE))

diff.ventasts.12<-diff(ventas.ts, lag = 12)

autoplot(diff.ventasts.12, ts.colour = "darkorange", ts.linetype = "dashed")

library(tseries)

adf<-adf.test(diff.ventasts.12)

adf$p.value

autoplot(acf(diff.ventasts.12, plot = FALSE))

autoplot(pacf(diff.ventasts.12, plot = FALSE))

#Mis modelos
arima1<- arima(ventas.ts,order=c(2,1,0), method = "ML")
arima1$aic

arima2<- arima(ventas.ts,order=c(2,1,1), method = "ML")
arima2$aic

arima3<- arima(ventas.ts,order=c(2,1,2), method = "ML")
arima3$aic

arima4<- arima(ventas.ts,order=c(1,1,2), method = "ML")
arima4$aic

mod_arima <- auto.arima(ventas.ts, seasonal = FALSE)

mod_arima

tsdisplay(residuals(mod_arima), lag.max=12, main='Residuos (0,1,2)')

kpss<-kpss.test(diff.ventasts.12)

kpss$p.value

mod_sarima <- auto.arima(ventas.ts, stepwise = TRUE, approximation = TRUE) #Selección por pasos y estimacion max. verosimilitud

mod_sarima

library(lmtest)

coeftest(mod_sarima)

autoplot(acf(mod_sarima$residuals, plot = FALSE))

autoplot(pacf(mod_sarima$residuals, plot = FALSE))

library(ggfortify)

ggtsdiag(mod_sarima)

independencia <- Box.test(mod_sarima$residuals, type="Ljung-Box") # Test de Ljung-Box
independencia$p.value

qqnorm(mod_sarima$residuals)
qqline(mod_sarima$residuals) 

normalidad <-shapiro.test(mod_sarima$residuals)    # Test de Shapiro-Wilk
normalidad$p.value  

prediccion <- forecast(mod_sarima, h=36) #nivel confianza 95%, h = periodos
autoplot(prediccion)

summary(prediccion)

### CASO 3 ##

library(readxl)

evidencia2c <- read_excel("evidencia2c.xlsx")

library(forecast)

## PREGUNTA A ##

## ¿Qué patrón observó Karin en la gráfica de serie de tiempo 
## de las ventas de Surtido Cookies?

ts <- ts(evidencia2c$mventas, frequency=12, start=c(2002,5))

plot(ts)

print(ts)

## RESPUESTA A:
## Se trata de una serie no estacionaria, parece que hay ademas un efecto
## estacional durante el segundo semestre

## PREGUNTA B ##

## ¿La autocorrelación es congruente con el patrón que se observa en la gráfica?

autocorrelacion<-acf(ts, type ="correlation", plot = FALSE)

plot(autocorrelacion)

### RESPUESTA B:
## Si hay un proceso de autocorrelacion en el segundo periodo, se puede
## comprobar con el correlograma

## PREGUNTA C ##

## Aplica un método de suavización para pronosticar las ventas 
## futuras de galletas y elabora un pronóstico 
## para los meses restantes de 2003.

fcast_ses <- ses(ts, h = 7)

plot(fcast_ses)

summary(fcast_ses)

## PREGUNTA D ##

## Aplica un método de medias móviles para pronosticar las ventas 
## futuras de galletas y elabora un pronóstico 
## para los meses restantes de 2003.

moving_average = forecast(ma(ts[1:13], order=3), h=7)

plot(moving_average)

summary(moving_average)
