## ACTIVIDAD 5 ##

## PROBLEMA 1 ##

## Los tiempos de atención a clientes en las cajas de un supermercado 
## y los valores de las compras están registrados en la siguiente tabla.

library(readxl)

actividad5a <- read_excel("actividad5a.xlsx")

attach(actividad5a)

# Definir variables

Y <- cbind(Y)
X1 <- cbind(X)

## PREGUNTA 1 ##

## Estimen la ecuacion de regresion lineal.

olsreg1 <- lm(Y ~ X1)
summary(olsreg1)

## PREGUNTA 2 ##

## Calculen las predicciones (valores puntuales) para los siguientes valores 
## de X: 3, 4, 5, 6,7.

Y1hat <- fitted(olsreg1)
summary(Y1hat)
plot(Y1hat ~ X)

## PREGUNTA 3 ##

## Obtengan los intervalos de confianza al 99 ##

confint(olsreg1, level=0.99)

## supuetos del modelo ##

## Homocedasticidad ##

library(lmtest)

bptest(olsreg1)

## se observa que el valor-P es mayor que el nivel de significancia usual de 0.05
## por lo tanto, hay evidencias para decir que se cumple la homocedasticidad.

# Residuos
e1hat <- resid(olsreg1)
summary(e1hat)
plot(e1hat ~ X1)

## Se cumple con el supuesto de media cero en los residuos 

## Nota: aqui no se verifica multicolinealidad ya que solo tenemos una variable

rm(list = ls())



## PROBLEMA 2 ##

## A continuacion se presentan la calificacion de una muestra aleatoria 
## de estudiantes de nuevo ingreso a cierta universidad, 
## en la clase de estadistica. Así como sus calificaciones en el examen 
## y las clases perdidas por estudiante.

library(readxl)

actividad5b <- read_excel("actividad5b.xlsx")

attach(actividad5b)

# Definir variables

Y <- cbind(Y)
X1 <- cbind(X1, X2)

## PRIMERA PREGUNTA ##
## Ajuste una ecuacion de regresion lineal multiple

olsreg1 <- lm(Y ~ X1)
summary(olsreg1)

## Matriz de correlaciones ##
cor(Y, X1)

C<-actividad5b[,2:4]

library(GGally)

ggpairs(C)


## SEGUNDA PREGUNTA ##

## Estime la calificaciOn de estadIstica para un estudiante que en la prueba 
## de inteligencia obtuvo 50 de calificación y perdiO 3 clases.

# supuestos del modelo #

## Homocedasticidad ##

library(lmtest)

bptest(olsreg1)

## se observa que el valor-P es mayor que el nivel de significancia usual de 0.05
## por lo tanto, hay evidencia para decir que se cumple la homocedasticidad.

# Residuos
e1hat <- resid(olsreg1)
summary(e1hat)

par(mfrow=c(2, 2))
plot(olsreg1, las=1, col='deepskyblue4', which=1:3)


## Multicolinealidad ##

library(olsrr)

ols_vif_tol(olsreg1)

##  Los VIF superiores a 10 son signos de multicolinealidad 
## grave que requieren corrección.

rm(list = ls())

### PROBLEMA 3 ###

## En una compañía manufacturera de línea blanca se llevan a cabo algunos 
## ensayos experimentales, con un horno para determinar una forma de predecir 
## el tiempo de coccion (y), a diferentes niveles de ancho del horno (x1) 
## y a diferentes temperaturas (x2).

library(readxl)

actividad5c <- read_excel("actividad5c.xlsx")

attach(actividad5c)

# Definir variables

Y <- cbind(Y)
X1 <- cbind(X1, X2)

## PRIMERA PREGUNTA ##
## Estime la ecuación de regresión lineal múltiple.

olsreg1 <- lm(Y ~ X1)
summary(olsreg1)

## Matriz de correlaciones ##
cor(Y, X1)

C<-actividad5c[,1:3]

library(GGally)

ggpairs(C)

## SEGUNDA PREGUNTA ##

## Interpreta los coeficientes individuales de la ecuaciOn de regresión 
## lineal múltiple considerando el contexto del problema.

## TERCERA PREGUNTA ##

## Pronostica el tiempo de cocimiento cuando el nivel del ancho del horno 
## es de 5 pies y la temperatura de cocción es de 25°Centigrados.

## supuestos ##

## Homocedasticidad ##

library(lmtest)

bptest(olsreg1)

## El p valor es de 0.01, esto significa que hay heterocedasticidad

# Residuos
e1hat <- resid(olsreg1)
summary(e1hat)

par(mfrow=c(2, 2))
plot(olsreg1, las=1, col='deepskyblue4', which=1:3)

## Hay valores atipicos

## Multicolinealidad ##

library(olsrr)

ols_vif_tol(olsreg1)
