# Probabilidad de un evento
# simulando probabilidad con monedas y con datos
# Espacio muestral de lanzar tres monedas.
# Probabilidad de que salgan tres caras (CCC).

moneda <- c("aguila", "sello")
acumular<-NULL
n <- 3   # numero de veces de lanzar la moneda

acumular <- sample(moneda, n, replace = TRUE)

acumular 

table(acumular)/n  # Sacar sus frecuencias

## La extracción de dos bolas de una urna, que contiene 4 bolas blancas y 3 bolas negras
## La probabilidad de sacar una bola blanca y una negra

## La opción de R para esto es choose:

choose(4, 1) * choose(3, 1) / choose(7, 2)

## Tenemos 7 bolas en total y 2 oportunidades
## Tenemos 4 bolas blancas y buscamos la probabilidad de tener una
## Tenemos 3 bolas negras y buscamos la probabilidad de tener una tambien.

## La probabilidad de sacar dos bolas blancas

## La opción de R para esto es choose:

choose(4, 2) * choose(3, 0) / choose(7, 2)

## Tenemos 7 bolas en total y 2 oportunidades
## Tenemos 4 bolas blancas y buscamos la probabilidad de tener dos
## Tenemos 3 bolas negras.