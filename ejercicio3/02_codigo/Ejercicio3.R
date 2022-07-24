
### Ejemplos para el apartado 1a del ejercicio 3

##### A

## P(Z > 2)

pnorm(2, mean = 0, sd = 1, lower.tail = F)

# La razón es que si nos vamos a las tablas de Z en el 2 encontramos
## 0.9772, si restamos 1 - 0.9772 nos queda 0.0228


##### B

## P(-2 < Z < 2).

pnorm(c(2), mean = 0, sd = 1) - pnorm(c(-2), mean = 0, sd = 1)


##### C

## P(0 < Z < 1.73).

pnorm(c(1.73), mean = 0, sd = 1) - pnorm(c(0), mean = 0, sd = 1)


### P(Z = 1.17)

pnorm(1.17, mean = 0, sd = 1, lower.tail = T)


### EJERCICIO 3 APARTADO 1A
### A
## P (Z <= 1.17)


### B
## P (0 <= Z <= 1.17)


### C
## P(Z >= 1.17)


### D
## P(Z >= -1.17)


### Para el apartado 2 del ejercicio 3 se necesita la libreria tigerstats

install.packages("tigerstats")

library(tigerstats)

## P(Z = 1.17) 

pnormGC(1.17, region="above", mean=0, sd=1,graph=TRUE)

## P(Z = -1.17) 

pnormGC(-1.17, region="below", mean=0, sd=1,graph=TRUE)


## EJERCICIO 3 APARTADO 2
## A
## P(Z = 1.17)
pnormGC(1.17, region="above", mean=0, sd=1,graph=TRUE)


## B
## P(Z = -1.46) 
pnormGC(-1.46, region="below", mean=0, sd=1,graph=TRUE)



## C
## P(Z = 1.04)
pnormGC(1.04, region="above", mean=0, sd=1,graph=TRUE)


## D
## P(Z = 2.66) 

