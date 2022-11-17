# Remover notacion cientifica
options(scipen = 999)

# Cargar librerias
library(readxl)

# Cargar datos
caso_practico2_archivo <- ("caso_practico2_archivo.xls")

# Analisis
attach(caso_practico2_archivo)

# Definir variables

Y <- cbind(edad)
X1 <- cbind(peso, altura)

# Regresión lineal 
olsreg1 <- lm(Y ~ X1)
summary(olsreg1)
