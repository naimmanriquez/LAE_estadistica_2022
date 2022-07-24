## Instalar librerias
# install.packages("readxl")
# install.packages("sjmisc")
# install.packages("tidyverse")
# install.packages("ggplot2")

## Cargar librerias
library(readxl)
library(sjmisc)
library(tidyverse)
library(ggplot2)

# Cargar datos
bd <- read_excel("jugadores_tigres.xlsx")

# Estadisticos descriptivos
# Media y mediana 
summary(bd$Edad)
# Varianza
var(bd$Edad)
# Desviacion estandar 
sd(bd$Edad)

# Tabla de frecuencias
bd %>% frq(Edad)

# N = Frecuencia absoluta
# Valid = Frecuencia relativa
# Cum = Frecuencia acumulada

# Histograma
ggplot(data = bd,
       mapping = aes(x = Edad)) +
  geom_histogram(bins = 9,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Edades de los jugadores del primer equipo: Tigres UANL',
       x = 'Edad',
       y = 'conteos',
       subtitle = 'Histograma',
       caption = 'Fuente: Plantilla del Torneo Apertura 2022')
