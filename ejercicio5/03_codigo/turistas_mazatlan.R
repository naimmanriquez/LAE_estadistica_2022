# Cargar librerias
library(tidyverse)
library(ggplot2)

# Remover notacion cientifica
options(scipen = 999)

# Grafica
ggplot(data=turistas_mazatlan, aes(x=fecha, y=turistas, group=1)) +
  geom_line()+
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
