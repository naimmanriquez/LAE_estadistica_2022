# Cargar librerias
library(tidyverse)
library(ggplot2)

turistas_rosario$meses <- NA
turistas_rosario$meses[turistas_rosario$mes == "enero"] <- "01"
turistas_rosario$meses[turistas_rosario$mes == "febrero"] <- "02"
turistas_rosario$meses[turistas_rosario$mes == "marzo"] <- "03"
turistas_rosario$meses[turistas_rosario$mes == "abril"] <- "04"
turistas_rosario$meses[turistas_rosario$mes == "mayo"] <- "05"
turistas_rosario$meses[turistas_rosario$mes == "junio"] <- "06"
turistas_rosario$meses[turistas_rosario$mes == "julio"] <- "07"
turistas_rosario$meses[turistas_rosario$mes == "agosto"] <- "08"
turistas_rosario$meses[turistas_rosario$mes == "septiembre"] <- "09"
turistas_rosario$meses[turistas_rosario$mes == "octubre"] <- "10"
turistas_rosario$meses[turistas_rosario$mes == "noviembre"] <- "11"
turistas_rosario$meses[turistas_rosario$mes == "diciembre"] <- "12"

# Combinar anio y mes
turistas_rosario$fecha <- paste(turistas_rosario$anio, turistas_rosario$meses, sep = "-")

# Convertir a numerico 
turistas_rosario$cantidad <- as.numeric(as.character(turistas_rosario$cantidad))
# Convertir a numerico 
turistas_rosario$cuartos_ocupados <- as.numeric(as.character(turistas_rosario$cuartos_ocupados))

# Grafica
ggplot(data=turistas_rosario, aes(x=fecha, y=cantidad, group=1)) +
  geom_line()+
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Grafica
ggplot(data=turistas_rosario, aes(x=fecha, y=cuartos_ocupados, group=1)) +
  geom_line()+
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Grafica
ggplot(data=turistas_rosario, aes(x=fecha, y=cuartos_ocupados, group=1)) +
  geom_line()+
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# ggplot(data=turistas_rosario) + geom_line(aes(x=fecha, y=cuartos_ocupados, group=1),color='red') + 
#  geom_line(aes(x=fecha, y=cuartos_disponibles, group=1),color='blue') + 
#  ylab('Cuartos disponibles y ocupados')+xlab('Fecha') +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
