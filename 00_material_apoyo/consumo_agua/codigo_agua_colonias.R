# Removemos notacion cientifica
options(scipen = 999)

# Cargamos librerias
library(ggplot2)
library(tidyverse)
library(readxl)

# Cargar datos
consumo_total_bd <- read.csv("consumo_totales.csv")

# Primeras graficas: total colonias
consumos_total_wcol <- consumo_total_bd %>%
  group_by(fecha) %>%
  summarise(consumo = sum(Freq))

ggplot(data=consumos_total_wcol, aes(x=fecha, y=consumo, group=1)) +
  geom_point(aes(x = fecha, y = consumo)) +
  geom_line()+
  geom_smooth(aes(x = fecha, y = consumo)) +
  theme_get() +
  labs(title = "",
       y = "Consumo total (m3)",
       x = "Fecha") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## Zona central y oriente
zonas_central_oriente <- consumo_total_bd %>% 
  filter(zona == "Central" | zona == "Oriente")

ggplot(data=zonas_central_oriente, aes(x=fecha, y=Freq, group=1)) +
  geom_point(aes(x = fecha, y = Freq)) +
  geom_line()+
  geom_smooth(aes(x = fecha, y = Freq)) +
  facet_wrap(~ zona) +
  theme_get() +
  labs(title = "",
       y = "Consumo total (m3)",
       x = "Fecha") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## Zona centro y norte
zonas_centro_norte <- consumo_total_bd %>% 
  filter(zona == "Centro" | zona == "Norte")

ggplot(data=zonas_centro_norte, aes(x=fecha, y=Freq, group=1)) +
  geom_point(aes(x = fecha, y = Freq)) +
  geom_line()+
  geom_smooth(aes(x = fecha, y = Freq)) +
  facet_wrap(~ zona) +
  theme_get() +
  labs(title = "",
       y = "Consumo total (m3)",
       x = "Fecha") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## Zona cerritos y dorada
zonas_cerritos_dorada <- consumo_total_bd %>% 
  filter(zona == "Cerritos" | zona == "Dorada")

ggplot(data=zonas_cerritos_dorada, aes(x=fecha, y=Freq, group=1)) +
  geom_point(aes(x = fecha, y = Freq)) +
  geom_line()+
  geom_smooth(aes(x = fecha, y = Freq)) +
  facet_wrap(~ zona) +
  theme_get() +
  labs(title = "",
       y = "Consumo total (m3)",
       x = "Fecha") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Zona turistica
zonas_turistica <- consumo_total_bd %>% 
  filter(zona == "Turistica Residencial")

ggplot(data=zonas_turistica, aes(x=fecha, y=Freq, group=1)) +
  geom_point(aes(x = fecha, y = Freq)) +
  geom_line()+
  geom_smooth(aes(x = fecha, y = Freq)) +
  facet_wrap(~ zona) +
  theme_get() +
  labs(title = "",
       y = "Consumo total (m3)",
       x = "Fecha") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Zona Sur
zona_sur <- consumo_total_bd %>% 
  filter(zona == "Sur")

ggplot(data=zona_sur, aes(x=fecha, y=Freq, group=1)) +
  geom_point(aes(x = fecha, y = Freq)) +
  geom_line()+
  geom_smooth(aes(x = fecha, y = Freq)) +
  facet_wrap(~ zona) +
  theme_get() +
  labs(title = "",
       y = "Consumo total (m3)",
       x = "Fecha") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## Estadisticos descriptivos
tapply(consumo_total_bd$Freq, consumo_total_bd$zona, summary)
tapply(consumo_total_bd$Freq, consumo_total_bd$zona, sd)

