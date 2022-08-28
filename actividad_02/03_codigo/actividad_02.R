# Remover notacion cientifica
options(scipen = 999)
# Cargar librerias
library(readxl)
library(ggplot2)
library(tidyverse)
library(sjmisc)

# Cargar datos
actividad_dos <- read_excel("actividad_dos.xlsx")

# Calcula la media, desviacion estandar, maximo y minimo.
## Media
tapply(actividad_dos$calificacion, actividad_dos$grupo, mean)
## desviacion estandar
tapply(actividad_dos$calificacion, actividad_dos$grupo, sd)
## Minimo
tapply(actividad_dos$calificacion, actividad_dos$grupo, min)
# Maximo
tapply(actividad_dos$calificacion, actividad_dos$grupo, max)


# ¿Cual de los dos grupos tiene mejor promedio?, ¿por que?
# Boxplot
ggplot(actividad_dos, aes(y = calificacion)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.15,
               color = 1) +  # Color barras 
  geom_boxplot(fill = 3,           # Color caja
               alpha = 0.5,        # Transparencia
               color = 1,          # Color del borde
               outlier.colour = 2) + # Color atípicos
  facet_wrap(~ grupo) +
  labs(title = 'Calificaciones de los grupos',
       y = 'Calificacion',
       subtitle = 'Diagrama de cajas',
       caption = 'Fuente: Actividad 2, Canvas - Tecmilenio')

# ¿Cuál de los dos grupos tiene la mayor desviación estándar?, ¿por qué?
# Distribucion
# Graficos de distribucion
## Hiatograma con distribucion
ggplot(actividad_dos, aes(x = calificacion)) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white") +
  facet_wrap(~ grupo) +
  stat_function(fun = dnorm, args = list(mean = mean(actividad_dos$calificacion), sd = sd(actividad_dos$calificacion))) +
  labs(title = 'Calificaciones de los grupos',
       y = 'Calificacion',
       subtitle = 'Curvas de distribución',
       caption = 'Fuente: Actividad 2, Canvas - Tecmilenio')


## Filtrar grupos
grupo_uno <- actividad_dos %>% 
  filter(grupo == "1")

grupo_dos <- actividad_dos %>% 
  filter(grupo == "2")

# ¿En que intervalo de notas se encuentra la mayor poblacion del grupo uno?
# ¿En que intervalo de notas se encuentra la mayor poblacion del grupo dos?
# Tabla de frecuencias
grupo_uno %>% frq(calificacion)
grupo_dos %>% frq(calificacion)
# Intervalos
table(cut(actividad_dos$calificacion, breaks=seq(10,100,10)), actividad_dos$grupo)

# ¿En que grupo crees que los alumnos esten aprendiendo de forma mas eficiente y por que?
