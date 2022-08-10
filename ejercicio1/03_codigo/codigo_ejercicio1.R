## Instalar librerias
# install.packages("readxl")
# install.packages("sjmisc")
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("ggsoccer")

## Cargar librerias
library(readxl) #Herramienta para leer archivos de excel
library(sjmisc) #Herramienta para hacer tablas de frecuencias
library(tidyverse) #Herramienta para manipular datos
library(ggplot2) #Herramienta para hacer graficas
library(ggsoccer) #Herramienta para hacer analisis de futbol

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

## Gráfico de la cancha
ggplot() +
  annotate_pitch(colour = "white",
                 fill = "#3ab54a") +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "#3ab54a"))


# Boxplot
ggplot(bd, aes(y = Edad)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.15,
               color = 1) +  # Color barras 
  geom_boxplot(fill = 3,           # Color caja
               alpha = 0.5,        # Transparencia
               color = 1,          # Color del borde
               outlier.colour = 2) + # Color atípicos
  labs(title = 'Edades de los jugadores del primer equipo: Tigres UANL',
       y = 'Edad',
       subtitle = 'Diagrama de cajas',
       caption = 'Fuente: Plantilla del Torneo Apertura 2022')



# Histograma
hist(bd$Edad, prob = TRUE,
     col = "white",
     main = "")

# Nuevo gráfico
par(new = TRUE)

# Box plot
boxplot(bd$Edad, horizontal = TRUE, axes = FALSE,
        col = rgb(0, 0.8, 1, alpha = 0.5))

# Caja
box()


