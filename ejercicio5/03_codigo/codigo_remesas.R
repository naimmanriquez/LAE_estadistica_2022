#### cargar la base de datos
## Removemos notacion cientifica
options(scipen = 999)
library(readxl)
library(ggplot2)

# Grafica
ggplot(data=remesas, aes(x=Periodo, y=Mazatlan, group=1)) +
  geom_line()+
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
