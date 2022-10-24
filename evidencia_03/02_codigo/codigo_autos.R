## Codigo ##
# 1. Removemos notacion cientifica
options(scipen = 999)

## Librerias
library(tidyverse) 
library(caret)
library(corrplot)
library(ggplot2)
library(ggthemes)
library(lmtest)
library(GGally)
library(olsrr)
library(RColorBrewer)
library(viridis)

data <- readRDS("data.rds")

## Estadistica descriptiva 
summary(data$tax)
## Verificar si tenemos valores perdidos
data %>% summarise(across(tax,~ sum(is.na(.))))
# Quitar una variable
data <- data %>% select(-tax2) 
# Quitar missing values
data <- na.omit(data) 
# Verificamos que no tenemos perdidos
data %>% summarise(across(tax,~ sum(is.na(.))))


## Estadistica descriptiva de la base de datos

round(prop.table(table(data$brand, data$fuelType), 1), 2)
round(prop.table(table(data$transmission, data$brand), 1), 2)

## Audi

audi <- data %>%    
  filter (brand == "audi")

## Precio promedio por marca
audi %>% 
  ggplot() + geom_boxplot(aes(x=brand, y=price)) + 
  ggtitle("Precio promedio para audi")

summary(audi$price)

## Analisis exploratorio de datos

## Relacion entre precio y año
data %>% mutate(Year = as.factor(year)) %>% ggplot() + 
  geom_boxplot(aes(x=Year, y=price)) + 
  ggtitle("Precio promedio por año")

## Numero de autos en nuestra base de datos

data %>% count(year) %>% ggplot() + 
  geom_area(aes(x=year, y=n)) + 
  geom_point(aes(x=year, y=n)) + 
  geom_text(aes(x=year, y=n, label=n), hjust=1, vjust=0) +
  geom_line(data=data %>% count(year, fuelType), aes(x=year, y=n, color=fuelType)) +
  geom_point(data=data %>% count(year, fuelType), aes(x=year, y=n, color=fuelType)) +
  ggtitle("Número de autos por año. El area en negro es el total.")

## Precio por año por marca
data %>% mutate(Year = as.factor(year)) %>% 
  ggplot() + 
  geom_boxplot(aes(x=Year, y=price, color=brand)) + 
  ggtitle("Precio por año por marca")

## Tabla de datos por marca
table(data$brand)

## Precio por marca y por tipo
data %>% select(brand, price, transmission) %>% 
  ggplot() + geom_boxplot(aes(x=brand, y=price, color=transmission)) + 
  ggtitle("Precio por marca y transmision") + theme_minimal()

## Precio por marca y combustible
data %>% select(fuelType, brand, price) %>% 
  ggplot() + geom_boxplot(aes(x=brand, y=price, color=fuelType)) + 
  ggtitle("Precio por marca y por combustible")

## Precio promedio por marca
data %>% 
  ggplot() + geom_boxplot(aes(x=brand, y=price)) + 
  ggtitle("Precio promedio por marca")

## Precio por tipo de combustible
data %>% ggplot() + 
  geom_boxplot(aes(x=fuelType, y=price)) + 
  ggtitle("Precio promedio por tipo de combustible")

data %>% ggplot() + geom_boxplot(aes(x=transmission, y=price)) + 
  ggtitle("Precio por transmision")

data %>% select(where(is.numeric), -year) %>% 
  gather(variable, value) %>% 
  ggplot() + geom_histogram(aes(x=value), bins=30) + 
  facet_wrap(~ variable, scales="free") + 
  ggtitle("Histograma")

data %>% select(where(is.numeric)) %>% plot()

options(repr.plot.width=10, repr.plot.height=8)

corrplot(data %>% select(where(is.numeric)) %>% cor(), method="number")

data_p <- data %>% group_by(model, transmission) %>% 
  summarize(mean=mean(price), count=length(price)) %>% 
  ungroup() %>% arrange(desc(mean)) %>% 
  inner_join(data, by=c("model", "transmission")) %>% 
  mutate(diff=price-mean)

data_p

data %>% filter(model=="A Class") %>% 
  ggplot(aes(x=mileage, y=price, color=transmission)) + 
  geom_point() + geom_smooth()

data_p <- data_p %>% mutate(price = ifelse(abs(diff) > mean, mean, price))

data_p %>% filter(model=="A Class") %>% 
  ggplot(aes(x=mileage, y=price, color=transmission)) + 
  geom_point() + geom_smooth()

preprocess <- preProcess(data_p %>% select(-model, -diff, -mean, -count), method = c("range"))

centered_data <- preprocess %>% predict(data_p) %>% select(-model, -diff, -mean, -count)

linreg <- lm(price~., data=centered_data )
summary(linreg)

options(repr.plot.width=20, repr.plot.height=8)
qqnorm(resid(linreg))
qqline(resid(linreg), col="steelblue")

hist(resid(linreg), breaks=100)

centered_data %>% add_column(res=resid(linreg)) %>% select(where(is.factor), res)  %>% mutate(across(-res, as.character)) %>% gather(key, value, -res) %>%
  ggplot() + geom_boxplot(aes(x=value, y=res)) + facet_wrap(~key, scales="free")

centered_data %>% add_column(res=resid(linreg)) %>% select(where(is.numeric))  %>% gather(key, value, -res) %>% ggplot() + 
  geom_point(aes(x=value, y=res)) + facet_wrap(~ key, scales="free")

data %>% add_column(res=resid(linreg)) %>% arrange(desc(res))

cooksd <- cooks.distance(linreg)
centered_data %>% add_column(cooks=cooksd, res=resid(linreg)) %>% ggplot(aes(x=res, y=cooks)) + geom_point() + ggtitle("Cooks Distance")

data %>% add_column(cooks=cooksd, res=resid(linreg)) %>% arrange(desc(cooks))

control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 10,
)

lg <- train(
  price ~ .,
  data=centered_data,
  trControl=control,
  method="lm",
)

lg
