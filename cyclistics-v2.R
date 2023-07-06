#instalación y carga de librerías

install.packages("tidyverse")
install.packages("lubridate")
library(tidyverse)
library(lubridate)


#Primer dataset: dailyActivity_merged.csv

read.csv("dailyActivity_merged.csv")
glimpse(read.csv("dailyActivity_merged.csv"))
class(read.csv("dailyActivity_merged.csv")$ActivityDate) #las fechas están formateadas como caracteres y los ids como números

d_Activity <- read.csv("dailyActivity_merged.csv") #Cargamos el set a una variable
str(d_Activity) #Observamos la estructura

d_Activity <- d_Activity %>% 
  mutate(ActivityDate = mdy(ActivityDate)) #Cambiamos el formato de la variable ActivityDate a fecha

d_Activity <- d_Activity %>% 
  mutate(Id = factor(Id)) #Cambiamos el formato de la variable Id a factor para categorizar los identificadores de los usuarios
  str(d_Activity) # corroboramos la estructura del set de datos