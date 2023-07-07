#instalación y carga de librerías

install.packages("tidyverse")
install.packages("lubridate")
install.packages("hms")
library(tidyverse)
library(lubridate)
library(hms)
library(RColorBrewer)

#PRIMER DATASET: dailyActivity_merged.csv

read.csv("dailyActivity_merged.csv")
glimpse(read.csv("dailyActivity_merged.csv"))
class(read.csv("dailyActivity_merged.csv")$ActivityDate) #las fechas están formateadas como caracteres y los ids como números

Actividad_D <- read.csv("dailyActivity_merged.csv") #Cargamos el set a una variable
str(Actividad_D) #Observamos la estructura

Actividad_D <- Actividad_D %>% 
  mutate(ActivityDate = mdy(ActivityDate)) #Cambiamos el formato de la variable ActivityDate a fecha

Actividad_D <- Actividad_D %>% 
  mutate(Id = factor(Id)) #Cambiamos el formato de la variable Id a factor para categorizar los identificadores de los usuarios
  str(Actividad_D) # corroboramos la estructura del set de datos
  
  Actividad_D_lite <- Actividad_D %>% 
  select(Id, ActivityDate, TotalDistance, Calories) #hacemos un data frame reducido a partir de lo anterior

Actividad_D_lite <- Actividad_D_lite %>% 
  rename(Fecha = ActivityDate, Distancia = TotalDistance, Calorias = Calories) 

Actividad_D_lite <- Actividad_D_lite %>% 
  arrange(Id, Fecha) #ordenamos los datos por Id y por Fecha

unique(Actividad_D_lite$Id) # Contamos la cantidad de usuarios en los datos
unique(Actividad_D_lite$Fecha) # Contamos la cantidad de fechas en los días

summary(Actividad_D_lite$Distancia) # Obtenemos las medidas descriptivas para la distancia
summary(Actividad_D_lite$Calorias) # Obtenemos las medidas descriptivas para las calorías


#graficamos el consumo calórico de los usuarios a lo largo del mes de estudio

ggplot(Actividad_D_lite, aes(x=Fecha,y=Calorias,group=Id,color=Id)) +
  geom_line(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle=45,vjust=0.5,hjust=0.5)) +
  labs(title = "Consumo calórico de los usuarios a lo largo de un mes de uso")

#graficamos el consumo calórico de cada usuario individualmente a lo largo del mes de estudio

ggplot(Actividad_D_lite,aes(x=Fecha,y=Calorias,group=Id,color=Id)) +
  geom_line(show.legend = FALSE) +
  theme(axis.text.x = element_blank()) +
  facet_wrap(~ Id) +
  labs(title = "Consumo calórico individual de cada usuario a lo largo de un mes de uso")

#graficamos la distancia caminada de los usuarios a lo largo del mes de estudio

ggplot(Actividad_D_lite, aes(x=Fecha,y=Distancia,group=Id,color=Id)) +
  geom_line(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle=60,vjust=0.5,hjust=0.5)) +
  labs(title="Distancia caminada por los usuarios a lo largo de un mes de uso")
  

#graficamos la distancia caminada de cada usuario individualmente a lo largo del mes de estudio

ggplot(Actividad_D_lite,aes(x=Fecha,y=Distancia,group=Id,color=Id)) +
  geom_line(show.legend = FALSE) +
  theme(axis.text.x = element_blank()) +
  facet_wrap(~ Id) +
  labs(title="Distancia caminada por cada usuario individual a lo largo de un mes de uso")


#graficamos la relación entre consumo calórico y distancia caminada

ggplot(Actividad_D_lite,aes(x=Distancia,y=Calorias)) +
  geom_point(aes(color = Calorias)) +
  scale_color_viridis_c(option = "inferno") +
  geom_smooth(method = lm)



#SEGUNDO DATASET: hourlyCalories_merged.csv

read.csv("hourlyCalories_merged.csv")
str(read.csv("hourlyCalories_merged.csv")) #encontramos la misma limitación. los Ids se encuentran en formato numero y las fechas como chr

Calorias_H_D <- read.csv("hourlyCalories_merged.csv") #cargamos la variable en el entorno de R
str(Calorias_H_D)

Calorias_H_D <- Calorias_H_D %>% 
  mutate(ActivityHour = mdy_hms(ActivityHour),
         time=as_hms(ActivityHour),
         date=as_date(ActivityHour),
         wday=wday(ActivityHour)) %>% 
  mutate(Id = factor(Id),wday=factor(wday),wday=recode(wday,
                                                       "1" = "Domingo",
                                                       "2" = "Lunes",
                                                       "3" = "Martes",
                                                       "4" = "Miercoles",
                                                       "5" = "Jueves",
                                                       "6" = "Viernes",
                                                       "7" = "Sabado"))

# formateamos el id como factor y la fecha en multiples columnas


Calorias_H <- Calorias_H_D %>% 
  select(Id, Calories, time) #seleccionamos los datos para obtener consumo calorico versus hora del día.

Calorias_H <- Calorias_H %>% 
  rename(Calorias=Calories,Hora=time) #renombramos las columnas en castellano

Calorias_D <- Calorias_H_D %>% 
  select(Id, Calories, wday) %>% 
  arrange(wday) #seleccionamos los datos para analizar consumo calórico versus día de la semana

Calorias_D <- Calorias_D %>% 
  rename(Calorias = Calories, Dia_semana=wday) #renombramos las columnas en castellano


#en el siguiente comando obtenemos la suma de todas aquellas quemas calóricas que se dieron a la misma hora

Calorias_H_lite <- aggregate(Calorias_H$Calorias, by=list(Hora=Calorias_H$Hora), FUN=sum)

#Renombramos la tabla para mejor lectura

Calorias_H_lite <- Calorias_H_lite %>% 
  rename(Calorias_totales=x)

#luego hacemos algo similar para las quemas calóricas que ocurrieron el mismo día de la semana

Calorias_D_lite <- aggregate(Calorias_D$Calorias, by=list(Dia_de_la_semana=Calorias_D$Dia_semana), FUN=sum)

#renombramos las columnas como en el data set anterior

Calorias_D_lite <- Calorias_D_lite %>% 
  rename(Calorias_totales=x)

#GRAFICAS

#grafica del consumo calórico versus hora del día
ggplot(Calorias_H_lite,aes(x=Hora,y=Calorias_totales,fill=Calorias_totales)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Consumo calórico de los usuarios a lo largo del día") +
  scale_fill_gradient(low = "yellow",high="red")


#grafica del consumo calórico versus día de la semana
ggplot(Calorias_D_lite,aes(x=Dia_de_la_semana,y=Calorias_totales,fill=Calorias_totales)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Consumo calórico de los usuarios a lo largo de la semana") +
  scale_fill_gradient(low = "yellow",high="red")


#Pasos por hora


Pasos_H_D <- read.csv("hourlySteps_merged.csv")
str(Pasos_H_D)

Pasos_H_D <- Pasos_H_D %>% 
  mutate(ActivityHour = mdy_hms(ActivityHour),
         time=as_hms(ActivityHour),
         date=as_date(ActivityHour),
         wday=wday(ActivityHour)) %>% 
  mutate(Id = factor(Id),wday=factor(wday),wday=recode(wday,
                                                       "1" = "Domingo",
                                                       "2" = "Lunes",
                                                       "3" = "Martes",
                                                       "4" = "Miercoles",
                                                       "5" = "Jueves",
                                                       "6" = "Viernes",
                                                       "7" = "Sabado"))
str(Pasos_H_D)
