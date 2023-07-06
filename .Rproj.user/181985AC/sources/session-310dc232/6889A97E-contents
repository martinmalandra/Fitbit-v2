#instalación y carga de librerías

install.packages("tidyverse")
install.packages("lubridate")
install.packages("hms")
library(tidyverse)
library(lubridate)
library(hms)

#PRIMER DATASET: dailyActivity_merged.csv

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
  
d_Activity_lite <- d_Activity %>% 
  select(Id, ActivityDate, TotalDistance, Calories) #hacemos un data frame reducido a partir de lo anterior

d_Activity_lite <- d_Activity_lite %>% 
  rename(Fecha = ActivityDate, Distancia = TotalDistance, Calorias = Calories) %>% 

d_Activity_lite <- d_Activity_lite %>% 
  arrange(Id,Fecha) #ordenamos los datos por Id y por Fecha

unique(d_Activity_lite$Id) # Contamos la cantidad de usuarios en los datos
unique(d_Activity_lite$Fecha) # Contamos la cantidad de fechas en los días

summary(d_Activity_lite$Distancia) # Obtenemos las medidas descriptivas para la distancia
summary(d_Activity_lite$Calorias) # Obtenemos las medidas descriptivas para las calorías


#graficamos el consumo calórico de los usuarios a lo largo del mes de estudio

ggplot(d_Activity_lite, aes(x=Fecha,y=Calorias,group=Id,color=Id)) +
  geom_line(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle=45,vjust=0.5,hjust=0.5))

#graficamos el consumo calórico de cada usuario individualmente a lo largo del mes de estudio

ggplot(d_Activity_lite,aes(x=Fecha,y=Calorias,group=Id,color=Id)) +
  geom_line(show.legend = FALSE) +
  theme(axis.text.x = element_blank()) +
  facet_wrap(~ Id)

#graficamos la distancia caminada de cada usuario a lo largo del mes de estudio

ggplot(d_Activity_lite, aes(x=Fecha,y=Distancia,group=Id,color=Id)) +
  geom_line(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle=60,vjust=0.5,hjust=0.5))

#graficamos la distancia caminada de cada usuario individualmente a lo largo del mes de estudio

ggplot(d_Activity_lite,aes(x=Fecha,y=Distancia,group=Id,color=Id)) +
  geom_line(show.legend = FALSE) +
  theme(axis.text.x = element_blank()) +
  facet_wrap(~ Id)



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
  select(Id, Calories, time) #seleccionamos los datos para obtener consumo calorico versus hora.

Calorias_H <- Calorias_H %>% 
  rename(Calorias=Calories,Hora=time) #renombramos las columnas en castellano

Calorias_D <- Calorias_H_D %>% 
  select(Id, Calories, wday) %>% 
  arrange(wday) #seleccionamos los datos para analizar consumo calórico versus día de la semana

Calorias_D <- Calorias_D %>% 
  rename(Calorias = Calories, Dia_semana=wday) #renombramos las columnas en castellano


#en el siguiente comando obtenemos la suma de todas aquellas quemas calóricas que se dieron a la misma hora

Calorias_H_lite <- aggregate(Calorias_H$Calorias, by=list(Hora=Calorias_H$Hora), FUN=sum)

#luego hacemos algo similar para las quemas calóricas que ocurrieron el mismo día de la semana

Calorías_D_lite <- aggregate(Calorias_D$Calorias, by=list(Dia_de_la_semana=Calorias_D$Dia_semana), FUN=sum)


