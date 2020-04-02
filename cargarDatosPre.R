## FuNCION PARA LEER LOS DATOS ##
## DE TODOS LOS OBSERVADORES ##

# Version 0.1


#cargo librerias
library(tidyverse)
library(R.matlab)
library(readr)
library(reshape2)

#ubico el directorio donde se encuentran los archivos
setwd(paste("D:/Dropbox/Posdoc",
            "/Percepcion Deporte",
            "/Experimento MOT VA",
            "/Archivos Computadora Vision",
            "/Experimento Anibal",
            "/MATLAB_Diciembre",
            "/VENTANA ATENCION/Ventana version final/Datos/Pre test/CSV/", sep = ""))
#Leo los nombres de los de archivos que contienen la extenxion .csv
files <- list.files(pattern = "*.csv", full.names = T)
#Cargo los datos de esos archivos en una tabla y uno las filas
tbl <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows()
#Creo una tabla sin las columnas del gaze
tbl_sin_gaze <- tbl[,-(7:9)]
#Elimino las filas que tengas valores NA
tbl_sin_gaze <- as.data.frame(tbl_sin_gaze)
tbl_sin_gaze <- na.omit(tbl_sin_gaze)
#Elimino filas 
datos <- tbl_sin_gaze[!grepl("Cantidad_TGC_1", tbl_sin_gaze$Cantidad_TGC_1),]
#Redondeo los datos de la columna separacion
datos$Separacion <- round(as.numeric(datos$Separacion), digits = 0)
#Obtengo nuevas variables
datos <- mutate(datos, correctas_A = Cantidad_TGC_1 == Respuesta_A, 
                      correctas_B = Cantidad_TGC_2 == Respuesta_B)
#Obtengo las respuesta correctas
datos <- mutate(datos, correctas = correctas_A == correctas_B)
#Convierto a numero variables logicas
datos[c(7:9)] <- sapply(datos[c(7:9)], as.numeric)
#obtengo el n de cada presentacion
n <- c(sum(datos$Direccion == 0 & datos$Separacion == 3),
       sum(datos$Direccion == 0 & datos$Separacion == 5),
       sum(datos$Direccion == 0 & datos$Separacion == 8),
       sum(datos$Direccion == 0 & datos$Separacion == 11),
       sum(datos$Direccion == 0 & datos$Separacion == 14),
       sum(datos$Direccion == 0 & datos$Separacion == 17),
       sum(datos$Direccion == 0 & datos$Separacion == 20),
       sum(datos$Direccion == 45 & datos$Separacion == 3),
       sum(datos$Direccion == 45 & datos$Separacion == 5),
       sum(datos$Direccion == 45 & datos$Separacion == 8),
       sum(datos$Direccion == 45 & datos$Separacion == 11),
       sum(datos$Direccion == 45 & datos$Separacion == 14),
       sum(datos$Direccion == 45 & datos$Separacion == 17),
       sum(datos$Direccion == 45 & datos$Separacion == 20),
       sum(datos$Direccion == 90 & datos$Separacion == 3),
       sum(datos$Direccion == 90 & datos$Separacion == 5),
       sum(datos$Direccion == 90 & datos$Separacion == 8),
       sum(datos$Direccion == 90 & datos$Separacion == 11),
       sum(datos$Direccion == 90 & datos$Separacion == 14),
       sum(datos$Direccion == 90 & datos$Separacion == 17),
       sum(datos$Direccion == 90 & datos$Separacion == 20),
       sum(datos$Direccion == 135 & datos$Separacion == 3),
       sum(datos$Direccion == 135 & datos$Separacion == 5),
       sum(datos$Direccion == 135 & datos$Separacion == 8),
       sum(datos$Direccion == 135 & datos$Separacion == 11),
       sum(datos$Direccion == 135 & datos$Separacion == 14),
       sum(datos$Direccion == 135 & datos$Separacion == 17),
       sum(datos$Direccion == 135 & datos$Separacion == 20))

#promedios de las respuesta
averages <- datos %>% group_by( Direccion, Separacion) %>% 
  summarise(n = n(), nYes = sum(correctas), nNo = n - nYes, p = nYes / n)

#Grafico de cajas
#1 Conveirto a factor la separacion 

averages2 <- data.frame(x = averages$Separacion, 
                        y = averages$p , z = averages$Direccion)

averages2$x <- factor(averages2$x)
averages2$z <- factor(averages2$z)

fill <- "#4271AE"
line <- "#1F3552"

Box_Sep_Fact <- ggplot(averages2, aes(x = x, y= y)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4, fill = fill, colour = line,  alpha = 0.7) + 
  labs(x = "Separacion [°]", y = "Correctas [-]", title = "") +
  geom_jitter(shape=16, position=position_jitter(0.2)) 
  
 


Box_Sep_Fact 


Box_Dir_Fact <- ggplot(averages2, aes(x = z , y= y)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) + 
  labs(x = "Dirección [°]", y = "Correctas [-]", title = "") +
  geom_jitter(shape=16, position=position_jitter(0.2))


Box_Dir_Fact

