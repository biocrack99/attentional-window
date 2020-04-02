##Script Grafico con coordenadas polar 
#Grafico el ancho de la ventana teorico segun un ajuste lineal
#a los datos de cada observador 

#cargo librerias
library(tidyverse)
library(R.matlab)
library(readr)
library(reshape2)
################################################################################LISTA DE LOS DATOS

#Lista para guardar los datos segun la cantidad de obsevadores
N <- 30
list_datos <- vector("list", N)

#Cargo los datos de la ventana atencional antes del entrenamiento del ILAV
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
for (i in seq_along(files))  {
  
  tbl <- sapply(files[i], read_csv, simplify=FALSE) %>% 
    bind_rows()
  #Creo una tabla sin las columnas del gaze
  tbl_sin_gaze <- tbl[,-(7:ncol(tbl))]
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
  
  averages$observadores <- substr(files[i], 3,5) 
  
  list_datos[i] <- list(averages)
  
}



###############################################################################
#Cargo los datos de la ventana atencional antes del entrenamiento de Hockey
setwd(paste("D:/Dropbox/Posdoc",
            "/Percepcion Deporte",
            "/Experimento MOT VA",
            "/Archivos Computadora Vision",
            "/Experimento Anibal",
            "/MATLAB_Diciembre",
            "/VENTANA ATENCION/Ventana version final/Datos/Hockey/Pre/", sep = ""))

files <- list.files(pattern = "*.mat", full.names = T)


for (i in seq_along(files))  {
  
  matlabFile  <- readMat(files[i])
  varNames    <- names(matlabFile$estructura.datos[,,1])
  datList     <- matlabFile$estructura.datos
  #datList    <- lapply(datList, unlist, use.names=FALSE)
  data_mat    <- as.data.frame(datList)
  #names(data_mat) <- varNames
  datos <- as.data.frame(t(data_mat))
  #veo que tipos de datos tengo
  glimpse(datos)
  sapply(datos,mode)
  #convierto de list a numeric
  datos[c(varNames[1:6])] <-sapply(datos[c(varNames[1:6])], as.numeric)
  #creo data frame sin los datos del gaze
  datos_sin_Gaze <- datos[,-(7:ncol(datos))]
  #redondeo los datos de la separacion
  datos_sin_Gaze$Separacion <- round(datos_sin_Gaze$Separacion, digits = 0)
  #convierto a tabla
  tabla <- tbl_df(datos_sin_Gaze)
  
  #obtengo nuevas variables
  tabla_total <- mutate(tabla, correctas_A = Cantidad.TGC.1 == Respuesta.A, 
                        correctas_B = Cantidad.TGC.2 == Respuesta.B)
  
  #obtengo las respuesta correctas
  tabla_total <- mutate(tabla_total, correctas = correctas_A == correctas_B)
  
  #convierto a numero variables logicas
  tabla_total[c(7:9)] <- sapply(tabla_total[c(7:9)], as.numeric)
  
  #obtengo el n de cada presentacion
  n <- c(sum(tabla_total$Direccion == 0 & tabla_total$Separacion == 3),
         sum(tabla_total$Direccion == 0 & tabla_total$Separacion == 5),
         sum(tabla_total$Direccion == 0 & tabla_total$Separacion == 8),
         sum(tabla_total$Direccion == 0 & tabla_total$Separacion == 11),
         sum(tabla_total$Direccion == 0 & tabla_total$Separacion == 14),
         sum(tabla_total$Direccion == 0 & tabla_total$Separacion == 17),
         sum(tabla_total$Direccion == 0 & tabla_total$Separacion == 20),
         sum(tabla_total$Direccion == 45 & tabla_total$Separacion == 3),
         sum(tabla_total$Direccion == 45 & tabla_total$Separacion == 5),
         sum(tabla_total$Direccion == 45 & tabla_total$Separacion == 8),
         sum(tabla_total$Direccion == 45 & tabla_total$Separacion == 11),
         sum(tabla_total$Direccion == 45 & tabla_total$Separacion == 14),
         sum(tabla_total$Direccion == 45 & tabla_total$Separacion == 17),
         sum(tabla_total$Direccion == 45 & tabla_total$Separacion == 20),
         sum(tabla_total$Direccion == 90 & tabla_total$Separacion == 3),
         sum(tabla_total$Direccion == 90 & tabla_total$Separacion == 5),
         sum(tabla_total$Direccion == 90 & tabla_total$Separacion == 8),
         sum(tabla_total$Direccion == 90 & tabla_total$Separacion == 11),
         sum(tabla_total$Direccion == 90 & tabla_total$Separacion == 14),
         sum(tabla_total$Direccion == 90 & tabla_total$Separacion == 17),
         sum(tabla_total$Direccion == 90 & tabla_total$Separacion == 20),
         sum(tabla_total$Direccion == 135 & tabla_total$Separacion == 3),
         sum(tabla_total$Direccion == 135 & tabla_total$Separacion == 5),
         sum(tabla_total$Direccion == 135 & tabla_total$Separacion == 8),
         sum(tabla_total$Direccion == 135 & tabla_total$Separacion == 11),
         sum(tabla_total$Direccion == 135 & tabla_total$Separacion == 14),
         sum(tabla_total$Direccion == 135 & tabla_total$Separacion == 17),
         sum(tabla_total$Direccion == 135 & tabla_total$Separacion == 20))
  
  #promedios de las respuesta
  averages <- tabla_total %>% group_by( Direccion, Separacion) %>% 
    summarise(n = n(), nYes = sum(correctas), nNo = n - nYes, p = nYes / n)
  
  averages$observadores <- substr(files[i], 3,5) 
  
  list_datos[i + 11] <- list(averages)
  


}

###############################################################################
#Cargo los datos de la ventana atencional despues del entrenamiento del ILAV
setwd(paste("D:/Dropbox/Posdoc",
            "/Percepcion Deporte",
            "/Experimento MOT VA",
            "/Archivos Computadora Vision",
            "/Experimento Anibal",
            "/MATLAB_Diciembre",
            "/VENTANA ATENCION/Ventana version final/Datos/Post test/CSV/", sep = ""))
#Leo los nombres de los de archivos que contienen la extenxion .csv
files <- list.files(pattern = "*.csv", full.names = T)

#rm(tbl, tbl_sin_gaze, datos, n, averages)


for (i in seq_along(files))  {
  
  tbl <- sapply(files[i], read_csv, simplify=FALSE) %>% 
    bind_rows()
  #Creo una tabla sin las columnas del gaze
  tbl_sin_gaze <- tbl[,-(7:ncol(tbl))]
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
  
  averages$observadores <- substr(files[i], 3,5) 
  
  list_datos[i + 15] <- list(averages)
  
}

###############################################################################
#Cargo los datos de la ventana atencional despues del entrenamiento Hockey 
#ubico el directorio donde se encuentran los archivos
setwd(paste("D:/Dropbox/Posdoc",
            "/Percepcion Deporte",
            "/Experimento MOT VA",
            "/Archivos Computadora Vision",
            "/Experimento Anibal",
            "/MATLAB_Diciembre",
            "/VENTANA ATENCION/Ventana version final/Datos/Hockey/Pos/", sep = ""))
#Leo los nombres de los de archivos que contienen la extenxion .mat
files <- list.files(pattern = "*.mat", full.names = T)


for (i in seq_along(files))  {
  
  matlabFile  <- readMat(files[i])
  varNames    <- names(matlabFile$estructura.datos[,,1])
  datList     <- matlabFile$estructura.datos
  #datList    <- lapply(datList, unlist, use.names=FALSE)
  data_mat    <- as.data.frame(datList)
  #names(data_mat) <- varNames
  datos <- as.data.frame(t(data_mat))
  #veo que tipos de datos tengo
  glimpse(datos)
  sapply(datos,mode)
  #convierto de list a numeric
  datos[c(varNames[1:6])] <-sapply(datos[c(varNames[1:6])], as.numeric)
  #creo data frame sin los datos del gaze
  datos_sin_Gaze <- datos[,-(7:ncol(datos))]
  #redondeo los datos de la separacion
  datos_sin_Gaze$Separacion <- round(datos_sin_Gaze$Separacion, digits = 0)
  #convierto a tabla
  tabla <- tbl_df(datos_sin_Gaze)
  
  #obtengo nuevas variables
  tabla_total <- mutate(tabla, correctas_A = Cantidad.TGC.1 == Respuesta.A, 
                        correctas_B = Cantidad.TGC.2 == Respuesta.B)
  
  #obtengo las respuesta correctas
  tabla_total <- mutate(tabla_total, correctas = correctas_A == correctas_B)
  
  #convierto a numero variables logicas
  tabla_total[c(7:9)] <- sapply(tabla_total[c(7:9)], as.numeric)
  
  #obtengo el n de cada presentacion
  n <- c(sum(tabla_total$Direccion == 0 & tabla_total$Separacion == 3),
         sum(tabla_total$Direccion == 0 & tabla_total$Separacion == 5),
         sum(tabla_total$Direccion == 0 & tabla_total$Separacion == 8),
         sum(tabla_total$Direccion == 0 & tabla_total$Separacion == 11),
         sum(tabla_total$Direccion == 0 & tabla_total$Separacion == 14),
         sum(tabla_total$Direccion == 0 & tabla_total$Separacion == 17),
         sum(tabla_total$Direccion == 0 & tabla_total$Separacion == 20),
         sum(tabla_total$Direccion == 45 & tabla_total$Separacion == 3),
         sum(tabla_total$Direccion == 45 & tabla_total$Separacion == 5),
         sum(tabla_total$Direccion == 45 & tabla_total$Separacion == 8),
         sum(tabla_total$Direccion == 45 & tabla_total$Separacion == 11),
         sum(tabla_total$Direccion == 45 & tabla_total$Separacion == 14),
         sum(tabla_total$Direccion == 45 & tabla_total$Separacion == 17),
         sum(tabla_total$Direccion == 45 & tabla_total$Separacion == 20),
         sum(tabla_total$Direccion == 90 & tabla_total$Separacion == 3),
         sum(tabla_total$Direccion == 90 & tabla_total$Separacion == 5),
         sum(tabla_total$Direccion == 90 & tabla_total$Separacion == 8),
         sum(tabla_total$Direccion == 90 & tabla_total$Separacion == 11),
         sum(tabla_total$Direccion == 90 & tabla_total$Separacion == 14),
         sum(tabla_total$Direccion == 90 & tabla_total$Separacion == 17),
         sum(tabla_total$Direccion == 90 & tabla_total$Separacion == 20),
         sum(tabla_total$Direccion == 135 & tabla_total$Separacion == 3),
         sum(tabla_total$Direccion == 135 & tabla_total$Separacion == 5),
         sum(tabla_total$Direccion == 135 & tabla_total$Separacion == 8),
         sum(tabla_total$Direccion == 135 & tabla_total$Separacion == 11),
         sum(tabla_total$Direccion == 135 & tabla_total$Separacion == 14),
         sum(tabla_total$Direccion == 135 & tabla_total$Separacion == 17),
         sum(tabla_total$Direccion == 135 & tabla_total$Separacion == 20))
  
  #promedios de las respuesta
  averages <- tabla_total %>% group_by( Direccion, Separacion) %>% 
    summarise(n = n(), nYes = sum(correctas), nNo = n - nYes, p = nYes / n)
  
  averages$observadores <- substr(files[i], 3,5) 
  
  list_datos[i + 26] <- list(averages)
  
}
