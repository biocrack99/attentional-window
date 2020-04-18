##Script para ordenar y cargor los datos de los archivos obtenidos
##durante el experimento de Ventana Atencional

#cargo librerias
library(tidyverse)
library(R.matlab)
library(readr)
library(reshape2)
library(plyr)
################################################################################LISTA DE LOS DATOS

#Lista para guardar los datos segun la cantidad de obsevadores
N <- 30
list_datos <- vector("list", N)
list_gaze <- vector ("list", N)
Ntrials <- 336

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
  

# GAZE --------------------------------------------------------------------
  #Creo un data frame con los datos del gaze en cada trial
  tbl_gaze <- tbl[, (5:8)]
  df_gaze <- as.data.frame(tbl_gaze)
  #cambio el nombre de las columnas del gaze
  colnames(df_gaze)[c(3,4)] <- c("XGaze.mm", "YGaze.mm")
  #Busco las filas donde se separa cada trial
  index <- which(df_gaze$Direccion == "Direccion")
  #Genero dos listas con los gazes en x y en y en cada trial
  ls_gaze_x <- vector("list", Ntrials)
  ls_gaze_y <- vector("list", Ntrials)
  
  for (j in seq_along(ls_gaze_x)){
  
    if (j == 1){
        
        ls_gaze_x[[j]] <- as.double(df_gaze$XGaze.mm[1:index[j]])
        
        ls_gaze_y[[j]] <- as.double(df_gaze$YGaze.mm[1:index[j]])
        
        
      }
    
    else if (1 < j && j <= 335) {    
      
        ls_gaze_x[[j]] <- as.double(df_gaze$XGaze.mm[(index[j-1] + 1):index[(j)]])
      
        ls_gaze_y[[j]] <- as.double(df_gaze$YGaze.mm[(index[j-1] + 1):index[(j)]])
      }
      
    else  { 
        
            ls_gaze_x[[j]] <- as.double(df_gaze$XGaze.mm[(index[j-1] + 1):(length(df_gaze$XGaze.mm))])
        
            ls_gaze_y[[j]] <- as.double(df_gaze$YGaze.mm[(index[j-1] + 1):(length(df_gaze$XGaze.mm))])
        
      }
    
    }
  
  #Omito los valores NA en la lista del gaze
  for (l in seq_along(ls_gaze_x)){
  
  ls_gaze_x[[l]] <- na.omit(ls_gaze_x[[l]])
  ls_gaze_y[[l]] <- na.omit(ls_gaze_y[[l]])
  
  }
  
  #Omit los valores NA
  df_gaze <- na.omit(df_gaze)
  df_gaze <- df_gaze[!grepl("Direccion", df_gaze$Direccion),]
  #redondeo los datos de la separacion
  df_gaze$Separacion <- round(as.numeric(df_gaze$Separacion))
  df_gaze$Direccion <- as.numeric(df_gaze$Direccion)
  df_gaze <- df_gaze[,-(3:4)]
  
  for (k in seq_along(ls_gaze_x)){
    
    if (k == 1) {
                  
      df_gaze$XGaze.mm[[k]] <-  ls_gaze_x[[k]][101:184]
      df_gaze$YGaze.mm[[k]] <-  ls_gaze_y[[k]][101:184]
      
    }
    
    else{
      
      df_gaze$XGaze.mm[[k]] <-  ls_gaze_x[[k]][1:84]
      df_gaze$YGaze.mm[[k]] <-  ls_gaze_y[[k]][1:84]
      
    }
      
      
    
      }
  
  
#--------------------------------------------------------------------------
  
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
  #Elimino la primer fila de los datos debido a que se usa para
  #enseñar el estimulo al obsevador y para tener una referencia a la 
  #hora de la fijacion en los demas trials
  datos = datos[-1,]
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
    dplyr::summarise(n = n(), nYes = sum(correctas), nNo = n - nYes, p = nYes / n)
  
  averages$observadores <- substr(files[i], 3,5) 
  
  list_datos[i] <- list(averages)
  
  list_gaze[i] <- list(df_gaze)#, df_gaze$Separacion, ls_gaze_x, ls_gaze_y )

  }

#Elimino  variables
rm(averages, datos, df_gaze, ls_gaze_x, ls_gaze_y, tbl, tbl_gaze, tbl_sin_gaze ) 


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
  #corto los datos del gaze 
  for (k in seq_along(1:Ntrials)){
    
    if (k == 1) {
      
      datos$XGaze.mm[[k]] <-  datos$XGaze.mm[[k]][101:184]
      datos$YGaze.mm[[k]] <-  datos$YGaze.mm[[k]][101:184]
      
    }
    
    else{
      
      datos$XGaze.mm[[k]] <-  datos$XGaze.mm[[k]][1:84]
      datos$YGaze.mm[[k]] <-  datos$YGaze.mm[[k]][1:84]
      
    }
  }
  #convierto de list a numeric
  datos[c(varNames[1:6])] <-sapply(datos[c(varNames[1:6])], as.numeric)
  #redondeo los datos de la separacion
  datos$Separacion <- round(datos$Separacion, digits = 0)
  #creo data frame sin los datos del gaze
  datos_sin_Gaze <- datos[,-(7:ncol(datos))]
  #convierto a tabla
  tabla <- tbl_df(datos_sin_Gaze)
  
  #obtengo nuevas variables
  tabla_total <- mutate(tabla, correctas_A = Cantidad.TGC.1 == Respuesta.A, 
                        correctas_B = Cantidad.TGC.2 == Respuesta.B)
  
  #obtengo las respuesta correctas
  tabla_total <- mutate(tabla_total, correctas = correctas_A == correctas_B)
  
  #convierto a numero variables logicas
  tabla_total[c(7:9)] <- sapply(tabla_total[c(7:9)], as.numeric)
  #Elimino la primer fila de los datos debido a que se usa para
  #enseñar el estimulo al obsevador y para tener una referencia a la 
  #hora de la fijacion en los demas trials
  tabla_total = tabla_total[-1,]
  
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
    dplyr::summarise(n = n(), nYes = sum(correctas), nNo = n - nYes, p = nYes / n)
  #genero columna para los observadores
  averages$observadores <- substr(files[i], 3,5) 
  
  #lista de los datos
  list_datos[i + 11] <- list(averages)
  
  #lista del gaze
  list_gaze[i + 11] <- list(datos[,-(1:4)]) 

}

#Elimino  variables
rm(averages, data_mat, datList, datos, datos_sin_Gaze, matlabFile, tabla, tabla_total, values) 

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

# GAZE --------------------------------------------------------------------
  #Creo un data frame con los datos del gaze en cada trial
  tbl_gaze <- tbl[, (5:8)]
  df_gaze <- as.data.frame(tbl_gaze)
  #cambio el nombre de las columnas del gaze
  colnames(df_gaze)[c(3,4)] <- c("XGaze.mm", "YGaze.mm")
  #Busco las filas donde se separa cada trial
  index <- which(df_gaze$Direccion == "Direccion")
  #Genero dos listas con los gazes en x y en y en cada trial
  ls_gaze_x <- vector("list", Ntrials)
  ls_gaze_y <- vector("list", Ntrials)
  
  for (j in seq_along(ls_gaze_x)){
    
    if (j == 1){
      
      ls_gaze_x[[j]] <- as.double(df_gaze$XGaze.mm[1:index[j]])
      
      ls_gaze_y[[j]] <- as.double(df_gaze$YGaze.mm[1:index[j]])
      
      
    }
    
    else if (1 < j && j <= 335) {    
      
      ls_gaze_x[[j]] <- as.double(df_gaze$XGaze.mm[(index[j-1] + 1):index[(j)]])
      
      ls_gaze_y[[j]] <- as.double(df_gaze$YGaze.mm[(index[j-1] + 1):index[(j)]])
    }
    
    else  { 
      
      ls_gaze_x[[j]] <- as.double(df_gaze$XGaze.mm[(index[j-1] + 1):(length(df_gaze$XGaze.mm))])
      
      ls_gaze_y[[j]] <- as.double(df_gaze$YGaze.mm[(index[j-1] + 1):(length(df_gaze$XGaze.mm))])
      
    }
    
  }
  
  #Omito los valores NA en la lista del gaze
  for (l in seq_along(ls_gaze_x)){
    
    ls_gaze_x[[l]] <- na.omit(ls_gaze_x[[l]])
    ls_gaze_y[[l]] <- na.omit(ls_gaze_y[[l]])
    
  }
  
  #Omit los valores NA
  df_gaze <- na.omit(df_gaze)
  df_gaze <- df_gaze[!grepl("Direccion", df_gaze$Direccion),]
  #redondeo los datos de la separacion
  df_gaze$Separacion <- round(as.numeric(df_gaze$Separacion))
  df_gaze$Direccion <- as.numeric(df_gaze$Direccion)
  df_gaze <- df_gaze[,-(3:4)]
  
  for (k in seq_along(ls_gaze_x)){
    
    if (k == 1) {
      
      df_gaze$XGaze.mm[[k]] <-  ls_gaze_x[[k]][101:184]
      df_gaze$YGaze.mm[[k]] <-  ls_gaze_y[[k]][101:184]
      
    }
    
    else{
      
      df_gaze$XGaze.mm[[k]] <-  ls_gaze_x[[k]][1:84]
      df_gaze$YGaze.mm[[k]] <-  ls_gaze_y[[k]][1:84]
      
    }
    
    
    
  }
  
#--------------------------------------------------------------------------
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
  #Elimino la primer fila de los datos debido a que se usa para
  #enseñar el estimulo al obsevador y para tener una referencia a la 
  #hora de la fijacion en los demas trials
  datos = datos[-1,]
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
    dplyr::summarise(n = n(), nYes = sum(correctas), nNo = n - nYes, p = nYes / n)
  
  averages$observadores <- substr(files[i], 3,5) 
  
  list_datos[i + 15] <- list(averages)
  
  #lista del gaze
  list_gaze[i + 15] <- list(df_gaze)
  
}


#Elimino  variables
rm(averages, datos, df_gaze, ls_gaze_x, ls_gaze_y, tbl, tbl_gaze, tbl_sin_gaze ) 


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
  #corto los datos del gaze
  for (k in seq_along(1:Ntrials)){
    
    if (k == 1) {
      
      datos$XGaze.mm[[k]] <-  datos$XGaze.mm[[k]][101:184]
      datos$YGaze.mm[[k]] <-  datos$YGaze.mm[[k]][101:184]
      
    }
    
    else{
      
      datos$XGaze.mm[[k]] <-  datos$XGaze.mm[[k]][1:84]
      datos$YGaze.mm[[k]] <-  datos$YGaze.mm[[k]][1:84]
      
    }
  }
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
  #Elimino la primer fila de los datos debido a que se usa para
  #enseñar el estimulo al obsevador y para tener una referencia a la 
  #hora de la fijacion en los demas trials
  tabla_total = tabla_total[-1,]
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
    dplyr::summarise(n = n(), nYes = sum(correctas), nNo = n - nYes, p = nYes / n)
  
  averages$observadores <- substr(files[i], 3,5) 
  
  list_datos[i + 26] <- list(averages)
  
  #lista del gaze
  list_gaze[i + 26] <- list(datos[,-(1:4)]) 
  
}

#Elimino las variables
rm(list=setdiff(ls(), c("list_datos", "list_gaze", "Ntrials"))) 
###############################################################################
#PROCESAMIENTO

#1- Agrego columna pre y pos en la lista donde se encuentras los datos

for (i in seq_along(list_datos)){
  
  if (i <= (length(list_datos)/2)){
    
    list_datos[[i]]$condicion <- c("pre")
    
  }
  
  else {
    
    list_datos[[i]]$condicion <- c("pos")
    
  }
  
  
}

#2- Agrego columna con los grupos en la lista donde se encuentras los datos

for (i in seq_along(list_datos)){
  
  if( (list_datos[[i]]$observadores == "aaf" )||(list_datos[[i]]$observadores == "agm" )|| (list_datos[[i]]$observadores == "lrc")||(list_datos[[i]]$observadores == "pab") ){
    
    list_datos[[i]]$grupo <- c("cl")
}
  
  else if ( (list_datos[[i]]$observadores == "afb" )||(list_datos[[i]]$observadores == "cic" )|| (list_datos[[i]]$observadores == "msz")|| (list_datos[[i]]$observadores == "nga") ){
    
    list_datos[[i]]$grupo <- c("rt")
  
}
  else if ( (list_datos[[i]]$observadores == "lfa" )||(list_datos[[i]]$observadores == "lms" )|| (list_datos[[i]]$observadores == "mcm")|| (list_datos[[i]]$observadores == "at-") ){
    
    list_datos[[i]]$grupo <- c("hk")
    
  }
  
  else if ( (list_datos[[i]]$observadores == "jjr" )||(list_datos[[i]]$observadores == "mab" )|| (list_datos[[i]]$observadores == "mdn")){
    
    list_datos[[i]]$grupo <- c("lt")
    
  }
  
}

#3- Creo un dataframe con la lista de datos

df_datos <- ldply (list_datos, data.frame)

#4- Agrego los nombres de los observadores a la lista del gaze

names(list_gaze) <- c("aaf",  "afb",  "agm", "cic", "jjr", "lrc", "mab", "mdn", "msz", "nga", "pab", "at", "lfa", "lms", "mcm", "aaf",  "afb",  "agm", "cic", "jjr", "lrc", "mab", "mdn", "msz", "nga", "pab", "at", "lfa", "lms", "mcm")

#5- Comparo para cada obsevador el gaze de cada trial con el gaze del primer
#trial que se toma como referencia.

#5.1
#Calculo la media del XGaze y del YGaze del primer trial
#Calculo la distancia de cada punto del Gaze de los sucesivo trials con
#respecto a la media del primer trial.
vr_num_Dist <- rep(NaN, 84)
cst_DIST <- 16
cst_NPOINTS <- 84
vr_TrialOK <- rep(NaN, Ntrials)
num_XGazeREF <- rep(NaN, Ntrials)
num_YGazeREF <- rep(NaN, Ntrials)

for (j in seq_along(list_gaze)){ 
  
  num_XGazeREF[j] <- mean(list_gaze[[j]]$XGaze.mm[[1]])
  num_YGazeREF[j] <- mean(list_gaze[[j]]$YGaze.mm[[1]])

for (k in seq_along(vr_TrialOK)){

for (i in seq_along(vr_num_Dist)){
  
  Xdist <- (list_gaze[[j]]$XGaze.mm[[k]][i] - num_XGazeREF[j])^2
  Ydist <- (list_gaze[[j]]$YGaze.mm[[k]][i] - num_YGazeREF[j])^2
  
  vr_num_Dist[i]  <- sqrt(sum(Xdist,Ydist)) 
  
}
#Determino el porcentaje de  puntos caen adentro de la zona de fijacion dada por el tamaño de la cruz en mm
  aceptado <- c(vr_num_Dist <= cst_DIST)
  porcentaje <- ((sum(aceptado == TRUE))/cst_NPOINTS)*100
  vr_TrialOK[k] <- porcentaje

}
  list_gaze[[j]]$TRialOK <- vr_TrialOK

  
}
  
  
#Funcion para generar un circulo 
#circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
#  r = diameter / 2
#  tt <- seq(0,2*pi,length.out = npoints)
#  xx <- center[1] + r * cos(tt)
#  yy <- center[2] + r * sin(tt)
#  return(data.frame(x = xx, y = yy))
#}

#dat <- circleFun(c(mean(list_gaze[[18]]$XGaze.mm[[1]]),mean(list_gaze[[18]]$YGaze.mm[[1]])),16,npoints = 100)


#
#geom_path will do open circles, geom_polygon will do filled circles
#p <- ggplot() + 
#  
#  geom_point(aes(x=list_gaze[[18]]$XGaze.mm[[1]],y=list_gaze[[18]]$YGaze.mm[[1]] )) +
   
#  geom_path(data = dat, aes(x,y)) +
  
#  geom_point(aes(x=list_gaze[[18]]$XGaze.mm[[300]],y=list_gaze[[18]]$YGaze.mm[[300]], colour = "red" ))

#p
