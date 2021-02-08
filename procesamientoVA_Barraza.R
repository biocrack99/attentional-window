##Experimento Ventana Atencional
##Script para el procesamiento de los datos del experimento de Ventana Atencion 
## usando la propuesta de procesamiento del Dr. José Barraza
## Autor: Aníbal de Paul
## Fecha: 25/09/2020

#cargo librerias
library(plyr)
library(tidyverse)
library(R.matlab)
library(readr)
library(reshape2)
library(e1071)
library(saccades)

#Lista para guardar los datos segun la cantidad de obsevadores
N <- 30
list_datosRaw <- vector("list", N)
list_datos <- vector("list", N)
list_gaze <- vector ("list", N)
list_fixations <- vector("list", N)

Ntrials <- 336


# CARGO DATOS ILAV PRE ENTRENAMIENTO --------------------------------------
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
  datos <- mutate(datos, 
                  correctas_A = Cantidad_TGC_1 == Respuesta_A, 
                  correctas_B = Cantidad_TGC_2 == Respuesta_B
  )
  #Obtengo las respuesta correctas
  datos <- mutate(datos, 
                  correctas = (correctas_A == 1 & 
                                 correctas_B == 1
                  )
  )
  
  #Convierto a numero variables logicas
  datos[c(7:9)] <- sapply(datos[c(7:9)], as.numeric)
  #Elimino la primer fila de los datos debido a que se usa para
  #enseñar el estimulo al obsevador y para tener una referencia a la 
  #hora de la fijacion en los demas trials
  datos = datos[-1,]
  #promedios de las respuesta
  averages <- datos %>% group_by( Direccion, Separacion) %>% 
    dplyr::summarise(n = n(), nYes = sum(correctas), nNo = n - nYes, p = nYes / n)
  
  averages$observadores <- substr(files[i], 3,5) 
  #datos crudos
  list_datosRaw[i] <- list(datos)
  #datos promedio
  list_datos[i] <- list(averages)
  #datos del gaze
  list_gaze[i] <- list(df_gaze)#, df_gaze$Separacion, ls_gaze_x, ls_gaze_y )
  
}

#Elimino  variables
rm(averages, datos, df_gaze, ls_gaze_x, ls_gaze_y, tbl, tbl_gaze, tbl_sin_gaze ) 



# CARGO DATOS HOCKEY PRE ENTRENAMIENTO ------------------------------
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
  
  # GAZE --------------------------------------------------------------------
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
  tabla_total <- mutate(tabla, 
                        correctas_A = Cantidad.TGC.1 ==  Respuesta.A, 
                        correctas_B = Cantidad.TGC.2 == Respuesta.B)
  
  
  #obtengo las respuesta correctas
  #tabla_total <- mutate(tabla_total, correctas = correctas_A == correctas_B)
  tabla_total <- mutate(tabla_total, 
                        correctas = (correctas_A == TRUE & correctas_B == TRUE))
  
  
  #convierto a numero variables logicas
  tabla_total[c(7:9)] <- sapply(tabla_total[c(7:9)], as.numeric)
  #Elimino la primer fila de los datos debido a que se usa para
  #enseñar el estimulo al obsevador y para tener una referencia a la 
  #hora de la fijacion en los demas trials
  tabla_total = tabla_total[-1,]
  #Cambio nombres de columnas
  colnames(tabla_total)[c(1,2,3,4)] <- c("Cantidad_TGC_1", "Cantidad_TGC_2", "Respuesta_A", "Respuesta_B")
  
  #promedios de las respuesta
  averages <- tabla_total %>% group_by( Direccion, Separacion) %>% 
    dplyr::summarise(n = n(), nYes = sum(correctas), nNo = n - nYes, p = nYes / n)
  #genero columna para los observadores
  averages$observadores <- substr(files[i], 3,5) 
  
  #lista de los datos crudos
  list_datosRaw[i+11] <- list(data.frame(tabla_total))
  
  
  #lista de los datos
  list_datos[i + 11] <- list(averages)
  
  #lista del gaze
  list_gaze[i + 11] <- list(datos[,-(1:4)]) 
  
}

#Elimino  variables
rm(averages, data_mat, datList, datos, datos_sin_Gaze, matlabFile, tabla, tabla_total, values) 


# CARGO DATOS ILAV POS ENTRENAMIENTO --------------------------------------
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
  datos <- mutate(datos, 
                  correctas_A = Cantidad_TGC_1 == Respuesta_A, 
                  correctas_B = Cantidad_TGC_2 == Respuesta_B
  )
  #Obtengo las respuesta correctas
  datos <- mutate(datos, 
                  correctas = (correctas_A == 1 & 
                                 correctas_B == 1
                  )
  )
  #Convierto a numero variables logicas
  datos[c(7:9)] <- sapply(datos[c(7:9)], as.numeric)
  #Elimino la primer fila de los datos debido a que se usa para
  #enseñar el estimulo al obsevador y para tener una referencia a la 
  #hora de la fijacion en los demas trials
  datos = datos[-1,]
  
  #promedios de las respuesta
  averages <- datos %>% group_by( Direccion, Separacion) %>% 
    dplyr::summarise(n = n(), nYes = sum(correctas), nNo = n - nYes, p = nYes / n)
  
  averages$observadores <- substr(files[i], 3,5) 
  #datos crudos
  list_datosRaw[i + 15] <- list(datos)
  
  list_datos[i + 15] <- list(averages)
  
  #lista del gaze
  list_gaze[i + 15] <- list(df_gaze)
  
}
#Elimino  variables
rm(averages, datos, df_gaze, ls_gaze_x, ls_gaze_y, tbl, tbl_gaze, tbl_sin_gaze ) 



# CARGO DATOS HOCEKY POS ENTRENAMIENTO ------------------------------------
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
  tabla_total <- mutate(tabla, 
                        correctas_A = Cantidad.TGC.1 == Respuesta.A, 
                        correctas_B = Cantidad.TGC.2 == Respuesta.B
  )
  #obtengo las respuesta correctas
  tabla_total <- mutate(tabla_total, 
                        correctas = (correctas_A == TRUE &
                                       correctas_B == TRUE
                        )
  )
  
  
  #convierto a numero variables logicas
  tabla_total[c(7:9)] <- sapply(tabla_total[c(7:9)], as.numeric)
  #Elimino la primer fila de los datos debido a que se usa para
  #enseñar el estimulo al obsevador y para tener una referencia a la 
  #hora de la fijacion en los demas trials
  tabla_total = tabla_total[-1,]
  #Cambio nombres de columnas
  colnames(tabla_total)[c(1,2,3,4)] <- c("Cantidad_TGC_1", "Cantidad_TGC_2", "Respuesta_A", "Respuesta_B")
  
  #promedios de las respuesta
  averages <- tabla_total %>% group_by( Direccion, Separacion) %>% 
    dplyr::summarise(n = n(), nYes = sum(correctas), nNo = n - nYes, p = nYes / n)
  
  averages$observadores <- substr(files[i], 3,5) 
  #lista de los datos crudos
  list_datosRaw[i+26] <- list(data.frame(tabla_total))
  
  
  list_datos[i + 26] <- list(averages)
  
  #lista del gaze
  list_gaze[i + 26] <- list(datos[,-(1:4)]) 
  
}

#Elimino las variables
rm(list=setdiff(ls(), c("list_datos", "list_gaze", "Ntrials", "list_datosRaw", "list_fixations"))) 



# PROCESAMIENTO -----------------------------------------------------------

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

obs <- c("pre_aaf", "pre_afb", "pre_agm", "pre_cic", 
         "pre_jjr", "pre_lrc", "pre_mab", "pre_mdn", 
         "pre_msz", "pre_nga", "pre_pab", "pre_at", 
         "pre_lfa", "pre_lms", "pre_mcm", "pos_aaf", 
         "pos_afb", "pos_agm", "pos_cic", "pos_jjr", 
         "pos_lrc", "pos_mab", "pos_mdn", "pos_msz", 
         "pos_nga", "pos_pab", "pos_at",  "pos_lfa", 
         "pos_lms", "pos_mcm")

names(list_gaze) <- obs 


# GAZE --------------------------------------------------------------------
#5- Comparo para cada obsevador el gaze de cada trial con el gaze del primer
#trial que se toma como referencia.
#5.1
#Calculo la media del XGaze y del YGaze del primer trial
#Calculo la distancia de cada punto del Gaze de los sucesivo trials con
#respecto a la media del primer trial.
#Compara la distancia calculada con un valor constante cst_DIST
vr_num_Dist <- rep(NaN, 84)
cst_DIST <- 40
cst_NPOINTS <- 84
vr_TrialOK <- rep(NaN, Ntrials)
num_XGazeREF <- rep(NaN, 30)
num_YGazeREF <- rep(NaN, 30)
for (j in seq_along(list_gaze)){ 
  
  num_XGazeREF[j] <- mean(list_gaze[[j]]$XGaze.mm[[1]])
  num_YGazeREF[j] <- mean(list_gaze[[j]]$YGaze.mm[[1]])
  
  for (k in seq_along(vr_TrialOK)){
    
    for (i in seq_along(vr_num_Dist)){
      
      Xdist <- (list_gaze[[j]]$XGaze.mm[[k]][i] - num_XGazeREF[j])^2
      Ydist <- (list_gaze[[j]]$YGaze.mm[[k]][i] - num_YGazeREF[j])^2
      
      vr_num_Dist[i]  <- sqrt(sum(Xdist,Ydist)) 
      
    }
    #Determino el porcentaje de  puntos caen adentro de la zona de fijacion dada por el tamaño de la cruz de fijacion en mm
    aceptado <- c(vr_num_Dist <= cst_DIST)
    porcentaje <- ((sum(aceptado == TRUE))/cst_NPOINTS)*100
    vr_TrialOK[k] <- porcentaje
    
  }
  list_gaze[[j]]$TRialOK <- vr_TrialOK
  list_gaze[[j]]$trial <- c(1:336)
  list_gaze[[j]]$time <- c(0:0.02:83*0.02)
  
}
#5.2
#Origino una lista de indices para cada observador con los trials aceptados para el procesamiento posterior
#Obtengo los trials que tienen un porcentaje mayor al 40% de los puntos del gaze dentro de la zona de fijacion
#Genero un data frame para cada observador con el porcentaje de respuestas correctas en cada Direccion y en cada Separacion
list_index <- vector("list", 40)
list_datosFinal <- vector ("list", 30)
list_datosFinal_x <- vector("list", 30)

for (i in seq_along(list_gaze)){
  
  list_index[[i]] <- which(list_gaze[[i]]$TRialOK >= 40)
  
  list_datosFinal[[i]] <- slice(list_datosRaw[[i]], c(list_index[[i]][-1]))
  
  list_datosFinal_x[[i]] <- slice(list_datosRaw[[i]], c(-list_index[[i]]))
  
  list_datosFinal[[i]] <-list_datosFinal[[i]] %>% 
    group_by( Direccion, Separacion) %>% 
    dplyr::summarise(n = n(), nYes = sum(correctas), nNo = n - nYes, p = nYes / n)
  
  list_datosFinal_x[[i]] <-list_datosFinal_x[[i]] %>% 
    group_by( Direccion, Separacion) %>% 
    dplyr::summarise(n = n(), nYes = sum(correctas), nNo = n - nYes, p = nYes / n)
  
  list_datosFinal[[i]]$Observadores <- list_datos[[i]]$observadores[1:length
(list_datosFinal[[i]]$p)]
  
  list_datosFinal_x[[i]]$Observadores <-list_datos[[i]]$observadores[1:length
(list_datosFinal_x[[i]]$p)]
  
  list_datosFinal[[i]]$Condicion <-list_datos[[i]]$condicion[1:length(list_datosFinal[[i]]$p)]
  
  list_datosFinal_x[[i]]$Condicion <- list_datos[[i]]$condicion[1:length(list_datosFinal_x[[i]]$p)]
  
  list_datosFinal[[i]]$Grupo <- list_datos[[i]]$grupo[1:length(list_datosFinal[[i]]$p)]
  
  list_datosFinal_x[[i]]$Grupo <- list_datos[[i]]$grupo[1:length(list_datosFinal_x[[i]]$p)]
  
  
}

#5.3 
#Creo un dataframe con los datos del gaze
df_datosFinal <- ldply (list_datosFinal, data.frame)
df_datosFinal_x <- ldply (list_datosFinal_x, data.frame)
df_datosFinal$Fijacion <- c("SI") 
df_datosFinal_x$Fijacion <- c("NO")
df_datos$Fijacion <- c("NA")
names(df_datos)[7:9] <- c("Observadores", "Condicion", "Grupo")
df_datos <- rbind(df_datos, df_datosFinal, df_datosFinal_x)

#ALTERNATIVA PROCESAMIENTO GAZE: LIBRERIA saccades -----------------------------

#6- Libreria saccades
#Creo una lista para preparar los datos según la libreria saccades
#usando a los datos de todos los observadores 
for (i in seq_along(list_gaze)){
  
  gaze <- data.frame(time = rep(c(0:0.02:83*0.02), 336), 
                     x = unlist(list_gaze[[i]]$XGaze.mm), 
                     y = unlist(list_gaze[[i]]$YGaze.mm), 
                     trial = rep(c(1:336), each = 84)
                     )
  
  fixations <- detect.fixations(gaze, 
                                lambda = 10, 
                                smooth.coordinates = TRUE, 
                                smooth.saccades = FALSE
                                )
  #Elimino las fijaciones que detecta el algoritmo con 
  #duracion menor a 0.28
  fixations <- subset(fixations,
                      dur > 0.28, 
                      select = c(trial:dur)
                      )
  
  #Calculo la distancia de las fijaciones detectadas a
  #la fijacion del primer trial
  fixations$dist <- sqrt((fixations$x-fixations$x[1])^2 +
                        (fixations$y-fixations$y[1])^2
                        ) 
  
  #Agrego a la lista de fijaciones
  list_fixations[i] <- list(fixations)

  }

#6.1
#Agrego columna pre y pos en la lista donde 
#se encuentras los datos de la fijaciones
for (i in seq_along(list_fixations)){
  
  if (i <= (length(list_fixations)/2)){
    
    list_fixations[[i]]$condicion <- c("pre")
    
  }
  
  else {
    
    list_fixations[[i]]$condicion <- c("pos")
    
  }
  
  
}

#6.2
#Variable para el nombre de los observadores
names(list_fixations) <- obs 

#6.3
#Lazo para detectar la cantidad de trials aceptados
#para el post-procesamiento. Cuento los trials donde no
#hay ningna fijacion para luego eliminarlos.
vr_index_trial <- 1:336
porcentajeTrialFinal <- NA
trialDescartar <- NA
for (i in seq_along(list_fixations)){
  
  n <- list_fixations[[i]] %>% 
    group_by(trial) %>%
    summarise (n=n())
# Vector con la cantidad de trials donde el sujeto realiza
# una o mas fijaciones mayor a 0.28 seg  
  porcentajeTrialFinal <-  nrow(n)/336
  #list_fixations[[i]]$porcentajeTrialOk <- porcentajeTrialFinal  
# Busco los trials para descartar
  trialDescartar <- which(is.na(match(vr_index_trial,n$trial)))               
  list_fixations[[i]] <- list(list_fixations[[i]], 
                              porcentajeTrialFinal, 
                              trialDescartar)                                            
}

#6.4 Elimino los trials donde no hay fijacion de la lista 
#de datos crudos para cada observador y creo una lista con 
#variables nuevas para las respuesta del observador 
list_datos_filtrados <- vector("list", 30)
for (i in seq_along(list_datosRaw)){
  
  list_datos_filtrados[[i]] <- list_datosRaw[[i]]
  
  if (!is_empty(list_fixations[[i]][[3]])){
  
  
    list_datos_filtrados[[i]] <-list_datos_filtrados[[i]][-list_fixations[[i]][[3]], ]
  
  }
  
  else {
  
    list_datos_filtrados[[i]] <- list_datos_filtrados[[i]]
    
    }
  
    
    
    list_datos_filtrados[[i]] <- mutate(list_datos_filtrados[[i]], 
                                      correctas_A = Cantidad_TGC_1 == Respuesta_A, 
                                      correctas_B = Cantidad_TGC_2 == Respuesta_B
                                      )
  #Obtengo las respuesta correctas
  list_datos_filtrados[[i]] <- mutate(list_datos_filtrados[[i]], 
                                      correctas = (correctas_A == 1 & 
                                                     correctas_B == 1
                  
                                                   )
  
                                      )
  list_datos_filtrados[[i]] <- list_datos_filtrados[[i]] %>% 
    group_by( Direccion, Separacion) %>% 
    dplyr::summarise(n = n(), 
                     nYes = sum(correctas), 
                     nNo = n - nYes, 
                     p = nYes / n)  
    
                                                                    
}
names(list_datos_filtrados) <- obs 

#6.5 Agrego columna pre y pos en la lista donde se encuentras los datos
for (i in seq_along(list_datos_filtrados)){
  
  if (i <= (length(list_datos_filtrados)/2)){
    
    list_datos_filtrados[[i]]$condicion <- c("pre")
    
  }
  
  else {
    
    list_datos_filtrados[[i]]$condicion <- c("pos")
    
  }
  
  list_datos_filtrados[[i]]$observadores <- list_datos[[i]]$observadores 
  
}

#6.6 Agrego columna con los grupos en la lista donde se encuentras los datos
for (i in seq_along(list_datos_filtrados)){
  
  if( (list_datos_filtrados[[i]]$observadores == "aaf" )||(list_datos_filtrados[[i]]$observadores == "agm" )|| (list_datos_filtrados[[i]]$observadores == "lrc")||(list_datos_filtrados[[i]]$observadores == "pab") ){
    
    list_datos_filtrados[[i]]$grupo <- c("cl")
  }
  
  else if ( (list_datos_filtrados[[i]]$observadores == "afb" )||(list_datos_filtrados[[i]]$observadores == "cic" )|| (list_datos_filtrados[[i]]$observadores == "msz")|| (list_datos_filtrados[[i]]$observadores == "nga") ){
    
    list_datos_filtrados[[i]]$grupo <- c("rt")
    
  }
  else if ( (list_datos_filtrados[[i]]$observadores == "lfa" )||(list_datos_filtrados[[i]]$observadores == "lms" )|| (list_datos_filtrados[[i]]$observadores == "mcm")|| (list_datos_filtrados[[i]]$observadores == "at-") ){
    
    list_datos_filtrados[[i]]$grupo <- c("hk")
    
  }
  
  else if ( (list_datos_filtrados[[i]]$observadores == "jjr" )||(list_datos_filtrados[[i]]$observadores == "mab" )|| (list_datos_filtrados[[i]]$observadores == "mdn")){
    
    list_datos_filtrados[[i]]$grupo <- c("lt")
    
  }
  
}


#PROCESAMIENTO GRUPO CONTROL ----------------------------------------------------

#1- Creo un dataframe con la lista de datos
df_datos_filtrados <- ldply (list_datos_filtrados, data.frame)
df_datos_filtrados <- df_datos_filtrados[,-1]

#2- Dataframe Grupo Control y nuevas variables 
df_GrupoControl_filtrados <-  df_datos_filtrados %>%
  
  filter(grupo == "cl") %>%
  
  group_by(Separacion, condicion) %>%
  
  summarise(MediaAciertos = mean(p), DesvAciertos = sd(p)) %>%
  
  mutate(Ratio =  
           MediaAciertos[which(condicion== "pos")]/ 
           MediaAciertos[which(condicion == "pre")],
         
         Difer = 
           MediaAciertos[which(condicion== "pos")]- 
           MediaAciertos[which(condicion == "pre")]
  )

#3- Genero nuevas variables con el porcentaje de aciertos Pre y Pos entrenamiento 
#para luego poder organizar el dataframe coomo lo hizo Jose           
df_GrupoControl_filtrados <-  df_GrupoControl_filtrados %>% 
  
  mutate( PreMedia = MediaAciertos[which(condicion == "pre")],
          PosMedia = MediaAciertos[which(condicion == "pos")],
          PreDesv  = DesvAciertos[which(condicion  == "pre")],
          PosDesv  = DesvAciertos[which(condicion  == "pos")] 
  )

#4- Elimino columnas y valores duplicados
df_GrupoControl_filtrados[2:4] <- list(NULL)
df_GrupoControl_filtrados <- unique(df_GrupoControl_filtrados)


# Modelos lineales
#1 Gráficos 
par(mfrow=c(1, 2))  
scatter.smooth(x=df_GrupoControl_filtrados$Separacion, 
               y=df_GrupoControl_filtrados$Ratio, 
               main="Ratio ~ Separacion"
)

scatter.smooth(x=df_GrupoControl_filtrados$Separacion, 
               y=df_GrupoControl_filtrados$Difer, 
               main="Difer ~ Separacion"
)

#Divido el area del gráfico en 2 columnas
par(mfrow=c(1, 2))  

#Boxplot para la variable Ratio Pos/Pre
boxplot(df_GrupoControl_filtrados$Ratio, 
        main="Ratio", 
        sub=paste("Outlier rows: ", boxplot.stats(df_GrupoControl_filtrados$Ratio)$out)
)  
#Boxplot para la variable Difer Pos-Pre
boxplot(df_GrupoControl_filtrados$Difer, 
        main="Diferencia", 
        sub=paste("Outlier rows: ", 
                  boxplot.stats(df_GrupoControl_filtrados$Difer)$out
        )
)  
#Grafico de densidad
par(mfrow=c(1, 2))  
#Densidad 
plot(density(df_GrupoControl_filtrados$Ratio), 
     
     main="Density Plot: Ratio", 
     
     ylab="Frecuencia", 
     
     sub=paste("Skewness:", 
               
               round(e1071::skewness(df_GrupoControl_filtrados$Ratio), 2)
     )
)  

polygon(density(df_GrupoControl_filtrados$Ratio), col="red")

plot(density(df_GrupoControl_filtrados$Difer), 
     
     main="Density Plot: Diferencia", 
     
     ylab="",
     
     sub=paste("Skewness:", round(e1071::skewness(df_GrupoControl_filtrados$Difer), 2))
     
)  

polygon(density(df_GrupoControl_filtrados$Difer), col="red")

correlation_ratio <- cor(df_GrupoControl_filtrados$Ratio, df_GrupoControl_filtrados$Separacion) 

correlation_difer <- cor(df_GrupoControl_filtrados$Difer, df_GrupoControl_filtrados$Separacion) 


lm_Ratio <-  lm(df_GrupoControl_filtrados$Ratio ~ df_GrupoControl_filtrados$Separacion)

summary(lm_Ratio)

lm_Difer <-  lm(df_GrupoControl_filtrados$Difer ~ df_GrupoControl_filtrados$Separacion)

summary(lm_Difer)


#GRUPO CON CARGA ATENCIONAL

#1- Dataframe Grupo con Carga Atencional y nuevas variables 
df_GrupoCarga_filtrados <-  df_datos_filtrados %>%
  
  filter(grupo == "lt") %>%
  
  group_by(Separacion, condicion) %>%
  
  summarise(MediaAciertos = mean(p), DesvAciertos = sd(p)) %>%
  
  mutate(Ratio =  
           MediaAciertos[which(condicion== "pos")]/ 
           MediaAciertos[which(condicion == "pre")],
         
         Difer = 
           MediaAciertos[which(condicion== "pos")]- 
           MediaAciertos[which(condicion == "pre")]
  )
#3- Genero nuevas variables con el porcentaje de aciertos Pre y Pos entrenamiento 
#para luego poder organizar el dataframe coomo lo hizo Jose           
df_GrupoCarga_filtrados <-  df_GrupoCarga_filtrados %>% 
  
  mutate( PreMedia = MediaAciertos[which(condicion == "pre")],
          PosMedia = MediaAciertos[which(condicion == "pos")],
          PreDesv  = DesvAciertos[which(condicion  == "pre")],
          PosDesv  = DesvAciertos[which(condicion  == "pos")] 
  )

#4- Elimino columnas y valores duplicados
df_GrupoCarga_filtrados[2:4] <- list(NULL)
df_GrupoCarga_filtrados <- unique(df_GrupoCarga_filtrados)


# Modelos lineales
#1 Gráficos 
par(mfrow=c(1, 2))  
scatter.smooth(x=df_GrupoCarga_filtrados$Separacion, 
               y=df_GrupoCarga_filtrados$Ratio, 
               main="Ratio ~ Separacion"
)

scatter.smooth(x=df_GrupoCarga_filtrados$Separacion, 
               y=df_GrupoCarga_filtrados$Difer, 
               main="Difer ~ Separacion"
)

#Divido el area del gráfico en 2 columnas
par(mfrow=c(1, 2))  

#Boxplot para la variable Ratio Pos/Pre
boxplot(df_GrupoCarga_filtrados$Ratio, 
        main="Ratio", 
        sub=paste("Outlier rows: ", 
                  boxplot.stats(df_GrupoCarga_filtrados$Ratio)$out)
)  
#Boxplot para la variable Difer Pos-Pre
boxplot(df_GrupoCarga_filtrados$Difer, 
        main="Diferencia", 
        sub=paste("Outlier rows: ", 
                  boxplot.stats(df_GrupoCarga_filtrados$Difer)$out
        )
)  
#Grafico de densidad
par(mfrow=c(1, 2))  
#Densidad 
plot(density(df_GrupoCarga_filtrados$Ratio), 
     
     main="Density Plot: Ratio", 
     
     ylab="Frecuencia", 
     
     sub=paste("Skewness:", 
               
               round(e1071::skewness(df_GrupoCarga_filtrados$Ratio), 2)
     )
)  

polygon(density(df_GrupoCarga_filtrados$Ratio), col="red")

plot(density(df_GrupoCarga_filtrados$Difer), 
     
     main="Density Plot: Diferencia", 
     
     ylab="",
     
     sub=paste("Skewness:", round(e1071::skewness(df_GrupoControl_filtrados$Difer), 2))
     
)  

polygon(density(df_GrupoCarga_filtrados$Difer), col="red")

correlation_ratio <- cor(df_GrupoCarga_filtrados$Ratio, df_GrupoCarga_filtrados$Separacion) 

correlation_difer <- cor(df_GrupoCarga_filtrados$Difer, df_GrupoCarga_filtrados$Separacion) 


lm_RatioCarga <-  lm(df_GrupoCarga_filtrados$Ratio ~ df_GrupoCarga_filtrados$Separacion)

summary(lm_RatioCarga)

lm_DiferCarga <-  lm(df_GrupoCarga_filtrados$Difer ~ df_GrupoCarga_filtrados$Separacion)

summary(lm_DiferCarga)

write.csv(df_datos_filtrados,"C:\\Users\\Anibal\\Documents\\R\\attentional-window\\datosVA_v.2.csv", row.names = TRUE)



# PROCESAMIENTO GRUPO CONTROL ----------------------------------------------------
# Creo un dataframe Grupo Control
df_GrupoControl <-  df_datos %>%
                    
                    filter(Grupo == "cl", Fijacion == "SI") %>%
                    
                    group_by(Separacion, Condicion) %>%
  
                    summarise(MediaAciertos = mean(p), DesvAciertos = sd(p)) %>%
  
                    mutate(Ratio = MediaAciertos[which(Condicion== "pos")]/                                        MediaAciertos[which(Condicion == "pre")],
               
                           Difer = MediaAciertos[which(Condicion== "pos")]-                                        MediaAciertos[which(Condicion == "pre")]
                           )
                
#2 Genero nuevas variables con el porcentaje de aciertos Pre y Pos entrenamiento para luego poder organizar el dataframe coomo lo hizo Jose           
df_GrupoControl <-  df_GrupoControl %>% 
  
                    mutate( PreMedia = MediaAciertos[which(Condicion == "pre")],
                            PosMedia = MediaAciertos[which(Condicion == "pos")],
                            PreDesv  = DesvAciertos[which(Condicion  == "pre")],
                            PosDesv  = DesvAciertos[which(Condicion  == "pos")] 
                          )
#2.1 Elimino columnas y valores dupplicados
df_GrupoControl[2:4] <- list(NULL)
df_GrupoControl <- unique(df_GrupoControl)
                            
#3 Modelos lineales
#3.1 Gráficos 
par(mfrow=c(1, 2))  
scatter.smooth(x=df_GrupoControl$Separacion, 
               y=df_GrupoControl$Ratio, 
               main="Ratio ~ Separacion"
               )

scatter.smooth(x=df_GrupoControl$Separacion, 
               y=df_GrupoControl$Difer, 
               main="Difer ~ Separacion"
               )

#Divido el area del gráfico en 2 columnas
par(mfrow=c(1, 2))  

#Boxplor para la variable Ratio Pos/Pre
boxplot(df_GrupoControl$Ratio, 
        main="Ratio", 
        sub=paste("Outlier rows: ", boxplot.stats(df_GrupoControl$Ratio)$out)
        )  
#Boxplot para la variable Difer Pos-Pre
boxplot(df_GrupoControl$Difer, 
        main="Diferencia", 
        sub=paste("Outlier rows: ", 
                  boxplot.stats(df_GrupoControl$Difer)$out
                  )
        )  
#Grafico de densidad
par(mfrow=c(1, 2))  
#Densidad 
plot(density(df_GrupoControl$Ratio), 
     
     main="Density Plot: Ratio", 
     
     ylab="Frecuencia", 
     
     sub=paste("Skewness:", 
     
               round(e1071::skewness(df_GrupoControl$Ratio), 2)
               )
     )  

polygon(density(df_GrupoControl$Ratio), col="red")

plot(density(df_GrupoControl$Difer), 
     
     main="Density Plot: Diferencia", 
     
     ylab="",
     
     sub=paste("Skewness:", round(e1071::skewness(df_GrupoControl$Difer), 2))
     
     )  

polygon(density(df_GrupoControl$Difer), col="red")

correlation_ratio <- cor(df_GrupoControl$Ratio, df_GrupoControl$Separacion) 

correlation_difer <- cor(df_GrupoControl$Difer, df_GrupoControl$Separacion) 


lm_Ratio <-  lm(df_GrupoControl$Ratio ~ df_GrupoControl$Separacion)

summary(lm_Ratio)

lm_Difer <-  lm(df_GrupoControl$Difer ~ df_GrupoControl$Separacion)

summary(lm_Difer)

