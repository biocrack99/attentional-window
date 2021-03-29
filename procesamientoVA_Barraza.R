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
library(fmsb)

#Listas para guardar los datos segun la cantidad de obsevadores
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
rm(averages, df_datos_filtrados, df_gaze, ls_gaze_x, ls_gaze_y, tbl, tbl_gaze, tbl_sin_gaze ) 



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
rm(averages, data_mat, datList, df_datos_filtrados, datos_sin_Gaze, matlabFile, tabla, tabla_total) 


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
rm(averages, df_datos_filtrados, df_gaze, ls_gaze_x, ls_gaze_y, tbl, tbl_gaze, tbl_sin_gaze ) 



# CARGO DATOS HOCKEY POS ENTRENAMIENTO ------------------------------------
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


# GAZE LIBRERIA saccades ------------------------------------------
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
df_porcetaje_trials <- 1:30
for (i in seq_along(list_fixations)){
  
  n <- list_fixations[[i]] %>% 
    group_by(trial) %>%
    dplyr::summarise (n=n())
# Vector con la cantidad de trials donde el sujeto realiza
# una o mas fijaciones mayor a 0.28 seg  
  porcentajeTrialFinal <-  nrow(n)/336
  #list_fixations[[i]]$porcentajeTrialOk <- porcentajeTrialFinal  
# Busco los trials para descartar
  trialDescartar <- which(is.na(match(vr_index_trial,n$trial)))               
  list_fixations[[i]] <- list(list_fixations[[i]], 
                              porcentajeTrialFinal, 
                              trialDescartar)
  df_porcetaje_trials[i] <- porcentajeTrialFinal
}

df_porcetaje_trials <- data.frame(obs, 
                                  porcentaje = df_porcetaje_trials,
                                  condicion = c('pre', 'pre', 'pre',
                                                'pre', 'pre', 'pre',
                                                'pre', 'pre', 'pre',
                                                'pre', 'pre', 'pre',
                                                'pre', 'pre', 'pre',
                                                'pos', 'pos', 'pos',
                                                'pos', 'pos', 'pos',
                                                'pos', 'pos', 'pos',
                                                'pos', 'pos', 'pos',
                                                'pos', 'pos', 'pos'))

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

#6.7 Summarize para el data frame de los porcentajes, quiero ver
# si hay cambio o no entre pre y pos

df_media_porcentaje <- df_porcetaje_trials %>%
  
  group_by(condicion) %>% summarise(media = median(porcentaje),
                                    desviacion = sd(porcentaje),
                                    mediana = median(porcentaje))


#PROCESAMIENTO GRUPO CONTROL ----------------------------------------------------

#1- Creo un dataframe con la lista de datos
df_datos_filtrados <- ldply (list_datos_filtrados, data.frame)
df_datos_filtrados <- df_datos_filtrados[,-1]

#2- Dataframe Grupo Control y nuevas variables 
df_GrupoControl_filtrados <-  df_datos_filtrados %>%
  
  filter(grupo == "cl" ) %>%
  
  group_by(Separacion, condicion) %>%
  
  summarise(MediaAciertos = mean(p), DesvAciertos = sd(p)) %>%
  
  mutate(Ratio =  
           MediaAciertos[which(condicion == "pos")]/ 
           MediaAciertos[which(condicion == "pre")],
         
         Difer = 
           MediaAciertos[which(condicion== "pos")]- 
           MediaAciertos[which(condicion == "pre")]
  )

df_GrupoControl_filtrados_dir <-  df_datos_filtrados %>%
  
  filter(grupo == "cl") %>%
  
  group_by(Direccion, condicion) %>%
  
  summarise(MediaAciertos = mean(p), DesvAciertos = sd(p)) %>%
  
  mutate(Ratio =  
           MediaAciertos[which(condicion== "pos")]/ 
           MediaAciertos[which(condicion == "pre")],
         
  )

#3- Genero nuevas variables con el porcentaje de aciertos Pre y Pos entrenamiento 
#para luego poder organizar el dataframe coomo lo hizo Jose           
df_GrupoControl_filtrados <-  df_GrupoControl_filtrados %>% 
  
  mutate( PreMedia = MediaAciertos[which(condicion == "pre")],
          PosMedia = MediaAciertos[which(condicion == "pos")],
          PreDesv  = DesvAciertos[which(condicion  == "pre")],
          PosDesv  = DesvAciertos[which(condicion  == "pos")] 
  )

df_GrupoControl_filtrados_dir <-  df_GrupoControl_filtrados_dir %>% 
  
  mutate( PreMedia = MediaAciertos[which(condicion == "pre")],
          PosMedia = MediaAciertos[which(condicion == "pos")],
          PreDesv  = DesvAciertos[which(condicion  == "pre")],
          PosDesv  = DesvAciertos[which(condicion  == "pos")] 
  )


#4- Elimino columnas y valores duplicados
df_GrupoControl_filtrados[2:4] <- list(NULL)
df_GrupoControl_filtrados <- unique(df_GrupoControl_filtrados)
df_GrupoControl_filtrados_dir[2:4] <- list(NULL)


# Modelos lineales
correlation_ratio <- cor(df_GrupoControl_filtrados$Ratio, df_GrupoControl_filtrados$Separacion) 

lm_RatioControl <-  lm(df_GrupoControl_filtrados$Ratio ~ df_GrupoControl_filtrados$Separacion)

lm_RatioControl_dir <-  lm(df_GrupoControl_filtrados_dir$Ratio ~ df_GrupoControl_filtrados_dir$Direccion)


summary(lm_RatioControl)
summary(lm_RatioControl_dir)

# Gráficos

ggplot(df_GrupoControl_filtrados, 
       aes(x = df_GrupoControl_filtrados$Separacion,
           y = df_GrupoControl_filtrados$Ratio)) + 
  geom_point(color = "red") +
  stat_smooth(method = "lm", col = "red")+
  labs(title = "Control Group", 
       x = "Separation[°]",
       y = "Ratio Pos/Pre [-]") +
  ylim(0,2)

#Radar
#1- Grafico radar con libreria fmsb
create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
  
}


max_min <- data.frame(
  Grados_90 = c(1, 0), Grados_135 = c(1, 0), Grados_180 = c(1, 0),
  Grados_225 = c(1, 0),  Grados_270 = c(1, 0), Grados_315 = c(1, 0), 
  Grados_0 = c(1, 0), Grados_45 = c(1, 0))

rownames(max_min) <- c("Max", "Min")

df_GrupoControl_radar <- data.frame(row.names = c("Pre", "Pos"), 
                                  Grados_90= t(df_GrupoControl_filtrados_dir[7,3:4]), 
                                  Grados_135 = t(df_GrupoControl_filtrados_dir[3,3:4]),
                                  Grados_180 = t(df_GrupoControl_filtrados_dir[1,3:4]), 
                                  Grados_225 = t(df_GrupoControl_filtrados_dir[5,3:4]), 
                                  Grados_270= t(df_GrupoControl_filtrados_dir[7,3:4]), 
                                  Grados_315 = t(df_GrupoControl_filtrados_dir[3,3:4]),
                                  Grados_0 = t(df_GrupoControl_filtrados_dir[1,3:4]), 
                                  Grados_45 = t(df_GrupoControl_filtrados_dir[5,3:4]))

df_GrupoControl_radar <- rbind(max_min, df_GrupoControl_radar)
#Condicion Pre y Pos en un mismo gráfico
op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = df_GrupoControl_radar, caxislabels = c(0, 0.25, 0.50, 0.75, 1.0),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x = "left", legend = rownames(df_GrupoControl_radar[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)

#Grafico radar para ventana atencional

#1- Creo dataframe con los datos de direccion
df_ventana_GrupoControl <- df_datos_filtrados %>%
  filter(grupo == "cl") %>%
  group_by(Direccion, Separacion, condicion) %>%
  summarise(MediaAciertos = mean(p))
  
#2- Modelo lineal
#Pre
lm_ventana_GrupoControl_pre_0 <- lm(filter(df_ventana_GrupoControl, Direccion == "0" & condicion == "pre")$MediaAciertos 
                                ~ filter(df_ventana_GrupoControl, Direccion == "0" & condicion == "pre")$Separacion)

lm_ventana_GrupoControl_pre_45 <- lm(filter(df_ventana_GrupoControl, Direccion == "45" & condicion == "pre")$MediaAciertos 
                                    ~ filter(df_ventana_GrupoControl, Direccion == "45" & condicion == "pre")$Separacion)

lm_ventana_GrupoControl_pre_90 <- lm(filter(df_ventana_GrupoControl, Direccion == "90" & condicion == "pre")$MediaAciertos 
                                    ~ filter(df_ventana_GrupoControl, Direccion == "90" & condicion == "pre")$Separacion)

lm_ventana_GrupoControl_pre_135 <- lm(filter(df_ventana_GrupoControl, Direccion == "135" & condicion == "pre")$MediaAciertos 
                                    ~ filter(df_ventana_GrupoControl, Direccion == "135" & condicion == "pre")$Separacion)

vc_coeficientes <- c(lm_ventana_GrupoControl_pre_90$coefficients,
                     lm_ventana_GrupoControl_pre_135$coefficients,
                     lm_ventana_GrupoControl_pre_0$coefficients,
                     lm_ventana_GrupoControl_pre_45$coefficients,
                     lm_ventana_GrupoControl_pre_90$coefficients,
                     lm_ventana_GrupoControl_pre_135$coefficients,
                     lm_ventana_GrupoControl_pre_0$coefficients,
                     lm_ventana_GrupoControl_pre_45$coefficients)



for (i in seq_along(df_GrupoControl_radar[3,])){
  
  df_GrupoControl_radar[3,i] <- (0.6 - vc_coeficientes[(2*i)-1])/vc_coeficientes[(2*i)]

}

# Condicion Pos
lm_ventana_GrupoControl_pos_0 <- lm(filter(df_ventana_GrupoControl, Direccion == "0" & condicion == "pos")$MediaAciertos 
                                    ~ filter(df_ventana_GrupoControl, Direccion == "0" & condicion == "pos")$Separacion)

lm_ventana_GrupoControl_pos_45 <- lm(filter(df_ventana_GrupoControl, Direccion == "45" & condicion == "pos")$MediaAciertos 
                                     ~ filter(df_ventana_GrupoControl, Direccion == "45" & condicion == "pos")$Separacion)

lm_ventana_GrupoControl_pos_90 <- lm(filter(df_ventana_GrupoControl, Direccion == "90" & condicion == "pos")$MediaAciertos 
                                     ~ filter(df_ventana_GrupoControl, Direccion == "90" & condicion == "pos")$Separacion)

lm_ventana_GrupoControl_pos_135 <- lm(filter(df_ventana_GrupoControl, Direccion == "135" & condicion == "pos")$MediaAciertos 
                                      ~ filter(df_ventana_GrupoControl, Direccion == "135" & condicion == "pos")$Separacion)

vc_coeficientes <- c(lm_ventana_GrupoControl_pos_90$coefficients,
                     lm_ventana_GrupoControl_pos_135$coefficients,
                     lm_ventana_GrupoControl_pos_0$coefficients,
                     lm_ventana_GrupoControl_pos_45$coefficients,
                     lm_ventana_GrupoControl_pos_90$coefficients,
                     lm_ventana_GrupoControl_pos_135$coefficients,
                     lm_ventana_GrupoControl_pos_0$coefficients,
                     lm_ventana_GrupoControl_pos_45$coefficients)



for (i in seq_along(df_GrupoControl_radar[4,])){
  
  df_GrupoControl_radar[4,i] <- (0.6 - vc_coeficientes[(2*i)-1])/vc_coeficientes[(2*i)]
  
}

df_GrupoControl_radar[1,] <- df_GrupoControl_radar[1,]*10


op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = df_GrupoControl_radar, caxislabels = c(0, 5, 10, 15, 20),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x = "left", legend = rownames(df_GrupoControl_radar[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)


#PROCESAMIENTO GRUPO CON CARGA ATENCIONAL-------------------------------

#1- Dataframe Grupo con Carga Atencional y nuevas variables 
# Respecto a la separacion
df_GrupoCarga_filtrados <-  df_datos_filtrados %>%
  
  filter(grupo == "lt") %>%
  
  group_by(Separacion, condicion) %>%
  
  summarise(MediaAciertos = mean(p), DesvAciertos = sd(p)) %>%
  
  mutate(Ratio =  
           MediaAciertos[which(condicion== "pos")]/ 
           MediaAciertos[which(condicion == "pre")],
         
         
  )

df_GrupoCarga_filtrados_dir <-  df_datos_filtrados %>%
  
  filter(grupo == "lt") %>%
  
  group_by(Direccion, condicion) %>%
  
  summarise(MediaAciertos = mean(p), DesvAciertos = sd(p)) %>%
  
  mutate(Ratio =  
           MediaAciertos[which(condicion== "pos")]/ 
           MediaAciertos[which(condicion == "pre")],
         
         
  )


#3- Genero nuevas variables con el porcentaje de aciertos Pre y Pos entrenamiento 
#para luego poder organizar el dataframe coomo lo hizo Jose           
df_GrupoCarga_filtrados <-  df_GrupoCarga_filtrados %>% 
  
  mutate( PreMedia = MediaAciertos[which(condicion == "pre")],
          PosMedia = MediaAciertos[which(condicion == "pos")],
          PreDesv  = DesvAciertos[which(condicion  == "pre")],
          PosDesv  = DesvAciertos[which(condicion  == "pos")] 
  )

df_GrupoCarga_filtrados_dir <-  df_GrupoCarga_filtrados_dir %>% 
  
  mutate( PreMedia = MediaAciertos[which(condicion == "pre")],
          PosMedia = MediaAciertos[which(condicion == "pos")],
          PreDesv  = DesvAciertos[which(condicion  == "pre")],
          PosDesv  = DesvAciertos[which(condicion  == "pos")] 
  )


#4- Elimino columnas y valores duplicados
df_GrupoCarga_filtrados[2:4] <- list(NULL)
df_GrupoCarga_filtrados_dir[2:4] <- list(NULL)
df_GrupoCarga_filtrados <- unique(df_GrupoCarga_filtrados)


#5- Gráficos
#Lineal
ggplot() + 
  geom_point(data = df_GrupoControl_filtrados, 
             aes(Separacion, Ratio), color = "red") +
  stat_smooth(data = df_GrupoControl_filtrados, 
              aes(Separacion,Ratio),
              method = "lm", col = "red")+
  geom_point(data = df_GrupoCarga_filtrados, 
             aes(Separacion,
                 Ratio), color="blue")+
  stat_smooth(data = df_GrupoCarga_filtrados,
              aes(Separacion,
                  Ratio),
              method = "lm", col = "blue") +
  labs(title = "Control and Load Group", 
       x = "Separation[°]",
       y = "Ratio Pos/Pre [-]") +
  ylim(0,2)

#Radar

df_GrupoCarga_radar <- data.frame(row.names = c("Pre", "Pos"), 
                        Grados_90= t(df_GrupoCarga_filtrados_dir[7,3:4]), 
                        Grados_135 = t(df_GrupoCarga_filtrados_dir[3,3:4]),
                        Grados_180 = t(df_GrupoCarga_filtrados_dir[1,3:4]), 
                        Grados_225 = t(df_GrupoCarga_filtrados_dir[5,3:4]), 
                        Grados_270= t(df_GrupoCarga_filtrados_dir[7,3:4]), 
                        Grados_315 = t(df_GrupoCarga_filtrados_dir[3,3:4]),
                        Grados_0 = t(df_GrupoCarga_filtrados_dir[1,3:4]), 
                        Grados_45 = t(df_GrupoCarga_filtrados_dir[5,3:4]))

df_GrupoCarga_radar <- rbind(max_min, df_GrupoCarga_radar)
#Condicion Pre y Pos en un mismo gráfico
op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = df_GrupoCarga_radar, caxislabels = c(0, 0.25, 0.50, 0.75, 1.0),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x = "left", legend = rownames(df_GrupoCarga_radar[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)

#Grafico radar para ventana atencional

#1- Creo dataframe con los datos de direccion

df_ventana_GrupoCarga <- df_datos_filtrados %>%
  filter(grupo == "lt") %>%
  group_by(Direccion, Separacion, condicion) %>%
  summarise(MediaAciertos = mean(p))

#2- Modelo lineal
#Pre
lm_ventana_GrupoCarga_pre_0 <- lm(filter(df_ventana_GrupoCarga, Direccion == "0" & condicion == "pre")$MediaAciertos 
                                    ~ filter(df_ventana_GrupoCarga, Direccion == "0" & condicion == "pre")$Separacion)

lm_ventana_GrupoCarga_pre_45 <- lm(filter(df_ventana_GrupoCarga, Direccion == "45" & condicion == "pre")$MediaAciertos 
                                     ~ filter(df_ventana_GrupoCarga, Direccion == "45" & condicion == "pre")$Separacion)

lm_ventana_GrupoCarga_pre_90 <- lm(filter(df_ventana_GrupoCarga, Direccion == "90" & condicion == "pre")$MediaAciertos 
                                     ~ filter(df_ventana_GrupoCarga, Direccion == "90" & condicion == "pre")$Separacion)

lm_ventana_GrupoCarga_pre_135 <- lm(filter(df_ventana_GrupoCarga, Direccion == "135" & condicion == "pre")$MediaAciertos 
                                      ~ filter(df_ventana_GrupoCarga, Direccion == "135" & condicion == "pre")$Separacion)

vc_coeficientes <- c(lm_ventana_GrupoCarga_pre_90$coefficients,
                     lm_ventana_GrupoCarga_pre_135$coefficients,
                     lm_ventana_GrupoCarga_pre_0$coefficients,
                     lm_ventana_GrupoCarga_pre_45$coefficients,
                     lm_ventana_GrupoCarga_pre_90$coefficients,
                     lm_ventana_GrupoCarga_pre_135$coefficients,
                     lm_ventana_GrupoCarga_pre_0$coefficients,
                     lm_ventana_GrupoCarga_pre_45$coefficients)



for (i in seq_along(df_GrupoCarga_radar[3,])){
  
  df_GrupoCarga_radar[3,i] <- (0.6 - vc_coeficientes[(2*i)-1])/vc_coeficientes[(2*i)]
  
}

# Condicion Pos
lm_ventana_GrupoCarga_pos_0 <- lm(filter(df_ventana_GrupoCarga, Direccion == "0" & condicion == "pos")$MediaAciertos 
                                    ~ filter(df_ventana_GrupoCarga, Direccion == "0" & condicion == "pos")$Separacion)

lm_ventana_GrupoCarga_pos_45 <- lm(filter(df_ventana_GrupoCarga, Direccion == "45" & condicion == "pos")$MediaAciertos 
                                     ~ filter(df_ventana_GrupoCarga, Direccion == "45" & condicion == "pos")$Separacion)

lm_ventana_GrupoCarga_pos_90 <- lm(filter(df_ventana_GrupoCarga, Direccion == "90" & condicion == "pos")$MediaAciertos 
                                     ~ filter(df_ventana_GrupoCarga, Direccion == "90" & condicion == "pos")$Separacion)

lm_ventana_GrupoCarga_pos_135 <- lm(filter(df_ventana_GrupoCarga, Direccion == "135" & condicion == "pos")$MediaAciertos 
                                      ~ filter(df_ventana_GrupoCarga, Direccion == "135" & condicion == "pos")$Separacion)

vc_coeficientes <- c(lm_ventana_GrupoCarga_pos_90$coefficients,
                     lm_ventana_GrupoCarga_pos_135$coefficients,
                     lm_ventana_GrupoCarga_pos_0$coefficients,
                     lm_ventana_GrupoCarga_pos_45$coefficients,
                     lm_ventana_GrupoCarga_pos_90$coefficients,
                     lm_ventana_GrupoCarga_pos_135$coefficients,
                     lm_ventana_GrupoCarga_pos_0$coefficients,
                     lm_ventana_GrupoCarga_pos_45$coefficients)



for (i in seq_along(df_GrupoCarga_radar[4,])){
  
  df_GrupoCarga_radar[4,i] <- (0.6 - vc_coeficientes[(2*i)-1])/vc_coeficientes[(2*i)]
  
}

df_GrupoCarga_radar[1,] <- df_GrupoCarga_radar[1,]*10


op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = df_GrupoCarga_radar, caxislabels = c(0, 5, 10, 15, 20),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x = "left", legend = rownames(df_GrupoCarga_radar[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)


  
#6- Modelo  
lm_RatioCarga <-  lm(df_GrupoCarga_filtrados$Ratio ~ df_GrupoCarga_filtrados$Separacion)

summary(lm_RatioCarga)

#write.csv(df_datos_filtrados,"C:\\Users\\Anibal\\Documents\\R\\attentional-window\\datosVA_v.2.csv", row.names = TRUE)


#PROCESAMIENTO GRUPO CON TIEMPO DE REACCION-------------------------------

#1- Dataframe Grupo Tiempo de Reaccion y nuevas variables 
df_GrupoReaccion_filtrados <-  df_datos_filtrados %>%
  
  filter(grupo == "rt") %>%
  
  group_by(Separacion, condicion) %>%
  
  summarise(MediaAciertos = mean(p), DesvAciertos = sd(p)) %>%
  
  mutate(Ratio =  
           MediaAciertos[which(condicion== "pos")]/ 
           MediaAciertos[which(condicion == "pre")]
         )
#Direccion
df_GrupoReaccion_filtrados_dir <-  df_datos_filtrados %>%
  
  filter(grupo == "rt") %>%
  
  group_by(Direccion, condicion) %>%
  
  summarise(MediaAciertos = mean(p), DesvAciertos = sd(p)) %>%
  
  mutate(Ratio =  
           MediaAciertos[which(condicion== "pos")]/ 
           MediaAciertos[which(condicion == "pre")]
  )



#3- Genero nuevas variables con el porcentaje de aciertos Pre y Pos entrenamiento 
#para luego poder organizar el dataframe coomo lo hizo Jose           
df_GrupoReaccion_filtrados <-  df_GrupoReaccion_filtrados %>% 
  
  mutate( PreMedia = MediaAciertos[which(condicion == "pre")],
          PosMedia = MediaAciertos[which(condicion == "pos")],
          PreDesv  = DesvAciertos[which(condicion  == "pre")],
          PosDesv  = DesvAciertos[which(condicion  == "pos")] 
  )
df_GrupoReaccion_filtrados_dir <-  df_GrupoReaccion_filtrados_dir %>% 
  
  mutate( PreMedia = MediaAciertos[which(condicion == "pre")],
          PosMedia = MediaAciertos[which(condicion == "pos")],
          PreDesv  = DesvAciertos[which(condicion  == "pre")],
          PosDesv  = DesvAciertos[which(condicion  == "pos")] 
  )

#4- Elimino columnas y valores duplicados
df_GrupoReaccion_filtrados[2:4] <- list(NULL)
df_GrupoReaccion_filtrados <- unique(df_GrupoReaccion_filtrados)
df_GrupoReaccion_filtrados_dir[2:4] <- list(NULL)

#Gráficos 

ggplot() + 
  geom_point(data = df_GrupoControl_filtrados, 
             aes(Separacion, Ratio), color = "red") +
  stat_smooth(data = df_GrupoControl_filtrados, 
              aes(Separacion,Ratio),
              method = "lm", col = "red")+
  geom_point(data = df_GrupoReaccion_filtrados, 
             aes(Separacion,
                 Ratio), color = "black")+
  stat_smooth(data = df_GrupoReaccion_filtrados,
              aes(Separacion,
                  Ratio),
              method = "lm", col = "black") +
  labs(title = "Control and Reaction Group", 
       x = "Separation[°]",
       y = "Ratio Pos/Pre [-]") +
  ylim(0,2) 

#Radar
df_GrupoReaccion_radar <- data.frame(row.names = c("Pre", "Pos"), 
                                  Grados_90= t(df_GrupoReaccion_filtrados_dir[7,3:4]), 
                                  Grados_135 = t(df_GrupoReaccion_filtrados_dir[3,3:4]),
                                  Grados_180 = t(df_GrupoReaccion_filtrados_dir[1,3:4]), 
                                  Grados_225 = t(df_GrupoReaccion_filtrados_dir[5,3:4]), 
                                  Grados_270= t(df_GrupoReaccion_filtrados_dir[7,3:4]), 
                                  Grados_315 = t(df_GrupoReaccion_filtrados_dir[3,3:4]),
                                  Grados_0 = t(df_GrupoReaccion_filtrados_dir[1,3:4]), 
                                  Grados_45 = t(df_GrupoReaccion_filtrados_dir[5,3:4]))

df_GrupoReaccion_radar <- rbind(max_min, df_GrupoReaccion_radar)
#Condicion Pre y Pos en un mismo gráfico
op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = df_GrupoReaccion_radar, caxislabels = c(0, 0.25, 0.50, 0.75, 1.0),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x = "left", legend = rownames(df_GrupoReaccion_radar[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)
#Grafico radar para ventana atencional

#1- Creo dataframe con los datos de direccion

df_ventana_GrupoReaccion <- df_datos_filtrados %>%
  filter(grupo == "rt" & Separacion != "3") %>%
  group_by(Direccion, Separacion, condicion) %>%
  summarise(MediaAciertos = mean(p))

#2- Modelo lineal
#Pre
lm_ventana_GrupoReaccion_pre_0 <- lm(filter(df_ventana_GrupoReaccion, Direccion == "0" & condicion == "pre")$MediaAciertos 
                                    ~ filter(df_ventana_GrupoReaccion, Direccion == "0" & condicion == "pre")$Separacion)

lm_ventana_GrupoReaccion_pre_45 <- lm(filter(df_ventana_GrupoReaccion, Direccion == "45" & condicion == "pre")$MediaAciertos 
                                     ~ filter(df_ventana_GrupoReaccion, Direccion == "45" & condicion == "pre")$Separacion)

lm_ventana_GrupoReaccion_pre_90 <- lm(filter(df_ventana_GrupoReaccion, Direccion == "90" & condicion == "pre")$MediaAciertos 
                                     ~ filter(df_ventana_GrupoReaccion, Direccion == "90" & condicion == "pre")$Separacion)

lm_ventana_GrupoReaccion_pre_135 <- lm(filter(df_ventana_GrupoReaccion, Direccion == "135" & condicion == "pre")$MediaAciertos 
                                      ~ filter(df_ventana_GrupoReaccion, Direccion == "135" & condicion == "pre")$Separacion)

vc_coeficientes <- c(lm_ventana_GrupoReaccion_pre_90$coefficients,
                     lm_ventana_GrupoReaccion_pre_135$coefficients,
                     lm_ventana_GrupoReaccion_pre_0$coefficients,
                     lm_ventana_GrupoReaccion_pre_45$coefficients,
                     lm_ventana_GrupoReaccion_pre_90$coefficients,
                     lm_ventana_GrupoReaccion_pre_135$coefficients,
                     lm_ventana_GrupoReaccion_pre_0$coefficients,
                     lm_ventana_GrupoReaccion_pre_45$coefficients)



for (i in seq_along(df_GrupoReaccion_radar[3,])){
  
  df_GrupoReaccion_radar[3,i] <- (0.6 - vc_coeficientes[(2*i)-1])/vc_coeficientes[(2*i)]
  
}

# Condicion Pos
lm_ventana_GrupoReaccion_pos_0 <- lm(filter(df_ventana_GrupoReaccion, Direccion == "0" & condicion == "pos")$MediaAciertos 
                                    ~ filter(df_ventana_GrupoReaccion, Direccion == "0" & condicion == "pos")$Separacion)

lm_ventana_GrupoReaccion_pos_45 <- lm(filter(df_ventana_GrupoReaccion, Direccion == "45" & condicion == "pos")$MediaAciertos 
                                     ~ filter(df_ventana_GrupoReaccion, Direccion == "45" & condicion == "pos")$Separacion)

lm_ventana_GrupoReaccion_pos_90 <- lm(filter(df_ventana_GrupoReaccion, Direccion == "90" & condicion == "pos")$MediaAciertos 
                                     ~ filter(df_ventana_GrupoReaccion, Direccion == "90" & condicion == "pos")$Separacion)

lm_ventana_GrupoReaccion_pos_135 <- lm(filter(df_ventana_GrupoReaccion, Direccion == "135" & condicion == "pos")$MediaAciertos 
                                      ~ filter(df_ventana_GrupoReaccion, Direccion == "135" & condicion == "pos")$Separacion)

vc_coeficientes <- c(lm_ventana_GrupoReaccion_pos_90$coefficients,
                     lm_ventana_GrupoReaccion_pos_135$coefficients,
                     lm_ventana_GrupoReaccion_pos_0$coefficients,
                     lm_ventana_GrupoReaccion_pos_45$coefficients,
                     lm_ventana_GrupoReaccion_pos_90$coefficients,
                     lm_ventana_GrupoReaccion_pos_135$coefficients,
                     lm_ventana_GrupoReaccion_pos_0$coefficients,
                     lm_ventana_GrupoReaccion_pos_45$coefficients)



for (i in seq_along(df_GrupoReaccion_radar[4,])){
  
  df_GrupoReaccion_radar[4,i] <- (0.6 - vc_coeficientes[(2*i)-1])/vc_coeficientes[(2*i)]
  
}

df_GrupoReaccion_radar[1,] <- df_GrupoReaccion_radar[1,]*10


op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = df_GrupoReaccion_radar, caxislabels = c(0, 5, 10, 15, 20),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x = "left", legend = rownames(df_GrupoReaccion_radar[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)


#Modelo Lineal

lm_RatioReaccion <-  lm(df_GrupoReaccion_filtrados$Ratio ~ df_GrupoCarga_filtrados$Separacion)

summary(lm_RatioReaccion)


#PROCESAMIENTO GRUPO COMBINADO--------------------------------------------------------------------
#1- Dataframe Grupo Combinado y nuevas variables 
df_GrupoCombinado_filtrados <-  df_datos_filtrados %>%
  
  filter(grupo == "hk") %>%
  
  group_by(Separacion, condicion) %>%
  
  summarise(MediaAciertos = mean(p), DesvAciertos = sd(p)) %>%
  
  mutate(Ratio =  
           MediaAciertos[which(condicion== "pos")]/ 
           MediaAciertos[which(condicion == "pre")]
         )
#Direccion
df_GrupoCombinado_filtrados_dir <-  df_datos_filtrados %>%
  
  filter(grupo == "hk") %>%
  
  group_by(Direccion, condicion) %>%
  
  summarise(MediaAciertos = mean(p), DesvAciertos = sd(p)) %>%
  
  mutate(Ratio =  
           MediaAciertos[which(condicion== "pos")]/ 
           MediaAciertos[which(condicion == "pre")]
  )


#3- Genero nuevas variables con el porcentaje de aciertos Pre y Pos entrenamiento 
#para luego poder organizar el dataframe coomo lo hizo Jose           
df_GrupoCombinado_filtrados <-  df_GrupoCombinado_filtrados %>% 
  
  mutate( PreMedia = MediaAciertos[which(condicion == "pre")],
          PosMedia = MediaAciertos[which(condicion == "pos")],
          PreDesv  = DesvAciertos[which(condicion  == "pre")],
          PosDesv  = DesvAciertos[which(condicion  == "pos")] 
  )

df_GrupoCombinado_filtrados_dir <-  df_GrupoCombinado_filtrados_dir %>% 
  
  mutate( PreMedia = MediaAciertos[which(condicion == "pre")],
          PosMedia = MediaAciertos[which(condicion == "pos")],
          PreDesv  = DesvAciertos[which(condicion  == "pre")],
          PosDesv  = DesvAciertos[which(condicion  == "pos")] 
  )


#4- Elimino columnas y valores duplicados
df_GrupoCombinado_filtrados[2:4] <- list(NULL)
df_GrupoCombinado_filtrados <- unique(df_GrupoCombinado_filtrados)
df_GrupoCombinado_filtrados_dir[2:4] <- list(NULL)


#Graficos

ggplot() + 
  geom_point(data = df_GrupoControl_filtrados, 
             aes(Separacion, Ratio), color = "red") +
  stat_smooth(data = df_GrupoControl_filtrados, 
              aes(Separacion,Ratio),
              method = "lm", col = "red")+
  geom_point(data = df_GrupoCombinado_filtrados, 
             aes(Separacion,
                 Ratio), color = "orange")+
  stat_smooth(data = df_GrupoCombinado_filtrados,
              aes(Separacion,
                  Ratio),
              method = "lm", col = "orange") +
  labs(title = "Control and Combined Task Group", 
       x = "Separation[°]",
       y = "Ratio Pos/Pre [-]") +
  ylim(0,2) 
#Radar
df_GrupoCombinado_radar <- data.frame(row.names = c("Pre", "Pos"), 
                                     Grados_90= t(df_GrupoCombinado_filtrados_dir[7,3:4]), 
                                     Grados_135 = t(df_GrupoCombinado_filtrados_dir[3,3:4]),
                                     Grados_180 = t(df_GrupoCombinado_filtrados_dir[1,3:4]), 
                                     Grados_225 = t(df_GrupoCombinado_filtrados_dir[5,3:4]), 
                                     Grados_270= t(df_GrupoCombinado_filtrados_dir[7,3:4]), 
                                     Grados_315 = t(df_GrupoCombinado_filtrados_dir[3,3:4]),
                                     Grados_0 = t(df_GrupoCombinado_filtrados_dir[1,3:4]), 
                                     Grados_45 = t(df_GrupoCombinado_filtrados_dir[5,3:4]))

df_GrupoCombinado_radar <- rbind(max_min, df_GrupoCombinado_radar)
#Condicion Pre y Pos en un mismo gráfico
op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = df_GrupoCombinado_radar, caxislabels = c(0, 0.25, 0.50, 0.75, 1.0),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x = "left", legend = rownames(df_GrupoCombinado_radar[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)

#Grafico radar para ventana atencional

#1- Creo dataframe con los datos de direccion

df_ventana_GrupoCombinado <- df_datos_filtrados %>%
  filter(grupo == "cl" & Separacion != "3")  %>%
  group_by(Direccion, Separacion, condicion) %>%
  summarise(MediaAciertos = mean(p))

#2- Modelo lineal
#Pre
lm_ventana_GrupoCombinado_pre_0 <- lm(filter(df_ventana_GrupoCombinado, Direccion == "0" & condicion == "pre")$MediaAciertos 
                                    ~ filter(df_ventana_GrupoCombinado, Direccion == "0" & condicion == "pre")$Separacion)

lm_ventana_GrupoCombinado_pre_45 <- lm(filter(df_ventana_GrupoCombinado, Direccion == "45" & condicion == "pre")$MediaAciertos 
                                     ~ filter(df_ventana_GrupoCombinado, Direccion == "45" & condicion == "pre")$Separacion)

lm_ventana_GrupoCombinado_pre_90 <- lm(filter(df_ventana_GrupoCombinado, Direccion == "90" & condicion == "pre")$MediaAciertos 
                                     ~ filter(df_ventana_GrupoCombinado, Direccion == "90" & condicion == "pre")$Separacion)

lm_ventana_GrupoCombinado_pre_135 <- lm(filter(df_ventana_GrupoCombinado, Direccion == "135" & condicion == "pre")$MediaAciertos 
                                      ~ filter(df_ventana_GrupoCombinado, Direccion == "135" & condicion == "pre")$Separacion)

vc_coeficientes <- c(lm_ventana_GrupoCombinado_pre_90$coefficients,
                     lm_ventana_GrupoCombinado_pre_135$coefficients,
                     lm_ventana_GrupoCombinado_pre_0$coefficients,
                     lm_ventana_GrupoCombinado_pre_45$coefficients,
                     lm_ventana_GrupoCombinado_pre_90$coefficients,
                     lm_ventana_GrupoCombinado_pre_135$coefficients,
                     lm_ventana_GrupoCombinado_pre_0$coefficients,
                     lm_ventana_GrupoCombinado_pre_45$coefficients)



for (i in seq_along(df_GrupoCombinado_radar[3,])){
  
  df_GrupoCombinado_radar[3,i] <- (0.6 - vc_coeficientes[(2*i)-1])/vc_coeficientes[(2*i)]
  
}

# Condicion Pos
lm_ventana_GrupoCombinado_pos_0 <- lm(filter(df_ventana_GrupoCombinado, Direccion == "0" & condicion == "pos")$MediaAciertos 
                                    ~ filter(df_ventana_GrupoCombinado, Direccion == "0" & condicion == "pos")$Separacion)

lm_ventana_GrupoCombinado_pos_45 <- lm(filter(df_ventana_GrupoCombinado, Direccion == "45" & condicion == "pos")$MediaAciertos 
                                     ~ filter(df_ventana_GrupoCombinado, Direccion == "45" & condicion == "pos")$Separacion)

lm_ventana_GrupoCombinado_pos_90 <- lm(filter(df_ventana_GrupoCombinado, Direccion == "90" & condicion == "pos")$MediaAciertos 
                                     ~ filter(df_ventana_GrupoCombinado, Direccion == "90" & condicion == "pos")$Separacion)

lm_ventana_GrupoCombinado_pos_135 <- lm(filter(df_ventana_GrupoCombinado, Direccion == "135" & condicion == "pos")$MediaAciertos 
                                      ~ filter(df_ventana_GrupoCombinado, Direccion == "135" & condicion == "pos")$Separacion)

vc_coeficientes <- c(lm_ventana_GrupoCombinado_pos_90$coefficients,
                     lm_ventana_GrupoCombinado_pos_135$coefficients,
                     lm_ventana_GrupoCombinado_pos_0$coefficients,
                     lm_ventana_GrupoCombinado_pos_45$coefficients,
                     lm_ventana_GrupoCombinado_pos_90$coefficients,
                     lm_ventana_GrupoCombinado_pos_135$coefficients,
                     lm_ventana_GrupoCombinado_pos_0$coefficients,
                     lm_ventana_GrupoCombinado_pos_45$coefficients)



for (i in seq_along(df_GrupoCombinado_radar[4,])){
  
  df_GrupoCombinado_radar[4,i] <- (0.6 - vc_coeficientes[(2*i)-1])/vc_coeficientes[(2*i)]
  
}

df_GrupoCombinado_radar[1,] <- df_GrupoCombinado_radar[1,]*10


op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = df_GrupoCombinado_radar, caxislabels = c(0, 5, 10, 15, 20),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x = "left", legend = rownames(df_GrupoCombinado_radar[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)

lm_RatioCombinado <-  lm(df_GrupoCombinado_filtrados$Ratio ~ df_GrupoCombinado_filtrados$Separacion)

summary(lm_RatioCombinado)


#GRAFICOS RADAR ------------------------------------------------------
#1- Creo dataframe con todos los grupos y el valor de la 
#ventana atencional
df_ventana_radar <- rbind(df_GrupoControl_radar, 
                    df_GrupoCarga_radar[-(1:2),], 
                    df_GrupoReaccion_radar[-(1:2),], 
                    df_GrupoCombinado_radar[-(1:2),])

#2- Creo un vector con el valor medio de ventana atencional
#para cada direccion en la condicion Pre 
vr_ventana_mean <- apply(df_ventana_radar[c(3,5,7,9),],
                         2, mean)
#3- Elimino filas que no hacen falta
df_ventana_radar_mean <- rbind(vr_ventana_mean,
                          df_ventana_radar[-c(3,5,7,9),])
#4- Ordeno las filas
df_ventana_radar_mean <- df_ventana_radar_mean[c(2,3,1,4:nrow(df_ventana_radar_mean)),]

#5- Cambio en nombre de las filas
rownames(df_ventana_radar_mean) <- c("Max", "Min",
                                    "Average", "Control",
                                    "Load", "Reaction Time",
                                    "Combined")
#6- Ecualizo el máximo
df_ventana_radar_mean[1,] <- df_ventana_radar_mean[1,]*2


opar <- par() 
# Defino los parametros del gráfico en una grilla de 2x2,
# con los márgenes apropiados
par(mar = rep(0.8,4))
par(mfrow = c(2,2))
# Crea un gráfico radar para cada grupo
for (i in 4:nrow(df_ventana_radar_mean)) {
  radarchart(
    df_ventana_radar_mean[c(1:3, i), ],
    pfcol = c("#99999980",NA),
    pcol= c(NA,2), plty = 1, plwd = 2,
    title = row.names(df_ventana_radar_mean)[i]
  )
}
# Restore the standard par() settings
par <- par(opar) 

#Solo para el grupo Combinado
opar <- par() 
# Defino los parametros del gráfico en una grilla de 2x2,
# con los márgenes apropiados
par(mar = rep(0.8,4))
par(mfrow = c(1,1))
# Crea un gráfico radar para cada grupo
for (i in 4:nrow(df_GrupoCombinado_radar)) {
  radarchart(
    df_GrupoCombinado_radar[c(1:3, i), ],
    pfcol = c("#99999980",NA),
    pcol= c(NA,2), plty = 1, plwd = 2,
    title = "Combined Group"
  )
}
# Restore the standard par() settings
par <- par(opar) 


















#PROCESAMIENTO POR ZONAS--------------------------------------------------------
# La idea es agrupar calculando una media la separacion en diferentes zonas (Cercana, Media, Lejana)
# para poder notar el efecto que hay en las zonas de menor separacion 
grafico_radar_zonas <- function(df, max_min) {
  
  df_zonas <- df %>%
    mutate(Zona = case_when(Separacion <= 8 ~ 'Cercana', 
                            Separacion > 8  & Separacion <=14 ~ 'Media', 
                            Separacion > 14 ~ 'Lejana')) %>%
    group_by(Zona, Direccion, condicion) %>% 
    summarise(MediaAciertos = mean(MediaAciertos))
  #Obtengo los valores de razón de respuestas correctas Zona Cercana
  vr_pre <- t(df_zonas[which(df_zonas$Zona == "Cercana" & 
                               df_zonas$condicion == "pre"), 
                       ncol(df_zonas)])
  vr_pos <- t(df_zonas[which(df_zonas$Zona == "Cercana" & 
                               df_zonas$condicion == "pos"), 
                       ncol(df_zonas)])
  
  #Ordeno y concateneo
  vr_pre <- append(vr_pre[1,c(4,2,1,3)], vr_pre[1,c(4,2,1,3)])
  vr_pos <- append(vr_pos[1,c(4,2,1,3)], vr_pos[1,c(4,2,1,3)])
  # Dataframe Zonas  para grafico radar
  df_zonas_radar <- rbind(max_min, vr_pre, vr_pos)*100
  row.names(df_zonas_radar)[3:4] <- c("Cernana Pre", "Cercana Pos")
  #Obtengo los valores de razón de respuestas correctas Zona Media
  vr_pre <- t(df_zonas[which(df_zonas$Zona == "Media" & 
                               df_zonas$condicion == "pre"), 
                       ncol(df_zonas)])
  vr_pos <- t(df_zonas[which(df_zonas$Zona == "Media" & 
                               df_zonas$condicion == "pos"),
                       ncol(df_zonas)])
  #Ordeno y concateneo
  vr_pre <- append(vr_pre[1,c(4,2,1,3)], vr_pre[1,c(4,2,1,3)])*100
  vr_pos <- append(vr_pos[1,c(4,2,1,3)], vr_pos[1,c(4,2,1,3)])*100
  # Dataframe Zonas  para grafico radar
  df_zonas_radar <- rbind(df_zonas_radar, vr_pre, vr_pos)
  row.names(df_zonas_radar)[5:6] <- c("Media Pre", "Media Pos")
  
  #Obtengo los valores de razón de respuestas correctas Zona Lejana
  vr_pre <- t(df_zonas[which(df_zonas$Zona == "Lejana" & 
                               df_zonas$condicion == "pre"),
                       ncol(df_zonas)])
  vr_pos <- t(df_zonas[which(df_zonas$Zona == "Lejana" & 
                               df_zonas$condicion == "pos"), 
                       ncol(df_zonas)])
  #Ordeno y concateneo
  vr_pre <- append(vr_pre[1,c(4,2,1,3)], vr_pre[1,c(4,2,1,3)])*100
  vr_pos <- append(vr_pos[1,c(4,2,1,3)], vr_pos[1,c(4,2,1,3)])*100
  # Dataframe Zonas Grupo Control para grafico radar
  df_zonas_radar <- rbind(df_zonas_radar, vr_pre, vr_pos)
  row.names(df_zonas_radar)[7:8] <- c("Lejana Pre", "Lejana Pos")
  # Grafico radar
  titles <- c("Near", "Medium", "Far")
  opar <- par() 
  # Define settings for plotting in a 3x4 grid, with appropriate margins:
  par(mar = rep(1.2,4))
  par(mfrow = c(1,3))
  # Produce a radar-chart for each student
  for (i in 1:3) {
    radarchart(
      df_zonas_radar[c(1,2,((i+1)*2)-1,(i+1)*2), ],
      pfcol = c("#99999980",NA),
      pcol= c(NA,2), plty = 1, plwd = 2,
      title = titles[i],
      axistype = 1,
      caxislabels = c(0, 25, 50, 75, 100),
      vlabels = c('90°', '135°', '180°', '225°', '270°', '315°', '0°', '45°'),
      vlcex = 1,
    )
  }
  # legend(
  #   x = "bottom", legend = c('Pretraining', 'Postraining'), horiz = FALSE,
  #   bty = "n", pch = 16 , col =c("#99999980",2),
  #   text.col = "black", cex = 1.3, pt.cex = 2, inset = 0, y.intersp = 1
  # )
  # Restore the standard par() settings
  par <- par(opar) 
  
}

grafico_radar_zonas(df_ventana_GrupoControl, max_min) 
grafico_radar_zonas(df_ventana_GrupoCarga, max_min) 
grafico_radar_zonas(df_ventana_GrupoReaccion, max_min)
grafico_radar_zonas(df_ventana_GrupoCombinado, max_min)










# Seccion pruebas --------------------------------------------------------------
#Comparo los datos sin eliminar los trials debido a la falta
#de fijacion
df_prueba <- df_datos %>% 
  filter(Grupo == "cl") %>%
  group_by(Separacion, Condicion) %>%
  summarise(MediaAciertos = mean(p), DesvAciertos = sd(p)) %>%
  mutate(Ratio =  
           MediaAciertos[which(Condicion== "pos")]/ 
           MediaAciertos[which(Condicion == "pre")],
  )

df_prueba <-  df_prueba %>% 
  mutate( PreMedia = MediaAciertos[which(Condicion == "pre")],
          PosMedia = MediaAciertos[which(Condicion == "pos")],
          PreDesv  = DesvAciertos[which(Condicion  == "pre")],
          PosDesv  = DesvAciertos[which(Condicion  == "pos")] 
  )
  
df_prueba[2:4] <- list(NULL)
df_prueba <- unique(df_prueba)

ggplot() + 
  geom_point(data = df_prueba, 
             aes(Separacion, Ratio), color = "red") +
  stat_smooth(data = df_prueba, 
              aes(Separacion,Ratio),
              method = "lm", col = "red")+
  labs(title = "Control Group", 
       x = "Separation[°]",
       y = "Ratio Pos/Pre [-]") +
  ylim(0,2) 


#Grafico radar con libreria fmsb
create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )

}


#Grupo Control
df_prueba <- data.frame(row.names = c("Pre", "Pos"), 
                        Grados_90= c(0.4159155, 0.4704770), 
                        Grados_135 = c(0.4405763, 0.482182),
                        Grados_180 = c(0.5073216, 0.4998048), 
                        Grados_225 = c(0.4475587, 0.5199821), 
                        Grados_270= c(0.4159155, 0.4704770), 
                        Grados_315 = c(0.4405763, 0.482182),
                        Grados_0 = c(0.5073216, 0.4998048), 
                        Grados_45 = c(0.4475587, 0.5199821))


# Bind the variable ranges to the data
df <- rbind(max_min, df_prueba)
Pre_data <- df[c("Max", "Min", "Pre"), ]
radarchart(Pre_data)
#Condicion Pre
op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(Pre_data, caxislabels = c(0, 0.25, 0.50, 0.75, 1.0))
par(op)
#Condicion Pre y Pos en un mismo gráfico
op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = df, caxislabels = c(0, 0.25, 0.50, 0.75, 1.0),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x = "left", legend = rownames(df[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)

#Radar en panel 
df <- rbind(df_GrupoControl_radar, 
            df_GrupoCarga_radar[-(1:2),], 
            df_GrupoReaccion_radar[-(1:2),], 
            df_GrupoCombinado_radar[-(1:2),])

# Define colors and titles
colors <- c("#00AFBB", "#E7B800", "#FC4E07", "#6B8E23" )
titles <- c("Control", "Carga", "Reaccion", "Combinado" )

# Reduce plot margin using par()
# Split the screen in 3 parts
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(1,4))

# Create the radar chart
for(i in 1:4){
  create_beautiful_radarchart(
    data = df[c(1, 2, ((2*i) + 2)), ], caxislabels = c(0, 5, 10, 15, 20),
    color = colors[i], title = titles[i]
  )
}
par(op)

#Prueba normalidad

par(mfrow = c(1,2))
qqnorm(df_datos_filtrados[df_datos_filtrados$grupo == "cl"& df_datos_filtrados$condicion == "pre","p"], main = "Control")
qqline(df_datos_filtrados[df_datos_filtrados$grupo == "cl"& df_datos_filtrados$condicion == "pre","p"])
qqnorm(df_datos_filtrados[df_datos_filtrados$grupo == "lt"& df_datos_filtrados$condicion == "pre","p"], main = "Carga")
qqline(df_datos_filtrados[df_datos_filtrados$grupo == "lt"& df_datos_filtrados$condicion == "pre","p"])
par(mfrow = c(1,2))
qqnorm(df_datos_filtrados[df_datos_filtrados$grupo == "rt"& df_datos_filtrados$condicion == "pre","p"], main = "Reaccion")
qqline(df_datos_filtrados[df_datos_filtrados$grupo == "rt"& df_datos_filtrados$condicion == "pre","p"])
qqnorm(df_datos_filtrados[df_datos_filtrados$grupo == "hk"& df_datos_filtrados$condicion == "pre","p"], main = "Combinado")
qqline(df_datos_filtrados[df_datos_filtrados$grupo == "hk"& df_datos_filtrados$condicion == "pre","p"])


par(mfrow = c(1,2))
qqnorm(df_datos_filtrados[df_datos_filtrados$Separacion & df_datos_filtrados$condicion == "pre","p"], main = "Control")
qqline(df_datos_filtrados[df_datos_filtrados$Separacion & df_datos_filtrados$condicion == "pre","p"])

# FUNCION PARA CREAR GRAFICOS RADARES POR ZONAS


  