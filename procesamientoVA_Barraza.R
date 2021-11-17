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
library(ggiraph)
library(ggiraphExtra)
library(lme4)
library(sjPlot)
library(ggeffects)


#Listas para guardar los datos segun la cantidad de obsevadores
N <- 30
list_datosRaw <- vector("list", N)
list_datos <- vector("list", N)
list_gaze <- vector ("list", N)
list_fixations <- vector("list", N)
list_fixations_raw <- list()
Ntrials <- 336

# CARGO DATOS ILAV PRE ENTRENAMIENTO --------------------------------------
#Cargo los datos de la ventana atencional antes del entrenamiento del ILAV
#ubico el directorio donde se encuentran los archivos
setwd(paste("B:/Dropbox/Posdoc",
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
      
      df_gaze$XGaze.mm[[k]] <-  ls_gaze_x[[k]][169:184]#101
      df_gaze$YGaze.mm[[k]] <-  ls_gaze_y[[k]][169:184]
      
    }
    
    else{
      
      df_gaze$XGaze.mm[[k]] <-  ls_gaze_x[[k]][69:84]
      df_gaze$YGaze.mm[[k]] <-  ls_gaze_y[[k]][69:84]
      
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
setwd(paste("B:/Dropbox/Posdoc",
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
      
      datos$XGaze.mm[[k]] <-  datos$XGaze.mm[[k]][169:184]
      datos$YGaze.mm[[k]] <-  datos$YGaze.mm[[k]][169:184]
      
    }
    
    else{
      
      datos$XGaze.mm[[k]] <-  datos$XGaze.mm[[k]][69:84]
      datos$YGaze.mm[[k]] <-  datos$YGaze.mm[[k]][69:84]
      
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
setwd(paste("B:/Dropbox/Posdoc",
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
      
      df_gaze$XGaze.mm[[k]] <-  ls_gaze_x[[k]][169:184]
      df_gaze$YGaze.mm[[k]] <-  ls_gaze_y[[k]][169:184]
      
    }
    
    else{
      
      df_gaze$XGaze.mm[[k]] <-  ls_gaze_x[[k]][69:84]
      df_gaze$YGaze.mm[[k]] <-  ls_gaze_y[[k]][69:84]
      
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
setwd(paste("B:/Dropbox/Posdoc",
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
      
      datos$XGaze.mm[[k]] <-  datos$XGaze.mm[[k]][169:184]
      datos$YGaze.mm[[k]] <-  datos$YGaze.mm[[k]][169:184]
      
    }
    
    else{
      
      datos$XGaze.mm[[k]] <-  datos$XGaze.mm[[k]][69:84]
      datos$YGaze.mm[[k]] <-  datos$YGaze.mm[[k]][69:84]
      
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
  list_datosRaw[i + 26] <- list(data.frame(tabla_total))
  
  
  list_datos[i + 26] <- list(averages)
  
  #lista del gaze
  list_gaze[i + 26] <- list(datos[,-(1:4)]) 
  
}

#Elimino las variables
rm(list=setdiff(ls(), c("list_datos", "list_gaze", "Ntrials", "list_datosRaw", "list_fixations", "list_fixations_raw"))) 





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
  

  gaze <- data.frame(#time = rep(c(0:0.02:83*0.02), 336), 
                     time = rep(seq(1.38,1.68, by = 0.02),336),
                     x = unlist(list_gaze[[i]]$XGaze.mm), 
                     y = unlist(list_gaze[[i]]$YGaze.mm), 
                     trial = rep(c(1:336), each = 16)#84
                     )
  
  
  
  fixationsraw <- detect.fixations(gaze, 
                                lambda = 10, 
                                smooth.coordinates = TRUE, 
                                smooth.saccades = FALSE
                                )
  #sin filtrar
  list_fixations_raw[i] <- list(fixationsraw)
  #Elimino las fijaciones que detecta el algoritmo con 
  #duracion menor a 0.28
  
  #tmp
  #fixationsraw <- subset(fixationsraw, dur > 0.28)
  
  #fixations <- fixationsraw %>% group_by(trial) %>% 
  #  mutate(durtotal = sum(dur)) %>% 
  #  summarise(trial = mean(trial), durtotal = mean(durtotal), 
  #           x = mean(x), y =mean(y))
  
  #meanfix <- mean(fixations$durtotal)
  #sdfix <- sd(fixations$durtotal)
  #fixations <- subset(fixations, durtotal >= (meanfix-sdfix)) 
                      #select=c(trial, durtotal))
  
  
  
  #fixations <- subset(fixations,
   #                   dur > 0.28, 
  #                    select = c(trial:dur)
  #                    )
  
  #Calculo la distancia de las fijaciones detectadas a
  #la fijacion del primer trial
  fixationsraw$dist <- sqrt((fixationsraw$x-fixationsraw$x[1])^2 +
                        (fixationsraw$y-fixationsraw$y[1])^2
                        ) 
  
  #fixationsraw$distrela <- sqrt((fixationsraw$x - lag(fixationsraw$x, default = NA))^2 + 
  #                               (fixationsraw$y - lag(fixationsraw$y, default = NA))^2)
  
  #Calculo los valores outliers de las fijacinones en x e y 
  outliersx <- boxplot.stats(fixationsraw$x)$out
  outliersy <- boxplot.stats(fixationsraw$y)$out
  
  #Encuentro y elimino los trials donde se producen esos valores
  #de fijaciones
  #Para x
  fixationsraw <- fixationsraw[-which(fixationsraw$x %in% outliersx),] 
  #Para y
  fixationsraw <- fixationsraw[-which(fixationsraw$y %in% outliersy),] 
  
  #ordeno las fijaciones de mayor a menor distancia a la fijacion
  #del primer trial
  #fixationsraw <- fixationsraw[order(-fixationsraw$dist),]
  
  #elimino el 10% de las fijaciones mas alejadas
  #fixationsraw <- fixationsraw[-c(1:round(0.1*nrow(fixationsraw))),]
  
  #elimino las fijaciones detectadas con duracion 0
  fixationsraw <- filter(fixationsraw, dur != 0)
  
  
  
  #Agrego a la lista de fijaciones
  #ordeno las fijaciones por trial
  #fixationsraw <- fixationsraw[order(fixationsraw$trial),]
  
  #filtradas
  list_fixations[i] <- list(fixationsraw)
  
  

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
#para el post-procesamiento. 
vr_index_trial <- 1:336
porcentajeTrialFinal <- NA
trialDescartar <- NA
df_porcetaje_trials <- 1:30
for (i in seq_along(list_fixations)){
  
  n <- list_fixations[[i]] %>% 
    group_by(trial) %>%
    dplyr::summarise (n=n())
# Vector con la cantidad de trials eliminados
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
  #list_datos_filtrados[[i]] <- list_datos_filtrados[[i]] %>% 
  #  group_by( Direccion, Separacion) %>% 
  #  dplyr::summarise(n = n(), 
  #                   nYes = sum(correctas), 
  #                   nNo = n - nYes, 
  #                   p = nYes / n)  
    
                                                                    
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
  
  #list_datos_filtrados[[i]]$observadores <- list_datos[[i]]$observadores 
  list_datos_filtrados[[i]]$observadores <- list_datos[[i]]$observadores[1]
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

#6.8 Gráfico gaze y fijaciones

#Creo data.frame con los datos de fijaciones crudas y las fijaciones filtradas
df_fixation_raw <- as.data.frame(do.call(rbind, list_fixations_raw))
nfixationsraw <- unlist(lapply(list_fixations_raw,nrow))
df_fixation_raw$observadores <- rep(c('aaf', 'afb', 'agm',
                                      'cic', 'jjr', 'lrc',
                                      'mab', 'mdn', 'msz',
                                      'nga', 'pab', 'at',
                                      'lfa','lms', 'mcm',
                                      'aaf', 'afb', 'agm',
                                      'cic', 'jjr', 'lrc',
                                      'mab', 'mdn', 'msz',
                                      'nga', 'pab', 'at',
                                      'lfa','lms', 'mcm'), nfixationsraw)

df_fixation_raw <- df_fixation_raw[,c(1,4,5,11)]

#Agrego condicion
df_fixation_raw$condicion <- rep(c('pre', 'pos'), 
                                 c(sum(nfixationsraw[1:15]),
                                  sum(nfixationsraw[16:30])))
#Agrego tipo
df_fixation_raw$tipo <- 'raw'
    
  


#Data frame fijaciones filtradas
df_fixation <- data.frame()
nfixations <- c()
for (i in seq_along(list_fixations)){
  
  #df_temp <- data.frame()
  df_temp <- list_fixations[[i]][[1]]
  nfixations <- append(nfixations, length(list_fixations[[i]][[1]]$trial))
  df_fixation <- rbind(df_fixation, df_temp)
}
df_fixation <- df_fixation[,c(1,4,5,12)]
df_fixation$observadores <- rep(c('aaf', 'afb', 'agm',
                                      'cic', 'jjr', 'lrc',
                                      'mab', 'mdn', 'msz',
                                      'nga', 'pab', 'at',
                                      'lfa','lms', 'mcm',
                                      'aaf', 'afb', 'agm',
                                      'cic', 'jjr', 'lrc',
                                      'mab', 'mdn', 'msz',
                                      'nga', 'pab', 'at',
                                      'lfa','lms', 'mcm'), nfixations)

df_fixation$tipo <- 'filter'

#Dataframe final
df_fixations_total <- rbind(df_fixation_raw,df_fixation)


#Graficos

ggplot(df_fixations_total) + geom_point(aes(x,y, color= condicion)) +
  facet_wrap(~observadores + tipo)

#Selecciono los observadores para el gráfico
facets <- c("aaf", "jjr", "afb", "at")
colnames(df_fixations_total)[c(4,5,6)] <- c("Subjects", "Condition","Type")
#Grafico 1
p1 <- ggplot(transform(df_fixations_total[ df_fixations_total$Subjects %in% facets,],
                 Type = factor(Type, levels = c("raw", "filter")))) + 
  
  geom_point(data = filter(transform(df_fixations_total[ df_fixations_total$Subjects %in% facets,],
                                     Type = factor(Type, levels = c("raw", "filter"))),
                           Type =="raw"),
             aes(x,y,alpha = Condition, fill = Condition), colour = "black", size = 5, pch=21) + 
  
  geom_point(data = filter(transform(df_fixations_total[ df_fixations_total$Subjects %in% facets,],
                                     Type = factor(Type, levels = c("raw", "filter"))), 
                           Type =="filter"),
             aes(x,y,alpha = Condition, fill = Condition), colour = "black", size = 5, pch=21) +
  
  scale_alpha_discrete(range = c(0.5, 0.5)) +
  
  facet_wrap(~Subjects+Type, ncol = 4) 
  
  
#Algunos cambios de diseño
p1 + theme_bw() +
  labs(x ="Gaze X Coordinate [mm]", y = "Gaze Y Coordinate [mm]") +
  
  scale_x_continuous(labels = c("-400","-200", "0", "200", "400"), 
                     breaks = c(-400, -200,0,200,400)) + 
  
  theme(axis.text = element_text(size = 23),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_blank(),
        strip.text = element_text(size = 25))
        
# #PROCESAMIENTO LINEAL-----------------------------------------------------------
# #EFECTO POR OBSERVADOR
# #
# colnames(df_datos_filtrados)[c(1:2,7:9)] <- c("Direction", "Distance", "Condition", 
#                                               "Subjects", "Group")
# #GRUPO CONTROL POR OBSRVADORES
# ggplot(subset(df_datos_filtrados, Group == 'cl' ),
#        aes(x = Distance, y = p, color = Condition)) + 
#   geom_point(shape = 1, size = 3)+
#   geom_smooth(method = 'lm') +
#   facet_wrap(~Subjects) + 
#   scale_x_continuous("Distance", labels = as.character(df_datos_filtrados$Distance[1:7]), breaks = df_datos_filtrados$Distance[1:7]) +
#   scale_y_continuous("Proportion", labels = c("0.0", "0.5", "1.0"), breaks = c(0.0,0.5,1.0))
# 
# #MODELO LINEAL
# df_datos_filtrados$Condition <- as.factor(df_datos_filtrados$Condition)
# lm_subjects_control <- lm(p ~ Distance*Condition - 1, data = subset(df_datos_filtrados, Group == 'cl'))
# summary(lm_subjects_control)
# anova(lm_subjects_control)
# 
# 
# 
# 
# 
# #GRUPO CON CARGA ATENCIONAL
# #Por observadores
# ggplot(subset(df_datos_filtrados, Group == 'lt' ),
#        aes(x = Distance, y = p, color = Condition)) + 
#   geom_point(shape = 1, size = 3)+
#   geom_smooth(method = 'lm') +
#   facet_wrap(~Subjects) + 
#   scale_x_continuous("Distance", labels = as.character(df_datos_filtrados$Distance[1:7]), breaks = df_datos_filtrados$Distance[1:7]) +
#   scale_y_continuous("Proportion", labels = c("0.0", "0.5", "1.0"), breaks = c(0.0,0.5,1.0))
# 
# #MODELO LINEAL
# lm_subjects_load <- lm(p ~ Distance*Condition - 1, data = subset(df_datos_filtrados, Group == 'lt'))
# summary(lm_subjects_load)
# anova(lm_subjects_load)
# 
# #GRUPO CON TIEMPO DE REACCION
# #Por observadores
# ggplot(subset(df_datos_filtrados, Group == 'rt' ),
#        aes(x = Distance, y = p, color = Condition)) + 
#   geom_point(shape = 1, size = 3)+
#   geom_smooth(method = 'lm') +
#   facet_wrap(~Subjects) + 
#   scale_x_continuous("Distance", labels = as.character(df_datos_filtrados$Distance[1:7]), breaks = df_datos_filtrados$Distance[1:7]) +
#   scale_y_continuous("Proportion", labels = c("0.0", "0.5", "1.0"), breaks = c(0.0,0.5,1.0))
# #MODELO LINEAL
# lm_subjects_reaction <- lm(p ~ Distance*Condition - 1, data = subset(df_datos_filtrados, Group == 'rt'))
# summary(lm_subjects_reaction)
# anova(lm_subjects_reaction)
# #GRUPO COMBINADO
# #Por observadores
# ggplot(subset(df_datos_filtrados, Group == 'hk' ),
#        aes(x = Distance, y = p, color = Condition)) + 
#   geom_point(shape = 1, size = 3)+
#   geom_smooth(method = 'lm') +
#   facet_wrap(~Subjects) + 
#   scale_x_continuous("Distance", labels = as.character(df_datos_filtrados$Distance[1:7]), breaks = df_datos_filtrados$Distance[1:7]) +
#   scale_y_continuous("Proportion", labels = c("0.0", "0.5", "1.0"), breaks = c(0.0,0.5,1.0))
# #MODELO LINEAL
# lm_subjects_combined <- lm(p ~ Distance*Condition - 1, data = subset(df_datos_filtrados, Group == 'hk'))
# summary(lm_subjects_combined)
# anova(lm_subjects_combined)
# 
# #TODOS LOS OBSERVADORES 
# #vs SEPARACION
# df_datos_filtrados$Group <- as.factor(df_datos_filtrados$Group)
# df_datos_filtrados$Subjects <- as.factor(df_datos_filtrados$Subjects)
# ggplot(df_datos_filtrados,
#        aes(x = Distance, y = p, color = Condition)) + 
#   geom_point(shape = 1)+
#   geom_smooth(method = 'lm') +
#   facet_wrap(~factor(Subjects, levels=c('aaf', 'agm', 'lrc', 'pab', 'afb', 'cic', 'msz', 'nga', 'at-', 'lfa', 'lms', 'mcm', 'jjr', 'mab', 'mdn'))~factor(Group, levels = c('cl', 'lt', 'rt', 'hk')))
# #VS DIRECCION
# df_datos_filtrados$Direction <- as.integer(df_datos_filtrados$Direction)
# ggplot(df_datos_filtrados,
#        aes(x = as.factor(Direction), y = p, color = Condition)) + 
#   geom_boxplot()+
#   geom_smooth(method = 'lm') +
#   facet_wrap(~factor(Subjects, levels=c('aaf', 'agm', 'lrc', 'pab', 'afb', 'cic', 'msz', 'nga', 'at-', 'lfa', 'lms', 'mcm', 'jjr', 'mab', 'mdn'))~factor(Group, levels = c('cl', 'lt', 'rt', 'hk')))+
#   theme(legend.background = element_blank())
#   
# 
# 

#GLM CON DISTRIBUCION BINOMIAL--------------------------------------------------
#
UnirDataFrames <- function(ls_raw, indice){
  df_pre <- data.frame(subset(ls_raw[[indice]], select = c(5,6,9)))
  df_pos <- data.frame(subset(ls_raw[[indice + 15]],select = c(5,6,9)))
  df_pre$Condicion <- as.factor(c('pre'))
  df_pos$Condicion <- as.factor(c('pos'))
  df_obs <- rbind(df_pre,df_pos)
  return(df_obs)
  
}

df_aaf <- UnirDataFrames(list_datos_filtrados,1)
df_aaf$correctas <- as.numeric(df_aaf$correctas)
df_afb <- UnirDataFrames(list_datos_filtrados,2)
df_afb$correctas <- as.numeric(df_afb$correctas)
df_agm <- UnirDataFrames(list_datos_filtrados,3)
df_agm$correctas <- as.numeric(df_agm$correctas)
df_cic <- UnirDataFrames(list_datos_filtrados,4)
df_cic$correctas <- as.numeric(df_cic$correctas)
df_jjr <- UnirDataFrames(list_datos_filtrados,5)
df_jjr$correctas <- as.numeric(df_jjr$correctas)
df_lrc <- UnirDataFrames(list_datos_filtrados,6)
df_lrc$correctas <- as.numeric(df_lrc$correctas)
df_mab <- UnirDataFrames(list_datos_filtrados,7)
df_mab$correctas <- as.numeric(df_mab$correctas)
df_mdn <- UnirDataFrames(list_datos_filtrados,8)
df_mdn$correctas <- as.numeric(df_mdn$correctas)
df_msz <- UnirDataFrames(list_datos_filtrados,9)
df_msz$correctas <- as.numeric(df_msz$correctas)
df_nga <- UnirDataFrames(list_datos_filtrados,10)
df_nga$correctas <- as.numeric(df_nga$correctas)
df_pab <- UnirDataFrames(list_datos_filtrados,11)
df_pab$correctas <- as.numeric(df_pab$correctas)
df_at <- UnirDataFrames(list_datos_filtrados,12)
df_at$correctas <- as.numeric(df_at$correctas)
df_lfa <- UnirDataFrames(list_datos_filtrados,13)
df_lfa$correctas <- as.numeric(df_lfa$correctas)
df_lms <- UnirDataFrames(list_datos_filtrados,14)
df_lms$correctas <- as.numeric(df_lms$correctas)
df_mcm <- UnirDataFrames(list_datos_filtrados,15)
df_mcm$correctas <- as.numeric(df_mcm$correctas)


#MODELOS
#
# model_aaf <- glm(correctas ~ Separacion * Condicion, family=binomial, data = df_aaf)
# model_afb <- glm(formula= correctas ~ Separacion * Condicion, data=df_afb, family=binomial)
# model_agm <- glm(formula= correctas ~ Separacion * Condicion, data=df_agm, family=binomial)
# model_cic <- glm(formula= correctas ~ Separacion * Condicion, data=df_cic, family=binomial)
# model_jjr <- glm(formula= correctas ~ Separacion * Condicion, data=df_jjr, family=binomial)
# model_lrc <- glm(formula= correctas ~ Separacion * Condicion, data=df_lrc, family=binomial)
# model_mab <- glm(formula= correctas ~ Separacion * Condicion, data=df_mab, family=binomial)
# model_mdn <- glm(formula= correctas ~ Separacion * Condicion, data=df_mdn, family=binomial)
# model_msz <- glm(formula= correctas ~ Separacion * Condicion, data=df_msz, family=binomial)
# model_nga <- glm(formula= correctas ~ Separacion * Condicion, data=df_nga, family=binomial)
# model_pab <- glm(formula= correctas ~ Separacion * Condicion, data=df_pab, family=binomial)
# model_at  <- glm(formula= correctas ~ Separacion * Condicion, data=df_at, family=binomial)
# model_lfa <- glm(formula= correctas ~ Separacion * Condicion, data=df_lfa, family=binomial)
# model_lms <- glm(formula= correctas ~ Separacion * Condicion, data=df_lms, family=binomial)
# model_mcm <- glm(formula= correctas ~ Separacion * Condicion, data=df_mcm, family=binomial)
# 
# #CONTROL POR OBSERVADOR
# ggPredict(model_aaf, show.summary = TRUE, se = TRUE, jitter = TRUE)
# ggPredict(model_agm, show.summary = TRUE, se = TRUE, jitter = TRUE)
# ggPredict(model_lrc, show.summary = TRUE, se = TRUE, jitter = TRUE)
# ggPredict(model_pab, show.summary = TRUE, se = TRUE, jitter = TRUE)
#CONTROL GRUPO
df_GrupoControl_bin <- rbind(df_aaf,df_agm,df_lrc, df_pab)
df_GrupoControl_bin$Subjects <- as.factor(rep(c('aaf','agm','lrc', 'pab'), 
                                              c(length(df_aaf[,1]),length(df_agm[,1]),length(df_lrc[,1]),length(df_pab[,1]))))
colnames(df_GrupoControl_bin) <- c('Direction', 'Distance', 'Response', 'Condition', 'Subjects')
# model_GrupoControl_bin <- glm(Response ~ Distance * Condition * Subjects , 
#                               family = binomial, 
#                               data = df_GrupoControl_bin)
# ggPredict(model_GrupoControl_bin, show.summary = TRUE, se = TRUE, jitter = TRUE) + 
#   scale_x_continuous("Distance", labels = as.character(sort(unique(df_GrupoControl_bin$Distance))), breaks = sort(unique(df_GrupoControl_bin$Distance))) +
#   scale_y_continuous("Response", labels = c("0.0", "1.0"), breaks = c(0.0,1.0))
# 
# #TODO EL GRUPO
# model_GrupoControl_bin_all <- glm(Response ~ Distance * Condition, 
#                               family = binomial, 
#                               data = df_GrupoControl_bin[,-5])
# 
# ggPredict(model_GrupoControl_bin_all, se = TRUE, jitter = TRUE) + 
#   scale_x_continuous("Distance", labels = as.character(sort(unique(df_GrupoControl_bin$Distance))), 
#                      breaks = sort(unique(df_GrupoControl_bin$Distance))) +
#   scale_y_continuous("Response", labels = c("0.0", "1.0"), breaks = c(0.0,1.0))
# 
# 
# #CARGA POR OBSERVADOR
# summary(model_jjr)
# summary(model_mab)
# summary(model_mdn)
#GRUPO CARGA
df_GrupoCarga_bin <- rbind(df_jjr,df_mab,df_mdn)

df_GrupoCarga_bin$Subjects <- as.factor(rep(c('jjr','mab','mdn'), 
                                              c(length(df_jjr[,1]),length(df_mab[,1]),length(df_mdn[,1]))))


colnames(df_GrupoCarga_bin) <- c('Direction', 'Distance', 'Response', 'Condition', 'Subjects')

# model_GrupoCarga_bin <- glm(Response ~ Distance * Condition * Subjects , 
#                               family = binomial, 
#                               data = df_GrupoCarga_bin)
# ggPredict(model_GrupoCarga_bin, se = TRUE)
# #TODO EL GRUPO
# model_GrupoCarga_bin_all <- glm(Response ~ Distance * Condition, 
#                                   family = binomial, 
#                                   data = df_GrupoCarga_bin[,-5])
# 
# ggPredict(model_GrupoCarga_bin_all, se = TRUE, jitter = TRUE) + 
#   scale_x_continuous("Distance", labels = as.character(sort(unique(df_GrupoCarga_bin$Distance))), 
#                      breaks = sort(unique(df_GrupoCarga_bin$Distance))) +
#   scale_y_continuous("Response", labels = c("0.0", "1.0"), breaks = c(0.0,1.0))
# 
# 
# #TIEMPO DE REACCION POR OBSERVADOR
# summary(model_afb)
# summary(model_cic)
# summary(model_msz)
# summary(model_nga)
#GRUPO REACCION
df_GrupoReaccion_bin <- rbind(df_afb,df_cic,df_msz, df_nga)

df_GrupoReaccion_bin$Subjects <- as.factor(rep(c('afb','cic','msz', 'nga'), 
                                              c(length(df_afb[,1]),length(df_cic[,1]),length(df_msz[,1]),length(df_nga[,1]))))

colnames(df_GrupoReaccion_bin) <- c('Direction', 'Distance', 'Response', 'Condition', 'Subjects')
# model_GrupoReaccion_bin <- glm(Response ~ Distance * Condition * Subjects , 
#                             family = binomial, 
#                             data = df_GrupoReaccion_bin)
# ggPredict(model_GrupoReaccion_bin, se = TRUE)
# #TODO EL GRUPO
# model_GrupoReaccion_bin_all <- glm(Response ~ Distance * Condition, 
#                                 family = binomial, 
#                                 data = df_GrupoReaccion_bin[,-5])
# 
# ggPredict(model_GrupoReaccion_bin_all, se = TRUE, jitter = TRUE) + 
#   scale_x_continuous("Distance", labels = as.character(sort(unique(df_GrupoReaccion_bin$Distance))), 
#                      breaks = sort(unique(df_GrupoReaccion_bin$Distance))) +
#   scale_y_continuous("Response", labels = c("0.0", "1.0"), breaks = c(0.0,1.0))
# 
# #COMBINADO POR OBERVADOR
# summary(model_at)
# summary(model_lfa)
# summary(model_lms)
# summary(model_mcm)
#GRUPO COMBINADO
df_GrupoCombinado_bin <- rbind(df_at,df_lfa,df_lms, df_mcm)
df_GrupoCombinado_bin$Subjects <- as.factor(rep(c('at','lfa','lms', 'mcm'), 
                                              c(length(df_at[,1]),length(df_lfa[,1]),length(df_lms[,1]),length(df_mcm[,1]))))
colnames(df_GrupoCombinado_bin) <- c('Direction', 'Distance', 'Response', 'Condition', 'Subjects')
# model_GrupoCombinado_bin <- glm(Response ~ Distance * Condition * Subjects , 
#                                family = binomial, 
#                                data = df_GrupoCombinado_bin)
# ggPredict(model_GrupoCombinado_bin, se = TRUE) + scale_y_continuous(labels = c("0.0", "0.5", "1.0"), breaks = c(0.0,0.5,1.0))
# #TODO EL GRUPO
# model_GrupoCombinado_bin_all <- glm(Response ~ Distance * Condition, 
#                                 family = binomial, 
#                                 data = df_GrupoCombinado_bin[,-5])
# 
# ggPredict(model_GrupoCombinado_bin_all, se = TRUE, jitter = TRUE) + 
#   scale_x_continuous("Distance", labels = as.character(sort(unique(df_GrupoCarga_bin$Distance))), 
#                      breaks = sort(unique(df_GrupoCombinado_bin$Distance))) +
#   scale_y_continuous("Response", labels = c("0.0", "1.0"), breaks = c(0.0,1.0))

#GLMM CON DISTRIBUCION BINOMIAL-------------------------------------------------
#GRUPO CONTROL
df_GrupoControl_bin <- df_GrupoControl_bin %>%
  mutate(Condition = if_else(Condition == "pre","pretest", "posttest"))

df_GrupoControl_bin$Direction <- as.numeric(df_GrupoControl_bin$Direction)

gmmGrupoControl_model <- glmer(Response ~ Distance + Direction + Condition +  (1 | Subjects),
                                 family = binomial, data = df_GrupoControl_bin)

df_GrupoControl_plot <- df_GrupoControl_bin %>%
  group_by(Subjects,Distance, Condition) %>% 
  summarise(n = n(), p = sum(Response)/n)

df_GrupoControl_predict <- ggpredict(gmmGrupoControl_model, terms = c("Distance", "Condition"))


##GRAFICOS
ggplot(data = df_GrupoControl_plot) + 
  geom_point(aes(Distance,p*100, 
                 colour = Condition,
                 shape = Subjects),
             size = 6) +
  geom_line(data=df_GrupoControl_predict, 
            aes(df_GrupoControl_predict$x, 
                df_GrupoControl_predict$predicted*100, 
                colour = df_GrupoControl_predict$group))+
  geom_ribbon(data=df_GrupoControl_predict, 
              aes(df_GrupoControl_predict$x, 
                  df_GrupoControl_predict$predicted*100, 
                  colour = df_GrupoControl_predict$group,
                  ymin = df_GrupoControl_predict$conf.low*100, 
                  ymax = df_GrupoControl_predict$conf.high*100), 
              alpha = 0.1,
              linetype=0) +
  scale_x_continuous("Distance [°]", 
                     labels = as.character(c(3,5,8,11,14,17,20)), 
                     breaks = c(3,5,8,11,14,17,20)) +
  ylim(0, 100) + ylab("Correct answers [%]") +
  theme_bw() +
  theme(axis.text = element_text(size = 23),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 25)) 
  
  

##TABLA
tab_model(gmmGrupoControl_model, show.est = T,  p.threshold = c(0.1, 0.05, 0.01, 0.001))




#GRUPO CARGA
df_GrupoCarga_bin <- df_GrupoCarga_bin %>%
  mutate(Condition = if_else(Condition == "pre","pretest", "posttest"))

df_GrupoCarga_bin$Direction <- as.numeric(df_GrupoCarga_bin$Direction)
 

gmmGrupoCarga_model <- glmer(Response ~ Distance + Direction + Condition + (1 | Subjects),
                               family = binomial, data = df_GrupoCarga_bin)


df_GrupoCarga_plot <- df_GrupoCarga_bin %>%
  group_by(Subjects,Distance, Condition) %>% 
  summarise(n = n(), p = sum(Response)/n)

df_GrupoCarga_predict <- ggpredict(gmmGrupoCarga_model, terms = c("Distance", "Condition"))

##GRAFICOS
ggplot(data = df_GrupoCarga_plot) + 
  geom_point(aes(Distance,p*100, 
                 colour = Condition, 
                 shape = Subjects),
             size = 6) +
  geom_line(data=df_GrupoCarga_predict, 
            aes(df_GrupoCarga_predict$x, 
                df_GrupoCarga_predict$predicted*100, 
                colour = df_GrupoCarga_predict$group))+
  geom_ribbon(data=df_GrupoCarga_predict, 
              aes(df_GrupoCarga_predict$x, 
                  df_GrupoCarga_predict$predicted*100, 
                  colour = df_GrupoCarga_predict$group,
                  ymin = df_GrupoCarga_predict$conf.low*100, 
                  ymax = df_GrupoCarga_predict$conf.high*100), 
              alpha = 0.1,
              linetype=0) +
  scale_x_continuous("Distance [°]", 
                     labels = as.character(c(3,5,8,11,14,17,20)), 
                     breaks = c(3,5,8,11,14,17,20)) +
  scale_y_continuous("Percentage correct [-]", 
                     labels = as.character(c(0,25,50,75,100)), 
                     breaks = c(0,25,50,75,100)) +
  ylim(0, 100) + ylab("Correct answers [%]") +
  theme_bw() +
  theme(axis.text = element_text(size = 23),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 25)) 


##TABLA
tab_model(gmmGrupoCarga_model, transform = NULL, show.est = T,  p.threshold = c(0.1, 0.05, 0.01, 0.001))
tab_model(gmmGrupoCarga_model_Dir, transform = NULL, show.est = T,  p.threshold = c(0.1, 0.05, 0.01, 0.001))



#GRUPO TIEMPO DE REACCION
df_GrupoReaccion_bin <- df_GrupoReaccion_bin %>%
  mutate(Condition = if_else(Condition == "pre","pretest", "posttest"))

df_GrupoReaccion_bin$Direction <- as.numeric(df_GrupoReaccion_bin$Direction)

gmmGrupoReaccion_model <- glmer(Response ~ Distance + Direction + Condition + (1 | Subjects),
                             family = binomial, data = df_GrupoReaccion_bin)

df_GrupoReaccion_plot <- df_GrupoReaccion_bin %>%
  group_by(Subjects,Distance, Condition) %>% 
  summarise(n = n(), p = sum(Response)/n)

df_GrupoReaccion_predict <- ggpredict(gmmGrupoReaccion_model, terms = c("Distance", "Condition"))

##GRAFICOS
ggplot(data = df_GrupoReaccion_plot) + 
  geom_point(aes(Distance,p*100, 
                 colour = Condition, 
                 shape = Subjects),
             size = 6) +
  geom_line(data=df_GrupoReaccion_predict, 
            aes(df_GrupoReaccion_predict$x, 
                df_GrupoReaccion_predict$predicted*100, 
                colour = df_GrupoReaccion_predict$group))+
  geom_ribbon(data=df_GrupoReaccion_predict, 
              aes(df_GrupoReaccion_predict$x, 
                  df_GrupoReaccion_predict$predicted*100, 
                  colour = df_GrupoReaccion_predict$group,
                  ymin = df_GrupoReaccion_predict$conf.low*100, 
                  ymax = df_GrupoReaccion_predict$conf.high*100), 
              alpha = 0.1,
              linetype=0) +
  scale_x_continuous("Distance [°]", 
                     labels = as.character(c(3,5,8,11,14,17,20)), 
                     breaks = c(3,5,8,11,14,17,20)) +
  ylim(0, 100) + ylab("Correct answers [%]") +
  theme_bw() +
  theme(axis.text = element_text(size = 23),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 25)) 


##TABLA
tab_model(gmmGrupoReaccion_model, show.est = T)
tab_model(gmmGrupoReaccion_model_Dir, transform = NULL, show.est = T,  p.threshold = c(0.1, 0.05, 0.01, 0.001))


#GRUPO COMBINADO
df_GrupoCombinado_bin <- df_GrupoCombinado_bin %>%
  mutate(Condition = if_else(Condition == "pre","pretest", "posttest"))

df_GrupoCombinado_bin$Direction <- as.numeric(df_GrupoCombinado_bin$Direction)

gmmGrupoCombinado_model <- glmer(Response ~ Distance + Direction + Condition + (1 | Subjects),
                                 family = binomial, data = df_GrupoCombinado_bin)

df_GrupoCombinado_plot <- df_GrupoCombinado_bin %>%
  group_by(Subjects,Distance, Condition) %>% 
  summarise(n = n(), p = sum(Response)/n)

df_GrupoCombinado_predict <- ggpredict(gmmGrupoCombinado_model, terms = c("Distance", "Condition"))

##GRAFICOS
ggplot(data = df_GrupoCombinado_plot) + 
  geom_point(aes(Distance,p*100, 
                 colour = Condition, 
                 shape = Subjects),
             size = 6) +
  geom_line(data=df_GrupoCombinado_predict, 
            aes(df_GrupoCombinado_predict$x, 
                df_GrupoCombinado_predict$predicted*100, 
                colour = df_GrupoCombinado_predict$group))+
  geom_ribbon(data=df_GrupoCombinado_predict, 
              aes(df_GrupoCombinado_predict$x, 
                  df_GrupoCombinado_predict$predicted*100, 
                  colour = df_GrupoCombinado_predict$group,
                  ymin = df_GrupoCombinado_predict$conf.low*100, 
                  ymax = df_GrupoCombinado_predict$conf.high*100), 
              alpha = 0.1,
              linetype=0) +
  scale_x_continuous("Distance [°]", 
                     labels = as.character(c(3,5,8,11,14,17,20)), 
                     breaks = c(3,5,8,11,14,17,20)) +
  ylim(0, 100) + ylab("Correct answers [%]") +
  theme_bw() +
  theme(axis.text = element_text(size = 23),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 25)) 

##TABLA
tab_model(gmmGrupoCombinado_model, show.est = T)
tab_model(gmmGrupoCombinado_model_Dir, transform = NULL, show.est = T,  p.threshold = c(0.1, 0.05, 0.01, 0.001))

##TABLA TOTAL
tab_model(
  gmmGrupoControl_model, gmmGrupoCarga_model, gmmGrupoReaccion_model, gmmGrupoCombinado_model,
  show.intercept = F,
  show.est = T,
  show.se = F,
  show.re.var = T,
  show.aic = F,
  show.icc = F,
  #pred.labels = c("Distance", "Condition[Posttest]", "Distance*Condition"),
  dv.labels = c("Control", "Attentional load", "Reaction time", "Combined task"),
  string.pred = "Coefficient",
  string.ci = "CI(95%)",
  string.p = "P-Value",
  p.style = "numeric",
  file = "B:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Graficos/Ajuste GLMM/Tabla.html"
)


overdisp_fun <- function(model) {
  ## number of variance parameters in an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m) * (nrow(m) + 1)/2
  }
  # The next two lines calculate the residual degrees of freedom
  model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
  rdf <- nrow(model.frame(model)) - model.df
  # extracts the Pearson residuals
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  # Generates a p-value. If less than 0.05, the data are overdispersed.
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}





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


  