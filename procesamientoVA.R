##Experimento Ventana Atencional
##Script para ordenar y cargar los datos de los archivos obtenidos durante el experimento de Ventana Atencional

#cargo librerias
library(plyr)
library(tidyverse)
library(R.matlab)
library(readr)
library(reshape2)


#Lista para guardar los datos segun la cantidad de obsevadores
N <- 30
list_datosRaw <- vector("list", N)
list_datos <- vector("list", N)
list_gaze <- vector ("list", N)
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
rm(list=setdiff(ls(), c("list_datos", "list_gaze", "Ntrials", "list_datosRaw"))) 


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

names(list_gaze) <- c("pre_aaf",  "pre_afb",  "pre_agm", "pre_cic", "pre_jjr", "pre_lrc", "pre_mab", "pre_mdn", "pre_msz", "pre_nga", "pre_pab", "pre_at", "pre_lfa", "pre_lms", "pre_mcm", "pos_aaf",  "pos_afb",  "pos_agm", "pos_cic", "pos_jjr", "pos_lrc", "pos_mab", "pos_mdn", "pos_msz", "pos_nga", "pos_pab", "pos_at", "pos_lfa", "pos_lms", "pos_mcm")


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

  
}
#5.2 
#Grafico un panel con los porcentajes del gaze que caen dentro de la zona de #fijacion antes y despues del entrenamiento
Obs <- c("aaf", "afb", "agm", "cic", "jjr", "lrc", "mab", "mdn", "msz", "nga", "pab", "at",  "lfa", "lms", "mcm") 
df_porcentaje <- ldply(list_gaze, data.frame)
df_porcentaje <- df_porcentaje[,-c(1,7)] 
df_porcentaje$Observador <- rep(rep(Obs,each = 336), 2)
df_porcentaje$Condicion <- rep(c("pre", "pos"),each = (10080/2))
df_porcentaje$Ntrials <- rep(1:336, 30)

ggplot(data = df_porcentaje, aes(x= Ntrials , y= TRialOK)) + geom_point(aes(color = Condicion), alpha = 0.5) +  geom_text(aes( x= 1, y = 100, label = "Referencia", hjust = -0.2), size = 3) +
  facet_wrap( ~ Observador, scales="free_x")

#Grafico los porcentajes de trials para cada observador antes y despues del entrenamiento que tienen un porcentaje de 80,60,40 y 20 de puntos de fijación dentro de la zona de fijacion definida en el dataframe df_porcentaje
#Creo data frame
df_aceptado <- data.frame(Observador = rep(Obs, 2), Condicion = rep(c("pre", "pos"), each = 15 ), Porcentaje80 = c(NaN),  Porcentaje60 = c(NaN), Porcentaje50 = c(NaN), Porcentaje20 = c(NaN))
#Nuevas varibles con los porcentajes de trials para cada observador que superan un 80, 60, 40 y 20 % de puntos de fijacion dentro de la zona de fijacion
for (i in seq_along(1:30)){
  
  
  df_aceptado$Porcentaje80[i] <- ((sum(list_gaze[[i]]$TRialOK[2:336] >= 80))/335)*100
  df_aceptado$Porcentaje60[i] <- ((sum(list_gaze[[i]]$TRialOK[2:336] >= 60))/335)*100
  df_aceptado$Porcentaje50[i] <- ((sum(list_gaze[[i]]$TRialOK[2:336] >= 40))/335)*100
  df_aceptado$Porcentaje20[i] <- ((sum(list_gaze[[i]]$TRialOK[2:336] >= 20))/335)*100
  
  if( (df_aceptado$Observador[i] == "aaf" )||(df_aceptado$Observador[i] == "agm" )|| (df_aceptado$Observador[i] == "lrc")||(df_aceptado$Observador[i] == "pab") ){
    
    df_aceptado$Grupo[i] <- c("cl")
    
  }
  
  else if ((df_aceptado$Observador[i] == "afb" )||(df_aceptado$Observador[i] == "cic" )|| (df_aceptado$Observador[i] == "msz")|| (df_aceptado$Observador[i] == "nga") ){
      
    df_aceptado$Grupo[i] <- c("rt")
      
    }
    else if ( (df_aceptado$Observador[i] == "lfa" )||(df_aceptado$Observador[i] == "lms" )|| (df_aceptado$Observador[i] == "mcm")|| (df_aceptado$Observador[i] == "at") ){
      
      df_aceptado$Grupo[i] <- c("hk")
      
    }
    
    else if ( (df_aceptado$Observador[i] == "jjr" )||(df_aceptado$Observador[i] == "mab" )|| (df_aceptado$Observador[i] == "mdn")){
      
      df_aceptado$Grupo[i] <- c("lt")
      
    } 
  
  }

#Graficos de barras para comparar 
ggplot() + geom_col(data = df_aceptado, aes(x = Observador, y = Porcentaje50, fill = Condicion), position = "dodge") 



#Creo un data frame para ver los datos de cada observador en un gráfico
df_tempo <- data.frame(Ntrials = rep(1:336, 2), Obervador = "aaf", Condicion = rep(c("pre", "pos"), each=336), Porcentaje = c(list_gaze$pre_lms$TRialOK, list_gaze$pos_lms$TRialOK))
  
ggplot(df_tempo, aes(x = Ntrials)) + geom_point(aes(y = Porcentaje, color = Condicion)) + geom_text(aes( x= Ntrials[1], y = Porcentaje[1], label = "Referencia pre", hjust = -0.2), size = 3) + geom_text(aes( x= Ntrials[1], y = Porcentaje[337], label = "Referencia pos", hjust = -0.2, vjust = 2), size = 3) + geom_point(aes(x = Ntrials[1], y = Porcentaje[337]), size = 3)
                                                
#5.3
#Origino una lista de indices para cadaa observador con los trials aceptados para el procesamiento posterior
#Obtengo los trials que tienen un porcentaje mayor al 40% de los puntos del gaze dentro de la zona de fijacion
#Genero un data frame para cada observador con el porcentaje de respuestas correctas en cada Direccion y en cada Separacion
list_index <- vector("list", 30)
list_datosFinal <- vector ("list", 30)
list_datosFinal_x <- vector("list", 30)

for (i in seq_along(list_gaze)){
  
 list_index[[i]] <- which(list_gaze[[i]]$TRialOK >= 40)
 list_datosFinal[[i]] <- slice(list_datosRaw[[i]], c(list_index[[i]][-1]))
 list_datosFinal_x[[i]] <- slice(list_datosRaw[[i]], c(-list_index[[i]]))
 list_datosFinal[[i]] <-list_datosFinal[[i]] %>% group_by( Direccion, Separacion) %>% dplyr::summarise(n = n(), nYes = sum(correctas), nNo = n - nYes, p = nYes / n)
 list_datosFinal_x[[i]] <-list_datosFinal_x[[i]] %>% group_by( Direccion, Separacion) %>% dplyr::summarise(n = n(), nYes = sum(correctas), nNo = n - nYes, p = nYes / n)
 
 list_datosFinal[[i]]$Observadores <- list_datos[[i]]$observadores[1:length(list_datosFinal[[i]]$p)]
 list_datosFinal_x[[i]]$Observadores <- list_datos[[i]]$observadores[1:length(list_datosFinal_x[[i]]$p)]
 list_datosFinal[[i]]$Condicion <- list_datos[[i]]$condicion[1:length(list_datosFinal[[i]]$p)]
 list_datosFinal_x[[i]]$Condicion <- list_datos[[i]]$condicion[1:length(list_datosFinal_x[[i]]$p)]
 list_datosFinal[[i]]$Grupo <- list_datos[[i]]$grupo[1:length(list_datosFinal[[i]]$p)]
 list_datosFinal_x[[i]]$Grupo <- list_datos[[i]]$grupo[1:length(list_datosFinal_x[[i]]$p)]
 
 
 }

# GRAFICOS VARIOS ---------------------------------------------------------
#6 Grafico los resultados obtenido
df_datosFinal <- ldply (list_datosFinal, data.frame)
df_datosFinal_x <- ldply (list_datosFinal_x, data.frame)
df_datosFinal$Fijacion <- c("SI") 
df_datosFinal_x$Fijacion <- c("NO")
df_datos$Fijacion <- c("NA")
names(df_datos)[7:9] <- c("Observadores", "Condicion", "Grupo")
df_datos <- rbind(df_datos, df_datosFinal, df_datosFinal_x)

ggplot(df_datos, aes(x= as.factor(Separacion), y= p)) + geom_boxplot(aes(fill= Fijacion, Grupo)) +
  facet_wrap( ~ Separacion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Grupos", title = "Promedio respuestas correctas por grupo ")

#6.1 Panel con Boxplot
ggplot(df_datos %>% filter(Fijacion == "NA"), aes(x= as.factor(Direccion), y= p)) + geom_boxplot(aes(fill=Grupo, Condicion)) +
  facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Condición", title = "Todos los trials")

ggplot(df_datos %>% filter(Fijacion == "SI"), aes(x= as.factor(Direccion), y= p)) + geom_boxplot(aes(fill=Grupo, Condicion)) +
  facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Condición", title = "Con FIJACION")

ggplot(df_datos %>% filter(Fijacion == "NO"), aes(x= as.factor(Direccion), y= p)) + geom_boxplot(aes(fill=Grupo, Condicion)) +
  facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Condición", title = "Sin FIJACION")

ggplot(df_datos%>% filter(Fijacion == "NA"), aes(x= as.factor(Separacion), y= p)) + geom_boxplot(aes(fill=Grupo,Condicion)) +
  facet_wrap( ~ Separacion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Condición", title = "Todos los trials")

ggplot(df_datos%>% filter(Fijacion == "SI"), aes(x= as.factor(Separacion), y= p)) + geom_boxplot(aes(fill=Grupo,Condicion)) +
  facet_wrap( ~ Separacion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Condición", title = "Con FIJACION")

ggplot(df_datos%>% filter(Fijacion == "NO"), aes(x= as.factor(Separacion), y= p)) + geom_boxplot(aes(fill=Grupo,Condicion)) +
  facet_wrap( ~ Separacion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Condición", title = "Sin FIJACION")

#6.2 Panel sin el grupo de hockey ni el control
ggplot(df_datos %>% filter(Grupo == c("lt","rt"), Fijacion == "SI"), aes(x= as.factor(Direccion), y= p)) + geom_boxplot(aes(fill= Condicion)) +
  facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Dirección", title = " Grupos LT y RT con fijacion")

ggplot(df_datos %>% filter(Grupo == c("lt","rt"), Fijacion == "NO"), aes(x= as.factor(Direccion), y= p)) + geom_boxplot(aes(fill= Condicion)) +
  facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Dirección", title = " Grupos LT y RT sin fijacion")

ggplot(df_datos %>% filter(Grupo == c("lt","rt"), Fijacion == "SI"), aes(x= as.factor(Separacion), y= p)) + geom_boxplot(aes(fill=Condicion)) +
  facet_wrap( ~ Separacion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Separacion", title = " Grupos LT y RT con fijacion")

ggplot(df_datos %>% filter(Grupo == c("lt","rt"), Fijacion == "NO"), aes(x= as.factor(Separacion), y= p)) + geom_boxplot(aes(fill=Condicion)) +
  facet_wrap( ~ Separacion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Separacion", title = " Grupos LT y RT sin fijacion")

#Por grupos
#Grupo Control
ggplot(df_datos %>% filter(Grupo == "cl", Fijacion == "NA"), aes(x= as.factor(Separacion), y= p)) + geom_boxplot(aes(fill = Condicion)) +
  facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Dirección [°]", title = "Grupo Control")


#Grupo Tiempo de Reaccion
ggplot(df_datos %>% filter(Grupo == "rt", Fijacion == "NA"), aes(x= as.factor(Separacion), y= p)) + geom_boxplot(aes(fill = Condicion)) +
  facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Dirección [°]", title = "Grupo Tiempo de reacción")


#Grupo con Carga de seguimiento
ggplot(df_datos %>% filter(Grupo == "lt", Fijacion == "NA"), aes(x= as.factor(Separacion), y= p)) + geom_boxplot(aes(fill = Condicion)) +
  facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Dirección [°]", title = "Grupo Carga")

ggplot(df_datos %>% filter(Grupo == "lt", Fijacion == "SI"), aes(x= as.factor(Separacion), y= p)) + geom_boxplot(aes(fill = Condicion)) +
  facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Dirección [°]", title = "Grupo Carga")



#Grupo Hockey
ggplot(df_datos %>% filter(Grupo == "hk", Fijacion == "NA"), aes(x= as.factor(Separacion), y= p)) + geom_boxplot(aes(fill = Condicion)) +
  facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Dirección [°]", title = "Grupo Hockey")

#Por Separacion
#Separacion 3°
ggplot(df_datos %>% filter(Separacion == 3 , Fijacion == "SI"), aes(x= as.factor(Direccion), y= p)) + geom_boxplot(aes(fill=Condicion, Grupo)) +
  facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Separacion", title = " Separacion 3")

#Separacion 5°
ggplot(df_datos %>% filter(Separacion == 5 , Fijacion == "SI"), aes(x= as.factor(Direccion), y= p)) + geom_boxplot(aes(fill=Condicion, Grupo)) +
  facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Separacion", title = " Separacion 5")

#Separacion 8°
ggplot(df_datos %>% filter(Separacion == 8 , Fijacion == "SI"), aes(x= as.factor(Direccion), y= p)) + geom_boxplot(aes(fill=Condicion, Grupo)) +
  facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Separacion", title = " Separacion 8")

#Separacion 11°
ggplot(df_datos %>% filter(Separacion == 11 , Fijacion == "SI"), aes(x= as.factor(Direccion), y= p)) + geom_boxplot(aes(fill=Condicion, Grupo)) +
  facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Separacion", title = " Separacion 11")

#Separacion 14°
ggplot(df_datos %>% filter(Separacion == 14 , Fijacion == "NO"), aes(x= as.factor(Direccion), y= p)) + geom_boxplot(aes(fill=Condicion, Grupo)) +
  facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Separacion", title = " Separacion 14")

#Separacion 17°
ggplot(df_datos %>% filter(Separacion == 17 , Fijacion == "NO"), aes(x= as.factor(Direccion), y= p)) + geom_boxplot(aes(fill=Condicion, Grupo)) +
  facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Separacion", title = " Separacion 17")

#Separacion 20°
ggplot(df_datos %>% filter(Separacion == 20 , Fijacion == "NO"), aes(x= as.factor(Direccion), y= p)) + geom_boxplot(aes(fill=Condicion, Grupo)) +
  facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Respuestas Correctas", x = "Separacion", title = " Separacion 20")



# NUEVAS VARIABLES --------------------------------------------------------
#Cambio nombre a data frames
df_datosCF <- df_datosFinal
df_datosSF <- df_datosFinal_x
#Elimino data frames
rm(df_datosFinal, df_datosFinal_x)
#Creo data frames pre y pos entreno
df_rendimiento_pre <- filter(df_datos, Condicion == "pre")
df_rendimiento_pos <- filter(df_datos, Condicion == "pos")
#Creo un data frame con los valores repetidos de los data frame antes y despeus del entrenamiento
df_rendimiento <- join_all(list(df_rendimiento_pre, df_rendimiento_pos), by = c("Direccion","Separacion", "Observadores", "Grupo", "Fijacion"), type = "inner")
#Elimino y renombre algunas variables
df_rendimiento <- df_rendimiento[,-c(3:5, 8, 11:13, 15)]
names(df_rendimiento)[3] <- c("ResPre")
names(df_rendimiento)[7] <- c("ResPos")

#Genero una variable rendimiento
df_rendimiento <- df_rendimiento %>% mutate(Rendimiento = ResPos/ResPre)

#Gráficos
#Rendimiento vs Direccion
ggplot(df_rendimiento, aes(Direccion, Rendimiento)) + geom_boxplot(aes(fill = Grupo, Fijacion)) + geom_abline(slope = 0, intercept = 1) + facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Rendimiento", x = "Fijación")
#Rendimiento vs Separacion
ggplot(df_rendimiento, aes(Separacion, Rendimiento)) + geom_boxplot(aes(fill = Grupo, Fijacion)) + geom_abline(slope = 0, intercept = 1) + facet_wrap( ~ Separacion, scales="free_x") + labs( y = "Rendimiento", x = "Fijación")
#Grupo lt
ggplot(df_rendimiento %>% filter(Grupo == "lt"), aes(as.factor(Direccion), Rendimiento)) + geom_boxplot(aes(fill = Fijacion)) + geom_abline(slope = 0, intercept = 1) + facet_wrap( ~ Direccion, scales="free_x") + labs( y = "Rendimiento", x = "Direccion")

ggplot(df_rendimiento %>% filter(Grupo == "lt"), aes(as.factor(Separacion), Rendimiento)) + geom_boxplot(aes(fill = Fijacion)) + geom_abline(slope = 0, intercept = 1) + facet_wrap( ~ Separacion, scales="free_x") + labs( y = "Rendimiento", x = "Separacion")

ggplot(df_rendimiento%>% filter(Grupo == "lt"), aes(as.factor(Fijacion), Rendimiento)) + geom_boxplot(aes(fill = Direccion)) + geom_abline(slope = 0, intercept = 1) + facet_wrap(~ Fijacion, scales="free_x") + labs( y = "Rendimiento", x = "Direccion")

ggplot(df_rendimiento%>% filter(Grupo == "lt"), aes(as.factor(Fijacion), Rendimiento)) + geom_boxplot(aes(fill = as.factor(Separacion))) + geom_abline(slope = 0, intercept = 1) + facet_wrap(~ Fijacion, scales="free_x") + labs( y = "Rendimiento", x = "Fijacion")

#Fijacion
ggplot(df_rendimiento, aes(as.factor(Fijacion), Rendimiento)) + geom_boxplot(aes(fill = Grupo)) + geom_abline(slope = 0, intercept = 1) + facet_wrap(~ Fijacion, scales="free_x") + labs( y = "Rendimiento", x = "Fijacion")

ggplot(df_rendimiento, aes(as.factor(Fijacion), Rendimiento)) + geom_boxplot(aes(fill = Grupo ,Direccion)) + geom_abline(slope = 0, intercept = 1) + facet_wrap(~ Fijacion, scales="free_x") + labs( y = "Rendimiento", x = "Direccion")

ggplot(df_rendimiento, aes(as.factor(Fijacion), Rendimiento)) + geom_boxplot(aes(fill = Grupo ,as.factor(Separacion))) + geom_abline(slope = 0, intercept = 1) + facet_wrap(~ Fijacion, scales="free_x") + labs( y = "Rendimiento", x = "Separacion")


# GUARDAR DATOS -----------------------------------------------------------
getCurrentFileLocation <-  function()
{
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0)
  {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}
#Guardo dataframes para el informe en MD
file_procesamientoVA<- paste(getCurrentFileLocation(),'/procesamientoVA.RData', sep = "")
save.image(file = file_procesamientoVA)

#Gaurdo datos para enviar a Jose 
write.csv(df_datos,"C:\\Users\\Anibal\\Documents\\R\\attentional-window\\datosVA_v.1.csv", row.names = TRUE)
