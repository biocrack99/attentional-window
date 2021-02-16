##Experimento Ventana Atencional
##Script para ir probando distintas cuestiones

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

#3- Creo dataframes con la lista de datos

df_datos <- ldply (list_datos, data.frame)
df_datosRaw <- ldply (list_datosRaw, data.frame)

#4- Gráfico sobre la distribución de clicks 
ggplot(data= df_datosRaw) + 
  
      geom_bar(aes(Respuesta_A, fill = "red")) + 
  
      geom_bar(aes(Cantidad_TGC_1),alpha = .5) 


ggplot(data= df_datosRaw) + 
  
      geom_bar(aes(Respuesta_B), alpha = .5) + 
  
      geom_bar(aes(Cantidad_TGC_2),alpha = .3) 


#5- Data frames para poder mejorar el gráfico de densidad  

#Data frame posicion del estimulo A
df_distribucionA <- select(df_datosRaw, c(1,3))

df_distribucionA <- data.frame(Cantidad=unlist(df_distribucionA, use.names = 
                                                 
                                                 FALSE
                                               )
                               )

df_distribucionA$Condicion <- rep(c("Cantidad", "Clicks"), each = 10050)

df_distribucionA$Ubicacion <- rep(c("A"), each = 20100)

#Data frame posicion del estimulo B
df_distribucionB <- select(df_datosRaw, c(2,4))

df_distribucionB <- data.frame(Cantidad=unlist(df_distribucionB, use.names = 
                                                 
                                                 FALSE
                                               )
                               )

df_distribucionB$Condicion <- rep(c("Cantidad", "Clicks"), each = 10050)

df_distribucionB$Ubicacion <- rep(c("B"), each = 20100)

#Graficos de densidad para comparar cantidad con clicks
#Posicion A del estímulo
ggplot(df_distribucionA, aes(Cantidad, fill = Condicion)) + 
  
  geom_density(alpha = 0.2)

ggplot(df_distribucionA, aes(x= as.numeric(Cantidad))) + 
  
  geom_histogram()

#Posicion A del estímulo
ggplot(df_distribucionB, aes(Cantidad, fill = Condicion)) + 
  
  geom_density(alpha = 0.2)

ggplot(df_distribucionB, aes(x= as.numeric(Cantidad))) + 
  
  geom_histogram()


#Dataframes con variables para histograma
df_resumenA <- df_distribucionA %>% 
                                  group_by( Cantidad, Condicion) %>% 
                                  summarise(n = n())  %>%
                                  mutate(Relativo = n/10050)

df_resumenB <- df_distribucionB %>% 
                                  group_by( Cantidad, Condicion) %>% 
                                  summarise(n = n())  %>%
                                  mutate(Relativo = n/10050)

#Plot histogramas
ggplot() + 
  
  geom_col(data = df_resumenA, aes(x = Cantidad, y = n, fill = Condicion), 
           
           position = "dodge"
           
          ) 

ggplot() + 
  
  geom_col(data = df_resumenB, aes(x = Cantidad, y = n, fill = Condicion), 
           
           position = "dodge"
           
  )  
#Para comparar con los valores teóricos calculados
ggplot() + 
  
  geom_col(data = df_resumenA, aes(x = Cantidad, y = Relativo, fill = Condicion), 
           position = "dodge"
           
  ) 

ggplot() + 
  
  geom_col(data = df_resumenB, aes(x = Cantidad, y = Relativo, fill = Condicion), 
           position = "dodge"
           
  )  