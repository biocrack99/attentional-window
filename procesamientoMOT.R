##Experimento Ventana Atencional
##Script para ordenar y cargar los datos de los archivos obtenidos durante el entrenamiento MOT
## Autor: Aníbal de Paul
## Fecha: 26/05/2020

# Cargo librerias
library(plyr)
library(tidyverse)
library(R.matlab)
library(readr)
library(reshape2)

# Cargar datos por Grupo y por Observador
# Existe una carpeta por Observador
#Lista para los datos cargados
ls_datos <- vector("list")
ls_datos_ct <- vector("list") 
nu_sesion <- 6
# Grupo LT ----------------------------------------------------------------
## JJR ---------------------------------------------------------------------
# Ubico directorio donde de ecuentras los archivos .mat
setwd("D:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/LT MOT/Datos/JJR/")
# Nombre de los archivos en la carpeta con extension .mat
files <- list.files(pattern = "*.mat", full.names = T)
# Cargo los datos
for (i in seq_along (files)){
  
  matlab_file  <- readMat(files[i])
  var_names    <- names(matlab_file$estructura.datos[,,1])
  ls_dat       <- matlab_file$estructura.datos
  mx_data <- as.data.frame(ls_dat)
  df_datos <- as.data.frame(t(mx_data))
  df_datos$R <- unlist(df_datos$R)
  df_datos <- mutate(df_datos, Porcentaje = (df_datos$R/4)*100)
  #Sesion de entrenamiento 
  df_datos$Sesion <- i
  #Lista datos Observador JJR
  ls_datos[i] <- list(df_datos)
              
}
## MAB ---------------------------------------------------------------------
# Ubico directorio donde de ecuentras los archivos .mat
setwd("D:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/LT MOT/Datos/MAB/")
# Nombre de los archivos en la carpeta con extension .mat
files <- list.files(pattern = "*.mat", full.names = T)
# Cargo los datos
for (i in seq_along (files)){
  
  matlab_file  <- readMat(files[i])
  var_names    <- names(matlab_file$estructura.datos[,,1])
  ls_dat       <- matlab_file$estructura.datos
  mx_data    <- as.data.frame(ls_dat)
  df_datos <- as.data.frame(t(mx_data))
  df_datos$R <- unlist(df_datos$R)
  df_datos <- mutate(df_datos, Porcentaje = (df_datos$R/4)*100)
  #Sesion de entrenamiento 
  df_datos$Sesion <- i
  #Lista datos Observador JJR
  ls_datos[i + nu_sesion] <- list(df_datos)
  
}

## MDN ---------------------------------------------------------------------
# Ubico directorio donde de ecuentras los archivos .mat
setwd("D:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/LT MOT/Datos/MDN/")
# Nombre de los archivos en la carpeta con extension .mat
files <- list.files(pattern = "*.mat", full.names = T)
# Cargo los datos
for (i in seq_along (files)){
  
  matlab_file  <- readMat(files[i])
  var_names    <- names(matlab_file$estructura.datos[,,1])
  ls_dat       <- matlab_file$estructura.datos
  mx_data    <- as.data.frame(ls_dat)
  df_datos <- as.data.frame(t(mx_data))
  df_datos$R <- unlist(df_datos$R)
  df_datos <- mutate(df_datos, Porcentaje = (df_datos$R/4)*100)
  #Sesion de entrenamiento 
  df_datos$Sesion <- i
  #Lista datos Observador JJR
  ls_datos[i + (2*nu_sesion)] <- list(df_datos)
  
}

# Grupo CT ----------------------------------------------------------------
## LT
## Concentrado 
## AT----------------------------------------------------------------------
# Ubico directorio donde de ecuentras los archivos .mat
setwd("D:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/LT MOT/Datos/Hockey/Concentrado/AT")
# Nombre de los archivos en la carpeta con extension .mat
files <- list.files(pattern = "*.mat", full.names = T)
# Cargo los datos
for (i in seq_along (files)){
  
  matlab_file  <- readMat(files[i])
  var_names    <- names(matlab_file$estructura.datos[,,1])
  ls_dat       <- matlab_file$estructura.datos
  mx_data    <- as.data.frame(ls_dat)
  df_datos <- as.data.frame(t(mx_data))
  df_datos$R <- unlist(df_datos$R)
  df_datos <- mutate(df_datos, Porcentaje = (df_datos$R/4)*100)
  #Sesion de entrenamiento 
  df_datos$Sesion <- i
  #Modalidad
  df_datos$Modalidad <- "Concentrado"
  #Observador
  df_datos$Observador <- "at-"
  #Grupo
  df_datos$Grupo <- "lt"
  #Lista datos Observador JJR
  ls_datos_ct[i] <- list(df_datos)
  
}
## Expandido 
## LFA ---------------------------------------------------------------------
# Ubico directorio donde se encuentran los archivos .mat
setwd("D:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/LT MOT/Datos/Hockey/Expandido/LFA")
# Nombre de los archivos en la carpeta con extension .mat
files <- list.files(pattern = "*.mat", full.names = T)
# Cargo los datos
for (i in seq_along (files)){
  
  matlab_file  <- readMat(files[i])
  var_names    <- names(matlab_file$estructura.datos[,,1])
  ls_dat       <- matlab_file$estructura.datos
  mx_data    <- as.data.frame(ls_dat)
  df_datos <- as.data.frame(t(mx_data))
  df_datos$R <- unlist(df_datos$R)
  df_datos <- mutate(df_datos, Porcentaje = (df_datos$R/4)*100)
  #Sesion de entrenamiento 
  df_datos$Sesion <- i
  #Modalidad
  df_datos$Modalidad <- "Expandido"
  #Observador
  df_datos$Observador <- "lfa"
  #Grupo
  df_datos$Grupo <- "lt"
  #Lista datos Observador JJR
  ls_datos_ct[i+1] <- list(df_datos)
  
}
## LMS ---------------------------------------------------------------------
# Ubico directorio donde se encuentran los archivos .mat
setwd("D:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/LT MOT/Datos/Hockey/Expandido/LMS")
# Nombre de los archivos en la carpeta con extension .mat
files <- list.files(pattern = "*.mat", full.names = T)
# Cargo los datos
for (i in seq_along (files)){
  
  matlab_file  <- readMat(files[i])
  var_names    <- names(matlab_file$estructura.datos[,,1])
  ls_dat       <- matlab_file$estructura.datos
  mx_data    <- as.data.frame(ls_dat)
  df_datos <- as.data.frame(t(mx_data))
  df_datos$R <- unlist(df_datos$R)
  df_datos <- mutate(df_datos, Porcentaje = (df_datos$R/4)*100)
  #Sesion de entrenamiento 
  df_datos$Sesion <- i
  #Modalidad
  df_datos$Modalidad <- "Expandido"
  #Observador
  df_datos$Observador <- "lms"
  #Grupo
  df_datos$Grupo <- "lt"
  #Lista datos Observador JJR
  ls_datos_ct[i+7] <- list(df_datos)
  
}

## MCM ---------------------------------------------------------------------
# Ubico directorio donde se encuentran los archivos .mat
setwd("D:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/LT MOT/Datos/Hockey/Expandido/MCM")
# Nombre de los archivos en la carpeta con extension .mat
files <- list.files(pattern = "*.mat", full.names = T)
# Cargo los datos
for (i in seq_along (files)){
  
  matlab_file  <- readMat(files[i])
  var_names    <- names(matlab_file$estructura.datos[,,1])
  ls_dat       <- matlab_file$estructura.datos
  mx_data    <- as.data.frame(ls_dat)
  df_datos <- as.data.frame(t(mx_data))
  df_datos$R <- unlist(df_datos$R)
  df_datos <- mutate(df_datos, Porcentaje = (df_datos$R/4)*100)
  #Sesion de entrenamiento 
  df_datos$Sesion <- i
  #Modalidad
  df_datos$Modalidad <- "Expandido"
  #Observador
  df_datos$Observador <- "mcm"
  #Grupo
  df_datos$Grupo <- "lt"
  #Lista datos Observador JJR
  ls_datos_ct[i+13] <- list(df_datos)
  
}

## RT
## Concentrado 
## AT----------------------------------------------------------------------
# Ubico directorio donde de ecuentras los archivos .mat
setwd("D:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/RT MOT/Datos/Hockey/Concentrado/AT")
# Nombre de los archivos en la carpeta con extension .mat
files <- list.files(pattern = "*.mat", full.names = T)
# Cargo los datos
for (i in seq_along (files)){
  
  matlab_file  <- readMat(files[i])
  var_names    <- names(matlab_file$estructura.datos[,,1])
  ls_dat       <- matlab_file$estructura.datos
  mx_data    <- as.data.frame(ls_dat)
  df_datos <- as.data.frame(t(mx_data))
  df_datos$R <- unlist(df_datos$R)
  df_datos <- mutate(df_datos, Porcentaje = (df_datos$R/4)*100)
  #Sesion de entrenamiento 
  df_datos$Sesion <- i
  #Modalidad
  df_datos$Modalidad <- "Concentrado"
  #Observador
  df_datos$Observador <- "at-"
  #Grupo
  df_datos$Grupo <- "rt"
  #Lista datos Observador JJR
  ls_datos_ct[i+19] <- list(df_datos)
  
}

## Expandido 
## LFA ---------------------------------------------------------------------
# Ubico directorio donde se encuentran los archivos .mat
setwd("D:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/RT MOT/Datos/Hockey/Expandido/LFA")
# Nombre de los archivos en la carpeta con extension .mat
files <- list.files(pattern = "*.mat", full.names = T)
# Cargo los datos
for (i in seq_along (files)){
  
  matlab_file  <- readMat(files[i])
  var_names    <- names(matlab_file$estructura.datos[,,1])
  ls_dat       <- matlab_file$estructura.datos
  mx_data    <- as.data.frame(ls_dat)
  df_datos <- as.data.frame(t(mx_data))
  df_datos$R <- unlist(df_datos$R)
  df_datos <- mutate(df_datos, Porcentaje = (df_datos$R/4)*100)
  #Sesion de entrenamiento 
  df_datos$Sesion <- i
  #Modalidad
  df_datos$Modalidad <- "Expandido"
  #Observador
  df_datos$Observador <- "lfa"
  #Grupo
  df_datos$Grupo <- "rt"
  #Lista datos Observador JJR
  ls_datos_ct[i+20] <- list(df_datos)
  
}
## LMS ---------------------------------------------------------------------
# Ubico directorio donde se encuentran los archivos .mat
setwd("D:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/RT MOT/Datos/Hockey/Expandido/LMS")
# Nombre de los archivos en la carpeta con extension .mat
files <- list.files(pattern = "*.mat", full.names = T)
# Cargo los datos
for (i in seq_along (files)){
  
  matlab_file  <- readMat(files[i])
  var_names    <- names(matlab_file$estructura.datos[,,1])
  ls_dat       <- matlab_file$estructura.datos
  mx_data    <- as.data.frame(ls_dat)
  df_datos <- as.data.frame(t(mx_data))
  df_datos$R <- unlist(df_datos$R)
  df_datos <- mutate(df_datos, Porcentaje = (df_datos$R/4)*100)
  #Sesion de entrenamiento 
  df_datos$Sesion <- i
  #Modalidad
  df_datos$Modalidad <- "Expandido"
  #Observador
  df_datos$Observador <- "lms"
  #Grupo
  df_datos$Grupo <- "rt"
  #Lista datos Observador JJR
  ls_datos_ct[i+26] <- list(df_datos)
  
}

## MCM ---------------------------------------------------------------------
# Ubico directorio donde se encuentran los archivos .mat
setwd("D:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/RT MOT/Datos/Hockey/Expandido/MCM")
# Nombre de los archivos en la carpeta con extension .mat
files <- list.files(pattern = "*.mat", full.names = T)
# Cargo los datos
for (i in seq_along (files)){
  
  matlab_file  <- readMat(files[i])
  var_names    <- names(matlab_file$estructura.datos[,,1])
  ls_dat       <- matlab_file$estructura.datos
  mx_data    <- as.data.frame(ls_dat)
  df_datos <- as.data.frame(t(mx_data))
  df_datos$R <- unlist(df_datos$R)
  df_datos <- mutate(df_datos, Porcentaje = (df_datos$R/4)*100)
  #Sesion de entrenamiento 
  df_datos$Sesion <- i
  #Modalidad
  df_datos$Modalidad <- "Expandido"
  #Observador
  df_datos$Observador <- "mcm"
  #Grupo
  df_datos$Grupo <- "rt"
  #Lista datos Observador JJR
  ls_datos_ct[i+32] <- list(df_datos)
  
}










# Grupo RT ----------------------------------------------------------------
## AFB ---------------------------------------------------------------------
# Ubico directorio donde de ecuentras los archivos .mat
setwd("D:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/RT MOT/Datos/AFB/")
# Nombre de los archivos en la carpeta con extension .mat
files <- list.files(pattern = "*.mat", full.names = T)
# Cargo los datos
for (i in seq_along (files)){
  
  matlab_file  <- readMat(files[i])
  var_names    <- names(matlab_file$estructura.datos[,,1])
  ls_dat       <- matlab_file$estructura.datos
  mx_data    <- as.data.frame(ls_dat)
  df_datos <- as.data.frame(t(mx_data))
  df_datos$R <- unlist(df_datos$R)
  df_datos <- mutate(df_datos, Porcentaje = (df_datos$R/4)*100)
  #Sesion de entrenamiento 
  df_datos$Sesion <- i
  #Lista datos Observador JJR
  ls_datos[i + 18] <- list(df_datos)
  
}
## CIC ---------------------------------------------------------------------
# Ubico directorio donde de ecuentras los archivos .mat
setwd("D:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/RT MOT/Datos/CIC/")
# Nombre de los archivos en la carpeta con extension .mat
files <- list.files(pattern = "*.mat", full.names = T)
# Cargo los datos
for (i in seq_along (files)){
  
  matlab_file  <- readMat(files[i])
  var_names    <- names(matlab_file$estructura.datos[,,1])
  ls_dat       <- matlab_file$estructura.datos
  mx_data    <- as.data.frame(ls_dat)
  df_datos <- as.data.frame(t(mx_data))
  df_datos$R <- unlist(df_datos$R)
  df_datos <- mutate(df_datos, Porcentaje = (df_datos$R/4)*100)
  #Sesion de entrenamiento 
  df_datos$Sesion <- i
  #Lista datos Observador JJR
  ls_datos[i + 24] <- list(df_datos)
  
}
## MSZ ---------------------------------------------------------------------
# Ubico directorio donde de ecuentras los archivos .mat
setwd("D:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/RT MOT/Datos/MSZ/")
# Nombre de los archivos en la carpeta con extension .mat
files <- list.files(pattern = "*.mat", full.names = T)
# Cargo los datos
for (i in seq_along (files)){
  
  matlab_file  <- readMat(files[i])
  var_names    <- names(matlab_file$estructura.datos[,,1])
  ls_dat       <- matlab_file$estructura.datos
  mx_data    <- as.data.frame(ls_dat)
  df_datos <- as.data.frame(t(mx_data))
  df_datos$R <- unlist(df_datos$R)
  df_datos <- mutate(df_datos, Porcentaje = (df_datos$R/4)*100)
  #Sesion de entrenamiento 
  df_datos$Sesion <- i
  #Lista datos Observador JJR
  ls_datos[i + 30] <- list(df_datos)
  
}
## NGA ---------------------------------------------------------------------
# Ubico directorio donde de ecuentras los archivos .mat
setwd("D:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/RT MOT/Datos/NGA/")
# Nombre de los archivos en la carpeta con extension .mat
files <- list.files(pattern = "*.mat", full.names = T)
# Cargo los datos
for (i in seq_along (files)){
  
  matlab_file  <- readMat(files[i])
  var_names    <- names(matlab_file$estructura.datos[,,1])
  ls_dat       <- matlab_file$estructura.datos
  mx_data    <- as.data.frame(ls_dat)
  df_datos <- as.data.frame(t(mx_data))
  df_datos$R <- unlist(df_datos$R)
  df_datos <- mutate(df_datos, Porcentaje = (df_datos$R/4)*100)
  #Sesion de entrenamiento 
  df_datos$Sesion <- i
  #Lista datos Observador JJR
  ls_datos[i + 36] <- list(df_datos)
  
}




