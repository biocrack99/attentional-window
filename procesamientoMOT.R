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
library(ggpval)
library(ggprism)

# Cargar datos por Grupo y por Observador
# Existe una carpeta por Observador
#Lista para los datos cargados
ls_datos <- vector("list")
ls_datos_ct <- vector("list") 
nu_sesion <- 6
# Grupo LT ----------------------------------------------------------------
## JJR ---------------------------------------------------------------------
# Ubico directorio donde de ecuentras los archivos .mat
setwd("B:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/LT MOT/Datos/JJR/")
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
setwd("B:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/LT MOT/Datos/MAB/")
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
setwd("B:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/LT MOT/Datos/MDN/")
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
setwd("B:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/LT MOT/Datos/Hockey/Concentrado/AT")
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
setwd("B:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/LT MOT/Datos/Hockey/Expandido/LFA")
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
setwd("B:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/LT MOT/Datos/Hockey/Expandido/LMS")
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
setwd("B:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/LT MOT/Datos/Hockey/Expandido/MCM")
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
setwd("B:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/RT MOT/Datos/Hockey/Concentrado/AT")
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
setwd("B:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/RT MOT/Datos/Hockey/Expandido/LFA")
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
setwd("B:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/RT MOT/Datos/Hockey/Expandido/LMS")
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
setwd("B:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/RT MOT/Datos/Hockey/Expandido/MCM")
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
setwd("B:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/RT MOT/Datos/AFB/")
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
setwd("B:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/RT MOT/Datos/CIC/")
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
setwd("B:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/RT MOT/Datos/MSZ/")
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
setwd("B:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/MOT/RT MOT/Datos/NGA/")
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

#Elimino variables innecesarias
rm(df_datos, ls_dat, mx_data, matlab_file)


# Procesamiento ---------------------------------------------------------------
#1- Comparar porcentaje de aciertos entre 1 y 6 sesion para cada observador y cada grupo
#Grupo LT----------------------------------------------------------------------
df_obs_lt <- data.frame(Observador = rep(c("jjr", "mab", "mdn"), each = 300), 
                        Porcentaje = c(ls_datos[[1]]$Porcentaje, 
                                       ls_datos[[6]]$Porcentaje, 
                                       ls_datos[[7]]$Porcentaje, 
                                       ls_datos[[12]]$Porcentaje, 
                                       ls_datos[[13]]$Porcentaje, 
                                       ls_datos[[18]]$Porcentaje ), 
                        Session = rep(c("Session 1", "Session 6"), 3, each=150))
 
df_obs <- df_obs_lt %>% 
                      group_by(Observador, Session) %>% 
                      summarise(Media = mean(Porcentaje), SD = sd(Porcentaje))

#Test t apareado
TestT <- function(df, name, sesionpre, sesionpos) {
  
  pre <- subset(df, Observador == name & Session == sesionpre, select = c(Porcentaje))
  pre <- pre[,1]
  pos <- subset(df, Observador == name & Session == sesionpos , select = c(Porcentaje))
  pos <- pos[,1]
  test <- t.test(pre, pos, paired = T, alternative = "two.sided")
  print(test$p.value)
  
}

#JJR
testjjr <- TestT(df_obs_lt, 'jjr', 'Session 1', 'Session 6')
#MAB
testmab <- TestT(df_obs_lt, 'mab', 'Session 1', 'Session 6')
#MDN
testmdn <- TestT(df_obs_lt, 'mdn', 'Session 1', 'Session 6')


#Gráfico de barras para comparar el rendimiento en el primer y el ultimo entrenamiento con MOT para los sujetos del grupo LT

p_lt <- ggplot(df_obs, aes(x=Session, y=Media)) + 
  geom_bar(aes(fill = Session), stat="identity", color="black", 
           position=position_dodge()) +
  geom_linerange(aes(ymin=Media, ymax=Media+SD), width=.2, size = 1,
                position=position_dodge(.9)) +
  facet_wrap(~ Observador) +
  labs(y = "Mean Percentage Target Id [%]")

# p_lt + labs(y = "Mean Target id [%]")+
#   theme_bw() +
#   scale_fill_manual(values=c('#999999','#E69F00')) +
#   theme(axis.text = element_text(size = 23),
#         axis.title = element_blank(),
#         legend.text = element_text(size = 20),
#         legend.title = element_blank(),
#         strip.text = element_text(size = 25)) 

#fucion para elimar el cero de la izquierda en los valores p
numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.3f", val)) }

#Valores p
valores_p_lt <- tibble::tribble(
  ~group1, ~group2, ~p.adj,  ~y.position, ~Observador,
  "Session 1","Session 6",    "<.001",  110, "jjr",
  "Session 1","Session 6",    "<.001", 110,  "mab",
  "Session 1","Session 6",    paste("p =", numformat(signif(testmdn, digits = 3)), sep =" "),  110, "mdn"
  )

#Diseño del gráfico agregando valores de significancia
p_lt + theme_bw() +
  theme(axis.title.y = element_text(size = 23),
        legend.position = "none",
        axis.text = element_text(size = 23),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 23),
        axis.title = element_blank(),
        legend.text = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size = 20)) +
  add_pvalue(valores_p_lt, label.size = 5)
  


#Grupo RT--------------------------------------------------------
df_obs_rt <- data.frame(Observador = rep(c("afb", "cic", "msz", "nga"), each = 300), 
                        Porcentaje = c( ls_datos[[19]]$Porcentaje,
                                        ls_datos[[25]]$Porcentaje,                                                    
                                        ls_datos[[26]]$Porcentaje,                                                    
                                        ls_datos[[32]]$Porcentaje,                                                    
                                        ls_datos[[33]]$Porcentaje,                                                    
                                        ls_datos[[38]]$Porcentaje,
                                        ls_datos[[37]]$Porcentaje,
                                        ls_datos[[42]]$Porcentaje
                                       ), 
                        Session = rep(c("Session 1", "Session 6"), 4, each=150)
              )

df_obs <- df_obs_rt %>% 
                  group_by(Observador, Session) %>% 
                  summarise(Media = mean(Porcentaje), SD = sd(Porcentaje))
#Test t apareado
#AFB
testafb <- TestT(df_obs_rt, 'afb', 'Session 1', 'Session 6')
#CIC
testcic <- TestT(df_obs_rt, 'cic', 'Session 1', 'Session 6')
#MSZ
testmsz <- TestT(df_obs_rt, 'msz', 'Session 1', 'Session 6')
#NGA
testnga <- TestT(df_obs_rt, 'nga', 'Session 1', 'Session 6')

#Gráfico de barras para comparar el rendimiento en el primer y el ultimo entrenamiento con MOT para los sujetos del grupo LT
p_rt <- ggplot(df_obs, aes(x=Session, y=Media)) + 
  geom_bar(aes(fill = Session), stat="identity", color="black", 
           position=position_dodge()) +
  geom_linerange(aes(ymin=Media, ymax=Media+SD), width=.2, size = 1,
                 position=position_dodge(.9)) +
  facet_wrap(~ Observador, ncol = 4) +
  labs(y = "Mean Percentage Target Id [%]")

#Valores p
valores_p_rt <- tibble::tribble(
  ~group1, ~group2, ~p.adj,  ~y.position, ~Observador,
  "Session 1","Session 6", paste("p =", numformat(signif(testafb, digits = 3)), sep =" "),  110, "afb",
  "Session 1","Session 6", paste("p =", numformat(signif(testcic, digits = 3)), sep =" "),  110, "cic",
  "Session 1","Session 6", "<.001",  110, "msz",
  "Session 1","Session 6", paste("p =", numformat(signif(testnga, digits = 3)), sep =" "),  110, "nga"
)

#Diseño del gráfico agregando valores de significancia
p_rt + theme_bw() +
  theme(axis.title.y = element_text(size = 23),
        legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 23),
        axis.title = element_blank(),
        legend.text = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size = 20)) +
  add_pvalue(valores_p_rt, label.size = 5)


#Grupo CT--------------------------------------------------------
#AT
df_at <- data.frame( Observador = "at",
                     Sesion = rep(c ("Sesion LT", "Sesion RT"), each = 150), 
                     Porcentaje = c( ls_datos_ct[[1]]$Porcentaje, 
                                     ls_datos_ct[[20]]$Porcentaje)
          )

#Dataframe para graficar las medias del acierto
df_at <- df_at %>% 
         group_by(Sesion) %>% 
         summarise(Media = mean(Porcentaje))

#Analisis del tiempo de reaccion de at
#Media, desviacion y errados.
db_rt <- mean(unlist(ls_datos_ct[[20]]$RT), na.rm = TRUE)
db_rt_sd <- sd(unlist(ls_datos_ct[[20]]$RT), na.rm = TRUE)
in_errados <- (sum(is.na(ls_datos_ct[[20]]$RT)))/150

#Sujetos con entrenamiento expandido LFA, LMS, MCM
#Modo LT
df_obs_ct_lt <- data.frame(Observador = rep(c("lfa", "lms", "mcm"), each = 100), 
                        Media = c(ls_datos_ct[[2]]$Porcentaje, 
                                       ls_datos_ct[[7]]$Porcentaje, 
                                       ls_datos_ct[[8]]$Porcentaje, 
                                       ls_datos_ct[[13]]$Porcentaje, 
                                       ls_datos_ct[[14]]$Porcentaje, 
                                       ls_datos_ct[[19]]$Porcentaje ), 
                        Session = rep(c("Session 1", "Session 6"), 3, each=50))

df_ct_lt <- df_obs_ct_lt %>% 
  group_by(Observador, Session) %>% 
  summarise(Porcentaje = mean(Media), SD = sd(Media)) #%>%
  
#Modo RT
df_obs_ct_rt <- data.frame(Observador = rep(c("lfa", "lms", "mcm"), each = 100), 
                           Media = c(ls_datos_ct[[21]]$Porcentaje, 
                                          ls_datos_ct[[26]]$Porcentaje, 
                                          ls_datos_ct[[27]]$Porcentaje, 
                                          ls_datos_ct[[32]]$Porcentaje, 
                                          ls_datos_ct[[33]]$Porcentaje, 
                                          ls_datos_ct[[38]]$Porcentaje ), 
                           Session = rep(c("Session 1", "Session 6"), 3, each=50))


df_ct_rt <- df_obs_ct_rt %>% 
  group_by(Observador, Session) %>% 
  summarise(Porcentaje = mean(Media), SD = sd(Media))
  
#Dataframe total 
#Unidos los modos lt y rt
df_ct <- rbind(df_obs_ct_lt, df_obs_ct_rt)
df_ct <- rename(df_ct, Porcentaje = Media) 

#Test t apareado
testlfa <- TestT(df_ct, 'lfa', 'Session 1', 'Session 6')

testlms <- TestT(df_ct, 'lms', 'Session 1', 'Session 6')

testmcm <- TestT(df_ct, 'mcm', 'Session 1', 'Session 6')

df_ct <- rename(df_ct, Media = Porcentaje) 

df_ct <- df_ct %>% 
  group_by(Observador, Session) %>% 
  summarise(Porcentaje = mean(Media), SD = sd(Media))

p_ct <- ggplot(df_ct, aes(x=Session, y=Porcentaje)) + 
  geom_bar(aes(fill = Session), stat="identity", color="black", 
           position=position_dodge()) +
  geom_linerange(aes(ymin=Porcentaje, ymax=Porcentaje+SD), width=.2, size = 1,
                 position=position_dodge(.9)) +
  facet_wrap(~ Observador) +
  labs(y = "Mean Percentage Target Id [%]")

#Valores p
valores_p_ct <- tibble::tribble(
  ~group1, ~group2, ~p.adj,  ~y.position, ~Observador,
  "Session 1","Session 6", paste("p =", numformat(signif(testlfa, digits = 3)), sep =" "),  110, "lfa",
  "Session 1","Session 6", paste("p =", numformat(signif(testlms, digits = 3)), sep =" "),  110, "lms",
  "Session 1","Session 6", paste("p =", numformat(signif(testmcm, digits = 3)), sep =" "),  110, "mcm")

#Diseño del gráfico agregando valores de significancia
p_ct + theme_bw() +
  theme(axis.title.y = element_text(size = 23),
        legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 23),
        axis.title = element_blank(),
        legend.text = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size = 20)) +
  add_pvalue(valores_p_ct, label.size = 5)




# Procesamiento Marzo 2021------------------------------------------------------

#Dataframe Grupo No Deportista
df_obs_lt$Grupo <- c("Carga")
df_obs_rt$Grupo <- c("Reaccion")
df_obs_no_deportistas<- rbind(df_obs_lt,df_obs_rt)
df_obs_no_deportistas$Expertisia <- rep(c("No deportista"), 2100)

#Dataframe Grupo Deportista
df_obs_deportistas <- data.frame(Observador = rep(c("lfa", "lms", "mcm", "lfa", "lms", "mcm"), each = 100), 
                                 Porcentaje = c(ls_datos_ct[[2]]$Porcentaje, 
                                          ls_datos_ct[[7]]$Porcentaje, 
                                          ls_datos_ct[[8]]$Porcentaje, 
                                          ls_datos_ct[[13]]$Porcentaje, 
                                          ls_datos_ct[[14]]$Porcentaje, 
                                          ls_datos_ct[[19]]$Porcentaje,
                                          ls_datos_ct[[21]]$Porcentaje,
                                          ls_datos_ct[[26]]$Porcentaje,
                                          ls_datos_ct[[27]]$Porcentaje,
                                          ls_datos_ct[[32]]$Porcentaje,
                                          ls_datos_ct[[33]]$Porcentaje,
                                          ls_datos_ct[[38]]$Porcentaje),
                                 Sesion = rep(c("Sesion 1", "Sesion 6"), 6, each=50),
                                 Grupo = rep(c("Carga", "Reaccion"), each = 300),
                                 Expertisia = rep(c("Deportista"), 600))
                                  

#Dataframe para grafico de barras
df_grafico_barras <- rbind(df_obs_no_deportistas,df_obs_deportistas)
#Grafico
df_prueba <- df_grafico_barras %>% 
  group_by(Expertisia, Sesion) %>% 
  summarise(Media = mean(Porcentaje))

ggplot(data = filter(df_prueba, Sesion == "Sesion 6")) + 
  geom_col(aes(x = Expertisia, y = Media), 
           position = "dodge") 
         
ggplot(data=df_prueba, aes(x=Sesion, y=Media, fill=Expertisia)) +
  geom_bar(stat="identity", position=position_dodge())


# Procesamiento InVision 2021------------------------------------------------------

#Coordenadas de las pelotas del primer trial de MOT con carga atencional que se presentaron
#a un observador
df_coordenadas_trial <- data.frame(Pelotas = rep(c("P1","P2","P3","P4","P5","P6","P7","P8", "P9", "P10", "P11", "P12"), each = 160), 
                                   x = as.vector(ls_datos[[1]][["coordenadas"]][["1.120"]][,,1,]), 
                                   y = as.vector(ls_datos[[1]][["coordenadas"]][["1.120"]][,,2,]),
                                   time = rep(seq(0,8-0.05, by = 0.05), 12))

plot_coor <- ggplot() + 
  geom_point(data = df_coordenadas_trial, aes(x,y,  color = time)) +
  geom_line(aes(x = as.vector(ls_datos[[1]][["XGaze.mm"]][["1.120"]])*(1024/1650) + 512,
                 y = as.vector(ls_datos[[1]][["YGaze.mm"]][["1.120"]])*(1024/1650) + 768/2))+
  geom_smooth(aes(x = as.vector(ls_datos[[1]][["XGaze.mm"]][["1.120"]])*(1024/1650) + 512,
                y = as.vector(ls_datos[[1]][["YGaze.mm"]][["1.120"]])*(1024/1650) + 768/2)) 


plot_coor 

  
  

#

