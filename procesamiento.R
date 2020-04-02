#Experimento Modulacion de la atencion visual con MOT
#Cantidad de Observadores: 16
#Cargo solamente los datos de un observador

#cargo librerias
library(tidyverse)
library(R.matlab)
library(ggplot2)
library(quickpsy)
library(reshape2)
library(modelr)
options(na.action = na.warn)

#ubico el directorio donde se encuentran los archivos del pre test
setwd(paste("D:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/VENTANA ATENCION/Ventana version final/Datos/Pre test/", sep = ""))


#leo y ordeno las matrices
matlabFile  <- readMat('msz-1-Nov.mat')
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
datos_sin_Gaze <- datos[,-(7:8)]
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

#Obtener la ventana de atencion
# 1) Obtener las tablas para cada direccion
tabla_0 <- filter(averages, Direccion == 0 )
tabla_45 <- filter(averages, Direccion == 45 )
tabla_90 <- filter(averages, Direccion == 90 )
tabla_135 <- filter(averages, Direccion == 135 )


# 2) Graficar los datos
#Creo data frame para poder graficar los datos 
df_ventana <- data.frame( x = tabla_0$Separacion, D0 = tabla_0$p, D45 =  
                            
                            tabla_45$p, D90 = tabla_90$p, D135 = tabla_135$p)

gra <-ggplot(df_ventana, aes(as.factor(x), y = value, color = Direcion)) + 
  
  geom_point(aes(y = D0, col = "0°")) + 
  
  geom_point(aes(y = D45, col = "45°")) +
  
  geom_point(aes(y = D90, col = "90°")) + 
  
  geom_point(aes(y = D135, col = "135°")) +
  
  labs(x = "Separacion [°]", y = "Correctas [-]", title = "") 
  
gra  
#Grafico 0°
gra0 <-ggplot(df_ventana, aes(x, y = D0))+ 
  
  geom_point(aes(y = D0, col = "0°")) +
  
  labs(x = "Separacion [°]", y = "Correctas [-]", title = "") +
  
  theme(legend.title = element_blank()) +
  
  stat_smooth(method = "lm", color = "red")

gra0
 
#Grafico 45°
gra45 <-ggplot(df_ventana, aes(x, y = D45))+ 
  
  geom_point(aes(y = D45, col = "45°")) +
  
  labs(x = "Separacion [°]", y = "Correctas [-]", title = "") +
  
  theme(legend.title = element_blank()) +
  
  stat_smooth(method = "lm", color = "red")

gra45


#Grafico 90°
gra90 <-ggplot(df_ventana, aes(x, y = D90))+ 
  
  geom_point(aes(y = D90, col = "90°")) +
  
  labs(x = "Separacion [°]", y = "Correctas [-]", title = "") +
  
  theme(legend.title = element_blank()) +
  
  stat_smooth(method = "lm", color = "red")

gra90

#Grafico 135°
gra135 <-ggplot(df_ventana, aes(x, y = D135))+ 
  
  geom_point(aes(y = D135, col = "135°")) +
  
  labs(x = "Separacion [°]", y = "Correctas [-]", title = "") +
  
  theme(legend.title = element_blank()) +
  
  stat_smooth(method = "lm", color = "red")

gra135

##CARGO LOS DATOS POST TEST 
#ubico el directorio donde se encuentran los archivos del post test
setwd(paste("D:/Dropbox/Posdoc/Percepcion Deporte/Experimento MOT VA/Archivos Computadora Vision/Experimento Anibal/MATLAB_Diciembre/VENTANA ATENCION/Ventana version final/Datos/Post test/", sep = ""))



#leo y ordeno las matrices
matlabFile  <- readMat('msz-19-Dic-pos.mat')
varNames    <- names(matlabFile$estructura.datos[,,1])
datList     <- matlabFile$estructura.datos
#datList    <- lapply(datList, unlist, use.names=FALSE)
data_mat    <- as.data.frame(datList)
#names(data_mat) <- varNames
datospost <- as.data.frame(t(data_mat))
#veo que tipos de datos tengo
glimpse(datospost)
sapply(datospost,mode)
#convierto de list a numeric
datospost[c(varNames[1:6])] <-sapply(datospost[c(varNames[1:6])], as.numeric)
#creo data frame sin los datos del gaze
datospost_sin_Gaze <- datospost[,-(7:9)]
#redondeo los datos de la separacion
datospost_sin_Gaze$Separacion <- round(datospost_sin_Gaze$Separacion, digits = 0)
#convierto a tabla
tablapost <- tbl_df(datospost_sin_Gaze)  
#obtengo nuevas variables
tablapost_total <- mutate(tablapost, correctas_A = Cantidad.TGC.1 == Respuesta.A, 
                      correctas_B = Cantidad.TGC.2 == Respuesta.B)

#obtengo las respuesta correctas
tablapost_total <- mutate(tablapost_total, correctas = correctas_A == correctas_B)

#convierto a numero variables logicas
tablapost_total[c(7:9)] <- sapply(tablapost_total[c(7:9)], as.numeric)

#obtengo el n de cada presentacion
npost <- c(sum(tablapost_total$Direccion == 0 & tablapost_total$Separacion == 3),
       sum(tablapost_total$Direccion == 0 & tablapost_total$Separacion == 5),
       sum(tablapost_total$Direccion == 0 & tablapost_total$Separacion == 8),
       sum(tablapost_total$Direccion == 0 & tablapost_total$Separacion == 11),
       sum(tablapost_total$Direccion == 0 & tablapost_total$Separacion == 14),
       sum(tablapost_total$Direccion == 0 & tablapost_total$Separacion == 17),
       sum(tablapost_total$Direccion == 0 & tablapost_total$Separacion == 20),
       sum(tablapost_total$Direccion == 45 & tablapost_total$Separacion == 3),
       sum(tablapost_total$Direccion == 45 & tablapost_total$Separacion == 5),
       sum(tablapost_total$Direccion == 45 & tablapost_total$Separacion == 8),
       sum(tablapost_total$Direccion == 45 & tablapost_total$Separacion == 11),
       sum(tablapost_total$Direccion == 45 & tablapost_total$Separacion == 14),
       sum(tablapost_total$Direccion == 45 & tablapost_total$Separacion == 17),
       sum(tablapost_total$Direccion == 45 & tablapost_total$Separacion == 20),
       sum(tablapost_total$Direccion == 90 & tablapost_total$Separacion == 3),
       sum(tablapost_total$Direccion == 90 & tablapost_total$Separacion == 5),
       sum(tablapost_total$Direccion == 90 & tablapost_total$Separacion == 8),
       sum(tablapost_total$Direccion == 90 & tablapost_total$Separacion == 11),
       sum(tablapost_total$Direccion == 90 & tablapost_total$Separacion == 14),
       sum(tablapost_total$Direccion == 90 & tablapost_total$Separacion == 17),
       sum(tablapost_total$Direccion == 90 & tablapost_total$Separacion == 20),
       sum(tablapost_total$Direccion == 135 & tablapost_total$Separacion == 3),
       sum(tablapost_total$Direccion == 135 & tablapost_total$Separacion == 5),
       sum(tablapost_total$Direccion == 135 & tablapost_total$Separacion == 8),
       sum(tablapost_total$Direccion == 135 & tablapost_total$Separacion == 11),
       sum(tablapost_total$Direccion == 135 & tablapost_total$Separacion == 14),
       sum(tablapost_total$Direccion == 135 & tablapost_total$Separacion == 17),
       sum(tablapost_total$Direccion == 135 & tablapost_total$Separacion == 20))

#promedios de las respuesta
averagespost <- tablapost_total %>% group_by( Direccion, Separacion) %>% 
  summarise(n = n(), nYes = sum(correctas), nNo = n - nYes, p = nYes / n)

#Obtener la ventana de atencion
# 1) Obtener las tablas para cada direccion
tablapost_0 <- filter(averagespost, Direccion == 0 )
tablapost_45 <- filter(averagespost, Direccion == 45 )
tablapost_90 <- filter(averagespost, Direccion == 90 )
tablapost_135 <- filter(averagespost, Direccion == 135 )
# 2) Graficar los datos
#Creo data frame para poder graficar los datos 
df_ventanapost <- data.frame( x = tablapost_0$Separacion, D0 = tablapost_0$p, 
                              
                              D45 = tablapost_45$p, D90 = tablapost_90$p, 
                              
                              D135= tablapost_135$p)

grapost <-ggplot(df_ventanapost, aes(as.factor(x), y = value, 
                                     color = Direcion)) + 
  
  geom_point(aes(y = D0, col = "0°")) + 
  
  geom_point(aes(y = D45, col = "45°")) +
  
  geom_point(aes(y = D90, col = "90°")) + 
  
  geom_point(aes(y = D135, col = "135°")) +
  
  labs(x = "Separacion [°]", y = "Correctas [-]", title = "") 

grapost

#Grafico 0°
grapost0 <-ggplot(data = df_ventana) + 
  
  geom_point(aes(x = df_ventana$x, y = df_ventana$D0, col = "0°" ), color = 
               
                "blue") +
  
  geom_smooth(aes(x = df_ventana$x, y = df_ventana$D0), method = lm, se = FALSE) +
  
  
  geom_point(aes(x = df_ventanapost$x, y = df_ventanapost$D0, col = "0°"), 
             
                color = "red") +
  
  geom_smooth(aes(x = df_ventanapost$x, y = df_ventanapost$D0), method = lm, 
              
                color = "red", se = FALSE) +
  
  labs(x = "Separacion [°]", y = "Correctas [-]", title = "Direccion 0°") 
  
  
  #stat_smooth(method = "lm", color = "red")

grapost0

#Grafico 45°
grapost45 <-ggplot(data = df_ventana) + 
  
  geom_point(aes(x = df_ventana$x, y = df_ventana$D0, col = "0°" ), color = 
             
              "blue") +
  
  geom_smooth(aes(x = df_ventana$x, y = df_ventana$D45), method = lm) +
  
  
  geom_point(aes(x = df_ventanapost$x, y = df_ventanapost$D0, col = "0°"), 
             
             color = "red") +
  
  geom_smooth(aes(x = df_ventanapost$x, y = df_ventanapost$D45), method = lm, 
             
             color = "red") +
  
  labs(x = "Separacion [°]", y = "Correctas [-]", title = "Dir 45°") 
  
#stat_smooth(method = "lm", color = "red")

grapost45


#Grafico 90°
grapost90 <-ggplot(data = df_ventana) + 
  
  geom_point(aes(x = df_ventana$x, y = df_ventana$D0, col = "0°" ), color = 
               
             "blue") +
  
  geom_smooth(aes(x = df_ventana$x, y = df_ventana$D90), method = lm) +
  
  
  geom_point(aes(x = df_ventanapost$x, y = df_ventanapost$D0, col = "0°"), 
             
             color = "red") +
  
  geom_smooth(aes(x = df_ventanapost$x, y = df_ventanapost$D90), method = lm, 
              
             color = "red") +
  
  labs(x = "Separacion [°]", y = "Correctas [-]", title = "Dir 90°") 
  
  

#stat_smooth(method = "lm", color = "red")

grapost90

#Grafico 135°
grapost135 <-ggplot(data = df_ventana) + 
  
  geom_point(aes(x = df_ventana$x, y = df_ventana$D0, col = "0°" ), color = 
               
              "blue") +
  
  geom_smooth(aes(x = df_ventana$x, y = df_ventana$D135), method = lm) +
  
  
  geom_point(aes(x = df_ventanapost$x, y = df_ventanapost$D0, col = "0°"), 
             
              color = "red") +
  
  geom_smooth(aes(x = df_ventanapost$x, y = df_ventanapost$D135), method = lm, 
              
              color = "red") +
  
  labs(x = "Separacion [°]", y = "Correctas [-]", title = "Dir 135°") 
  
  

#stat_smooth(method = "lm", color = "red")

grapost135

#Grafico de cajas
#1 Convierto a factor la separacion de datos Pre y Post
dfboxpre <- data.frame(x = averages$Separacion, 
                        y = averages$p , z = averages$Direccion)
#dfboxpre$x <- factor(dfboxpre$x)
#dfboxpre$z <- factor(dfboxpre$z)
dfboxpre <- mutate(dfboxpre, Condicion = "pre")

dfboxpost <- data.frame(x = averagespost$Separacion, 
                        y = averagespost$p , z = averagespost$Direccion)
#dfboxpost$x <- factor(dfboxpost$x)
#dfboxpost$z <- factor(dfboxpost$z)
dfboxpost <- mutate(dfboxpost, Condicion = "post")

dfbox <- bind_rows(dfboxpre,dfboxpost)

dfbox$Condicion <- factor(dfbox$Condicion)
dfbox$x <- factor(dfbox$x)
dfbox$z <- factor(dfbox$z)

ggplot(data = dfbox, aes(x= x, y= y)) + geom_boxplot(aes(fill=Condicion)) +
  facet_wrap( ~ x, scales="free_x")

ggplot(data = dfbox, aes(x= z, y= y)) + geom_boxplot(aes(fill=Condicion)) +
  facet_wrap( ~ z, scales="free_x")



Box_Sep_Fact_Pre <- ggplot(dfboxpre, aes(x = as.factor(x), y= y)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) + 
    labs(x = "Separacion [°]", y = "Correctas [-]", title = "") +
    geom_jitter(shape=16, position=position_jitter(0.2))

Box_Sep_Fact_Pre 

Box_Sep_Fact_Post <- ggplot(dfboxpost, aes(x = as.factor(x), y= y)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) + 
    labs(x = "Separacion [°]", y = "Correctas [-]", title = "") +
    geom_jitter(shape=16, position=position_jitter(0.2))

Box_Sep_Fact_Post 


#Grafico panel con geom_point

Panel_point <- ggplot(dfbox, aes(as.numeric(paste(x)), y)) + 
  
  geom_point(aes(as.numeric(paste(x)),y)) + 
  
  facet_grid(Condicion~z) +
  
  stat_smooth(method = "lm") +
  
  labs(x = "Separacion [°]", y = "Correctas [-]", title = "") 

Panel_point

#Tratando de ajustar un modelo los datos

dfSeparacion <- tabla_total %>% group_by(Separacion) %>% 
       summarise(n = n(), nYes = sum(correctas), nNo = n - nYes, p = nYes / n)

dfSeparacionpost <- tablapost_total %>% group_by(Separacion) %>% 
  summarise(n = n(), nYes = sum(correctas), nNo = n - nYes, p = nYes / n)

pData <- ggplot(dfSeparacion, aes(x = Separacion, y = p)) + 
  geom_point(color = "blue") + stat_smooth(method = "lm") +
  geom_point(aes(x = dfSeparacionpost$Separacion, y = dfSeparacionpost$p),color= "red") +
  stat_smooth(aes(x = dfSeparacionpost$Separacion, 
                  y = dfSeparacionpost$p),method = "lm", color = "red")
pData

#Modelo lineal

LinMod <- lm(p ~ Separacion, data = tabla_0)
# Analicemos lo que nos dice la salida del modelo
summary(LinMod)
pData <- ggplot(tabla_0, aes(x = Separacion, y = p)) + 
  geom_point() + geom_abline(intercept = LinMod$coefficients[1], slope = LinMod$coefficients[2], linetype="dashed", size=1.5)
pData

##Creo una lista con las tablas de respuesta en cada direccion

ListDireccion <-list(tabla_0, tabla_45, tabla_90, tabla_135,
                     tablapost_0, tablapost_45, tablapost_90, tablapost_135)

#ListDireccion <-list(tablapost_0, tablapost_45, tablapost_90, tablapost_135)
#Funcion para aplicar un modelo lineal 

#ValoresModelo <-function(ListTbl){
  
#  out <- vector("double", length(ListTbl))
  
#  for (i in seq_along(ListTbl))
    
#    out[i] <- lm(p ~ Separacion, data = ListTbl[[i]])
  
#}

#ValoresModelo(ListDireccion)

# Aplico la funcion de modelado lineal a cada elemento de la lista 
# para obtener los coeficientes de la recta que ajusta a cada set de datos

models <- ListDireccion %>% 
  
  map(~lm(p ~ Separacion, data = .)) 
    

# Funcion para obtener el valor umbral de separacion del estimulo
# para el cual el observador acerta con un ratio de 0.7

umbrales <- vector("double", length(models))
for (i in seq_along(models))  {
  
  umbrales[i] <- (0.5-models[[i]]$coefficients[1])/models[[i]]$coefficients[2]
                              
  }

a1 <- vector("double", length(models))
a2 <- vector("double", length(models))
for (i in seq_along(models)) {
  a1[[i]] <- models[[i]]$coefficients[1]
  a2[[i]] <- models[[i]]$coefficients[2]
  
  }


ggplot(tabla_0, aes(Separacion, p)) + 
  
geom_abline(aes(intercept = a1[1], slope = a2[1]),alpha = 1/4, size = 1) +
  
geom_point() + 
  
geom_abline(aes(intercept = a1[2], slope = a2[2]),alpha = 1/4, color ="red", size = 1) +
  
geom_point(aes(tabla_45$Separacion, tabla_45$p), color = "red") + 

geom_abline(aes(intercept = a1[3], slope = a2[3]),alpha = 1/4,color = "blue", size = 1) +
  
geom_point(aes(tabla_90$Separacion, tabla_90$p), color = "blue") + 
  
geom_abline(aes(intercept = a1[4], slope = a2[4]),alpha = 1/4, color ="green", size = 1)+

geom_point(aes(tabla_135$Separacion,tabla_135$p), color = "green") 


#Prediciones del modelo lineal
modpre <- lm(p ~ Separacion, data = averages)
grid <- averages %>% 
  data_grid(Separacion) %>% 
  add_predictions(modpre)

ggplot(averages, aes(Separacion)) + 
  geom_point(aes(y = p)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)

modpos <- lm(p ~ Separacion, data = averagespost)
grid <- averagespost %>% 
  data_grid(Separacion) %>% 
  add_predictions(modpos)

ggplot(averagespost, aes(Separacion)) + 
  geom_point(aes(y = p)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)

ggplot(averages, aes(as.factor(Separacion), modpre$residuals)) +
  geom_ref_line(h = 0, colour = "blue", size = 1) +
  geom_point() 
  
ggplot(averagespost, aes(as.factor(Separacion), modpos$residuals)) +
  geom_ref_line(h = 0, colour = "blue", size = 1) +
  geom_point() 






