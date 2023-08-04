#GRUPO: IGNACIO FERNÁNDEZ SÁNCHEZ-PASCUALA, FERMÍN GONZÁLEZ PEREIRO, JAVIER CASTELLANO SORIA
library(TeachingDemos)

datos <- read.csv("D:\\Yo\\Universidad\\Curso 20-21\\Estadística\\Trabajo\\pulsmujer.txt", header = FALSE, sep = "\t")
tabla <- as.data.frame(table(datos)) #Hallamos tabla de frecuencias
relativa<-prop.table(tabla$Freq) #Hallamos la relativa
relativa_acum<-cumsum(relativa) #Hallamos la acumulada
datos_sin_rep<-list() #tabla$datos es un Factor, vamos a crear con un for una lista que tenga sus valores para poder representarlos en el eje X
titulo = "DISTRIBUCIÓN EMPÍRICA - I. CONFIANZA (Pulsaciones M)"
nombre_x = "Pulsaciones (Mujeres)"
for (j in tabla$datos){
  datos_sin_rep<-c(datos_sin_rep, as.numeric(j)) #Por si son caracteres
}


longitud = length(relativa_acum)
limx = datos_sin_rep[[longitud]]
for (i in 1:(longitud-1)){
  if (i == 1){
    plot(c(datos_sin_rep[[i]], datos_sin_rep[[i+1]]), c(relativa_acum[i], relativa_acum[i]), type = "l", xlim = c(datos_sin_rep[[1]], limx), ylim = c(-0.2, 1.2), ylab = "", xlab = nombre_x, main = titulo)
    par(new=TRUE)
  }else{ #Para no superponer títulos
    plot(c(datos_sin_rep[i], datos_sin_rep[[i+1]]), c(relativa_acum[i], relativa_acum[i]), type = "l", xlim = c(datos_sin_rep[[1]], limx), ylim = c(-0.2, 1.2), ylab = "", xlab = "", main = "")
    par(new=TRUE)
  }
}


#Ahora vamos a hallar los intervalos de confianza (0.95) para la función de distribución de la normal que usamos como modelo (nos basamos en el TCL)

icsup <- list()#los extremos superiores del intervalo de confianza
icinf <- list()#los extremos inferiores del intervalo de confianza
tam <- length(datos[,1])#tamaño de la muestra
for (i in 1:(longitud)){
  icsup <- c(icsup, relativa_acum[i]+(1.96)/(2*(tam)^(1/2)))
  icinf <- c(icinf, relativa_acum[i]-(1.96)/(2*(tam)^(1/2)))
}

for (i in 1:(longitud-1)){
  if (i == 1){
    plot(c(datos_sin_rep[[i]], datos_sin_rep[[i+1]]), c(icsup[[i]], icsup[[i]]), type = "l", xlim = c(datos_sin_rep[[1]], limx), ylim = c(-0.2, 1.2), ylab = "", xlab = nombre_x, main = titulo, col = "blue")
    par(new=TRUE)
    plot(c(datos_sin_rep[[i]], datos_sin_rep[[i+1]]), c(icinf[[i]], icinf[[i]]), type = "l", xlim = c(datos_sin_rep[[1]], limx), ylim = c(-0.2, 1.2), ylab = "", xlab = "", main = "", col = "blue")
    par(new=TRUE)
  }else{ #Para no superponer títulos
    plot(c(datos_sin_rep[[i]], datos_sin_rep[[i+1]]), c(icsup[[i]], icsup[[i]]), type = "l", xlim = c(datos_sin_rep[[1]], limx), ylim = c(-0.2, 1.2), ylab = "", xlab = "", main = "", col = "blue")
    par(new=TRUE)
    plot(c(datos_sin_rep[[i]], datos_sin_rep[[i+1]]), c(icinf[[i]], icinf[[i]]), type = "l", xlim = c(datos_sin_rep[[1]], limx), ylim = c(-0.2, 1.2), ylab = "", xlab = "", main = "", col = "blue")
    par(new=TRUE)
  }
}

#------------------------------------------------------------------------------------------------------

#Ahora con los datos de la relativa_acum tipificados vamos a comparar la distribución empírica con la N(0, 1)

#Dibujemos la distribución normal (0,1)
#Metemos tabla de la normal desde 0 hasta 3.9 de 0.1 en 0.1
relativa_acum_normal <- read.csv("D:\\Yo\\Universidad\\Curso 20-21\\Estadística\\Trabajo\\tabla_normal.txt", header = FALSE, sep = "\t") #Nos da valores de la distribución desde el 0
eje_x <- list()
eje_y <- list()
eje_x <- c(eje_x, -3.9)
eje_y <- c(eje_y, 0)
titulo2 = "N(0,1) - EMPÍRICA TIPIFICADA (Pulsaciones M)"

for (i in 1:38){#ponemos lo que corresponde a los ejes para x negativas
  eje_x <- c(eje_x, -3.9 + i*0.1)
  eje_y <- c(eje_y, 1 - relativa_acum_normal[40-i, 2])#Por simetría de la normal
}
#Completamos los ejes con la parte positiva de la x
for (i in 1:40){
  eje_x <- c(eje_x, relativa_acum_normal[i,1])
  eje_y <- c(eje_y, relativa_acum_normal[i, 2])
}

plot(eje_x, eje_y, type = "l", xlim = c(-4, 4), ylim = c(0, 1), ylab = "", xlab = "", main = titulo2, col = "red")
#Vamos a dibujar un polígono de frecuencias de la variable tipificada suponiendo la media y la desviación típica las obtenidads con la muestra
media = mean(datos[,1])
desviacion = sd(datos[,1])
eje_x_tip <- list()
for (i in 1:longitud){
  eje_x_tip <- c(eje_x_tip, (datos_sin_rep[[i]]-media)/desviacion) 
}
par(new=TRUE)#Mantener dibujo
#Dibujamos el polígono de frecuencias relativas acumuladas
plot(eje_x_tip, relativa_acum, type = "l", xlim = c(-4, 4), ylim = c(0, 1), ylab = "", xlab = "", main = "", col = "blue")



#-------------------------------------------------------------------------------------------------------
#Vamos a hacer contrastes de hipótesis
alpha = 0.05
#Vamos a hallar el intervalo en el cual si la hipótesis inicial (sobre la media) pertenece a dicho intervalo, no se rechazaría teniendo en cuenta un contraste bilateral
#Empezamos desde la media muestral hasta que se rechace la hipótesis
precision = 0.001
extremo_1 = 0
extremo_2 = 0
extremo_s1 = 0
extremo_s2 = 0

for (i in 1:1000000){
  pvalor<-t.test(datos[,1], mu=media+i*precision, conf.level = 1-alpha, alternative = "two.side")$p.value
  if (pvalor<0.05){
    extremo_2 = media+(i-1)*precision
    break
  }
}
#Ahora lo mismo para el extremo izquierdo
for (i in 1:10000){
  pvalor<-t.test(datos[,1], mu=media-i*precision, conf.level = 1-alpha, alternative = "two.side")$p.value
  if (pvalor<0.05){
    extremo_1 = media-(i-1)*precision
    break
  }
}

#Vamos a hacerlo con la desviación típica
for (i in 1:1000000){
  pvalor<-sigma.test(datos[,1], sigma=desviacion+i*precision, conf.level = 1-alpha, alternative = "two.side")$p.value
  if (pvalor<0.05){
    extremo_s2 = desviacion+(i-1)*precision
    break
  }
}
#Ahora lo mismo para el extremo izquierdo
for (i in 1:10000){
  pvalor<-sigma.test(datos[,1], sigma=desviacion-i*precision, conf.level = 1-alpha, alternative = "two.side")$p.value
  if (pvalor<0.05){
    extremo_s1 = desviacion-(i-1)*precision
    break
  }
}



#------------------------------------------------------------------------------------------------------------
#Vamos a comparar polígonos de frecuencias entre hombres y mujeres
plot(datos_sin_rep, relativa_acum, type = "l", xlim = c(96, 101), ylim = c(0, 1), ylab = "", xlab = "", main = "", col = "blue")
par(new=TRUE)


#---------------------------------------------------------------------------------------------------
#Vamos a hallar el intervalo de confianza para la diferencia de medias entre pulsaciones de hombres y mujeres
muestra1 <- read.csv("D:\\Yo\\Universidad\\Curso 20-21\\Estadística\\Trabajo\\pulsaciones_hombre.txt", header = FALSE, sep = "\t")
muestra2 <- read.csv("D:\\Yo\\Universidad\\Curso 20-21\\Estadística\\Trabajo\\pulsmujer.txt", header = FALSE, sep = "\t")
media1 = mean(muestra1[,1])
media2 = mean(muestra2[,1])
cuasides1 = (65/64)**(1/2)*sd(muestra1[,1])
cuasides2 = (65/64)**(1/2)*sd(muestra2[,1])
cuasi1 = cuasides1**2
cuasi2 = cuasides2**2
#vemos que el k más próximo a Sc es 119, nos fijamos en la t de student de 119 grados de libertad, lo demás lo hallamos con la calculadora


