# 4-forma-de-graficar-funciones-de-densidad-continua


#                         Tipo de Funciones:

#     Función histograma y densidad
#     Función plot
#     Función curve
#     Función plotDistr
#     Función ggplot


#............................Histrogra y linea de densidad
z<- rnorm(100,0,1)# valores aleatorios de la distribución normal
hist(z, freq = F)
lines(density(z), lwd=3, col="red")# curva de densidad de los valores de z



#.............................Función plot grafico .
xv<-seq(-3,3,0.1)
fd<-dnorm(xv)# función de densidad
fa <-pnorm(xv)# Función acumulada
dfn<- data.frame(xv,fd,fa)

plot(xv,fd, type = "l", main = "Función Densidad " ,col="red", ylim =c(0,0.5),
     xlab = "Valores de X", ylab = "Función densidad", lwd=3)
legend ("topright", c("media=0, sd=1"),
        lty =c(1 ) , lwd=c(2) , col="red")
abline(v=0)

#Grafica para la densidad
x<- seq(-4 ,8, 0.5 )
f1<- dnorm(x,0,1)#devuel la función de densidad
f2 <- dnorm(x,2,1.5)
f3 <- dnorm(x,4,2)

plot(x,f1, type = "l", main = "Función Densida Normal" ,col=4 ,lwd =3,
     xlab = "Valores de X", ylab = "Función densidad", xlim=c(-4,8), ylim = c(0,0.4))
lines (x,f2 ,lty =1, lwd =3, col =2)
lines (x,f3 ,lty =1, lwd =3, col =7)
legend ("topright", c("mu=0,sd=1 ", "mu=2, sd=1.5", "mu=4,sd=2"),
        lty =c(1 ,1,1) , lwd=c(2,2,2) , col=c(4,2,7))

#Grafica de la Funcion acumulada
f1<- pnorm(x,0,1)# Nos devuele la función acumulada
f2 <- pnorm(x,2,1.5)
f3 <- pnorm(x,4,2)

plot(x,f1, type = "l", main = "Función Acumulada Normal" ,col=4 ,lwd =3,
     xlab = "Valores de X", ylab = "Función acumulada")
lines (x,f2 ,lty =1, lwd =3, col =2)
lines (x,f3 ,lty =1, lwd =3, col =7)
legend (2.8,0.23, c("mu=0,sd=1 ", "mu=2, sd=1.5", "mu=4,sd=2"),
        lty =c(1 ,1) , lwd=c(2,2) , col=c(4,2))



#.............................Funcion Curve
#funcion densidad

curve(dnorm(x,0,1),-4,8,ylab="y", lwd=3)#función de densidad
curve(dnorm(x,2,1),add=TRUE,col="blue", lwd=3)
curve(dnorm(x,4,1),add=TRUE,col="red", lwd=3)
legend("topright",c("N(0,1)","N(2,1)","N(4,1)"),
       col=c("black","blue","red"),lty=c(1,1,1))

#Funcion Curve, funcion acumulada
curve(pnorm(x,0,1),-4,8,ylab="y", lwd=3, main="Distribución Acumulada")
curve(pnorm(x,2,1),add=TRUE,col="blue", lwd=3)
curve(pnorm(x,4,1),add=TRUE,col="red", lwd=3)
legend(4.6,0.25,c("N(0,1)","N(2,1)","N(4,1)"),
       col=c("black","blue","red"),lty=c(1,1,1))

#Función curve con sombreado 
regionX=seq(150,168,0.01)            # Intervalo a sombrear
xP <- c(150,regionX,168)             # Base de los polígonos que crean el efecto "sombra"
yP <- c(0,dnorm(regionX,170,12),0)   # Altura de los polígonos sombreados
curve(dnorm(x,170,12),xlim=c(130,210),yaxs="i",ylim=c(0,0.035),ylab="f(x)",
      main='Densidad N(170,12)') 
polygon(xP,yP,col="orange1")
box()

#Función curve 
X=rnorm(10000, 170, 12)# nos devuelve valores aleatorios de la distribución normal
hist(X,freq=FALSE,col="lightsalmon",main="Histograma",sub="Datos simulados de una N(170,12)")
curve(dnorm(x,170,12),xlim=c(110,220),col="blue",lwd=2,add=TRUE)
legend("topright",c("N(170,12)"), col=c("blue"),lty=c(1,1,1))
      
#....................... Funcion plotDistr

#install.packages(" RcmdrMisc")
library(RcmdrMisc)
x <- seq(-4, 4, length=100)
plotDistr(x, dnorm(x), xlab="Z", ylab="p(z)", lwd=3,
          main="Distribución Normal de Densidad")



plotDistr(x, dnorm(x), xlab="Z", ylab="p(z)",lwd=3,
          main="Función de Densidad \n Distribución Normal",
          region=list(c(1.96, Inf), c(-Inf, -1.96)), col=c("red", "blue"))
abline(v=0)

plotDistr(x, dnorm(x), xlab="Z", ylab="p(z)",lwd=3,
          main="Distribución Normal de Densidad",
          region=list(c(qnorm(0), qnorm(.025)), c(qnorm(.975), qnorm(1)))) # same 
abline(v=0)




#.............................Función ggplot.
library(ggplot2)
library(dplyr)
library(gganimate)

#función densidad
dfn %>% ggplot(aes(xv,fd) )+ geom_line(size=1.5, colour="Green")+
  theme_light()+ ggtitle("Función Densidad \n Media=0, sd=1 ")+
  ylab("Función densida")+xlab("valores de x")


#Función ggplot. Funcion acumulada

dfn %>% ggplot(aes(xv,fa))+ geom_line(size=1.5, col=10)+
  theme_light()+ ggtitle("Función Acumulda \n Media=0, sd=1 ")+
  ylab("Función densidad")+xlab("valores de x")


library(gganimate)
#Función ggplot. función densidad animada
dfn %>% ggplot(aes(xv,fd) )+ geom_line(size=1.5, col=30)+
  theme_light()+ ggtitle("Función Densidad \n Media=0, sd=1 ")+
  ylab("Función densida")+xlab("valores de x")+
transition_reveal(xv)

#Función ggplot. Función Acumulada animada

dfn %>% ggplot(aes(xv,fa))+ geom_line(size=1.5, colour="Red")+
  theme_light()+ ggtitle("Función Densidad \n Media=0, sd=1 ")+
  ylab("Función densidad")+xlab("valores de x")+
  transition_reveal(xv)
