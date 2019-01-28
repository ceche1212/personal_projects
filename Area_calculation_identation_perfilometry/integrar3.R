library(dplyr)
library(DescTools)
library(ggplot2)


#1) Leer la data

tabla<-read.csv("SSEP3B-TC-Passive-WT1-a-2.CSV",header = T,skip = 2)

names(tabla)<-c("x","y")

plot(tabla$y~tabla$x,col="red",type="l")

#2) Suavizar la curva con un spline

tabla_spline<-smooth.spline(x = tabla$x, y = tabla$y, df = 20)


plot(tabla_spline$y~tabla_spline$x,col="red",type="l")

temp_data<-cbind(tabla_spline$x,tabla_spline$y)
temp_data<-as.data.frame(temp_data)
names(temp_data)<-c("x","y")

#3) Dividir la data a la mitad en el minimo del valle

#Encontrar punto minimo de la curva y dividir la data

minimo_valle_y<-min(temp_data$y)
which(temp_data$y==minimo_valle_y)

mitad1<-temp_data[1:which(temp_data$y==minimo_valle_y),]
mitad2<-temp_data[(which(temp_data$y==minimo_valle_y)+1):nrow(temp_data),]

par(mfrow=c(1,2))

plot(mitad1$y~mitad1$x,col="red",type="l")
plot(mitad2$y~mitad2$x,col="blue",type="l")


#limpieza mitad1 solo valores por debajo de 0 o plano elejido

plano<-min(mitad1[mitad1$y>0,])

mitad1<-filter(mitad1,mitad1$y<=plano)

par(mfrow=c(1,2))

plot(mitad1$y~mitad1$x,col="red",type="l")
abline(v=mean(mitad1$x))
plot(mitad2$y~mitad2$x,col="blue",type="l")



# calcular distancia entre puntos

x=mitad1$x

distancia_x<-data.frame()
distancia<-c(NA)

for (i in seq(x)) {
        
        distancia[i]<-x[i+1]-x[i]
        distancia_x<-rbind(distancia_x,distancia[i])
        
}

names(distancia_x)<-c("distancia_x")

mitad1$distancia_x<-distancia_x[[1]]


mitad1<-filter(mitad1,mitad1$distancia_x<=median(mitad1$distancia_x,na.rm = T),mitad1$x>mean(mitad1$x))
par(mfrow=c(1,2))

plot(mitad1$y~mitad1$x,col="red",type="l")
plot(mitad2$y~mitad2$x,col="blue",type="l")


#limpieza mitad2 solo valores por debajo de 0 o plano elejido

mitad2<-filter(mitad2,mitad2$y<=plano)

# calcular distancia entre puntos

x2=mitad2$x

distancia_x2<-data.frame()
distancia2<-c(NA)

for (i in seq(x2)) {
        
        distancia2[i]<-x2[i+1]-x2[i]
        distancia_x2<-rbind(distancia_x2,distancia2[i])
        
}

names(distancia_x2)<-c("distancia_x")

mitad2$distancia_x<-distancia_x2[[1]]


mitad2<-filter(mitad2,mitad2$distancia_x<=median(mitad2$distancia_x,na.rm = T),mitad2$x<mean(mitad2$x))
par(mfrow=c(1,2))

plot(mitad1$y~mitad1$x,col="red",type="l")
plot(mitad2$y~mitad2$x,col="blue",type="l")
abline(v=mean(mitad2$x))

#unir la data

newdata<-rbind(mitad1,mitad2)

newdata<-filter(newdata,newdata$y<=0)

newdata$y<-newdata$y*(-1)


par(mfrow=c(1,1))
plot(newdata$y~newdata$x,col="red",type="l")

AUC(x=newdata$x,y=newdata$y,method = "trapezoid")

tabla2<-filter(tabla,tabla$x>=newdata$x[1],tabla$x<=newdata$x[nrow(newdata)])
tabla2$y<-tabla2$y*(-1)

par(mfrow=c(1,2))
plot(newdata$y~newdata$x,col="red",type="l")
plot(tabla2$y~tabla2$x,col="blue",type="l")


AUC(x=tabla2$x,y=tabla2$y,method = "trapezoid")

par(mfrow=c(2,2))

plot(newdata$y~newdata$x,col="red",type="l")
plot(tabla2$y~tabla2$x,col="blue",type="l")

barplot(c(AUC(x=newdata$x,y=newdata$y,method = "trapezoid"),
          AUC(x=tabla2$x,y=tabla2$y,method = "trapezoid")),names.arg = c("spline","real_data"),col = c("red","blue"))


(AUC(x=newdata$x,y=newdata$y,method = "trapezoid")-AUC(x=tabla2$x,y=tabla2$y,method = "trapezoid"))/AUC(x=tabla2$x,y=tabla2$y,method = "trapezoid")



