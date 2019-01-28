library(dplyr)
library(DescTools)
library(ggplot2)


#1) Leer la data

tabla<-read.csv("SSEP1-TC-Passive-WT2-a-3.CSV",header = T,skip = 2)

names(tabla)<-c("x","y")

plot(tabla$y~tabla$x,col="red",type="l")

#2) Suavizar la curva con un spline

tabla_spline<-smooth.spline(x = tabla$x, y = tabla$y, df = 20)


plot(tabla_spline$y~tabla_spline$x,col="red",type="l")

temp_data<-cbind(tabla_spline$x,tabla_spline$y)
temp_data<-as.data.frame(temp_data)
names(temp_data)<-c("x","y")

#Encontrar punto minimo de la curva y dividir la data

minimo_valle_y<-min(temp_data$y)
which(temp_data$y==minimo_valle_y)

mitad1<-temp_data[1:which(temp_data$y==minimo_valle_y),]
mitad2<-temp_data[(which(temp_data$y==minimo_valle_y)+1):nrow(temp_data),]

par(mfrow=c(1,2))

plot(mitad1$y~mitad1$x,col="red",type="l")
plot(mitad2$y~mitad2$x,col="blue",type="l")

#limpiar las dos mitades del excedente en caso de pile up

mitad1<-mitad1[which(mitad1$y==max(mitad1$y)):nrow(mitad1),]
mitad2<-mitad2[1:which(mitad2$y==max(mitad2$y)),]

par(mfrow=c(1,2))

plot(mitad1$y~mitad1$x,col="red",type="l")
plot(mitad2$y~mitad2$x,col="blue",type="l")

# Unir la data en un solo data frame y cortar en el plano

newdata<-rbind(mitad1,mitad2)

newdata<-filter(newdata,newdata$y<=0)

newdata$y<-newdata$y*(-1)


par(mfrow=c(1,1))
plot(newdata$y~newdata$x,col="red",type="l")

AUC(x=newdata$x,y=newdata$y,method = "trapezoid")


