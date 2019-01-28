library(dplyr)
library(DescTools)


tabla<-read.csv("SSEP1-TC-Passive-WT2-a-3.CSV",header = T,skip = 2)

names(tabla)<-c("x","y")

summary(tabla$x)
summary(tabla$y)

#spline smooth

baba<-smooth.spline(x = tabla$x, y = tabla$y, df = 20)

tabla2<-cbind(baba$x,baba$y)
tabla2<-as.data.frame(tabla2)
names(tabla2)<-c("x","y")

tabla2$x<-as.data.frame(baba$x)
tabla2$y<-baba$y
y<-tabla2$y

tabla3<-tabla2[tabla2$y<0,]
x=tabla3$x

distanciax<-data.frame()
papa<-c(NA)

for (i in seq(x)) {
        
        papa[i]<-x[i+1]-x[i]
        distanciax<-rbind(distanciax,papa[i])
        
}

names(distanciax)<-c("distanciax")

tabla3$distanciax<-distanciax[[1]]
medianadistanciax<-median(tabla3$distanciax,na.rm = T)

tabla3$cuttoff<-ifelse(tabla3$distanciax>10*medianadistanciax,"Yes","No")
which(tabla3$cuttoff=="Yes")


#3)Calcular pendientes

pendientes_x<-data.frame()
pend<-c(NA)
x=temp_data$x
y=temp_data$y


for (i in seq(x)) {
        
        pend[i]<-(y[i+1]-y[i])/(x[i+1]-x[i])
        pendientes_x<-rbind(pendientes_x,pend[i])
        
}

names(pendientes_x)<-c("slope")

temp_data$pendientes_x<-pendientes_x[[1]]

##Encontrar punto minimo de la curva y dividir la data

minimo_valle_y<-min(temp_data$y)
which(temp_data$y==minimo_valle_y)

mitad1<-temp_data[1:which(temp_data$y==minimo_valle_y),]
mitad2<-temp_data[(which(temp_data$y==minimo_valle_y)+1):nrow(temp_data),]

par(mfrow=c(1,2))

plot(mitad1$y~mitad1$x,col="red",type="l")
plot(mitad2$y~mitad2$x,col="blue",type="l")



