library(gplots)
library(R.utils)
library(fields)
library(grDevices)
library(plotrix)
#library(rgl)
#library(animation)
#library(misc3d)
#library(plot3D)
#library(lattice)
#carico le librerie
#
#setwd("../dati")
# 1 data
# 2  ora
# 3 "$RadiazioneIncidente 
# 4  $RadiazioneRiflessa 
# 5  $UmiditaRelativa 
# 6  $TemperaturaAria 
# 7  $VelocitaVento 
# 8  $Precipitazioni
# 8  $Pressione 
# 10  $TemperaturaRadiometroSup
# 11  $TemperaturaRadiometroInf 
# 12  $RadiazioneOLIncidente 
# 13  $RadiazioneOLRiflessa
par(mfrow=c(5,5))

for(i in 1:5){
  data18 <- read.table(paste0("dati/2018-10-0", i, ".dat"), sep=" ", skip=0)
  nn <- length(data18[, 3])
  hour <- 1:nn /60
  plot(hour,data18[,3]*0.28) #$RadiazioneIncidente
  plot(hour,data18[,6]) #$TemperaturaAria
  plot(hour,data18[,7])
  plot(hour,data18[,8])
  plot(hour,data18[,9])
}


data18=read.table("dati/2018-10-05.dat",sep=" ",skip=0)
nn=length(data18[,3])
hour=c(1:nn)/60
par(mfrow=c(2,1))
#plot(hour,data18[,3]*0.28-data18[,13])
plot(hour,data18[,3]*0.28) #$RadiazioneIncidente
#plot(hour,data18[,4])
#plot(hour,data18[,5])
plot(hour,data18[,6]) #$TemperaturaAria
#plot(hour,data18[,7])
#plot(hour,data18[,8])
#plot(hour,data18[,9])
#plot(hour,data18[,10])
#plot(hour,data18[,11])
#plot(hour,data18[,12])
#plot(hour,data18[,13])
#max(data18[,3]*0.28)

