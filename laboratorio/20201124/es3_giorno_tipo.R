# Calcolo del giorno tipo

files <- list.files(path = "dati/settembre", pattern = "*.dat", full.names = TRUE, recursive = FALSE)

#Temperatura
#Umidità
#Velocità del vento

# 5  $UmiditaRelativa
# 6  $TemperaturaAria
# 7  $VelocitaVento

umidity <- matrix(ncol=30,nrow=1500)
temperature <-  matrix(ncol=30,nrow=1500)
speed <-  matrix(ncol=30,nrow=1500)

for (i in 1:length(files)) {
  data <- read.table(files[i], sep = " ", skip = 0)
  for (j in 1:length((data[,1]))) {
    umidity[j,i] <- (data[,5])[j]
    temperature[j,i] <- (data[,6])[j]
    speed[j,i] <- (data[,7])[j]
  }
}

d_umidity <- array(dim = 1440)
for(i in 1:1440){
  d_umidity[i] <- mean(umidity[i,])
}

d_temperature<- array(dim = 1440)
for(i in 1:1440){
  d_temperature[i] <- mean(temperature[i,])
}

d_speed<- array(dim = 1440)
for(i in 1:1440){
  d_speed[i] <- mean(speed[i,])
}

t <- paste0(trunc(1:1440/60),":",1:1440%%60)

par(mfrow = c(3, 1))
plot(d_umidity, type = "l", xlab = "Time", ylab = "Umidity",xaxt = 'n');
axis(1, at = 1:1440, labels = t)
plot(d_temperature, type = "l", xlab = "Time", ylab = "Temperature",xaxt = 'n');
axis(1, at = 1:1440, labels = t)
plot(d_speed, type = "l", xlab = "Time", ylab = "Speed",xaxt = 'n');
axis(1, at = 1:1440, labels = t)