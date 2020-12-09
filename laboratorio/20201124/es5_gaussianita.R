# Calcolo della PDF delle fluttuazioni di velocità,
# verifica gaussianità tramite  test chi-quadro su
# i seguenti intervalli temporali giorno,  mese,
# e suddivisi giorno-notte

data <- read.table("dati/settembre/2018-09-12.dat", sep = " ", skip = 0)
w_speed <- data[,7]

diurno <-  w_speed[(1:(60*12))+(8*60)]
notturno <-  na.omit(c(w_speed[(1:(60*8))],w_speed[(1:(60*4)+(20*60))]))
par(mfrow = c(1,1))
par(mar = c(5, 5, 5, 5))

testChiNorm <- function (data,title,intervals = 5) {
  len <- length(data)
  mean <- mean(data)
  deviation <- sd(data)
  space <- (max(data)-min(data))/intervals
  o<-array(dim = intervals)
  e<-array(dim = intervals)
  for(i in 1:intervals){
    r <- data[(i-1)*space < data & data < (i)*space]
    o[i]<-length(r)
    p<-pnorm((i)*space,mean,deviation)-pnorm((i-1)*space,mean,deviation)
    e[i]<-p*len
  }
  res <- ((o-e)^2)/e
  chi2<- sum(res)


  plot(density(data), pch=19, main = paste0("Intervals: ",intervals," - Chi2: ",chi2,"\n--------\n",title),ylim=c(0,2))

  x <- seq(min(c(min(data),-max(data))),max(c(max(data),-min(data))) , length=100)
  y <- dnorm(x,mean,deviation)
  lines(x,y,col="green", lty="dashed")
  #plot(x,y,main = paste0("norm(",mean,",",deviation,")"),type = "l")
  legend("topleft", legend=c("Data PDF", "Norlam"),
         col=c("black", "green"), lty=1:2, cex=0.8)

  chi2
}

chi2_d <- testChiNorm(diurno,"Diurno",10)
chi2_n <- testChiNorm(notturno,"Notturno",10)

chi2_9_10 <- testChiNorm(w_speed[(1:60) + (9*60)], "Datte 9:00 alle 10:00",10)

chi2_tot <-   testChiNorm(w_speed,"Giorno",10)