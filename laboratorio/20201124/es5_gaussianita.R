# Calcolo della PDF delle fluttuazioni di velocità,
# verifica gaussianità tramite  test chi-quadro su
# i seguenti intervalli temporali giorno,  mese,
# e suddivisi giorno-notte



i_speed<-c()
for(i in 1:30){
  d <- if(i<10) paste0("0",i) else i
  data <- read.table(paste0("dati/settembre/2018-09-",d,".dat"), sep = " ", skip = 0)
  w_speed <- data[,7]
  i<-w_speed[(1:(60*1)) + (9*60)]
  i_speed <- c(i-mean(i),i_speed)
}

data <- read.table("dati/settembre/2018-09-12.dat", sep = " ", skip = 0)
w_speed <- data[,7]

diurno <-  w_speed[(1:(60*12))+(8*60)]
notturno <-  na.omit(c(w_speed[(1:(60*8))],w_speed[(1:(60*4)+(20*60))]))
par(mfrow = c(1,1))
par(mar = c(5, 5, 5, 5))

testChiNorm <- function (data,title,intervals = 5) {
  data <- na.omit(data)
  len <- length(data)
  mean <- mean(data)
  deviation <- sd(data)
  space <- abs((max(data)-min(data))/intervals)
  o<-array(dim = intervals)
  e<-array(dim = intervals)
  m <- min(data)
  for(i in 1:intervals){
    r <- data[m + (i-1)*space < data & data < m + (i)*space]
    o[i]<-length(r)
    p<-pnorm(m + (i)*space,mean,deviation)-pnorm(m+(i-1)*space,mean,deviation)
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
#chi2_d <- testChiNorm(diurno - mean(diurno),"Diurno",10)
#chi2_n <- testChiNorm(notturno - mean(notturno),"Notturno",10)
#i <- w_speed[(1:60) + (9*60)]
#chi2_9_10 <- testChiNorm(i-mean(i), "Dalle 9:00 alle 10:00",10)
#chi2_tot <-   testChiNorm(w_speed-mean(w_speed),"Giorno",10)
chi2_m_i <- testChiNorm(i_speed-mean(i_speed),"Mensile dalle 9:00 alle 10:00",5)