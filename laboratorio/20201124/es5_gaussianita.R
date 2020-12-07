# Calcolo della PDF delle fluttuazioni di velocità,
# verifica gaussianità tramite  test chi-quadro su
# i seguenti intervalli temporali giorno,  mese,
# e suddivisi giorno-notte

data <- read.table("dati/settembre/2018-09-10.dat", sep = " ", skip = 0)
w_speed <- data[,7]

diurno <-  w_speed[(1:(60*12))+(8*60)]
notturno <-  na.omit(c(w_speed[(1:(60*8))],w_speed[(1:(60*4)+(20*60))]))

testChiNorm <- function (data) {
  len <- length(data)
  mean <- mean(data)
  deviation <- sd(data)
  space <- (max(data)-min(data)/5)
  O<-array(dim = 5)
  o[1] <- countif()
}