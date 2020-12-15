# Title     : TODO
# Objective : TODO
# Created by: Gaborando
# Created on: 24/11/2020

moment <- function(x, order = 1, center = FALSE)
{
  if (center) {
    x <- x - mean(x)
  }
  sum(x^order) / length(x)
}

findGaussian <- function(x){
  moment(x,4,TRUE) / (moment(x,2,TRUE) ^ 2)
}

par(mfrow = c(2, 4))
m1v6 <- array(dim = 5)
m1cv6 <- array(dim = 5)
m2v6 <- array(dim = 5)
m2cv6 <- array(dim = 5)
m1v7 <- array(dim = 5)
m1cv7 <- array(dim = 5)
m2v7 <- array(dim = 5)
m2cv7 <- array(dim = 5)
m3cv6 <- array(dim = 5)
m3cv7 <- array(dim = 5)
gaussian6 <- array(dim=5)
gaussian7 <- array(dim=5)

for (i in 1:30) {
  d<-if(i<10) paste0("0",i) else i
  data <- read.table(paste0("dati/settembre/2018-09-", d, ".dat"), sep = " ", skip = 0)
  m1v6[i] <- moment(data[, 6], order = 1, center = FALSE)
  m1cv6[i] <- moment(data[, 6], order = 1, center = TRUE)
  m2v6[i] <- moment(data[, 6], order = 2, center = FALSE)
  m2cv6[i] <- moment(data[, 6], order = 2, center = TRUE)
  m1v7[i] <- moment(data[, 7], order = 1, center = FALSE)
  m1cv7[i] <- moment(data[, 7], order = 1, center = TRUE)
  m2v7[i] <- moment(data[, 7], order = 2, center = FALSE)
  m2cv7[i] <- moment(data[, 7], order = 2, center = TRUE)
  m3cv7[i] <- moment(data[,7], order = 3, center = TRUE)
  m3cv6[i] <- moment(data[,6], order = 3, center = TRUE)
  gaussian6[i] <- findGaussian(data[,6])
  gaussian7[i] <- findGaussian(data[,7])
}
plot(m1v6, type = "b", xlim = c(1, 30));
#plot(m1cv6, type="b", xlim = c(1,30));
#plot(m2v6, type="b", xlim = c(1,30));
plot(m2cv6, type = "b", xlim = c(1, 30));
plot(m3cv6, type = "b", xlim = c(1, 30));
plot(gaussian6, type = "b", xlim = c(1, 30), main = mean(gaussian6));


plot(m1v7, type = "b", xlim = c(1, 30));
#plot(m1cv7, type="b", xlim = c(1,30));
#plot(m2v7, type="b", xlim = c(1,30));
plot(m2cv7, type = "b", xlim = c(1, 30));
plot(m3cv7, type = "b", xlim = c(1, 30));
plot(gaussian7, type = "b", xlim = c(1, 30), main = mean(gaussian7));

