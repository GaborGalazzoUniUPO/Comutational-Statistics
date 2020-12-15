
# Title     : TODO
# Objective : TODO
# Created by: Gaborando
# Created on: 25/11/2020

moment <- function(x, order = 1, center = FALSE)
{
  if (center) {
    x <- x - mean(x)
  }
  sum(x^order) / length(x)
}

rel_moment <- function(vec, ord , center){
  sum <- 0;
  for(k in 0:ord){
    sum <- sum + (choose(ord,k) * moment(vec,k,!(center)) * (ifelse(center, -1 , 1 )*mean(vec)^(ord-k)))
  }
  sum
}



data <- read.table("dati/settembre/2018-09-01.dat", sep = " ", skip = 0)
m1 <- moment(data[, 6], order = 2, center = TRUE)
m2 <- rel_moment(data[, 6],2, TRUE)
d <- m1 - m2  # errore

m3 <- moment(data[, 6], order = 20, center = TRUE)
m4 <- rel_moment(data[, 6],20, TRUE)
d2 <- m3 - m4  # errore
