# Title     : TODO
# Objective : TODO
# Created by: Gaborando
# Created on: 02/12/2020
par(mfrow = c(1, 2))
files <- list.files(path = "dati/settembre", pattern = "*.dat", full.names = TRUE, recursive = FALSE)
speed <- matrix(ncol = 30, nrow = 1500)
for (i in 1:length(files)) {
  data <- read.table(files[i], sep = " ", skip = 0)
  for (j in 1:length((data[, 1]))) {
    speed[j, i] <- (data[, 7])[j]
  }
}
step<-2
for (h in seq(0,24-step,by=step)) {
  all <- array()
  for (i in 1:30) {
    all <- na.omit(c(all, speed[, i][(1:(60*step))+(h*60)]))
  }
  t <- all - mean(all)
  hist(t,   main = paste0("Histogram of: ",h,":00-",h+step,":00"), xlim = c(-max(abs(t)),max(abs(t))))
  plot(density(t) ,main = paste0("Density of: ",h,":00-",h+step,":00"),   xlim =  c(-max(abs(t)),max(abs(t))))
}
