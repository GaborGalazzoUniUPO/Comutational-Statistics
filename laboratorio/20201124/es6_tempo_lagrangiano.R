
par(mfrow = c(2,1))

w_speed<-NULL
for(i in 1:3){
  i<-if(i<10) paste0("0",i) else i
  data <- read.table(paste0("dati/settembre/2018-09-",i,".dat"), sep = " ", skip = 0)
  w_speed <- c(w_speed,data[,7])
}
w_speed[is.na(w_speed)] <- 0


h<-9
interval<-1

i<-w_speed[(1:(60*interval)) + (h*60)]
u <- i - mean(i)
isZero<-FALSE
r<-array(dim=(30*interval))
for(τ in 0:(30*interval)){
  s<- NULL
  for(t in 1:(length(u)-τ)){
    s<-c(s,u[t]*u[t+τ])
  }
  r[τ]<-mean(s)
}
r<-r/r[1]
#r <- (r-min(r)+0.001)/(max(r)-min(r))
plot(1:(30*interval),r)
Tl_s<-sum(r)
lines(exp(-(1:length(r))/Tl_s),col="green", lty="dashed")
m<-line(log(r))
Tl_e_rl<- -1/m$coefficients[2]
Tl_e_2<-mean(-(1:(30*interval))/log(r))
g<- -(1:(30*interval))/log(r)
m<-mode(g)
lines(exp(-(1:length(r))/Tl_e_rl),col="blue", lty="dashed")
lines(exp(-(1:length(r))/Tl_e_2),col="red", lty="dashed")
#acf(u)


i<-w_speed[(1:(60*interval)) + (h*60)]
r <- acf(i,type = "covariance",plot = FALSE,lag.max = 30*interval)$acf[,,1]
r<-r/sd(u)^2
m<-line(log(r))
tL <- -1/m$coefficients[2]
plot(r)
lines(exp(-(1:length(r))/tL))