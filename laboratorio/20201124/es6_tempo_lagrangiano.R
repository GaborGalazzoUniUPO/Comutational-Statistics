require(pracma)
par(mfrow = c(2,1))
par(mar = c(5, 5,0.5,0.5))

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
r<-array(dim=(30*interval))
for(τ in 0:(30*interval)){
  s<- NULL
  for(t in 1:(length(u)-τ)){
    s<-c(s,u[t]*u[t+τ])
  }
  r[τ]<-mean(s)
}
r<-r/r[1]
#r[r<=0]<-0.00001
plot(r)
m<-line(log(r))
Tl_e_rl<- -1/m$coefficients[2]
g<- -(1:(30*interval))/log(r)
lines(exp(-(1:length(r))/Tl_e_rl),col="blue", lty="dashed")
#acf(u)


i<-w_speed[(1:(60*interval)) + (h*60)]
u <- i - mean(i)
r1 <- acf(u,type = "correlation",plot = FALSE,lag.max = 30*interval)$acf[,,1]
r1<-r1/r1[1]
#r1[r1<=0]<-0.00001
m<-line(log(r1))
tL <- -1/m$coefficients[2]
plot(r1)
lines(exp(-(1:length(r1))/tL))