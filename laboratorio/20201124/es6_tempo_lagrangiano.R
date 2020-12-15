
w_speed<-NULL
for(i in 1:30){
  i<-if(i<10) paste0("0",i) else i
  data <- read.table(paste0("dati/settembre/2018-09-",i,".dat"), sep = " ", skip = 0)
  w_speed <- c(w_speed,data[,7])
}
w_speed[is.na(w_speed)] <- 0


h<-8
interval<-70

i<-w_speed[(1:(60*interval)) + (h*60)]
u <- i - mean(i)

r<-array(dim=(30*interval))
for(τ in 1:(30*interval)){
  s<- NULL
  for(t in 1:(length(u)-τ)){
    s<-c(s,u[t]*u[t+τ])
  }
  r[τ]<-max(mean(s),0.001)/(sd(u)^2)
}
#r <- (r-min(r)+0.001)/(max(r)-min(r))
plot(1:(30*interval),r)
Tl_s<-sum(r)
lines(1:(30*interval),exp(-(1:(30*interval))/Tl_s),col="green", lty="dashed")
Tl_e<-mean(-(1:(30*interval))/log(r))
lines(1:(30*interval),exp(-(1:(30*interval))/Tl_e),col="blue", lty="dashed")
#acf(u)