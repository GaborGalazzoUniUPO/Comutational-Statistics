# realizzazione del Modello di Langevin per il caso
# unidimensionale omogeneo e PDF gaussiana.
# Si disegnino le traiettorie in un piano (t,x).
# Si calcolino le statistiche delle velocità ottenute e
# si confrontino con il valor medio (nullo) e
# la deviazione standard usata in input.
require(pracma)
par(mfrow = c(2,1))
par(mar = c(4, 4,0.5, 0))

h<-9
interval<-1

data <- read.table("dati/settembre/2018-09-10.dat", sep = " ", skip = 0)
w_speed <- data[,7]
i<-w_speed[(1:(60*interval)) + (h*60)]


u<-i-mean(i)
r <- acf(u,type = "correlation",plot = FALSE)$acf[,,1]
m<-line(log(r))
tL <- -1/m$coefficients[2]

dT<-0.1*tL
c0<-2
ε<-(2*sd(u)^2)/(c0*tL)

num_particelle<-10000

all_u1 <- matrix(NA,length(u),num_particelle)
all_x1 <- matrix(NA,length(u),num_particelle)

for(j in 1:num_particelle){

  norm<-rnorm(length(u),mean(u),sd(u))
  u1<-c(0)
  x1<-c(0)

  for(i in 1:(length(u)-1)){
    dW <- norm[i]*sqrt(dT) # incremento nel rumore bianco basato su normale da u
    #dU <- (-((u[i]/tL)*dT))+(sqrt(c0*ε)*dW) + u[i] # spostamento della pareticella
    dU<-(1-dT/tL)*u[i]+(sqrt(c0*ε)*dW)
    u1<-c(u1,u1[i]+dU) # nuova posizione particella
    x1<-c(x1,x1[i]+(u1[i+1]*dT)) # nuova velocità particella
  }

  all_u1[,j] <- u1
  all_x1[,j] <- x1
}
M<-max(abs(min(all_u1)),abs(max(all_u1)))
plot(NULL, NULL, xlab = "TEMPO", ylab="POSIZIONE", xlim = c(0,length(u)), ylim = c(-M,M))
colors<-rainbow(num_particelle)
for(i in 1:num_particelle){
  lines(all_u1[,i],col = colors[i])
}
M<-max(abs(min(all_x1)),abs(max(all_x1)))
plot(NULL, NULL,xlab = "TEMPO", ylab="VELOCITA'", xlim = c(0,length(u)), ylim = c(-M,M))
for(i in 1:num_particelle){
  lines(all_x1[,i],col = colors[i])
}

print("MEDIA all_x1")
mean(all_x1)
print("SD all_x1")
sd(all_x1)
mean(all_u1)
sd(all_u1)
print("MEDIA u")
mean(u)
print("SD u")
sd(u)