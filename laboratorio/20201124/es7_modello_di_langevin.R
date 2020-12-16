# realizzazione del Modello di Langevin per il caso
# unidimensionale omogeneo e PDF gaussiana.
# Si disegnino le traiettorie in un piano (t,x).
# Si calcolino le statistiche delle velocità ottenute e
# si confrontino con il valor medio (nullo) e
# la deviazione standard usata in input.

h<-9
interval<-1

data <- read.table("dati/settembre/2018-09-12.dat", sep = " ", skip = 0)
w_speed <- data[,7]
i<-w_speed[(1:(60*interval)) + (h*60)]
u<-i-mean(i)

r <- acf(i,type = "covariance",plot = FALSE)$acf[,,1]
r<-r/r[1]
m<-line(log(r))
tL <- -1/m$coefficients[2]
plot(r)
lines(exp(-(1:length(r))/tL))

dT<-0.1*tL
c<-2*sd(u)/tL