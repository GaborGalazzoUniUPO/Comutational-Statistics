#- Verifica grafica della PDF delle fluttuazioni di velocità per diversi periodi temporali.
par("mar")
par(mar=c(2,2,2,2))

step<-1
par(mfrow = c(12/step, 4))
data <- read.table("dati/settembre/2018-09-12.dat", sep = " ", skip = 0)
w_speed <- data[,7]
for(h in seq(0,24-step,by=step)){
wt_speed <- na.omit(w_speed[(1:(60*step))+(h*60)]) #velocità dalle 15 alle 16
f_wt_speed <- (wt_speed-mean(wt_speed)) #turbolenza = {x-η | x ∈ X }
  m <- max(abs(f_wt_speed))
hist(f_wt_speed, main = paste0("Histogram of: ",h,":00-",h+step,":00"), xlim = c(-m,m))
plot(density(f_wt_speed),main = paste0("PDF of: ",h,":00-",h+step,":00"), xlim = c(-m,m))
}

diurno <-  w_speed[(1:(60*12))+(8*60)]
notturno <-  na.omit(c(w_speed[(1:(60*8))],w_speed[(1:(60*4)+(20*60))]))
par(mfrow = c(2, 2))

t_diurno <- diurno-mean(diurno)
m_d <- max(abs(t_diurno))
t_notturno <- notturno-mean(notturno)
m_n <- max(abs(t_notturno))

hist(t_diurno, main = "Histogram of: 08:00-20:00",  xlim = c(-m_d,m_d))
plot(density(t_diurno),main = "PDF of: 08:00-20:00", xlim = c(-m_d,m_d))
hist(notturno-mean(notturno), main = "Histogram of: 20:00-08:00(+1)", xlim = c(-m_n,m_n))
plot(density(t_notturno),main = "Histogram of: 20:00-08:00(+1)", xlim = c(-m_n,m_n))
