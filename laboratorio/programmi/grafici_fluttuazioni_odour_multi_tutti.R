library(gplots)
library(R.utils)
library(fields)
library(grDevices)
library(plotrix)
#library(rgl)
#library(animation)
#library(misc3d)
#library(plot3D)
#library(lattice)
#carico le librerie
#
# ATTENZIONE: questo programma è valido per la sorgente di un
# dato diametro. E' solamente necessario cambiare il nome de= FALSE)i
# file caricati per ottenere i grafici degli altri diametri
#apro il file delle fluttuazioni
# setwd("/home/ferrero/Dropbox/odour")

#################INIZIO#################      B
dati_flut = as.matrix(read.table("factor2_eq/campo_flut_z=8.500m_ds=1.240_b.txt",sep=""))

#leggo il campo di concentrazione media
fconc<-file("factor2_22/concentrazione_z=8.500m_ds=1.240_b.txt","r")
#
#n° punti grigliato
lx = as.numeric(readLines(fconc,n=1))
ly = as.numeric(readLines(fconc,n=1))
lz = as.numeric(readLines(fconc,n=1))
#
#coordinate grigliato
cx = scan(fconc,n=lx,sep="")
cy = scan(fconc,n=ly,sep="")
cz = scan(fconc,n=lz,sep="")
#

cmedia = as.matrix(read.table(fconc,sep=""))
#
close(fconc)
#
# array delle fluttuazioni
fluttuazioni = array(0,dim=c(lx,ly,lz))
#
# array della cmedia
conc_media = array(0,dim=c(lx,ly,lz))
intensity = array(0,dim=c(lx,ly,lz))

#
#salva cmedia e fluttuazioni in 2 array 3D
for(j in 1:lz){
#	print(j)
	for(i in 1:lx){	
		fluttuazioni[i,,j] = dati_flut[(lx*(j-1)+i),]
		conc_media[i,,j] = cmedia[(lx*(j-1)+i),]
	}
}
#q=121.7 g/h   Mp=q/Np*  DT          0.0000000000003   Np=10  DT=0.1
# EXP B-H =121.7 g/h  I,J,K,L,M,N,O=252.5 
massa=121.7/3600/10*0.1
rinorm=0.0000000000003/massa/1.e+6
conc_media1=conc_media/rinorm
fluttuazioni1=sqrt(fluttuazioni)/rinorm
for(i in 1:lx){
		for(s in 1:ly){
			for(m in 1:lz){
		if(conc_media1[i,s,m]>0){intensity[i,s,m]=fluttuazioni1[i,s,m]/conc_media1[i,s,m]}
		else{intensity[i,s,m]=0	}
			}
		}
		#
}
#intensity=fluttuazioni/conc_media/conc_media
#EXP B
#xmis=c(60,72,84,96,108,120,147,160,172,185,197,209)
#ymis=c(158,142,127,111,95,80,252,236,220,205,189,174)
#EXP B, C
xmis=c(108,185)
ymis=c(95,205)
#ROTAZIONE
# EXP B    U=3.9
ang=-(270-209)/180*3.14

XT1P1=xmis*cos(ang)-ymis*sin(ang)
YT1P1=xmis*sin(ang)+ymis*cos(ang)
cy1=cy+250
#quartz()
#contour(cx,cy1,intensity[,,1],main="Intensities")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,fluttuazioni1[,,1],main="Fluctuations")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,conc_media1[,,1],main="Mean concentrations")
#points(XT1P1,YT1P1+250)
loc=array(c(XT1P1,YT1P1+250),dim = c(2,2))
aa=conc_media1[,,1]
maxaa=max(aa)
obj=list(x=cx,y=cy1,z=aa)
lookcm=interp.surface(obj,loc)
#plot(YT1P1[1:6],lookcm[1:6])
##points(YT1P1[7:12],lookcm[7:12])
#image.plot( as.surface( loc, look))
bb=fluttuazioni1[,,1]
obj=list(x=cx,y=cy1,z=bb)
lookfl=interp.surface(obj,loc)
lookint=lookfl/lookcm
stat1=matrix(NA,nrow=4,ncol=2)
stat0=matrix(NA,nrow=2,ncol=4)
stat0[,]=c(sqrt(xmis**2+ymis**2),lookcm,lookfl,lookint)
stat1b=t(stat0)
#stat1[,]=c(xmis,ymis,lookcm,lookfl,lookint)
#format(stat1,digits = 2)
colnames(stat1b) = c("b      T1P1"," T1P2")
rownames(stat1b) = c("DIST ","CMEAN","  CSD","  I  ")
# stat1b = format(stat1b,digits = 2,scientific=F)
#write.table(stat1,file="dat_stat.csv",append = FALSE, quote = FALSE,sep="\t")
print("B")
print(maxaa)
#write.table(stat1,append = FALSE, quote = FALSE)
##################FINE###############
#################INIZIO################# C
dati_flut = as.matrix(read.table("factor2_eq/campo_flut_z=8.500m_ds=1.240_c.txt",sep=""))

#leggo il campo di concentrazione media
fconc<-file("factor2_22/concentrazione_z=8.500m_ds=1.240_c.txt","r")
#
#n° punti grigliato
lx = as.numeric(readLines(fconc,n=1))
ly = as.numeric(readLines(fconc,n=1))
lz = as.numeric(readLines(fconc,n=1))
#
#coordinate grigliato
cx = scan(fconc,n=lx,sep="")
cy = scan(fconc,n=ly,sep="")
cz = scan(fconc,n=lz,sep="")
#

cmedia = as.matrix(read.table(fconc,sep=""))
#
close(fconc)
#
# array delle fluttuazioni
fluttuazioni = array(0,dim=c(lx,ly,lz))
#
# array della cmedia
conc_media = array(0,dim=c(lx,ly,lz))
intensity = array(0,dim=c(lx,ly,lz))

#
#salva cmedia e fluttuazioni in 2 array 3D
for(j in 1:lz){
#	print(j)
	for(i in 1:lx){	
		fluttuazioni[i,,j] = dati_flut[(lx*(j-1)+i),]
		conc_media[i,,j] = cmedia[(lx*(j-1)+i),]
	}
}
#q=121.7 g/h   Mp=q/Np*  DT          0.0000000000003   Np=10  DT=0.1
# EXP B-H =121.7 g/h  I,J,K,L,M,N,O=252.5 
massa=121.7/3600/10*0.1
rinorm=0.0000000000003/massa/1.e+6
conc_media1=conc_media/rinorm
fluttuazioni1=sqrt(fluttuazioni)/rinorm
for(i in 1:lx){
		for(s in 1:ly){
			for(m in 1:lz){
		if(conc_media1[i,s,m]>0){intensity[i,s,m]=fluttuazioni1[i,s,m]/conc_media1[i,s,m]}
		else{intensity[i,s,m]=0	}
			}
		}
		#
}
#intensity=fluttuazioni/conc_media/conc_media
#EXP B
#xmis=c(60,72,84,96,108,120,147,160,172,185,197,209)
#ymis=c(158,142,127,111,95,80,252,236,220,205,189,174)
#EXP B, C
xmis=c(108,185)
ymis=c(95,205)
#ROTAZIONE

# EXP C    U=4.6
ang=-(270-216)/180*3.14

XT1P1=xmis*cos(ang)-ymis*sin(ang)
YT1P1=xmis*sin(ang)+ymis*cos(ang)
cy1=cy+250
#quartz()
#contour(cx,cy1,intensity[,,2],main="Intensities")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,fluttuazioni1[,,2],main="Fluctuations")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,conc_media1[,,2],main="Mean concentrations")
#points(XT1P1,YT1P1+250)
loc=array(c(XT1P1,YT1P1+250),dim = c(2,2))
aa=conc_media1[,,1]
maxaa=max(aa)
obj=list(x=cx,y=cy1,z=aa)
lookcm=interp.surface(obj,loc)
#plot(YT1P1[1:6],lookcm[1:6])
##points(YT1P1[7:12],lookcm[7:12])
#image.plot( as.surface( loc, look))
bb=fluttuazioni1[,,1]
obj=list(x=cx,y=cy1,z=bb)
lookfl=interp.surface(obj,loc)
lookint=lookfl/lookcm
stat1=matrix(NA,nrow=4,ncol=2)
stat0=matrix(NA,nrow=2,ncol=4)
stat0[,]=c(sqrt(xmis**2+ymis**2),lookcm,lookfl,lookint)
stat1c=t(stat0)
#stat1[,]=c(xmis,ymis,lookcm,lookfl,lookint)
#format(stat1,digits = 2)
colnames(stat1c) = c("c      T1P1"," T1P2")
rownames(stat1c) = c("DIST ","CMEAN","  CSD","  I  ")
# stat1c = format(stat1c,digits = 2,scientific=F)
#write.table(stat1,file="dat_stat.csv",append = FALSE, quote = FALSE,sep="\t")
print("C")
print(maxaa)
#write.table(stat1,append = FALSE, quote = FALSE)
##################FINE###############
#################INIZIO################# D
dati_flut = as.matrix(read.table("factor2_eq/campo_flut_z=8.500m_ds=1.240_d.txt",sep=""))

#leggo il campo di concentrazione media
fconc<-file("factor2_22/concentrazione_z=8.500m_ds=1.240_d.txt","r")
#
#n° punti grigliato
lx = as.numeric(readLines(fconc,n=1))
ly = as.numeric(readLines(fconc,n=1))
lz = as.numeric(readLines(fconc,n=1))
#
#coordinate grigliato
cx = scan(fconc,n=lx,sep="")
cy = scan(fconc,n=ly,sep="")
cz = scan(fconc,n=lz,sep="")
#

cmedia = as.matrix(read.table(fconc,sep=""))
#
close(fconc)
#
# array delle fluttuazioni
fluttuazioni = array(0,dim=c(lx,ly,lz))
#
# array della cmedia
conc_media = array(0,dim=c(lx,ly,lz))
intensity = array(0,dim=c(lx,ly,lz))

#
#salva cmedia e fluttuazioni in 2 array 3D
for(j in 1:lz){
#	print(j)
	for(i in 1:lx){	
		fluttuazioni[i,,j] = dati_flut[(lx*(j-1)+i),]
		conc_media[i,,j] = cmedia[(lx*(j-1)+i),]
	}
}
#q=121.7 g/h   Mp=q/Np*  DT          0.0000000000003   Np=10  DT=0.1
# EXP B-H =121.7 g/h  I,J,K,L,M,N,O=252.5 
massa=121.7/3600/10*0.1
rinorm=0.0000000000003/massa/1.e+6
conc_media1=conc_media/rinorm
fluttuazioni1=sqrt(fluttuazioni)/rinorm
for(i in 1:lx){
		for(s in 1:ly){
			for(m in 1:lz){
		if(conc_media1[i,s,m]>0){intensity[i,s,m]=fluttuazioni1[i,s,m]/conc_media1[i,s,m]}
		else{intensity[i,s,m]=0	}
			}
		}
		#
}
#intensity=fluttuazioni/conc_media/conc_media
#EXP B
#xmis=c(60,72,84,96,108,120,147,160,172,185,197,209)
#ymis=c(158,142,127,111,95,80,252,236,220,205,189,174)
#ROTAZIONE
# EXP D   U=2.5
xmis=c(84,172)
ymis=c(127,220)
ang=-(270-230)/180*3.14

XT1P1=xmis*cos(ang)-ymis*sin(ang)
YT1P1=xmis*sin(ang)+ymis*cos(ang)
cy1=cy+250
#quartz()
#contour(cx,cy1,intensity[,,2],main="Intensities")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,fluttuazioni1[,,2],main="Fluctuations")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,conc_media1[,,2],main="Mean concentrations")
#points(XT1P1,YT1P1+250)
loc=array(c(XT1P1,YT1P1+250),dim = c(2,2))
aa=conc_media1[,,1]
maxaa=max(aa)
obj=list(x=cx,y=cy1,z=aa)
lookcm=interp.surface(obj,loc)
#plot(YT1P1[1:6],lookcm[1:6])
##points(YT1P1[7:12],lookcm[7:12])
#image.plot( as.surface( loc, look))
bb=fluttuazioni1[,,1]
obj=list(x=cx,y=cy1,z=bb)
lookfl=interp.surface(obj,loc)
lookint=lookfl/lookcm
stat1=matrix(NA,nrow=4,ncol=2)
stat0=matrix(NA,nrow=2,ncol=4)
stat0[,]=c(sqrt(xmis**2+ymis**2),lookcm,lookfl,lookint)
stat1d=t(stat0)
#stat1[,]=c(xmis,ymis,lookcm,lookfl,lookint)
#format(stat1,digits = 2)
colnames(stat1d) = c("d      T1P1"," T1P2")
rownames(stat1d) = c("DIST ","CMEAN","  CSD","  I  ")
# stat1d = format(stat1d,digits = 2,scientific=F)
#write.table(stat1,file="dat_stat.csv",append = FALSE, quote = FALSE,sep="\t")
print("D")
print(maxaa)
#write.table(stat1,append = FALSE, quote = FALSE)
##################FINE###############
#################INIZIO################# E
dati_flut = as.matrix(read.table("factor2_eq/campo_flut_z=8.500m_ds=1.240_e.txt",sep=""))

#leggo il campo di concentrazione media
fconc<-file("factor2_22/concentrazione_z=8.500m_ds=1.240_e.txt","r")
#
#n° punti grigliato
lx = as.numeric(readLines(fconc,n=1))
ly = as.numeric(readLines(fconc,n=1))
lz = as.numeric(readLines(fconc,n=1))
#
#coordinate grigliato
cx = scan(fconc,n=lx,sep="")
cy = scan(fconc,n=ly,sep="")
cz = scan(fconc,n=lz,sep="")
#

cmedia = as.matrix(read.table(fconc,sep=""))
#
close(fconc)
#
# array delle fluttuazioni
fluttuazioni = array(0,dim=c(lx,ly,lz))
#
# array della cmedia
conc_media = array(0,dim=c(lx,ly,lz))
intensity = array(0,dim=c(lx,ly,lz))

#
#salva cmedia e fluttuazioni in 2 array 3D
for(j in 1:lz){
#	print(j)
	for(i in 1:lx){	
		fluttuazioni[i,,j] = dati_flut[(lx*(j-1)+i),]
		conc_media[i,,j] = cmedia[(lx*(j-1)+i),]
	}
}
#q=121.7 g/h   Mp=q/Np*  DT          0.0000000000003   Np=10  DT=0.1
# EXP B-H =121.7 g/h  I,J,K,L,M,N,O=252.5 
massa=121.7/3600/10*0.1
rinorm=0.0000000000003/massa/1.e+6
conc_media1=conc_media/rinorm
fluttuazioni1=sqrt(fluttuazioni)/rinorm
for(i in 1:lx){
		for(s in 1:ly){
			for(m in 1:lz){
		if(conc_media1[i,s,m]>0){intensity[i,s,m]=fluttuazioni1[i,s,m]/conc_media1[i,s,m]}
		else{intensity[i,s,m]=0	}
			}
		}
		#
}
#intensity=fluttuazioni/conc_media/conc_media
#EXP B
#xmis=c(60,72,84,96,108,120,147,160,172,185,197,209)
#ymis=c(158,142,127,111,95,80,252,236,220,205,189,174)
#EXPERIMENTO E
xmis=c(134,248)
ymis=c(61,124)
ang=-(270-247)/180*3.14

XT1P1=xmis*cos(ang)-ymis*sin(ang)
YT1P1=xmis*sin(ang)+ymis*cos(ang)
cy1=cy+250
#quartz()
#contour(cx,cy1,intensity[,,2],main="Intensities")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,fluttuazioni1[,,2],main="Fluctuations")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,conc_media1[,,2],main="Mean concentrations")
#points(XT1P1,YT1P1+250)
loc=array(c(XT1P1,YT1P1+250),dim = c(2,2))
aa=conc_media1[,,1]
maxaa=max(aa)
obj=list(x=cx,y=cy1,z=aa)
lookcm=interp.surface(obj,loc)
#plot(YT1P1[1:6],lookcm[1:6])
##points(YT1P1[7:12],lookcm[7:12])
#image.plot( as.surface( loc, look))
bb=fluttuazioni1[,,1]
obj=list(x=cx,y=cy1,z=bb)
lookfl=interp.surface(obj,loc)
lookint=lookfl/lookcm
stat1=matrix(NA,nrow=4,ncol=2)
stat0=matrix(NA,nrow=2,ncol=4)
stat0[,]=c(sqrt(xmis**2+ymis**2),lookcm,lookfl,lookint)
stat1e=t(stat0)
#stat1[,]=c(xmis,ymis,lookcm,lookfl,lookint)
#format(stat1,digits = 2)
colnames(stat1e) = c("e      T1P1"," T1P2")
rownames(stat1e) = c("DIST ","CMEAN","  CSD","  I  ")
# stat1e = format(stat1e,digits = 2,scientific=F)
#write.table(stat1,file="dat_stat.csv",append = FALSE, quote = FALSE,sep="\t")
print("E")
print(maxaa)
#write.table(stat1,append = FALSE, quote = FALSE)
##################FINE###############
#################INIZIO#################  F
dati_flut = as.matrix(read.table("factor2_eq/campo_flut_z=8.500m_ds=1.240_f.txt",sep=""))

#leggo il campo di concentrazione media
fconc<-file("factor2_22/concentrazione_z=8.500m_ds=1.240_f.txt","r")
#
#n° punti grigliato
lx = as.numeric(readLines(fconc,n=1))
ly = as.numeric(readLines(fconc,n=1))
lz = as.numeric(readLines(fconc,n=1))
#
#coordinate grigliato
cx = scan(fconc,n=lx,sep="")
cy = scan(fconc,n=ly,sep="")
cz = scan(fconc,n=lz,sep="")
#

cmedia = as.matrix(read.table(fconc,sep=""))
#
close(fconc)
#
# array delle fluttuazioni
fluttuazioni = array(0,dim=c(lx,ly,lz))
#
# array della cmedia
conc_media = array(0,dim=c(lx,ly,lz))
intensity = array(0,dim=c(lx,ly,lz))

#
#salva cmedia e fluttuazioni in 2 array 3D
for(j in 1:lz){
#	print(j)
	for(i in 1:lx){	
		fluttuazioni[i,,j] = dati_flut[(lx*(j-1)+i),]
		conc_media[i,,j] = cmedia[(lx*(j-1)+i),]
	}
}
#q=121.7 g/h   Mp=q/Np*  DT          0.0000000000003   Np=10  DT=0.1
# EXP B-H =121.7 g/h  I,J,K,L,M,N,O=252.5 
massa=121.7/3600/10*0.1
rinorm=0.0000000000003/massa/1.e+6
conc_media1=conc_media/rinorm
fluttuazioni1=sqrt(fluttuazioni)/rinorm
for(i in 1:lx){
		for(s in 1:ly){
			for(m in 1:lz){
		if(conc_media1[i,s,m]>0){intensity[i,s,m]=fluttuazioni1[i,s,m]/conc_media1[i,s,m]}
		else{intensity[i,s,m]=0	}
			}
		}
		#
}
#intensity=fluttuazioni/conc_media/conc_media
#EXP B
#xmis=c(60,72,84,96,108,120,147,160,172,185,197,209)
#ymis=c(158,142,127,111,95,80,252,236,220,205,189,174)

# EXP F   
xmis=c(122,235)
ymis=c(77,140)
ang=-(270-241)/180*3.14

XT1P1=xmis*cos(ang)-ymis*sin(ang)
YT1P1=xmis*sin(ang)+ymis*cos(ang)
cy1=cy+250
#quartz()
#contour(cx,cy1,intensity[,,2],main="Intensities")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,fluttuazioni1[,,2],main="Fluctuations")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,conc_media1[,,2],main="Mean concentrations")
#points(XT1P1,YT1P1+250)
loc=array(c(XT1P1,YT1P1+250),dim = c(2,2))
aa=conc_media1[,,1]
maxaa=max(aa)
obj=list(x=cx,y=cy1,z=aa)
lookcm=interp.surface(obj,loc)
#plot(YT1P1[1:6],lookcm[1:6])
##points(YT1P1[7:12],lookcm[7:12])
#image.plot( as.surface( loc, look))
bb=fluttuazioni1[,,1]
obj=list(x=cx,y=cy1,z=bb)
lookfl=interp.surface(obj,loc)
lookint=lookfl/lookcm
stat1=matrix(NA,nrow=4,ncol=2)
stat0=matrix(NA,nrow=2,ncol=4)
stat0[,]=c(sqrt(xmis**2+ymis**2),lookcm,lookfl,lookint)
stat1f=t(stat0)
#stat1[,]=c(xmis,ymis,lookcm,lookfl,lookint)
#format(stat1,digits = 2)
colnames(stat1f) = c("f      T1P1"," T1P2")
rownames(stat1f) = c("DIST ","CMEAN","  CSD","  I  ")
# stat1f = format(stat1f,digits = 2,scientific=F)
#write.table(stat1,file="dat_stat.csv",append = FALSE, quote = FALSE,sep="\t")
print("F")
print(maxaa)
#write.table(stat1,append = FALSE, quote = FALSE)
##################FINE###############
#################INIZIO#################  G
dati_flut = as.matrix(read.table("factor2_eq/campo_flut_z=8.500m_ds=1.240_g.txt",sep=""))

#leggo il campo di concentrazione media
fconc<-file("factor2_22/concentrazione_z=8.500m_ds=1.240_g.txt","r")
#
#n° punti grigliato
lx = as.numeric(readLines(fconc,n=1))
ly = as.numeric(readLines(fconc,n=1))
lz = as.numeric(readLines(fconc,n=1))
#
#coordinate grigliato
cx = scan(fconc,n=lx,sep="")
cy = scan(fconc,n=ly,sep="")
cz = scan(fconc,n=lz,sep="")
#

cmedia = as.matrix(read.table(fconc,sep=""))
#
close(fconc)
#
# array delle fluttuazioni
fluttuazioni = array(0,dim=c(lx,ly,lz))
#
# array della cmedia
conc_media = array(0,dim=c(lx,ly,lz))
intensity = array(0,dim=c(lx,ly,lz))

#
#salva cmedia e fluttuazioni in 2 array 3D
for(j in 1:lz){
#	print(j)
	for(i in 1:lx){	
		fluttuazioni[i,,j] = dati_flut[(lx*(j-1)+i),]
		conc_media[i,,j] = cmedia[(lx*(j-1)+i),]
	}
}
#q=121.7 g/h   Mp=q/Np*  DT          0.0000000000003   Np=10  DT=0.1
# EXP B-H =121.7 g/h  I,J,K,L,M,N,O=252.5 
massa=121.7/3600/10*0.1
rinorm=0.0000000000003/massa/1.e+6
conc_media1=conc_media/rinorm
fluttuazioni1=sqrt(fluttuazioni)/rinorm
for(i in 1:lx){
		for(s in 1:ly){
			for(m in 1:lz){
		if(conc_media1[i,s,m]>0){intensity[i,s,m]=fluttuazioni1[i,s,m]/conc_media1[i,s,m]}
		else{intensity[i,s,m]=0	}
			}
		}
		#
}
#intensity=fluttuazioni/conc_media/conc_media
#EXP B
#xmis=c(60,72,84,96,108,120,147,160,172,185,197,209)
#ymis=c(158,142,127,111,95,80,252,236,220,205,189,174)

# EXP G   
xmis=c(122,235)
ymis=c(77,140)
ang=-(270-241)/180*3.14
XT1P1=xmis*cos(ang)-ymis*sin(ang)
YT1P1=xmis*sin(ang)+ymis*cos(ang)
cy1=cy+250
#quartz()
#contour(cx,cy1,intensity[,,2],main="Intensities")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,fluttuazioni1[,,2],main="Fluctuations")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,conc_media1[,,2],main="Mean concentrations")
#points(XT1P1,YT1P1+250)
loc=array(c(XT1P1,YT1P1+250),dim = c(2,2))
aa=conc_media1[,,1]
maxaa=max(aa)
obj=list(x=cx,y=cy1,z=aa)
lookcm=interp.surface(obj,loc)
#plot(YT1P1[1:6],lookcm[1:6])
##points(YT1P1[7:12],lookcm[7:12])
#image.plot( as.surface( loc, look))
bb=fluttuazioni1[,,1]
obj=list(x=cx,y=cy1,z=bb)
lookfl=interp.surface(obj,loc)
lookint=lookfl/lookcm
stat1=matrix(NA,nrow=4,ncol=2)
stat0=matrix(NA,nrow=2,ncol=4)
stat0[,]=c(sqrt(xmis**2+ymis**2),lookcm,lookfl,lookint)
stat1g=t(stat0)
#stat1[,]=c(xmis,ymis,lookcm,lookfl,lookint)
#format(stat1,digits = 2)
colnames(stat1g) = c("g      T1P1"," T1P2")
rownames(stat1g) = c("DIST ","CMEAN","  CSD","  I  ")
# stat1g = format(stat1g,digits = 2,scientific=F)
#write.table(stat1,file="dat_stat.csv",append = FALSE, quote = FALSE,sep="\t")
print("G")
print(maxaa)
#write.table(stat1,append = FALSE, quote = FALSE)
##################FINE############### 

#################INIZIO#################  H
dati_flut = as.matrix(read.table("factor2_eq/campo_flut_z=8.500m_ds=1.240_h.txt",sep=""))

#leggo il campo di concentrazione media
fconc<-file("factor2_22/concentrazione_z=8.500m_ds=1.240_h.txt","r")
#
#n° punti grigliato
lx = as.numeric(readLines(fconc,n=1))
ly = as.numeric(readLines(fconc,n=1))
lz = as.numeric(readLines(fconc,n=1))
#
#coordinate grigliato
cx = scan(fconc,n=lx,sep="")
cy = scan(fconc,n=ly,sep="")
cz = scan(fconc,n=lz,sep="")
#

cmedia = as.matrix(read.table(fconc,sep=""))
#
close(fconc)
#
# array delle fluttuazioni
fluttuazioni = array(0,dim=c(lx,ly,lz))
#
# array della cmedia
conc_media = array(0,dim=c(lx,ly,lz))
intensity = array(0,dim=c(lx,ly,lz))

#
#salva cmedia e fluttuazioni in 2 array 3D
for(j in 1:lz){
#	print(j)
	for(i in 1:lx){	
		fluttuazioni[i,,j] = dati_flut[(lx*(j-1)+i),]
		conc_media[i,,j] = cmedia[(lx*(j-1)+i),]
	}
}
#q=121.7 g/h   Mp=q/Np*  DT          0.0000000000003   Np=10  DT=0.1
# EXP B-H =121.7 g/h  I,J,K,L,M,N,O=252.5 
massa=121.7/3600/10*0.1
rinorm=0.0000000000003/massa/1.e+6
conc_media1=conc_media/rinorm
fluttuazioni1=sqrt(fluttuazioni)/rinorm
for(i in 1:lx){
		for(s in 1:ly){
			for(m in 1:lz){
		if(conc_media1[i,s,m]>0){intensity[i,s,m]=fluttuazioni1[i,s,m]/conc_media1[i,s,m]}
		else{intensity[i,s,m]=0	}
			}
		}
		#
}
#intensity=fluttuazioni/conc_media/conc_media
#EXP B
#xmis=c(60,72,84,96,108,120,147,160,172,185,197,209)
#ymis=c(158,142,127,111,95,80,252,236,220,205,189,174)

# EXP H   
xmis=c(146,248)
ymis=c(46,124)
ang=-(270-237)/180*3.14
XT1P1=xmis*cos(ang)-ymis*sin(ang)
YT1P1=xmis*sin(ang)+ymis*cos(ang)
cy1=cy+250
#quartz()
#contour(cx,cy1,intensity[,,2],main="Intensities")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,fluttuazioni1[,,2],main="Fluctuations")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,conc_media1[,,2],main="Mean concentrations")
#points(XT1P1,YT1P1+250)
loc=array(c(XT1P1,YT1P1+250),dim = c(2,2))
aa=conc_media1[,,1]
maxaa=max(aa)
obj=list(x=cx,y=cy1,z=aa)
lookcm=interp.surface(obj,loc)
#plot(YT1P1[1:6],lookcm[1:6])
##points(YT1P1[7:12],lookcm[7:12])
#image.plot( as.surface( loc, look))
bb=fluttuazioni1[,,1]
obj=list(x=cx,y=cy1,z=bb)
lookfl=interp.surface(obj,loc)
lookint=lookfl/lookcm
stat1=matrix(NA,nrow=4,ncol=2)
stat0=matrix(NA,nrow=2,ncol=4)
stat0[,]=c(sqrt(xmis**2+ymis**2),lookcm,lookfl,lookint)
stat1h=t(stat0)
#stat1[,]=c(xmis,ymis,lookcm,lookfl,lookint)
#format(stat1,digits = 2)
colnames(stat1h) = c("h      T1P1"," T1P2")
rownames(stat1h) = c("DIST ","CMEAN","  CSD","  I  ")
# stat1h = format(stat1h,digits = 2,scientific=F)
#write.table(stat1,file="dat_stat.csv",append = FALSE, quote = FALSE,sep="\t")
print("H")
print(maxaa)
#write.table(stat1,append = FALSE, quote = FALSE)
##################FINE############### 
#################INIZIO#################      I
dati_flut = as.matrix(read.table("factor2_eq/campo_flut_z=8.500m_ds=1.240_i.txt",sep=""))

#leggo il campo di concentrazione media
fconc<-file("factor2_22/concentrazione_z=8.500m_ds=1.240_i.txt","r")
#
#n° punti grigliato
lx = as.numeric(readLines(fconc,n=1))
ly = as.numeric(readLines(fconc,n=1))
lz = as.numeric(readLines(fconc,n=1))
#
#coordinate grigliato
cx = scan(fconc,n=lx,sep="")
cy = scan(fconc,n=ly,sep="")
cz = scan(fconc,n=lz,sep="")
#

cmedia = as.matrix(read.table(fconc,sep=""))
#
close(fconc)
#
# array delle fluttuazioni
fluttuazioni = array(0,dim=c(lx,ly,lz))
#
# array della cmedia
conc_media = array(0,dim=c(lx,ly,lz))
intensity = array(0,dim=c(lx,ly,lz))

#
#salva cmedia e fluttuazioni in 2 array 3D
for(j in 1:lz){
#	print(j)
	for(i in 1:lx){	
		fluttuazioni[i,,j] = dati_flut[(lx*(j-1)+i),]
		conc_media[i,,j] = cmedia[(lx*(j-1)+i),]
	}
}
#q=121.7 g/h   Mp=q/Np*  DT          0.0000000000003   Np=10  DT=0.1
# EXP B-H =121.7 g/h  I,J,K,L,M,N,O=252.5 
massa=252.5/3600/10*0.1
rinorm=0.0000000000003/massa/1.e+6
conc_media1=conc_media/rinorm
fluttuazioni1=sqrt(fluttuazioni)/rinorm
for(i in 1:lx){
		for(s in 1:ly){
			for(m in 1:lz){
		if(conc_media1[i,s,m]>0){intensity[i,s,m]=fluttuazioni1[i,s,m]/conc_media1[i,s,m]}
		else{intensity[i,s,m]=0	}
			}
		}
		#
}
#intensity=fluttuazioni/conc_media/conc_media
#EXP B
#xmis=c(60,72,84,96,108,120,147,160,172,185,197,209)
#ymis=c(158,142,127,111,95,80,252,236,220,205,189,174)

xmis=c(81,114)
ymis=c(127,86)
#ROTAZIONE
# EXP I    
ang=-(270-220)/180*3.14

XT1P1=xmis*cos(ang)-ymis*sin(ang)
YT1P1=xmis*sin(ang)+ymis*cos(ang)
cy1=cy+250
#contour(cx,cy1,intensity[,,1],main="Intensities")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,fluttuazioni1[,,1],main="Fluctuations")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,conc_media1[,,1],main="Mean concentrations")
#points(XT1P1,YT1P1+250)
loc=array(c(XT1P1,YT1P1+250),dim = c(2,2))
aa=conc_media1[,,1]
maxaa=max(aa)
obj=list(x=cx,y=cy1,z=aa)
lookcm=interp.surface(obj,loc)
#plot(YT1P1[1:6],lookcm[1:6])
##points(YT1P1[7:12],lookcm[7:12])
#image.plot( as.surface( loc, look))
bb=fluttuazioni1[,,1]
obj=list(x=cx,y=cy1,z=bb)
lookfl=interp.surface(obj,loc)
lookint=lookfl/lookcm
stat1=matrix(NA,nrow=4,ncol=2)
stat0=matrix(NA,nrow=2,ncol=4)
stat0[,]=c(sqrt(xmis**2+ymis**2),lookcm,lookfl,lookint)
stat1i=t(stat0)
#stat1[,]=c(xmis,ymis,lookcm,lookfl,lookint)
#format(stat1,digits = 2)
colnames(stat1i) = c("i      T1P1"," T1P2")
rownames(stat1i) = c("DIST ","CMEAN","  CSD","  I  ")
# stat1i = format(stat1i,digits = 2,scientific=F)
#write.table(stat1,file="dat_stat.csv",append = FALSE, quote = FALSE,sep="\t")
print("I")
print(maxaa)
#write.table(stat1,append = FALSE, quote = FALSE)
##################FINE###############
#################INIZIO################# J
dati_flut = as.matrix(read.table("factor2_eq/campo_flut_z=8.500m_ds=1.240_j.txt",sep=""))

#leggo il campo di concentrazione media
fconc<-file("factor2_22/concentrazione_z=8.500m_ds=1.240_j.txt","r")
#
#n° punti grigliato
lx = as.numeric(readLines(fconc,n=1))
ly = as.numeric(readLines(fconc,n=1))
lz = as.numeric(readLines(fconc,n=1))
#
#coordinate grigliato
cx = scan(fconc,n=lx,sep="")
cy = scan(fconc,n=ly,sep="")
cz = scan(fconc,n=lz,sep="")
#

cmedia = as.matrix(read.table(fconc,sep=""))
#
close(fconc)
#
# array delle fluttuazioni
fluttuazioni = array(0,dim=c(lx,ly,lz))
#
# array della cmedia
conc_media = array(0,dim=c(lx,ly,lz))
intensity = array(0,dim=c(lx,ly,lz))

#
#salva cmedia e fluttuazioni in 2 array 3D
for(j in 1:lz){
#	print(j)
	for(i in 1:lx){	
		fluttuazioni[i,,j] = dati_flut[(lx*(j-1)+i),]
		conc_media[i,,j] = cmedia[(lx*(j-1)+i),]
	}
}
#q=121.7 g/h   Mp=q/Np*  DT          0.0000000000003   Np=10  DT=0.1
# EXP B-H =121.7 g/h  I,J,K,L,M,N,O=252.5 
massa=252.5/3600/10*0.1
rinorm=0.0000000000003/massa/1.e+6
conc_media1=conc_media/rinorm
fluttuazioni1=sqrt(fluttuazioni)/rinorm
for(i in 1:lx){
		for(s in 1:ly){
			for(m in 1:lz){
		if(conc_media1[i,s,m]>0){intensity[i,s,m]=fluttuazioni1[i,s,m]/conc_media1[i,s,m]}
		else{intensity[i,s,m]=0	}
			}
		}
		#
}
#intensity=fluttuazioni/conc_media/conc_media
#EXP B
#xmis=c(60,72,84,96,108,120,147,160,172,185,197,209)
#ymis=c(158,142,127,111,95,80,252,236,220,205,189,174)
#EXP B, C
xmis=c(81,110)
ymis=c(127,92)
#ROTAZIONE

# EXP J    
ang=-(270-222)/180*3.14

XT1P1=xmis*cos(ang)-ymis*sin(ang)
YT1P1=xmis*sin(ang)+ymis*cos(ang)
cy1=cy+250
#contour(cx,cy1,intensity[,,1],main="Intensities")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,fluttuazioni1[,,1],main="Fluctuations")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,conc_media1[,,1],main="Mean concentrations")
#points(XT1P1,YT1P1+250)
loc=array(c(XT1P1,YT1P1+250),dim = c(2,2))
aa=conc_media1[,,1]
maxaa=max(aa)
obj=list(x=cx,y=cy1,z=aa)
lookcm=interp.surface(obj,loc)
#plot(YT1P1[1:6],lookcm[1:6])
##points(YT1P1[7:12],lookcm[7:12])
#image.plot( as.surface( loc, look))
bb=fluttuazioni1[,,1]
obj=list(x=cx,y=cy1,z=bb)
lookfl=interp.surface(obj,loc)
lookint=lookfl/lookcm
stat1=matrix(NA,nrow=4,ncol=2)
stat0=matrix(NA,nrow=2,ncol=4)
stat0[,]=c(sqrt(xmis**2+ymis**2),lookcm,lookfl,lookint)
stat1j=t(stat0)
#stat1[,]=c(xmis,ymis,lookcm,lookfl,lookint)
#format(stat1,digits = 2)
colnames(stat1j) = c("j      T1P1"," T1P2")
rownames(stat1j) = c("DIST ","CMEAN","  CSD","  I  ")
# stat1j = format(stat1j,digits = 2,scientific=F)
#write.table(stat1,file="dat_stat.csv",append = FALSE, quote = FALSE,sep="\t")
print("J")
print(maxaa)
#write.table(stat1,append = FALSE, quote = FALSE)
##################FINE###############
#################INIZIO################# K
dati_flut = as.matrix(read.table("factor2_eq/campo_flut_z=8.500m_ds=1.240_k.txt",sep=""))

#leggo il campo di concentrazione media
fconc<-file("factor2_22/concentrazione_z=8.500m_ds=1.240_k.txt","r")
#
#n° punti grigliato
lx = as.numeric(readLines(fconc,n=1))
ly = as.numeric(readLines(fconc,n=1))
lz = as.numeric(readLines(fconc,n=1))
#
#coordinate grigliato
cx = scan(fconc,n=lx,sep="")
cy = scan(fconc,n=ly,sep="")
cz = scan(fconc,n=lz,sep="")
#

cmedia = as.matrix(read.table(fconc,sep=""))
#
close(fconc)
#
# array delle fluttuazioni
fluttuazioni = array(0,dim=c(lx,ly,lz))
#
# array della cmedia
conc_media = array(0,dim=c(lx,ly,lz))
intensity = array(0,dim=c(lx,ly,lz))

#
#salva cmedia e fluttuazioni in 2 array 3D
for(j in 1:lz){
#	print(j)
	for(i in 1:lx){	
		fluttuazioni[i,,j] = dati_flut[(lx*(j-1)+i),]
		conc_media[i,,j] = cmedia[(lx*(j-1)+i),]
	}
}
#q=121.7 g/h   Mp=q/Np*  DT          0.0000000000003   Np=10  DT=0.1
# EXP B-H =121.7 g/h  I,J,K,L,M,N,O=252.5 
massa=252.5/3600/10*0.1
rinorm=0.0000000000003/massa/1.e+6
conc_media1=conc_media/rinorm
fluttuazioni1=sqrt(fluttuazioni)/rinorm
for(i in 1:lx){
		for(s in 1:ly){
			for(m in 1:lz){
		if(conc_media1[i,s,m]>0){intensity[i,s,m]=fluttuazioni1[i,s,m]/conc_media1[i,s,m]}
		else{intensity[i,s,m]=0	}
			}
		}
		#
}
#intensity=fluttuazioni/conc_media/conc_media
#EXP B
#xmis=c(60,72,84,96,108,120,147,160,172,185,197,209)
#ymis=c(158,142,127,111,95,80,252,236,220,205,189,174)
#ROTAZIONE
# EXP K   
xmis=c(81,114)
ymis=c(127,86)
ang=-(270-220)/180*3.14

XT1P1=xmis*cos(ang)-ymis*sin(ang)
YT1P1=xmis*sin(ang)+ymis*cos(ang)
cy1=cy+250
#contour(cx,cy1,intensity[,,1],main="Intensities")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,fluttuazioni1[,,1],main="Fluctuations")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,conc_media1[,,1],main="Mean concentrations")
#points(XT1P1,YT1P1+250)
loc=array(c(XT1P1,YT1P1+250),dim = c(2,2))
aa=conc_media1[,,1]
maxaa=max(aa)
obj=list(x=cx,y=cy1,z=aa)
lookcm=interp.surface(obj,loc)
#plot(YT1P1[1:6],lookcm[1:6])
##points(YT1P1[7:12],lookcm[7:12])
#image.plot( as.surface( loc, look))
bb=fluttuazioni1[,,1]
obj=list(x=cx,y=cy1,z=bb)
lookfl=interp.surface(obj,loc)
lookint=lookfl/lookcm
stat1=matrix(NA,nrow=4,ncol=2)
stat0=matrix(NA,nrow=2,ncol=4)
stat0[,]=c(sqrt(xmis**2+ymis**2),lookcm,lookfl,lookint)
stat1k=t(stat0)
#stat1[,]=c(xmis,ymis,lookcm,lookfl,lookint)
#format(stat1,digits = 2)
colnames(stat1k) = c("k      T1P1"," T1P2")
rownames(stat1k) = c("DIST ","CMEAN","  CSD","  I  ")
# stat1k = format(stat1k,digits = 2,scientific=F)
#write.table(stat1,file="dat_stat.csv",append = FALSE, quote = FALSE,sep="\t")
print("K")
print(maxaa)
#write.table(stat1,append = FALSE, quote = FALSE)
##################FINE###############
#################INIZIO################# L
dati_flut = as.matrix(read.table("factor2_eq/campo_flut_z=8.500m_ds=1.240_l.txt",sep=""))

#leggo il campo di concentrazione media
fconc<-file("factor2_22/concentrazione_z=8.500m_ds=1.240_l.txt","r")
#
#n° punti grigliato
lx = as.numeric(readLines(fconc,n=1))
ly = as.numeric(readLines(fconc,n=1))
lz = as.numeric(readLines(fconc,n=1))
#
#coordinate grigliato
cx = scan(fconc,n=lx,sep="")
cy = scan(fconc,n=ly,sep="")
cz = scan(fconc,n=lz,sep="")
#

cmedia = as.matrix(read.table(fconc,sep=""))
#
close(fconc)
#
# array delle fluttuazioni
fluttuazioni = array(0,dim=c(lx,ly,lz))
#
# array della cmedia
conc_media = array(0,dim=c(lx,ly,lz))
intensity = array(0,dim=c(lx,ly,lz))

#
#salva cmedia e fluttuazioni in 2 array 3D
for(j in 1:lz){
#	print(j)
	for(i in 1:lx){	
		fluttuazioni[i,,j] = dati_flut[(lx*(j-1)+i),]
		conc_media[i,,j] = cmedia[(lx*(j-1)+i),]
	}
}
#q=121.7 g/h   Mp=q/Np*  DT          0.0000000000003   Np=10  DT=0.1
# EXP B-H =121.7 g/h  I,J,K,L,M,N,O=252.5 
massa=252.5/3600/10*0.1
rinorm=0.0000000000003/massa/1.e+6
conc_media1=conc_media/rinorm
fluttuazioni1=sqrt(fluttuazioni)/rinorm
for(i in 1:lx){
		for(s in 1:ly){
			for(m in 1:lz){
		if(conc_media1[i,s,m]>0){intensity[i,s,m]=fluttuazioni1[i,s,m]/conc_media1[i,s,m]}
		else{intensity[i,s,m]=0	}
			}
		}
		#
}
#intensity=fluttuazioni/conc_media/conc_media
#EXP L
#xmis=c(60,72,84,96,108,120,147,160,172,185,197,209)
#ymis=c(158,142,127,111,95,80,252,236,220,205,189,174)
#EXPERIMENTO E
xmis=c(81,114)
ymis=c(127,86)
ang=-(270-222)/180*3.14

XT1P1=xmis*cos(ang)-ymis*sin(ang)
YT1P1=xmis*sin(ang)+ymis*cos(ang)
cy1=cy+250
#contour(cx,cy1,intensity[,,1],main="Intensities")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,fluttuazioni1[,,1],main="Fluctuations")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,conc_media1[,,1],main="Mean concentrations")
#points(XT1P1,YT1P1+250)
loc=array(c(XT1P1,YT1P1+250),dim = c(2,2))
aa=conc_media1[,,1]
maxaa=max(aa)
obj=list(x=cx,y=cy1,z=aa)
lookcm=interp.surface(obj,loc)
#plot(YT1P1[1:6],lookcm[1:6])
##points(YT1P1[7:12],lookcm[7:12])
#image.plot( as.surface( loc, look))
bb=fluttuazioni1[,,1]
obj=list(x=cx,y=cy1,z=bb)
lookfl=interp.surface(obj,loc)
lookint=lookfl/lookcm
stat1=matrix(NA,nrow=4,ncol=2)
stat0=matrix(NA,nrow=2,ncol=4)
stat0[,]=c(sqrt(xmis**2+ymis**2),lookcm,lookfl,lookint)
stat1l=t(stat0)
#stat1[,]=c(xmis,ymis,lookcm,lookfl,lookint)
#format(stat1,digits = 2)
colnames(stat1l) = c("l      T1P1"," T1P2")
rownames(stat1l) = c("DIST ","CMEAN","  CSD","  I  ")
# stat1l = format(stat1l,digits = 2,scientific=F)
#write.table(stat1,file="dat_stat.csv",append = FALSE, quote = FALSE,sep="\t")
print("L")
print(maxaa)
#write.table(stat1,append = FALSE, quote = FALSE)
##################FINE###############
#################INIZIO#################  M
dati_flut = as.matrix(read.table("factor2_eq/campo_flut_z=8.500m_ds=1.240_m.txt",sep=""))

#leggo il campo di concentrazione media
fconc<-file("factor2_22/concentrazione_z=8.500m_ds=1.240_m.txt","r")
#
#n° punti grigliato
lx = as.numeric(readLines(fconc,n=1))
ly = as.numeric(readLines(fconc,n=1))
lz = as.numeric(readLines(fconc,n=1))
#
#coordinate grigliato
cx = scan(fconc,n=lx,sep="")
cy = scan(fconc,n=ly,sep="")
cz = scan(fconc,n=lz,sep="")
#

cmedia = as.matrix(read.table(fconc,sep=""))
#
close(fconc)
#
# array delle fluttuazioni
fluttuazioni = array(0,dim=c(lx,ly,lz))
#
# array della cmedia
conc_media = array(0,dim=c(lx,ly,lz))
intensity = array(0,dim=c(lx,ly,lz))

#
#salva cmedia e fluttuazioni in 2 array 3D
for(j in 1:lz){
#	print(j)
	for(i in 1:lx){	
		fluttuazioni[i,,j] = dati_flut[(lx*(j-1)+i),]
		conc_media[i,,j] = cmedia[(lx*(j-1)+i),]
	}
}
#q=121.7 g/h   Mp=q/Np*  DT          0.0000000000003   Np=10  DT=0.1
# EXP B-H =121.7 g/h  I,J,K,L,M,N,O=252.5 
massa=252.5/3600/10*0.1
rinorm=0.0000000000003/massa/1.e+6
conc_media1=conc_media/rinorm
fluttuazioni1=sqrt(fluttuazioni)/rinorm
for(i in 1:lx){
		for(s in 1:ly){
			for(m in 1:lz){
		if(conc_media1[i,s,m]>0){intensity[i,s,m]=fluttuazioni1[i,s,m]/conc_media1[i,s,m]}
		else{intensity[i,s,m]=0	}
			}
		}
		#
}
#intensity=fluttuazioni/conc_media/conc_media
#EXP B
#xmis=c(60,72,84,96,108,120,147,160,172,185,197,209)
#ymis=c(158,142,127,111,95,80,252,236,220,205,189,174)

# EXP M   
xmis=c(116,222)
ymis=c(88,161)
ang=-(270-232)/180*3.14

XT1P1=xmis*cos(ang)-ymis*sin(ang)
YT1P1=xmis*sin(ang)+ymis*cos(ang)
cy1=cy+250
#contour(cx,cy1,intensity[,,1],main="Intensities")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,fluttuazioni1[,,1],main="Fluctuations")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,conc_media1[,,1],main="Mean concentrations")
#points(XT1P1,YT1P1+250)
loc=array(c(XT1P1,YT1P1+250),dim = c(2,2))
aa=conc_media1[,,1]
maxaa=max(aa)
obj=list(x=cx,y=cy1,z=aa)
lookcm=interp.surface(obj,loc)
#lookcm=interp(cx, cy1, aa, xo=XT1P1, yo=YT1P1+250, linear = TRUE, extrap=FALSE, duplicate = "error", dupfun = NULL, ncp = NULL)
#plot(YT1P1[1:6],lookcm[1:6])
##points(YT1P1[7:12],lookcm[7:12])
#image.plot( as.surface( loc, look))
bb=fluttuazioni1[,,1]
obj=list(x=cx,y=cy1,z=bb)
lookfl=interp.surface(obj,loc)
lookint=lookfl/lookcm
stat1=matrix(NA,nrow=4,ncol=2)
stat0=matrix(NA,nrow=2,ncol=4)
stat0[,]=c(sqrt(xmis**2+ymis**2),lookcm,lookfl,lookint)
stat1m=t(stat0)
#stat1[,]=c(xmis,ymis,lookcm,lookfl,lookint)
#format(stat1,digits = 2)
colnames(stat1m) = c("m      T1P1"," T1P2")
rownames(stat1m) = c("DIST ","CMEAN","  CSD","  I  ")
# stat1m = format(stat1m,digits = 2,scientific=F)
#write.table(stat1,file="dat_stat.csv",append = FALSE, quote = FALSE,sep="\t")
print("M")
print(maxaa)
#write.table(stat1,append = FALSE, quote = FALSE)
##################FINE###############
#################INIZIO#################  N
dati_flut = as.matrix(read.table("factor2_eq/campo_flut_z=8.500m_ds=1.240_n.txt",sep=""))

#leggo il campo di concentrazione media
fconc<-file("factor2_22/concentrazione_z=8.500m_ds=1.240_n.txt","r")
#
#n° punti grigliato
lx = as.numeric(readLines(fconc,n=1))
ly = as.numeric(readLines(fconc,n=1))
lz = as.numeric(readLines(fconc,n=1))
#
#coordinate grigliato
cx = scan(fconc,n=lx,sep="")
cy = scan(fconc,n=ly,sep="")
cz = scan(fconc,n=lz,sep="")
#

cmedia = as.matrix(read.table(fconc,sep=""))
#
close(fconc)
#
# array delle fluttuazioni
fluttuazioni = array(0,dim=c(lx,ly,lz))
#
# array della cmedia
conc_media = array(0,dim=c(lx,ly,lz))
intensity = array(0,dim=c(lx,ly,lz))

#
#salva cmedia e fluttuazioni in 2 array 3D
for(j in 1:lz){
#	print(j)
	for(i in 1:lx){	
		fluttuazioni[i,,j] = dati_flut[(lx*(j-1)+i),]
		conc_media[i,,j] = cmedia[(lx*(j-1)+i),]
	}
}
#q=121.7 g/h   Mp=q/Np*  DT          0.0000000000003   Np=10  DT=0.1
# EXP B-H =121.7 g/h  I,J,K,L,M,N,O=252.5 
massa=252.5/3600/10*0.1
rinorm=0.0000000000003/massa/1.e+6
conc_media1=conc_media/rinorm
fluttuazioni1=sqrt(fluttuazioni)/rinorm
for(i in 1:lx){
		for(s in 1:ly){
			for(m in 1:lz){
		if(conc_media1[i,s,m]>0){intensity[i,s,m]=fluttuazioni1[i,s,m]/conc_media1[i,s,m]}
		else{intensity[i,s,m]=0	}
			}
		}
		#
}
#intensity=fluttuazioni/conc_media/conc_media
#EXP B
#xmis=c(60,72,84,96,108,120,147,160,172,185,197,209)
#ymis=c(158,142,127,111,95,80,252,236,220,205,189,174)

# EXP N   
xmis=c(116,222)
ymis=c(88,161)
ang=-(270-230)/180*3.14
XT1P1=xmis*cos(ang)-ymis*sin(ang)
YT1P1=xmis*sin(ang)+ymis*cos(ang)
cy1=cy+250
#contour(cx,cy1,intensity[,,1],main="Intensities")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,fluttuazioni1[,,1],main="Fluctuations")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,conc_media1[,,1],main="Mean concentrations")
#points(XT1P1,YT1P1+250)
loc=array(c(XT1P1,YT1P1+250),dim = c(2,2))
aa=conc_media1[,,1]
maxaa=max(aa)
obj=list(x=cx,y=cy1,z=aa)
lookcm=interp.surface(obj,loc)
#plot(YT1P1[1:6],lookcm[1:6])
##points(YT1P1[7:12],lookcm[7:12])
#image.plot( as.surface( loc, look))
bb=fluttuazioni1[,,1]
obj=list(x=cx,y=cy1,z=bb)
lookfl=interp.surface(obj,loc)
lookint=lookfl/lookcm
stat1=matrix(NA,nrow=4,ncol=2)
stat0=matrix(NA,nrow=2,ncol=4)
stat0[,]=c(sqrt(xmis**2+ymis**2),lookcm,lookfl,lookint)
stat1n=t(stat0)
#stat1[,]=c(xmis,ymis,lookcm,lookfl,lookint)
#format(stat1,digits = 2)
colnames(stat1n) = c("n      T1P1"," T1P2")
rownames(stat1n) = c("DIST ","CMEAN","  CSD","  I  ")
# stat1n = format(stat1n,digits = 2,scientific=F)
#write.table(stat1,file="dat_stat.csv",append = FALSE, quote = FALSE,sep="\t")
print("N")
print(maxaa)
#write.table(stat1,append = FALSE, quote = FALSE)
##################FINE############### 

#################INIZIO#################  O
dati_flut = as.matrix(read.table("factor2_eq/campo_flut_z=8.500m_ds=1.240_o.txt",sep=""))

#leggo il campo di concentrazione media
fconc<-file("factor2_22/concentrazione_z=8.500m_ds=1.240_o.txt","r")
#
#n° punti grigliato
lx = as.numeric(readLines(fconc,n=1))
ly = as.numeric(readLines(fconc,n=1))
lz = as.numeric(readLines(fconc,n=1))
#
#coordinate grigliato
cx = scan(fconc,n=lx,sep="")
cy = scan(fconc,n=ly,sep="")
cz = scan(fconc,n=lz,sep="")
#

cmedia = as.matrix(read.table(fconc,sep=""))
#
close(fconc)
#
# array delle fluttuazioni
fluttuazioni = array(0,dim=c(lx,ly,lz))
#
# array della cmedia
conc_media = array(0,dim=c(lx,ly,lz))
intensity = array(0,dim=c(lx,ly,lz))

#
#salva cmedia e fluttuazioni in 2 array 3D
for(j in 1:lz){
#	print(j)
	for(i in 1:lx){	
		fluttuazioni[i,,j] = dati_flut[(lx*(j-1)+i),]
		conc_media[i,,j] = cmedia[(lx*(j-1)+i),]
	}
}
#q=121.7 g/h   Mp=q/Np*  DT          0.0000000000003   Np=10  DT=0.1
# EXP B-H =121.7 g/h  I,J,K,L,M,N,O=252.5 
massa=252.5/3600/10*0.1
rinorm=0.0000000000003/massa/1.e+6
conc_media1=conc_media/rinorm
fluttuazioni1=sqrt(fluttuazioni)/rinorm
for(i in 1:lx){
		for(s in 1:ly){
			for(m in 1:lz){
		if(conc_media1[i,s,m]>0){intensity[i,s,m]=fluttuazioni1[i,s,m]/conc_media1[i,s,m]}
		else{intensity[i,s,m]=0	}
			}
		}
		#
}
#intensity=fluttuazioni/conc_media/conc_media
#EXP B
#xmis=c(60,72,84,96,108,120,147,160,172,185,197,209)
#ymis=c(158,142,127,111,95,80,252,236,220,205,189,174)

# EXP O   
xmis=c(116,222)
ymis=c(88,161)
ang=-(270-222)/180*3.14
XT1P1=xmis*cos(ang)-ymis*sin(ang)
YT1P1=xmis*sin(ang)+ymis*cos(ang)
cy1=cy+250
#contour(cx,cy1,intensity[,,3],main="Intensities")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,fluttuazioni1[,,3],main="Fluctuations")
#points(XT1P1,YT1P1+250)
#quartz()
#contour(cx,cy1,conc_media1[,,3],main="Mean concentrations")
#points(XT1P1,YT1P1+250)
loc=array(c(XT1P1,YT1P1+250),dim = c(2,2))
aa=conc_media1[,,1]
maxaa=max(aa)
obj=list(x=cx,y=cy1,z=aa)
lookcm=interp.surface(obj,loc)
#plot(YT1P1[1:6],lookcm[1:6])
##points(YT1P1[7:12],lookcm[7:12])
#image.plot( as.surface( loc, look))
bb=fluttuazioni1[,,1]
obj=list(x=cx,y=cy1,z=bb)
lookfl=interp.surface(obj,loc)
lookint=lookfl/lookcm
stat1=matrix(NA,nrow=4,ncol=2)
stat0=matrix(NA,nrow=2,ncol=4)
stat0[,]=c(sqrt(xmis**2+ymis**2),lookcm,lookfl,lookint)
stat1o=t(stat0)
#stat1[,]=c(xmis,ymis,lookcm,lookfl,lookint)
#format(stat1,digits = 2)
colnames(stat1o) = c("o      T1P1"," T1P2")
rownames(stat1o) = c("DIST ","CMEAN","  CSD","  I  ")
# stat1o = format(stat1o,digits = 2,scientific=F)
#write.table(stat1,file="dat_stat.csv",append = FALSE, quote = FALSE,sep="\t")
print("O")
print(maxaa)
#write.table(stat1,append = FALSE, quote = FALSE)
##################FINE############### 


# write.table(stat1b,append = FALSE, quote = FALSE)
# write.table(stat1c,append = FALSE, quote = FALSE)
# write.table(stat1d,append = FALSE, quote = FALSE)
# write.table(stat1e,append = FALSE, quote = FALSE)
# write.table(stat1f,append = FALSE, quote = FALSE)
# write.table(stat1g,append = FALSE, quote = FALSE)
# write.table(stat1h,append = FALSE, quote = FALSE)
# write.table(stat1i,append = FALSE, quote = FALSE)
# write.table(stat1j,append = FALSE, quote = FALSE)
# write.table(stat1k,append = FALSE, quote = FALSE)
# write.table(stat1l,append = FALSE, quote = FALSE)
# write.table(stat1m,append = FALSE, quote = FALSE)
# write.table(stat1n,append = FALSE, quote = FALSE)
# write.table(stat1o,append = FALSE, quote = FALSE)

statutto=matrix(NA,nrow=28,ncol=4)
colnames(statutto) = c("DIST ","CMEAN","  CSD","  I  ")
statutto[1:2,]=t(stat1b)
statutto[3:4,]=t(stat1c)
statutto[5:6,]=t(stat1d)
statutto[7:8,]=t(stat1e)
statutto[9:10,]=t(stat1f)
statutto[11:12,]=t(stat1g)
statutto[13:14,]=t(stat1h)
statutto[15:16,]=t(stat1i)
statutto[17:18,]=t(stat1j)
statutto[19:20,]=t(stat1k)
statutto[21:22,]=t(stat1l)
statutto[23:24,]=t(stat1m)
statutto[25:26,]=t(stat1n)
statutto[27:28,]=t(stat1o)
write.table(statutto,append = FALSE, quote = FALSE)
meanc=mean(statutto[,2],na.rm=T)
means=mean(statutto[,3],na.rm=T)
meani=mean(statutto[,4],na.rm=T)
meantutto=c(meanc,means,meani)
print(meantutto)

data=read.table("Uttenweiler_experiments_data.csv",sep=";",skip=1)
 

mis=data
cinter=statutto[,2:4]
# mean
meanDm=mean(mis[,1],na.rm=TRUE)
meanMm=mean(cinter[,1],na.rm=TRUE)
FBm=(meanDm-meanMm)/(meanDm+meanMm)*2
NMSEm=mean((mis[,1]-cinter[,1])^2,na.rm=TRUE)/meanDm/meanMm
Rm=cor(mis[,1],cinter[,1], use="pairwise.complete.obs")
maxDm=max(mis[,1],na.rm=TRUE)
maxMm=max(cinter[,1],na.rm=TRUE)

conto=0
Sf2=0
for (i in seq(length(mis[,1]))){
         if(mis[i,1]!=0 & !is.na(mis[i,1])) {
        conto=conto+1
	if (cinter[i,1]/mis[i,1] < 2 & cinter[i,1]/mis[i,1] > 0.5){
		Sf2=Sf2+1
	 }
	}
        }
f2m=Sf2/conto*100

Sf5=0
conto=0
for (i in seq(length(mis[,1]))){
        if(mis[i,1]!=0 & !is.na(mis[i,1])) {
        conto=conto+1
	if (cinter[i,1]/mis[i,1] < 5 & cinter[i,1]/mis[i,1] > 0.2 ){
		Sf5=Sf5+1
	}
	}
}
f5m=Sf5/conto*100

#standard deviations

meanDs=mean(mis[,2],na.rm=TRUE)
meanMs=mean(cinter[,2],na.rm=TRUE)
FBs=(meanDs-meanMs)/(meanDs+meanMs)*2
NMSEs=mean((mis[,2]-cinter[,2])^2,na.rm=TRUE)/meanDs/meanMs
Rs=cor(mis[,2],cinter[,2], use="pairwise.complete.obs")
maxDs=max(mis[,2],na.rm=TRUE)
maxMs=max(cinter[,2],na.rm=TRUE)

conto=0
Sf2=0
for (i in seq(length(mis[,2]))){
         if(mis[i,2]!=0 & !is.na(mis[i,2])) {
        conto=conto+1
	if (cinter[i,2]/mis[i,2] < 2 & cinter[i,2]/mis[i,2] > 0.5){
		Sf2=Sf2+1
	 }
	}
        }
f2s=Sf2/conto*100

Sf5=0
conto=0
for (i in seq(length(mis[,2]))){
        if(mis[i,2]!=0 & !is.na(mis[i,2])) {
        conto=conto+1
	if (cinter[i,2]/mis[i,2] < 5 & cinter[i,2]/mis[i,2] > 0.2 ){
		Sf5=Sf5+1
	}
	}
}
f5s=Sf5/conto*100

# Intensity

meanDi=mean(mis[,3],na.rm=TRUE)
meanMi=mean(cinter[,3],na.rm=TRUE)
FBi=(meanDi-meanMi)/(meanDi+meanMi)*2
NMSEi=mean((mis[,3]-cinter[,3])^2,na.rm=TRUE)/meanDi/meanMi
Ri=cor(mis[,3],cinter[,3], use="pairwise.complete.obs")
maxDi=max(mis[,3],na.rm=TRUE)
maxMi=max(cinter[,3],na.rm=TRUE)

conto=0
Sf2=0
for (i in seq(length(mis[,3]))){
         if(mis[i,3]!=0 & !is.na(mis[i,3])) {
        conto=conto+1
	if (cinter[i,3]/mis[i,3] < 2 & cinter[i,3]/mis[i,3] > 0.5){
		Sf2=Sf2+1
	 }
	}
        }
f2i=Sf2/conto*100

Sf5=0
conto=0
for (i in seq(length(mis[,3]))){
        if(mis[i,3]!=0 & !is.na(mis[i,3])) {
        conto=conto+1
	if (cinter[i,3]/mis[i,3] < 5 & cinter[i,3]/mis[i,3] > 0.2 ){
		Sf5=Sf5+1
	}
	}
}
f5i=Sf5/conto*100



stat1=matrix(NA,nrow=3,ncol=9)
stat1[1,]=c(meanDm,meanMm,maxDm,maxMm, FBm,NMSEm,Rm,f2m,f5m)
stat1[2,]=c(meanDs,meanMs,maxDs,maxMs, FBs,NMSEs,Rs,f2s,f5s)
stat1[3,]=c(meanDi,meanMi,maxDi,maxMi, FBi,NMSEi,Ri,f2i,f5i)
colnames(stat1) = c("       mean m","  mean c","   max m", "   max c", "     FB","    NMSE","      R","     F2","     F5")
rownames(stat1) = c("MEAN","STD ","INT ")
stat1 = format(stat1,digits = 3,scientific=F)
write.table(stat1,append = F, quote = FALSE)
write.table(stat1,file='fire.txt',append = FALSE, quote = FALSE)

#SEPARO LE DISTANZE
mis1=matrix(NA,nrow=28,ncol=3)
mis2=matrix(NA,nrow=28,ncol=3)
cinter1=matrix(NA,nrow=28,ncol=3)
cinter2=matrix(NA,nrow=28,ncol=3)
for (i in seq(length(mis[,3]))){
	if(statutto[i,1]<200){
	mis1[i,1]=mis[i,1]	
	mis1[i,2]=mis[i,2]	
	mis1[i,3]=mis[i,3]
	cinter1[i,1]=cinter[i,1]	
	cinter1[i,2]=cinter[i,2]	
	cinter1[i,3]=cinter[i,3]	
	}else{
    mis2[i,1]=mis[i,1]	
	mis2[i,2]=mis[i,2]	
	mis2[i,3]=mis[i,3]
	cinter2[i,1]=cinter[i,1]	
	cinter2[i,2]=cinter[i,2]	
	cinter2[i,3]=cinter[i,3]			
}
}

#MEAN
meanDm1=mean(mis1[,1],na.rm=TRUE)
meanMm1=mean(cinter1[,1],na.rm=TRUE)
FBm1=(meanDm1-meanMm1)/(meanDm1+meanMm1)*2
NMSEm1=mean((mis1[,1]-cinter1[,1])^2,na.rm=TRUE)/meanDm1/meanMm1
Rm1=cor(mis1[,1],cinter1[,1], use="pairwise.complete.obs")
maxDm1=max(mis1[,1],na.rm=TRUE)
maxMm1=max(cinter1[,1],na.rm=TRUE)

meanDm2=mean(mis2[,1],na.rm=TRUE)
meanMm2=mean(cinter2[,1],na.rm=TRUE)
FBm2=(meanDm2-meanMm2)/(meanDm2+meanMm2)*2
NMSEm2=mean((mis2[,1]-cinter2[,1])^2,na.rm=TRUE)/meanDm2/meanMm2
Rm2=cor(mis2[,1],cinter2[,1], use="pairwise.complete.obs")
maxDm2=max(mis2[,1],na.rm=TRUE)
maxMm2=max(cinter2[,1],na.rm=TRUE)

#STANDARD DEVIATION
meanDs1=mean(mis1[,2],na.rm=TRUE)
meanMs1=mean(cinter1[,2],na.rm=TRUE)
FBs1=(meanDs1-meanMs1)/(meanDs1+meanMs1)*2
NMSEs1=mean((mis1[,2]-cinter1[,2])^2,na.rm=TRUE)/meanDs1/meanMs1
Rs1=cor(mis1[,2],cinter1[,2], use="pairwise.complete.obs")
maxDs1=max(mis1[,2],na.rm=TRUE)
maxMs1=max(cinter1[,2],na.rm=TRUE)

meanDs2=mean(mis2[,2],na.rm=TRUE)
meanMs2=mean(cinter2[,2],na.rm=TRUE)
FBs2=(meanDs2-meanMs2)/(meanDs2+meanMs2)*2
NMSEs2=mean((mis2[,2]-cinter2[,2])^2,na.rm=TRUE)/meanDs2/meanMs2
Rs2=cor(mis2[,2],cinter2[,2], use="pairwise.complete.obs")
maxDs2=max(mis2[,2],na.rm=TRUE)
maxMs2=max(cinter2[,2],na.rm=TRUE)

#INTENSITY
meanDi1=mean(mis1[,3],na.rm=TRUE)
meanMi1=mean(cinter1[,3],na.rm=TRUE)
FBi1=(meanDi1-meanMi1)/(meanDi1+meanMi1)*2
NMSEi1=mean((mis1[,3]-cinter1[,3])^2,na.rm=TRUE)/meanDi1/meanMi1
Ri1=cor(mis1[,3],cinter1[,3], use="pairwise.complete.obs")
maxDi1=max(mis1[,3],na.rm=TRUE)
maxMi1=max(cinter1[,3],na.rm=TRUE)

meanDi2=mean(mis2[,3],na.rm=TRUE)
meanMi2=mean(cinter2[,3],na.rm=TRUE)
FBi2=(meanDi2-meanMi2)/(meanDi2+meanMi2)*2
NMSEi2=mean((mis2[,3]-cinter2[,3])^2,na.rm=TRUE)/meanDi2/meanMi2
Ri2=cor(mis2[,3],cinter2[,3], use="pairwise.complete.obs")
maxDi2=max(mis2[,3],na.rm=TRUE)
maxMi2=max(cinter2[,3],na.rm=TRUE)

stat11=matrix(NA,nrow=3,ncol=7)
stat11[1,]=c(meanDm1,meanMm1,maxDm1,maxMm1, FBm1,NMSEm1,Rm1)
stat11[2,]=c(meanDs1,meanMs1,maxDs1,maxMs1, FBs1,NMSEs1,Rs1)
stat11[3,]=c(meanDi1,meanMi1,maxDi1,maxMi1, FBi1,NMSEi1,Ri1)
colnames(stat11) = c("       mean m","  mean c","   max m", "   max c", "     FB","    NMSE","      R")
rownames(stat11) = c("MEAN","STD ","INT ")
stat11 = format(stat11,digits = 3,scientific=F)
write.table(stat11,append = F, quote = FALSE)
write.table(stat11,file='fire.txt',append = T, quote = FALSE)

stat12=matrix(NA,nrow=3,ncol=7)
stat12[1,]=c(meanDm2,meanMm2,maxDm2,maxMm2, FBm2,NMSEm2,Rm2)
stat12[2,]=c(meanDs2,meanMs2,maxDs2,maxMs2, FBs2,NMSEs2,Rs2)
stat12[3,]=c(meanDi2,meanMi2,maxDi2,maxMi2, FBi2,NMSEi2,Ri2)
colnames(stat12) = c("       mean m","  mean c","   max m", "   max c", "     FB","    NMSE","      R")
rownames(stat12) = c("MEAN","STD ","INT ")
stat12 = format(stat12,digits = 3,scientific=F)
write.table(stat12,append = F, quote = FALSE)
write.table(stat12,file='fire.txt',append = T, quote = FALSE)

quartz(width=12)
par(mfrow=c(2,3))
 plot(cinter[,1],mis[,1],xlim=c(0,50),ylim=c(0,50),main="Mean",xlab="model",ylab="observations",cex.axis=1.5,cex.lab=1.5)
 abline(0,1)
 plot(cinter1[,1],mis1[,1],xlim=c(0,50),ylim=c(0,50),main="Mean",xlab="model",ylab="observations",cex.axis=1.5,cex.lab=1.5)
 abline(0,1)
 plot(cinter2[,1],mis2[,1],xlim=c(0,50),ylim=c(0,50),main="Mean",xlab="model",ylab="observations",cex.axis=1.5,cex.lab=1.5)
 abline(0,1)
 qqplot(cinter[,1],mis[,1],xlim=c(0,50),ylim=c(0,50),main="Mean",xlab="model",ylab="observations",cex.axis=1.5,cex.lab=1.5)
 abline(0,1)
 qqplot(cinter1[,1],mis1[,1],xlim=c(0,50),ylim=c(0,50),main="Mean",xlab="model",ylab="observations",cex.axis=1.5,cex.lab=1.5)
 abline(0,1)
 qqplot(cinter2[,1],mis2[,1],xlim=c(0,50),ylim=c(0,50),main="Mean",xlab="model",ylab="observations",cex.axis=1.5,cex.lab=1.5)
 abline(0,1)

quartz()
 par(mfrow=c(3,4))
 
 plot(cinter[,2],mis[,2],xlim=c(0,25),ylim=c(0,25),main="Standard deviation",xlab="model",ylab="observations",cex.axis=1.5,cex.lab=1.5)
 abline(0,1)
 plot(cinter[,3],mis[,3],xlim=c(0,3),ylim=c(0,3),main="Intensity",xlab="model",ylab="observations",cex.axis=1.5,cex.lab=1.5)
 abline(0,1)
 qqplot(cinter[,2],mis[,2],xlim=c(0,25),ylim=c(0,25),main="Standard deviation",xlab="model",ylab="observations",cex.axis=1.5,cex.lab=1.5)
 abline(0,1)
 qqplot(cinter[,3],mis[,3],xlim=c(0,3),ylim=c(0,3),main="Intensity",xlab="model",ylab="observations",cex.axis=1.5,cex.lab=1.5)
 abline(0,1)
 
 plot(cinter1[,2],mis1[,2],xlim=c(0,25),ylim=c(0,25),main="Standard deviation",xlab="model",ylab="observations",cex.axis=1.5,cex.lab=1.5)
 abline(0,1)
 plot(cinter1[,3],mis1[,3],xlim=c(0,3),ylim=c(0,3),main="Intensity",xlab="model",ylab="observations",cex.axis=1.5,cex.lab=1.5)
 abline(0,1)
  qqplot(cinter1[,2],mis1[,2],xlim=c(0,25),ylim=c(0,25),main="Standard deviation",xlab="model",ylab="observations",cex.axis=1.5,cex.lab=1.5)
 abline(0,1)
 qqplot(cinter1[,3],mis1[,3],xlim=c(0,3),ylim=c(0,3),main="Intensity",xlab="model",ylab="observations",cex.axis=1.5,cex.lab=1.5)
 abline(0,1)
 
 plot(cinter2[,2],mis2[,2],xlim=c(0,25),ylim=c(0,25),main="Standard deviation",xlab="model",ylab="observations",cex.axis=1.5,cex.lab=1.5)
 abline(0,1)
 plot(cinter2[,3],mis2[,3],xlim=c(0,3),ylim=c(0,3),main="Intensity",xlab="model",ylab="observations",cex.axis=1.5,cex.lab=1.5)
 abline(0,1)
 qqplot(cinter2[,2],mis2[,2],xlim=c(0,25),ylim=c(0,25),main="Standard deviation",xlab="model",ylab="observations",cex.axis=1.5,cex.lab=1.5)
 abline(0,1)
 qqplot(cinter2[,3],mis2[,3],xlim=c(0,3),ylim=c(0,3),main="Intensity",xlab="model",ylab="observations",cex.axis=1.5,cex.lab=1.5)
 abline(0,1)

quartz()
par(mfrow=c(3,3))

taylor.diagram(mis[,1],cinter[,1],col="green",add=F)
taylor.diagram(mis[,2],cinter[,2],col="magenta",add=F)
taylor.diagram(mis[,3],cinter[,3],col="blue",add=F)

taylor.diagram(mis1[,1],cinter1[,1],col="green",add=F)
taylor.diagram(mis1[,2],cinter1[,2],col="magenta",add=F)
taylor.diagram(mis1[,3],cinter1[,3],col="blue",add=F)

taylor.diagram(mis2[,1],cinter2[,1],col="green",add=F)
taylor.diagram(mis2[,2],cinter2[,2],col="magenta",add=F)
taylor.diagram(mis2[,3],cinter2[,3],col="blue",add=F)


cintereq=cinter
cinter1eq=cinter1
cinter2eq=cinter2

# cintermc=cinter
# cinter1mc=cinter1
# cinter2mc=cinter2

# cinter22=cinter
# cinter122=cinter1
# cinter222=cinter2

# cinter=cintereq
# cinter1=cinter1eq
# cinter2=cinter2eq

quartz(height=10)
par(mfrow=c(3,2))
taylor.diagram(mis[,2],cintereq[,2],col="green",add=F)
# taylor.diagram(mis[,2],cintermc[,2],col="red",add=T)
taylor.diagram(mis[,2],cinter22[,2],col="blue",add=T)

taylor.diagram(mis[,3],cintereq[,3],col="green",add=F)
# taylor.diagram(mis[,3],cintermc[,3],col="red",add=T)
taylor.diagram(mis[,3],cinter22[,3],col="blue",add=T)

taylor.diagram(mis1[,2],cinter1eq[,2],col="green",add=F)
# taylor.diagram(mis1[,2],cinter1mc[,2],col="red",add=T)
taylor.diagram(mis1[,2],cinter122[,2],col="blue",add=T)

taylor.diagram(mis1[,3],cinter1eq[,3],col="green",add=F)
# taylor.diagram(mis1[,3],cinter1mc[,3],col="red",add=T)
taylor.diagram(mis1[,3],cinter122[,3],col="blue",add=T)

taylor.diagram(mis2[,2],cinter2eq[,2],col="green",add=F)
# taylor.diagram(mis2[,2],cinter2mc[,2],col="red",add=T)
taylor.diagram(mis2[,2],cinter222[,2],col="blue",add=T)

taylor.diagram(mis2[,3],cinter2eq[,3],col="green",add=F)
# taylor.diagram(mis2[,3],cinter2mc[,3],col="red",add=T)
taylor.diagram(mis2[,3],cinter222[,3],col="blue",add=T)

#plotto  trends
vento=read.table("statuttoconvento.txt",sep=" ",skip=1)
xsp=matrix(NA,length(vento[,1]),2)
xsp[,1]=vento[,2]/vento[,6]
#plot 1
quartz(width=12)
plot(vento[,2]/vento[,6],mis[,1],ylim=c(0,60),xlab="t",ylab=expression(italic(paste(C))))
# sp <- smooth.spline(xsp[,1],mis[,1],spar=0.8)
# lines(sp$x,sp$y)

time=vento[,2]/vento[,6]
timemean=array(dim=10)
mismean=array(dim=10)
deltat=8
punti=120/deltat
for(k in 1:punti){
	timesum=0
	missum=0
	cc=0
  tmin= (k-1)*deltat
  tmax=tmin+deltat
 for(ipar in seq(length(time))){
 	if(time[ipar] >tmin & time[ipar]<tmax ){
 #		print(c(k,ipar,time[ipar],mis[ipar,1]))
 	timesum=timesum+time[ipar]
# 	print(c(k,timemean[k],time[ipar]))
 	missum=missum+mis[ipar,1]
 	cc=cc+1
 	}
}
timemean[k]=timesum/cc
mismean[k]=missum/cc
}
lines(timemean,mismean)
points(vento[,2]/vento[,6],cinter22[,1],col='blue')
# sp <- smooth.spline(xsp[,1],cinter22[,1],spar=0.8)
# lines(sp$x,sp$y,col='blue')

for(k in 1:punti){
	timesum=0
	missum=0
	cc=0
  tmin= (k-1)*deltat
  tmax=tmin+deltat
 for(ipar in seq(length(time))){
 	if(time[ipar] >tmin & time[ipar]<tmax ){
 #		print(c(k,ipar,time[ipar],mis[ipar,1]))
 	timesum=timesum+time[ipar]
# 	print(c(k,timemean[k],time[ipar]))
 	missum=missum+cinter22[ipar,1]
 	cc=cc+1
 	}
}
timemean[k]=timesum/cc
mismean[k]=missum/cc
}
lines(timemean,mismean,col="blue")
#plot 2
quartz(width=12)
plot(vento[,2]/vento[,6],cinter22[,2],ylim=c(0,30),xlab="t",,col='blue',ylab=expression(italic(paste(sigma[c]))))
# sp <- smooth.spline(xsp[,1],cinter22[,2],spar=0.8)
# lines(sp$x,sp$y,col='blue')
for(k in 1:punti){
	timesum=0
	missum=0
	cc=0
  tmin= (k-1)*deltat
  tmax=tmin+deltat
 for(ipar in seq(length(time))){
 	if(time[ipar] >tmin & time[ipar]<tmax ){
 #		print(c(k,ipar,time[ipar],mis[ipar,1]))
 	timesum=timesum+time[ipar]
# 	print(c(k,timemean[k],time[ipar]))
 	missum=missum+cinter22[ipar,2]
 	cc=cc+1
 	}
}
timemean[k]=timesum/cc
mismean[k]=missum/cc
}
lines(timemean,mismean,col="blue")
points(vento[,2]/vento[,6],cintermc[,2],col='red')
# sp <- smooth.spline(xsp[,1],cintermc[,2],spar=0.8)
# lines(sp$x,sp$y,col='red')
for(k in 1:punti){
	timesum=0
	missum=0
	cc=0
  tmin= (k-1)*deltat
  tmax=tmin+deltat
 for(ipar in seq(length(time))){
 	if(time[ipar] >tmin & time[ipar]<tmax ){
 #		print(c(k,ipar,time[ipar],mis[ipar,1]))
 	timesum=timesum+time[ipar]
# 	print(c(k,timemean[k],time[ipar]))
 	missum=missum+cintermc[ipar,2]
 	cc=cc+1
 	}
}
timemean[k]=timesum/cc
mismean[k]=missum/cc
}
lines(timemean,mismean,col="red")
points(vento[,2]/vento[,6],cintereq[,2],col='green')
# sp <- smooth.spline(xsp[,1],cintereq[,2],spar=0.8)
# lines(sp$x,sp$y,col='green')
for(k in 1:punti){
	timesum=0
	missum=0
	cc=0
  tmin= (k-1)*deltat
  tmax=tmin+deltat
 for(ipar in seq(length(time))){
 	if(time[ipar] >tmin & time[ipar]<tmax ){
 #		print(c(k,ipar,time[ipar],mis[ipar,1]))
 	timesum=timesum+time[ipar]
# 	print(c(k,timemean[k],time[ipar]))
 	missum=missum+cintereq[ipar,2]
 	cc=cc+1
 	}
}
timemean[k]=timesum/cc
mismean[k]=missum/cc
}
lines(timemean,mismean,col="green")

points(vento[,2]/vento[,6],mis[,2])
# sp <- smooth.spline(xsp[,1],mis[,2],spar=0.8)
# lines(sp$x,sp$y)
for(k in 1:punti){
	timesum=0
	missum=0
	cc=0
  tmin= (k-1)*deltat
  tmax=tmin+deltat
 for(ipar in seq(length(time))){
 	if(time[ipar] >tmin & time[ipar]<tmax ){
 #		print(c(k,ipar,time[ipar],mis[ipar,1]))
 	timesum=timesum+time[ipar]
# 	print(c(k,timemean[k],time[ipar]))
 	missum=missum+mis[ipar,2]
 	cc=cc+1
 	}
}
timemean[k]=timesum/cc
mismean[k]=missum/cc
}
lines(timemean,mismean)
#plot 3
quartz(width=12)
plot(vento[,2]/vento[,6],mis[,3],ylim=c(0,5),xlab="t",ylab=expression(italic(paste(i[c]))))
# sp <- smooth.spline(xsp[,1],mis[,3],spar=0.8)
# lines(sp$x,sp$y)
for(k in 1:punti){
	timesum=0
	missum=0
	cc=0
  tmin= (k-1)*deltat
  tmax=tmin+deltat
 for(ipar in seq(length(time))){
 	if(time[ipar] >tmin & time[ipar]<tmax ){
 #		print(c(k,ipar,time[ipar],mis[ipar,1]))
 	timesum=timesum+time[ipar]
# 	print(c(k,timemean[k],time[ipar]))
 	missum=missum+mis[ipar,3]
 	cc=cc+1
 	}
}
timemean[k]=timesum/cc
mismean[k]=missum/cc
}
lines(timemean,mismean)

points(vento[,2]/vento[,6],cintermc[,3],col='red')
# sp <- smooth.spline(xsp[,1],cintermc[,3],spar=0.8)
# lines(sp$x,sp$y,col='red')
for(k in 1:punti){
	timesum=0
	missum=0
	cc=0
  tmin= (k-1)*deltat
  tmax=tmin+deltat
 for(ipar in seq(length(time))){
 	if(time[ipar] >tmin & time[ipar]<tmax ){
 #		print(c(k,ipar,time[ipar],mis[ipar,1]))
 	timesum=timesum+time[ipar]
# 	print(c(k,timemean[k],time[ipar]))
 	missum=missum+cintermc[ipar,3]
 	cc=cc+1
 	}
}
timemean[k]=timesum/cc
mismean[k]=missum/cc
}
lines(timemean,mismean,col="red")

points(vento[,2]/vento[,6],cintereq[,3],col='green')
# sp <- smooth.spline(xsp[,1],cintereq[,3],spar=0.8)
# lines(sp$x,sp$y,col='green')
for(k in 1:punti){
	timesum=0
	missum=0
	cc=0
  tmin= (k-1)*deltat
  tmax=tmin+deltat
 for(ipar in seq(length(time))){
 	if(time[ipar] >tmin & time[ipar]<tmax ){
 #		print(c(k,ipar,time[ipar],mis[ipar,1]))
 	timesum=timesum+time[ipar]
# 	print(c(k,timemean[k],time[ipar]))
 	missum=missum+cintereq[ipar,3]
 	cc=cc+1
 	}
}
timemean[k]=timesum/cc
mismean[k]=missum/cc
}
lines(timemean,mismean,col="green")

points(vento[,2]/vento[,6],cinter22[,3],col='blue')
# sp <- smooth.spline(xsp[,1],cinter22[,3],spar=0.8)
# lines(sp$x,sp$y,col='blue')
for(k in 1:punti){
	timesum=0
	missum=0
	cc=0
  tmin= (k-1)*deltat
  tmax=tmin+deltat
 for(ipar in seq(length(time))){
 	if(time[ipar] >tmin & time[ipar]<tmax ){
 #		print(c(k,ipar,time[ipar],mis[ipar,1]))
 	timesum=timesum+time[ipar]
# 	print(c(k,timemean[k],time[ipar]))
 	missum=missum+cinter22[ipar,3]
 	cc=cc+1
 	}
}
timemean[k]=timesum/cc
mismean[k]=missum/cc
}
lines(timemean,mismean,col="blue")




#calcolo R90
ksp=(1/mis[,3])^1.086
lambda=gamma(1+1/ksp)/mis[,1]
R90=(-log(0.1))^(1/ksp)/(lambda*mis[,1])

ksp=(1/cinter22[,3])^1.086
lambda=gamma(1+1/ksp)/cinter22[,1]
R9022=(-log(0.1))^(1/ksp)/(lambda*cinter22[,1])

ksp=(1/cintereq[,3])^1.086
lambda=gamma(1+1/ksp)/cintereq[,1]
R90eq=(-log(0.1))^(1/ksp)/(lambda*cintereq[,1])

ksp=(1/cintermc[,3])^1.086
lambda=gamma(1+1/ksp)/cintermc[,1]
R90mc=(-log(0.1))^(1/ksp)/(lambda*cintermc[,1])

R90C=R90mc

meanM=mean(R90,na.rm=TRUE)
meanC=mean(R90C,na.rm=TRUE)
FB=(meanM-meanC)/(meanC+meanM)*2
NMSE=mean((R90-R90C)^2,na.rm=TRUE)/meanC/meanM
R=cor(R90,R90C, use="pairwise.complete.obs")
maxM=max(R90,na.rm=TRUE)
maxC=max(R90C,na.rm=TRUE)
write.table(t(c(meanM,meanC,maxM,maxC,FB,NMSE,R)))
