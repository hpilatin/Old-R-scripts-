library(scales)
library(ncdf4)
library(geosphere)
library(tdr)
library(humidity)

setwd("C:/Users/pilat/Desktop/Topsis_heves/observation_data2/")
load("obs_wind2.Rdata")
rainsta<-matrix(NA,241,length(med_sta))
a<-numeric(length(med_sta))

for (i in 1:length(med_sta)) {
  y_pr<-sta_wind_med[1:241,i]
  rainsta[,i]<-y_pr
  
}
coorsta<-sub_coor[,]
colnames(rainsta)<-coorsta[,1]
rainsta[is.na(rainsta)] <- 0

S<-4
RAINFALL_d1_med<-array(NA,c(nrow(rainsta),ncol(rainsta),S))
deneme<-matrix(NA,dim(rainsta)[1],S)
for (ss in 1:S)
  
{
  a<-"01"   #### write your domain number
  b<-"2018-12-10"
  setwd("G:/sst_analysis_completed/med_sst")
  nc<-nc_open(paste((2*ss-1),"_",(2*ss),"_","wrfout_","d",a,"_",b,"",sep=""))
  rm(a,b)
  u<-ncvar_get (nc,varid="U10")
  v<- ncvar_get (nc,varid="V10")
  rainfall<-sqrt((u^2)+(v^2))
  
  lat_dum<-ncvar_get (nc,varid="XLAT")
  long_dum<-ncvar_get (nc,varid="XLONG")
  
  
  long<-matrix(NA,dim(lat_dum)[1],dim(lat_dum)[2])
  a<-numeric(dim(lat_dum)[2])
  for(i in 1:dim(lat_dum)[1])
  {
    for (j in 1:dim(lat_dum)[2])
    {
      if(length(unique(long_dum[i,j,]))==1)
      {a[j]<-unique(long_dum[i,j,])}
      else 
      {a[j]<-mean(long_dum[i,j,])}
    }
    long[i,]<-a
  }
  
  lat<-matrix(NA,dim(lat_dum)[1],dim(lat_dum)[2])
  a<-numeric(dim(lat_dum)[2])
  for(i in 1:dim(lat_dum)[1])
  {
    for (j in 1:dim(lat_dum)[2])
    {
      if(length(unique(lat_dum[i,j,]))==1) 
      {a[j]<-unique(lat_dum[i,j,])}
      else 
      {a[j]<-mean(lat_dum[i,j,])}
    }
    lat[i,]<-a
  }
  
  if(ss==1) {
    distt<-matrix(NA,dim(lat)[1],dim(lat)[2])														 #
    min_dist<-matrix(NA,dim(rainsta)[2],2)  ## row and column number of closest grid for each station#
    for (i in 1:dim(rainsta)[2])																	 #
    {																								 #
      for (j in 1:length(lat))																		 #
      {																								 #
        distt[j]<-distm(coorsta[i,2:3],c(long[j],lat[j]))												 #
      }																								 #
      min_dist[i,]<-which(distt==min(distt),arr.ind=TRUE)												 #
    }																					
    
  }
  
  else
  {print(ss)}
  for ( i in 1:ncol(rainsta))
  {
    RAINFALL_d1_med[,i,ss]<-rainfall[min_dist[i,1],min_dist[i,2],]
  }
}

dates <- c("2018/12/10", "2018/12/11", "2018/12/12", "2018/12/13", "2018/12/14", "2018/12/15", "2018/12/16", "2018/12/17", "2018/12/18", "2018/12/19","2018/12/20" )
wrfs<-RAINFALL_d1_med[,,1]
ghrs<-RAINFALL_d1_med[,,2]
meds<-RAINFALL_d1_med[,,3]
nceps<-RAINFALL_d1_med[,,4]
wrf<-rowMeans(wrfs)
ghr<-rowMeans(ghrs)
med<-rowMeans(meds)
ncep<-rowMeans(nceps)
Obs<-rowMeans(rainsta,na.rm=TRUE)

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
par(pty ="m", mar=c(2.5,4,2,0.5),mfrow=c(2,2))

A<-plot(wrf,type="l",lty=1,col="purple",xlab='Dates',ylab='Wind Speed at 10 m (m/s)',lwd=3,xaxt='n',ylim=c(0,10),cex.lab=1.3, cex.axis = 1.3)
lines(Obs,type="l",col="black",lwd=3, lty=1)
axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.3)


legend("topleft",c(paste("Correlation=",round(cor(wrf,Obs,method = "pearson"),2),sep=""), paste("RMSE=",round(RMSE(wrf,Obs),2),sep="") ), cex=1,text.font=2)


B<-plot(ghr,type="l",lty=1,col="green",xlab='Dates',ylab='',lwd=3,xaxt='n',ylim=c(0,10),cex.lab=1.3, cex.axis = 1.3)
lines(Obs,type="l",col="black",lwd=3, lty=1)

axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.3)

legend("topleft",c(paste("Correlation=",round(cor(ghr,Obs,method = "pearson"),2),sep=""), paste("RMSE=",round(RMSE(ghr,Obs),2),sep="") ), cex=1,text.font=2)

C<-plot(med,type="l",lty=1,col="red",xlab='Dates',ylab='Wind Speed at 10 m (m/s)',lwd=3,xaxt='n',ylim=c(0,10),cex.lab=1.3, cex.axis = 1.3)
lines(Obs,type="l",col="black",lwd=3, lty=1)
axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.3)
legend("topleft",c(paste("Correlation=",round(cor(med,Obs,method = "pearson"),2),sep=""), paste("RMSE=",round(RMSE(med,Obs),2),sep="") ), cex=1,text.font=2)

D<-plot(ncep,type="l",lty=1,col="blue",xlab='Dates',ylab='',lwd=3,xaxt='n',ylim=c(0,10),cex.lab=1.3, cex.axis = 1.3)
lines(Obs,type="l",col="black",lwd=3, lty=1)

axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.3)
legend("topleft",c(paste("Correlation=",round(cor(ncep,Obs,method = "pearson"),2),sep=""), paste("RMSE=",round(RMSE(ncep,Obs),2),sep="") ), cex=1,text.font=2)


