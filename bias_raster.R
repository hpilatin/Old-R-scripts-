library(scales)
library(ncdf4)
library(geosphere)
library(tdr)
setwd("C:/Users/pilat/Desktop/Topsis_heves/")
load("observation_data.Rdata")
library(readxl)
hevesfolder<-"C:/Users/pilat/Desktop/Topsis_heves/"
inname<-paste(sep='',hevesfolder, 'extreme events.xlsx')
sheet_num<-6 ####SHEET NUMBER IS IMPORTANT!!!!!!!!!!
ext_events_med<-as.data.frame(read_excel(inname, skip=0,sheet=sheet_num,col_names =TRUE))   ####SHEET NUMBER IS IMPORTANT!!!!!!!!!!
v<- 24 #### the program start how many hours before the event day (for ex. to start 3 day before 3*24=72
zz<- 49 #### the program finish how many hours after the event day. Also event day data (24 data) are included.
###### So if you would like to finish 3 days after the event day write (3*24+24=96)
a<-numeric(length(med_sta))
ava_sta_med<-matrix(NA,length(ext_events_med[,1]),1)
k<-63 #### Starting row of the event in the excel file
na_bound<-0.05
rainsta1<-matrix(NA,v+zz,length(med_sta))
u<- as.numeric(ext_events_med[(k-1),1]) ####  starting row of the events. You can find these values from excel file
for (i in 1:length(med_sta)) {
  y_pr<-sta_precip_med[(u-v+1):(u+zz),i] #### +1 in (u+zz+1) is just for manupulating x axes labels. for preparing data for model take (u+zz)
  ### IF more than 5% of them are NA then do not take this station.
  rainsta1[,i]<-y_pr
  a[i]<-length(y_pr[is.na(y_pr)])/length(y_pr)
  ava_sta_med<-length(which(a<na_bound))
}
rainsta<-rainsta1[,which(a<na_bound)]
coorsta<-sub_coor[med_sta[which(a<na_bound)],]
colnames(rainsta)<-coorsta[,1]
S<-24 ## scenerio number in domain 1
RAINFALL<-array(NA,c(nrow(rainsta),ncol(rainsta),S))
deneme<-matrix(NA,dim(rainsta)[1],S)


for (ss in 1:S)
{
  #### you need to name your file like wrfout_<domain>_<starting time>.nc
  ####(ex.wrfout_d01_2016-08-31.nc)
  a<-"02"   #### write your domain number
  b<-"2017-11-26" ## write your starting day of WRF your data
  setwd("E:/Heves/2017-11-26")
  nc<-nc_open(paste((2*ss-1),"_",(2*ss),"_","wrfout_","d",a,"_",b,"",sep=""))
  rm(a,b)
  con<-ncvar_get (nc,varid="RAINC")
  ncon<-ncvar_get (nc,varid="RAINNC")
  rainfall_cum<-ncon+con
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
  rainfall<-array(NA,c(dim(rainfall_cum)))
  dumm<-matrix(NA,dim(rainfall_cum)[2],dim(rainfall_cum)[3])
  a<-numeric(dim(lat_dum)[3])
  for (i in 1:dim(rainfall_cum)[1])
  {
    for (j in 1: dim(rainfall_cum)[2])
    {
      a<-c(rainfall_cum[i,j,1],diff(rainfall_cum[i,j,]))
      dumm[j,]<-a
    }
    rainfall[i,,]<-dumm
  }
  rm(dumm,a,i,j)
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
  for ( i in 1:ncol(rainsta))
  {
    RAINFALL[,i,ss]<-rainfall[min_dist[i,1],min_dist[i,2],]
  }
  deneme[,ss]<- c(rowMeans(RAINFALL[,,ss]))
}
mean<-rowMeans(deneme)
df<-data.frame(RAINFALL)
b<-rowMeans(rainsta,na.rm=TRUE)
matplot(deneme,type="l",lty=2,col="gray",xlab='Hours',ylab='Rainfall(mm)',main='GFS Scenarios Comparison for Domain 2 on 2017/11/26',lwd=1)
lines(mean,type="l",col="red",lwd=2)
lines(b,type="l",col="blue",lwd=2)
legend("topleft", legend=c( "mean of wrfout scenarios","wrfout scenarios","mean of observation stations"),col=c("red","gray", "blue"), lty=1:2, cex=0.8,text.font=2)




S<-24
bias<-matrix(NA,dim(rainsta)[1],S)
for (ss in 1 :S)
{
  a<-c(rowMeans(gfs[,,ss]))
  b<-rowMeans(rainsta,na.rm=TRUE)
  for (i in 1:73)
  {
    bias[i,ss]<- (a[i]-b[i]) / 73
  }
}

OR 

S<-24
bias<-matrix(NA,dim(rainsta)[1],S)
for (ss in 1 :S)
{
  a<-c(rowMeans(gfs[,,ss]))
  b<-rowMeans(rainsta,na.rm=TRUE)
  bias[,ss]<- (a-b)
}



X<-48
fark<-matrix(NA,dim(rainsta)[1],X)
for (ss in 1 :X)
{
  c<-c(rowMeans(RAINFALL_d1[,,ss]))
  d<-rowMeans(rainsta,na.rm=TRUE)
  for (i in 1:73)
  {
    fark[i,ss]<- (c[i]-d[i]) 
  }
}

bias<-matrix(NA,1,X)
for (ss in 1 :X)
{
  bias[ss]<-c(mean(fark[,ss]))
}

library(ggplot2)

labels=c("S1", "S2","S3","S4","S5","S6","S7","S8","S9","S10","S11","S12","S13","S14","S15","S16","S17","S18","S19","S20","S21","S22","S23","S24", "S25", "S26", "S27", "S28", "S29", "S30", "S31", "S32", "S33", "S34", "S35", "S36", "S37", "S38", "S39", "S40", "S41", "S42", "S43", "S44", "S45", "S46", "S47", "S48")

barplot(bias, col = c("red","black"),ylim= c(0,0.5),beside = TRUE, xlab = "Scenario numbers", ylab= "Mean Bias (mm)", cex.axis = 1, names.arg = c(labels),cex.lab =1.2)
legend("topleft", c("a) Domain 2"), cex = 1, text.font=2.5)
legend("topright", c("GFS","ERA5"), cex = 1, fill = c("red","black"))

;;; Bias raster for GFS - ERA5 datasets;;;

library(dplyr)
library(lubridate)
library(fields)
library(magrittr)
library(colorspace)
hcl_palettes(plot = TRUE)
colors<-diverge_hsv(10)

hours<-c("0","12h","24h","36h","48h","60h","72h")

G<-24
fark<-matrix(NA,dim(rainsta)[1],G)
for (ss in 1 :G)
{
  c<-c(rowMeans(gfs[,,ss]))
  d<-rowMeans(rainsta,na.rm=TRUE)
  for (i in 1:73)
  {
    fark[i,ss]<- (c[i]-d[i]) 
  }
}
glabels=c("S1", "S2","S3","S4","S5","S6","S7","S8","S9","S10","S11","S12","S13","S14","S15","S16","S17","S18","S19","S20","S21","S22","S23","S24")

image.plot(1:73,seq(from=1, to=24, by = 1),fark,axes=FALSE, 
           xlab = "Run Hours", ylab = " GFS Scenarios", col = colors,horizontal = FALSE,legend.only=FALSE,legend.shrink = 1,midpoint = TRUE, cex.lab =1.2, legend.args=list(text='Bias (mm)', side=3, font=1, line= 1, cex=1.2))

axis(side=2, seq(1, 24, by = 1), labels=glabels, las=1, cex.axis=0.9, tck = 0)

axis(side=1, seq(0, 72, by = 12), las=1, cex.axis=1, label=hours, tck = 0)


E<-24
farke<-matrix(NA,dim(rainsta)[1],E)
for (ss in 1 :E)
{
  ce<-c(rowMeans(era[,,ss]))
  de<-rowMeans(rainsta,na.rm=TRUE)
  for (i in 1:73)
  {
    farke[i,ss]<- (ce[i]-de[i]) 
  }
}
elabels=c("S25", "S26", "S27", "S28", "S29", "S30", "S31", "S32", "S33", "S34", "S35", "S36", "S37", "S38", "S39", "S40", "S41", "S42", "S43", "S44", "S45", "S46", "S47", "S48")


image.plot(1:73,seq(from=1, to=24, by = 1),farke,axes=FALSE, 
           xlab = "Run Hours", ylab = " ERA5 Scenarios", col = colors,horizontal = FALSE,legend.only=FALSE,legend.shrink = 1,midpoint = TRUE, cex.lab =1.2, legend.args=list(text='Bias (mm)', side=3, font=1, line= 1, cex=1.2))

axis(side=2, seq(1, 24, by = 1), labels=elabels, las=1, cex.axis=0.9, tck = 0)

axis(side=1, seq(0, 72, by = 12), las=1, cex.axis=1, label=hours, tck = 0)

;;; Bias raster for SST;;;

library(dplyr)
library(lubridate)
library(fields)
library(magrittr)
library(colorspace)
hcl_palettes(plot = TRUE)
colors<-diverge_hsv(10)

hours<-c("0","24h","48h","72h", "96h","120h","144h","168h","192h","216h","240h")

G<-4
fark<-matrix(NA,dim(rainsta)[1],G)
for (ss in 1 :G)
{
  c<-c(rowMeans(RAINFALL_d1_ebls[,,ss]))
  d<-rowMeans(rainsta,na.rm=TRUE)
  for (i in 1:241)
  {
    fark[i,ss]<- (c[i]-d[i]) 
  }
}
glabels=c("ERA5", "GHRSST","MEDS","NCEP")

image.plot(1:241,seq(from=1, to=4, by = 1),fark,axes=FALSE, 
           xlab = "Run Hours", ylab = " ", col = colors,horizontal = FALSE,legend.only=FALSE,legend.shrink = 1,midpoint = TRUE, cex.lab =1.2, legend.args=list(text='Bias (mm)', side=3, font=1, line= 1, cex=1.2))

axis(side=2, seq(1, 4, by = 1), labels=glabels, las=1, cex.axis=0.9, tck = 0)

axis(side=1, seq(0, 240, by = 24), las=1, cex.axis=1, label=hours, tck = 0)


library(rgdal)
library(raster)
library(ggplot2)
library(reshaped2)

kare<-raster(bias,  xmn=0, xmx=73, ymn=1, ymx=24)
kare_df = as.data.frame(kare, xy=TRUE)

ggplot(kare_df,breaks=c(-1,-0.75,-0.7,-0.55,-0.5,-0.25,-0.2,0,0.2,0.25,0.5,0.55,0.7,0.75,1),height = "1000px",size= 5,at = breaks) +
  geom_raster(aes(x, y, fill=layer)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1))

