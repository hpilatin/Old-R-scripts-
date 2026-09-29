library(scales)
library(ncdf4)
library(geosphere)
library(tdr)

setwd("/home/pilatin/Desktop/Heves")
load("observation_data.Rdata")

library(readxl)
hevesfolder<- "/home/pilatin/Desktop/Heves/"
inname<-paste(sep='',hevesfolder, 'extreme events.xlsx') 
sheet_num<-5####SHEET NUMBER IS IMPORTANT!!!!!!!!!!
ext_events_ebls<-as.data.frame(read_excel(inname, skip=0,sheet=sheet_num,col_names =TRUE))   ####SHEET NUMBER IS IMPORTANT!!!!!!!!!!


v<- 21 #### the program start how many hours before the event day (for ex. to start 3 day before 3*24=72
zz<- 52 #### the program finish how many hours after the event day. Also event day data (24 data) are included.
###### So if you would like to finish 3 days after the event day write (3*24+24=96)


a<-numeric(length(ebls_sta))
ava_sta_ebls<-matrix(NA,length(ext_events_ebls[,1]),1)
k<-95 #### Starting row of the event in the excel file
na_bound<-0.05

rainsta1<-matrix(NA,v+zz,length(ebls_sta))
u<- as.numeric(ext_events_ebls[(k-1),1]) ####  starting row of the events. You can find these values from excel file

for (i in 1:length(ebls_sta)) {
y_pr<-sta_precip_ebls[(u-v+1):(u+zz),i] #### +1 in (u+zz+1) is just for manupulating x axes labels. for preparing data for model take (u+zz)
### IF more than 5% of them are NA then do not take this station.
rainsta1[,i]<-y_pr
a[i]<-length(y_pr[is.na(y_pr)])/length(y_pr)

ava_sta_ebls<-length(which(a<na_bound))

}
rainsta<-rainsta1[,which(a<na_bound)]
coorsta<-sub_coor[ebls_sta[which(a<na_bound)],]

colnames(rainsta)<-coorsta[,1]

subset(main_data,main_data[,1]==18221 & main_data[,3]==2017 & main_data[,4]==8 & main_data[,5]==14)  ### this is just for check



#### you need to name your file like wrfout_<domain>_<starting time>.nc 
####(ex.wrfout_d01_2016-08-31.nc)
a<-"02"   #### write your domain number
b<-"2017-08-14" ## write your starting day of WRF your data
setwd(paste("E:/",b,sep=""))#####ADJUST YOUR WORKING DIRECTORY TO WHERE YOU STORE THE WRF OUTPUT######
nc<-nc_open(paste("7_8_","wrfout_","d",a,"_",b,".nc",sep=""))
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


### check for result
##a<-100
##b<-97
##all(rainfall[a,b,]==c(rainfall_cum[a,b,1],diff(rainfall_cum[a,b,])))




##################################################################################################
distt<-matrix(NA,dim(lat)[1],dim(lat)[2])														 #
min_dist<-matrix(NA,dim(rainsta)[2],2)  ## row and column number of closest grid for each station#
for (i in 1:dim(rainsta)[2])																	 #
{																								 #
for (j in 1:length(lat))																		 #
{																								 #
distt[j]<-distm(coorsta[i,2:3],c(long[j],lat[j]))												 #
}																								 #
min_dist[i,]<-which(distt==min(distt),arr.ind=TRUE)												 #
}																								 #
##################################################################################################

####METRICS    
# CSI, POD and FAR are used extensively by the National Weather Service to verify severe thunderstorm and tornado warnings.

#A: Number of Correct Detection == length(intersect(c,d))
#B: Number of False Alarms == length(setdiff(d,c))
#C: Number of Misses 
#D: Number of Correct Negative

#1-)PROBABILITY OF DETECTION

POD<-numeric(dim(rainsta)[2]) #### A/(A+C)

 for( i in 1:dim(rainsta)[2])
{
 a<-rainsta[,i]
 b<-rainfall[min_dist[i,1],min_dist[i,2],]
 c<-which(a>0.1)
 d<-which(b>0.1)
 POD[i]<- length(intersect(c,d))/length(c)
 }

rm(a,b,c,d)

#2-)FALSE ALARM RATIO

FAR<-numeric(dim(rainsta)[2])  #### B/(A+B)

for( i in 1:dim(rainsta)[2])
{
a<-rainsta[,i]
 b<-rainfall[min_dist[i,1],min_dist[i,2],]
 c<-which(a>0.1)
 d<-which(b>0.1)
 FAR[i]<-length(setdiff(d,c))/(length(setdiff(d,c))+length(intersect(c,d)))
 }
rm(a,b,c,d)

#3-)CRITICAL SUCCESS INDEX or THREAT SCORE

CSI<-numeric(dim(rainsta)[2])  #### A/(A+B+C)

for( i in 1:dim(rainsta)[2])
{
a<-rainsta[,i]
 b<-rainfall[min_dist[i,1],min_dist[i,2],]
 c<-which(a>0.1)
 d<-which(b>0.1)
 CSI[i]<-length(intersect(c,d))/(length(c)+length(setdiff(d,c)))
 }
rm(a,b,c,d)

#4-)PERCENT CORRECT


PC<-numeric(dim(rainsta)[2])  #### (A+D)/TOTAL EVENT

for( i in 1:dim(rainsta)[2])
{
a<-rainsta[,i]
 b<-rainfall[min_dist[i,1],min_dist[i,2],]
 c<-which(a>0.1)
 d<-which(b>0.1)
 e<-which(a<0.1)
 f<-which(b<0.1)
 PC[i]<-(length(intersect(c,d))+length(intersect(e,f)))/length(a)
 }
rm(a,b,c,d)



#5-)FREQUENCY BIAS INDEX

FBI<-numeric(dim(rainsta)[2])  #### (A+B)/(A+C)

for( i in 1:dim(rainsta)[2])
{
a<-rainsta[,i]
 b<-rainfall[min_dist[i,1],min_dist[i,2],]
 c<-which(a>0.1)
 d<-which(b>0.1)
  if(all(a<=0.1,na.rm=TRUE))
  {
  FBI[i]<-NA
  }
  else{
 FBI[i]<-(length(setdiff(d,c))+length(intersect(c,d)))/length(c)
 }
 }
rm(a,b,c,d)


#6-)ROOT MEAN SQUARE ERROR


RMSE<-numeric(dim(rainsta)[2])

for( i in 1:dim(rainsta)[2])
{
a<-rainsta[,i]
 b<-rainfall[min_dist[i,1],min_dist[i,2],]
 RMSE[i]<-sqrt(mean((a - b)^2,na.rm=TRUE))
 }
rm(a,b)


#7-) MEAN BIAS ERROR   (mean(a-b))

 MBE<-numeric(dim(rainsta)[2])
 
 
 for( i in 1:dim(rainsta)[2])
 {
 a<-rainsta[,i]
  b<-rainfall[min_dist[i,1],min_dist[i,2],]
  MBE[i]<-tdStats(a,b,functions="mbe")
  }

#8-) STANDARD DEVIATION

SD<-numeric(dim(rainsta)[2])


for( i in 1:dim(rainsta)[2])
{
a<-rainsta[,i]
 b<-rainfall[min_dist[i,1],min_dist[i,2],]
 if(all(a<=0.1,na.rm=TRUE) | all(b<=0.1,na.rm=TRUE))
 {
 SD[i]<-NA
 }
  else {
 SD[i]<-sd(a,na.rm=TRUE)/sd(b,na.rm=TRUE)
 }
 }
rm(a,b)


#9-) Correlation


 CORR<-numeric(dim(rainsta)[2])
 for( i in 1:dim(rainsta)[2])
 {
 a<-rainsta[,i]
 b<-rainfall[min_dist[i,1],min_dist[i,2],]
 if(all(a<=0.1,na.rm=TRUE) | all(b<=0.1,na.rm=TRUE))
 {
 CORR[i]<-NA
 }
 else 
 {CORR[i]<-cor(a,b,use="na.or.complete")}
 }
rm(a,b)


#####   RESCALE OF FBI,MBE AND SD

#1-)FBI

FBI_RES<-FBI
FBI_RES[FBI>2 & !is.na(FBI)]<-0
FBI_RES[FBI<=2 & FBI>1 & !is.na(FBI)]<-2-FBI_RES[FBI<=2 & FBI>1 & !is.na(FBI)]

#2-) MBE
MBE_RES<-abs(MBE)

#3-) SD
SD_RES<-SD
SD_RES[SD>2 & !is.na(SD)]<-0
SD_RES[SD<=2 & SD>1 & !is.na(SD)]<-2-SD_RES[SD<=2 & SD>1 & !is.na(SD)]


a<-c(mean(POD,na.rm=TRUE),mean(FAR,na.rm=TRUE),mean(CSI,na.rm=TRUE),mean(PC),mean(FBI_RES,na.rm=TRUE),mean(RMSE),mean(MBE_RES),mean(SD_RES,na.rm=TRUE),mean(CORR,na.rm=TRUE))
metrics<-matrix(a,1,length(a),byrow=TRUE)
rm(a)

 colnames(metrics)<-c("POD","FAR","CSI","PC","FBI","RMSE","MBE","SD","CORR")
 
 
