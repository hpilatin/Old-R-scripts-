library(scales)
library(ncdf4)
library(geosphere)
library(tdr)

setwd("C:/Users/pilat/Desktop/Topsis_heves/")
load("observation_data.Rdata")

library(readxl)
hevesfolder<-"C:/Users/pilat/Desktop/Topsis_heves/"
inname<-paste(sep='',hevesfolder, 'extreme events.xlsx') 
sheet_num<-6####SHEET NUMBER IS IMPORTANT!!!!!!!!!!
ext_events_med<-as.data.frame(read_excel(inname, skip=0,sheet=sheet_num,col_names =TRUE))   ####SHEET NUMBER IS IMPORTANT!!!!!!!!!!


v<- 24  #### the program start how many hours before the event day (for ex. to start 3 day before 3*24=72
zz<- 49  #### the program finish how many hours after the event day. Also event day data (24 data) are included.
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
performance<-matrix(NA,48,9) ## matrix(NA,scenerio number, metric number)
RAINFALL<-array(NA,c(nrow(rainsta),ncol(rainsta),S))

for (ss in 1:S)
  
{
  #### you need to name your file like wrfout_<domain>_<starting time>.nc 
  ####(ex.wrfout_d01_2016-08-31.nc)
  a<-"01"   #### write your domain number
  b<-"2017-11-26" ## write your starting day of WRF your data
  setwd(paste("E:/Heves/",b,sep=""))#####ADJUST YOUR WORKING DIRECTORY TO WHERE YOU STORE THE WRF OUTPUT######
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
    RAINFALL[,i,ss]<-rainfall[min_dist[i,1],min_dist[i,2],]
  }
}
