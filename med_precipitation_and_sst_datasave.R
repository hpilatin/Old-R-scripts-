library(xlsx)
setwd("C:/Users/pilat/Downloads/")
hevesfolder<-"C:/Users/pilat/Downloads/"
inname<-paste(sep='',hevesfolder, 'f.txt') 
main_data<-read.table(inname, sep='|', skip=0,header = TRUE)
sta<-main_data[,1]
names<-main_data[,2]
YMDH<-main_data[,3:6] ####YEAR-MONTH-DAY-HOUR#####
PP<-main_data[,7]
PP[ is.na(PP)] #####check for NA data#####
usta<-sort(unique(sta)) ##### ID of unique stations#####
stanum<-length(usta)##### number of stations#####
dum01 = (1:stanum) 
NN<-length(PP)
ymax<-max(YMDH[,1])
ymin<-min(YMDH[,1])
yearnum<-1
daynum<- 30
daysize= 11
h<-length(unique(YMDH[,4])) #### data number in each day

precip1 = array(NA, dim=c(stanum,yearnum,daynum,h))

for(i in 1:NN) {
  READYEAR = YMDH[i,1]
  READMON  = YMDH[i,2]
  READDAY  = YMDH[i,3]
  READHOUR = YMDH[i,4]
  staloc = dum01[ usta == sta[i] ] #####  [ which number of usta equal to sta[i]]
  DOY = READDAY
  
  
  yyyy = READYEAR - ymin + 1
  hour<-READHOUR +1 
  {  
    precip1[staloc, yearnum, DOY, hour] = PP[i]
  }
  if( (i %% 1e5) == 0 ) { print( (round(i/NN*100)/100) ) }	## this line is added just to print out the progress of the for loop over the screen because the for loop takes too long time
}

d<-precip1[,1,c(10:20),]
precip = array(NA, dim=c(stanum,1,11,h))
for ( i in 1:11)
{
  precip[,,i,]<-d[,i,]
}

library(rgdal)
library(rgeos)
library(sp)
basins = readOGR(dsn=path.expand('D:/wrf_data_cases/Havzalar'),layer="Havzalar")  
hevesfolder<-"C:/Users/pilat/Downloads/"
inname<-paste(sep='',hevesfolder, 'station_med.txt')
coordinates<-read.table(inname, sep='|', skip=0,header = TRUE)
sub_coor<-matrix(NA,length(usta),3) 



for (i in 1:length(usta)) {
  a <- which(usta[i] == coordinates[,1])
  k<-as.matrix(coordinates[a,c(1,6,5)])
  sub_coor[i,]<-k
}


daynum<-11

dat<-data.frame(x=sub_coor[,2],y=sub_coor[,3])
coordinates(dat) <- ~ x+y
proj4string(dat) <- proj4string(basins) ### tell R that dat coordinates are in the same lat/lon reference system with basins
over(dat, as(basins, "SpatialPolygons")) #### over function shows that which points belong to which spatial object.
#### By doing this we saw that there are some stations out of Turkey (NA values). 
########It is due to shape file. We should check these stations if they are in or out of med or ebls regions

med_sta<-1:length(usta)
med_staa<- med_sta
daynum<-11
sty<-ymin ###### start year of available data
yyy<-ymax+1 ###### (last year of available data) + 1 
tyear<-1 ####### total data year
dat_num<-daynum*h ######## data number in each year
tdat<-dat_num*tyear ######## total data
tday<-daynum*tyear ###total day length
yl<-matrix(NA,dat_num,yearnum)
c<-matrix(NA,h,daynum)
sta_precip<-matrix(NA,tdat,stanum)


for (i in 1:stanum) {
  for (k in 1:yearnum) {
    for (j in 1:11) {
      a<-precip[i,k,j,]
      c[,j]<-a
    }
    yl[,k]<-as.numeric(c)
  }
  sta_precip[,i]<-as.numeric(yl)
}

sta_precip_meddum<-sta_precip[,med_staa]

med_sta<-med_staa

sta_precip_med<-sta_precip[,med_sta]
colnames(sta_precip_med)<-usta[med_sta] 
stanum_med<-length(med_sta)


save.image("C:/Users/pilat/Desktop/Topsis_heves/observation_data2/updated_obs_precip2.Rdata")


library(scales)

library(ncdf4)
library(geosphere)
library(tdr)
library(tdr)
setwd("C:/Users/pilat/Desktop/Topsis_heves/observation_data2/")
load("updated_obs_precip2.Rdata")

rainsta<-matrix(NA,241,length(med_sta))
a<-numeric(length(med_sta))
for (i in 1:length(med_sta)) {
  y_pr<-sta_precip_med[1:241,i]
  rainsta[,i]<-y_pr
  
}
coorsta<-sub_coor[,]
colnames(rainsta)<-coorsta[,1]

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
    RAINFALL_d1_med[,i,ss]<-rainfall[min_dist[i,1],min_dist[i,2],]
  }
  
  deneme[,ss]<- c(rowMeans(RAINFALL_d2_med[,,ss]))
  
}

save.image("C:/Users/pilat/Desktop/Topsis_heves/observation_data2/obs_precip2.Rdata")