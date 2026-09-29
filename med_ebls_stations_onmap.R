library(rgdal)
library(rgeos)
library(sp)
basins = readOGR(dsn=path.expand('D:/wrf_data_cases/Havzalar'),layer="Havzalar")  
hevesfolder<-"C:/Users/pilat/Downloads/deneme_data/"
inname<-paste(sep='',hevesfolder, '2019122391DD_Beni_Oku!!!.txt')
coordinates<-read.table(inname, sep='|', skip=0,header = TRUE)
sub_coor<-matrix(NA,length(usta),3) 


for (i in 1:length(usta)) {
  a <- which(usta[i] == coordinates[,1])
  k<-as.matrix(coordinates[a,c(1,6,5)])
  sub_coor[i,]<-k
}

basin_num_med<-which(basins$Havza_Ad =="Antalya") 
basin_num_ebls<-which(basins$Havza_Ad =="DoÃ°u Karadeniz") ## shape number 22 is ebls

dat<-data.frame(x=sub_coor[,2],y=sub_coor[,3])
coordinates(dat) <- ~ x+y
proj4string(dat) <- proj4string(basins) ### tell R that dat coordinates are in the same lat/lon reference system with basins
over(dat, as(basins, "SpatialPolygons")) #### over function shows that which points belong to which spatial object.
#### By doing this we saw that there are some stations out of Turkey (NA values). 
########It is due to shape file. We should check these stations if they are in or out of med or ebls regions

na_coor_sta<-(1:length(usta))[is.na(over(dat, as(basins, "SpatialPolygons")))]## these stations give NA (not belong to any spatial object in the shape file)

med_sta<-(1:length(usta))[over(dat, as(basins, "SpatialPolygons"))==basin_num_med]
med_sta<-med_sta[!is.na(med_sta)]#### the stations in med (before checking na stations) 
ebls_sta<-(1:length(usta))[over(dat, as(basins, "SpatialPolygons"))==basin_num_ebls]
ebls_sta<-ebls_sta[!is.na(ebls_sta)]#### the stations in ebls (before checking na stations) 


plot(basins)
points( sub_coor[c(ebls_sta,med_sta),c(2,3)], typ='p', pch = c(16),col=2  ,cex=0.8) 
points( sub_coor[c(na_coor_sta),c(2,3)], typ='p', pch = c(16),col=3  ,cex=1.2) ##### by drawing this, 
#we saw that NA stations are the stations which are on border. So, we may find the stations in ebls by decreasing lat value of na stations
##by 0.1 and find med stations by increasing lat value of na stations by 0.5) 
med_staa<- med_sta
ebls_staa<-ebls_sta
sty<-ymin ###### start year of available data
yyy<-ymax+1 ###### (last year of available data) + 1 
tyear<-10 ####### total data year
dat_num<-daynum*h ######## data number in each year
tdat<-dat_num*tyear ######## total data
tday<-daynum*tyear ###total day length
yl<-matrix(NA,dat_num,yearnum)
c<-matrix(NA,h,daynum)
sta_precip<-matrix(NA,tdat,stanum)


for (i in 1:stanum) {
  for (k in 1:yearnum) {
    for (j in 1:daynum) {
      a<-precip[i,k,j,]
      c[,j]<-a
    }
    yl[,k]<-as.numeric(c)
  }
  sta_precip[,i]<-as.numeric(yl)
}

sta_precip_meddum<-sta_precip[,med_staa]
sta_precip_eblsdum<-sta_precip[,ebls_staa]

ebls_sta<-ebls_staa
med_sta<-med_staa

sta_precip_med<-sta_precip[,med_sta]
sta_precip_ebls<-sta_precip[,ebls_sta]

colnames(sta_precip_med)<-usta[med_sta] ##### station IDs are column names
colnames(sta_precip_ebls)<-usta[ebls_sta]  ##### station IDs are column names

plot(basins)
points( sub_coor[c(ebls_sta,med_sta),c(2,3)], typ='p', pch = c(16),col=2  ,cex=0.8) 

stanum_ebls<-length(ebls_sta)
stanum_med<-length(med_sta)

save.image("C:/Users/pilat/Desktop/Topsis_heves/observation_data2/obs_precip2.Rdata")


