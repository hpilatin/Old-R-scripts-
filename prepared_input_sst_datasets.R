library(ncdf4)
library(raster)

# Parent Domain - MED:

p<-'+proj=lcc +lat_1=30 +lat_2=60 +lat_0=39.0000114440918 +lon_0=35.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'
dede<-raster()
g<-projectRaster(dede,crs=p) 
nrow(g)<-110
ncol(g)<-231
l<-c(-1041971.6550,1040530.4422,-496816.6918,494850.9735)
extent(g) <- l

sg <- stack(lapply(1:41, function(x) setValues(g, runif(ncell(g)))))
ff<-stack("C:/Users/pilat/Documents/wrflowınp.tif")
qa<-as.vector(ff)

outg<- setValues(sg, qa)
rnc <- writeRaster(outg, filename=file.path("D:/outg.nc"), format="CDF", overwrite=TRUE,varname="SST", varunit="K",longname="SST variable -- raster layer to netCDF", xname="lon", yname="lat")

# Child Domain - MED:

library(ncdf4)
library(raster)
flow<-nc_open("D:/wrflowinp_d02")
sst<-ncvar_get(flow, "SST") 

p<-'+proj=lcc +lat_1=30 +lat_2=60 +lat_0=39.0000114440918 +lon_0=35.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'
dede<-raster()
g<-projectRaster(dede,crs=p) 
nrow(g)<-87
ncol(g)<-72
l<-c(-491741.6063, -275727.8313, -296890.3818, -35873.7369)
extent(g) <- l

sg <- stack(lapply(1:41, function(x) setValues(g, runif(ncell(g)))))
ff<-stack("C:/Users/pilat/Documents/domain2tif.tif")
qa<-as.vector(ff)

outg<- setValues(sg, qa)

rnc <- writeRaster(outg, filename=file.path("D:/domain2.nc"), format="CDF", overwrite=TRUE,varname="SST", varunit="K",longname="SST variable -- raster layer to netCDF", xname="lon", yname="lat")

##############################################################################

# Parent Domain - EBLS:

library(ncdf4)
library(raster)

p<-'+proj=lcc +lat_1=30 +lat_2=60 +lat_0=39.0000114440918 +lon_0=35.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'
dede<-raster()
g<-projectRaster(dede,crs=p) 
nrow(g)<-110
ncol(g)<-231
l<-c(-1041971.6550,1040530.4422,-496816.6918,494850.9735)
extent(g) <- l

sg <- stack(lapply(1:41, function(x) setValues(g, runif(ncell(g)))))
ff<-stack("C:/Users/pilat/Documents/wrflowınp.tif")
qa<-as.vector(ff)

outg<- setValues(sg, qa)
rnc <- writeRaster(outg, filename=file.path("D:/boutg.nc"), format="CDF", overwrite=TRUE,varname="SST", varunit="K",longname="SST variable -- raster layer to netCDF", xname="lon", yname="lat")

# Child Domain - EBLS:

library(ncdf4)
library(raster)

p<-'+proj=lcc +lat_1=30 +lat_2=60 +lat_0=39.0000114440918 +lon_0=35.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'
dede<-raster()
g<-projectRaster(dede,crs=p) 
nrow(g)<-51
ncol(g)<-135
l<-c(121841.3373, 527775.1291, 134333.4915, 287686.2573)
extent(g) <- l

sg <- stack(lapply(1:41, function(x) setValues(g, runif(ncell(g)))))
ff<-stack("C:/Users/pilat/Documents/domain2tif.tif")
qa<-as.vector(ff)

outg<- setValues(sg, qa)

rnc <- writeRaster(outg, filename=file.path("D:/bdomain2.nc"), format="CDF", overwrite=TRUE,varname="SST", varunit="K",longname="SST variable -- raster layer to netCDF", xname="lon", yname="lat")
