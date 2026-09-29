
load("G:/sst_analysis_completed/ebls_sst/ebls_d02.Rdata")
 deneme_ebls<-matrix(NA,dim(rainsta)[1],4)
 A<-4
 for(ss in 1:4)
   {
         deneme_ebls[,ss]<- c(rowMeans(RAINFALL_d2_ebls[,,ss]))
     }
 
   mean_ebls<-rowMeans(deneme_ebls)
   
     Obs<-rowMeans(rainsta,na.rm=TRUE)
     
       glabels=c("WRF", "GHRSST","MEDSPI","NCEP")
       hours<-c("0","24h","48h","72h","96h","120h","144h","168h","192h","216h","240h")
       dates<- c("2018/12/10", "2018/12/11", "2018/12/12", "2018/12/13", "2018/12/14", "2018/12/15", "2018/12/16", "2018/12/17", "2018/12/18","2018/12/19", "2018/12/20")
       matplot(deneme_ebls,type="l",lty=2,col="gray",xlab='Dates',ylab='Rainfall(mm)',lwd=1,xaxt='n', cex.lab =1.3, cex.axis = 1.3)
       lines(mean_ebls,type="l",col="red",lwd=3)
       lines(Obs,type="l",col="blue",lwd=3)
       legend("topleft", legend=c( "mean of wrfout scenarios","wrfout scenarios","mean of observation stations"),col=c("red","gray", "blue"), lty=1:2, lwd =3, cex=1,text.font=2) 
       axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.3)
       legend("topright",c("SST Analysis - 3 km",paste("correlation=",round(cor(mean_ebls,Obs,method = "pearson"),2),sep="")), cex=1,text.font=2)
       
         matplot(deneme_ebls,type="l",lty=2,col="gray",xlab='Dates',ylab='Rainfall(mm)',lwd=1,xaxt='n', cex.lab =1.3, cex.axis = 1.3)
       lines(mean_ebls,type="l",col="red",lwd=3)
       lines(Obs,type="l",col="blue",lwd=3)
       legend("topleft", legend=c( "mean of wrfout scenarios","wrfout scenarios","mean of observation stations"),col=c("red","gray", "blue"), lty=1:2, lwd =3, cex=1,text.font=2) 
       
       axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.3)
       legend("topright",c("SST Analysis - 3 km",paste("correlation=",round(cor(mean_ebls,Obs,method = "pearson"),2),sep="")), cex=1,text.font=2)
       
         matplot(deneme_ebls,type="l",lty=2,col="gray",xlab='Dates',ylab='Rainfall(mm)',lwd=1,xaxt='n', cex.lab =1.3, cex.axis = 1.3, ylim=c(0,4))
       lines(mean_ebls,type="l",col="red",lwd=3)
       lines(Obs,type="l",col="blue",lwd=3)
       legend("topleft", legend=c( "mean of wrfout scenarios","wrfout scenarios","mean of observation stations"),col=c("red","gray", "blue"), lty=1:2, lwd =3, cex=1,text.font=2) 
       
         axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.3)
      legend("topright",c("SST Analysis - 3 km",paste("correlation=",round(cor(mean_ebls,Obs,method = "pearson"),2),sep="")), cex=1,text.font=2)
       
         one_gfs<-RAINFALL_d2_ebls[,,1]
         one_bestg<-rowMeans(one_gfs)
         plot(one_bestg,type="l",lty=1,col="purple",xlab='Dates',ylab='Rainfall(mm)',lwd=3,xaxt='n',ylim=c(0,5),cex.lab=1.3, cex.axis = 1.3)
         lines(Obs,type="l",col="green",lwd=3)
         legend("topleft", legend=c( "mean of WRF SST wrfout","mean of observation stations"),col=c("purple", "green"), lty=1:1, lwd = 3, cex=1,text.font=2) 
         axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.3)
         legend("topright",c("1st - 9 km",paste("correlation=",round(cor(one_bestg,Obs,method = "pearson"),2),sep="")), cex=1,text.font=2)
         
           one_bestg<-rowMeans(one_gfs)
           plot(one_bestg,type="l",lty=1,col="purple",xlab='Dates',ylab='Rainfall(mm)',lwd=3,xaxt='n',ylim=c(0,4),cex.lab=1.3, cex.axis = 1.3)
           lines(Obs,type="l",col="green",lwd=3)
           legend("topleft", legend=c( "mean of WRF SST wrfout","mean of observation stations"),col=c("purple", "green"), lty=1:1, lwd = 3, cex=1,text.font=2) 
           axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.3)
         legend("topright",c("1st - 9 km",paste("correlation=",round(cor(one_bestg,Obs,method = "pearson"),2),sep="")), cex=1,text.font=2)
           
             one_bestg<-rowMeans(one_gfs)
             plot(one_bestg,type="l",lty=1,col="purple",xlab='Dates',ylab='Rainfall(mm)',lwd=3,xaxt='n',ylim=c(0,4),cex.lab=1.3, cex.axis = 1.3)
             lines(Obs,type="l",col="green",lwd=3)
             legend("topleft", legend=c( "mean of WRF SST wrfout","mean of observation stations"),col=c("purple", "green"), lty=1:1, lwd = 3, cex=1,text.font=2) 
             axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.3)
             legend("topright",c("1st - 3 km",paste("correlation=",round(cor(one_bestg,Obs,method = "pearson"),2),sep="")), cex=1,text.font=2)
             
               sec_gfs<-RAINFALL_d2_ebls[,,3]
               sec_bestg<-rowMeans(sec_gfs)
               plot(sec_bestg,type="l",lty=1,col="purple",xlab='Dates',ylab='Rainfall(mm)',lwd=3,xaxt='n',ylim=c(0,5),cex.lab=1.3, cex.axis = 1.3)
               lines(Obs,type="l",col="green",lwd=3)
               legend("topleft", legend=c( "mean of Medspiration SST wrfout","mean of observation stations"),col=c("purple", "green"), lty=1:1, lwd = 3, cex=1,text.font=2) 
               axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.3)
               legend("topright",c("2nd - 3 km",paste("correlation=",round(cor(sec_bestg,Obs,method = "pearson"),2),sep="")), cex=1,text.font=2)
             
                 sec_bestg<-rowMeans(sec_gfs)
                 plot(sec_bestg,type="l",lty=1,col="purple",xlab='Dates',ylab='Rainfall(mm)',lwd=3,xaxt='n',ylim=c(0,4),cex.lab=1.3, cex.axis = 1.3)
                 lines(Obs,type="l",col="green",lwd=3)
                 legend("topleft", legend=c( "mean of Medspiration SST wrfout","mean of observation stations"),col=c("purple", "green"), lty=1:1, lwd = 3, cex=1,text.font=2) 
                 axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.3)
                 legend("topright",c("2nd - 3 km",paste("correlation=",round(cor(sec_bestg,Obs,method = "pearson"),2),sep="")), cex=1,text.font=2)
                 
                   thr_gfs<-RAINFALL_d2_ebls[,,2]
                   thr_bestg<-rowMeans(thr_gfs)
                   plot(thr_bestg,type="l",lty=1,col="purple",xlab='Dates',ylab='Rainfall(mm)',lwd=3,xaxt='n',ylim=c(0,4),cex.lab=1.3, cex.axis = 1.3)
                   lines(Obs,type="l",col="green",lwd=3)
                   legend("topleft", legend=c( "mean of GHRSST SST wrfout","mean of observation stations"),col=c("purple", "green"), lty=1:1, lwd = 3, cex=1,text.font=2) 
                   axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.3)
                   legend("topright",c("3rd - 3 km",paste("correlation=",round(cor(thr_bestg,Obs,method = "pearson"),2),sep="")), cex=1,text.font=2)
                   
                     for_gfs<-RAINFALL_d2_ebls[,,4]
                     for_bestg<-rowMeans(for_gfs)
                     plot(for_bestg,type="l",lty=1,col="purple",xlab='Dates',ylab='Rainfall(mm)',lwd=3,xaxt='n',ylim=c(0,4),cex.lab=1.3, cex.axis = 1.3)
                     lines(Obs,type="l",col="green",lwd=3)
                     legend("topleft", legend=c( "mean of NCEP SST wrfout","mean of observation stations"),col=c("purple", "green"), lty=1:1, lwd = 3, cex=1,text.font=2) 
                     axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.3)
                     legend("topright",c("4th - 3 km",paste("correlation=",round(cor(for_bestg,Obs,method = "pearson"),2),sep="")), cex=1,text.font=2)
                     
                   plot(for_bestg,type="l",lty=1,col="purple",xlab='Dates',ylab='Rainfall(mm)',lwd=3,xaxt='n',ylim=c(0,5),cex.lab=1.3, cex.axis = 1.3)
                   lines(Obs,type="l",col="green",lwd=3)
                   lines(one_bestg,type="l",col="red",lwd=3)
                   
                  axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.3)
                     
                  legend("topleft", legend=c( "mean of worst (NCEP) scenario","mean of observation stations","mean of best (WRF) scenario"),col=c("purple", "green", "red"), lty=1:1, lwd = 3, cex=1,text.font=2)
                  legend("topright",c("SST - 3 km",paste("correlation=",round(cor(for_bestg,Obs,method = "pearson"),2),sep=""),paste("correlation=",round(cor(one_bestg,Obs,method = "pearson"),2),sep="")),col=c("white", "purple","red"), lty=c(1:1),cex=1,text.font=2, lwd=3)
                    
                      
                  plot(for_bestg,type="l",lty=1,col="purple",xlab='Dates',ylab='Rainfall(mm)',lwd=3,xaxt='n',ylim=c(0,4),cex.lab=1.3, cex.axis = 1.3)
                   lines(Obs,type="l",col="green",lwd=3)
                  
                   lines(one_bestg,type="l",col="red",lwd=3)
                   
                     axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.3)
                   
                     legend("topleft", legend=c( "mean of worst (NCEP) scenario","mean of observation stations","mean of best (WRF) scenario"),col=c("purple", "green", "red"), lty=1:1, lwd = 3, cex=1,text.font=2)
                   
                   legend("topright",c("SST - 3 km",paste("correlation=",round(cor(for_bestg,Obs,method = "pearson"),2),sep=""),paste("correlation=",round(cor(one_bestg,Obs,method = "pearson"),2),sep="")),col=c("white", "purple","red"), lty=c(1:1),cex=1,text.font=2, lwd=3)
                   
                     dene<-RAINFALL_d2_ebls[,,1]
                   bestg<-dene[,48]
                   ist<-rainsta[,48]         
                   matplot(bestg,type="l",lty=1,col="red",xlab='Dates',ylab='Rainfall(mm)',lwd=2,xaxt='n',ylim=c(0,40), cex.axis = 1.1, cex.lab=1.3)
                   lines(ist,col="blue",lwd=2,lty=1)
                   axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.1)
                   legend("topleft", legend=c( "WRF SST wrfout of nearest grid to main observation station ","main observation station"),col=c("red","blue"), lty=1:1, cex=0.9,text.font=2)  
                   legend("topright", legend=c( "SST-3 km","Station 18554"), cex=1,text.font=2) 
                   
                     matplot(bestg,type="l",lty=1,col="red",xlab='Dates',ylab='Rainfall(mm)',lwd=2,xaxt='n',ylim=c(0,50), cex.axis = 1.1, cex.lab=1.3)
                   lines(ist,col="blue",lwd=2,lty=1)
                   axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.1)
                   legend("topleft", legend=c( "WRF SST wrfout of nearest grid to main observation station ","main observation station"),col=c("red","blue"), lty=1:1, cex=0.9,text.font=2)  
                   legend("topright", legend=c( "SST-3 km","Station 18554"), cex=1,text.font=2) 
                   
                   matplot(bestg,type="l",lty=1,col="red",xlab='Dates',ylab='Rainfall(mm)',lwd=2,xaxt='n',ylim=c(0,50), cex.axis = 1.1, cex.lab=1.3)
                   lines(ist,col="blue",lwd=2,lty=1)
                   axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.1)
                   legend("topleft", legend=c( "WRF SST wrfout of nearest grid to main observation station ","main observation station"),col=c("red","blue"), lty=1:1, cex=1,text.font=2)  
                   legend("topright", legend=c( "SST-3 km","Station 18554"), cex=1,text.font=2) 
                   
                   dene<-RAINFALL_d2_ebls[,,4]
                   bestg<-dene[,48]
                   ist<-rainsta[,48]         
                   matplot(bestg,type="l",lty=1,col="red",xlab='Dates',ylab='Rainfall(mm)',lwd=2,xaxt='n',ylim=c(0,50), cex.axis = 1.1, cex.lab=1.3)
                   lines(ist,col="blue",lwd=2,lty=1)
                   axis(1, at = seq(1, 241, by = 24), las=1,label=dates, srt=0, cex.axis = 1.1)
                   legend("topleft", legend=c( "NCEP SST wrfout of nearest grid to main observation station ","main observation station"),col=c("red","blue"), lty=1:1, cex=1,text.font=2)  
                   legend("topright", legend=c( "SST-3 km","Station 18554"), cex=1,text.font=2) 
                   
                     box_gfs = cbind(Obs,deneme_ebls)
                  
                   library(RColorBrewer)
                   jBrewColors <- brewer.pal(n = 8, name = "Set2")
                   
                     boxplot(box_gfs, xlab="Scenario names", ylab="Hourly Mean Rainfall(mm)", names = c("Obs","WRF", "GHRSST","Medspiration","NCEP"),use.rows = TRUE,col = jBrewColors, cex.axis = 1.1, cex.lab=1.3)
                   boxplot(box_gfs, xlab="Scenario names", ylab="Hourly Mean Rainfall(mm)", names = c("Obs","WRF", "GHRSST","Medspiration","NCEP"),use.rows = TRUE,col = jBrewColors, cex.axis = 1.1, cex.lab=1.3,ylim=c(0,2))
                   X<-4
                   fark<-matrix(NA,dim(rainsta)[1],X)
                   for (ss in 1 :X)
                     {
                           c<-c(rowMeans(RAINFALL_d2_ebls[,,ss]))
                           d<-rowMeans(rainsta,na.rm=TRUE)
                           for (i in 1:241)
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
                   barplot(bias, col = c("red","black"),ylim= c(-0.2,0.2),beside = TRUE, xlab = "Scenario numbers", ylab= "Mean Bias (mm)", cex.axis = 1, names.arg = c(glabels),cex.lab =1.2)
                   legend("topleft", c("b) Domain 2"), cex = 1, text.font=2.5)
                   barplot(bias, col = c("red","black"),ylim= c(-0.2,0.2),beside = TRUE, xlab = "Scenario names", ylab= "Mean Bias (mm)", cex.axis = 1, names.arg = c(glabels),cex.lab =1.2)
                   legend("topleft", c("b) Domain 2"), cex = 1, text.font=2.5)
                   barplot(bias, col = c("red","black"),ylim= c(-0.1,0.1),beside = TRUE, xlab = "Scenario names", ylab= "Mean Bias (mm)", cex.axis = 1, names.arg = c(glabels),cex.lab =1.2)
                   legend("topleft", c("b) Domain 2"), cex = 1, text.font=2.5)
                   barplot(bias, col = c("red","black"),ylim= c(-0.2,0.2),beside = TRUE, xlab = "Scenario names", ylab= "Mean Bias (mm)", cex.axis = 1, names.arg = c(glabels),cex.lab =1.2)
                   legend("topleft", c("b) Domain 2"), cex = 1, text.font=2.5)
                   library(dplyr)
                   library(lubridate)
                   library(fields)
                   library(magrittr)
                   library(colorspace)
                   hcl_palettes(plot = TRUE)
                   colors<-diverge_hsv(10)
                   
                     G<-4
                   fark<-matrix(NA,dim(rainsta)[1],G)
                   for (ss in 1 :G)
                      {
                           c<-c(rowMeans(RAINFALL_d2_ebls[,,ss]))
                           d<-rowMeans(rainsta,na.rm=TRUE)
                           for (i in 1:241)
                             {
                                   fark[i,ss]<- (c[i]-d[i]) 
                               }
                           
                         image.plot(1:241,seq(from=1, to=4, by = 1),fark,axes=FALSE, 
                                                 xlab = "Run Hours", ylab = " ", col = colors,horizontal = FALSE,legend.only=FALSE,legend.shrink = 1,midpoint = TRUE, cex.lab =1.2, legend.args=list(text='Bias (mm)', side=3, font=1, line= 1, cex=1.2))
                       
                         axis(side=2, seq(1, 4, by = 1), labels=glabels, las=1, cex.axis=0.9, tck = 0)
                       
                         axis(side=1, seq(0, 240, by = 24), las=1, cex.axis=1, label=hours, tck = 0)
                       G<-4
                       fark<-matrix(NA,dim(rainsta)[1],G)
                       for (ss in 1 :G)
                         {
                               c<-c(rowMeans(RAINFALL_d2_ebls[,,ss]))
                               d<-rowMeans(rainsta,na.rm=TRUE)
                               for (i in 1:241)
                                 {
                                       fark[i,ss]<- (c[i]-d[i]) 
                                   }
                               
                             }
                       
                        
                         G<-4
                         fark<-matrix(NA,dim(rainsta)[1],G)
                         for (ss in 1 :G)
                           {
                                 c<-c(rowMeans(RAINFALL_d2_ebls[,,ss]))
                                 d<-rowMeans(rainsta,na.rm=TRUE)
                                 for (i in 1:241)
                                 {
                                         fark[i,ss]<- (c[i]-d[i]) 
                                     }
                                 
                               }
                         image.plot(1:241,seq(from=1, to=4, by = 1),fark,axes=FALSE, 
                                     +            xlab = "Run Hours", ylab = " ", col = colors,horizontal = FALSE,legend.only=FALSE,legend.shrink = 1,midpoint = TRUE, cex.lab =1.2, legend.args=list(text='Bias (mm)', side=3, font=1, line= 1, cex=1.2))
                         
                           axis(side=2, seq(1, 4, by = 1), labels=glabels, las=1, cex.axis=0.9, tck = 0)
                         
                           axis(side=1, seq(0, 240, by = 24), las=1, cex.axis=1, label=hours, tck = 0)
                        
                        