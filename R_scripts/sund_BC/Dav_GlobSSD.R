########################################################
##### plots ezg from different runoff measurements ####
#######################################################


library(sp)
library(plyr)
library(ggplot2)
library(rgeos)
library(maptools)
library(raster)
library(adehabitat)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(rgdal)
library("lubridate")
require(stats)



first="YES"

oudir="N:/gebhyd/3_Hyv/projekte/bafu_ch2018/ch2018_scen/"
#do.call(file.remove, list(list.files(oudir, full.names = TRUE)))


        toload=paste(oudir,"tmp_dav.dat",sep="")
        towrite=paste(oudir,"tmp_dav_ssd_max.dat",sep="")
        print(toload)      
        spm  <- read.table(toload,skip = 1, header = FALSE, stringsAsFactors = FALSE,sep=" ")

        res=spm[,5]
        res2=spm[,5]
        gres=spm[,4]
        gres2=spm[,4]
# climatology
        date.vec = ISOdate(spm[,1],spm[,2],spm[,3],tz="UTC")
        date.noyear = format(date.vec,format="%m%d")
        date.seq=seq(ISOdate(2012,01,01),ISOdate(2012,12,31),by="day")
        date.seq.noyear=format(date.seq,format="%m%d")
        maxclim=NA*(1:length(date.seq))
        maxgclim=NA*(1:length(date.seq))
        for (i in 1:length(date.seq)){
          
          doi = date.seq[i]
          
          # climatology window (odd!)
          win.len = 3
          ii      = (-(win.len-1)/2):((win.len-1)/2)
          dois=rep(doi,win.len)
          for ( j in 1:win.len){
            dois[j] = doi + (ii[j]*24*3600)
          }  
          dois.noyear=format(dois,format="%m%d")
          # position of dates in date vector 
          ind=which(date.noyear%in%dois.noyear)
          clim=res[ind]
          gclim=gres[ind]
          maxclim[i]=max(clim,na.rm=T)
          maxgclim[i]=max(gclim,na.rm=T)
        }
        
        u.date <- unique(date.noyear)
        for (u.i in 1:length(date.seq.noyear)){
        u.ind=which(date.noyear%in%date.seq.noyear[u.i])
#        print(u.ind)
        for (uu.i in 1:length(u.ind)){
        res2[u.ind[uu.i]]=round(res[u.ind[uu.i]]/maxclim[u.i],digits=4)
        res3[u.ind[uu.i]]=round(maxclim[u.i],digits=4)
        gres2[u.ind[uu.i]]=round(gres[u.ind[uu.i]]/maxgclim[u.i],digits=4)
        gres3[u.ind[uu.i]]=round(maxgclim[u.i],digits=4)
#        print(cbind(res2[u.ind[uu.i]],maxclim[u.i]))
        }
        }
        
        wr=cbind.data.frame(spm[,1],spm[,2],spm[,3],"24",spm[,4],spm[,5],res3,res2,gres3,gres2)       
        write.table(wr,file = towrite, append = F, quote = F, row.names = F,
        col.names = F, sep = " ")
