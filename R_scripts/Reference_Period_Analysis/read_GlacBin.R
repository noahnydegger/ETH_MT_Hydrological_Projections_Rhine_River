source("/Users/noahnydegger/GitHub/ETH/ETH_MT_Hydrological_Projections_Rhine_River/Data/ThS/swisscors2lonlat")

# args    = commandArgs()
# nr.args = length(args)
# files   = args[8:(nr.args-1)] 
# #print(files)
# out.file = args[nr.args]
# #print("output")
# print(out.file)

gebiet <- "ThS"

chain_file <- "chains_vs_glac_ch2018.dat"

data_path <- file.path(here::here(), "Data", gebiet)

glac_dir <- file.path(data_path, "ThSGlac", "MOHC-HadGEM2-ES_CCLM4-8-17_r1i1p1_rcp85")

# List all files in the directory (filter by extension if needed, e.g., pattern = "*.dat")
files <- list.files(glac_dir, full.names = TRUE)
print(files)
# Output file specification
out.file <- file.path(data_path, "glac_test", "glac_test.txt")
print(out.file)


cols    <- new.env()
rows    <- new.env()
lons    <- new.env()
lats    <- new.env()
lengths <- new.env()
maxlon  <- c()
minlon  <- c()
maxlat  <- c()
minlat  <- c()

for (i in 1:length(files)){
  to.read   <- file(files[i],"rb")
  col       <- readBin(to.read, double(),size=4, n = 1, endian = "little")
  row       <- readBin(to.read, double(),size=4, n = 1, endian = "little")
  xu        <- readBin(to.read, double(),size=4, n = 1, endian = "little")
  yu        <- readBin(to.read, double(),size=4, n = 1, endian = "little")
  dist      <- readBin(to.read, double(),size=4, n = 1, endian = "little")  
  nodata    <- readBin(to.read, double(),size=4, n = 1, endian = "little")
  swiss.lon <- seq(xu,xu+(dist*(col-1)),by=dist)
  swiss.lat <- seq(yu,yu+(dist*(row-1)),by=dist)
  maxlon    <- max(maxlon,swiss.lon)[1]
  minlon    <- min(minlon,swiss.lon)[1]
  maxlat    <- max(maxlat,swiss.lat)[1]
  minlat    <- min(minlat,swiss.lat)[1]
  assign( paste("swisslon",i,sep=""), swiss.lon,    envir=lons)
  assign( paste("swisslat",i,sep=""), swiss.lat,    envir=lats)
  assign( paste("len",i,sep=""),      (row*col)+12, envir=lengths)
  assign( paste("col",i,sep=""),      col,          envir=cols)
  assign( paste("row",i,sep=""),      row,          envir=rows)
  close(to.read)  
}


leftdow = swisscors2lonlat(chx=minlon,chy=minlat)
rightup = swisscors2lonlat(chx=maxlon,chy=maxlat)

lonlim = c(leftdow[1],rightup[1])
latlim = c(leftdow[2],rightup[2])

alllon  = seq(minlon,maxlon,by=dist)    
alllat  = seq(minlat,maxlat,by=dist)
alldata = array(NA,dim=c(length(alllon),length(alllat)))



for (i in 1:length(files)){
  
  to.read   <- file(files[i],"rb")
  a         <- readBin(to.read, double(),size=4, n = get(paste("len",  i,sep=""),envir=lengths), endian = "little")
  close(to.read)
  col       <- get(paste("col",  i,sep=""),envir=cols)
  row       <- get(paste("row",  i,sep=""),envir=rows)
  a         <- a[13:length(a)]
  a         <- array(a,dim=c(col,row))
  a_corr    <- array(NA,dim=c(col,row))
  for (lat in 1:row){
    a_corr[,lat] <- a[,(row+1)-lat]
  }
  a_corr[a_corr==nodata] <- NA
  indx <- which(alllon%in%get(paste("swisslon",i,sep=""),envir=lons))
  indy <- which(alllat%in%get(paste("swisslat",i,sep=""),envir=lats))
  alldata[indx,indy][is.na(a_corr)==F] <- a_corr[is.na(a_corr)==F]
  
}  

all = alldata*NA
for (lat in 1:dim(all)[2]){
  all[,lat] <- alldata[,(dim(all)[2]+1)-lat]
}


all[is.na(all)]=nodata
cols = as.numeric(length(alllon))
rows = as.numeric(length(alllat))
xu   = min(alllon)
yu   = min(alllat)
h7   = nodata 
h8   = nodata 
h9   = nodata 
h10  = nodata 
h11  = nodata 
h12  = nodata 

# write data to new file

zz <- file(out.file, "wb")
writeBin(cols,zz, size =4,endian = "little")
writeBin(rows,zz, size =4,endian = "little")
writeBin(xu,zz, size =4,endian = "little")
writeBin(yu,zz, size =4,endian = "little")
writeBin(dist,zz, size =4,endian = "little")
writeBin(nodata,zz, size =4,endian = "little")
writeBin(h7,zz, size =4,endian = "little")
writeBin(h8,zz, size =4,endian = "little")
writeBin(h9,zz, size =4,endian = "little")
writeBin(h10,zz, size =4,endian = "little")
writeBin(h11,zz, size =4,endian = "little")
writeBin(h12,zz, size =4,endian = "little")
writeBin(as.vector(all),zz, size =4,endian = "little")
close(zz)









