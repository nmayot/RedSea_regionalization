### Clustering the dataset ---------------------------------
#
#

rm(list=ls()) # clear all variable
library("e1071") # for clustering

#-----
## 1) Open climatological dataset
folder <- "data/CCI_4_2_RedSea/" # folder with data
filename <- "RedSea_climato_1998_2019_8D_CCI_new.rdata" # Name of the dataset
load(file = paste(folder,filename,sep="")) # load dataset (row = pixels, column = variables)
CHL <- dataset$CHL_norm # dataset to be cluterized
gp <- which(!is.na(CHL[,1])) # pixel (observations) to be clusterized
#-----


#-----
## 2) Climatological Kmeans
nclu <- 4 # number of clusters wanted
niter <- 500 # number of iteration before to stop the kmeans process
tschl <- scale(CHL[gp,], scale=T, center=T) # scaled data

fcm        <- cmeans(tschl, centers=nclu, iter.max=niter, m=1.5) # Fuzzy C-Means clustering
clim_clus  <- fcm$cluster
clim_gp    <- gp
new_order  <- c(1,2,3,4) # if you need to change the number associated with each cluster
clim_clus  <- new_order[clim_clus]
membership <- fcm$membership[,new_order]
mb         <- apply(membership,1,max)
clim_centers <- fcm$centers[order(new_order),]
#-----


#-----
## 3) Save climatological cmeans
dataset$gp         <- gp           # TS clusterized
dataset$cluster    <- clim_clus    # clusters
dataset$centers    <- clim_centers # centers
dataset$membership <- membership   # membership to all clusters
dataset$mb         <- mb           # maximal membership
save(dataset, file = paste(folder,"Climato_cmeans_1998_2019.rdata",sep="")) # save rdata into the same folder
#-----


#-----
## 4) Annual FCM with climatological centers as initial values
rm(list=ls()) # clear all variable
folder <- "data/CCI_4_2_RedSea/" # folder with data
load(file = paste(folder,"Climato_cmeans_1998_2019.rdata",sep="")) # load dataset (row = pixels, column = variables)

clim_centers <- dataset$centers[c(3,4),]
LAT <- dataset$LAT
LON <- dataset$LON

filename <- "RedSea_annual_1998_2019_8D_CCI_new.rdata" # Name of the dataset with annual times series
load(file = paste(folder,filename,sep="")) # load dataset (row = pixels, column = variables)
CHL <- dataset$CHL_norm # normalized times series

years <- 1998:2019
yyyy_clus <- 1998:2018 # years to be cluterized
DOY8 <- seq(1,366,8)

DOY <- dataset$DOY # Day of each column
yyyy <- dataset$yyyy # year of each column

DOY8 <- DOY8[c(24:46,1:23)] # Days wanted - July to July
movavg <- function(x,n){filter(x, rep(1 / n, n), sides = 2, circular = T)} # moving average function

res_clus <- matrix(nrow=nrow(CHL),ncol=length(yyyy_clus)) # Matrix to save annual FCM results 
res_mb <- matrix(nrow=nrow(CHL),ncol=length(yyyy_clus)) # Matrix to save annual FCM results 
res_centers <- array(NA,dim=c(dim(clim_centers)[1],length(DOY8),length(yyyy_clus))) # Matrix to save annual FCM results 

for (y in yyyy_clus) {
  
  idx <- which(yyyy == y & DOY == DOY8[1]) # index first column to be considered
  idx <- idx:(idx+45) # all 46 columns to be used as variables (46 8-day weeks = 1 year)
  gp <- which(!is.na(CHL[,idx[1]]) &
              LAT >= 20) # all rows to be used as observation (non-NA pixel and north of 20N)
  
  tschl <- CHL[gp,idx] # collect times series
  tschl <- t(apply(tschl,1,function(x) movavg(x,3))) # smooth them
  tschl <- scale(tschl, scale=T, center=T) # scale them
  
  fcm <- cmeans(tschl, clim_centers,m=1.5) # clusterize them (m = 1.5, determine from climatological FCM)
  res_clus[gp,y == yyyy_clus] <- fcm$cluster # save cluster results
  res_mb[gp,y == yyyy_clus] <- apply(fcm$membership,1,function(x) max(x))
  mb <- res_mb[gp,y == yyyy_clus]
  gp <- gp
  res_centers[,,y == yyyy_clus] <- data.matrix(aggregate(CHL[gp,idx],
                                                         list(fcm$cluster),mean)[,2:47])
}
#-----

dataclus <- c()
dataclus$yyyy_clus <- res_clus
dataclus$yyyy_mb <- res_mb
dataclus$yyyy_centers <- res_centers
dataclus$LON <- LON
dataclus$LAT <- LAT
save(dataclus, file = "data/yyyy_clus_1998_2019.rdata") # save rdata into the same folder
