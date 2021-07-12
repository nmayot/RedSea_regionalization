# Prepare the interannual dataset for the clustering analysis
#
# 

# Necessary packages
setwd("Documents/Bigelow postdoc/project/RedSea/")
rm(list=ls()) # clear all variable
source("script/NM_functions.R")
library("ncdf4") # package to open NetCDF

#-----
## 1) Prepare the time variable
#
years <- c(1998:2019)  # years of the climato
nweeks <- 46 # there are 46 weeks of 8-day in one year
DOY8 <- seq(1,366,8) # julian day of the first day of each 8-day week (1, 9, 17..., 361)
DOY <- rep(DOY8, length(years))
yyyy <- sort(rep(years, nweeks))
DOY <- DOY[23:(length(DOY)-22)] # July to July - add one day at the beginning and at the end (for the interpolation)
yyyy <- yyyy[23:(length(yyyy)-22)]
#-----


#-----
## 2) Prepare the multiannual dataset
folder <- "data/CCI_4_2_RedSea/" # folder with data

# Read each each year and each week
for (y in years) {
  # and for each year
  DOY8 <- DOY[yyyy == y] # to select years with available days
  for (j in DOY8) {
    dtime <- (as.Date(paste("01-01-", y, sep=""),"%d-%m-%Y") + j) - 1
    filename <- list.files(path = paste(folder, y, "/", sep=""),
                           pattern = paste(format(dtime,"%Y%m%d"),sep=""),
                           full.names = T) # search filename by year and date
    
    if (length(filename) != 0) { # if week sampled this year
      nc <- nc_open(filename) # open file
      
      if (!exists("lon")) {
        # lon and lat vectors for each pixel
        lat <- ncvar_get(nc, "lat")
        lon <- ncvar_get(nc, "lon")
        LON = matrix(lon,length(lon),length(lat))
        LAT = matrix(lat,length(lon),length(lat),byrow=T)
        LON <- as.vector(LON)
        LAT <- as.vector(LAT)
       
        # To save data
        chl_8d <- matrix(nrow=length(LON), ncol=length(DOY)) # to save climato
      }
      
      chla <- ncvar_get(nc, "chlor_a") # open chlor_a variable
      chla <- as.vector(chla) # convert into vector
      chla[chla == -999] <- NA # remove NA
      chl_8d[,which(DOY == j & yyyy == y)] <- chla # save into the matrix before weekly data
    } else {
      print(paste(y, " ",j,sep="")) # file not found
    }
  }
}
#-----


#-----
## 3) interpolate all time series
nbx <- dim(chl_8d)[2]
goodp <- which(rowSums(is.na(chl_8d)) <= dim(chl_8d)[2]-2) # Need at least two points
chl_na <- is.na(chl_8d) 
chl_8d_int <- matrix(nrow=dim(chl_8d)[1],ncol=dim(chl_8d)[2]) 
chl_8d_int[goodp,] <- t(apply(chl_8d[goodp,],1,function(x) approx(1:nbx,x,xout = 1:nbx, rule=1)$y)) # no extrapolation

chl_8d_int <- chl_8d_int[,2:(length(DOY)-1)] # Remove first and last week that were kept only for the interpolation 
chl_na     <- chl_na[,2:(length(DOY)-1)]     # Remove first and last week that were kept only for the interpolation
yyyy       <- yyyy[2:(length(DOY)-1)]       # Remove first and last week that were kept only for the interpolation
DOY        <- DOY[2:(length(DOY)-1)]        # Remove first and last week that were kept only for the interpolation 
#-----


#-----
## 4) Evaluate which annual time series have to be taken into account
#
nblim <- 4
nbint <- 23
lag <- 0
goody <- t(apply(chl_na,1,function(x) selectTS(x,nweeks,nblim,nbint,lag)))
#-----

#-----
## 5) Normalized annual time series have to be taken into account
#
lag <- 0
chl_norm <- t(apply(chl_8d_int,1,function(x) normTS(x,nweeks,lag)))
chl_norm[!goody] <- NA
#-----


#-----
## 6) Build the dataset and save it
#
dataset <- c()
dataset$DOY      <- DOY         # days
dataset$yyyy     <- yyyy        # years
dataset$LON      <- LON         # longitude of each pixel
dataset$LAT      <- LAT         # latitude of each pixel
dataset$CHL      <- chl_8d_int  # rows = pixels, column = 8-day weeks (x46)
dataset$CHL_na   <- chl_na      # Locations of previous NAs
dataset$CHL_norm <- chl_norm    # years to be taken into
save(dataset, file = paste(folder,"RedSea_annual_1998_2019_8D_CCI_new.rdata",sep="")) # save rdata into the same folder
#-----
