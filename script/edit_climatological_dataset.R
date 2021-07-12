# Prepare the climatological dataset for the clustering analysis
#
# 

# Necessary packages
rm(list=ls()) # clear all variable
source("script/NM_functions.R")
library("ncdf4") # package to open NetCDF


#-----
## 1) Prepare the time variable
years <- 1998:2019 # years for the climato
nweeks <- 46 # there are 46 weeks of 8-day in one year
DOY8 <- seq(1,366,8) # julian day of the first day of each 8-day week (1, 9, 17..., 361)
DOY <- rep(DOY8, length(years))
yyyy <- sort(rep(years, nweeks))
DOY <- DOY[24:(length(DOY)-23)] # DOY from July to July
yyyy <- yyyy[24:(length(yyyy)-23)] # Years from July to July
#-----


#-----
## 2) Prepare the climatology
#
# Read each week and each year > average week over years
folder <- "data/CCI_4_2_RedSea/" # folder with data

# for each week
for (j in DOY8) {
  chl_week <- c() # initialization
  years <- yyyy[DOY ==j] # to select years with available days
  # and for each year
  for (y in years) {
    dtime <- (as.Date(paste("01-01-", y, sep=""),"%d-%m-%Y") + j) - 1
    filename <- list.files(path = paste(folder, y, "/", sep=""),
                           pattern = paste(format(dtime,"%Y%m%d"),sep=""),
                           full.names = T) # search filename by year and date
    
    if (length(filename) != 0) { # if weekly image available this year
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
        chl_clim <- matrix(nrow=length(LON), ncol=length(DOY8)) # to save climato
      }
      
      chla <- ncvar_get(nc, "chlor_a") # open chlor_a variable
      chla <- as.vector(chla) # convert into vector
      chla[chla == -999] <- NA # remove NA
      chl_week <- cbind(chl_week, chla) # save into the matrix before weekly average
      nc_close(nc) # close file
    } else {
      print(paste(y, " ",j,sep="")) # file not found
    }
  }
  chl_week <- rowMeans(chl_week, na.rm = TRUE) # weekly climatology
  chl_clim[,j == DOY8] <- chl_week # save into matrix (row = pixel, column = week)
}
#-----


#-----
## 3) interpolate all time series
nbx <- dim(chl_clim)[2]
goodp <- which(rowSums(is.na(chl_clim)) <= dim(chl_clim)[2]-2) # Need at least two points
chl_na <- is.na(chl_clim) 
chl_clim_int <- matrix(nrow=dim(chl_clim)[1],ncol=dim(chl_clim)[2]) 
chl_clim[goodp,] <- t(apply(chl_clim[goodp,],1,function(x) approx(1:nbx,x,xout = 1:nbx, rule=2:2)$y)) # repeat latest values to extrap
#-----


#-----
## 4) Evaluate which annual time series have to be taken into account
#
nblim <- 4 # minimum of weeks without data between two weeks with data
nbint <- 23 # maximum of point interpolated
lag <- 0
goody <- t(apply(chl_na,1,function(x) selectTS(x,nweeks,nblim,nbint,lag)))
#-----


#-----
## 5) Normalized annual time series have to be taken into account
#
lag <- 0
chl_norm <- t(apply(chl_clim,1,function(x) normTS(x,nweeks,lag)))
chl_norm[!goody] <- NA
chl_norm[!goody] <- NA
#-----


#-----
## 6) Switch weeks: annual cycle is from july to july
#
chl_norm <- chl_norm[,c(24:46,1:23)]
chl_na   <- chl_na[,c(24:46,1:23)]
chl_clim <- chl_clim[,c(24:46,1:23)]
DOY8     <- DOY8[c(24:46,1:23)]
#-----

#-----
## 7) Build the dataset and save it
#
dataset <- c()
dataset$DOY8     <- DOY8 # years to be taken into
dataset$LON      <- LON # rows = longitude of each pixel
dataset$LAT      <- LAT # rows = latitude of each pixel
dataset$CHL      <- chl_clim # rows = pixels, column = 8-day weeks (x46)
dataset$CHL_na   <- chl_na # Locations of previous NAs
dataset$CHL_norm <- chl_norm # years to be taken into
save(dataset, file = paste(folder,"RedSea_climato_1998_2019_8D_CCI_new.rdata",sep="")) # save rdata into the same folder
#-----