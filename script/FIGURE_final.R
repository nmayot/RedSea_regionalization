rm(list=ls()) # clear all variable
library(gridExtra)
library(ggplot2) # for plotting

load("data/CCI_4_2_RedSea/Climato_cmeans_1998_2019.rdata") # save rdata into the same folder

LON  <- dataset$LON
LAT  <- dataset$LAT
gp   <- dataset$gp
clus <- dataset$cluster
mb   <- dataset$mb

lon <- LON[gp]
lat <- LAT[gp]
col <- c("#c994c7","#3182bd","#9ecae1","#feb24c","#31a354")


####### FIGURE 01
# for the map
p1 <- ggplot() +
        geom_tile(aes(x = lon, y = lat, fill = factor(clus))) +
        scale_fill_manual(values = col) +
        coord_quickmap(xlim = c(32,44), ylim=c(10,30), clip = "off") +  # Prevents stretching when resizing
        theme_bw() +
        xlab("Longitude") +
        ylab("Latitude") +
        labs(fill = "Clusters") +
        geom_text(aes(x = 29, y = 30, label = "a)"), size = 5)


# for the time series
CHL <- dataset$CHL_norm
avg <- data.matrix(aggregate(CHL[gp,],list(clus),mean)[,2:47])
std <- data.matrix(aggregate(CHL[gp,],list(clus),sd)[,2:47])

DOY8 <- seq(1,366,8)
dtime <- (as.Date(paste("01-01-2000", sep=""),"%d-%m-%Y") + DOY8[24]+DOY8+3.5) - 2

for (ccc in 1:max(clus)) {
  txt_plot <- paste("TS_",toString(ccc)," <- ggplot() +",
          "geom_path(aes(x = dtime, y = avg[",toString(ccc),",]), color = col[",toString(ccc),"], size = 2) +",
          "geom_path(aes(x = dtime, y = avg[",toString(ccc),",]+std[",toString(ccc),",])) +",
          "geom_path(aes(x = dtime, y = avg[",toString(ccc),",]-std[",toString(ccc),",])) +",
          "xlab(\" \")+ ylab(\"CHL normalized to max\") +",
          "coord_cartesian(ylim = c(0, 1), xlim = c(dtime[1]-2,dtime[46]+2), expand = FALSE, clip = \"off\") +",
          "scale_x_date(date_breaks = \"1 month\", date_labels = \"%b\")" ,sep="")
  if (ccc == 1) {
    txt_plot <- paste(txt_plot, "+ geom_text(aes(x = dtime[1]-80, y = 1, label = \"b)\" ), size = 5)", sep="")
  }
  eval(parse(text= txt_plot))
  
}
g <- grid.arrange(p1, TS_1, TS_2, TS_3, TS_4, layout_matrix = rbind(c(1,2,3),c(1,4,6)))

ggsave('figures/Fig_01.png',g,height=5,width=13)


####### FIGURE 03
# For the membership map
p1 <- ggplot() +
        geom_tile(aes(x = lon, y = lat, fill = mb)) +
        coord_quickmap(xlim = c(32,44), ylim=c(10,30), clip = "off") +  # Prevents stretching when resizing
        theme_bw() +
        xlab("Longitude") +
        ylab("Latitude") +
        labs(fill = "Membership") +
        lims(fill = c(.3,1)) +
        geom_text(aes(x = 29, y = 30, label = "a)"), size = 5)

# For latitudinal occurence
lstep <- 2
lbreaks <- seq(11,31,lstep)
llabels <- lbreaks[1:(length(lbreaks)-1)]+lstep/2
grp <- cut(LAT,breaks=lbreaks,right=F,labels=llabels)

clus_lat <- c()
lat_mb <- c()
ccc_lat <- c()
for (ccc in 1:max(clus)) {
  txt_plot <- paste("mb_ccc <- aggregate(clus,list(grp[gp]),",
    "function(x) {100*(length(which(x == ccc))/length(x))})",sep="")
  eval(parse(text= txt_plot))
  clus_lat <- c(clus_lat, mb_ccc[,2])
  ccc_lat <- c(ccc_lat, rep(ccc,length(mb_ccc[,1])))
  lat_mb <- c(lat_mb, llabels[as.numeric(mb_ccc[,1])])
}
data_lat <- data.frame(lat = lat_mb, ocur = clus_lat, clus = ccc_lat)

plot_lat1 <- ggplot(data = data_lat) +
  geom_path(aes(y=lat, x = ocur, col = factor(clus))) +
  scale_color_manual(values = col) +
  ylab("Latitude") +
  xlab("Frequency of cluster (%)") +
  labs(col = "Cluster") +
  coord_cartesian(xlim = c(0,100), ylim=c(10,30), clip = "off") +
  geom_text(aes(x = -40, y = 30, label = "b)"), size = 5)

# For latitudinal membership
mb_lat <- c()
sd_lat <- c()
lat_mb <- c()
ccc_lat <- c()
for (ccc in 1:max(clus)) {
  f <- which(clus == ccc)
  txt_plot <- paste("mb_ccc <- aggregate(mb[f],list(grp[gp[f]]),mean,na.rm=T)",sep="")
  eval(parse(text= txt_plot))
  mb_lat <- c(mb_lat, mb_ccc[,2])
  lat_mb <- c(lat_mb, llabels[as.numeric(mb_ccc[,1])])
  txt_plot <- paste("mb_ccc <- aggregate(mb[f],list(grp[gp[f]]),sd,na.rm=T)",sep="")
  eval(parse(text= txt_plot))
  sd_lat <- c(sd_lat, mb_ccc[,2])
  ccc_lat <- c(ccc_lat, rep(ccc,length(mb_ccc[,1])))
}
data_lat <- data.frame(lat = lat_mb, mb = mb_lat, clus = ccc_lat, sdmax = mb_lat+sd_lat, sdmin = mb_lat-sd_lat)

plot_lat2 <- ggplot(data = data_lat) +
  geom_path(aes(y=lat, x = mb, col = factor(clus)), show.legend = FALSE) +
  scale_color_manual(values = col) +
  ylab(NULL) +
  xlab("Probability of membership ") +
  coord_cartesian(xlim = c(.3,1), clip = "off") +
  geom_text(aes(x = 0.15, y = 30, label = "c)"), size = 5)

g <- grid.arrange(p1, plot_lat1, plot_lat2,layout_matrix = rbind(c(1,1,2,3),c(1,1,2,3)))

ggsave('figures/Fig_03.png',g,height=5,width=13)


####### FIGURE 04
# Climatological cluster + individual annual cycle - row values 
rm(list=ls()) # clear all variable
col <- c("#c994c7","#3182bd","#9ecae1","#feb24c","#31a354")
load("data/CCI_4_2_RedSea/Climato_cmeans_1998_2019.rdata") # save rdata into the same folder
CHL <- dataset$CHL
gp   <- dataset$gp
clus <- dataset$cluster
avg <- data.matrix(aggregate(CHL[gp,],list(clus),mean)[1:4,2:47])
avg_1 <- avg[1,]
avg_2 <- avg[2,]
avg_3 <- avg[3,]
avg_4 <- avg[4,]

load("data/CCI_4_2_RedSea/RedSea_annual_1998_2019_8D_CCI.rdata") # load dataset (row = pixels, column = variables)
yyyy_wtd <- 1998:2018
CHL  <- dataset$CHL
yyyy <- dataset$yyyy
DOY  <- dataset$DOY
avg_yyyy <- array(NA,dim = c(4,46,length(yyyy_wtd))) # Matrix to save annual FCM results 

for (y in yyyy_wtd) {
  idx <- which(yyyy == y & DOY == 185) # index first column to be considered
  idx <- idx:(idx+45) # all 46 columns to be used as variables (46 8-day weeks = 1 year)
  avg_yyyy[,,y == c(yyyy_wtd)] <- data.matrix(aggregate(CHL[gp,idx],list(clus),mean,na.rm=T)[,2:47])
}

grp1 <- c(1998:2001,2012:2018)
grp2 <- c(2002:2011)
years <- sort(rep(yyyy_wtd,46))
grp <- rep(1,length(years))
grp[years %in% grp2] <- 2

DOY8 <- seq(1,366,8)
dtime <- (as.Date(paste("01-01-2000", sep=""),"%d-%m-%Y") + DOY8[24]+DOY8+3.5) - 2
ytime <- rep(dtime,dim(avg_yyyy)[3])
data_yyyy <- data.frame(CHL_1 = as.vector(avg_yyyy[1,,]),
                        CHL_2 = as.vector(avg_yyyy[2,,]),
                        CHL_3 = as.vector(avg_yyyy[3,,]),
                        CHL_4 = as.vector(avg_yyyy[4,,]),
                        yyyy = years, ytime = ytime, grp = grp)


txt_sub <- "g <- grid.arrange("
for (ccc in 1:4) {
  txt_plot <- paste("TS_",toString(ccc)," <- ggplot() +",
                    "geom_path(data = data_yyyy, aes(x = ytime, y = CHL_",toString(ccc),", group = factor(yyyy), color = factor(grp))) +",
                    "geom_path(aes(x = dtime, y = avg_",toString(ccc),"), color = col[",toString(ccc),"], size = 2) +",
                    "xlab(\"\")+ ylab(bquote(\"CHL (mg\"~m^-3*\")\")) + labs(color =\"Group\") +",
                    "coord_cartesian(xlim = c(dtime[1]-6,dtime[46]+2), expand = FALSE) +",
                    "scale_x_date(date_breaks = \"1 month\",date_labels = \"%b\")",sep="")
  
  eval(parse(text= txt_plot))
  txt_sub <- paste(txt_sub,"TS_",toString(ccc),",",sep="")
}

txt_sub <- paste(txt_sub,"nrow=",toString(2),",ncol=",toString(2),")",sep="")
eval(parse(text= txt_sub))

ggsave('figures/Fig_04.png',g,height=5,width=13)


####### FIGURE 05
# For annual maps
rm(list=ls()) # clear all variable
load("data/yyyy_clus_1998_2019.rdata") # save rdata into the same folder
yyyy_clus <- 1998:2018
res <- dataclus$yyyy_clus
lon <- dataclus$LON
lat <- dataclus$LAT
col <- c("#9ecae1","#feb24c")

for (y in yyyy_clus) {
  gp <- which(!is.na(res[,which(yyyy_clus==y)]))
  clus <- res[gp,which(yyyy_clus==y)]
  txt_plot <- paste("clus_",toString(y)," <- res[,which(yyyy_clus==y)]",sep="")
  eval(parse(text= txt_plot))
  
  txt_plot <- paste("map_",toString(y)," <- ggplot() + geom_tile(aes(x = lon, y = lat, fill = factor(clus_",toString(y),")))",
                    "+ scale_fill_manual(values = col,guide=F) + coord_quickmap(xlim = c(32,40), ylim=c(20,30)) +",
                    "theme_void() + borders(fill=\"grey90\",colour=\"grey\") +",
                    "annotate(\"text\", x = 34, y = 21, label =",toString(y),")",
                    sep="")
  eval(parse(text= txt_plot))
}

txt_sub <- "g <- grid.arrange("
for (y in yyyy_clus) {
  txt_sub <- paste(txt_sub,"map_",toString(y),",",sep="")
}

# average annual map cluster
res <- dataclus$yyyy_clus
clus <- unlist(apply(res,1, function(x) as.numeric(names(sort(table(x),decreasing=T)[1]))))
gp <- which(!is.na(rowMeans(res,na.rm=T)))
lon_avg <- dataclus$LON[gp]
lat_avg <- dataclus$LAT[gp]
col <- c("#9ecae1","#feb24c")

p_2 <- ggplot() + geom_tile(aes(x = lon_avg, y = lat_avg, fill = factor(clus))) +
  scale_fill_manual(values = col,guide=F) + coord_quickmap(xlim = c(32,40), ylim=c(20,30)) +
  borders(fill="grey90",colour="grey") + theme_bw() + xlab("Longitude") + ylab("Latitude")

txt_sub <- paste(txt_sub,"p_2, layout_matrix = rbind(c(1,2,3,4,5,6,7,22,22,22),c(8,9,10,11,12,13,14,22,22,22),c(15,16,17,18,19,20,21,22,22,22)))",sep="")
eval(parse(text= txt_sub))

ggsave('figures/Fig_05.png',g,height=5,width=12)


####### FIGURE 06
# Average annual cycle + all individual cycle - CHL_norm
rm(list=ls()) # clear all variable
load("data/yyyy_clus_1998_2019.rdata") # save rdata into the same folder
CHL <- dataclus$yyyy_centers
years <- sort(rep(1998:2018,46))
DOY8 <- seq(1,366,8)
dtime <- (as.Date(paste("01-01-2000", sep=""),"%d-%m-%Y") + DOY8[24]+DOY8+3.5) - 2
ytime <- rep(dtime,dim(CHL)[3])
data_yyyy <- data.frame(CHL_3 = as.vector(CHL[1,,]), CHL_4 = as.vector(CHL[2,,]), yyyy = years, ytime = ytime)
avg_3 <- rowMeans(CHL[1,,])
avg_4 <- rowMeans(CHL[2,,])
col <- c("#c994c7","#3182bd","#9ecae1","#feb24c","#31a354")

txt_sub <- "g <- grid.arrange("
for (ccc in 3:4) {
  txt_plot <- paste("TS_",toString(ccc)," <- ggplot() +",
                    "geom_path(data = data_yyyy, aes(x = ytime, y = CHL_",toString(ccc),", group = factor(yyyy))) +",
                    "geom_path(aes(x = dtime, y = avg_",toString(ccc),"), color = col[",toString(ccc),"], size = 2) +",
                    "xlab(\"\")+ ylab(\"CHL normalized to max\") +",
                    "coord_cartesian(ylim = c(0, 1), xlim = c(dtime[1]-6,dtime[46]+2), expand = FALSE) +",
                    "scale_x_date(date_breaks = \"1 month\",date_labels = \"%b\")",sep="")
  
  eval(parse(text= txt_plot))
  txt_sub <- paste(txt_sub,"TS_",toString(ccc),",",sep="")
}

txt_sub <- paste(txt_sub,"nrow=",toString(1),",ncol=",toString(2),")",sep="")
eval(parse(text= txt_sub))  

ggsave('figures/Fig_06.png',g,height=5,width=13)


####### FIGURE 07
# Average annual cycle + individual annual cycle - row values 
rm(list=ls()) # clear all variable
load("data/CCI_4_2_RedSea/RedSea_annual_1998_2019_8D_CCI.rdata") # load dataset (row = pixels, column = variables)
load("data/yyyy_clus_1998_2019.rdata") # save rdata into the same folder
col <- c("#c994c7","#3182bd","#9ecae1","#feb24c","#31a354")
CHL  <- dataset$CHL
yyyy <- dataset$yyyy
DOY  <- dataset$DOY
LAT  <- dataset$LAT
avg_yyyy <- array(NA,dim = c(2,46,21)) # Matrix to save annual FCM results 

for (y in 1998:2018) {
  idx <- which(yyyy == y & DOY == 185) # index first column to be considered
  idx <- idx:(idx+45) # all 46 columns to be used as variables (46 8-day weeks = 1 year)
  cluster <- dataclus$yyyy_clus[,y == c(1998:2018)]
  avg_yyyy[,,y == c(1998:2018)] <- data.matrix(aggregate(CHL[,idx],list(cluster),mean)[,2:47])
}

avg_3 <- rowMeans(avg_yyyy[1,,])
avg_4 <- rowMeans(avg_yyyy[2,,])

years <- sort(rep(1998:2018,46))
DOY8 <- seq(1,366,8)
dtime <- (as.Date(paste("01-01-2000", sep=""),"%d-%m-%Y") + DOY8[24]+DOY8+3.5) - 2
ytime <- rep(dtime,dim(avg_yyyy)[3])

yyyy_wtd <- 1998:2018
grp1 <- c(1998:2001,2009,2011:2018)
grp2 <- c(2002:2008, 2010)
years <- sort(rep(yyyy_wtd,46))
grp <- rep(1,length(years))
grp[years %in% grp2] <- 2

data_yyyy <- data.frame(CHL_3 = as.vector(avg_yyyy[1,,]), CHL_4 = as.vector(avg_yyyy[2,,]), yyyy = years, ytime = ytime, grp = grp)


txt_sub <- "g <- grid.arrange("
for (ccc in 3:4) {
  txt_plot <- paste("TS_",toString(ccc)," <- ggplot() +",
                    "geom_path(data = data_yyyy, aes(x = ytime, y = CHL_",toString(ccc),", group = factor(yyyy), color = factor(grp))) +",
                    "geom_path(aes(x = dtime, y = avg_",toString(ccc),"), color = col[",toString(ccc),"], size = 2) +",
                    "xlab(",noquote("\"\""),")+ ylab(",noquote("\"CHL (mg\"~m^-3*\")\""),") + labs(color =",noquote("\"Group\""),") +",
                    "coord_cartesian(ylim = c(0, .6), xlim = c(dtime[1]-6,dtime[46]+2), expand = FALSE) +",
                    "scale_x_date(date_breaks = ",noquote("\"1 month\""),",date_labels = ",noquote("\"%b\""),")",sep="")
  
  eval(parse(text= txt_plot))
  txt_sub <- paste(txt_sub,"TS_",toString(ccc),",",sep="")
}

txt_sub <- paste(txt_sub,"nrow=",toString(1),",ncol=",toString(2),")",sep="")
eval(parse(text= txt_sub))

ggsave('figures/Fig_07.png',g,height=5,width=13)


####### FIGURE S2
# for annual maps of data availability
rm(list=ls()) # clear all variable
load("data/CCI_4_2_RedSea/RedSea_annual_1998_2019_8D_CCI.rdata") # Name of the dataset with annual times series
CHL <- dataset$CHL_norm # normalized times series

yyyy_clus <- 1998:2018 # years to be cluterized

DOY <- dataset$DOY # Day of each column
yyyy <- dataset$yyyy # year of each column
res_clus <- matrix(nrow=nrow(CHL),ncol=length(yyyy_clus)) # Matrix to save annual FCM results 
lon  <- dataset$LON
lat  <- dataset$LAT
col = c("#de2d26","#636363")

for (y in yyyy_clus) {
  
  idx <- which(yyyy == y & DOY == 185) # index first column to be considered
  gp <- which(!is.na(CHL[,idx])) # all rows to be used as observation (non-NA pixel and north of 20N)
  res <- rep(2,dim(CHL)[1])
  res[gp] <- 1
  txt_plot <- paste("clus_",toString(y)," <- res",sep="")
  eval(parse(text= txt_plot))
  
  txt_plot <- paste("map_",toString(y)," <- ggplot() + geom_tile(aes(x = lon, y = lat, fill = factor(clus_",
                    toString(y),")))",
                    "+ scale_fill_manual(values=col, guide=F) + coord_quickmap(xlim = c(32,44), ylim=c(10,30)) +",
                    "theme_void() + borders(fill=",noquote("\"grey90\""),",colour=",noquote("\"grey\""),") +",
                    "annotate(",noquote("\"text\""),", x = 34, y = 21, label =",toString(y),")",
                    sep="")
  eval(parse(text= txt_plot))
}

txt_sub <- "g <- grid.arrange("
for (y in yyyy_clus) {
  txt_sub <- paste(txt_sub,"map_",toString(y),",",sep="")
}
txt_sub <- paste(txt_sub,"nrow=",toString(3),",ncol=",toString(7),")",sep="")
eval(parse(text= txt_sub))

ggsave('figures/Fig_S2.png',g,height=10,width=10)
