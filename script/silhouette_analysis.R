##### To determine number of clusters
#
# First, load the climatological dataset.
# Second, clustering anaysis were performed with an increasing number of cluster (from 2 to 7).
# Finally, a silhouette analysis was performed for each of this clustering results.
#
# Results from these silhouette analysis could be plotted with the script "/script/FIGURE_silhouette_analysis.R"
#

rm(list=ls()) # clear all variable
library("e1071") # for clustering
source("script/NM_functions.R")

# --- Load dataset --------------------------------------------------
folder <- "data/CCI_4_2_RedSea/" # folder with data
filename <- "RedSea_climato_1998_2019_8D_CCI.rdata" # Name of the dataset
load(file = paste(folder,filename,sep="")) # load dataset (row = pixels, column = variables)

CHL <- dataset$CHL_norm # dataset to be cluterized
gp <- which(!is.na(CHL[,1])) # pixel (observations) to be clusterized


# --- Clustering --------------------------------------------------
tschl <- scale(CHL[gp,], scale=T, center=T) # scaled data
niter <- 1000 # number of iteration before to stop the Fuzzy c-means process

clusters <- c()
for (nclu in 2:7) {
  fcm      <- cmeans(tschl, centers=nclu, iter.max=niter, m=1.5) # Fuzzy c-means clustering
  clusters <- cbind(clusters, fcm$cluster)
  
  if( !exists("past_clus") ) {
    past_clus <- clusters[,dim(clusters)[2]]
  } else {
    past_clus <- clusters[,dim(clusters)[2]-1]
  }
  
  new_clus <- match_clus(past_clus, clusters[,dim(clusters)[2]])
  clusters[,dim(clusters)[2]] <- new_clus
}


# --- Silhouette analysis --------------------------------------------------
siX <- c()
subclus <- c()
subsetsize <- .3 # Use 30% of the dataset

for(n in 1:dim(clusters)[2]) { # loop on the number of clusters
  print(n)
  res <- si_analysis(tschl, clusters[,n], subsetsize)
  siX <- cbind(siX, res$si)
  subclus <- cbind(subclus, res$cluster)
}


