#---------------------------------------------------------------
# select time series
#---------------------------------------------------------------

selectTS <- function(x,n,nblim,nbint,lag) {
  #
  # remove annual time series with less than expected measured data and too much interpolated data
  #
  #---INPUTS
  #
  # x: time series (TRUE = NA, FALSE = non-NA)
  # n: length of annual time series (number of column, e.g., 46 if 8day dataset)
  # nblim: minimum of weeks without data between two weeks with data
  # nbint: minimum of point interpolated
  # lag: nb of columns to avoid at the begining
  #
  #---OUTPUTS
  #
  # res: vector with TRUE or FALSE for each annual time series evaluate (TRUE = keep it)
  #
  res <- rep(FALSE,lag)
  for (j in seq(lag+1,length(x),n)) {
    res <- c(res, rep(max(diff(c(0, which(!x[j:(j+n-1)]), n+1))) < nblim & sum(!x) >= nbint,n))
  }
  if (length(res) > length(x)) {
    res <- res[1:length(x)]
  }
  return(res)
}

#---------------------------------------------------------------
# Normalize time series
#---------------------------------------------------------------

normTS <- function(x,n,lag) {
  #
  # remove annual time series with less than expected measured data and too much interpolated data
  #
  #---INPUTS
  #
  # x: time series (TRUE = NA, FALSE = non-NA)
  # n: length of annual time series (number of column, e.g., 46 if 8day dataset)
  # lag: nb of columns to avoid at the begining
  #
  #---OUTPUTS
  #
  # chl_norm: vector with normalized time series
  #
  chl_norm <- rep(NA,lag)
  for (j in seq(lag+1,length(x),n)) {
    chl_norm <- c(chl_norm, x[j:(j+n-1)]/max(x[j:(j+n-1)]))
  }
  return(chl_norm)
}

#---------------------------------------------------------------
# Match cluster numbers (i.e. names)
#---------------------------------------------------------------

match_clus <- function(clus_01, clus_02) {
  
  # Necessary information:
  #
  #     - clus01 and clus02: two vectors of integers indicating the group to which each row is allocated
  #     - The two vectors should have the same length (e.g. locations -lat & lon- should match)
  #     - The two vectors can contain NA values
  
  nclu <- max(clus_02, na.rm = T) # number of groups
  
  # build the confusion matrix
  conf <- c() # initialize confusion matrix
  for (n in 1:nclu) {
    conf <- c(conf, hist(clus_02[clus_01 == n], breaks = seq(.5,((nclu)+.5)), plot=FALSE)$count) # groups from clus_01 now in clus_02
  }
  conf <- matrix(conf,nrow = nclu,byrow = T)
  
  # modify the second cluster vector
  clus_02_new  <- clus_02 # initialize the new cluster vector
  for (n in 1:nclu) {
    pastcl <- which(conf == max(conf, na.rm = T), arr.ind = TRUE)[1,2]
    newcl <- which.max(conf[,pastcl])
    clus_02_new[clus_02 == pastcl] <- newcl
    conf[newcl,] <- NA
    conf[,pastcl] <- NA
  }
  
  # return results
  return(clus_02_new)
}


#---------------------------------------------------------------
# Silhouette analysis with a subset on the dataset
#---------------------------------------------------------------

si_analysis <- function(X, clus, subsetsize) {
  
  # Necessary information:
  #   -          X: The numeric matrix (the data matrix) used as input into the clustering function (cases*variables).
  #   -       clus: The vector of integers indicating the cluster to which each point (cases) is allocated
  #   - subsetsize: percentage of the dataset to be sampled (between 0 and 1; 0.01=1%, 0.1=10%, 0.2=20%...)
  
  nclu    <- max(clus, na.rm = T) # The number of clusters
  subloc  <- sample(1:dim(X)[1], round(subsetsize*dim(X)[1]), replace=F) # ramdomly subset database
  subX    <- X[subloc,] # subset time series
  subclus <- clus[subloc] # subset clusters
  
  distX <- as.matrix(dist(subX)) # distance matrix between all the time series of the subset
  
  # estimate the silhouette value of each point (time series)
  siX <- c()
  for (l in 1:dim(subX)[1]) {
    value <- aggregate(distX[l,],list(subclus),mean,simplify = T)
    a <- value[subclus[l],2]
    b <- min(value[subclus[l] != 1:nclu,2])
    s <- (b - a)/max(a,b)
    siX <- c(siX,s)
  }
  
  # order silhouette values (by cluster and from the maximum to the minimum values)
  all_idx <- c()
  for (n in 1:nclu) {
    idx <- which(n == subclus)
    idx <- idx[order(siX[idx],decreasing = T)]
    all_idx <- c(all_idx,idx) 
  }
  subloc <- subloc[all_idx]
  subX <- subX[all_idx,]
  subclus <- subclus[all_idx]
  siX <- siX[all_idx]
  
  # group results into a list
  result <- list(si = siX, cluster = subclus, X = subX, index = subloc)
  return(result)
}