# The plot showing silhouette scores
#
# It produces the Figure S1 (supplementary informations)
#
# The silhouette scores are from the script "/script/silhouette_analysis.R".
# Run "silhouette_analysis.R" and do not remove objects from the environment
#

library(ggplot2)
library(gridExtra)

col <- c("#feb24c","#c994c7","#9ecae1","#3182bd","#31a354","#e31a1c", "#fb9a99", "#b2df8a")

# Silhouette plot with 3 and 4 clusters
n <- 2
SI_3 <- data.frame(id = 1:length(subclus[,n]), si = siX[,n], cluster = subclus[,n])
p1 <- ggplot(SI_3) +
  geom_bar(aes(x=id, y=si, fill=factor(cluster)), stat="identity") +
  scale_fill_manual(values = col[1:(n+1)]) +
  labs(y="Silhouette value", x="", fill="Clusters") + theme(axis.text.x = element_blank())

n <- 3
SI_4 <- data.frame(id = 1:length(subclus[,n]), si = siX[,n], cluster = subclus[,n])
p2 <- ggplot(SI_4) +
  geom_bar(aes(x=id, y=si, fill=factor(cluster)), stat="identity") +
  scale_fill_manual(values = col[1:(n+1)]) +
  labs(y="Silhouette value", x="", fill="Clusters") + theme(axis.text.x = element_blank())


# Changes in the average silhhouette value associated with each cluster and when increasing the number of clusters (from 2 to 7 clusters)
total_siX <- data.frame(SI=colMeans(siX),C=2:7)
tbl <- array(dim = c(dim(clusters)[2]+1,dim(clusters)[2]))
for (n in 1:dim(clusters)[2]) {
  res <- aggregate(siX[,n], list(subclus[,n]),mean)
  tbl[res[,1],n] <- res[,2]
}

grp_siX <- data.frame(Clusters=as.character(rep(c("4","1","3","2","5","6","7"),6)), C = sort(rep(c(2,3,4,5,6,7),7)), SI = as.vector(tbl))
grp_siX <- data.frame(Clusters=as.character(rep(c(1:7),6)), C = sort(rep(c(2,3,4,5,6,7),7)), SI = as.vector(tbl))

p3 <- ggplot(data = grp_siX) +
  geom_path(aes(y=SI,x=C,colour=Clusters)) +
  geom_point(aes(y=SI,x=C,colour=Clusters)) +
  scale_colour_manual(values = col) +
  labs(y="Silhouette value") 


p4 <- ggplot(data = total_siX) +
  geom_path(aes(y=SI,x=C)) +
  geom_point(aes(y=SI,x=C)) +
  labs(y="Silhouette value")


# Influence of the subset size in the average silhouette value (a subset size of 30% was used to produce the figures above)
tbl <- c()
for (subsetsize in c(.01, .025, .05, .075, .1, .2, .3, .4, .5, .6, .7)) {
  print(subsetsize)
  res_sub <- si_analysis(tschl, clusters[,3], subsetsize)
  tbl <- cbind(tbl, aggregate(res_sub$si, list(res_sub$cluster),mean)[,2])
}

si_sub <- data.frame(si = c(tbl), Clusters = as.character(rep(1:4,11)), Percentage = sort(rep(c(1, 2.5, 5, 7.5, 10, 20, 30, 40, 50, 60, 70),4)))

p5 <- ggplot(data = si_sub) +
  geom_line(aes(x = Percentage, y = si, colour = Clusters)) +
  geom_point(aes(x = Percentage, y = si, colour = Clusters)) +
  scale_colour_manual(values = col) +
  labs(y="Silhouette value")

g <- grid.arrange(p1, p2, p3, p4, p5, nrow = 3)

ggsave('figures/Fig_S1.png',g,height=9,width=10)

