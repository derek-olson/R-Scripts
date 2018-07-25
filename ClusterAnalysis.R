#library(vegan)
data = read.csv(file = "D:/Region2/NNF/SubPopsExport_NotForested_ForCluster.csv", header = TRUE)
sd = as.matrix((subset.data.frame(data, select = c(AGCR:YUGL))))
print(sd)


wssplot <- function(sd, nc=50, seed=50){
  wss <- (nrow(sd)-1)*sum(apply(sd,2,var))
  for (i in 1:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(sd, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Average Cluster Variance")}



wssplot(sd)

##Copy Start 2
minNumClusters = 1
maxNumClusters = 50

###############################################################################################
#Do k-means on scaled df for minNumClusters through maxNumClusters and store cluster numbers


df3 = (sd)

outdf = data
for (i in minNumClusters:maxNumClusters)
  {
  set.seed(500)
  fit_kmeans = kmeans(df3,i)
  print(aggregate(df3, by=list(fit_kmeans$cluster),FUN=mean))
  outdf = data.frame(outdf,fit_kmeans$cluster)
  colnames(outdf)[dim(outdf)[2]] = paste(toString(i),"_cluster_scaled",sep = "")
  }

## Save results as csv
write.csv(outdf, file = "D:/Region2/NNF/NNFClusters50_03162016.csv", row.names=FALSE)
