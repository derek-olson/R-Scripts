library(vegan)

#Read in the data
data = read.csv(file = "D:/Region2/NNF/ClusterByGA/SubPopsAll_Pivot_1000.csv", header = TRUE)

#Set the max number of clusters to plot
clustNum = 20

#Define min and max cluster sizes
minNumClusters = 1
maxNumClusters = 20

#Funtion to plot variance by cluster size
wssplot <- function(sd, nc=clustNum, seed=500){
  wss = -99
  for (k in 1:nc){
    set.seed(seed)
    wss[k] <- sum(kmeans(sd, centers=k)$withinss, iter.max = 100)}
    plot(1:nc, wss, type="b", xlab="Number of Clusters", ylab="Sum Cluster Variance", main = paste(i))}

# Create a list of unique values for each geographic area
ga = unique(data[,1])
df = -999925752735
#Loop through each ga and plot cluster size
for (i in ga)
  {
    print(i)
    sd = data[data$GA==i,3:86]
    wssplot(sd)
    
    outdf = data[data$GA==i,]
    for (j in minNumClusters:maxNumClusters)
    {
      set.seed(500)
      fit_kmeans = kmeans(sd,j, iter.max = 100)
      outdf = cbind(outdf,fit_kmeans$cluster)
      colnames(outdf)[dim(outdf)[2]] = paste(toString(j),"_cluster_scaled",sep = "")
    }
   if (df == -999925752735)
     {df = outdf
   } else {
        df = rbind(df, outdf)
      }
    
    
    #df = rbind(data, outdf2)
  }

write.csv(df, file = "D:/Region2/NNF/NNFGAClusters50_04212016.csv", row.names=FALSE)
