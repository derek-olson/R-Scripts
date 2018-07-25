library(stats)
library(mvoutlier)
library(scatterplot3d)

#Read in Clusters
clusters = read.csv(file = "D:/Region2/NNF/NNFGAClusters50_04212016.csv", header = TRUE)
clustNum10 = clusters[,c("GA", "SPATIAL_ID","X10_cluster_scaled")]
#rm(clusters)

#Read in zonal stats
zonalStats = read.csv(file = "D:/Region2/NNF/Imagery/NNF_Admin_Images/NNF_Zonal_Stats_noNA.csv", header = TRUE)

#Combine clusters and zonal stats
combined = merge(clustNum10,zonalStats,by.x="SPATIAL_ID",by.y="SPATIAL_ID", all=FALSE)

#Calculate principal components and mahalanobis distance for each cluster. Populate a list with spatial ID and distance.
#Initiate empty lists
List = 8
outliers = -1578785561
ga = unique(clusters[,1])
NumClust = sort(unique(combined[,3]))
colorDF = merge(x = clustNum10, y = List, by.x = 'SPATIAL_ID', by.y = 'subset2[, 1]')

for (j in ga)
  {
    for (i in NumClust )
    {
      #Subset the data
      subset = combined[combined[,2]==j,]
      subset2 = subset[subset[,3]==i,]
      sd = subset2[,4:123]
      #Color variables for 3d scatter plots added after List is created
#       color1 = colorDF[colorDF[,2]==j,]
#       color2 = color1[color1[,3]==i,]
      
      #Do PCA
      pca = prcomp(sd,scale=TRUE,retx=TRUE)
      test = summary(pca)
      test = test[6]
      test = unlist(test)
      indexVec = seq(3,length(test),3)
      test = test[indexVec]
      xVec = seq(1,length(test),1)
  
      #Plot cumulative importance
      #plot(xVec,test,type='l')
    
      dimPCA = dim(as.data.frame(pca[5]))[2]
      newData = as.data.frame(pca[5])[1:dimPCA]
      pcaData = as.data.frame(cbind(subset2[,1],newData))
      
      #Plot data
      plot(pcaData[2:3], main = paste("cluster", j, i))
      #scatterplot3d(x=color2[,4:6], color = color2$color, main = paste("cluster", j, i))
      
  
      #Determine and graph multivariate outliers
      out = color.plot(pcaData[2:3], quan=0.75)
      outlierList = cbind(pcaData[1],out$outliers)
      if (outliers == -1578785561){
        outliers = outlierList
      }else{
        outliers = rbind(outliers, outlierList)
      }
  
      #Calculate Mahalanobis distance
      mList = c()
      for (k in 1:dim(pcaData)[1])
      {
        split = pcaData[k,2:4]
        rest = pcaData[-k,2:4]
        mahal = mahalanobis(split,colMeans(rest),cov(rest))
        mList = c(mList, mahal)
      }
  
        distList = cbind(pcaData[,1:4], mList)
        if (List == 8)
      {
        List = distList
      } else{
        List =rbind(List, distList)
      }
    }
}

#Calculate quantiles 
quant1 = quantile(List$mList, probs=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), type=8)

quantyler = function(inNum)
{
  if (inNum <= quant1[1]){
    return(1)
  }else if (inNum <= quant1[2]){
    return(2)
  }else if (inNum <= quant1[3]){
    return(3)
  }else if (inNum <= quant1[4]){
    return(4)
  }else if (inNum <= quant1[5]){
    return(5)
  }else if (inNum <= quant1[6]){
    return(6)
  }else if (inNum <= quant1[7]){
    return(7)
  }else if (inNum <= quant1[8]){
    return(8)
  }else if (inNum <= quant1[9]){
    return(9)
  }else if (inNum <= quant1[10]){
    return(10)
  }
}
quantileList = sapply(List$mList, quantyler)

#Define colors by quantile value
rbpal = colorRampPalette(rev(c('red','yellow','green', 'blue','purple')))
List$color = rbpal(10)[quantileList]

#Plot data 2d and 3d for all clusters
#plot(List$x.PC1, List$x.PC2, col = List$color,pch=20,cex= 0.75)
scatterplot3d(x=List[2:4], color= List$color)

#Create histogram from Mahalanobis distances in 10 breaks.
#hist(List$mList,breaks = c(seq(0,10,1),1000000000),xlim=c(0,10))

#Add quantile values to table of spatial id's, principal components, and Mahalanobis distances
quantList = cbind(List,quantileList)
#Create histogram of true and false values by Mahalanobis distance
everything = merge(x= quantList, y= outliers, by.x='subset2[, 1]', by.y='subset2[, 1]')
hist(everything[everything[,8]==TRUE,5],breaks = c(seq(0,10,0.25),1000000000),xlim=c(0,10), ylim = c(0,1), col=rgb(0.8,0.1,0.1,0.5), main="", xlab="")
hist(everything[everything[,8]==FALSE,5],breaks = c(seq(0,10,0.25),1000000000),xlim=c(0,10),add=TRUE, col=rgb(0.1,0.1,0.8,0.5), main="")
title(main="Histogram of Outlier Frequency by Mahalanobis Distance", sub="Pink = Outlier, Purple = Not Outlier", xlab= "Mahalanobis Distance", ylab="Proportion of Observations") 

#Write Spatial ID, Principal Components, and Mahalanobis Distance to CSV
#write.csv(everything, file = "D:/Region2/NNF/ClusterByGA/NNF_SPID_PC_MD_outliers.csv", row.names=FALSE)

#######################################################################################################################

#linear model for NDVI vs Mahalanobis Distance
# NDVI=abs(zonalStats$NNF_Slope_NDVI_clip_1_Mean)
# NDVItable = cbind(zonalStats[1], NDVI)
# 
# distM=everything$mList
# distMtable=cbind(everything[1],distM)
# head(distMtable)
# 
# newTest=merge(x=NDVItable, y=distMtable, by.x = 'SPATIAL_ID', by.y = 'subset[, 1]')
# head(newTest)
# LinMod = lm(newTest[,2]~ newTest[,3])
# summary(LinMod)
# plot(LinMod)
# plot(newTest[,2], newTest[,3], xlim=c(0,.05), ylim=c(0,100))
# abline(lm(newTest[,2]~ newTest[,3]))
