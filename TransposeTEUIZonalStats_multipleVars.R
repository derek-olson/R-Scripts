#######################################################################################################################
##Load the Library
#######################################################################################################################
#install.packages("reshape2")
library(reshape2)

#######################################################################################################################
##Load the data - FIRST TWO Columns OF THE TEUI CSV EXPORT MAY NEED TO BE REMOVED DEPENDING ON IF MAPUNIT WAS SPECIFIED
#######################################################################################################################
data = read.csv(file = '//166.2.126.25/teui1/4_Derek/KP_TEUI_ZonalStats.csv', header = T, sep = ',')

#######################################################################################################################
##Go through each column and transpose the data then merge all the intermediate data frames back together
#######################################################################################################################
colNum = dim(data)[2]
rowNum = length(unique(data[,1]))
dfAll = data.frame(matrix(NA, nrow = rowNum, ncol = 1))

for (i in 4:colNum)
  {
  sub = data[,c(1,2,3,i)]  
  df = dcast(sub, FeatureId ~ Raster + RasterBand)
  varName = colnames(sub[4])
  colnames(df) <- paste(colnames(df),varName, sep = "_") 
  dfAll = cbind(dfAll, df)
}

#######################################################################################################################
##Export the data
#######################################################################################################################
#write.csv(x = dfAll, file = 'H:/workspace/TEUI/GMUG/Zonal/NewZonalStats/GMUG_LTA_draft_attributes_All.csv')
