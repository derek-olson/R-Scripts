## San Juan Burn Area Update - Area Weighted Averaging

##Derek Olson, GTAC

####################################################################################################################
##load libraries- install if necessary
####################################################################################################################
library(dplyr)

###################################################################################################################
##Data used to find which polygons subpops attributes need to be attributed. Data was derived from ESRI using the tabulate intersection tool
###################################################################################################################
x = read.csv(file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/AreaWeightedAveraging/Tabulate_Intersection_02022018_2.csv', header = TRUE, sep = ",")
names(x) = c('SPATIAL_ID_NEW', 'SPATIAL_ID_OLD', 'AREA', 'PERCENTAGE')
###################################################################################################################
##Create list to find records less than and greate than 75%
###################################################################################################################
lt75 = x[x$PERCENTAGE<75,]
gt75 = x[x$PERCENTAGE>=75,]

###################################################################################################################
##find records in the less than 75% list that are also in the greater than 75% list in order to remove them
###################################################################################################################
inBoth = lt75$SPATIAL_ID_NEW %in% gt75$SPATIAL_ID_NEW
a = cbind(lt75, inBoth)
b = a[a$inBoth == FALSE,]

###################################################################################################################
##read in the subpops data DO I NEED TO FILTER OUT RECORDS THAT DID NOT MEET THE 75% THRESHOLD
###################################################################################################################
oldSubpops = read.csv(file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/AreaWeightedAveraging/AreaWeightedAveraging_Subpops_02022018_Export.csv', header = TRUE, sep = ",")

###################################################################################################################
##Get the subpops for the greater than 75% polygons
###################################################################################################################
oldSubpopsSubgroup = read.csv(file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/AreaWeightedAveraging/AreaWeightedAveraging_Subpops_subgroup_02022018_Export_2.csv', header = TRUE, sep = ",")
gt75_list = gt75[,1:2]
gt75_subpops = merge(x = gt75_list, y = oldSubpopsSubgroup, by.x = 'SPATIAL_ID_OLD', by.y = 'SPATIAL_ID')
gt75_subpops_out = gt75_subpops[,2:dim(gt75_subpops)[2]]
write.csv(x = gt75_subpops_out, file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/AreaWeightedAveraging/SubpopsForGreaterThan75PCTPolygons_02022018.csv')
###################################################################################################################
##subset the data and loop through each subset to calclaute the spatially adjusted cover
###################################################################################################################
new = unique(b$SPATIAL_ID_NEW)
old = b$SPATIAL_ID_OLD

df = data.frame(matrix(ncol=10, nrow = 0))
dfOut = data.frame(matrix(ncol=6, nrow = 0))

for (newID in new)
{
    print(newID) 
    b_subset = b[b$SPATIAL_ID_NEW == newID,]
    old = dim(b_subset)[1]
    
    for (j in 1:old)
      {
        oldID = factor(b_subset[j,2],levels=levels(oldSubpops[,1]))
        weight = b_subset[j,4]/100
        s = cbind(newID, oldSubpops[oldSubpops[,1]==oldID,], weight)
        newPlant_Cover = s$PLANT_COVER*s$weight
        spNewCov = cbind(s,newPlant_Cover)
        df = rbind(df,spNewCov)
     }
}

for (newID in new)
{
  print(newID) 
  b_subset = b[b$SPATIAL_ID_NEW == newID,]
  old = dim(b_subset)[1]
  
  for (j in 1:old)
  {
    oldID = factor(b_subset[j,2],levels=levels(oldSubpops[,1]))
    weight = b_subset[j,4]/100
    t = cbind(newID, oldSubpops[oldSubpops[,1]==oldID,], weight)
    newDead = t$DEAD_PCT*t$weight
    spNewDeadCov = cbind(t,newDead)
    dfOut = rbind(dfOut,spNewDeadCov)
  }
}
outDF1 = df %>% group_by(newID, GROUP_1, SPECIES_SYMBOL, TREE_SIZE_CODE, SHRUB_SIZE) %>% summarise(sum(newPlant_Cover))
names(outDF1) = c('Spatial_ID', 'GROUP_1', 'SPECIES_SYMBOL', 'TREE_SIZE_CODE', 'SHRUB_SIZE', 'PLANT_COVER')

outDF2 = dfOut %>% group_by(newID, GROUP_1, SPECIES_SYMBOL, TREE_SIZE_CODE, SHRUB_SIZE) %>% summarise(sum(newDead))
names(outDF2) = c('Spatial_ID', 'GROUP_1', 'SPECIES_SYMBOL', 'TREE_SIZE_CODE', 'SHRUB_SIZE', 'DEAD_PCT')

outDF3 = cbind(as.data.frame(outDF1), as.data.frame(outDF2$DEAD_PCT))
outDF = subset(outDF3, outDF3$PLANT_COVER >= 1)

###################################################################################################################
##evaluate the results by spatial id
###################################################################################################################
sumCheck = outDF %>% group_by(Spatial_ID) %>% summarise(sum(PLANT_COVER))
View(sumCheck)

###################################################################################################################
##write the output to csv
###################################################################################################################
write.csv(x = outDF, file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/AreaWeightedAveraging/Subpops_AreaWeightedAverages_02022018.csv')











