#This script uses the nearest neighbor determination fom the San Juan Imputation script to join the collapsed
#subpopulations records to corresponding polygon

#####################################################################################################
##Load libraries
#####################################################################################################
library(dplyr)
#####################################################################################################
##Read in the data
#####################################################################################################
kval = read.csv(file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/Imputation/GNN_imputation_01182018_allPolys.csv', header = TRUE, sep = ",")
FID_SPID = read.csv(file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/Imputation/fid_spatialID_01172018.csv', header = TRUE, sep = ",")
Subpops = read.csv(file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/Imputation/ReferenceSubpops_12132017.csv', header = TRUE, sep = ",")

#####################################################################################################
##Returns the unique Spatial ID for the polygon
#####################################################################################################
join = merge(x = kval, y = FID_SPID, by.x = "zstatfid", by.y = "zstatfid")
join1 = data.frame(join$zstatfid, join$SPATIAL_ID)
names(join1) = c('FID', 'SPATIAL_ID_unique')

#####################################################################################################
##Returns the Spatial ID of the polygon whose subpops will be used
#####################################################################################################
join2 = merge(x = kval, y = FID_SPID, by.x = "k3", by.y = "zstatfid")
join3 = data.frame(join2$zstatfid, join2$SPATIAL_ID)
names(join3) = c('FID', 'SPATIAL_ID_subpops')

#####################################################################################################
##Combine the Spatial Id's
#####################################################################################################
SPIDs = merge(x = join1, y = join3, by.x = 'FID', by.y = 'FID')

#####################################################################################################
##Get the subpops for each polygon
#####################################################################################################
allSubpops = merge(x = SPIDs, y = Subpops, by.x = 'SPATIAL_ID_subpops', by.y = 'SPATIAL_ID')
#write.csv(x = allSubpops, file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/Imputation/GNN_Imputed_Subpops_K3_existingRemoved_01192018.csv')
#####################################################################################################
##Export table 
#####################################################################################################
# impGrpData = read.csv(file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/Imputation/ImputationGroupRevised_01172018.csv', header = TRUE, sep = ",")
# testList = impGrpData[impGrpData$ImputeTo == 'VEGCOV field data', c(1,4)]
# list = as.list(testList$zstatfid)
# testOut = allSubpops[!allSubpops$FID %in% list, ]
# 
# write.csv(x = allSubpops, file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/Imputation/GNN_Imputed_Subpops_K1_01182018.csv')

#####################################################################################################
##Create crosswalk
#####################################################################################################
crsswlk = allSubpops[,c(1:3)]
crsswlk = crsswlk %>% distinct(SPATIAL_ID_subpops, FID, SPATIAL_ID_unique)
write.csv(x = crsswlk, file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/Imputation/crosswalk_01182018.csv')

#####################################################################################################
##Export table for polygons not already existing in the GDB 
#####################################################################################################
z = read.csv(file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/Imputation/AllSubpopsToImportIntoGDB_01182018.csv', header = TRUE, sep = ",")
noData = read.csv(file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/Imputation/polygonsToNotAddSubpopsRecordsFor_01182018.csv', header = TRUE, sep = ",")
testList = noData$SPATIAL_ID
testOut = z[!z$SPATIAL_ID %in% testList, ]
write.csv(x = testOut, file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/Imputation/GNN_Imputed_Subpops_K1_existingRemoved_01182018.csv')



