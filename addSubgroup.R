data = read.csv(file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/AreaWeightedAveraging/Subpops_AreaWeightedAverages_02022018.csv' , header = T, sep = ",")
  
ids = unique(data$Spatial_ID)
SUBGROUP =c()
for (id in ids)
{
  print(id)
  spid = data[data$Spatial==id,]
  groups = unique(spid$GROUP_1)
  for (group in groups)
  {
    print(group)
    rec = subset(spid, GROUP_1==group)
    for(i in 1:dim(rec)[1])
    {
      print(i)
      sg = paste("R2-0", i, sep ="")
      print(sg)
      SUBGROUP = c(SUBGROUP,sg)
    }
  }
}
SUBGROUP_1 = as.data.frame(SUBGROUP)
out = cbind(data, SUBGROUP_1)

write.csv(x = out, file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/AreaWeightedAveraging/Subpops_AreaWeightedAverages_subgroup_02022018.csv')
