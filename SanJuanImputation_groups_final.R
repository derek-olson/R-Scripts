## San Juan Burn Area Update - Imputation

##Derek Olson, GTAC

#This script determines the 3 nearest reference polygons for each polygon requiring imputation and flags outliers.
##An output text file is created for each group of fires. Those text files were combined into 1 in Excel. 
####################################################################################################################
##load libraries- install if necessary
####################################################################################################################
#library(VSURF)
library(yaImpute)
library(randomForest)
library(vegan)
####################################################################################################################
##load datasets
####################################################################################################################
impGrpData = read.csv(file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/Imputation/ImputationGroupRevised_01172018.csv', header = TRUE, sep = ",")
md = read.csv(file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/ZonalStats/ModelDataset_12182017.csv', header = TRUE, sep = ",")
yvals = read.csv(file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/Imputation/CollapsedYvals_12062017.csv', header = TRUE, sep = ",")
FID_SPID = read.csv(file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/Imputation/fid_spatialID_01172018.csv', header = TRUE, sep = ",")

####################################################################################################################
##Set up the data
####################################################################################################################
yvals = merge(x = FID_SPID, y = yvals, by.x = 'SPATIAL_ID', by.y = 'SPATIAL_ID')
yvals = yvals[,c(2,1,3:30)]
rownames(yvals) = NULL

firegroup = impGrpData[,c(1,5,7,10)]
yvals = merge(x = firegroup, y = yvals, by.x = 'SPATIAL_ID', by.y = 'SPATIAL_ID')
#yvals = yvals[complete.cases(yvals[,]),]
yvals = yvals[order(yvals$zstatfid),]

xvals = merge(x = FID_SPID, y = md, by.x = 'zstatfid', by.y = 'FID')
xvals = merge(x = firegroup, y = xvals, by.x = 'SPATIAL_ID', by.y = 'SPATIAL_ID')
xvals = xvals[order(xvals$zstatfid),]
rownames(xvals) = NULL

####################################################################################################################
##remove correlated variables - This is handled by the yai package in the loop
####################################################################################################################
# xcor = xvals[,5:dim(xvals)[2]]
# tmp = cor(xvals[,5:dim(xvals)[2]])
# tmp[upper.tri(tmp)] = 0
# diag(tmp) = 0
# data.new = xcor[,!apply(tmp,2,function(x) any(x>0.75))]
# xvals = cbind(xvals[,1:4], data.new)
####################################################################################################################
##find most important variables using vsurf
##most dominant species by cover was calculated for each polygon and used for variable importance estimation
##this takes a long time to run and did not see model improvement
####################################################################################################################
# ds = read.csv(file = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/ZonalStats/RefDataDomSpp.csv', header = TRUE, sep = ",")
# a = dim(ds)[2]
# domSp = whatsMax(ds[,2:a])
# b = dim(md)[2]
# impVars = cbind(domSp[,1], md[1:327,2:b])
# c = dim(impVars)[2]
# vars = VSURF(impVars[,3:c],impVars[,1],data = impVars,parallel=TRUE,ncores=4)
# PredsLimited = unlist(vars[2])
# PredsLeast = unlist(vars[3])
# xvsurf = x[,PredsLimited]
# head(xvsurf)

####################################################################################################################
##subset the data 
####################################################################################################################
group = sort(unique(impGrpData$fireGroup))

  for(i in group)
  {
    #Subset the data by group
    y2 = yvals[yvals$fireGroup==i,]
    x2 = xvals[xvals$fireGroup==i,]
    
    #ensure that correct records from the x and y tables line up 
    yjoin = as.data.frame(y2[,c(5)])
    colnames(yjoin)[1] = "zstatfid"
    xjoin = merge(x = x2, y = yjoin, by.x = 'zstatfid', by.y = 'zstatfid', all.x = TRUE, sort = FALSE)
    rownames(xjoin) = xjoin$zstatfid 
    ximp = xjoin[xjoin$ImputeTo==1,6:dim(xvals)[2]]
    xref = xjoin[xjoin$FieldData==1,6:dim(xvals)[2]]
    x = rbind(xref, ximp)
    
    #get table deminsions
    xdepth = dim(x)[1]
    yLength = dim(y2)[2]
    # ydepth = dim(yjoin)[1] + 1

    #format the y table
    rownames(y2) = y2$zstatfid
    y = y2[,6:yLength]
    y = y[,colSums(y[,])>10]
    colnames(y)[1] = "Ground Cover"
    #y[ydepth:xdepth,]=NA

    #find best variables
    vsel = varSelection(x = x, y = y, method = "addVars", yaiMethod = "gnn")
    bvars = bestVars(vsel, nbest = 40)
    print(bvars)
    plot(vsel)

    #use best vars
    x = x[,colnames<-(bvars)]

    #Impute using the bootstrapping parameter to determine error and choose a imputation method
    euc = yai(x = x, y = y, k = 1, method = 'euclidean', bootstrap = TRUE)
    mal = yai(x = x, y = y, k = 1, method = 'mahalanobis', bootstrap = TRUE)
    msn = yai(x = x, y = y, k = 1, method = 'msn', bootstrap = TRUE)
    gnn = yai(x = x, y = y, k = 1,  method = 'gnn', bootstrap = TRUE)

    #compare rmsd plots
    rmsd = compare.yai(euc, mal, msn, gnn) ##add rf when not bootstrapping
    apply(rmsd, 2, mean, na.rm = TRUE)
    #plot(rmsd)
    print(rmsd)

    #get relative error values for each species for each method
    print(i)
    eucRMSD = mean(rmsd$euc[!is.na(rmsd$euc.rmsdS)])
    print(eucRMSD)
    malRMSD = mean(rmsd$mal[!is.na(rmsd$mal.rmsdS)])
    print(malRMSD)
    msnRMSD = mean(rmsd$msn[!is.na(rmsd$msn.rmsdS)])
    print(msnRMSD)
    gnnRMSD = mean(rmsd$gnn[!is.na(rmsd$gnn.rmsdS)])
    print(gnnRMSD)

    #get the best method
    err = c(eucRMSD, malRMSD, msnRMSD, gnnRMSD)
    minErr = which.min(err)
    method = c("euc", "mal", "msn", "gnn")
    print(method[minErr])

    #get actual error values for each species for each method
    rmsdEUC = rmsd(euc)
    print(rmsdEUC)
    rmsdMAL = rmsd(mal)
    print(rmsdMAL)
    rmsdMSN = rmsd(msn)
    print(rmsdMSN)
    rmsdGNN = rmsd(gnn)
    print(rmsdGNN)

    #plot the imputed values for the chosen imputation method and group
    plot(gnn,vars=yvars(gnn)[1:9], sub = "Bootstrapped")
    plot(gnn,vars=yvars(gnn)[10:18], sub = "Bootstrapped")
    #plot(eval(parse(text = method[minErr])),vars=yvars(eval(parse(text = method[minErr])))[1:8], title(method[minErr], outer = TRUE, sub = "Bootstrapped"))
    #plot(eval(parse(text = method[minErr])),vars=yvars(eval(parse(text = method[minErr])))[10:18], title( method[minErr], outer = TRUE, sub = "Bootstrapped"))

    #get the 3 nearest neighbor ID's
    gnnID = yai(x = x, y = y, k = 3,  method = "gnn")
    getGNNID = cbind(foruse(gnnID),foruse(gnnID,kth=1),foruse(gnnID,kth=2),foruse(gnnID,kth=3))
    names(getGNNID) = c('use', 'dist','k1', 'dist1', 'k2', 'dist2', 'k3', 'dist3')
    zstatfid = as.numeric(row.names(getGNNID))
    getGNNID = cbind(zstatfid, getGNNID)

    nd = notablyDifferent(gnnID)
    ndTrgs = nd$notablyDifferent.trgs
    ndTrgs = as.data.frame(ndTrgs)
    rn = as.numeric(row.names(ndTrgs))
    ndTrgs = cbind(rn,ndTrgs)

    ed = merge(x = getGNNID, y = ndTrgs, by.x = 'zstatfid', by.y = 'rn', all.x = TRUE)

    outname = '//166.2.126.25/R2_VegMaps/San_Juan/Burn_Area_Regen_Modeling/Data/Imputation/GNN_imputation_01182018_allPolys'
    write.csv(x = ed, file = paste0(outname, i, ".csv"))
    }

