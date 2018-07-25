##################################################################################################
##Get common extent of rasters
##################################################################################################
#load libraries
library(raster)
library(rgdal)
library(sp)
library(RStoolbox)

#set working directory and get rasters
workspace = "//166.2.126.25/teui1/4_Derek/SalmonChallis/LayersForSegmentation/30m/projected/rescale/final"
rasts = list.files(path = workspace, pattern = "*.img$", full.names = TRUE)
outNames = list.files(path = workspace, pattern = "*.img$", full.names = FALSE)

#Define the raster output projection and resolution
sr = "+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
resolution = 30

#project rasters on the fly
rast.list = list()

for (i in 1:length(rasts)){rast.list[i] = brick(rasts[i])}

extList = c()

for (rast in rast.list)
{
  ext = as.list(unlist(rast@extent))
  extdf = as.data.frame(rbind(c(ext[1],ext[3]),c(ext[2],ext[4])))
  srIn = rast@crs
  points = SpatialPoints(extdf, srIn)
  pointReproj = spTransform(points, sr)
  print(pointReproj@proj4string)
  extList = rbind(extList, unlist(list(pointReproj@coords)))
}

extList = as.data.frame(extList)
names(extList) = c("xmin", "xmax", "ymin", "ymax")
minVals = as.data.frame(apply(extList, 2, min))
maxVals = as.data.frame(apply(extList, 2, max))
vals = as.data.frame(cbind(minVals, maxVals))
colnames(vals) = c("min", "max")
cropRas = raster(xmn = vals[1,2], xmx = vals[2,1], ymn = vals[3,2], ymx = vals[4,1] , res = resolution, crs = sr)

#project rasters
dir.create(paste0(workspace, "/projected"))
dir.name = paste0(workspace, "/projected")

for (rast in rast.list)
{
  print(rast)
  rast.project = projectRaster(rast, to = cropRas, method = "bilinear")
  print(rast.project)
  dt = rast.project@file@datanotation
  out.name = rast.project@data@names
  writeRaster(rast.project, datatype = dt, bylayer = TRUE, filename = paste0(dir.name, "/", out.name, ".img"))
}

#deal with no data values
workspace = "//166.2.126.25/teui1/4_Derek/SalmonChallis/LayersForSegmentation/30m/projected"
rasts = list.files(path = workspace, pattern = "*.img$", full.names = TRUE)
outNames = list.files(path = workspace, pattern = "*.img$", full.names = FALSE)
rasStack = raster::stack(rasts)
plot(rasStack)

rasStack[is.na(calc(rasStack,fun = sum))] = NA
writeRaster(rasStack, filename = names(rasStack), bylayer = TRUE, format = "HFA")

##################################################################################################
##funtions for bit depth
##################################################################################################
workspace = "//166.2.126.25/teui1/4_Derek/SalmonChallis/LayersForSegmentation/30m/projected/rescale/final/projected"
rasts = list.files(path = workspace, pattern = "*.img$", full.names = TRUE)
outNames = list.files(path = workspace, pattern = "*.img$", full.names = FALSE)

rast.list = list()

for (i in 1:length(rasts)){rast.list[i] = brick(rasts[i])}

reScale = function(r){
  rasCalc = calc(r, fun=function(ras){ras * 1000})
  rescaleImage(x, xmin, xmax, forceMinMax = TRUE)
  outRas = paste0(r@data@names, "_16bit.img")
  writeRaster(rasCalc, filename = paste0(workspace, "/", outRas), datatype = 'INT2S')
  }

lapply(rast.list, FUN = reScale)

##################################################################################################
##function to resample rasters
##################################################################################################
#Choose whether to upsample or down sample the rasters
downSample = TRUE
#chose the factor by which to upsample or downsample
resampleFactor = 2
#choose the method to upsample
upMethod = mean
#choose the method to downsample

if (downSample == TRUE)
  {
    downSamp = function (x) {disaggregate(x, fact = resampleFactor, method = 'bilinear')}
      }else{upSamp = function (x) {aggregate(x, fact=resampleFactor, fun=upMethod, expand=TRUE, na.rm=TRUE, filename='', ...)
    }
  }
##################################################################################################
##clip rasters
##################################################################################################
bound = readOGR(dsn = "H:/workspace/TEUI/GMUG", layer = "GMUG_ALP_zone13_Buffer_envelope")
mosaicClip = crop(mosaic.project, extent(bound))

##################################################################################################
##raster principal components
##################################################################################################
rast = "H:/workspace/TEUI/GMUG/climate/GMUG_ClimateComposite2_1km.tif"

x = brick(rast)

b1 = raster(x, layer = 1)
b3 = raster(x, layer = 3)

r <- stack(b1,b3) 
r.val <- getValues(r)
idx <- which(!is.na(r.val)) 
pca <- princomp(r.val, cor=T)

ncomp <- 2 # first two principle components
r.pca <- r[[1:ncomp]]
for(i in 1:ncomp) { r.pca[[i]][idx] <- pca$scores[,i] } 

plot(r.pca)

writeRaster(x = r.pca, filename = "H:/workspace/TEUI/GMUG/climate/GMUG_Climate_PCA.img", dataType = 'INT2u')
