##################################################################################################
##Create DEM derivatives
##################################################################################################

##################################################################################################
##load libraries
##################################################################################################
library(raster)
library(RSAGA)
library(rgdal)

#mosaicClip = raster("H:/workspace/TEUI/GMUG/DEM/gmug_dem_mosaic_crop.img")

slope = terrain(x = dem, opt = 'slope', unit = 'degrees', neighbors = 8)

aspect = terrain(x = dem, opt = 'aspect', unit = 'radians', neighbors = 8, filename = "H:/workspace/TEUI/GMUG/segmentation/aspect.tif")
cos_aspect = cos(aspect)
sin_aspect = sin(aspect)
writeRaster(cos_aspect, datatype = 'INT1U', "H:/workspace/TEUI/GMUG/DEM/cos_aspect.img")
writeRaster(sin_aspect, datatype = 'INT1U', "H:/workspace/TEUI/GMUG/DEM/sin_aspect.img")

TRI = terrain(x = mosaicClip, opt = 'TRI', filename = "H:/workspace/TEUI/GMUG/DEM/TRI.img")

roughness = terrain(x = mosaicClip, opt = 'roughness', filename = "H:/workspace/TEUI/GMUG/DEM/roughness.img")

##################################################################################################
##Calculate TPI
##################################################################################################
##window size
w = 31 
##create matrix
m = matrix(1/(w^2-1), nc=w, nr=w)
m[ceiling(0.5 * length(m))] = 0
##run focal statistics
f = focal(mosaicClip, m)
TPI = mosaicClip - f
writeRaster(TPI, datatype = 'INT2U', "H:/workspace/TEUI/GMUG/DEM/tpi.img")

##################################################################################################
##Calculate SAGA Wetness #BROKEN
##################################################################################################
rsaga.get.modules()

env = rsaga.env("C:/Users/derekolson/Documents/saga-4.0.1_x64")

env$workspace = "//166.2.126.25/teui1/4_Ryan/R9/Superior_NF/Imagery/DEM_no_sinks"

rsaga.get.modules(lib = , env = env)

rsaga.sgrd.to.esri(in.sgrds = "SA_DEM1_focal5_focal5 [no sinks].sgrd", out.path =getwd(), format = "ascii", env = env) 

rsaga.esri.to.sgrd(in.grids = "gmug_dem_mosaic_crop.asc", in.path = "H:/workspace/TEUI/GMUG/DEM/")
rsaga.wetness.index(mosaicClip, "H:/workspace/TEUI/GMUG/DEM/swi.sgrd")

##################################################################################################
##Create trishade
##################################################################################################
slope_r = terrain(x = dem, opt = 'slope', unit = 'radians', neighbors = 8)
hill1 = hillShade(slope_r, aspect, 40, 30)
hill2 = hillShade(slope_r, aspect, 40, 150)
hill3 = hillShade(slope_r, aspect, 40, 270)
trishade = brick(hill3, hill2, hill1)
writeRaster(trishade, datatype = 'INT2U', "H:/workspace/TEUI/GMUG/DEM/trishade.img")