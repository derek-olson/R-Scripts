library(raster)

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
