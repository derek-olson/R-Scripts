a = replicate(4, rnorm(3, 15, 8))
b = replicate(4, rnorm(3, 22, 8)) 
c = replicate(4, rnorm(3, 29, 8))
x = rbind(a,b,c)
x
myK = kmeans(x, centers=4, iter.max=10, nstart=10)
myK
plot(x, col = 1)

voronoipolygons <- function(myK) {
  require(deldir)
  require(sp)
  if (.hasSlot(x, 'coords')) {
    crds <- x@coords  
  } else crds <- x
  z <- deldir(crds[,1], crds[,2])
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP <- SpatialPolygons(polys)
  voronoi <- SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1],
                                                          y=crds[,2], row.names=sapply(slot(SP, 'polygons'), 
                                                                                       function(x) slot(x, 'ID'))))
}

library(rgdal)
v <- voronoipolygons(myKm)

plot(v, add=T)