library(raster)
library(sdmpredictors)

sea_surface_temperature <- load_layers(c("BO_A1B_2100_sstmean")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
sea_surface_temperature.crop <- crop(sea_surface_temperature, ne.atlantic.ext) 

tempdata <- data.frame(cbind(coordinates(sea_surface_temperature.crop),values(sea_surface_temperature.crop)))
names(tempdata) <- c("Long","Lat","value")

xy <- tempdata[,c(2,3)]

spdf <- SpatialPointsDataFrame(coords = xy, data = tempdata,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

mydf[complete.cases(over(spdf, poly)), ]





ESI <- polygon(c(-63.5,-62.5,-62.5,-63.5), c(44.0,44.0, 44.9,44.9),)

coords <- SpatialPointsDataFrame(coordinates(annual_mean_temperature.crop))


bigextent <- extent(annual_mean_temperature.crop)

bigpolylims[bigpolylims$names=="ymin",2]
cds1 <- cbind(c(bigpolylims[bigpolylims$names=="xmin",2]+1,bigpolylims[bigpolylims$names=="xmax",2]-1,
                bigpolylims[bigpolylims$names=="xmax",2]-1,bigpolylims[bigpolylims$names=="xmin",2]+1), 
              c(bigpolylims[bigpolylims$names=="ymin",2]+1,bigpolylims[bigpolylims$names=="ymin",2]+1,
                bigpolylims[bigpolylims$names=="ymax",2]-1,bigpolylims[bigpolylims$names=="ymax",2]-1))
cds2 <- cbind(c(-63.5,-62.5,-62.5,-63.5), c(44.0,44.0, 44.9,44.9))


polys <- SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), 
                                                       data.frame(ID=c(1)))))

polys <- SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), 
                                                       Polygons(list(Polygon(cds2)), 2))),data.frame(ID=c(1,2)))

polys <- SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), 
                                                       Polygons(list(Polygon(cds2)), 2))),data.frame(ID=c(1,2)))



v <- extract(annual_mean_temperature.crop,polys,layer=1,nl=1)

tt=xyFromCell(annual_mean_temperature.crop,polys,layer=1,nl=1)


ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
annual_mean_temperature.crop <- crop(annual_mean_temperature, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(annual_mean_temperature.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Annual Mean Temperature 2050")


r <- raster(ncol=36, nrow=18)
r[] <- round(runif(ncell(r),1,10),digits=0)

# Create two polygons
cds1 <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
cds2 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
polys <- SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), 
                                                       Polygons(list(Polygon(cds2)), 2))),data.frame(ID=c(1,2)))

v <- extract(r, polys)
