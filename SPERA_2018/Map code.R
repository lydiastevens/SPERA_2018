## Map script ----

#load libraries ---------------
library(marmap)
library(maps)
library(mapdata)
library(maptools)

#record of where you are working
curdir <- getwd()

## Set map limits ------------
Lat.lim=c(40,58)
Long.lim=c(-75,-47)


#Plotting colours
blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2","lightsteelblue1")
greys <- c("grey40","grey55","grey70","grey80")
greys <- c(grey(0.6), grey(0.93), grey(0.99))


#Download bathymetry data
setwd("C:/Users/StevensLy/Documents/Database/Data/")
getNOAA.bathy(lon1 = Long.lim[1], lon2 = Long.lim[2], lat1 = Lat.lim[1], lat2 = Lat.lim[2],
              resolution = 4,keep=TRUE) -> MapDepths
setwd(curdir)

# #which species are found more than 1% of the time
# #128 species
# goodspecies2 <- names(which(table(subdata[subdata$REGION=="MARITIME","species"])>37))
# pointdata <- subdata[!subdata$species%in%names(which(table(subdata$species)>37)),c("SLONG","SLAT","year_final")]

#Plot the bathymetry then layer adminsitrative boundaries, then a land mask on top
plot(MapDepths, deep=-10000,shallow=-10000, image = TRUE, 
     land = FALSE, lwd = 0.1, 
     bpal = list(c(0, max(MapDepths), greys), c(min(MapDepths), 0, blues)),
     xlab = expression(paste("Longitude ",~degree*W,sep="")),
     ylab = expression(paste("Latitude ",~degree*N,sep="")))## Download baythymetry

map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="grey", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)#
map.axes();map.scale(ratio=FALSE)

points(pointdata$SLONG,pointdata$SLAT,pch=19,cex=0.05)

plot(bounds)
plot(MapDepths, deep=-10000,shallow=-10000, image = TRUE, land = TRUE, lwd = 0.1, 
     bpal = list(c(0, max(MapDepths), greys), c(min(MapDepths), 0, blues)),add=T)## Download baythymetry



### List rows that just go to Class

subdata$classfilter=FALSE

subdata$temp <- rowSums(is.na(subdata[,c("order","family","genus")]))
subdata[!is.na(subdata$class) & subdata$temp==3,"classfilter"]=TRUE
subdata$orderfilter <- rowSums(is.na(subdata[,c("family","genus")]))
subdata$genusfilter <- sum(is.na(subdata$family))

