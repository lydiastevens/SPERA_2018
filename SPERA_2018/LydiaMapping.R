## script for Lydia Mapping... 


## OK, so first, here is your quick plot code and below, is a more detailed ggplot version

library(marmap)
a <- data.frame(alldata_specieslevel[alldata_specieslevel$species=="Atlantopandalus propinqvus",]) 
b <- data.frame(alldata_specieslevel[alldata_specieslevel$species=="Pandalus borealis",])                 
c <- data.frame(alldata_specieslevel[alldata_specieslevel$species=="Pandalus montagui",])                  
shrimp <- c(a,b,c)
names(shrimp)
shrimp2 <- data.frame(shrimp$latitude, shrimp$longitude)

## this is good. You can use this points dataset below too. 


getNOAA.bathy(lon1=-30,lon2=-80,lat1=40,lat2=60, resolution=10) -> shrimpmap
plot(shrimpmap, image=TRUE, deep=-6000, shallow=0, step=1000)
points(shrimp$longitude, shrimp$latitude, col="red", cex=.6)

## It looks like you're using the bathymetric maps from the marmap package. What I will do down below is just the coastline map, but you can layer in additional maps like this one. You can always increase the resolution too to help with seeing everything. Just increase that value of 10 to like 30 or 40 and move up from there.

## I won't include anything about the points as you have above, because the way you have it set-up there, it will work below as well. As in, you could use the "shrimp" dataframe to get this map below

## There are also extra packages in here - mostly because I can't remember which specific ones are used. Can't hurt to have them all. They are currently the best for mapping in R

## package read-in
require(maps)
require(mapdata)
require(mapproj)
require(maptools)
require(plotrix)
require(raster)
require(gdistance)
require(rgdal)
require(sp)
require(ggplot2)
require(marmap)
require(ggrepel)
require(tmap)
require(tmaptools)
list(canada$NAME_1)

## create basemaps
## there are two here. One is the worldHires and the other is from the satellite GADM source. I prefer the GADM for coastline work, but feel free to explore the first. You can adjust the resoltion on the coastline map by adjusting the level.

coast_map <- fortify(map("worldHires", fill=TRUE, plot=FALSE))
provinces <- c("New Brunswick", "Nova Scotia", "Prince Edward Island", "Québec", "Newfoundland and Labrador")
canada <- getData("GADM",country="CAN",level=1)
ca.provinces <- canada[canada$NAME_1 %in% provinces,]
coastmap2 <- fortify(canada)
coastmap3<- fortify(ca.provinces)
coastmap3$region <-rep("Canada", nrow(coastmap3))

## coastmap3 is the one that will be used here, but sub it out for coast_map and you will see the difference in maps

shrimpmap<- ggplot()
shrimpmap <- shrimpmap + geom_map(data=coastmap3, map=coastmap3,
                                aes(x=long, y=lat, map_id=region),
                                fill="ivory2", color="black", size=1)
shrimpmap <- shrimpmap + geom_map(data=data.frame(region="Canada"), map=coastmap3,
                                aes(map_id=region), fill="grey90", size=1)
shrimpmap <- shrimpmap + xlim(-80, -30) + ylim(40, 60)
## adjust these lat longs above to get the proper dimensions of the map
shrimpmap <- shrimpmap + theme(panel.background = element_rect(fill = "#F3FFFF"))
shrimpmap <- shrimpmap + theme(panel.grid.major = element_line(colour = "black", linetype = "dotted"))
shrimpmap <- shrimpmap + ylab('Latitude')+ xlab('Longitude')
shrimpmap <- shrimpmap + theme(axis.title.x = element_text(face="bold", colour="#000000", size=14))
shrimpmap <- shrimpmap + theme(axis.title.y = element_text(face="bold", colour="#000000", size=14))
shrimpmap <- shrimpmap + theme(axis.text.x = element_text(face="bold", colour="#000000", size=12))
shrimpmap <- shrimpmap + theme(axis.text.y = element_text(face="bold", colour="#000000", size=12))
shrimpmap <- shrimpmap + theme(panel.background = element_rect(colour = "black"))
shrimpmap <- shrimpmap + theme(panel.border = element_rect(colour = "black", fill=NA))
shrimpmap <- shrimpmap + geom_point(data = shrimp2, aes(x=shrimp.longitude, y=shrimp.latitude, drop=FALSE), size = 1, shape =21, fill= "red")
## in here you can adjust the size of the point, the colour, the shape etc. 
shrimpmap <- shrimpmap + geom_text(data = NULL, aes(x=-63, y=47.1, label="Gulf of St. Lawrence"), size=5)
## this is an example of how to add a text label to anywhere on the map. you can adjust the lat/long to put it in different locations. 
shrimpmap

