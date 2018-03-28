library(sdmpredictors)
library(leaflet)
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
require(dplyr)
library(reshape)
list(canada$NAME_1)

#Loading datasets in sdmpredictors package package 
datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)

#Layers in package
list_layers(datasets, terrestrial=FALSE, marine=TRUE, monthly=TRUE,
            version=NULL)


map <- fortify(map("worldHires", fill=TRUE, plot=FALSE, add=Tr))
provinces <- c("New Brunswick", "Nova Scotia", "Prince Edward Island", "Québec", "Newfoundland and Labrador")
canada <- getData("GADM",country="CAN",level=1)
ca.provinces <- canada[canada$NAME_1 %in% provinces,]
map2 <- fortify(canada)
map3 <- fortify(ca.provinces)
map3$region <-rep("Canada", nrow(coastmap3))


##################################################################
#Load dissolved oxygen data (Dissolved oxygen concentration [DO])
#Load Map with the lat/long extent
#Lay dissolved oxygen data over map
dissolved_oxygen <- load_layers(c("BO_dissox"))
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
dissolved_oxygen.crop <- crop(dissolved_oxygen, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(dissolved_oxygen.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Dissolved Oxygen")
map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
#################################################################



################################################################
#Load dissolved pH data (measure of acidity in the ocean)
#Load Map with the lat/long extent
#Lay pH data over map
pH <- load_layers(c("BO_ph"))
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
pH.crop <- crop(pH, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(pH.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "pH")
map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
#################################################################



#################################################################
#Load bathymetric slope data ( Bathymetric slope was measured in degrees ranging from 0¤ (flat surface) to 90¤ (vertical slope))
#Load Map with the lat/long extent
#Lay bathymetric slope data over map
bathymetric_slope <- load_layers(c("MS_biogeo06_bathy_slope_5m")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
bathymetric_slope.crop <- crop(bathymetric_slope, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(bathymetric_slope.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Bathymetric Slope")
map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################


#################################################################
#Load Mean Sea Surface Temperature (Sea surface temperature is the temperature of the water at the ocean surface. This parameter indicates the temperature of the topmost meter of the ocean water column)
#Load Map with the lat/long extent
#Lay Mean Sea Surface Temperature data over map
surfacetemp_mean <- load_layers(c("BO_sstmean")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
surfacetemp_mean.crop <- crop(surfacetemp_mean, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(surfacetemp_mean.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Mean Surface Temperature")
map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################
 
#################################################################
#Load Mean Sea Surface Salinity (Measurements of sea surface salinity (SSS) were obtained from in situ oceanographic observations compiled by NOAA?s World Ocean Atlas 2009 (WOA09; Antonov et al. 2010))
#Load Map with the lat/long extent
#Lay Mean Sea Surface Salinity data over map
seasurface_salinity <- load_layers(c("MS_biogeo08_sss_mean_5m")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
seasurface_salinity.crop <- crop(seasurface_salinity, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(seasurface_salinity.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Mean Surface Salinity (Annual Mean)")
map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################


###############################################################
#Load Sea surface temperature (coldest ice-free month) Data. 
#Load Map with the lat/long extent
#Lay Mean Sea Surface Salinity data over map
seasurface_temperature_cold <- load_layers(c("MS_biogeo14_sst_min_5m")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
seasurface_temperature_cold.crop <- crop(seasurface_temperature_cold, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(seasurface_temperature_cold.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Sea Surface Temperature (Coldest Ice-Free Month)")
map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
###############################################################


###############################################################
#Load Sea surface temperature (warmest ice-free month) Data. 
#Load Map with the lat/long extent
#Lay Mean Sea Surface Salinity data over map
seasurface_temperature_warm <- load_layers(c("MS_biogeo15_sst_max_5m")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
seasurface_temperature_warm.crop <- crop(seasurface_temperature_warm, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(seasurface_temperature_warm.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Sea Surface Temperature (Warmest Ice-Free Month)")
map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=Long.lim, ylim=Lat.lim, col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)

###############################################################












