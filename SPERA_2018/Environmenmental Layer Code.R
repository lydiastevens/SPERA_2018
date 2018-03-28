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
library(ggmap)



#######----Load this Base Map----########
#######----Change accordingly----########
coast_map <- fortify(map("worldHires", fill=TRUE, plot=FALSE))
provinces <- c("New Brunswick", "Nova Scotia", "Prince Edward Island", "Québec", "Newfoundland and Labrador", "Ontario")
canada <- getData("GADM",country="CAN",level=1)
ca.provinces <- canada[canada$NAME_1 %in% provinces,]
coastmap2 <- fortify(canada)
coastmap3<- fortify(ca.provinces)
coastmap3$region <-rep("Canada", nrow(coastmap3))
################################################

#Loading datasets in sdmpredictors package package#
#There are four datasets within this package (WorldClim, Bio-ORACLE, MARSPEC, ENVIREM)#
#Some are terrestrial and some are marine#
datasets <- list_datasets(terrestrial = TRUE, marine = TRUE)

#Current Layers in package#
tt <- list_layers(datasets, terrestrial=FALSE, marine=TRUE, monthly=TRUE,
                  version=NULL)

#Future Layers in package#
future_layers <- list_layers_future(datasets=c(), scenario=NA, year=NA,
                                    terrestrial=FALSE, marine=TRUE, monthly=TRUE, version=NULL)

#Definitions and information for all the layer codes#
listlayers <- list_layers()
View(listlayers)


######---Example of Map Code for Future Temperature Layer---#####
#################################################################
#Load Mean Sea Surface Temperature for Year 2100 (Sea surface temperature is the temperature of the water at the ocean surface)
#Load Map with the lat/long extent
#Lay Mean Sea Surface Temperature data over map
surfacetemp_mean_year2100 <- load_layers(c("BO_A1B_2100_sstmean")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
surfacetemp_mean_year2100.crop <- crop(surfacetemp_mean_year2100, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(surfacetemp_mean_year2100.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Mean Surface Temperature Year 2100")
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="white", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)
map("worldHires", xlim=c(-70.9, -47.3), ylim=c(41.9, 58.6), col="black", fill=TRUE, resolution=0,interior = FALSE,lty=0,add=T)

###############################################################


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


###############################################################
######---Example of Map Code for Terrestrial Layer---#####
###############################################################
#Load Mean Temperature Data: Terrestrial. 
#Load Map with the lat/long extent
#Lay Mean Temperature data over map
annual_mean_temperature <- load_layers(c("WC_bio1_cc26_2050")) 
ne.atlantic.ext <- extent(-70.9, -47.3, 41.9, 58.6)
annual_mean_temperature.crop <- crop(annual_mean_temperature, ne.atlantic.ext) 
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127")) 
plot(annual_mean_temperature.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(main = "Annual Mean Temperature 2050")
###############################################################
