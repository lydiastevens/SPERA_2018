
##########################################################################################################
######################################SPERA Project Code##################################################

## This code is a continuation from "final_code_maritime_newfoundland_310317.R". 
## Maritime and Newfoundland Code required a lot of data cleaning and was combined prior to this script.
## This script now combines cleaned Maritime, Newfoundland, Gulf and Quebec Region groundfish trawl data
## It them combines groundfish trawl data with functional trait data that was gather through a variety of sources (see work reports)
## The spreadsheet that includes all four regions, groundfish trawl data and functional traits can be found in the spreadsheet:
## fourregions_merged_130318
## To work from the latest fourregions_merged_130318 scroll to line #508

## Load packages    
library(lubridate)
library(dplyr)
library(rfishbase)
library(taxize)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(reshape)
require(maps)
require(mapdata)
require(mapproj)
require(maptools)
require(plotrix)
require(gdistance)
require(rgdal)
require(sp)
require(marmap)
require(ggrepel)
require(tmap)
require(tmaptools)


#load datasets from different regions for analysis
maritimenewfoundland <- read.csv("C:/Users/stevensly/Documents/Projects/SPERA/Spreadsheets/Archived/newdata170317_Lydia3.csv",stringsAsFactors = F)
quebec<-read.csv("C:/Users/stevensly/Documents/Projects/SPERA/Spreadsheets/Archived/Quebec Region RV DFO Survey_SLGO.csv",stringsAsFactors = F)
gulf<-read.csv("C:/Users/stevensly/Documents/Projects/SPERA/Spreadsheets/Archived/Gulf Region RV DFO Survey.csv",stringsAsFactors = F)

#subset maritime and newfoundland data into separate regions
maritime<-maritimenewfoundland[maritimenewfoundland$REGION=="MARITIME",]
newfoundland<-maritimenewfoundland[maritimenewfoundland$REGION=="NEWFOUNDLAND",]

#we now have four regions
#quebec, gulf, maritime, newfoundland

head(quebec)
head(gulf)
head(newfoundland)
head(maritime)

#######MARITIME#######
min(maritime$year_final) #1970
max(maritime$year_final) #2016
unique(maritime$species) #395 unique species
table(maritime$species)
max(table(maritime$species)) #Hippoglossoides platessoides occurred the most frequently (10067 occasions)
View(table(maritime$species))

#######NEWFOUNDLAND#######
min(newfoundland$year_final) #1995
max(newfoundland$year_final) #2013
unique(newfoundland$species) #134 unique species
table(newfoundland$species)
max(table(newfoundland$species)) #Hippoglossoides platessoides occurred the most frequently (14180 occasions)
View(table(newfoundland$species))

#######QUEBEC#######
min(quebec$Date) #1995
max(quebec$Date) #2015
unique(quebec$Latin.name) #103 unique species
table(quebec$Latin.name)
max(table(quebec$Latin.name)) #Gadus Morhua occurred the most frequently (2943 occasions)
View(table(quebec$Latin.name))

#######GULF#######
min(gulf$year) #1971
max(gulf$year) #2016
names(gulf[,16:103]) #88 unique species

#Cleaning Dates in Quebec dataset
Date<-as.POSIXct(quebec$Date, format="%Y-%m-%d", tz ="UTC")
quebec$year_final <- year(Date)
quebec$month_final <- month(Date)
quebec$day_final <- day(Date)
names(quebec)

#Fixing column names in Quebec dataset
quebec$region<-"QUEBEC"
colnames(quebec)[7]<-paste("species")
head(quebec) 
unique(quebec$species)

##Running Classify function for quebec Species
##Create new colum for species that are going to be taxized
quebec$newsciname = quebec$species
quebec$newsciname<-tolower(quebec$newsciname)
head(quebec)
unique(quebec$newsciname)


quebec$newsciname <- gsub("ammodytes sp.","ammodytes",quebec$newsciname)
SpeciesList <- unique(quebec$newsciname)

# Used an old function to expand species name into taxonomic groups 
# source("C:/Users/StevensLy/Documents/Database/Code/MPA database/ClassifyFunction.R")
# SpeciesList <- unique(quebec$newsciname)
# outlist <- lapply(SpeciesList[which(SpeciesList==""):length(SpeciesList)],FUN=Classify)
# outlist <- lapply((SpeciesList), FUN=Classify)
# taxInfo <- do.call("rbind", outlist)
#write.csv(taxInfo, file='C:/Users/StevensLy/Documents/Database/Data/taxinfoquebec.csv')
taxinfo_quebec <- read.csv("C:/Users/stevensly/Documents/Projects/SPERA/Spreadsheets/Archived/taxinfoquebec.csv",stringsAsFactors = F)
names(taxinfo_quebec)

#merging column data from taxinfo_quebec to quebec.
quebec2 <- merge(quebec[,-grep("species",x = names(quebec))], taxinfo_quebec, by="newsciname")
head(quebec2)



##Replace Column Name, which uses species code with scientific name
names(gulf)
speciescode<-read.csv("C:/Users/stevensly/Documents/Projects/SPERA/Spreadsheets/Archived/Species Code Info.csv",stringsAsFactors = F)
View(speciescode)

#Match species names from "speciescode data" to the codes in gulf data
colnames(gulf) <- gsub("X_","",colnames(gulf))
gulf2 <- gulf #so the code doesn't bug out

for(i in 16:103){
  colnames(gulf)[i]=speciescode[speciescode$CODE==as.numeric(colnames(gulf2)[i]),"SPEC"]
}

names(gulf)

#Subsetted data
t<-gulf[1:15]
tt<-gulf[16:103]
#melted so species weren't individual columns, but rather one column (wide to long)
meltgulf<-melt(tt)
totalgulf<-data.frame(t, meltgulf)
colnames(totalgulf)[16]<-paste("species")
colnames(totalgulf)[1]<-paste("year_final")
colnames(totalgulf)[6]<-paste("month_final")
colnames(totalgulf)[7]<-paste("day_final")
totalgulf$region<-"GULF"
head(totalgulf)
names(totalgulf)
unique(totalgulf$species) #88 unique species

#changed species from upper case to lower case
totalgulf$species<-tolower(totalgulf$species)
head(totalgulf)

#remove rows that had NA or 0 listed for weight
totalgulf <- totalgulf[is.na(totalgulf$value) == FALSE,]
totalgulf <- totalgulf[totalgulf$value>0,]
#length(table(totalgulf2$species))

##create new colum for species that are going to be taxized
totalgulf$newsciname = totalgulf$species
totalgulf$newsciname<-tolower(totalgulf$newsciname)
head(totalgulf)
unique(totalgulf$newsciname)

#removed taxominc groups at the end of each species
totalgulf$newsciname <- gsub("sebastes sp.","sebastes",totalgulf$newsciname)
totalgulf$newsciname <- gsub("lycodes sp.","lycodes",totalgulf$newsciname)
totalgulf$newsciname <- gsub("paralepididae f.","paralepididae",totalgulf$newsciname)
totalgulf$newsciname <- gsub("tunicata s.p.","tunicata",totalgulf$newsciname)
totalgulf$newsciname <- gsub("decapoda o.","decapoda",totalgulf$newsciname)
totalgulf$newsciname <- gsub("hyas sp.","hyas",totalgulf$newsciname)
totalgulf$newsciname <- gsub("paguroidea s.f.","paguroidea",totalgulf$newsciname)
totalgulf$newsciname <- gsub("aphrodita sp.","aphrodita",totalgulf$newsciname)
totalgulf$newsciname <- gsub("gastropoda o.","gastropoda",totalgulf$newsciname)
totalgulf$newsciname <- gsub("bivalvia c.","bivalvia",totalgulf$newsciname)
totalgulf$newsciname <- gsub("pectinidae f.","pectinidae",totalgulf$newsciname)
totalgulf$newsciname <- gsub("octopoda o.","octopoda",totalgulf$newsciname)
totalgulf$newsciname <- gsub("asteroidea s.c.","asteroidea",totalgulf$newsciname)
totalgulf$newsciname <- gsub("ophiuroidea s.c.","ophiuroidea",totalgulf$newsciname)
totalgulf$newsciname <- gsub("gorgonocephalidae,asteronychidae f.","euryalida",totalgulf$newsciname)
totalgulf$newsciname <- gsub("strongylocentrotus sp.","strongylocentrotus",totalgulf$newsciname)
totalgulf$newsciname <- gsub("clypeasteroida o.","clypeasteroida",totalgulf$newsciname)
totalgulf$newsciname <- gsub("holothuroidea c.","holothuroidea",totalgulf$newsciname)
totalgulf$newsciname <- gsub("clypeasteroida o.","clypeasteroida",totalgulf$newsciname)
totalgulf$newsciname <- gsub("anthozoa c.","anthozoa",totalgulf$newsciname)
totalgulf$newsciname <- gsub("scyphozoa c.","scyphozoa",totalgulf$newsciname)
totalgulf$newsciname <- gsub("porifera p.","porifera",totalgulf$newsciname)
totalgulf$newsciname <- gsub("liparis sp.","liparidae",totalgulf$newsciname)
totalgulf$newsciname <- gsub("gymnelis viridis","gymnelus viridis",totalgulf$newsciname)
totalgulf$newsciname <- gsub("lumpenus lumpretaeformis","lumpenus lampretaeformis",totalgulf$newsciname)
totalgulf$newsciname <- gsub("enchelyopus cimbrius","enchelyopus cimbrius",totalgulf$newsciname)

totalgulf <- totalgulf[is.na(totalgulf$newsciname) == FALSE,]
SpeciesList <- unique(totalgulf$newsciname)

#used Classify function to expand species names into taxonomic groups 
source("C:/Users/StevensLy/Documents/Projects/SPERA/Code/ClassifyFunction.R")
SpeciesList <- unique(totalgulf$newsciname)
outlist <- lapply(SpeciesList[which(SpeciesList=="gadus morhua"):length(SpeciesList)],FUN=Classify)
outlist <- lapply((SpeciesList), FUN=Classify)
taxInfo <- do.call("rbind", outlist)
#write.csv(taxInfo, file='C:/Users/StevensLy/Documents/Database/Data/taxinfogulf.csv')
taxinfo_gulf <- read.csv("C:/Users/stevensly/Documents/Projects/SPERA/Spreadsheets/Archived/taxinfogulf.csv",stringsAsFactors = F)
names(taxinfo_gulf)
taxinfo_gulf$X <- NULL
names(taxinfo_gulf)

#merging column data from taxinfo_gulf to gulf.
totalgulf3 <- merge(totalgulf[,-grep("species",x = names(totalgulf))], taxinfo_gulf, by="newsciname")

#simplify column names before using rbind.all.columns function to bind columns
names(quebec2)
colnames(quebec2)[grep("Longitude",colnames(quebec2))]<-"longitude"
colnames(quebec2)[grep("Latitude",colnames(quebec2))]<-"latitude"
colnames(quebec2)[grep("year_final",colnames(quebec2))]<-"year"
colnames(quebec2)[grep("day_final",colnames(quebec2))]<-"month"
colnames(quebec2)[grep("month_final",colnames(quebec2))]<-"day"
colnames(quebec2)[grep("Weight..kg.",colnames(quebec2))]<-"weight_kg"
colnames(quebec2)[grep("Date",colnames(quebec2))]<-"date"
colnames(quebec2)[grep("Format",colnames(quebec2))]<-"format"
colnames(quebec2)[grep("Location",colnames(quebec2))]<-"location"
colnames(quebec2)[grep("Taxon",colnames(quebec2))]<-"taxon"
colnames(quebec2)[grep("Individual.Count",colnames(quebec2))]<-"individual.count"
colnames(quebec2)[grep("Presence",colnames(quebec2))]<-"presence"
colnames(quebec2)[grep("Biomass",colnames(quebec2))]<-"biomass"
colnames(quebec2)[grep("Density",colnames(quebec2))]<-"density"
colnames(quebec2)[grep("Coverage",colnames(quebec2))]<-"coverage"
colnames(quebec2)[grep("Sampling.Method",colnames(quebec2))]<-"sampling.method"
colnames(quebec2)[grep("Collection",colnames(quebec2))]<-"collection"
colnames(quebec2)[grep("Coverage",colnames(quebec2))]<-"coverage"
colnames(quebec2)[grep("Owner.Institution",colnames(quebec2))]<-"owner.institution"
quebec2$X <- NULL
names(quebec2)

names(totalgulf3)
colnames(totalgulf3)[grep("year_final",colnames(totalgulf3))]<-"year"
colnames(totalgulf3)[grep("month_final",colnames(totalgulf3))]<-"month"
colnames(totalgulf3)[grep("day_final",colnames(totalgulf3))]<-"day"
colnames(totalgulf3)[grep("NAME_",colnames(totalgulf3))]<-"gulf_name"
names(totalgulf3)

#This rbind.all.columns will match columns from one dataset to another AND keep non matching columns (see below)
#rbind.all.columns <- function(x, y) {
#x.diff <- setdiff(colnames(x), colnames(y))
#y.diff <- setdiff(colnames(y), colnames(x))
#x[, c(as.character(y.diff))] <- NA
#y[, c(as.character(x.diff))] <- NA
#NAMES <- intersect(names(x),names(y))
#return(rbind(x[,NAMES], y[,NAMES]))
#}

#This function will match columns from one dataset to another AND keep non matching columns
source("rbindcolsfun.R")

#combined gulf data with quebec data. Columns that were the same matched together
#columns that were different from each regions were also added and filled with NAs where there was not data
gulfquebec <- rbind.all.columns(totalgulf3, quebec2)
names(gulfquebec)


#write.csv(gulfquebec, file='C:/Users/stevensly/Documents/Projects/SPERA/Spreadsheets/Archived/gulfquebec.csv')
#write.csv(maritimenewfoundland, file='C:/Users/StevensLy/Documents/Database/Data/maritimenewfoundland.csv')
#gulfquebec <- read.csv("C:/Users/stevensly/Documents/Projects/SPERA/Data/Archived/gulfquebec.csv",stringsAsFactors = F)


#next step is to combine gulfquebecdata with maritimenewfounald data
#read in maritime and newfoundland datasets that have already been cleaned
maritimenewfoundland <- read.csv("C:/Users/stevensly/Documents/Projects/SPERA/Spreadsheets/Archived/maritimenewfoundland.csv",stringsAsFactors = F)

names(maritimenewfoundland)
colnames(maritimenewfoundland)[grep("STRAT_TYPE",colnames(maritimenewfoundland))]<-"strat_type"
colnames(maritimenewfoundland)[grep("MISSION",colnames(maritimenewfoundland))]<-"mission"
colnames(maritimenewfoundland)[grep("NAME",colnames(maritimenewfoundland))]<-"name"
colnames(maritimenewfoundland)[grep("SEASON",colnames(maritimenewfoundland))]<-"season"
colnames(maritimenewfoundland)[grep("DATETIME",colnames(maritimenewfoundland))]<-"datetime"
colnames(maritimenewfoundland)[grep("DIST",colnames(maritimenewfoundland))]<-"distance"
colnames(maritimenewfoundland)[grep("SURF_TEMP",colnames(maritimenewfoundland))]<-"surf_temp"
colnames(maritimenewfoundland)[grep("BOTT_TEMP",colnames(maritimenewfoundland))]<-"bott_temp"
colnames(maritimenewfoundland)[grep("BOTT_SAL",colnames(maritimenewfoundland))]<-"bott_salinity"
colnames(maritimenewfoundland)[grep("ELONG",colnames(maritimenewfoundland))]<-"e_longitude"
colnames(maritimenewfoundland)[grep("ELAT",colnames(maritimenewfoundland))]<-"e_latitude"
colnames(maritimenewfoundland)[grep("DMIN",colnames(maritimenewfoundland))]<-"dmin"
colnames(maritimenewfoundland)[grep("DMAX",colnames(maritimenewfoundland))]<-"dmax"
colnames(maritimenewfoundland)[grep("TOTNO",colnames(maritimenewfoundland))]<-"totno"
colnames(maritimenewfoundland)[grep("TOTWGT",colnames(maritimenewfoundland))]<-"totwgt"
colnames(maritimenewfoundland)[grep("SPEC",colnames(maritimenewfoundland))]<-"spec"
colnames(maritimenewfoundland)[grep("COMM",colnames(maritimenewfoundland))]<-"comm"
colnames(maritimenewfoundland)[grep("NEWDATETIME",colnames(maritimenewfoundland))]<-"newdatetime"
colnames(maritimenewfoundland)[grep("month_final",colnames(maritimenewfoundland))]<-"month"
colnames(maritimenewfoundland)[grep("day_final",colnames(maritimenewfoundland))]<-"day"
colnames(maritimenewfoundland)[grep("year_final",colnames(maritimenewfoundland))]<-"year"
names(maritimenewfoundland)


#determining what species are found in gulf and quebec regions but not in maritime newfoundland
setdiff(gulfquebec$species, maritimenewfoundland$species)
#update species where there names have changed
gulfquebec$species <- gsub("Urophycis chesteri","Phycis chesteri",gulfquebec$species)
gulfquebec$species <- gsub("Lumpenus maculatus","Leptoclinus maculatus",gulfquebec$species)

dim(gulfquebec)
dim(maritimenewfoundland)

#combine maritimenewfound data with gulquebec data
allregions <- rbind.all.columns(gulfquebec,maritimenewfoundland)
head(allregions)
names(allregions)


##fixing mistakes between all datasets
allregions$species <- gsub("Spirontocarus spinus","Spirontocaris spinus",allregions$species)
allregions$species <- gsub("Raja fyllae","Rajella fyllae",allregions$species)
allregions$species <- gsub("Ulcina olrikii","Aspidophoroides olrikii",allregions$species)
allregions$genus <- gsub("Ulcina","Aspidophoroides",allregions$genus)
allregions$newsciname <- gsub("ulcina olrikii","aspidophoroides olrikii",allregions$newsciname)
allregions$class <- gsub("Teleostei","Actinopterygii",allregions$class)
# allregions$species <- gsub("Gadus ogac","Gadus macrocephalus",allregions$species)
# allregions$newsciname <- gsub("gadus ogac","gadus macrocephalus",allregions$newsciname)
write.csv(allregions, file='C:/Users/stevensly/Documents/Projects/SPERA/Spreadsheets/Archived/allregions2.csv')


#Set frequencies to have a range (1-0). Where the species with the highest capture percentage is 1 and the lowest is 0
#This can level out if gear wasn't working and only one trawl survey was done catching a lot of fish versus many 
#trawl surveys catching fewer fish (I think?)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#Create dataframe. Fill it with nothing
#Make for loop to filter by region and year and calculate frequency (percentage)
freqinfo <- NULL #empty data
for (i in unique(allregions$region)[!is.na(unique(allregions$region))]){
  for (y in unique(allregions[!is.na(allregions$region) & allregions$region==i,"year"])){
    
    
    temp <- dplyr::filter(allregions,region==i,year==y)
    
    freq_obs <- as.data.frame(table(temp$species)/nrow(temp))
    colnames(freq_obs) <- c("species","frequency")
    
    freq_obs$region=i
    freq_obs$year=y
    
    freq_obs$freq_stand <- range01(freq_obs$frequency)
    
    freqinfo <- rbind(freqinfo,freq_obs)
    
    
  } #end of y 'year_final' loop
  
} #end of i 'region' loop

freqinfo <- freqinfo[order(freqinfo$region,freqinfo$year,freqinfo$freq_stand),]
View(freqinfo)

###Determining which species were captured more than 1% of the time in 
###trawl sets for each region

#################MARITIME####################
#combining region, date, lat, long into a tag
allregions$tag <- paste(allregions$region,paste(allregions$year,allregions$month,allregions$day,sep="-"),
                        allregions$longitude,allregions$latitude,sep="_")
#filter dataset by anything after 2005 (last decade) and Gulf region
subdata <- dplyr::filter(allregions,year>2005,region=="MARITIME")
#what is 1% of the unique species
Precent_1_stations <- floor(length(unique(subdata$tag))*0.01)
#which species are found more than __% of the time
goodspecies_maritime <- names(which(table(subdata$species)>Precent_1_stations))
pointdata_maritime <- subdata[!subdata$species%in%names(which(table(subdata$species)>Precent_1_stations)),c("longitude","latitude","year_final")]

goodspecies_maritime
########################################

####################NEWFOUNDLAND######################
#combining region, date, lat, long into a tag
allregions$tag <- paste(allregions$region,paste(allregions$year,allregions$month,allregions$day,sep="-"),
                        allregions$longitude,allregions$latitude,sep="_")
#filter dataset by anything after 2005 (last decade) and Newfoundland region
subdata <- filter(allregions,year>2005,region=="NEWFOUNDLAND")
#what is 1% of the unique species
Precent_1_stations <- floor(length(unique(subdata$tag))*0.01)
#which species are found more than __% of the time
goodspecies_newfoundland <- names(which(table(subdata$species)>Precent_1_stations))
pointdata_newfoundland <- subdata[!subdata$species%in%names(which(table(subdata$species)>Precent_1_stations)),c("longitude","latitude","year_final")]

goodspecies_newfoundland
########################################

######################GULF######################
allregions$tag <- paste(allregions$region,paste(allregions$year,allregions$mont,allregions$day,sep="-"),
                        allregions$longitude,allregions$latitude,sep="_")
#filter dataset by anything after 2005 (last decade) and Gulf region
subdata <- filter(allregions,year>2005,region=="GULF")
#what is 1% of the unique species
Precent_1_stations <- floor(length(unique(subdata$tag))*0.01)
#which species are found more than __% of the time
goodspecies_gulf <- names(which(table(subdata$species)>Precent_1_stations))
pointdata_gulf <- subdata[!subdata$species%in%names(which(table(subdata$species)>Precent_1_stations)),c("longitude","latitude","year_final")]

goodspecies_gulf
########################################

######################QUEBEC######################
allregions$tag <- paste(allregions$region,paste(allregions$year_,allregions$month,allregions$day,sep="-"),
                        allregions$longitude,allregions$latitude,sep="_")
#filter dataset by anything after 2005 (last decade) and Quebec Region
subdata <- filter(allregions,year>2005,region=="QUEBEC")
#what is 1% of the unique species
Precent_1_stations <- floor(length(unique(subdata$tag))*0.01)
#which species are found more than __% of the time
goodspecies_quebec <- names(which(table(subdata$species)>Precent_1_stations))
pointdata_quebec <- subdata[!subdata$species%in%names(which(table(subdata$species)>Precent_1_stations)),c("longitude","latitude","year_final")]

goodspecies_quebec
########################################

#sensitivity analysis
x <- NULL
for (i in seq(0.01,0.75,0.01)){
  cutoff <- floor(length(unique(subdata$tag))*i)
  x <- c(x,length(names(which(table(subdata$species)>cutoff))))
}
xplotdata <- data.frame(cutoff=seq(0.01,0.75,0.01),numspecies=x)
ggplot2::ggplot(xplotdata,aes(x=cutoff,y=numspecies))+geom_point()+theme_bw()


##Finding differences between regions
#There are 184 unique species in each region more than 1% of the time (this does not include animals 
#captured that only have taxonomic information higher than species level)
known <- unique(c(goodspecies_newfoundland, goodspecies_maritime))
unknown <- unique(c(goodspecies_gulf, goodspecies_quebec))
diff <- setdiff(known, unknown)
diff

#create a dataframe with unique species from each region that were found more than 1% of the time since 2005
unique_species <- data.frame(unique(c(goodspecies_maritime,goodspecies_newfoundland,goodspecies_gulf,goodspecies_quebec)))
names(unique_species) <- paste("species")
head(unique_species)

#creating a new dataset for functional traits only of unique species. 
names(allregions)
functionaltraits <-allregions[,c(19:25,57:139)] 
unique(functionaltraits$species) 
names(functionaltraits)

#merge unique_species with functional traits by species
df <- merge(unique_species,functionaltraits, by="species")
head(df)
head(unique(df$species))
dflong <- unique(df[df$species %in% unique_species$species,])
head(dflong)
functionaldatabase <- dflong[!duplicated(dflong$species), ]
head(functionaldatabase)

##****NOTE****##
##read in functionaldatabase below!! The functional database made above was just a start.
#I added more functional traits in excel to complete
#Re-writing the functionaldatabase will remove any of the work that was completed in excel!!!
functionaldatabase<-read.csv("C:/Users/stevensly/Documents/Projects/SPERA/Spreadsheets/Archived/functionaldatabase_271117.csv",stringsAsFactors = F)
head(functionaldatabase)
##fixing mistakes
functionaldatabase$species <- gsub("Spirontocarus spinus","Spirontocaris spinus",functionaldatabase$species)
functionaldatabase$species <- gsub("Raja fyllae","Rajella fyllae",functionaldatabase$species)
functionaldatabase$species <- gsub("Ulcina olrikii","Aspidophoroides olrikii",functionaldatabase$species)
functionaldatabase$genus <- gsub("Ulcina","Aspidophoroides",functionaldatabase$genus)
functionaldatabase$class <- gsub("Teleostei","Actinopterygii",functionaldatabase$class)


##Get mean length for all unique species in functionaldatabase
##This was to classify length category and cross reference with Bundy study.
alllength <- NULL
for (i in unique(functionaldatabase$species)){
  alllength <- rbind(alllength, data.frame(species=i, 
                                           meanlength=mean(functionaldatabase[functionaldatabase$species==i,"length_cm"],na.rm=T)))
}

alllength
##This becomes the small, medium and large groups for size
alllength[alllength$meanlength<31,]
alllength[alllength$meanlength[31:80],]
alllength[alllength$meanlength>80,]


##make a dataset that just includes trawl information from all four regions. 
##this will later be merged with functional trait database
names(allregions)
trawldata <- allregions[, c(1:18, 25:56)]
names(trawldata)
head(trawldata)


#this script contains only trawl data from all four regions
#write.csv(trawldata, file='C:/Users/stevensly/Documents/Projects/SPERA/Spreadsheets/Archived/trawldata_120318.csv')
#trawldata<-read.csv("C:/Users/stevensly/Documents/Projects/SPERA/Spreadsheets/Archived/trawldata_120318.csv",stringsAsFactors = F)

#next step is to merge the two databases together (trawldata and functionaldatabase)
names(functionaldatabase)
names(trawldata)
#before merging intersect and remove all similar columns, except for species
#intersect(names(trawldata), names(functionaldatabase))
alldata <- merge(trawldata,functionaldatabase,by="species")
head(alldata[alldata$region=="QUEBEC",])
names(alldata)
head(alldata)

####################################################################################################################
#####################################----FOUR REGIONS MERGED----#####################################################
####################################################################################################################

#write.csv(alldata, file='C:/Users/stevensly/Documents/Projects/SPERA/Spreadsheets/fourregions_merged_130318.csv')
fourregions_merged<-read.csv("C:/Users/stevensly/Documents/Projects/SPERA/Spreadsheets/fourregions_merged_130318.csv",stringsAsFactors = F)
head(fourregions_merged)
names(fourregions_merged)


##########################################################################
#####Everything from here on has been data visualization/exploration #####
##########################################################################

###Range of lat and long for plotting coordinates##
names(fourregions_merged)
min(fourregions_merged$latitude)
max(fourregions_merged$latitude)
min(fourregions_merged$longitude)
max(fourregions_merged$longitude)

##Set frequencies to have a range (1-0). Where the species with the highest capture percentage is 1 and the lowest is 0
##This can level out if gear wasn't working and only one trawl survey was done catching a lot of fish versus many 
##trawl surveys catching fewer fish (I think?)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

##Starting to break down data and determine percentage of species captured in each region##
##frequency of species captured in each region##
##some observations from quebec may have to be removed because they are in the data
##but recorded as absent
frequency_of_species <- NULL #empty data
for (i in unique(fourregions_merged$region)){
  for (y in unique(fourregions_merged[fourregions_merged$region==i,"year"]))
    for(x in unique(fourregions_merged$invertebrate)){
      
      temp <- dplyr::filter(fourregions_merged,region==i,year==y, invertebrate==x)
      
      freq_obs <- as.data.frame(table(temp$species)/nrow(temp))
      
      colnames(freq_obs) <- c("species","frequency")
      
      freq_obs$region=i
      freq_obs$year=y
      freq_obs$invertebrate=x
      
      freq_obs$freq_stand <- range01(freq_obs$frequency)
      
      frequency_of_species <- rbind(frequency_of_species,freq_obs)
      
      
    } #end of y 'year_final' loop
  
} #end of i 'REGION' loop

frequency_of_species <- frequency_of_species[order(frequency_of_species$year),]


#####----MARITIME: Percentage of Species----####
####Change year and percentage as needed####
fourregions_merged$tag <- paste(fourregions_merged$region,paste(fourregions_merged$year,fourregions_merged$month,fourregions_merged$day,sep="-"),
                                fourregions_merged$longitude,fourregions_merged$latitude,sep="_")
#filter dataset by anything after 2005 (last decade) and Gulf region
subdata <- dplyr::filter(fourregions_merged,year>2005,region=="MARITIME")
#what is 1% of the unique species
Precent_1_stations <- floor(length(unique(subdata$tag))*0.01)
#which species are found more than __% of the time
goodspecies_maritime <- names(which(table(subdata$species)>Precent_1_stations))
pointdata_maritime <- subdata[!subdata$species%in%names(which(table(subdata$species)>Precent_1_stations)),c("longitude","latitude","year")]

goodspecies_maritime


#####----NEWFOUNDLAND: Percentage of Species----####
####Change year and percentage as needed####
fourregions_merged$tag <- paste(fourregions_merged$region,paste(fourregions_merged$year,fourregions_merged$month,fourregions_merged$day,sep="-"),
                                fourregions_merged$longitude,fourregions_merged$latitude,sep="_")
#filter dataset by anything after 2005 (last decade) and Gulf region
subdata <- dplyr::filter(fourregions_merged,year>2005,region=="NEWFOUNDLAND")
#what is 1% of the unique species
Precent_1_stations <- floor(length(unique(subdata$tag))*0.01)
#which species are found more than __% of the time
goodspecies_newfoundland <- names(which(table(subdata$species)>Precent_1_stations))
pointdata_newfoundland <- subdata[!subdata$species%in%names(which(table(subdata$species)>Precent_1_stations)),c("longitude","latitude","year")]

goodspecies_newfoundland


#####----GULF: Percentage of Species----####
####Change year and percentage as needed####
fourregions_merged$tag <- paste(fourregions_merged$region,paste(fourregions_merged$year,fourregions_merged$month,fourregions_merged$day,sep="-"),
                                fourregions_merged$longitude,fourregions_merged$latitude,sep="_")
#filter dataset by anything after 2005 (last decade) and Gulf region
subdata <- dplyr::filter(fourregions_merged,year>2005,region=="GULF")
#what is 1% of the unique species
Precent_1_stations <- floor(length(unique(subdata$tag))*0.01)
#which species are found more than __% of the time
goodspecies_gulf <- names(which(table(subdata$species)>Precent_1_stations))
pointdata_gulf <- subdata[!subdata$species%in%names(which(table(subdata$species)>Precent_1_stations)),c("longitude","latitude","year")]

goodspecies_gulf


#####----QUEBEC: Percentage of Species----#####
####Change year and percentage as needed####
fourregions_merged$tag <- paste(fourregions_merged$region,paste(fourregions_merged$year,fourregions_merged$month,fourregions_merged$day,sep="-"),
                                fourregions_merged$longitude,fourregions_merged$latitude,sep="_")
#filter dataset by anything after 2005 (last decade) and Gulf region
subdata <- dplyr::filter(fourregions_merged,year>2005,region=="QUEBEC")
#what is 1% of the unique species
Precent_1_stations <- floor(length(unique(subdata$tag))*0.01)
#which species are found more than __% of the time
goodspecies_quebec <- names(which(table(subdata$species)>Precent_1_stations))
pointdata_quebec <- subdata[!subdata$species%in%names(which(table(subdata$species)>Precent_1_stations)),c("longitude","latitude","year")]

goodspecies_quebec


#####----All regions: Percentage of Species----#####
fourregions_merged$tag <- paste(fourregions_merged$region,paste(fourregions_merged$year,fourregions_merged$month,fourregions_merged$day,sep="-"),
                                fourregions_merged$longitude,fourregions_merged$latitude,sep="_")
#filter dataset by anything after 2005 (last decade) and Gulf region
subdata <- dplyr::filter(fourregions_merged, year>2005)
#what is 1% of the unique species
Precent_1_stations <- floor(length(unique(subdata$tag))*0.25)
#which species are found more than __% of the time
goodspecies_allregions <- names(which(table(subdata$species)>Precent_1_stations))
pointdata_allregions <- subdata[!subdata$species%in%names(which(table(subdata$species)>Precent_1_stations)),c("longitude","latitude","year")]

goodspecies_allregions


#######################################################
#####----Frequency of species from all regions----#####
#######################################################
##Once you determine the percentage of species you want, these plots will have fewer species.

plotfreq1<-ggplot(frequency_of_species)+
  geom_point(aes(x=species,y=freq_stand, colour=factor(region)))+
  ylab("Standardized Frequency")+xlab("Species")+ggtitle("Standardized frequency of species captured from all four regions")+
  coord_flip()+
  theme_bw();plotfreq1

####----Frequency of Vertebrate species from Maritime region----####
frequency_of_species_mv <- frequency_of_species%>%group_by(species, region, year, freq_stand)%>%filter(year>2005, region=="MARITIME", invertebrate=="0")%>%ungroup()%>%data.frame
plotfreqm<-ggplot(frequency_of_species_mv)+
  geom_point(aes(x=species,y=freq_stand, colour=factor(year)))+labs(colour="Year")+
  ylab("Standardized Frequency")+xlab("Species")+ggtitle("Standardized frequency of Maritime vertebrates since 2005")+
  coord_flip()+
  theme_bw();plotfreqm

####----Frequency of Invertebrate species from Maritime region----####
frequency_of_species_mi <- frequency_of_species%>%group_by(species, region, year, freq_stand)%>%filter(year>2005, region=="MARITIME", invertebrate=="1")%>%ungroup()%>%data.frame
plotfreqmi<-ggplot(frequency_of_species_mi)+
  geom_point(aes(x=species,y=freq_stand, colour=factor(year)))+labs(colour="Year")+
  ylab("Standardized Frequency")+xlab("Species")+ggtitle("Standardized frequency of Maritime invertebrates since 2005")+
  coord_flip()+
  theme_bw();plotfreqmi

####----Frequency of Vertebrate species from Gulf region----####
frequency_of_species_gv <- frequency_of_species%>%group_by(species, region, year, freq_stand)%>%filter(year>2005, region=="GULF", invertebrate=="0")%>%ungroup()%>%data.frame
plotfreqg <- ggplot(frequency_of_species_gv)+
  geom_point(aes(x=species,y=freq_stand, colour=factor(year)))+labs(colour="Year")+
  ylab("Standardized Frequency")+xlab("Species")+ggtitle("Standardized frequency of Gulf vertebrates since 2005")+
  coord_flip()+
  theme_bw();plotfreqg

####----Frequency of Invertebrate species from Gulf region----####
frequency_of_species_gi <- frequency_of_species%>%group_by(species, region, year, freq_stand)%>%filter(year>2005, region=="GULF", invertebrate=="1")%>%ungroup()%>%data.frame
plotfreqgi<-ggplot(frequency_of_species_gi)+
  geom_point(aes(x=species,y=freq_stand, colour=factor(year)))+labs(colour="Year")+
  ylab("Standardized Frequency")+xlab("Species")+ggtitle("Standardized frequency of Gulf invertebrates since 2005")+
  coord_flip()+
  theme_bw();plotfreqgi

####----Frequency of Vertebrate species from Quebec region----####
frequency_of_species_qv <- frequency_of_species%>%group_by(species, region, year, freq_stand)%>%filter(year>2005, region=="QUEBEC", invertebrate=="0")%>%ungroup()%>%data.frame
plotfreqq <- ggplot(frequency_of_species_qv)+
  geom_point(aes(x=species,y=freq_stand, colour=factor(year)))+labs(colour="Year")+
  ylab("Standardized Frequency")+xlab("Species")+ggtitle("Standardized frequency of Quebec vertebrates since 2005")+
  coord_flip()+
  theme_bw();plotfreqq

####----Frequency of Invertebrate species from Quebec region----####
frequency_of_species_qi <- frequency_of_species%>%group_by(species, region, year, freq_stand)%>%filter(year>2005, region=="QUEBEC", invertebrate=="1")%>%ungroup()%>%data.frame
plotfreqqi<-ggplot(frequency_of_species_qi)+
  geom_point(aes(x=species,y=freq_stand, colour=factor(year)))+labs(colour="Year")+
  ylab("Standardized Frequency")+xlab("Species")+ggtitle("Standardized frequency of Quebec invertebrates since 2005")+
  coord_flip()+
  theme_bw();plotfreqqi



#######################################################
###################----Mapping----#####################
#######################################################
##########--Provinces within Canada Mapping--##########
coast_map <- fortify(map("worldHires", fill=TRUE, plot=FALSE))
provinces <- c("New Brunswick", "Nova Scotia", "Prince Edward Island", "Québec", "Newfoundland and Labrador", "Ontario")
canada <- getData("GADM",country="CAN",level=1)
ca.provinces <- canada[canada$NAME_1 %in% provinces,]
coastmap2 <- fortify(canada)
coastmap3<- fortify(ca.provinces)
coastmap3$region <-rep("Canada", nrow(coastmap3))
######################################################


##All Captures Across Regions##
latlong <- data.frame(fourregions_merged$latitude, fourregions_merged$longitude)
captures<- ggplot()
captures <- captures + geom_map(data=coastmap3, map=coastmap3,
                                aes(x=long, y=lat, map_id=region),
                                fill="ivory2", color="black", size=1)
captures <- captures + geom_map(data=data.frame(region="Canada"), map=coastmap3,
                                aes(map_id=region), fill="grey90", size=1)
captures <- captures + xlim(-80, -45) + ylim(42, 58)
## adjust these lat longs above to get the proper dimensions of the map
captures <- captures + theme(panel.background = element_rect(fill = "#F3FFFF"))
captures <- captures + theme(panel.grid.major = element_line(colour = "black", linetype = "dotted"))
captures <- captures + ylab('Latitude')+ xlab('Longitude')
captures <- captures + theme(axis.title.x = element_text(face="bold", colour="#000000", size=14))
captures <- captures + theme(axis.title.y = element_text(face="bold", colour="#000000", size=14))
captures <- captures + theme(axis.text.x = element_text(face="bold", colour="#000000", size=12))
captures <- captures + theme(axis.text.y = element_text(face="bold", colour="#000000", size=12))
captures <- captures + theme(panel.background = element_rect(colour = "black"))
captures <- captures + theme(panel.border = element_rect(colour = "black", fill=NA))
captures <- captures + geom_point(data = latlong, aes(x=fourregions_merged.longitude, y=fourregions_merged.latitude, drop=FALSE), size = 1.0, shape =21, fill= "red")
## in here you can adjust the size of the point, the colour, the shape etc. 
captures

##Map species distribution. Can put whatever species or year you want to map in##


##Captures by Region##
latlongregion <- data.frame(fourregions_merged$latitude, fourregions_merged$longitude, fourregions_merged$region)
captures.regions<- ggplot()
captures.regions <- captures.regions + geom_map(data=coastmap3, map=coastmap3,
                                                aes(x=long, y=lat, map_id=region),
                                                fill="ivory2", color="black", size=1)
captures.regions <- captures.regions + geom_map(data=data.frame(region="Canada"), map=coastmap3,
                                                aes(map_id=region), fill="grey90", size=1)
captures.regions <- captures.regions + xlim(-80, -45) + ylim(42, 58)
## adjust these lat longs above to get the proper dimensions of the map
captures.regions <- captures.regions + theme(panel.background = element_rect(fill = "#F3FFFF"))
captures.regions <- captures.regions + theme(panel.grid.major = element_line(colour = "black", linetype = "dotted"))
captures.regions <- captures.regions + ylab('Latitude')+ xlab('Longitude')
captures.regions <- captures.regions + theme(axis.title.x = element_text(face="bold", colour="#000000", size=14))
captures.regions <- captures.regions + theme(axis.title.y = element_text(face="bold", colour="#000000", size=14))
captures.regions <- captures.regions + theme(axis.text.x = element_text(face="bold", colour="#000000", size=12))
captures.regions <- captures.regions + theme(axis.text.y = element_text(face="bold", colour="#000000", size=12))
captures.regions <- captures.regions + theme(panel.background = element_rect(colour = "black"))
captures.regions <- captures.regions + theme(panel.border = element_rect(colour = "black", fill=NA))
captures.regions <- captures.regions + geom_point(data =latlongregion, aes(x=fourregions_merged.longitude, y=fourregions_merged.latitude, 
                                                                           colour=factor(fourregions_merged.region), drop=FALSE), size = 1.0, shape =21)+
  labs(colour="Region")
## in here you can adjust the size of the point, the colour, the shape etc. 
captures.regions


##Map Species distribution, fill in species that you want mapped##
speciesdis <- data.frame(fourregions_merged$latitude, fourregions_merged$longitude, fourregions_merged$year, fourregions_merged$species, fourregions_merged$region)
speciesdis2 <- speciesdis[speciesdis$fourregions_merged.year>2005 & speciesdis$fourregions_merged.species=="Chionoecetes opilio",]
toppecies <- toppecies + geom_map(data=coastmap3, map=coastmap3,
                                  aes(x=long, y=lat, map_id=region),
                                  fill="ivory2", color="black", size=1)
toppecies <- toppecies + geom_map(data=data.frame(region="Canada"), map=coastmap3,
                                  aes(map_id=region), fill="grey90", size=1)
captures.regions <- captures.regions + xlim(-80, -45) + ylim(42, 58)
toppecies <- toppecies + theme(panel.background = element_rect(fill = "#F3FFFF"))
toppecies <- toppecies + theme(panel.grid.major = element_line(colour = "black", linetype = "dotted"))
toppecies <- toppecies + ylab('Latitude')+ xlab('Longitude')
toppecies <- toppecies + theme(axis.title.x = element_text(face="bold", colour="#000000", size=14))
toppecies <- toppecies + theme(axis.title.y = element_text(face="bold", colour="#000000", size=14))
toppecies <- toppecies + theme(axis.text.x = element_text(face="bold", colour="#000000", size=12))
toppecies <- toppecies + theme(axis.text.y = element_text(face="bold", colour="#000000", size=12))
toppecies <- toppecies + theme(panel.background = element_rect(colour = "black"))
toppecies <- toppecies + theme(panel.border = element_rect(colour = "black", fill=NA))
toppecies <- toppecies + geom_point(data = speciesdis2, aes(x=fourregions_merged.longitude, y=fourregions_merged.latitude, 
                                                            colour=factor(fourregions_merged.region), drop=FALSE, size = 1.0, shape =21))+
  labs(colour="Region")+ggtitle("Chionoecetes opilio Observations")+
  facet_wrap(~fourregions_merged.year)
## in here you can adjust the size of the point, the colour, the shape etc. 
toppecies


#####Mean Depth######
meandepth <- fourregions_merged%>%group_by(region, month)%>%summarise(meandepth=mean(depth,na.rm=T))%>%ungroup()%>%data.frame
mindepth <- fourregions_merged%>%group_by(region, month)%>%summarise(dmin=mean(dmin,na.rm=T))%>%ungroup()%>%data.frame
maxdepth <- fourregions_merged%>%group_by(region, month)%>%summarise(dmax=mean(dmax,na.rm=T))%>%ungroup()%>%data.frame

##Excludes data points from Quebec because there is no depth information for that region##
meandepth_plot <- ggplot(data=meandepth, aes(month, meandepth)) +
  geom_point(aes(colour=region))+
  labs(x="Month", y="Mean Depth (m)")+
  theme_bw()+
  scale_x_discrete(limits=c(1:12), labels=c("January", "February", "March", "April", "May", "June", "July", "August", "September",
                                            "October", "November", "December"))+
  theme(axis.text.x = element_text(angle=20))
meandepth_plot

##Species length##
meanlength <- fourregions_merged%>%group_by(species)%>%summarise(meanlength_cm=mean(length_cm,na.rm=T))%>%ungroup()%>%data.frame
meanlength_plot <- ggplot(data=meanlength, aes(species,meanlength_cm)) +
  geom_point(aes(colour=meanlength_cm))+
  labs(x="Species", y="Mean Length (cm)")+
  theme_bw()+
  scale_x_discrete(expand=c(0,0))+
  theme(axis.text.x = element_text(angle=90))
meanlength_plot


##############List invertebrates and vertebrates#################
invertebrates <- fourregions_merged[fourregions_merged$invertebrate==1,]
vertebrates <- fourregions_merged[fourregions_merged$invertebrate==0,]
unique(invertebrates$species) ##54 Invertebrates
unique(vertebrates$species) ##129 Vertebrates


###############----Temperature----###########################
#############################################################
##Temperature column is only for Gulf Region
##The surf_temp & bott_temp column is only for Maritime Region
##The bott_temp column is only for Newfoundland Region
##Quebec has no temperature data


##Mean temperature
meantemp <- fourregions_merged%>%group_by(species, region, year)%>%summarise(meantemp=mean(temperature,na.rm=T))%>%ungroup()%>%data.frame
##plot temperatures
temperature <- meantemp[!is.nan(meantemp$meantemp),]
ggplot(temperature, aes(x=meantemp, y=species))+
  geom_point()+theme_bw()

##I took the mean bottom temperatures of each capture and then took the mean temperatures of those species##
##This gave me one mean temperature for each
meanbottemp <- fourregions_merged%>%group_by(species, region, year)%>%summarise(meantemp=mean(bott_temp,na.rm=T))%>%ungroup()%>%data.frame
meantemp_species <- meanbottemp%>%group_by(species)%>%summarise(meantemp_species=mean(meantemp,na.rm=T))%>%ungroup()%>%data.frame

##plot temperatures
meanbottspecies <- meantemp_species[!is.na(meantemp_species$meantemp_species),]
ggplot(meantemp_species, aes(x=meantemp_species, y=species))+
  geom_point()+theme_bw()


##Vertebrate Temperature##
meantempvert <- vertebrates%>%group_by(species, region, year)%>%summarise(meantempvertebrates=mean(temperature,na.rm=T))%>%ungroup()%>%data.frame
meantempvert2 <- meantempinvert[!is.nan(meantempinvert$meantempvertebrates),]
meantempvert3<- meantempinvert2%>%group_by(species)%>%summarise(meantemp_species=mean(meantempvertebrates,na.rm=T))%>%ungroup()%>%data.frame

tt <- ggplot(meantempvert3, aes(x=meantemp_species, y=species))+
  geom_point()+theme_bw()


#############################################
####Tables of species by year within region###
#############################################
newfoundlandcount <- table(fourregions_merged$species, fourregions_merged$year, fourregions_merged$region=="NEWFOUNDLAND")
maritimecount <- table(fourregions_merged$species, fourregions_merged$year, fourregions_merged$region=="MARITIME")
gulfcount <- table(fourregions_merged$species, fourregions_merged$year, fourregions_merged$region=="GULF")
quebeccount <- table(fourregions_merged$species, fourregions_merged$year, fourregions_merged$region=="QUEBEC")