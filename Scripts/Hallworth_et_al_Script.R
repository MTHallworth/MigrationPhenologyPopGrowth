library(raster)
library(sp)

#####################################################################
#
# STRUCTURE DATA FOR TRUE SURVIVAL MODEL 
#
#####################################################################
CH <- read.csv("Data/AnnualHistory2009_2013_Males.csv")

OVENinds <- read.csv("Data/OVEN_band.csv")

CH <- merge(CH,OVENinds, by.x = "Band", by.y = "AluminumBand", all.x = TRUE)

OVEN_2009_locs <- read.csv("Data/OVEN_2009_locs.csv")
OVEN_2010_locs <- read.csv("Data/OVEN_2010_locs.csv")
OVEN_2011_locs <- read.csv("Data/OVEN_2011_locs.csv")
OVEN_2012_locs <- read.csv("Data/OVEN_2012_locs.csv")
OVEN_misc_locs <- read.csv("Data/misc_bird_locs.csv")

# reformat bird id to match the colorband format #
OVEN_2010_locs$colorband <- gsub("_","",paste0(substr(OVEN_2010_locs$Bird,1,2),",",substr(OVEN_2010_locs$Bird,3,4)))
OVEN_2011_locs$colorband <- gsub("_","",paste0(substr(OVEN_2011_locs$Bird,1,2),",",substr(OVEN_2011_locs$Bird,3,4)))
OVEN_2012_locs$colorband <- gsub("_","",paste0(substr(OVEN_2012_locs$Bird,1,2),",",substr(OVEN_2012_locs$Bird,3,4)))

# merge by colorband #
X2010 <- merge(OVEN_2010_locs,OVENinds,by.x = "colorband", by.y = "colorband", all.x = TRUE)
X2011 <- merge(OVEN_2011_locs,OVENinds,by.x = "colorband", by.y = "colorband", all.x = TRUE)
X2012 <- merge(OVEN_2012_locs,OVENinds,by.x = "colorband", by.y = "colorband", all.x = TRUE)

# subset to include only the few attributes of interest #
# convert date into POSIXct 
OVEN_2009_locs$Date <- as.POSIXlt(OVEN_2009_locs$DateBanded,format = "%m/%d/%Y")
OVEN_2009_locs <- OVEN_2009_locs[,c("AluminumBand","colorband","Age","Sex","EarliestDate","Date","x_proj","y_proj")]
colnames(OVEN_2009_locs) <- c("AluminumBand","colorband","Age","Sex","EarliestDate","Date","long","lat")

Locs2009 <- SpatialPointsDataFrame(SpatialPoints(cbind(OVEN_2009_locs$long,OVEN_2009_locs$lat),
                                               CRS("+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")),
                                 OVEN_2009_locs)

Locs2009<-Locs2009@data

Locs2010 <- X2010[,c("AluminumBand","colorband","Age","Sex","EarliestDate","Descriptio","x_proj","y_proj")]
colnames(Locs2010) <- c("AluminumBand","colorband","Age","Sex","EarliestDate","Date","long","lat")
Locs2010 <- Locs2010[!is.na(Locs2010$long),]
Locs2010$Date <- as.POSIXlt(Locs2010$Date, format = "%m/%d/%Y")
L.2010 <- SpatialPointsDataFrame(SpatialPoints(cbind(Locs2010$long,Locs2010$lat),
                                               CRS("+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")),
                                 Locs2010)

Locs2010$long <-L.2010@coords[,1]
Locs2010$lat <- L.2010@coords[,2]

Locs2011 <- X2011[,c("AluminumBand","colorband","Age","Sex","EarliestDate","Date","long","lat")]
Locs2011$Date <- as.POSIXlt(Locs2011$Date,format = "%d-%b-%y")
Locs2011 <- Locs2011[!is.na(Locs2011$long),]
L.2011 <- SpatialPointsDataFrame(SpatialPoints(cbind(Locs2011$long,Locs2011$lat),
                                               CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")),
                                Locs2011)
L.2011 <- spTransform(L.2011, CRS("+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
Locs2011$long <- L.2011@coords[,1]
Locs2011$lat <- L.2011@coords[,2]

Locs2012 <- X2012[,c("AluminumBand","colorband","Age","Sex","EarliestDate","Date","long","lat")]
Locs2012$Date <- as.POSIXlt(Locs2012$Date,format = "%d-%b-%y")
Locs2012 <- Locs2012[!is.na(Locs2012$long),]
L.2012 <- SpatialPointsDataFrame(SpatialPoints(cbind(Locs2012$long,Locs2012$lat),
                                               CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")),
                                Locs2012)
L.2012 <- spTransform(L.2012, CRS("+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
Locs2012$long <- L.2012@coords[,1]
Locs2012$lat <- L.2012@coords[,2]


OVEN_misc_locs$Date <- as.POSIXct(OVEN_misc_locs$Date, format = "%m/%d/%Y")
LocsMisc <- OVEN_misc_locs[,c("AluminumBand","colorband","Age","Sex","EarliestDate","Date","x_proj","y_proj")]
colnames(LocsMisc) <- c("AluminumBand","colorband","Age","Sex","EarliestDate","Date","long","lat")

locs <- rbind(Locs2009,Locs2010,Locs2011,Locs2012,LocsMisc)
locs$Year <- format(locs$Date,"%Y")
locs[locs$Year == 2006,]

locs <- merge(locs,OVENinds,by.x = "AluminumBand",by.y = "AluminumBand", all.x = TRUE)

locs <- locs[!locs$Age.x=="N",]

locs <- locs[!is.na(locs$long),]
#### Extract plot grids #####
spLocs <- SpatialPointsDataFrame(SpatialPoints(cbind(locs$long,locs$lat),
                                 CRS("+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")),
                                 locs)

#shapefile(spLocs,filename = "Spatial_Layers/OVEN_locs_2009_2012.shp",overwrite = TRUE)

Effort09 <- shapefile("Spatial_Layers/effort_2009.shp")
Effort10 <- shapefile("Spatial_Layers/effort_2010.shp")
Effort11 <- shapefile("Spatial_Layers/effort_2011.shp")
Effort12 <- shapefile("Spatial_Layers/effort_2012.shp")


grid.oven <- array(NA, c(334,4,4))

for(i in 1:nrow(Effort09)){
grid.oven[i,1,1] <- min(Effort09[i,]@polygons[[1]]@Polygons[[1]]@coords[,1])
grid.oven[i,2,1] <- max(Effort09[i,]@polygons[[1]]@Polygons[[1]]@coords[,1])
grid.oven[i,3,1] <- min(Effort09[i,]@polygons[[1]]@Polygons[[1]]@coords[,2])
grid.oven[i,4,1] <- max(Effort09[i,]@polygons[[1]]@Polygons[[1]]@coords[,2])
}

for(i in 1:nrow(Effort10)){
grid.oven[i,1,2] <- min(Effort10[i,]@polygons[[1]]@Polygons[[1]]@coords[,1])
grid.oven[i,2,2] <- max(Effort10[i,]@polygons[[1]]@Polygons[[1]]@coords[,1])
grid.oven[i,3,2] <- min(Effort10[i,]@polygons[[1]]@Polygons[[1]]@coords[,2])
grid.oven[i,4,2] <- max(Effort10[i,]@polygons[[1]]@Polygons[[1]]@coords[,2])
}

for(i in 1:nrow(Effort11)){
grid.oven[i,1,3] <- min(Effort11[i,]@polygons[[1]]@Polygons[[1]]@coords[,1])
grid.oven[i,2,3] <- max(Effort11[i,]@polygons[[1]]@Polygons[[1]]@coords[,1])
grid.oven[i,3,3] <- min(Effort11[i,]@polygons[[1]]@Polygons[[1]]@coords[,2])
grid.oven[i,4,3] <- max(Effort11[i,]@polygons[[1]]@Polygons[[1]]@coords[,2])
}

for(i in 1:nrow(Effort12)){
grid.oven[i,1,4] <- min(Effort12[i,]@polygons[[1]]@Polygons[[1]]@coords[,1])
grid.oven[i,2,4] <- max(Effort12[i,]@polygons[[1]]@Polygons[[1]]@coords[,1])
grid.oven[i,3,4] <- min(Effort12[i,]@polygons[[1]]@Polygons[[1]]@coords[,2])
grid.oven[i,4,4] <- max(Effort12[i,]@polygons[[1]]@Polygons[[1]]@coords[,2])
}

grid.oven[,1,][is.na(grid.oven[,1,])] <- 0
grid.oven[,2,][is.na(grid.oven[,2,])] <- 1
grid.oven[,3,][is.na(grid.oven[,3,])] <- 0
grid.oven[,4,][is.na(grid.oven[,4,])] <- 1

##############################################################################################################
males <- unique(CH$Band[!CH$Age=="N"])

# remove males that were banded as Nestlings and returned # 
males <- males[!(males %in% c("2391-68517","2401-30633","2401-30659"))]

year <- 2009:2013

maleLocs <- array(NA,c(length(males),5,2))

for(i in 1:length(males)){
for(y in 1:5){
tmp <- subset(locs,subset = (as.character(locs$AluminumBand) == as.character(males[i]) & as.numeric(locs$Year) == year[y]))
maleLocs[i,y,1] <- ifelse(nrow(tmp)>=1,median(tmp$long),NA)
maleLocs[i,y,2] <- ifelse(nrow(tmp)>=1,median(tmp$lat),NA)
}
}
rownames(maleLocs)<-males
colnames(maleLocs)<-year

###############################################################################################################
first <- rep(NA,length(males))
CapHist <- array(0,c(length(males),4))
rownames(CapHist) <- males
colnames(CapHist) <- 2009:2012
for(i in 1:length(males)){
tmp <- subset(CH,Band == males[i])
first[i] <- min(which(tmp[1,4:7]==1))
CapHist[i,which(tmp[1,4:7]==1)] <- 1
}

###############################################################################################################
#
#
# NEST SURVIVAL DATA  
#
#
###############################################################################################################

NestData<-read.csv("Data/NestDailysurvive.csv")
names(NestData)
# set up data #
nest.ch<-as.matrix(NestData[,grep(names(NestData),pattern = "ch.")])
plot<-as.numeric(NestData$Plot)
first.nest<-NestData$Active1
last.nest<-NestData$LastActive1
Nests<-dim(nest.ch)[1]

laydate <- NestData$MinLayDate

laydate <- (laydate - mean(laydate))/sd(laydate)

###############################################################################################################
#
#
# NON-BREEDING DEPARTURE - ARRIVAL
#
#
###############################################################################################################

glDates <- read.csv("Data/GL_dates_rates.csv")

mean(glDates[,6]-glDates[,5],na.rm = TRUE)
nb.depart <- seq(min(glDates[!is.na(glDates[,2]),2]),
                  max(glDates[!is.na(glDates[,2]),2]),,100)
b.depart <- seq(min(glDates[,5]),
                max(glDates[,5]),,100)

library(jagsUI)
win.data <- list(AllBirds = nrow(glDates),
                 departure.nb = glDates[!is.na(glDates[,2]),2],
                 arrive.b = glDates[!is.na(glDates[,2]),3],
                 departure.b = glDates[,5],
                 arrive.nb = glDates[,6],
                 sp.mig.rate = glDates[!is.na(glDates[,2]),4],
                 f.mig.rate = glDates[,7],
                 nb.depart = nb.depart,
                 b.depart = b.depart)

D <- jags(model = "Models/DepartArrive.txt",
          data = win.data,
          n.chains = 3,
          n.iter = 50000,
          n.burn = 25000,
          n.adapt = 10000,
          parallel = TRUE,
          param = c("Int.nb","Int.b","slope.nb","slope.b",
                    "Int.sp.mig","slope.sp.mig",
                    "Int.f.mig","slope.f.mig",
                    "b.arrive","nb.arrive",
                    "sp.mig.est","f.mig.est"))

###############################################################################################################
#
#
# Arrival INFORMATION  
#
#
###############################################################################################################
SI_data <- read.csv("Data/Seasonal_Interactions_Data.csv")
names(SI_data)
ArrivalDate <- SI_data$Arrival_Julian

m.Arrive <- mean(ArrivalDate,na.rm = TRUE)
sd.Arrive <- sd(ArrivalDate,na.rm = TRUE)

# Simulate arrival 5 days later than observed #
sim.arrive <- ArrivalDate+5
sim.arrive <- (sim.arrive - m.Arrive)/sd.Arrive

# Standardize arrival date #
ArrivalDate <- (ArrivalDate-mean(ArrivalDate,na.rm = TRUE))/sd(ArrivalDate,na.rm = TRUE)
# If no arrival date - feed the mean arrival #
ArrivalDate[is.na(ArrivalDate)]<- 0 
ArrivalDate[6] <- 0

###############################################################################################################
#
#
# CONDITION  
#
#
###############################################################################################################
# Arrival condition 
# Body condition of birds within 3 days of arrival
ArriveCondition <- SI_data$BodyCondition[which(SI_data$DiffCapArrival<=3)]
ArriveDateCond <- ArrivalDate[which(SI_data$DiffCapArrival<=3)]
nCond <- length(ArriveCondition)

# change in condition
ChangeCond <- SI_data$RecapCondition-SI_data$BodyCondition
# remove extreme outlier #
ChangeCond[59] <- NA
change.arrival <- ArrivalDate[which(!is.na(ChangeCond))]
ChangeCond <- ChangeCond[!is.na(ChangeCond)]
nChange <- length(ChangeCond)

###############################################################################################################
#
#
# PAIRING INFORMATION  
#
#
###############################################################################################################

Paired <- SI_data$Paired
nPair <- length(Paired)

# Pairing dates before first failures
PairDate150 <-SI_data$PairDate150
ArrivalDatePair <- ArrivalDate[which(!is.na(PairDate150))]
PairDate150 <- PairDate150[!is.na(PairDate150)]
nPairDate <- length(PairDate150)

###############################################################################################################
#
#
# REPRODUCTIVE INFORMATION  
#
#
###############################################################################################################
# CLUTCH COMPLETION INFORMATION #
ClutchComplete<-SI_data$DateFirstClutchComplete
clutchFledged <- SI_data$Number.Fledged[!is.na(ClutchComplete)]
clutcharrive <- ArrivalDate[!is.na(ClutchComplete)]
ClutchComplete <- ClutchComplete[!is.na(ClutchComplete)]

# Number of young fledged #
NumberFledged <- SI_data$Number.Fledged
non.na <- which(!is.na(NumberFledged))
arrival.fledged <- ArrivalDate[non.na]
NumberFledged <- NumberFledged[non.na]
nFledge <- length(NumberFledged)

###############################################################################################################
#
#
# SET UP SURVEY DATA   
#
#
###############################################################################################################
##### Find Survey locations that fall within long-term demography plots #####

# read in plot effort polygon file that defines demography plots #
effort <- shapefile("C:/Users/hallworthm/Dropbox (Smithsonian)/BTBW project/Spatial_Data/PlotShapefiles/PlotEffortYears_100m.shp")

# shapefile that contains survey locations #
surveyPoints <- shapefile("C:/Users/hallworthm/Google_Drive/BTBW_HBEF/Geolocator_2015/Spatial_Layers/WGS84/Schwarz_Bird_WGS84.shp")

# set projection of the plot effort polygon file #
crs(effort) <- "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" 

# project the survey points to match that of the demography plots #
surveyPoints <- spTransform(surveyPoints,CRS("+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Extrac the survey points that fall within the demography plots #
dem_locs <- surveyPoints[which(!is.na(over(surveyPoints,rgeos::gUnionCascaded(effort, id = effort$Plot2009)))),4]@data

PlotAttributes <- read.csv("Data/PlotAttributes.csv")

Elev <- merge(dem_locs,PlotAttributes[,c(1,3)],by.x = "SCHWARZ_ID", by.y = "SchwarzID")[,2]
Elev <- (Elev-mean(Elev))/sd(Elev)
###############################################################
#
#   Format survey data 
#
###############################################################

VWdata <- read.csv("C:/Users/hallworthm/Desktop/Seasonal_Interactions_MS/Data/Valley-wide_bird_census_1999_2016.csv", header = TRUE, sep = ",")

# Let's look at the first few rows of data 

head(VWdata)

# Remove data that needs to be deleted # This was fixed in FileMaker #
VWdata<-VWdata[VWdata$Comments!="**PLEASE DELETE**",]

# Some of the species codes have new lines for example "BTBW\n" this next code removes that & turns "BTBW\n" into "BTBW"
VWdata$Species<-gsub(pattern = "[\r\n]", replacement = "", x = VWdata$Species)
VWdata$Species<-gsub(pattern = "[\r\v]", replacement = "", x = VWdata$Species)

# Some rows of the data were entered by accident with no new species - they have blank species#
# remove those rows from the data set #
VWdata<-VWdata[VWdata$Species != "", ]


# Confirm that they were removed by showing the rows that have blank for species #
# should be 0 rows and it is #
VWdata[which(VWdata$Species ==""),]

# remove data that were used for a separate analysis - Multiple Observer experiment #
VWdata <- VWdata[VWdata$Replicate != "4-MultObs",]
VWdata$Replicate[VWdata$Replicate == "-3"] <- 3
VWdata$Replicate <- droplevels(VWdata$Replicate)

VWdata$Replicate <- as.numeric(as.character(VWdata$Replicate))

# Use only the data collected within 50m of the point so no double counting #
VWdata<-VWdata[VWdata$Distance==1,]

# Use only the survey locations that fall within the demography plots #
VWdata <- VWdata[VWdata$Plot %in% dem_locs$SCHWARZ_ID,]

# Determine the Number of counts
replicate <-min(VWdata$Replicate,na.rm=TRUE):max(VWdata$Replicate,na.rm=TRUE)

# Determine the number of periods within a count # - should be 3 - one record has value 4
# Identify and fix the record that was entered incorrectly - NOTE this was updated in FileMaker by MTH #
 
period<-min(VWdata$Period,na.rm=TRUE):max(VWdata$Period,na.rm=TRUE) # Within the count

# number of schwarz plots surveyed #
sites<-levels(as.factor(VWdata$Plot))

# number of species observed #
spp <- levels(factor(VWdata$Species))

# List the species of interest (S.O.I) #
S.O.I <- "OVEN"

# Format the dates
VWdata$Date2 <- as.Date(VWdata$Date, format="%m/%d/%Y")

# observation year #
VWdata$Year <- as.numeric(format(VWdata$Date2,"%Y"))

years <- 2009:2013

# observation ordinal day #
VWdata$DOY <- as.numeric(format(VWdata$Date2, "%j"))

# Create a Plot x Replicate array of abundance for the species of interest #
# this makes an empty 3 dimensional array - Plot x Replicate x Species that is filled with NAs #

# create an empty array (filled with NA) -   # of rows = sites   # of columns = periods   # of dimensions = replicates
spec.mat <- array(NA, c(length(sites),4,length(years),length(S.O.I)))

rownames(spec.mat)<-sites
colnames(spec.mat) <- replicate[1:4]
dimnames(spec.mat)[3][[1]] <- years
dimnames(spec.mat)[4][[1]] <- S.O.I

# Here is how to fill the array with the data that we want # 
for(y in 1:length(years)){
temp.yr <- subset(VWdata, Year == years[y])
for(s in 1:length(S.O.I)){
temp.sp <- subset(temp.yr, Species == S.O.I[s]) 
# for each replicate #
  for(i in 1:4){
      temp.rp <- subset(temp.sp, Replicate == replicate[i])
# for each plot #
         for(k in sites){
               temp.st <- subset(temp.rp, Plot == as.numeric(k))
               spec.mat[k,i,y,s] <- as.numeric(length(temp.st$Species[temp.st$New.Record == 1]))
          }  # For plot
      }  # For Replicate
  }# For Species
}# years 

#########################################################################################
#
# STRUCTURE OBSERVATION COVARIATES 
#
#########################################################################################

# DATE OF COUNT # 

doy <- array(NA, c(length(sites), length(replicate), length(years)))

rownames(doy) <- sites
dimnames(doy)[2][[1]] <- replicate
dimnames(doy)[3][[1]] <- years

for(y in 1:length(years)){
temp.yr <- subset(VWdata, Year == years[y])
for(k in sites){
  for(i in 1:4) {
    temp <- subset(temp.yr, Replicate == replicate[i] & Plot==k)
    temp.DOY <- as.numeric(levels(factor(temp$DOY)))
    if (length(temp.DOY)>0){
        doy[k,i,y] <- min(temp.DOY)
        } # if
      } # i 
   } # k
} # y

# Change the array from characters to numeric #
doy <- structure(as.numeric(doy), dim=dim(doy), dimnames=dimnames(doy))

# All covariates need to be standardized. You can use the function scale() to do that #
doy.scaled<-scale(doy)
doy.scaled[is.na(doy.scaled)]<-0

# FORMAT TIME OF COUNTS #

VWdata$Time2 <-gsub(" AM", "", VWdata$Time)

# Create a site x occasion matrix of times 

time <- array(NA, c(length(sites), length(replicate), length(years)))

rownames(time) <- sites
colnames(time)<-replicate
dimnames(time)[3][[1]] <- years

for(y in 1:length(years)){
temp.yr <- subset(VWdata,Year == years[y])
for(k in sites){
for(i in 1:4){
    temp <- subset(temp.yr, Replicate== replicate[i] & Plot==k)
    temp.time <- temp$Time2
    if(length(temp.time) > 0) {
        temp.time <- substr(temp.time,1,5)
        time[k,i,y] <- as.numeric(gsub(":","",(unique(temp.time)[1])))
        }
    }
}
}

# Scale the time covariate #
time <- (time - mean(time,na.rm = TRUE))/sd(time,na.rm = TRUE)
# Fill missing data with the mean - which is 0 after being scaled #
time[is.na(time)]<-0


########### FIX UP THE spec.mat A LITTLE BIT ##############################

OVEN <- date <- sur.time <- array(NA,c(dim(spec.mat)[1],3,dim(spec.mat)[3]))
for(i in 1:5){
OVEN[,,i] <- spec.mat[,2:4,i,1]
date[,,i] <- doy[,2:4,i]
sur.time[,,i] <- time[,2:4,i]
}
date <- (date - mean(date,na.rm = TRUE))/sd(date,na.rm = TRUE)
date[is.na(date)] <- 0 


###############################################################################################################


## ------------------------------------------------------------------------
N<-function(x){
  Nst<-apply(x,c(1,3),max,na.rm=TRUE)+2
  
  # If Nst==NA change value to 3 #
  Nst[Nst==-Inf]<-NA
  Nst[is.na(Nst)]<-3
  return(Nst)
}

#### ------------------ Function for surival state init ------------------- ####

known.state.cjs <- function(x,FirstCap){
   state <- x
   for (i in 1:dim(x)[1]){
      n1 <- min(which(x[i,]==1))
      n2 <- max(which(x[i,]==1))
      state[i,n1:n2] <- 1
      state[i,1:FirstCap[i]]<- NA
      state[i,n1] <- NA
      }
   state[state==0] <- NA
   return(state)
   }

#### ------------------ Function for inits  ------------------- ####
z <- known.state.cjs(x = CapHist, FirstCap = first)

# Set Initial values
inits <- function(){
              list(z = known.state.cjs(x = CapHist, FirstCap = first),
                   N=N(OVEN))
  
}

library(jagsUI)

 
win.data <- list(# # # # # # # # # # # #
                 # True Survival Model #
                 # # # # # # # # # # # #
                 FirstCap = first,
                 CH = CapHist,
                 inds = length(males),
                 FoundLocs = maleLocs[,1:4,], 
                 ngrids = nrow(grid.oven),
                 n.years = 4,
                 grid.study.boundaries = grid.oven,
                 xlim = c(278325,284125),
                 ylim = c(4868125,4871525),
                 # # # # # # # 
                 # Condition #
                 # # # # # # #
                 ArriveDateCond = ArriveDateCond,
                 ArriveCond = ArriveCondition ,
                 nCond = nCond,
                 nChange =  nChange,
                 ChangeDate = change.arrival,
                 ChangeCond = ChangeCond,
                 # # # # # # # # # #
                 # Pairing Success #
                 # # # # # # # # # #
                 nPair = nPair,#Birds
                 Paired = Paired,
                 ArrivalDate = ArrivalDate,
                 nPairDate = nPairDate,
                 PairDate150 = PairDate150,
                 ArrivalDatePair = ArrivalDatePair,
                 # # # # # # # # # # #
                 # Clutch Completion #
                 # # # # # # # # # # #
                 Clutch.complete.date = (ClutchComplete-mean(ClutchComplete))/sd(ClutchComplete),
                 abc = ifelse(clutchFledged>0,1,0),
                 nC.Fledge = length(clutchFledged),
                 clutchFledged = clutchFledged,
                 ClutchComplete = ClutchComplete,
                 clutcharrive = clutcharrive,
                 clutchbirds = length(ClutchComplete),
                 # # # # # # # # # 
                 # Nest Survival #
                 # # # # # # # # #
                 first.nest = first.nest,
                 last.nest = last.nest,
                 nest.ch = nest.ch,
                 Nests = Nests,
                 laydate = laydate,
                 # # # # # # # # # # # # #
                 # Reproductive success  #
                 # # # # # # # # # # # # #
                 nFledge = nFledge,#Birds
                 NumberFledged = NumberFledged,
                 a = ifelse(NumberFledged>0,1,0),
                 arrival.fledged = arrival.fledged,
                 # # # # # # # # # #
                 # N-mixture model #
                 # # # # # # # # # #
                 y = OVEN,
                 nyears = 5,
                 nreps = dim(OVEN)[2],
                 nschwarz = dim(OVEN)[1],
                 time = sur.time,
                 date = date,
                 Elev = Elev,
                 Renest = 0.348,
                 DoubleBrood = 0.130,
                 # # # # # # # # # #
                 # PREDICTION DATA #
                 # # # # # # # # # #
                 n.arrival = seq(-3,3,,1000),
                 sim.arrival = seq(min(sim.arrive,na.rm = TRUE),
                                   max(sim.arrive,na.rm = TRUE),,1000))

params <- c("mean.phi","mean.p","sigma", # True Survival params
            "a.change","beta.change", # Change in condition
            "a.arrive.con","beta.arrival.con", # Arrival condition
            "mean.pair","beta.pair","alpha.pair", # Pairing info
            "beta.pair.date",
            "nest.surv", "beta.clutch",#nest survival 
            "beta.FstClutch",#arrival clutch complete
            "beta.ClutchFldg",
            "percapfledge", "alpha.fledged","beta.fledged", # repro data
            "omega","omega1",
            "alpha","beta","pInt", # N-mix params
            "N", # population size 
            "lambda.est", "mean.lambda",
            "growth","JuvSurv",
            "new.lambda","mean.growth") #pop growth

n.adapt = 10000
n.iter = 50000
n.burn  = 25000
n.thin = 5

library(jagsUI)
a<-Sys.time()
Sys.time()
M<-jagsUI::jags(model = "Models/IntegratedPopulation_model.txt",
                module = c('glm'),
                data = win.data,
                inits = inits,
                n.adapt = n.adapt,
                n.chains = 3, 
		    n.cores = 3,
                n.iter = n.iter, 
                n.thin = n.thin,
                n.burn = n.burn,
                parallel = TRUE,
                DIC = FALSE,
                parameters.to.save = params,
		    seed = 12345678)
Sys.time()
Sys.time()-a
M
str(M$summary)
head(M$summary,100)
n.arrival <- seq(-3,3,,1000)
M$summary[grep("beta",dimnames(M$summary)[[1]]),]
