##############################################################################################
####   Script for adding eastings + northings, land-use, and climate data to sites file   ####
##############################################################################################

##### script setup ####

### Clear the current workspace (DOESN'T CLEAR LOADED LIBRARIES)
rm(list=ls())

### install if necessary and then load the libraries you need

j <- c("rstudioapi","rnrfa","rgdal","raster","rstudioapi","plyr","vegan","lme4","ggplot2","RColorBrewer","car","gridExtra","MuMIn","RVAideMemoire","emmeans","reshape2","moments","ggmap","grid","egg","RgoogleMaps","ncdf4")

new.packages <- j[!(j %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


lapply(j, require, character.only = TRUE)  # loads up any libraries that aren't already loaded

### set working directory to script's saved location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### RIS sites ####

# read in the base data
# the default importer would read GRIDREF as a factor, but we want it to be a character string here, so use stringsAsFactors=F

sites<-read.csv("Checked/Landuse and climate/sites.csv", header=TRUE, stringsAsFactors = F)
summary(sites)
sites$SITE <- revalue(sites$SITE, c("Barnfield" = "Barnfield Site 1"))

# we already have lat/lon data here so just need eastings/northings


# some of these sites are not labelled with location data (in this case not necessarily because the locations are sensitive!)
# take these out for now, but we'll need to preserve them and add them back in at the end

no_loc <- sites[which(sites$GRIDREF==""),]

sites_loc <- sites[which(!(sites$GRIDREF=="")),]

# now we are ready to convert whatever remains

x <- osg_parse(sites_loc$GRIDREF)

sites_loc$EASTING <- x[[1]]
sites_loc$NORTHING <- x[[2]]

### we now have eastings and northings! let's do a brief visual check that all's well by plotting them on a map...

# to do this we actually also need these with fewer digits
sites_loc$HEast <- sites_loc$EASTING/1000
sites_loc$HNorth <- sites_loc$NORTHING/1000

# plot base map to test
map1<- map_data(map = "world", region = c("UK","Ireland"))

map <- ggplot(map1, aes(x = long, y = lat))+
  geom_polygon(aes(group = group),fill = "white", colour = "black") +
  geom_point(data = sites_loc, aes(x = LON, y = LAT)) +
  theme_minimal()

map


# Write clean file

write.csv(sites_loc, "Checked/Landuse and climate/sites_full.csv", row.names=F)



############## habitat data ####

# we also want to add in habitat-type data for each of the sites, using the Land Cover Map 2007
# we want to have both the point habitat type at the site and also the modal habitat type in boxes of a couple of sizes around the site

# first read in the data again
lcm2007 <- raster("E:/LCM 2007 25m Raster GB/data/25m_target_class/GB/lcm2007_25m_gb.tif")
lcm2007


# now, we want to generate a small subset of this data for every site in sites_loc (as it's based on location data, 
# we still can't quite do all sites)

# we need the function that calculates the mode

getmode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}


# we want to extract three things for each point - the exact land use at the point,
# the small-scale main land use (100 m) and the large-scale main land use (1 km)

raster::extract(lcm2007,xy,buffer = 100,fun=getmode)

## now we are ready to extract this information for each row in sites_all

# seed an output table

landuse <- data.frame(n=numeric(),
                      SITENAME=factor(),
                      SITENO=numeric(),
                      point=numeric(),
                      small=numeric(),
                      large=numeric())


# loop over the data

for(n in 1:nrow(sites_loc)){
  SITENAME <- sites_loc[n,2]
  SITENO <- sites_loc[n,1]
  xy <- cbind(sites_loc[n,10:11])
  point <- raster::extract(lcm2007, xy) # get a value for the 25m squares
  small <- raster::extract(lcm2007, xy, buffer = 100, fun = getmode) # picks out the most common value in 100m^2
  large <- raster::extract(lcm2007, xy, buffer = 1000, fun = getmode) # same for 1000m^2
  out <- cbind(n,SITENAME,SITENO,point,small,large)
  landuse <- rbind(landuse,out)
  
  if(n == round(n,-1)){
    print(n)
  }
}


summary(landuse)
# in this case, suburban is the main habitat type at small-scale and arable at large-scale
# i.e. traps tend to be set near to towns and/or villages

# this takes quite a while to run so let's store this file here
write.table(landuse, file = "Checked/Landuse and climate/landuse.initial.txt", row.names = F)
landuse.saved <- read.table("Checked/Landuse and climate/landuse.initial.txt", header = T)

landuse.saved$SITENO <- as.factor(landuse.saved$SITENO)
summary(landuse.saved)

# now we want to merge this data on land use back into the main data on sites
# start by trimming the first column off as it was just record-keeping for the loop
landuse <- landuse.saved[,-1]

summary(landuse)

# merge the landuse data into the main geolocation data
sites_loc_lu <- merge(sites_loc,landuse)

# and finally, merge the locationless sites back in
sites_all_lu <- merge(sites_loc_lu, no_loc, all=T)


# Write clean file

write.csv(sites_all_lu, "Checked/Landuse and climate/sites_landuse.csv", row.names=F)
sites_landuse <- read.csv("Checked/Landuse and climate/sites_landuse.csv", header = T)
landuse_codes <- read.csv("Checked/Landuse and climate/LCM2007_classifications.csv", header = T)

# add these codes in to the sites_landuse starting with the point locations
siteswcodes <- merge(sites_landuse, landuse_codes, by.x = "point", by.y = "class.number", all = T )
names(siteswcodes)[names(siteswcodes) == 'LCM2007.class'] <- 'POINT'
siteswcodes <- merge(siteswcodes, landuse_codes, by.x = "small", by.y = "class.number", all = T)
names(siteswcodes)[names(siteswcodes) == 'LCM2007.class'] <- 'SMALL'
siteswcodes <- merge(siteswcodes, landuse_codes, by.x = "large", by.y = "class.number", all = T)
names(siteswcodes)[names(siteswcodes) == 'LCM2007.class'] <- 'LARGE'

## NEED TO REMOVE THE TRAPS NOT IN THE BRITISH ISLES
siteswcodes <- siteswcodes[(siteswcodes$LAT > 49.532822) & (siteswcodes$LON < 10),]
siteswcodesna <- na.omit(siteswcodes)


write.csv(siteswcodes, "Checked/Landuse and climate/siteswithall.csv", row.names = F)


## repeat this for the LCM1990 data

# first read in the data again
lcm1990 <- raster("../Raw/LCM1990/lcm-1990-25m_3062230/lcm1990_25m.tif")
lcm1990


# now, we want to generate a small subset of this data for every site in sites_loc (as it's based on location data, 
# we still can't quite do all sites)

# we want to extract three things for each point - the exact land use at the point,
# the small-scale main land use (100 m) and the large-scale main land use (1 km)

raster::extract(lcm1990,xy,buffer = 100,fun=getmode)

## now we are ready to extract this information for each row in sites_all

# seed an output table

landuse90 <- data.frame(n=numeric(),
                      SITENAME=factor(),
                      SITENO=numeric(),
                      point=numeric(),
                      small=numeric(),
                      large=numeric())


# loop over the data

for(n in 1:nrow(sites_loc)){
  SITENAME <- sites_loc[n,2]
  SITENO <- sites_loc[n,1]
  xy <- cbind(sites_loc[n,10:11])
  point <- raster::extract(lcm1990, xy) # get a value for the 25m squares
  small <- raster::extract(lcm1990, xy, buffer = 100, fun = getmode) # picks out the most common value in 100m^2
  large <- raster::extract(lcm1990, xy, buffer = 1000, fun = getmode) # same for 1000m^2
  out <- cbind(n,SITENAME,SITENO,point,small,large)
  landuse90 <- rbind(landuse90,out)
  
  if(n == round(n,-1)){
    print(n)
  }
}


summary(landuse90)
# in this case, suburban is the main habitat type at small-scale and arable at large-scale
# i.e. traps tend to be set near to towns and/or villages

# this takes quite a while to run so let's store this file here
write.table(landuse90, file = "Checked/Landuse and climate/landuse90.initial.txt", row.names = F)
landuse90.saved <- read.table("Checked/Landuse and climate/landuse90.initial.txt", header = T)

landuse90.saved$SITENO <- as.factor(landuse90.saved$SITENO)
summary(landuse90.saved)

# now we want to merge this data on land use back into the main data on sites
# start by trimming the first column off as it was just record-keeping for the loop
landuse90 <- landuse90.saved[,-1]

summary(landuse90)
colnames(landuse90) <- c("SITENAME","SITENO","point90","small90","large90")

# merge the landuse data into the main geolocation data
sites_loc_lu90 <- merge(sites_loc_lu,landuse90)

# and finally, merge the locationless sites back in
sites_all_lu90 <- merge(sites_loc_lu90, no_loc, all=T)


# Write clean file

write.csv(sites_all_lu90, "Checked/Landuse and climate/sites_landuse90.csv", row.names=F)
sites_landuse90 <- read.csv("Checked/Landuse and climate/sites_landuse90.csv", header = T)

### before writing this out, we want to do two things:
# (i) classify all sites (at small scale) into four land-use classes according to each LCM version
# (ii) note whether each site is consistent between 1990 and 2007 or has changed

# make lists of the relevant categories for each land-use class in each LCM
grasslandclass <- c(4,5,6,7,8,10,11)
woodlandclass <- c(1,2)
arableclass <- 3
urbanclass <- c(22,23)

grasslandclass90 <- c(5,6,7,8,9,10,11,12,13,19,25)
woodlandclass90 <- c(14,15,16)
arableclass90 <- 18
urbanclass90 <- c(20,21)


sites_landuse90$Class2007 <- as.factor(ifelse(sites_landuse90$small %in% grasslandclass, "Grassland",
                                  ifelse(sites_landuse90$small %in% woodlandclass, "Woodland",
                                         ifelse(sites_landuse90$small %in% arableclass, "Arable",
                                                ifelse(sites_landuse90$small %in% urbanclass, "Urban","Other")))))

sites_landuse90$Class1990 <- as.factor(ifelse(sites_landuse90$small90 %in% grasslandclass90, "Grassland",
                                            ifelse(sites_landuse90$small90 %in% woodlandclass90, "Woodland",
                                                   ifelse(sites_landuse90$small90 %in% arableclass90, "Arable",
                                                          ifelse(sites_landuse90$small90 %in% urbanclass90, "Urban","Other")))))



sites_landuse90$ChangeTest <- ifelse(sites_landuse90$Class2007 == sites_landuse90$Class1990, T, F)

sites_landuse90$ChangeFactor <- as.factor(paste(sites_landuse90$Class1990,sites_landuse90$Class2007, sep = "-"))



## NEED TO REMOVE THE TRAPS NOT IN THE BRITISH ISLES
sites_landuse90 <- sites_landuse90[(sites_landuse90$LAT > 49.532822) & (sites_landuse90$LON < 10),]

write.csv(sites_landuse90, "Checked/Landuse and climate/siteswithall.csv", row.names = F)



### now we repeat all this AGAIN, this time with Dudley Stamp data
# first read in the data again
dudley <- raster("../Raw/DudleyStamp/dudley25v8.tif")
dudley


# now, we want to generate a small subset of this data for every site in sites_loc (as it's based on location data, 
# we still can't quite do all sites)

# we want to extract three things for each point - the exact land use at the point,
# the small-scale main land use (100 m) and the large-scale main land use (1 km)

raster::extract(dudley,xy,buffer = 100,fun=getmode)
# this hasn't worked here because we'll need to use lat/long for this raster

## now we are ready to extract this information for each row in sites_all

# seed an output table

landuseds <- data.frame(n=numeric(),
                        SITENAME=factor(),
                        SITENO=numeric(),
                        point=numeric(),
                        small=numeric(),
                        large=numeric())


# loop over the data

for(n in 1:nrow(sites_loc)){
  SITENAME <- sites_loc[n,2]
  SITENO <- sites_loc[n,1]
  xy <- cbind(sites_loc[n,10:11])
  point <- raster::extract(dudley, xy) # get a value for the 25m squares
  small <- raster::extract(dudley, xy, buffer = 100, fun = getmode) # picks out the most common value in 100m^2
  large <- raster::extract(dudley, xy, buffer = 1000, fun = getmode) # same for 1000m^2
  out <- cbind(n,SITENAME,SITENO,point,small,large)
  landuseds <- rbind(landuseds,out)
  
  if(n == round(n,-1)){
    print(n)
  }
}


summary(landuseds)

# this takes quite a while to run so let's store this file here
write.table(landuseds, file = "Checked/Landuse and climate/landuseds.initial.txt", row.names = F)
landuseds.saved <- read.table("Checked/Landuse and climate/landuseds.initial.txt", header = T)

landuseds.saved$SITENO <- as.factor(landuseds.saved$SITENO)
summary(landuseds.saved)

# now we want to merge this data on land use back into the main data on sites
# start by trimming the first column off as it was just record-keeping for the loop
landuseds <- landuseds.saved[,-1]

summary(landuseds)
colnames(landuseds) <- c("SITENAME","SITENO","pointds","smallds","largeds")

# merge the landuse data into the main geolocation data
sites_loc_luds <- merge(sites_loc_lu90,landuseds)

# and finally, merge the locationless sites back in
sites_all_luds <- merge(sites_loc_luds, no_loc, all=T)


# Write clean file

write.csv(sites_all_luds, "Checked/Landuse and climate/sites_landuseds.csv", row.names=F)
sites_landuseds <- read.csv("Checked/Landuse and climate/sites_landuseds.csv", header = T)

### before writing this out, we want to do two things:
# (i) classify all sites (at small scale) into four land-use classes according to each LCM version
# (ii) note whether each site is consistent between 1931 and 2007 or has changed

# make lists of the relevant categories for each land-use class in DS


grasslandclassds <- 5
woodlandclassds <- 3
arableclassds <- 4
urbanclassds <- c(1,2)


sites_landuseds$Class2007 <- as.factor(ifelse(sites_landuseds$small %in% grasslandclass, "Grassland",
                                              ifelse(sites_landuseds$small %in% woodlandclass, "Woodland",
                                                     ifelse(sites_landuseds$small %in% arableclass, "Arable",
                                                            ifelse(sites_landuseds$small %in% urbanclass, "Urban","Other")))))

sites_landuseds$Class1990 <- as.factor(ifelse(sites_landuseds$small90 %in% grasslandclass90, "Grassland",
                                              ifelse(sites_landuseds$small90 %in% woodlandclass90, "Woodland",
                                                     ifelse(sites_landuseds$small90 %in% arableclass90, "Arable",
                                                            ifelse(sites_landuseds$small90 %in% urbanclass90, "Urban","Other")))))



sites_landuseds$ClassDS <- as.factor(ifelse(sites_landuseds$smallds %in% grasslandclassds, "Grassland",
                                              ifelse(sites_landuseds$smallds %in% woodlandclassds, "Woodland",
                                                     ifelse(sites_landuseds$smallds %in% arableclassds, "Arable",
                                                            ifelse(sites_landuseds$smallds %in% urbanclassds, "Urban","Other")))))



sites_landuseds$ChangeTestDS <- ifelse(sites_landuseds$Class2007 == sites_landuseds$ClassDS, T, F)
sites_landuseds$ChangeFactorDS <- as.factor(paste(sites_landuseds$ClassDS,sites_landuseds$Class2007, sep = "-"))

sites_landuseds$ChangeTest90 <- ifelse(sites_landuseds$Class2007 == sites_landuseds$Class1990, T, F)
sites_landuseds$ChangeFactor90 <- as.factor(paste(sites_landuseds$Class1990,sites_landuseds$Class2007, sep = "-"))



## NEED TO REMOVE THE TRAPS NOT IN THE BRITISH ISLES
sites_landuseds <- sites_landuseds[(sites_landuseds$LAT > 49.532822) & (sites_landuseds$LON < 10),]

write.csv(sites_landuseds, "Checked/Landuse and climate/siteswithall.csv", row.names = F)









### now we want to go through the same process for temperature and rainfall data
# at the moment we have this in raw, unprocessed form across 170 files
# so the first thing to do is to read this in, summarise it, and put it into a single useful dataframe

# I've summarised the necessary filename information to loop over this all in a .csv file

climate.ref <- read.csv("Checked/Landuse and climate/Climate data/References.csv", header = T, colClasses = "character")

# now run a loop over the 170 rainfall files to import and tidy the data

# seed an output

rainfall <- data.frame(easting = numeric(),
                       northing = numeric(),
                       year = factor(),
                       meanmonthlyrain = numeric(),
                       annualrain = numeric())


# run the loop

for (n in 1:170){
  print(n)
  file.metadata <- climate.ref[which(climate.ref$Loop_number == n), ]
  
  xcoord <- file.metadata[1,'X_cat']
  ycoord <- file.metadata[1,'Y_cat']
  
  # read in a file
  file <- read.csv(paste0("Checked/Landuse and climate/Climate data/Rainfall/ukcp09_gridded-land-obs-monthly_timeseries_rainfall_",xcoord,"_",ycoord,"_191001-201612.csv"), header = F)
  
  # wrangle that file
  row.names(file) <- file[,1]
  file <- data.frame(file[,-1, drop=FALSE])
  
  filet <- data.frame(t(file))
  
  filel <- gather(filet,month,rainfall,'X1910.01':'X2016.12')
  
  # extract the year
  filel$year <- substr(filel$month,2,5)
  
  # take average monthly and total annual rainfall for each square*year
  filecoll <- ddply(filel, .(easting,northing,year), summarise,
                    meanmonthlyrain = mean(rainfall),
                    annualrain = sum(rainfall))
  
  # write out
  rainfall <- rbind(rainfall,filecoll)
  
}

# write this out as it took a while to produce

write.csv(rainfall,"Checked/Landuse and climate/Climate data/rainfall_summarised.csv", row.names = F)

rainfall <- read.csv("Checked/Landuse and climate/Climate data/rainfall_summarised.csv", header = T)

summary(rainfall)

## now extract the temperature data, following the same procedure

temp <- data.frame(easting = numeric(),
                       northing = numeric(),
                       year = factor(),
                       meanmonthlytemp = numeric())


# run the loop

for (n in 1:170){
 print(n) 
  file.metadata <- climate.ref[which(climate.ref$Loop_number == n), ]
  
  xcoord <- file.metadata[1,'X_cat']
  ycoord <- file.metadata[1,'Y_cat']
  
  # read in a file
  file <- read.csv(paste0("Checked/Landuse and climate/Climate data/Temperature/ukcp09_gridded-land-obs-monthly_timeseries_mean-temperature_",xcoord,"_",ycoord,"_191001-201612.csv"), header = F)
  
  # wrangle that file
  row.names(file) <- file[,1]
  file <- data.frame(file[,-1, drop=FALSE])
  
  filet <- data.frame(t(file))
  
  filel <- gather(filet,month,meantemp,'X1910.01':'X2016.12')
  
  # extract the year
  filel$year <- substr(filel$month,2,5)
  
  # take average monthly and total annual rainfall for each square*year
  filecoll <- ddply(filel, .(easting,northing,year), summarise,
                    meanmonthlytemp = mean(meantemp))
  
  # write out
  temp <- rbind(temp,filecoll)
  
}

# write this out as it took a while to produce

write.csv(temp,"Checked/Landuse and climate/Climate data/temp_summarised.csv", row.names = F)

temp <- read.csv("Checked/Landuse and climate/Climate data/temp_summarised.csv", header = T)

summary(temp)


# because the two frames have been identically produced (evidenced by being the same length),
# we can just tag one onto the other

meanmonthlytemp <- temp$meanmonthlytemp

climate <- cbind(rainfall, meanmonthlytemp)


# now we need to pull out the relevant grid square for each of the sites, and extract the corresponding data

# set up a list to loop over
site.list <- levels(droplevels(siteswcodes$SITE))


# set up a function to round off eastings and northings to the same scale as used by the Met Office data
mround <- function(x,base){ 
  base*floor(x/base) + 2500 
} 

# set up an output frame
sites.climate <- data.frame(easting = numeric(),
                            northing = numeric(),
                            year = numeric(),
                            meanmonthyrain = numeric(),
                            annualrain = numeric(),
                            meanmonthlytemp = numeric(),
                            site = factor())




# run the loop

for(site in site.list){
  print(site)
  
  site.meta <- siteswcodes[which(siteswcodes$SITE == site), ]
  
  easting <- mround(site.meta$EASTING[[1]], 5000)
  northing <- mround(site.meta$NORTHING[[1]], 5000)
  
  if(!is.na(easting) & !is.na(northing)){
  site.clim <- climate[which(climate$easting == easting & climate$northing == northing), ]
  
  if(nrow(site.clim) > 0){
  
  site.clim$site <- site
  
  sites.climate <- rbind(sites.climate,site.clim)
  }  
  }
}


summary(sites.climate)

## at this point we can read in and merge the NDVI values, which we currently have for the 35 focal sites in all available years

## pull in and merge the NDVI data (which may need a spot of wrangling first...)

NDVI240 <- read.csv("Checked/Landuse and climate/Climate data/NPP/NDVI_240m_median_1983-2018.csv", header = T)

summary(NDVI240)

NDVI240 <- NDVI240[,1:38]

# make it long-form to start with

NDVI_long240 <- melt(NDVI240,
                  id.vars = c("trap","EASTING","NORTHING"),
                  variable.name = "year",
                  value.name = "NDVI240")

summary(NDVI_long240)

# fix the year variable so it's numeric
NDVI_long240$year <- as.numeric(as.character(substr(NDVI_long240$year, 2, 5)))

summary(NDVI_long240)


## repeat this with NDVI at a different resolution

NDVI30 <- read.csv("Checked/Landuse and climate/Climate data/NPP/NDVI_30m_1983-2018.csv", header = T)

summary(NDVI30)

# make it long-form to start with

NDVI_long30 <- melt(NDVI30,
                     id.vars = c("trap","EASTING","NORTHING"),
                     variable.name = "year",
                     value.name = "NDVI30")

summary(NDVI_long30)

# fix the year variable so it's numeric
NDVI_long30$year <- as.numeric(as.character(substr(NDVI_long30$year, 2, 5)))

summary(NDVI_long30)


## merge these two together
NDVI_long <- merge(NDVI_long240,NDVI_long30)


# merge this into the climate data, bearing in mind there are some missing years in the NDVI data
sites.climate$trap <- as.factor(sites.climate$site)

sites.climate.NDVI <- merge(sites.climate, NDVI_long, all.x = T)





## finally, we need to loop back over this dataframe and shuffle values for the previous and following years into extra columns

new.site.list <- levels(droplevels(as.factor(sites.climate.NDVI$site)))

sites.climate.adj <- data.frame()


for(site in new.site.list){
  print(site)
  
  site.clim <- sites.climate.NDVI[which(sites.climate.NDVI$site == site), ]
  
  for (year in 1910:2016){
    site.clim.year <- site.clim[which(site.clim$year == year), ]
    
    if (year == 1910){
      site.clim.year$prev.rain <- NA
      site.clim.year$prev.temp <- NA
      site.clim.year$prev.NDVI240 <- NA
      site.clim.year$prev.NDVI30 <- NA
    } else {
      site.clim.prev <- site.clim[which(site.clim$year == (year-1)), ]
      site.clim.year$prev.rain <- site.clim.prev$annualrain[[1]]
      site.clim.year$prev.temp <- site.clim.prev$meanmonthlytemp[[1]]
      site.clim.year$prev.NDVI240 <- site.clim.prev$NDVI240[[1]]
      site.clim.year$prev.NDVI30 <- site.clim.prev$NDVI30[[1]]
    }
    
    
    if (year == 2016){
      site.clim.year$next.rain <- NA
      site.clim.year$next.temp <- NA
      site.clim.year$next.NDVI240 <- NA
      site.clim.year$next.NDVI30 <- NA
    } else {
      site.clim.next <- site.clim[which(site.clim$year == (year+1)), ]
      site.clim.year$next.rain <- site.clim.next$annualrain[[1]]
      site.clim.year$next.temp <- site.clim.next$meanmonthlytemp[[1]]
      site.clim.year$next.NDVI240 <- site.clim.next$NDVI240[[1]]
      site.clim.year$next.NDVI30 <- site.clim.next$NDVI30[[1]]
    }
  
    sites.climate.adj <- rbind(sites.climate.adj,site.clim.year) 
    
  }
  
  
}




write.csv(sites.climate.adj, "Checked/Landuse and climate/Climate data/sites_climate.csv", row.names = F)





