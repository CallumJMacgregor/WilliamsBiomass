#############################################################
### Piecing together a tidied version of Jonny's analysis ###
#############################################################

#### AIMS ####

#1 examine the overall pattern of biomass change over time, using a segmented or non-linear model
#2 examine patterns of *annual change* in biomass over time
#3 examine the influence of baseline selection in determining perceived scale of biomass change
#4 examine patterns of biomass change in groups of traps under generic land-use types


rm(list=ls())

# before doing anything, read in some map data for later to solve problems of cross-masking packages
map1 <- ggplot2::map_data(map = "world", region = c("UK","Ireland"))


# read in packages

j <- c("tidyr","ggpubr","rstudioapi","plyr","vegan","lme4","ggplot2","RColorBrewer","car","gridExtra","MuMIn","RVAideMemoire","emmeans","reshape2","moments","dplyr","ggmap","grid","egg","segmented","scales","tseries","forecast","psych")

new.packages <- j[!(j %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(j, require, character.only = TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("CheckResidsFunction.R")

### set up the data that we need

# first, read in and wrangle the inoperative dates to get an adjusted biomass estimate for each site/year

# we have to do Barnfield (the longest-running trap) separately from the rest

# read in the inoperative days file for barnfield
inopbarn <- read.csv(paste0("../Raw/Callum Macgregor Moths Long Running Sites/Inoperative days Barnfield.csv"), header = TRUE)

#make sure R reads the 'date' column as dates:
inopbarn$Date <- as.Date(inopbarn$Date, format = '%d/%m/%Y')

# set up a list of all recorded years (across all sites) for looping over

year.list <- c(1933,1934,1935,1936,1937,1946,1947,1948,1949,1950,1964,1965,1966,1967,1968,1969,1970,
               1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,
               1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,
               2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)
year.list


# summarise the number of inoperative days per year

trapsinop <- ddply(inopbarn, .(Year,Trap), summarise,
                   Count = -sum(InopType))



# repeat for the other traps:

inopexbar <- read.csv(paste0("../Raw/Callum Macgregor Moths Long Running Sites/Inoperative days Excluding Barnfield.csv"), header = T) 

inopexbar$Date <- as.Date(inopexbar$Date, format = '%d/%m/%Y')


# set up a list of traps to loop over

trap.list <- levels(droplevels(inopexbar$Trap))
trap.list


# run the same code as above

trapinop <- ddply(inopexbar, .(Trap,Year), summarise,
                Count = -sum(InopType))


trapsinop <- rbind(trapsinop, trapinop)


write.csv(trapsinop, file = paste0("Checked/biomass analysis/all traps inoperative.csv"), row.names = FALSE)


## now we can adjust the biomass estimates in each year by the number of inoperative days
# and entirely chuck out any years where the trap was inoperative for > 4 months (i.e. 1/3 of the year)

# read back in the file we've just written (to allow shortcutting later!)
allinop <- read.csv(paste0("Checked/biomass analysis/all traps inoperative.csv"), header= T)


# function to check if a number is an integer
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

# calculate the proportion by which to adjust each year's biomass - with a zero for years with too little recording
allinop$prop <- ifelse(allinop$Count > 121, 0,
                       ifelse(is.wholenumber(allinop$Year/4),(366/(366-allinop$Count)),
                              (365/(365-allinop$Count))))

names(allinop)[names(allinop) == 'Trap'] <- 'trap'
names(allinop)[names(allinop) == 'Year'] <- 'YEAR'



# now read in the real estimated biomass data (from the bootstrapping) and make the appropriate adjustments

for (year in year.list){
  print(year)
  
  yearly <- read.csv(paste0("data transfer for chris and callum/Biomass calculations/EstBiomassyearsbootstrapped/EstBiomass",year,".csv"),header =T)
  
  newyear <- merge(yearly,allinop, by= c("YEAR", "trap"), all.x = TRUE, all.y=FALSE)
  newyear$prop[is.na(newyear$prop)] <- 1 # remove the NAs for the traps that had no inoperative days but still be able to multiply the biomass by the 'prop' which is now 1
  
  newyear$Count[is.na(newyear$Count)] <- 0
  
  newyear$inopbiomass <- newyear$EST.BIOMASS*newyear$prop
  newyear$inopSE <- newyear$EST.BIOMASS.SE*(newyear$prop^2)
  
  write.csv(newyear, file= paste0("Checked/biomass analysis/estimated biomassinop/EstBiomassinop",year,".csv"),row.names = FALSE)
  
}


## read in the final data for traps that have run for 30+ years


alltraps <- data.frame(trap = factor(),
                        YEAR = numeric(),
                        EST.BIOMASS = numeric(),
                        EST.BIOMASS.SE = numeric(),
                        LongestDuration = numeric(),
                        durationgroup = factor(),
                        Count = numeric(),
                        daysperyear = numeric(),
                        prop = numeric(),
                        inopbiomass = numeric(),
                        inopSE = numeric())



year.list <- c(1933,1934,1935,1936,1937,1946,1947,1948,1949,1950,1964,1965,1966,1967,1968,1969,1970,
               1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,
               1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,
               2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)
year.list

for (year in year.list){
  print(year)
  
  years <- read.csv(paste0("Checked/biomass analysis/estimated biomassinop/EstBiomassinop",year,".csv"),header = T)
  
  trap.thirty <- years[which(years$LongestDuration >= 30), ]
  
  
  alltraps <- rbind(alltraps,trap.thirty)
}

# update the trap list to only the long-running ones

trap.list <- levels(droplevels(alltraps$trap))
trap.list


# let's get rid of the first and last years of each trap's run, and also get rid of any other years with too many inoperative days

alltraps.end <- alltraps[0,]


for (trap in trap.list){
  trapdat <- alltraps[which(alltraps$trap == trap), ]

  for (n in 1:nrow(trapdat)){
    year <- trapdat$YEAR[[n]]
    
    trapdat$EndRun[[n]] <- ifelse(year == 2017, FALSE,
      ifelse(((year+1) %in% trapdat$YEAR) & ((year-1) %in% trapdat$YEAR), FALSE, TRUE))
    trapdat$WhichEnd[[n]] <- ifelse(trapdat$EndRun[[n]] == FALSE, "NA",
                                    ifelse(((year+1) %in% trapdat$YEAR),"Start", "End"))
  }

  alltraps.end <- rbind(alltraps.end, trapdat)
}

# pull out a list of runs for inspection
alltraps.ends <- subset(alltraps.end, EndRun == TRUE)


# and pull out the internal data for actual use
alltraps.noend <- subset(alltraps.end, EndRun == FALSE)

alltraps.no0 <- subset(alltraps.noend, inopbiomass > 0)


# get rid of the "Malham Tarn" site - although it's a 50+, it has a gap of fully 10 years
# which also bridges the candidate inflection point
alltraps.no0 <- alltraps.no0[which(alltraps.no0$trap != "Malham Tarn"), ]


# and also get rid of the earliest years of the Barnfield site, since this is the only trap running during this period
alltraps.no0 <- alltraps.no0[which(alltraps.no0$YEAR > 1966), ]


# for 1b and 1c
## now let's separate this into the two time groups - the long-running traps and the very long-running traps

# the "LongestDuration" variable is currently broken so manually create a subsetting variable -
# there are only four 50+ traps in truth

fiftyplus <- c("Alice Holt","Allotments","Barnfield Site 1","Geescroft I")

fifty <- alltraps.no0[which(alltraps.no0$trap %in% fiftyplus), ]
thirty <- alltraps.no0[which(!(alltraps.no0$trap %in% fiftyplus)), ]


# for 1d
## let's generate the same data but split by family (rather than fully-lumped)
# we have (generated elsewhere) the biomass per species
# but we need to apply the same rules as above to keep only site*year combinations that we want
# the quickest way to do this is to create a factor for siteXyear in the "alltraps.no0" dataframe and turn this into a list

alltraps.no0$siteXyear <- as.factor(paste(alltraps.no0$trap, alltraps.no0$YEAR, sep = "-"))

good.data <- levels(droplevels(alltraps.no0$siteXyear))

# now run a loop to match up everything

for (year in year.list){
  print(year)
  
  family <- read.csv(file = paste0("Checked/biomass analysis/Biomass grouped by species/species",year,".csv"), header=T)
  revalue(family$TRAPNAME, c("Brooms' Barn L" = "Brooms Barn")) -> family$TRAPNAME
  inop <- read.csv(file = paste0("Checked/biomass analysis/estimated biomassinop/EstBiomassinop",year,".csv"),header = T)
  inop$EST.BIOMASS <- NULL
  inop$EST.BIOMASS.SE <- NULL
  inop$Count <- NULL
  inop$inopbiomass <- NULL
  inop$inopSE <- NULL
  
  
  
  family.inop <- merge(family, inop, by.x = "TRAPNAME", by.y= "trap")
  family.inop$YEAR.y <- NULL
  
  names(family.inop)[names(family.inop) == 'YEAR.x'] <- 'YEAR'
  
  family.inop$inopbiomass <- family.inop$DRY_MASS*family.inop$prop
  family.inop$inopabund <- round(family.inop$countsum*family.inop$prop)
  
  
  # now remove data from outside the list of good data
  
  family.inop$siteXyear <- as.factor(paste(family.inop$TRAPNAME, family.inop$YEAR, sep = "-"))
  
  
  family.inop.good <- family.inop[which(family.inop$siteXyear %in% good.data), ]
  
  
  # remove any rows with zero biomass
  family.inop.no0 <- subset(family.inop.good, inopbiomass >0)
  
  write.csv(family.inop.no0, file = paste0("Checked/biomass analysis/biomass grouped by species inop/speciesinopbiomass",year,".csv"), row.names = F)
  
}


## now read this data back in to a single dataframe

fams <- data.frame(FAMILY = factor(),
                   YEAR = numeric(),
                   TRAPNAME = factor(),
                   LongestDuration = numeric(),
                   inopbiomass = numeric(),
                   inopabund = numeric())


yearlist64 <- c(1964, 1965,1966,1967,1968,1969,1970,
                1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,
                1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,
                2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)

for (year in yearlist64){ 
  print(year)
  
  years <- read.csv(paste0("Checked/biomass analysis/biomass grouped by species inop/speciesinopbiomass",year,".csv"),header = T)
  
  
  compressed <- ddply(years, .(FAMILY, YEAR, TRAPNAME, LongestDuration), summarise,
                      inopbiomass = sum(inopbiomass), inopabund = sum(inopabund)) # here we are abandoning the standard error
  
  
  fams <- rbind(compressed,fams)
}

summary(fams)

# pick out the most biomass-y families, deciding how many to use and what proportion of total biomass they collectively represent

fams.sum <- ddply(fams, .(FAMILY), summarise,
                  totalbiomass = sum(inopbiomass))

allbiomass <- sum(fams.sum$totalbiomass)

fams.sum$perc <- (fams.sum$totalbiomass*100)/allbiomass

fams.sum

# let's do the top 3 families: Noctuidae, Geometridae and Erebidae
# which collectively represent 93.33 % of all trapped biomass

fams.noct <- fams[which(fams$FAMILY == "Noctuidae"), ]
fams.geom <- fams[which(fams$FAMILY == "Geometridae"), ]
fams.ereb <- fams[which(fams$FAMILY == "Erebidae"), ]



# for 1e-f
## we now need to introduce land-use data for each trap

# read in this data which has been prepared elsewhere, and cut out the few columns we actually need
landuse <- read.csv("Checked/siteswithall.csv", header = TRUE)

summary(landuse)

landuse.trim <- landuse[,c('large','small','SITE','EASTING','NORTHING','HEast','HNorth','LAT','LON')]

colnames(landuse.trim) <- c("large","small","trap","EASTING","NORTHING","HEast","HNorth","LAT","LON")

# now merge this land-use data into the main dataset
all.landuse <- merge(landuse.trim, alltraps.no0)


## now we use those landuse categories to pick out traps in four main landuse types that collectively represent most sites in the dataset:
# arable, grassland, woodland and urban

grasslandclass <- c(4,5,6,7,8,10,11)
woodlandclass <- c(1,2)
arableclass <- 3
urbanclass <- c(22,23)

# we have these classes assigned at both small and large scales, 
# which represent the main landuses in circles of 100m and 1000m radius respectively around the trap

# so we need to split off a total of 8 dataframes

grass.small <- all.landuse[which(all.landuse$small %in% grasslandclass), ]
grass.large <- all.landuse[which(all.landuse$large %in% grasslandclass), ]

wood.small <- all.landuse[which(all.landuse$small %in% woodlandclass), ]
wood.large <- all.landuse[which(all.landuse$large %in% woodlandclass), ]

arable.small <- all.landuse[which(all.landuse$small %in% arableclass), ]
arable.large <- all.landuse[which(all.landuse$large %in% arableclass), ]

urban.small <- all.landuse[which(all.landuse$small %in% urbanclass), ]
urban.large <- all.landuse[which(all.landuse$large %in% urbanclass), ]



#### now do the analyses and accompanying figure panels for Fig 1 - raw biomass


### Fig 1a - biomass from all sites with segmented regression

# first and foremost, use the full data set to fit a linear regression (actually a GLMM)
# and an accompanying segmented regression to the data, and test which fits better using BIC (Bayesian Information Criterion)

# we're going to do these analyses with the 'segmented' package
# this fits a linear model to the data, then allows it to bend at different points 
# (i.e. to have a different slope before and after a break point)
# you specify a break point for it to start at and it works outwards from there to find the most parsimonious model
# we can specify this break point as a solid year (i.e. 1980)

# this method will basically force a segmented fit onto the data, but that doesn't mean it's true
# so we'll use BIC (Bayesian Information Criterion)
# to decide whether it provides a better fit than the simple linear model in each case,
# and carry the best one forwards.
# I've specifically chosen BIC as it penalises the model more harshly than either AIC or adjusted R^2 for adding terms
# so using BIC gives us the most certainty that the segmented model is definitely better than the linear one

# first, fit the regular linear model, trying a few different error distributions
hist(alltraps.no0$inopbiomass)
hist(log(alltraps.no0$inopbiomass))

# Poisson
model1aP <- glm(round(inopbiomass) ~ YEAR,
                  family = poisson (link = "log"),
                  data = alltraps.no0) 

summary(model1aP)

chkres(model1aP, alltraps.no0$YEAR, alltraps.no0$trap)
#
#
#
#

# log-normal
model1aG <- lm(log(inopbiomass) ~ YEAR,
                  data = alltraps.no0) 

summary(model1aG)

chkres(model1aG, alltraps.no0$YEAR, alltraps.no0$trap)
#
#
#
#

# these are a lot better than the Poisson


# negative binomial
model1aNB <- glm.nb(round(inopbiomass) ~ YEAR,
                    link = "log",
                  data = alltraps.no0) 

summary(model1aNB)

chkres(model1aNB, alltraps.no0$YEAR, alltraps.no0$trap)

# after some debate we've agreed that the log-normal structure equates to the geometric means,
# which are probably more theoretically appropriate as they reduce the influence of extreme years
# so let's carry forward the log-normal structure throughout for uniformity

summary(model1aG)
drop1(model1aG, test = "F")
BIC(model1aG)

# now fit the segmented model, setting 1979 as the start point to search for the break point

model1a.seg <- segmented(model1aG, seg.Z = ~YEAR, psi = 1979)

summary(model1a.seg)
BIC(model1a.seg)
anova(model1a.seg, model1aG)

chkres(model1a.seg, alltraps.no0$trap, alltraps.no0$YEAR)

## finally, construct a figure


newdataall <- expand.grid(YEAR = alltraps.no0$YEAR, trap = levels(droplevels(alltraps.no0$trap)), inopbiomass = 0)
newdataall$inopbiomass <- exp(predict(model1a.seg, newdata = newdataall, type="response"))
newdataall <- ddply(newdataall, .(YEAR), numcolwise(mean))


gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

averageall <- ddply(alltraps.no0,.(YEAR), summarise,
                    inopbiomass = gm_mean(inopbiomass))# TO ADD THE AVERAGE BIOMASS LINE


fig1a <- ggplot(alltraps.no0, aes(x = YEAR, y = inopbiomass))+
  geom_line(aes(group = trap),
            colour = "grey70") +
  geom_line(data=newdataall, aes(x=YEAR, y=inopbiomass)) +
  geom_line(data = averageall, aes(x= YEAR, y = inopbiomass), size= 0.75)+
  scale_y_log10(breaks = c(1000,10000,100000, 1000000), labels = comma, limits = c(200,1200000)) +
  labs(y = "Total annual \nbiomass (mg)" , x = "Year") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")

fig1a


ggsave("Checked/Figures/Panels/Fig1a.svg", plot = fig1a, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)



### supplementary analyses

## site-by-site plots

# we want to do effectively this analysis for every site and stitch it all together into a panel plot
# first refresh the trap list


traps.years <- alltraps.no0[,c('trap','YEAR')]
traps.starts <- ddply(traps.years, .(trap), summarise,
                      start = min(YEAR))

traps.starts <- traps.starts[order(traps.starts$start), ]


trap.list <- levels(droplevels(traps.starts$trap))


trap.stats <- data.frame()
trap.plots <- NULL

for (trap in trap.list){
  print(trap)
  
  i <- match(trap, trap.list)
  
  trapdat <- alltraps.no0[which(alltraps.no0$trap == trap), ]
  
  traplin <- lm(log(inopbiomass) ~ YEAR,
                data = trapdat) 
  
  BIC_traplin <- BIC(traplin)
  
  
  trapseg <- try(segmented(traplin, seg.Z = ~YEAR, psi = 1990))
  
  trap.plots[[i]] <- local({
    i <- i
    
  if("try-error" %in% class(trapseg)){
    trap.early <- nrow(trapdat[which(trapdat$YEAR <= 1982), ])
    trap.late <- nrow(trapdat[which(trapdat$YEAR > 1982), ])
    
    
    gtrap <- ggplot(trapdat, aes(x = YEAR, y = inopbiomass))+
      geom_vline(aes(xintercept = 1982.079),
                 linetype = "dashed",
                 colour = "grey50")+
      geom_line(size= 0.75)+
      scale_y_log10(breaks = c(1000,10000,100000, 1000000), labels = comma, limits = c(200,1200000)) +
      labs(y = "Total annual \nbiomass (mg)" , x = "Year") +
      theme_classic()+
      xlim(c(1967,2017))+
      annotate(geom = "text", x = 1974, y = 500, size = 8, 
               label = paste0("n = ",trap.early))+
      annotate(geom = "text", x = 1990, y = 500, size = 8, 
               label = paste0("n = ",trap.late))+
      annotate(geom = "text", x = 2000, y = 500000, size = 7,
               label = paste0("BIC (linear) = ", round(BIC_traplin, 3)))+
      annotate(geom = "text", x = 2000, y = 1000000, size = 7,
               label = paste0("BIC (segmented) = NA"))+
      theme(axis.text=element_text(size=15),
            axis.title=element_text(size=15),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
      ggtitle(trap)
    
    newdatatrap <- expand.grid(YEAR = trapdat$YEAR, inopbiomass = 0)
    newdatatrap$inopbiomass <- exp(predict(traplin, newdata = newdatatrap, type="response"))
      
    gtrap <- gtrap + geom_line(data=newdatatrap, aes(x=YEAR, y=inopbiomass))
    
    
    ggsave(paste0("Checked/Figures/IndividualSites/",trap,".png"), plot = gtrap, device = 'png',
           width = 240, height = 160, units = "mm", limitsize = T)
    
    
    es1.est <- summary(traplin)$coefficients[2,1]
    es1.se <- summary(traplin)$coefficients[2,2]
    break.est <- NA
    break.se <- NA
    es2.est <- NA
    es2.se <- NA
    
    
  } else {

  
  
  
  
  BIC_trapseg <- BIC(trapseg)
  
  
  trap.early <- nrow(trapdat[which(trapdat$YEAR <= 1982), ])
  trap.late <- nrow(trapdat[which(trapdat$YEAR > 1982), ])
  
  


  
  if (BIC_traplin < BIC_trapseg){
    newdatatrap <- expand.grid(YEAR = trapdat$YEAR, inopbiomass = 0)
    newdatatrap$inopbiomass <- exp(predict(traplin, newdata = newdatatrap, type="response"))
    
    gtrap <- ggplot(trapdat, aes(x = YEAR, y = inopbiomass))+
      geom_vline(aes(xintercept = 1982.079),
                 linetype = "dashed",
                 colour = "grey50")+
      geom_line(size= 0.75)+
      scale_y_log10(breaks = c(1000,10000,100000, 1000000), labels = comma, limits = c(200,1200000)) +
      labs(y = "Total annual \nbiomass (mg)" , x = "Year") +
      theme_classic()+
      xlim(c(1967,2017))+
      annotate(geom = "text", x = 1974, y = 500, size = 8, 
               label = paste0("n = ",trap.early))+
      annotate(geom = "text", x = 1990, y = 500, size = 8, 
               label = paste0("n = ",trap.late))+
      annotate(geom = "text", x = 2000, y = 500000, size = 7,
               label = paste0("BIC (linear) = ", round(BIC_traplin, 1)))+
      annotate(geom = "text", x = 2000, y = 1000000, size = 7,
               label = paste0("BIC (segmented) = ", round(BIC_trapseg, 1)))+
      theme(axis.text=element_text(size=15),
            axis.title=element_text(size=15),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
      ggtitle(trap)+
      geom_line(data=newdatatrap, aes(x=YEAR, y=inopbiomass))
    
    es1.est <- summary(traplin)$coefficients[2,1]
    es1.se <- summary(traplin)$coefficients[2,2]
    break.est <- NA
    break.se <- NA
    es2.est <- NA
    es2.se <- NA
    
  }
  
  if (BIC_traplin > BIC_trapseg){
    newdatatrap <- expand.grid(YEAR = trapdat$YEAR, inopbiomass = 0)
    newdatatrap$inopbiomass <- exp(predict(trapseg, newdata = newdatatrap, type="response"))
    

    break.est <- summary(trapseg)$psi[2]
    break.se <- summary(trapseg)$psi[3]
    trap.LCI <- break.est - (1.96*break.se)
    trap.UCI <- break.est + (1.96*break.se)
    
    gtrap <- ggplot(trapdat, aes(x = YEAR, y = inopbiomass))+
      geom_vline(aes(xintercept = 1982.079),
                 linetype = "dashed",
                 colour = "grey50")+
      geom_line(size= 0.75)+
      scale_y_log10(breaks = c(1000,10000,100000, 1000000), labels = comma, limits = c(200,1200000)) +
      labs(y = "Total annual \nbiomass (mg)" , x = "Year") +
      theme_classic()+
      xlim(c(1967,2017))+
      annotate(geom = "text", x = 1974, y = 500, size = 8, 
               label = paste0("n = ",trap.early))+
      annotate(geom = "text", x = 1990, y = 500, size = 8, 
               label = paste0("n = ",trap.late))+
      annotate(geom = "text", x = 2000, y = 500000, size = 7,
               label = paste0("BIC (linear) = ", round(BIC_traplin, 1)))+
      annotate(geom = "text", x = 2000, y = 1000000, size = 7,
               label = paste0("BIC (segmented) = ", round(BIC_trapseg, 1)))+
      theme(axis.text=element_text(size=15),
            axis.title=element_text(size=15),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
      ggtitle(trap)+ 
      geom_point(aes(x = break.est, y = 2000))+
      geom_errorbarh(aes(xmin = trap.LCI, xmax = trap.UCI, y = 2000), height = 0.25)+ 
      geom_line(data=newdatatrap, aes(x=YEAR, y=inopbiomass))
    
    es1.est <- summary(trapseg)$coefficients[2,1]
    es1.se <- summary(trapseg)$coefficients[2,2]
    es2.est <- summary(trapseg)$coefficients[3,1]
    es2.se <- summary(trapseg)$coefficients[3,2]
    
  
  }
  
    ggsave(paste0("Checked/Figures/IndividualSites/",trap,".png"), plot = gtrap, device = 'png',
           width = 240, height = 160, units = "mm", limitsize = T)
  }
  
  
    out <- cbind(trap,BIC_traplin,BIC_trapseg,es1.est,es1.se,break.est,break.se,es2.est,es2.se)
    trap.stats <- rbind(trap.stats, out)
    print(gtrap)  
    })
  
  
  
}



## now we want to turn the list of trap plots into a multiplot

fig.traps <- grid.arrange(grobs = trap.plots, nrow = 9)

ggsave("Checked/Figures/Fig_traps.svg", plot = fig.traps, device = svg,
       width = 700, height = 1200, units = "mm", limitsize = T)

## we have to repeat the same loop with slight tweaks to actually extract the trap stats

trap.stats <- data.frame()

for (trap in trap.list){
  print(trap)
  
  i <- match(trap, trap.list)
  
  trapdat <- alltraps.no0[which(alltraps.no0$trap == trap), ]
  
  traplin <- lm(log(inopbiomass) ~ YEAR,
                data = trapdat) 
  
  BIC_traplin <- BIC(traplin)
  
  
  trapseg <- try(segmented(traplin, seg.Z = ~YEAR, psi = 1990))

    
    if("try-error" %in% class(trapseg)){
      trap.early <- nrow(trapdat[which(trapdat$YEAR <= 1982), ])
      trap.late <- nrow(trapdat[which(trapdat$YEAR > 1982), ])
      
      
      gtrap <- ggplot(trapdat, aes(x = YEAR, y = inopbiomass))+
        geom_vline(aes(xintercept = 1982.079),
                   linetype = "dashed",
                   colour = "grey50")+
        geom_line(size= 0.75)+
        scale_y_log10(breaks = c(1000,10000,100000, 1000000), labels = comma, limits = c(200,1200000)) +
        labs(y = "Total annual \nbiomass (mg)" , x = "Year") +
        theme_classic()+
        xlim(c(1967,2017))+
        annotate(geom = "text", x = 1974, y = 500, size = 8, 
                 label = paste0("n = ",trap.early))+
        annotate(geom = "text", x = 1990, y = 500, size = 8, 
                 label = paste0("n = ",trap.late))+
        annotate(geom = "text", x = 2000, y = 500000, size = 7,
                 label = paste0("BIC (linear) = ", round(BIC_traplin, 3)))+
        annotate(geom = "text", x = 2000, y = 1000000, size = 7,
                 label = paste0("BIC (segmented) = NA"))+
        theme(axis.text=element_text(size=15),
              axis.title=element_text(size=15),
              legend.position = "none",
              plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
        ggtitle(trap)
      
      newdatatrap <- expand.grid(YEAR = trapdat$YEAR, inopbiomass = 0)
      newdatatrap$inopbiomass <- exp(predict(traplin, newdata = newdatatrap, type="response"))
      
      gtrap <- gtrap + geom_line(data=newdatatrap, aes(x=YEAR, y=inopbiomass))
      
      
      ggsave(paste0("Checked/Figures/IndividualSites/",trap,".png"), plot = gtrap, device = 'png',
             width = 240, height = 160, units = "mm", limitsize = T)
      
      
      es1.est <- summary(traplin)$coefficients[2,1]
      es1.se <- summary(traplin)$coefficients[2,2]
      break.est <- NA
      break.se <- NA
      es2.est <- NA
      es2.se <- NA
      
      
    } else {
      
      
      
      
      
      BIC_trapseg <- BIC(trapseg)
      
      
      trap.early <- nrow(trapdat[which(trapdat$YEAR <= 1982), ])
      trap.late <- nrow(trapdat[which(trapdat$YEAR > 1982), ])
      
      
      
      
      
      if (BIC_traplin < BIC_trapseg){
        newdatatrap <- expand.grid(YEAR = trapdat$YEAR, inopbiomass = 0)
        newdatatrap$inopbiomass <- exp(predict(traplin, newdata = newdatatrap, type="response"))
        
        gtrap <- ggplot(trapdat, aes(x = YEAR, y = inopbiomass))+
          geom_vline(aes(xintercept = 1982.079),
                     linetype = "dashed",
                     colour = "grey50")+
          geom_line(size= 0.75)+
          scale_y_log10(breaks = c(1000,10000,100000, 1000000), labels = comma, limits = c(200,1200000)) +
          labs(y = "Total annual \nbiomass (mg)" , x = "Year") +
          theme_classic()+
          xlim(c(1967,2017))+
          annotate(geom = "text", x = 1974, y = 500, size = 8, 
                   label = paste0("n = ",trap.early))+
          annotate(geom = "text", x = 1990, y = 500, size = 8, 
                   label = paste0("n = ",trap.late))+
          annotate(geom = "text", x = 2000, y = 500000, size = 7,
                   label = paste0("BIC (linear) = ", round(BIC_traplin, 1)))+
          annotate(geom = "text", x = 2000, y = 1000000, size = 7,
                   label = paste0("BIC (segmented) = ", round(BIC_trapseg, 1)))+
          theme(axis.text=element_text(size=15),
                axis.title=element_text(size=15),
                legend.position = "none",
                plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
          ggtitle(trap)+
          geom_line(data=newdatatrap, aes(x=YEAR, y=inopbiomass))
        
        es1.est <- summary(traplin)$coefficients[2,1]
        es1.se <- summary(traplin)$coefficients[2,2]
        break.est <- NA
        break.se <- NA
        es2.est <- NA
        es2.se <- NA
        
      }
      
      if (BIC_traplin > BIC_trapseg){
        newdatatrap <- expand.grid(YEAR = trapdat$YEAR, inopbiomass = 0)
        newdatatrap$inopbiomass <- exp(predict(trapseg, newdata = newdatatrap, type="response"))
        
        
        break.est <- summary(trapseg)$psi[2]
        break.se <- summary(trapseg)$psi[3]
        trap.LCI <- break.est - (1.96*break.se)
        trap.UCI <- break.est + (1.96*break.se)
        
        gtrap <- ggplot(trapdat, aes(x = YEAR, y = inopbiomass))+
          geom_vline(aes(xintercept = 1982.079),
                     linetype = "dashed",
                     colour = "grey50")+
          geom_line(size= 0.75)+
          scale_y_log10(breaks = c(1000,10000,100000, 1000000), labels = comma, limits = c(200,1200000)) +
          labs(y = "Total annual \nbiomass (mg)" , x = "Year") +
          theme_classic()+
          xlim(c(1967,2017))+
          annotate(geom = "text", x = 1974, y = 500, size = 8, 
                   label = paste0("n = ",trap.early))+
          annotate(geom = "text", x = 1990, y = 500, size = 8, 
                   label = paste0("n = ",trap.late))+
          annotate(geom = "text", x = 2000, y = 500000, size = 7,
                   label = paste0("BIC (linear) = ", round(BIC_traplin, 1)))+
          annotate(geom = "text", x = 2000, y = 1000000, size = 7,
                   label = paste0("BIC (segmented) = ", round(BIC_trapseg, 1)))+
          theme(axis.text=element_text(size=15),
                axis.title=element_text(size=15),
                legend.position = "none",
                plot.title = element_text(hjust = 0.5, size = 25, face = "bold"))+
          ggtitle(trap)+ 
          geom_point(aes(x = break.est, y = 2000))+
          geom_errorbarh(aes(xmin = trap.LCI, xmax = trap.UCI, y = 2000), height = 0.25)+ 
          geom_line(data=newdatatrap, aes(x=YEAR, y=inopbiomass))
        
        es1.est <- summary(trapseg)$coefficients[2,1]
        es1.se <- summary(trapseg)$coefficients[2,2]
        es2.est <- summary(trapseg)$coefficients[3,1]
        es2.se <- summary(trapseg)$coefficients[3,2]
        
        
      }
      
      ggsave(paste0("Checked/Figures/IndividualSites/",trap,".png"), plot = gtrap, device = 'png',
             width = 240, height = 160, units = "mm", limitsize = T)
    }
    
    
    out <- cbind(trap,BIC_traplin,BIC_trapseg,es1.est,es1.se,break.est,break.se,es2.est,es2.se)
    trap.stats <- rbind(trap.stats, out)
   
  
  
  
}

## split years

# we also want to split the data here according to the universal break point
# and independently test the slopes using a mixed-effects model

all.early <- alltraps.no0[which(alltraps.no0$YEAR <= 1982), ]
all.late <- alltraps.no0[which(alltraps.no0$YEAR > 1982), ]


## early

# log-normal

hist(log(all.early$inopbiomass))

model1spleG <- lmer(log(inopbiomass) ~ YEAR + (1|trap),
                    data = all.early) 


summary(model1spleG)

chkres(model1spleG, all.early$YEAR, all.early$trap)

# these are similar to the log-normal here, but in later analyses the negative binomial structure performs really poorly
# so let's carry forward the log-normal structure throughout for uniformity

drop1(model1spleG, test = "Chi")
BIC(model1spleG)


## late

# log-normal

hist(log(all.late$inopbiomass))

model1spllG <- lmer(log(inopbiomass) ~ YEAR + (1|trap),
                    data = all.late) 


summary(model1spllG)

chkres(model1spllG, all.late$YEAR, all.late$trap)

# these are similar to the log-normal here, but in later analyses the negative binomial structure performs really poorly
# so let's carry forward the log-normal structure throughout for uniformity

drop1(model1spllG, test = "Chi")
BIC(model1spllG)



### Fig 1b - biomass from 50+ year sites with segmented regression

# now we repeat the above, using only data from the most long-running traps


# first, fit the regular linear model, trying a few different error distributions
hist(fifty$inopbiomass)
hist(log(fifty$inopbiomass))

# log-normal
model1bG <- lm(log(inopbiomass) ~ YEAR,
               data = fifty) 

summary(model1bG)

chkres(model1bG, fifty$trap, fifty$YEAR)
#
#
#
#


drop1(model1bG, test = "F")
BIC(model1bG)
r.squaredGLMM(model1bG)

# now fit the segmented model, setting 1979 as the start point to search for the break point

model1b.seg <- segmented(model1bG, seg.Z = ~YEAR, psi = 1979)

summary(model1b.seg)
BIC(model1b.seg)
anova(model1b.seg, model1bG)

chkres(model1b.seg, fifty$trap, fifty$YEAR)

## finally, construct a figure


newdata50 <- expand.grid(YEAR = fifty$YEAR, trap = levels(droplevels(fifty$trap)), inopbiomass = 0)
newdata50$inopbiomass <- exp(predict(model1b.seg, newdata = newdata50, type="response"))
newdata50 <- ddply(newdata50, .(YEAR), numcolwise(mean))

average50 <- ddply(fifty,.(YEAR), summarise,
                    inopbiomass = gm_mean(inopbiomass))# TO ADD THE AVERAGE BIOMASS LINE


fig1b <- ggplot(fifty, aes(x = YEAR, y = inopbiomass))+
  geom_line(aes(group = trap),
            colour = "grey70") +
  geom_line(data=newdata50, aes(x=YEAR, y=inopbiomass)) +
  geom_line(data = average50, aes(x= YEAR, y = inopbiomass), size= 0.75)+
  scale_y_log10(breaks = c(1000,10000,100000, 1000000), labels = comma, limits = c(200,1200000)) +
  labs(y = "Total annual \nbiomass (mg)" , x = "Year") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")

fig1b


ggsave("Checked/Figures/Panels/Fig1b.svg", plot = fig1b, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)






### Fig 1c - biomass from 30+ year sites with segmented regression


# now we repeat the above, using only data from the slightly less long-running traps


# first, fit the regular linear model, trying a few different error distributions
hist(thirty$inopbiomass)
hist(log(thirty$inopbiomass))


# log-normal
model1cG <- lm(log(inopbiomass) ~ YEAR,
               data = thirty) 

summary(model1cG)

chkres(model1cG, thirty$trap, thirty$YEAR)
#
#
#
#

drop1(model1cG, test = "F")
BIC(model1cG)

# now fit the segmented model, setting 1979 as the start point to search for the break point

model1c.seg <- segmented(model1cG, seg.Z = ~YEAR, psi = 1979)

summary(model1c.seg)
BIC(model1c.seg)
anova(model1c.seg, model1cG)

chkres(model1c.seg, thirty$trap, thirty$YEAR)

## finally, construct a figure


newdata30 <- expand.grid(YEAR = thirty$YEAR, trap = levels(droplevels(thirty$trap)), inopbiomass = 0)
newdata30$inopbiomass <- exp(predict(model1c.seg, newdata = newdata30, type="response"))
newdata30 <- ddply(newdata30, .(YEAR), numcolwise(mean))

average30 <- ddply(thirty,.(YEAR), summarise,
                   inopbiomass = gm_mean(inopbiomass))# TO ADD THE AVERAGE BIOMASS LINE


fig1c <- ggplot(thirty, aes(x = YEAR, y = inopbiomass))+
  geom_line(aes(group = trap),
            colour = "grey70") +
  geom_line(data=newdata30, aes(x=YEAR, y=inopbiomass)) +
  geom_line(data = average30, aes(x= YEAR, y = inopbiomass), size= 0.75)+
  scale_y_log10(breaks = c(1000,10000,100000, 1000000), labels = comma, limits = c(200,1200000)) +
  labs(y = "Total annual \nbiomass (mg)" , x = "Year") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")

fig1c

ggsave("Checked/Figures/Panels/Fig1c.svg", plot = fig1c, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)




### Fig 1d - biomass from top 5(?) familes with segmented regression

# the next step is to repeat these analyses using only data from the three main families, which comprise > 90% of biomass
# and construct a single figure

## analyses

## Noctuidae

# first, fit the regular linear model, trying a few different error distributions
hist(fams.noct$inopbiomass)
hist(log(fams.noct$inopbiomass))



# log-normal
model1dnG <- lm(log(inopbiomass) ~ YEAR,
               data = fams.noct) 

summary(model1dnG)

chkres(model1dnG, fams.noct$TRAPNAME, fams.noct$YEAR)
#
#
#
#



drop1(model1dnG, test = "F")
BIC(model1dnG)

# now fit the segmented model, setting 1979 as the start point to search for the break point

model1dn.seg <- segmented(model1dnG, seg.Z = ~YEAR, psi = 1979)

summary(model1dn.seg)
BIC(model1dn.seg)

anova(model1dn.seg, model1dnG)

chkres(model1dn.seg, fams.noct$TRAPNAME, fams.noct$YEAR)

# set up the data for a figure

newdatadn <- expand.grid(YEAR = fams.noct$YEAR, TRAPNAME = levels(droplevels(fams.noct$TRAPNAME)), inopbiomass = 0)
newdatadn$inopbiomass <- exp(predict(model1dn.seg, newdata = newdatadn, type="response"))
newdatadn <- ddply(newdatadn, .(YEAR), numcolwise(mean))

averagedn <- ddply(fams.noct,.(YEAR), summarise,
                   inopbiomass = gm_mean(inopbiomass))# TO ADD THE AVERAGE BIOMASS LINE



# repeat these analyses for Geometridae and Erebidae

## Geometridae

# first, fit the regular linear model, trying a few different error distributions
hist(fams.geom$inopbiomass)
hist(log(fams.geom$inopbiomass))


# log-normal
model1dgG <- lm(log(inopbiomass) ~ YEAR,
                data = fams.geom) 

summary(model1dgG)

chkres(model1dgG, fams.geom$TRAPNAME, fams.geom$YEAR)
#
#
#
#

drop1(model1dgG, test = "F")
BIC(model1dgG)

# now fit the segmented model, setting 1979 as the start point to search for the break point

model1dg.seg <- segmented(model1dgG, seg.Z = ~YEAR, psi = 1979)

summary(model1dg.seg)
BIC(model1dg.seg)

anova(model1dg.seg, model1dgG)

chkres(model1dg.seg, fams.geom$TRAPNAME, fams.geom$YEAR)

# set up the data for a figure

newdatadg <- expand.grid(YEAR = fams.geom$YEAR, TRAPNAME = levels(droplevels(fams.geom$TRAPNAME)), inopbiomass = 0)
newdatadg$inopbiomass <- exp(predict(model1dg.seg, newdata = newdatadg, type="response"))
newdatadg <- ddply(newdatadg, .(YEAR), numcolwise(mean))

averagedg <- ddply(fams.geom,.(YEAR), summarise,
                   inopbiomass = gm_mean(inopbiomass))# TO ADD THE AVERAGE BIOMASS LINE


## Erebidae

# first, fit the regular linear model, trying a few different error distributions
hist(fams.ereb$inopbiomass)
hist(log(fams.ereb$inopbiomass))


# log-normal
model1deG <- lm(log(inopbiomass) ~ YEAR,
                data = fams.ereb) 

summary(model1deG)

chkres(model1deG, fams.ereb$TRAPNAME, fams.ereb$YEAR)
#
#
#
#


drop1(model1deG, test = "F")
BIC(model1deG)

# now fit the segmented model, setting 1979 as the start point to search for the break point

model1de.seg <- segmented(model1deG, seg.Z = ~YEAR, psi = 1979)

summary(model1de.seg)
BIC(model1de.seg)

anova(model1de.seg, model1deG)

chkres(model1de.seg, fams.ereb$TRAPNAME, fams.ereb$YEAR)

# set up the data for a figure

newdatade <- expand.grid(YEAR = fams.ereb$YEAR, TRAPNAME = levels(droplevels(fams.ereb$TRAPNAME)), inopbiomass = 0)
newdatade$inopbiomass <- exp(predict(model1de.seg, newdata = newdatade, type="response"))
newdatade <- ddply(newdatade, .(YEAR), numcolwise(mean))

averagede <- ddply(fams.ereb,.(YEAR), summarise,
                   inopbiomass = gm_mean(inopbiomass))# TO ADD THE AVERAGE BIOMASS LINE


# group all these extra data sets together
newdatadn$Family <- "Noctuidae"
newdatadg$Family <- "Geometridae"
newdatade$Family <- "Erebidae"

averagedn$Family <- "Noctuidae"
averagedg$Family <- "Geometridae"
averagede$Family <- "Erebidae"


newdatad <- rbind(newdatadn, newdatadg, newdatade)
averaged <- rbind(averagedn, averagedg, averagede)


## finally, construct a figure


fig1d <- ggplot()+
  geom_line(data=newdatad, aes(x=YEAR, y=inopbiomass, colour=Family), size = 1) +
  geom_line(data = averaged, aes(x= YEAR, y = inopbiomass, colour=Family), alpha= 0.75)+
  scale_colour_manual(values = c("blueviolet","darkolivegreen3","darkslategrey"))+
  scale_y_log10(breaks = c(1000,10000,100000,1000000), labels = comma, limits = c(200,1200000)) +
  labs(y = "Mean annual biomass \nper trap (mg)" , x = "Year") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=17))
fig1d

ggsave("Checked/Figures/Panels/Fig1d.svg", plot = fig1d, device = svg,
       width = 270, height = 160, units = "mm", limitsize = T)




### Fig 1e - biomass from broad land-uses with segmented regression

# our next step is to do the same thing again, but with data split into landuse categories
# we need to do this at both small- and large-scales

### small

## arable

# first, fit the regular linear model, trying a few different error distributions
hist(arable.small$inopbiomass)
hist(log(arable.small$inopbiomass))


# log-normal
model1esaG <- lm(log(inopbiomass) ~ YEAR,
                data = arable.small) 

summary(model1esaG)

chkres(model1esaG, arable.small$trap, arable.small$YEAR)
#
#
#
#


drop1(model1esaG, test = "F")
BIC(model1esaG)

# now fit the segmented model, setting 1979 as the start point to search for the break point

model1esa.seg <- segmented(model1esaG, seg.Z = ~YEAR, psi = 1979)

summary(model1esa.seg)
BIC(model1esa.seg)

anova(model1esa.seg, model1esaG)

chkres(model1esa.seg, arable.small$trap, arable.small$YEAR)

# set up the data for a figure

newdataesa <- expand.grid(YEAR = arable.small$YEAR, trap = levels(droplevels(arable.small$trap)), inopbiomass = 0)
newdataesa$inopbiomass <- exp(predict(model1esa.seg, newdata = newdataesa, type="response"))
newdataesa <- ddply(newdataesa, .(YEAR), numcolwise(mean))

averageesa <- ddply(arable.small,.(YEAR), summarise,
                   inopbiomass = gm_mean(inopbiomass))# TO ADD THE AVERAGE BIOMASS LINE



## woodland

# first, fit the regular linear model, trying a few different error distributions
hist(wood.small$inopbiomass)
hist(log(wood.small$inopbiomass))


# log-normal
model1eswG <- lm(log(inopbiomass) ~ YEAR,
                 data = wood.small) 

summary(model1eswG)

chkres(model1eswG, wood.small$trap, wood.small$YEAR)
#
#
#
#


drop1(model1eswG, test = "F")
BIC(model1eswG)

# now fit the segmented model, setting 1979 as the start point to search for the break point

model1esw.seg <- segmented(model1eswG, seg.Z = ~YEAR, psi = 1979)

summary(model1esw.seg)
BIC(model1esw.seg)

anova(model1esw.seg, model1eswG)

chkres(model1esw.seg, wood.small$trap, wood.small$YEAR)

# set up the data for a figure

newdataesw <- expand.grid(YEAR = wood.small$YEAR, trap = levels(droplevels(wood.small$trap)), inopbiomass = 0)
newdataesw$inopbiomass <- exp(predict(model1esw.seg, newdata = newdataesw, type="response"))
newdataesw <- ddply(newdataesw, .(YEAR), numcolwise(mean))

averageesw <- ddply(wood.small,.(YEAR), summarise,
                    inopbiomass = gm_mean(inopbiomass))# TO ADD THE AVERAGE BIOMASS LINE



## grassland

# first, fit the regular linear model, trying a few different error distributions
hist(grass.small$inopbiomass)
hist(log(grass.small$inopbiomass))


# log-normal
model1esgG <- lm(log(inopbiomass) ~ YEAR,
                 data = grass.small) 

summary(model1esgG)

chkres(model1esgG, grass.small$trap, grass.small$YEAR)
#
#
#
#

drop1(model1esgG, test = "F")
BIC(model1esgG)

# now fit the segmented model, setting 1979 as the start point to search for the break point

model1esg.seg <- segmented(model1esgG, seg.Z = ~YEAR, psi = 1979)

summary(model1esg.seg)
BIC(model1esg.seg)

anova(model1esg.seg, model1esgG)

chkres(model1esg.seg, grass.small$trap, grass.small$YEAR)

# set up the data for a figure

newdataesg <- expand.grid(YEAR = grass.small$YEAR, trap = levels(droplevels(grass.small$trap)), inopbiomass = 0)
newdataesg$inopbiomass <- exp(predict(model1esg.seg, newdata = newdataesg, type="response"))
newdataesg <- ddply(newdataesg, .(YEAR), numcolwise(mean))

averageesg <- ddply(grass.small,.(YEAR), summarise,
                    inopbiomass = gm_mean(inopbiomass))# TO ADD THE AVERAGE BIOMASS LINE




## urban

# first, fit the regular linear model, trying a few different error distributions
hist(urban.small$inopbiomass)
hist(log(urban.small$inopbiomass))



# log-normal
model1esuG <- lm(log(inopbiomass) ~ YEAR,
                 data = urban.small) 

summary(model1esuG)

chkres(model1esuG, urban.small$trap, urban.small$YEAR)
#
#
#
#


drop1(model1esuG, test = "F")
BIC(model1esuG)

# now fit the segmented model, setting 1979 as the start point to search for the break point

model1esu.seg <- segmented(model1esuG, seg.Z = ~YEAR, psi = 1979)

summary(model1esu.seg)
BIC(model1esu.seg)

anova(model1esu.seg, model1esuG)

chkres(model1esu.seg, urban.small$trap, urban.small$YEAR)

# set up the data for a figure

newdataesu <- expand.grid(YEAR = urban.small$YEAR, trap = levels(droplevels(urban.small$trap)), inopbiomass = 0)
newdataesu$inopbiomass <- exp(predict(model1esu.seg, newdata = newdataesu, type="response"))
newdataesu <- ddply(newdataesu, .(YEAR), numcolwise(mean))

averageesu <- ddply(urban.small,.(YEAR), summarise,
                    inopbiomass = gm_mean(inopbiomass))# TO ADD THE AVERAGE BIOMASS LINE




## now make the figures

## small

# group all these extra data sets together
newdataesa$Landuse <- "Arable"
newdataesw$Landuse <- "Woodland"
newdataesg$Landuse <- "Grassland"
newdataesu$Landuse <- "Urban"


averageesa$Landuse <- "Arable"
averageesw$Landuse <- "Woodland"
averageesg$Landuse <- "Grassland"
averageesu$Landuse <- "Urban"



newdataes <- rbind(newdataesa, newdataesw, newdataesg, newdataesu)
averagees <- rbind(averageesa, averageesw, averageesg, averageesu)


## finally, construct a figure


fig1es <- ggplot()+
  geom_line(data = averagees, aes(x= YEAR, y = inopbiomass, colour=Landuse), alpha= 0.75)+
  geom_line(data=newdataes, aes(x=YEAR, y=inopbiomass, colour=Landuse), size = 1) +
  scale_colour_manual(values = c("royalblue","goldenrod","grey30","coral3"), name = "Land-use")+
  scale_y_log10(breaks = c(1000,10000,100000,1000000), labels = comma, limits = c(200,1200000)) +
  labs(y = "Mean annual biomass \nper trap (mg)" , x = "Year") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=17))

fig1es

ggsave("Checked/Figures/Panels/Fig1e.svg", plot = fig1es, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)




### Fig 1f - biomass from broad land-uses with split regression


# for each of the land uses above, we want to split the dataset either side of 1980, 
# to get a single slope (with significance against zero) for trend before and after the universal break point

## small

# arable

# early

# first, fit the regular linear model, diving straight in at neg binom
arable.small.early <- arable.small[which(arable.small$YEAR <= 1982), ]


hist(arable.small.early$inopbiomass)

# negative binomial
model1fsaeNB <- lmer(log(inopbiomass) ~ YEAR + (1|trap),
                      data = arable.small.early) 

summary(model1fsaeNB)

chkres(model1fsaeNB, arable.small.early$trap, arable.small.early$YEAR)

# as before, it's close between negative binomial and log-normal but nb is more appropriate

drop1(model1fsaeNB, test = "Chi")
BIC(model1fsaeNB)

# calculate the % change per decade from the effect size
perc.ase <- 100*((exp(summary(model1fsaeNB)$coefficients[2,1])^10)-1)




# set up the data for a figure

newdatafsae <- expand.grid(YEAR = arable.small.early$YEAR, trap = levels(droplevels(arable.small.early$trap)), inopbiomass = 0)
newdatafsae$inopbiomass <- exp(predict(model1fsaeNB, newdata = newdatafsae, type="response"))
newdatafsae <- ddply(newdatafsae, .(YEAR), numcolwise(mean))
newdatafsae$Model <- "Arable, early"
newdatafsae$Significance <- "Significant"
newdatafsae$Landuse <- "Arable"


# late

# first, fit the regular linear model, diving straight in at neg binom
arable.small.late <- arable.small[which(arable.small$YEAR > 1982), ]


hist(arable.small.late$inopbiomass)

# negative binomial
model1fsalNB <- lmer(log(inopbiomass) ~ YEAR + (1|trap),
                       data = arable.small.late) 

summary(model1fsalNB)

chkres(model1fsalNB, arable.small.late$trap, arable.small.late$YEAR)

# model looks fine

drop1(model1fsalNB, test = "Chi")
BIC(model1fsalNB)

# calculate the % change per decade from the effect size
perc.asl <- 100*((exp(summary(model1fsalNB)$coefficients[2,1])^10)-1)




# set up the data for a figure

newdatafsal <- expand.grid(YEAR = arable.small.late$YEAR, trap = levels(droplevels(arable.small.late$trap)), inopbiomass = 0)
newdatafsal$inopbiomass <- exp(predict(model1fsalNB, newdata = newdatafsal, type="response"))
newdatafsal <- ddply(newdatafsal, .(YEAR), numcolwise(mean))
newdatafsal$Model <- "Arable, late"
newdatafsal$Significance <- "Non-significant"
newdatafsal$Landuse <- "Arable"

# woodland

# early

# first, fit the regular linear model, diving straight in at neg binom
wood.small.early <- wood.small[which(wood.small$YEAR <= 1982), ]


hist(wood.small.early$inopbiomass)

# negative binomial
model1fsweNB <- lmer(log(inopbiomass) ~ YEAR + (1|trap),
                       data = wood.small.early) 

summary(model1fsweNB)

chkres(model1fsweNB, wood.small.early$trap, wood.small.early$YEAR)

# as before, it's close between negative binomial and log-normal but nb is more appropriate

drop1(model1fsweNB, test = "Chi")
BIC(model1fsweNB)


# calculate the % change per decade from the effect size
perc.wse <- 100*((exp(summary(model1fsweNB)$coefficients[2,1])^10)-1)



# set up the data for a figure

newdatafswe <- expand.grid(YEAR = wood.small.early$YEAR, trap = levels(droplevels(wood.small.early$trap)), inopbiomass = 0)
newdatafswe$inopbiomass <- exp(predict(model1fsweNB, newdata = newdatafswe, type="response"))
newdatafswe <- ddply(newdatafswe, .(YEAR), numcolwise(mean))
newdatafswe$Model <- "wood, early"
newdatafswe$Significance <- "Significant"
newdatafswe$Landuse <- "Woodland"

# late

# first, fit the regular linear model, diving straight in at neg binom
wood.small.late <- wood.small[which(wood.small$YEAR > 1982), ]


hist(wood.small.late$inopbiomass)

# negative binomial
model1fswlNB <- lmer(log(inopbiomass) ~ YEAR + (1|trap),
                       data = wood.small.late) 

summary(model1fswlNB)

chkres(model1fswlNB, wood.small.late$trap, wood.small.late$YEAR)

# model looks fine

drop1(model1fswlNB, test = "Chi")
BIC(model1fswlNB)

# calculate the % change per decade from the effect size
perc.wsl <- 100*((exp(summary(model1fswlNB)$coefficients[2,1])^10)-1)



# set up the data for a figure

newdatafswl <- expand.grid(YEAR = wood.small.late$YEAR, trap = levels(droplevels(wood.small.late$trap)), inopbiomass = 0)
newdatafswl$inopbiomass <- exp(predict(model1fswlNB, newdata = newdatafswl, type="response"))
newdatafswl <- ddply(newdatafswl, .(YEAR), numcolwise(mean))
newdatafswl$Model <- "wood, late"
newdatafswl$Significance <- "Significant"
newdatafswl$Landuse <- "Woodland"

# grassland

# early

# first, fit the regular linear model, diving straight in at neg binom
grass.small.early <- grass.small[which(grass.small$YEAR <= 1982), ]


hist(grass.small.early$inopbiomass)

# negative binomial
model1fsgeNB <- lmer(log(inopbiomass) ~ YEAR + (1|trap),
                       data = grass.small.early) 

summary(model1fsgeNB)

chkres(model1fsgeNB, grass.small.early$trap, grass.small.early$YEAR)

# as before, it's close between negative binomial and log-normal but nb is more appropriate

drop1(model1fsgeNB, test = "Chi")
BIC(model1fsgeNB)

# calculate the % change per decade from the effect size
perc.gse <- 100*((exp(summary(model1fsgeNB)$coefficients[2,1])^10)-1)



# set up the data for a figure

newdatafsge <- expand.grid(YEAR = grass.small.early$YEAR, trap = levels(droplevels(grass.small.early$trap)), inopbiomass = 0)
newdatafsge$inopbiomass <- exp(predict(model1fsgeNB, newdata = newdatafsge, type="response"))
newdatafsge <- ddply(newdatafsge, .(YEAR), numcolwise(mean))
newdatafsge$Model <- "grass, early"
newdatafsge$Significance <- "Significant"
newdatafsge$Landuse <- "Grassland"

# late

# first, fit the regular linear model, diving straight in at neg binom
grass.small.late <- grass.small[which(grass.small$YEAR > 1982), ]


hist(grass.small.late$inopbiomass)

# negative binomial
model1fsglNB <- lmer(log(inopbiomass) ~ YEAR + (1|trap),
                       data = grass.small.late) 

summary(model1fsglNB)

chkres(model1fsglNB, grass.small.late$trap, grass.small.late$YEAR)

# model looks fine

drop1(model1fsglNB, test = "Chi")
BIC(model1fsglNB)

# calculate the % change per decade from the effect size
perc.gsl <- 100*((exp(summary(model1fsglNB)$coefficients[2,1])^10)-1)



# set up the data for a figure

newdatafsgl <- expand.grid(YEAR = grass.small.late$YEAR, trap = levels(droplevels(grass.small.late$trap)), inopbiomass = 0)
newdatafsgl$inopbiomass <- exp(predict(model1fsglNB, newdata = newdatafsgl, type="response"))
newdatafsgl <- ddply(newdatafsgl, .(YEAR), numcolwise(mean))
newdatafsgl$Model <- "grass, late"
newdatafsgl$Significance <- "Significant"
newdatafsgl$Landuse <- "Grassland"


# urban

# early

# first, fit the regular linear model, diving straight in at neg binom
urban.small.early <- urban.small[which(urban.small$YEAR <= 1982), ]


hist(urban.small.early$inopbiomass)

# negative binomial
model1fsueNB <- lmer(log(inopbiomass) ~ YEAR + (1|trap),
                       data = urban.small.early) 

summary(model1fsueNB)

chkres(model1fsueNB, urban.small.early$trap, urban.small.early$YEAR)

# as before, it's close between negative binomial and log-normal but nb is more appropriate

drop1(model1fsueNB, test = "Chi")
BIC(model1fsueNB)

# calculate the % change per decade from the effect size
perc.use <- 100*((exp(summary(model1fsueNB)$coefficients[2,1])^10)-1)



# set up the data for a figure

newdatafsue <- expand.grid(YEAR = urban.small.early$YEAR, trap = levels(droplevels(urban.small.early$trap)), inopbiomass = 0)
newdatafsue$inopbiomass <- exp(predict(model1fsueNB, newdata = newdatafsue, type="response"))
newdatafsue <- ddply(newdatafsue, .(YEAR), numcolwise(mean))
newdatafsue$Model <- "urban, early"
newdatafsue$Significance <- "Significant"
newdatafsue$Landuse <- "Urban"

# late

# first, fit the regular linear model, diving straight in at neg binom
urban.small.late <- urban.small[which(urban.small$YEAR > 1982), ]


hist(urban.small.late$inopbiomass)

# negative binomial
model1fsulNB <- lmer(log(inopbiomass) ~ YEAR + (1|trap),
                       data = urban.small.late) 

summary(model1fsulNB)

chkres(model1fsulNB, urban.small.late$trap, urban.small.late$YEAR)

# model looks fine

drop1(model1fsulNB, test = "Chi")
BIC(model1fsulNB)

# calculate the % change per decade from the effect size
perc.usl <- 100*((exp(summary(model1fsulNB)$coefficients[2,1])^10)-1)



# set up the data for a figure

newdatafsul <- expand.grid(YEAR = urban.small.late$YEAR, trap = levels(droplevels(urban.small.late$trap)), inopbiomass = 0)
newdatafsul$inopbiomass <- exp(predict(model1fsulNB, newdata = newdatafsul, type="response"))
newdatafsul <- ddply(newdatafsul, .(YEAR), numcolwise(mean))
newdatafsul$Model <- "urban, late"
newdatafsul$Significance <- "Significant"
newdatafsul$Landuse <- "Urban"




## construct the figure

# first pull together all the data simulations

newdatafs <- rbind(newdatafsae,newdatafswe,newdatafsge,newdatafsue,
                   newdatafsal,newdatafswl,newdatafsgl,newdatafsul)



# now make the figures

# small

fig1fs <- ggplot()+
  geom_line(data = averagees, aes(x= YEAR, y = inopbiomass, colour=Landuse), alpha = 0.75)+
  geom_line(data=newdatafs, aes(x=YEAR, y=inopbiomass, colour=Landuse, linetype=Significance, group=Model), size = 1) +
  scale_linetype_manual(values = c("dashed","solid"))+
  scale_colour_manual(values = c("royalblue","goldenrod","grey30","coral3"), name = "Land-use")+
  scale_y_log10(breaks = c(1000,10000,100000,1000000), labels = comma, limits = c(200,1200000)) +
  labs(y = "Mean annual biomass \nper trap (mg)" , x = "Year") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=17))

fig1fs


ggsave("Checked/Figures/Panels/Fig1f.svg", plot = fig1fs, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)




#### extract final figure 1

# we have all the (possible) individual panels, so let's create a couple of possible wranglings for Fig 1 and output them all

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


leg1 <- g_legend(fig1fs)


# small-scale only

leg1d <- g_legend(fig1d)
leg1es <- g_legend(fig1es)
leg1fs <- g_legend(fig1fs)

fig1.small <- grid.arrange(arrangeGrob(fig1a,fig1d + theme(legend.position = "none"),
                                       fig1es + theme(legend.position = "none"),
                                       fig1fs + theme(legend.position = "none"),
                                       ncol=2),
                           arrangeGrob(leg1d, leg1fs,
                                       ncol=1),
                           ncol=2, widths = c(10,2))

ggsave("Checked/Figures/Fig1_small.svg", plot = fig1.small, device = svg,
       width = 350, height = 250, units = "mm", limitsize = T)


### supplement 2

# we want to include a .csv file as the second supplement, which should have one row for every site*year combination
# and four columns of data: total biomass, and biomass of each of the three focal families

summary(alltraps.no0)
summary(fams.ereb)
summary(fams.geom)
summary(fams.noct)


# pick out the relevant data
table.s2.overall <- alltraps.no0[,c(1,2,9)]
table.s2.ereb <- fams.ereb[,c(2,3,5)]
table.s2.geom <- fams.geom[,c(2,3,5)]
table.s2.noct <- fams.noct[,c(2,3,5)]

# match up and prep the column names
colnames(table.s2.overall) <- c("Year","Trap","DerivedTotalBiomass")
colnames(table.s2.ereb) <- c("Year","Trap","DerivedBiomass_Erebidae")
colnames(table.s2.geom) <- c("Year","Trap","DerivedBiomass_Geometridae")
colnames(table.s2.noct) <- c("Year","Trap","DerivedBiomass_Noctuidae")


# begin merging, and put 0's into the 20 trap-years that caught no erebids
table.s2.eg <- merge(table.s2.ereb,table.s2.geom, all.y = T)
table.s2.eg[is.na(table.s2.eg)] <- 0

# complete merging
table.s2.fams <- merge(table.s2.eg,table.s2.noct)
table.s2 <- merge(table.s2.overall,table.s2.fams)

summary(table.s2)
head(table.s2)

# export
write.csv(table.s2, "Checked/TableS7.csv", row.names = F)

#### Fig S2 - break-points

# we want to extract the break-points and associated error from every segmented model we've run
# and turn them into a forest plot


# write a list of segmented model names

seg.mods.names.a <- c("model1a.seg","model1de.seg","model1dg.seg","model1dn.seg",
                    "model1esa.seg","model1esw.seg","model1esg.seg","model1esu.seg")

seg.mods.a <- list(model1a.seg,model1de.seg,model1dg.seg,model1dn.seg,
                 model1esa.seg,model1esw.seg,model1esg.seg,model1esu.seg)

# seed an output frame for a loop
seg.mod.breaks.a <- data.frame(Model = factor(),
                             Break = numeric(),
                             SE = numeric())

# and construct the loop

for (n in 1:8){
  mod <- seg.mods.a[[n]]
  Model <- seg.mods.names.a[[n]]
  
  Break <- summary(mod)$psi[2]
  SE <- summary(mod)$psi[3]
  
  out <- cbind(Model,Break,SE)
  
  seg.mod.breaks.a <- rbind(seg.mod.breaks.a,out)
}

seg.mod.breaks.a$Break <- as.numeric(as.character(seg.mod.breaks.a$Break))
seg.mod.breaks.a$SE <- as.numeric(as.character(seg.mod.breaks.a$SE))

seg.mod.breaks.a

# generate upper and lower 95% CIs from these

seg.mod.breaks.a$LCI <- seg.mod.breaks.a$Break - (1.96*seg.mod.breaks.a$SE)
seg.mod.breaks.a$UCI <- seg.mod.breaks.a$Break + (1.96*seg.mod.breaks.a$SE)

seg.mod.breaks.a$Model <- factor(seg.mod.breaks.a$Model, levels = rev(seg.mod.breaks.a$Model))


# we want to manually add a couple of columns of notation to this

seg.mod.breaks.a$ModelCode <- c("A","B","C","D","E","F","G","H")
seg.mod.breaks.a$ModelCode <- factor(seg.mod.breaks.a$ModelCode, levels = rev(seg.mod.breaks.a$ModelCode))

seg.mod.breaks.a$ModelGroup <- c("Full dataset",
                               "Erebidae","Geometridae","Noctuidae",
                               "Arable","Woodland","Grassland","Urban")
seg.mod.breaks.a$ModelGroup <- factor(seg.mod.breaks.a$ModelGroup, levels = rev(seg.mod.breaks.a$ModelGroup))


seg.mod.breaks.a$ModelCat <- c("Full dataset",
                             "Family","Family","Family",
                             "Land-use","Land-use","Land-use","Land-use")


## now we can start to construct the figure

figs2a <- ggplot(seg.mod.breaks.a, aes(x = ModelGroup, y = Break, ymin = LCI, ymax = UCI, colour = ModelCat))+
  geom_point()+
  geom_errorbar(width = 0.5)+
  coord_flip()+
  theme_classic()+
  scale_y_continuous(limits = c(1962,2018), breaks = seq(1970,2010,10))+
  xlab("Model")+ ylab("Estimated break point")+
  scale_colour_manual(values = c("royalblue","goldenrod","coral3"))+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey80"))


figs2a



ggsave("Checked/Figures/FigS2a.svg", plot = figs2a, device = svg,
       width = 150, height = 60, units = "mm", limitsize = T)



### now prepare the data for the analyses underpinning fig 2


### for Fig 2

# the first thing we need to do for Fig 2 is to calculate the annual change in biomass (relative to the previous year)
# for each site in each year

# we'll be calculating this from the full dataset:

summary(alltraps.no0)

# seed a list of sites to run across
trap.list <- levels(droplevels(alltraps.no0$trap))


# seed an output frame for a loop

prop.change <- data.frame(trap = factor(),
                          year = numeric(),
                          run = factor(),
                          est.biomass = numeric(),
                          inopbiomass = numeric(),
                          prev.est.biomass = numeric(),
                          prev.inop.biomass = numeric(),
                          prop.change.est.biomass = numeric(),
                          prop.change.inopbiomass = numeric())


# run the loop

for (trap in trap.list){
  print(trap)
  trap.data <- alltraps.no0[which(alltraps.no0$trap == trap), ]
  
  trap.first.year <- min(trap.data$YEAR)
  trap.last.year <- max(trap.data$YEAR)
  
  run <- 1
  
  for(year in (1+trap.first.year):min(trap.last.year, 2016)){
    if((nrow(trap.data[which(trap.data$YEAR == year), ]) > 0) & (nrow(trap.data[which(trap.data$YEAR == (year-1)), ]) > 0)){
      
      est.biomass <- trap.data[which(trap.data$YEAR == year),'EST.BIOMASS']
      inopbiomass <- trap.data[which(trap.data$YEAR == year),'inopbiomass']
      prev.est.biomass <- trap.data[which(trap.data$YEAR == (year-1)),'EST.BIOMASS']
      prev.inop.biomass <- trap.data[which(trap.data$YEAR == (year-1)),'inopbiomass']
      
      prop.change.est.biomass <- est.biomass/prev.est.biomass
      prop.change.inopbiomass <- inopbiomass/prev.inop.biomass
      
      out <- cbind(trap,year,run,est.biomass,inopbiomass,prev.est.biomass,prev.inop.biomass,prop.change.est.biomass,prop.change.inopbiomass)
      prop.change <- rbind(prop.change,out)
      
      if(nrow(trap.data[which(trap.data$YEAR == (year+1)),]) == 0){
        run <- run + 1
      }
    }
  }
}


prop.change$year <- as.numeric(as.character(prop.change$year))
prop.change$est.biomass <- as.numeric(as.character(prop.change$est.biomass))
prop.change$inopbiomass <- as.numeric(as.character(prop.change$inopbiomass))
prop.change$prev.est.biomass <- as.numeric(as.character(prop.change$prev.est.biomass))
prop.change$prev.inop.biomass <- as.numeric(as.character(prop.change$prev.inop.biomass))
prop.change$prop.change.est.biomass <- as.numeric(as.character(prop.change$prop.change.est.biomass))
prop.change$prop.change.inopbiomass <- as.numeric(as.character(prop.change$prop.change.inopbiomass))

prop.change$trap.run <- as.factor(paste(prop.change$trap, prop.change$run, sep = "-"))

summary(prop.change)


## finally merge the climate and land-use data into these data

# read it in
climate <- read.csv("Checked/Landuse and climate/Climate data/sites_climate.csv", header = T)

# merge it in
prop.changes.climate <- merge(prop.change, climate, all.x = T)


## and pull in the land-use data as well
landuse$trap <- landuse$SITE
change.climate.landuse <- merge(prop.changes.climate, landuse, all.x = T, by = "trap")


## finally, calculate a single annual mean for proportional change per land-use type (averaging across sites)

# append a landuse category to the data
change.climate.landuse$large.cat <- as.factor(ifelse(change.climate.landuse$large %in% grasslandclass, "Grassland",
                                                     ifelse(change.climate.landuse$large %in% woodlandclass, "Woodland",
                                                            ifelse(change.climate.landuse$large %in% arableclass, "Arable",
                                                                   ifelse(change.climate.landuse$large %in% urbanclass, "Urban","Other")))))


change.climate.landuse$small.cat <- as.factor(ifelse(change.climate.landuse$small %in% grasslandclass, "Grassland",
                                                     ifelse(change.climate.landuse$small %in% woodlandclass, "Woodland",
                                                            ifelse(change.climate.landuse$small %in% arableclass, "Arable",
                                                                   ifelse(change.climate.landuse$small %in% urbanclass, "Urban","Other")))))


# collapse the data by these categories
names(change.climate.landuse$trap) <- NULL
names(change.climate.landuse$trap.run) <- NULL
names(change.climate.landuse$run) <- NULL


change.annual.landuse.large <- ddply(change.climate.landuse, .(year,large.cat),summarise,
                                     mean.change.est.biomass = gm_mean(prop.change.est.biomass),
                                     mean.change.inopbiomass = gm_mean(prop.change.inopbiomass))


change.annual.landuse.small <- ddply(change.climate.landuse, .(year,small.cat),summarise,
                                     mean.change.est.biomass = gm_mean(prop.change.est.biomass),
                                     mean.change.inopbiomass = gm_mean(prop.change.inopbiomass))


# and overall
change.average.all <- ddply(change.climate.landuse, .(year), summarise,
                            mean.change.est.biomass = gm_mean(prop.change.est.biomass),
                            mean.change.inopbiomass = gm_mean(prop.change.inopbiomass),
                            mean.monthly.temp = mean(meanmonthlytemp),
                            mean.annual.rain = mean(annualrain))



# finally, cast this out so that there's a column for each landuse
cast.call.inop <- spread(change.annual.landuse.large[,c(1:2,4)], key = large.cat, value = mean.change.inopbiomass)
cast.call.est <- spread(change.annual.landuse.large[,c(1:3)], key = large.cat, value = mean.change.est.biomass)

cast.cals.inop <- spread(change.annual.landuse.small[,c(1:2,4)], key = small.cat, value = mean.change.inopbiomass)
cast.cals.est <- spread(change.annual.landuse.small[,c(1:3)], key = small.cat, value = mean.change.est.biomass)




### before analysing, a quick diversion in order to plot a map

### Fig S1 - map of sites

summary(all.landuse)

trap.summary <- ddply(all.landuse, .(trap,LongestDuration,small,EASTING,NORTHING,HEast,HNorth,LAT,LON), summarise,
                      no.years = length(YEAR),
                      first.year = min(YEAR))



trap.summary$small.cat <- as.factor(ifelse(trap.summary$small %in% grasslandclass, "Grassland",
                                           ifelse(trap.summary$small %in% woodlandclass, "Woodland",
                                                  ifelse(trap.summary$small %in% arableclass, "Arable",
                                                         ifelse(trap.summary$small %in% urbanclass, "Urban","Other")))))

trap.summary$duration.cat <- as.factor(ifelse(trap.summary$LongestDuration >= 50, "50+ years", "30-49 years"))


trap.summary$start.cat <- as.factor(ifelse(trap.summary$first.year > 1970, "After 1970","1970 or earlier"))


summary(trap.summary)

# write out this table for use elsewhere
write.csv(trap.summary, "Checked/trap_summary.csv", row.names = F)


# remind ourselves of the map data from the top
map1


map <- ggplot(map1, aes(x = long, y = lat))+
  geom_polygon(aes(group = group),fill = "white", colour = "black") +
  geom_point(data = trap.summary, aes(x = LON, y = LAT, shape = start.cat, colour = small.cat, fill = small.cat)) +
  scale_shape_manual(name = "Start of trapping",
                     values = c(25,24))+
  scale_fill_manual(name = "Land-use class",
                    values = c("goldenrod","royalblue","grey60","darkgreen"))+
  scale_colour_manual(name = "Land-use class",
                      values = c("goldenrod","royalblue","grey60","darkgreen"))+
  theme_classic()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

map



ggsave("Checked/Figures/FigS1.svg", plot = map, device = svg,
       width = 150, height = 150, units = "mm")








#### Fig 2 - change in biomass

### Fig 2a - change in biomass over time

# first, fit the regular linear model
# in this case the only appropriate error distribution is log-Gaussian since it is proportional-change data
# (i.e. lognormal-distributed about 1)
hist(change.climate.landuse$prop.change.inopbiomass)
hist(log(change.climate.landuse$prop.change.inopbiomass))


# log-normal
model2aG <- lmer(log(prop.change.inopbiomass) ~ year + (1|trap),
                 data = change.climate.landuse) 

summary(model2aG)

chkres(model2aG, change.climate.landuse$year, change.climate.landuse$trap)
#
#
#
#


drop1(model2aG, test = "Chi")

r.squaredGLMM(model2aG)



# since this was significant, let's predict some data to put a line on a graph


newdata2a <- expand.grid(year = change.climate.landuse$year, trap = levels(droplevels(change.climate.landuse$trap)), prop.change.inopbiomass = 0)
newdata2a$prop.change.inopbiomass <- predict(model2aG, newdata = newdata2a, type="response")
newdata2a <- ddply(newdata2a, .(year), numcolwise(mean))




fig2a <- ggplot(change.climate.landuse, aes(x = year, y = prop.change.inopbiomass))+
  geom_line(aes(group = trap.run),
            colour = "grey70") +
  geom_line(data = change.average.all, aes(x = year, y = mean.change.inopbiomass), size= 0.75)+
  geom_line(data = newdata2a, aes(x = year, y = exp(prop.change.inopbiomass)), colour = "royalblue", size = 0.8)+
  scale_y_log10(breaks = c(0.1,1,10), limits = c(0.02,20)) +
  labs(y = "Proportional biomass change \nsince previous year" , x = "Year") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")


fig2a

ggsave("Checked/Figures/Panels/Fig2a.svg", plot = fig2a, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)



### Fig 2b - change in biomass against temperature

# fit the regular linear model

# log-normal
model2bG <- lmer(log(prop.change.inopbiomass) ~ meanmonthlytemp + (1|trap),
                 data = change.climate.landuse) 

summary(model2bG)

chkres(model2bG, change.climate.landuse$meanmonthlytemp, change.climate.landuse$trap)
#
#
#
#


drop1(model2bG, test = "Chi")
r.squaredGLMM(model2bG)

# non-significant so no need to put a line on the graph

fig2b <- ggplot(change.climate.landuse, aes(x = meanmonthlytemp, y = prop.change.inopbiomass))+
  geom_point() +
  scale_y_log10(breaks = c(0.1,1,10), limits = c(0.02,20)) +
  labs(y = "Proportional biomass change \nsince previous year" , x = "Mean daily temperature (degrees Celsius)") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")


fig2b

ggsave("Checked/Figures/Panels/Fig2c.svg", plot = fig2b, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)



### Fig 2ba - change in biomass against previous year's temperature

# fit the regular linear model

# log-normal
model2bGa <- lmer(log(prop.change.inopbiomass) ~ prev.temp + (1|trap),
                 data = change.climate.landuse) 

summary(model2bGa)

chkres(model2bGa, change.climate.landuse$prev.temp, change.climate.landuse$trap)
#
#
#
#


drop1(model2bGa, test = "Chi")
r.squaredGLMM(model2bGa)

# non-significant so no need to put a line on the graph

fig2ba <- ggplot(change.climate.landuse, aes(x = prev.temp, y = prop.change.inopbiomass))+
  geom_point() +
  scale_y_log10(breaks = c(0.1,1,10), limits = c(0.02,20)) +
  labs(y = "Proportional biomass change \nsince previous year" , x = "Mean daily temperature \nin previous year (degrees Celsius)") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")


fig2ba



### Fig 2c - change in biomass against rainfall


# fit the regular linear model

# log-normal
model2cG <- lmer(log(prop.change.inopbiomass) ~ annualrain + (1|trap),
                 data = change.climate.landuse) 

summary(model2cG)

chkres(model2cG, change.climate.landuse$annualrain, change.climate.landuse$trap)
#
#
#
#


drop1(model2cG, test = "Chi")
r.squaredGLMM(model2cG)

# non-significant

fig2c <- ggplot(change.climate.landuse, aes(x = annualrain, y = prop.change.inopbiomass))+
  geom_point() +
  scale_y_log10(breaks = c(0.1,1,10), limits = c(0.02,20)) +
  labs(y = "Proportional biomass change \nsince previous year" , x = "Annual rainfall (mm)") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")


fig2c

ggsave("Checked/Figures/Panels/Fig2d.svg", plot = fig2c, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)




### Fig 2ca - change in biomass against previous year's rainfall


# fit the regular linear model

# log-normal
model2cGa <- lmer(log(prop.change.inopbiomass) ~ prev.rain + (1|trap),
                 data = change.climate.landuse) 

summary(model2cGa)

chkres(model2cGa, change.climate.landuse$prev.rain, change.climate.landuse$trap)
#
#
#
#


drop1(model2cGa, test = "Chi")
r.squaredGLMM(model2cGa)

# non-significant

fig2ca <- ggplot(change.climate.landuse, aes(x = prev.rain, y = prop.change.inopbiomass))+
  geom_point() +
  scale_y_log10(breaks = c(0.1,1,10), limits = c(0.02,20)) +
  labs(y = "Proportional biomass change \nsince previous year" , x = "Annual rainfall \nin previous year (mm)") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")


fig2ca





### Fig 2d - change in biomass against previous year's biomass


# fit the regular linear model

# log-normal
model2dG <- lmer(log(prop.change.inopbiomass) ~ log(prev.inop.biomass) + (1|trap),
                 data = change.climate.landuse) 

summary(model2dG)

chkres(model2dG, log(change.climate.landuse$prev.inop.biomass), change.climate.landuse$trap)
#
#
#
#


drop1(model2dG, test = "Chi")
r.squaredGLMM(model2dG)

# since this was significant, let's predict some data to put a line on a graph


newdata2d <- expand.grid(prev.inop.biomass = change.climate.landuse$prev.inop.biomass, trap = levels(droplevels(change.climate.landuse$trap)), prop.change.inopbiomass = 0)
newdata2d$prop.change.inopbiomass <- predict(model2dG, newdata = newdata2d, type="response")
newdata2d <- ddply(newdata2d, .(prev.inop.biomass), numcolwise(mean))





fig2d <- ggplot(change.climate.landuse, aes(x = prev.inop.biomass, y = prop.change.inopbiomass))+
  geom_point() +
  geom_line(data = newdata2d, aes(x = prev.inop.biomass, y = exp(prop.change.inopbiomass)), colour = "royalblue", size = 0.8)+
  scale_y_log10(breaks = c(0.1,1,10), limits = c(0.02,20)) +
  scale_x_log10(labels = comma)+
  labs(y = "Proportional biomass change \nsince previous year" , x = "Biomass in previous year (mg)") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")


fig2d

ggsave("Checked/Figures/Panels/Fig2b.svg", plot = fig2d, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)



### Fig 2k - change in biomass against NDVI


# fit the regular linear model

# first get rid of the earlier years for which we don't have NDVI estimates

NDVI240.dat <- change.climate.landuse[which(!is.na(change.climate.landuse$NDVI240)), ]
NDVI30.dat <- change.climate.landuse[which(!is.na(change.climate.landuse$NDVI30)), ]

NDVI.prev240.dat <- change.climate.landuse[which(!is.na(change.climate.landuse$prev.NDVI240)), ]
NDVI.prev30.dat <- change.climate.landuse[which(!is.na(change.climate.landuse$prev.NDVI30)), ]


# log-normal
model2kG <- lmer(log(prop.change.inopbiomass) ~ NDVI240 + (1|trap),
                 data = NDVI240.dat) 

summary(model2kG)

chkres(model2kG, NDVI240.dat$NDVI240, NDVI240.dat$trap)
#
#
#
#


drop1(model2kG, test = "Chi")
r.squaredGLMM(model2kG)

# non-significant

fig2k <- ggplot(NDVI240.dat, aes(x = NDVI240, y = prop.change.inopbiomass))+
  geom_point() +
  scale_y_log10(breaks = c(0.1,1,10), limits = c(0.02,20)) +
  labs(y = "Proportional biomass change \nsince previous year" , x = "NDVI") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")


fig2k

ggsave("Checked/Figures/Panels/Fig2k.svg", plot = fig2k, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)




### Fig 2ka - change in biomass against previous year's NDVI


# fit the regular linear model

# log-normal
model2kGa <- lmer(log(prop.change.inopbiomass) ~ prev.NDVI240 + (1|trap),
                  data = NDVI.prev240.dat) 

summary(model2kGa)

chkres(model2kGa, NDVI.prev240.dat$prev.NDVI240, NDVI.prev240.dat$trap)
#
#
#
#


drop1(model2kGa, test = "Chi")
r.squaredGLMM(model2kGa)

# non-significant

fig2ka <- ggplot(NDVI.prev240.dat, aes(x = prev.NDVI240, y = prop.change.inopbiomass))+
  geom_point() +
  scale_y_log10(breaks = c(0.1,1,10), limits = c(0.02,20)) +
  labs(y = "Proportional biomass change \nsince previous year" , x = "NDVI \nin previous year") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")


fig2ka


## repeat with NDVI estimated at a different spatial scale

# fit the regular linear model

# log-normal
model2kGb <- lmer(log(prop.change.inopbiomass) ~ NDVI30 + (1|trap),
                 data = NDVI30.dat) 

summary(model2kGb)

chkres(model2kGb, NDVI30.dat$NDVI30, NDVI30.dat$trap)
#
#
#
#


drop1(model2kGb, test = "Chi")
r.squaredGLMM(model2kGb)

# non-significant

fig2kb <- ggplot(NDVI30.dat, aes(x = NDVI30, y = prop.change.inopbiomass))+
  geom_point() +
  scale_y_log10(breaks = c(0.1,1,10), limits = c(0.02,20)) +
  labs(y = "Proportional biomass change \nsince previous year" , x = "NDVI") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")


fig2kb





### Fig 2kc - change in biomass against previous year's NDVI


# fit the regular linear model

# log-normal
model2kGc <- lmer(log(prop.change.inopbiomass) ~ prev.NDVI30 + (1|trap),
                  data = NDVI.prev30.dat) 

summary(model2kGc)

chkres(model2kGc, NDVI.prev30.dat$prev.NDVI30, NDVI.prev30.dat$trap)
#
#
#
#


drop1(model2kGc, test = "Chi")
r.squaredGLMM(model2kGc)

# non-significant

fig2kc <- ggplot(NDVI.prev30.dat, aes(x = prev.NDVI30, y = prop.change.inopbiomass))+
  geom_point() +
  scale_y_log10(breaks = c(0.1,1,10), limits = c(0.02,20)) +
  labs(y = "Proportional biomass change \nsince previous year" , x = "NDVI \nin previous year") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")


fig2kc



### try chucking the whole lot into one model
fullmod.dat <- NDVI240.dat[which(!is.na(NDVI240.dat$prev.NDVI240)), ]


model2m <- lmer(log(prop.change.inopbiomass) ~ year + prev.inop.biomass +
                    meanmonthlytemp + prev.temp +
                    annualrain + prev.rain +
                    NDVI240 + prev.NDVI240 +
                    (1|trap),
                  data = fullmod.dat) 

summary(model2m)

chkres(model2m, fullmod.dat$year, fullmod.dat$trap)
#
#
#
#


drop1(model2m, test = "Chi")
r.squaredGLMM(model2m)


# and raw biomass

model2ma <- lmer(log(inopbiomass) ~ year + prev.inop.biomass +
                  meanmonthlytemp + prev.temp +
                  annualrain + prev.rain +
                  NDVI240 + prev.NDVI240 +
                  (1|trap),
                data = fullmod.dat) 

summary(model2ma)

chkres(model2ma, fullmod.dat$year, fullmod.dat$trap)
#
#
#
#


drop1(model2ma, test = "Chi")
r.squaredGLMM(model2ma)



### Fig 2e-j - pairwise change in biomass amongst land-uses

# e - arable vs grassland

plot(cast.cals.inop$Arable ~ cast.cals.inop$Grassland)

# fit a regular log-log linear model

# log-normal
model2e <- lm(log(Arable) ~ log(Grassland),
              data = cast.cals.inop) 

summary(model2e)

chkres(model2e, log(cast.cals.inop$Arable), log(cast.cals.inop$Grassland))
#
#
#
#


drop1(model2e, test = "F")


cor.test(log(cast.cals.inop$Arable),log(cast.cals.inop$Grassland), 
    method = "pearson", use = "complete.obs")


# since this was significant, let's predict some data to put a line on a graph


newdata2e <- expand.grid(Grassland = cast.cals.inop$Grassland, Arable = 0)
newdata2e$Arable <- predict(model2e, newdata = newdata2e, type="response")
newdata2e <- ddply(newdata2e, .(Grassland), numcolwise(mean))



fig2e <- ggplot(cast.cals.inop, aes(x = Grassland, y = Arable))+
  geom_point() +
  geom_line(data = newdata2e, aes(x = Grassland, y = exp(Arable)), colour = "royalblue", size = 0.8)+
  scale_y_log10(limits = c(0.4,5)) +
  scale_x_log10(limits = c(0.4,5))+
  labs(y = "Arable:\nProportional biomass change \nsince previous year" ,
       x = "Grassland:\nProportional biomass change \nsince previous year") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")


fig2e

ggsave("Checked/Figures/Panels/Fig2e.svg", plot = fig2e, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)



# f - arable vs woodland


plot(cast.cals.inop$Arable ~ cast.cals.inop$Woodland)

# fit a regular log-log linear model

# log-normal
model2f <- lm(log(Arable) ~ log(Woodland),
              data = cast.cals.inop) 

summary(model2f)

chkres(model2f, log(cast.cals.inop$Arable), log(cast.cals.inop$Woodland))
#
#
#
#


drop1(model2f, test = "F")

cor.test(log(cast.cals.inop$Arable),log(cast.cals.inop$Woodland), 
         method = "pearson", use = "complete.obs")



# since this was significant, let's predict some data to put a line on a graph


newdata2f <- expand.grid(Woodland = cast.cals.inop$Woodland, Arable = 0)
newdata2f$Arable <- predict(model2f, newdata = newdata2f, type="response")
newdata2f <- ddply(newdata2f, .(Woodland), numcolwise(mean))



fig2f <- ggplot(cast.cals.inop, aes(x = Woodland, y = Arable))+
  geom_point() +
  geom_line(data = newdata2f, aes(x = Woodland, y = exp(Arable)), colour = "royalblue", size = 0.8)+
  scale_y_log10(limits = c(0.4,5)) +
  scale_x_log10(limits = c(0.4,5))+
  labs(y = "Arable:\nProportional biomass change \nsince previous year" ,
       x = "Woodland:\nProportional biomass change \nsince previous year") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")


fig2f


ggsave("Checked/Figures/Panels/Fig2f.svg", plot = fig2f, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)



# g - arable vs urban


plot(cast.cals.inop$Arable ~ cast.cals.inop$Urban)

# fit a regular log-log linear model

# log-normal
model2g <- lm(log(Arable) ~ log(Urban),
              data = cast.cals.inop) 

summary(model2g)

chkres(model2g, log(cast.cals.inop$Arable), log(cast.cals.inop$Urban))
#
#
#
#


drop1(model2g, test = "F")

cor.test(log(cast.cals.inop$Arable),log(cast.cals.inop$Urban), 
         method = "pearson", use = "complete.obs")


# since this was significant, let's predict some data to put a line on a graph


newdata2g <- expand.grid(Urban = cast.cals.inop$Urban, Arable = 0)
newdata2g$Arable <- predict(model2g, newdata = newdata2g, type="response")
newdata2g <- ddply(newdata2g, .(Urban), numcolwise(mean))



fig2g <- ggplot(cast.cals.inop, aes(x = Urban, y = Arable))+
  geom_point() +
  geom_line(data = newdata2g, aes(x = Urban, y = exp(Arable)), colour = "royalblue", size = 0.8)+
  scale_y_log10(limits = c(0.4,5)) +
  scale_x_log10(limits = c(0.4,5))+
  labs(y = "Arable:\nProportional biomass change \nsince previous year" ,
       x = "Urban:\nProportional biomass change \nsince previous year") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")


fig2g

ggsave("Checked/Figures/Panels/Fig2g.svg", plot = fig2g, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)




# h - grassland vs woodland

plot(cast.cals.inop$Grassland ~ cast.cals.inop$Woodland)

# fit a regular log-log linear model

# log-normal
model2h <- lm(log(Grassland) ~ log(Woodland),
              data = cast.cals.inop) 

summary(model2h)

chkres(model2h, log(cast.cals.inop$Grassland), log(cast.cals.inop$Woodland))
#
#
#
#


drop1(model2h, test = "F")

cor.test(log(cast.cals.inop$Grassland),log(cast.cals.inop$Woodland), 
         method = "pearson", use = "complete.obs")


# since this was significant, let's predict some data to put a line on a graph


newdata2h <- expand.grid(Woodland = cast.cals.inop$Woodland, Grassland = 0)
newdata2h$Grassland <- predict(model2h, newdata = newdata2h, type="response")
newdata2h <- ddply(newdata2h, .(Woodland), numcolwise(mean))



fig2h <- ggplot(cast.cals.inop, aes(x = Woodland, y = Grassland))+
  geom_point() +
  geom_line(data = newdata2h, aes(x = Woodland, y = exp(Grassland)), colour = "royalblue", size = 0.8)+
  scale_y_log10(limits = c(0.4,5)) +
  scale_x_log10(limits = c(0.4,5))+
  labs(y = "Grassland:\nProportional biomass change \nsince previous year" ,
       x = "Woodland:\nProportional biomass change \nsince previous year") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")


fig2h

ggsave("Checked/Figures/Panels/Fig2h.svg", plot = fig2h, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)




# i - grassland vs urban

plot(cast.cals.inop$Grassland ~ cast.cals.inop$Urban)

# fit a regular log-log linear model

# log-normal
model2i <- lm(log(Grassland) ~ log(Urban),
              data = cast.cals.inop) 

summary(model2i)

chkres(model2i, log(cast.cals.inop$Grassland), log(cast.cals.inop$Urban))
#
#
#
#


drop1(model2i, test = "F")


cor.test(log(cast.cals.inop$Grassland),log(cast.cals.inop$Urban), 
         method = "pearson", use = "complete.obs")


# since this was significant, let's predict some data to put a line on a graph


newdata2i <- expand.grid(Urban = cast.cals.inop$Urban, Grassland = 0)
newdata2i$Grassland <- predict(model2i, newdata = newdata2i, type="response")
newdata2i <- ddply(newdata2i, .(Urban), numcolwise(mean))



fig2i <- ggplot(cast.cals.inop, aes(x = Urban, y = Grassland))+
  geom_point() +
  geom_line(data = newdata2i, aes(x = Urban, y = exp(Grassland)), colour = "royalblue", size = 0.8)+
  scale_y_log10(limits = c(0.4,5)) +
  scale_x_log10(limits = c(0.4,5))+
  labs(y = "Grassland:\nProportional biomass change \nsince previous year" ,
       x = "Urban:\nProportional biomass change \nsince previous year") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")


fig2i



ggsave("Checked/Figures/Panels/Fig2i.svg", plot = fig2i, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)





# j - woodland vs urban

plot(cast.cals.inop$Woodland ~ cast.cals.inop$Urban)

# fit a regular log-log linear model

# log-normal
model2j <- lm(log(Woodland) ~ log(Urban),
              data = cast.cals.inop) 

summary(model2j)

chkres(model2j, log(cast.cals.inop$Woodland), log(cast.cals.inop$Urban))
#
#
#
#


drop1(model2j, test = "F")

cor.test(log(cast.cals.inop$Woodland),log(cast.cals.inop$Urban), 
         method = "pearson", use = "complete.obs")


# since this was significant, let's predict some data to put a line on a graph


newdata2j <- expand.grid(Urban = cast.cals.inop$Urban, Woodland = 0)
newdata2j$Woodland <- predict(model2j, newdata = newdata2j, type="response")
newdata2j <- ddply(newdata2j, .(Woodland), numcolwise(mean))



fig2j <- ggplot(cast.cals.inop, aes(x = Urban, y = Woodland))+
  geom_point() +
  geom_line(data = newdata2j, aes(x = Urban, y = exp(Woodland)), colour = "royalblue", size = 0.8)+
  scale_y_log10(limits = c(0.4,5)) +
  scale_x_log10(limits = c(0.4,5))+
  labs(y = "Woodland:\nProportional biomass change \nsince previous year" ,
       x = "Urban:\nProportional biomass change \nsince previous year") +
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")


fig2j

ggsave("Checked/Figures/Panels/Fig2j.svg", plot = fig2j, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)



### now combine and export these figures


fig2 <- grid.arrange(fig2a,fig2d,
                     fig2b,fig2c,
                     fig2e,fig2f,
                     fig2g,fig2h,
                     fig2i,fig2j,
                     ncol=2)

ggsave("Checked/Figures/Fig2_small.svg", plot = fig2, device = svg,
       width = 350, height = 550, units = "mm", limitsize = T)







#### pull out a few extra stats ####

### comparing the mean annual biomass per trap between different time periods

# generate these mean annual values

mean.annual <- ddply(alltraps.no0, .(YEAR), summarise,
                     mean.biomass = gm_mean(inopbiomass))


# split this off into periods of interest

mean.annual.start <- mean.annual[which(mean.annual$YEAR %in% 1967:1976), ]
mean.annual.end <- mean.annual[which(mean.annual$YEAR %in% 2008:2017), ]
mean.annual.peak <- mean.annual[which(mean.annual$YEAR %in% 1978:1987), ]


# calculate mean biomasses per trap/decade and s.e.
mean.start <- gm_mean(mean.annual.start$mean.biomass)
se.start <- sd(mean.annual.start$mean.biomass)/sqrt(nrow(mean.annual.start))

mean.end <- gm_mean(mean.annual.end$mean.biomass)
se.end <- sd(mean.annual.end$mean.biomass)/sqrt(nrow(mean.annual.end))

mean.peak <- gm_mean(mean.annual.peak$mean.biomass)
se.peak <- sd(mean.annual.peak$mean.biomass)/sqrt(nrow(mean.annual.peak))


# test the mean biomasses per trap/year against each other between decades
t.test(log(mean.annual.start$mean.biomass), log(mean.annual.end$mean.biomass))
t.test(log(mean.annual.start$mean.biomass), log(mean.annual.peak$mean.biomass))
t.test(log(mean.annual.end$mean.biomass), log(mean.annual.peak$mean.biomass))



## comparing biomass in different land-uses for the late period

mean.arable <- gm_mean(arable.small.late$inopbiomass)
se.arable <- sd(arable.small.late$inopbiomass)/sqrt(nrow(arable.small.late))

mean.urban <- gm_mean(urban.small.late$inopbiomass)
se.urban <- sd(urban.small.late$inopbiomass)/sqrt(nrow(urban.small.late))

mean.grass <- gm_mean(grass.small.late$inopbiomass)
se.grass <- sd(grass.small.late$inopbiomass)/sqrt(nrow(grass.small.late))

mean.wood <- gm_mean(wood.small.late$inopbiomass)
se.wood <- sd(wood.small.late$inopbiomass)/sqrt(nrow(wood.small.late))


# cross-test these
t.test(log(arable.small.late$inopbiomass), log(urban.small.late$inopbiomass))
t.test(log(arable.small.late$inopbiomass), log(grass.small.late$inopbiomass))
t.test(log(arable.small.late$inopbiomass), log(wood.small.late$inopbiomass))
t.test(log(grass.small.late$inopbiomass), log(urban.small.late$inopbiomass))
t.test(log(wood.small.late$inopbiomass), log(urban.small.late$inopbiomass))
t.test(log(grass.small.late$inopbiomass), log(wood.small.late$inopbiomass))



## calculate mean biomass per trap in 1983 only for each land-use
mean.arable.83 <- gm_mean(arable.small.late[which(arable.small.late$YEAR == 1983),'inopbiomass'])
mean.grass.83 <- gm_mean(grass.small.late[which(grass.small.late$YEAR == 1983),'inopbiomass'])
mean.wood.83 <- gm_mean(wood.small.late[which(wood.small.late$YEAR == 1983),'inopbiomass'])
mean.urban.83 <- gm_mean(urban.small.late[which(urban.small.late$YEAR == 1983),'inopbiomass'])




### test effect of baselines and continuous data ####

## we want to test how much the selection of different baselines, different durations of recording, 
# and of single years of recording vs continuous datasets,
# can influence the estimated (linear) scale of biomass change

## we want to do this both for the overall model (i.e. all sites included)
# and for each site individually


# first things first, set up a list of start and end dates

start <- c(1967:2016)
end <- c(1968:2017)


# let's do the analysis for the full dataset first

baselines.full <- data.frame()


for (x in start){
  for (y in end){
    
    # test duration and keep only combinations of at least 5 years
    duration <- y - x
    
    if (duration >= 5){
      
      print(paste(x,y,sep = "-"))
      
      # pull out relevant data
      years <- c(x:y)

      years.dat <- alltraps.no0[which(alltraps.no0$YEAR %in% years), ]
      
      
      # difference the first and last years
      first <- mean(years.dat[which(years.dat$YEAR == x),'inopbiomass'])
      last <- mean(years.dat[which(years.dat$YEAR == y),'inopbiomass'])
      
      diff.pa <- (last-first)/duration
      
      
      # calculate this difference as log-odds of change p.a.
      diff.lo <- (log(last) - log(first))/duration
      
      # and convert this into perc change pa
      diff.perc.pa <- 100*(exp(diff.lo)-1)
      
      
      
      # calculate model slope
      model.yearsNB <- glm.nb(round(inopbiomass) ~ YEAR,
                          link = "log",
                          data = years.dat) 
      
      
      slope.pa <- summary(model.yearsNB)$coefficients[2,1]
      sig.slope.pa <- drop1(model.yearsNB, test = "F")[2,5]
      
      
      # convert model slopes into perc change pa
      perc.pa <- 100*(exp(slope.pa)-1)
      
      
      out <- cbind(x,y,duration,first,last,diff.pa,diff.perc.pa,slope.pa,sig.slope.pa,perc.pa)
      
      baselines.full <- rbind(baselines.full,out)
    }
  }
}

summary(baselines.full)

# check whether direction of effect is consistent between linear model and point-to-point sample

baselines.full$agreement <- as.factor(ifelse(baselines.full$diff.perc.pa > 0 & baselines.full$perc.pa > 0, "Agree",
                                             ifelse(baselines.full$diff.perc.pa < 0 & baselines.full$perc.pa < 0, "Agree",
                                                    ifelse(baselines.full$diff.perc.pa == 0 & baselines.full$perc.pa == 0, "Agree",
                                                           "Disagree"))))

summary(baselines.full$agreement)

proportion.agree.full <- summary(baselines.full$agreement)[[1]]/nrow(baselines.full)



## reports

# biggest range of differences, and scale of differences (orders of magnitude)
# related to duration - funnel plot


figs3a <- ggplot(baselines.full, aes(x = duration, y = perc.pa))+
  geom_point()+
  scale_x_continuous(limits = c(0,55), breaks = seq(0,50,10))+
  scale_y_continuous(limits = c(-15,45), breaks = seq(-10,40,10))+
  labs(y = "% change per year in biomass,\nlinear model" ,
       x = "Duration of dataset (years)") +
  geom_hline(aes(yintercept = 0))+
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")



figs3a

ggsave("Checked/Figures/Panels/FigS3a.svg", plot = figs3a, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)




figs3b <- ggplot(baselines.full, aes(x = duration, y = diff.perc.pa))+
  geom_point()+
  scale_x_continuous(limits = c(0,55), breaks = seq(0,50,10))+
  scale_y_continuous(limits = c(-15,45), breaks = seq(-10,40,10))+
  labs(y = "% change per year in biomass,\ntwo-sample" ,
       x = "Duration of dataset (years)") +
  geom_hline(aes(yintercept = 0))+
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")

figs3b

ggsave("Checked/Figures/Panels/FigS3b.svg", plot = figs3b, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)




# frequency of disagreement in *direction of effect* between linear trend and point-to-point - 
# scatterplot should show this nicely


figs3e <- ggplot(baselines.full, aes(x = diff.perc.pa, y = perc.pa, colour = agreement))+
  geom_point()+
  scale_colour_manual(values = c("royalblue","goldenrod"))+
  labs(y = "% change per year in biomass,\nlinear model",
       x = "% change per year in biomass,\ntwo-sample")+
  scale_x_continuous(limits = c(-15,45), breaks = seq(-10,40,10))+
  scale_y_continuous(limits = c(-15,45), breaks = seq(-10,40,10))+
  geom_hline(aes(yintercept = 0))+
  geom_vline(aes(xintercept = 0))+
  geom_abline(aes(slope = 1, intercept = 0), linetype = "dotted")+
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")


figs3e


ggsave("Checked/Figures/Panels/FigS3e.svg", plot = figs3e, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)




## now repeat these analyses using each trap's data individually

baselines.trap <- data.frame()

for (trap in trap.list){
  print(trap)
  
  trap.data <- alltraps.no0[which(alltraps.no0$trap == trap), ]
  
  available.years <- levels(droplevels(as.factor(trap.data$YEAR)))
  
  first.year <- min(available.years)
  last.year <- max(available.years)
  
  start <- c(first.year:(as.numeric(last.year) - 1))
  end <- c((as.numeric(first.year) + 1):last.year)
  
  # now construct the loop


  for (x in start){
    print(x)
  for (y in end){
  
    
    
    # first check both years are in the available years (i.e. they aren't missing years)
    
    if (x %in% available.years & y %in% available.years){
      
    # test duration and keep only combinations of at least 5 years
    duration <- y - x
    
    if (duration >= 5){
      
      
      # pull out relevant data
      years <- c(x:y)
      
      years.dat <- trap.data[which(trap.data$YEAR %in% years), ]
      
      
      # test at least 5 years of actual data (i.e. not bridging a gap)
      
      if (nrow(years.dat) >= 5){
      
      # difference the first and last years
      first <- mean(years.dat[which(years.dat$YEAR == x),'inopbiomass'])
      last <- mean(years.dat[which(years.dat$YEAR == y),'inopbiomass'])
      
      diff.pa <- (last-first)/duration
      
      
      # calculate this difference as log-odds of change p.a.
      diff.lo <- (log(last) - log(first))/duration
      
      # and convert this into perc change pa
      diff.perc.pa <- 100*(exp(diff.lo)-1)
      
      
      
      
      # calculate model slope
      model.yearsNB <- glm.nb(round(inopbiomass) ~ YEAR,
                              link = "log",
                              data = years.dat) 
      
      
      slope.pa <- summary(model.yearsNB)$coefficients[2,1]
      sig.slope.pa <- drop1(model.yearsNB, test = "F")[2,5]
      
      
      # convert model slopes into perc change pa
      perc.pa <- 100*(exp(slope.pa)-1)
      
      
      out <- cbind(trap,x,y,duration,first,last,diff.pa,diff.perc.pa,slope.pa,sig.slope.pa,perc.pa)
      
      baselines.trap <- rbind(baselines.trap,out)
    }
  }
}
}
}
}

baselines.trap$diff.pa <- as.numeric(as.character(baselines.trap$diff.pa))
baselines.trap$perc.pa <- as.numeric(as.character(baselines.trap$perc.pa))
baselines.trap$diff.perc.pa <- as.numeric(as.character(baselines.trap$diff.perc.pa))

baselines.trap$agreement <- as.factor(ifelse(baselines.trap$diff.perc.pa > 0 & baselines.trap$perc.pa > 0, "Agree",
                                             ifelse(baselines.trap$diff.perc.pa < 0 & baselines.trap$perc.pa < 0, "Agree",
                                                    ifelse(baselines.trap$diff.perc.pa == 0 & baselines.trap$perc.pa == 0, "Agree",
                                                           "Disagree"))))


# this took a while so write and read it
write.csv(baselines.trap, "Checked/baselines_trap.csv", row.names = F)
baselines.trap <- read.csv("Checked/baselines_trap.csv", header = T)

summary(baselines.trap)


# check whether direction of effect is consistent between linear model and point-to-point sample


summary(baselines.trap$agreement)

proportion.agree.trap <- summary(baselines.trap$agreement)[[1]]/nrow(baselines.trap)



## reports

# biggest range of differences, and scale of differences (orders of magnitude)
# related to duration - funnel plot


figs3c <- ggplot(baselines.trap, aes(x = duration, y = perc.pa))+
  geom_point(alpha = 0.05)+
  scale_x_continuous(limits = c(0,55), breaks = seq(0,50,10))+
  scale_y_continuous(limits = c(-50,85), breaks = seq(-50,75,25))+
  labs(y = "% change per year in biomass,\nlinear model" ,
       x = "Duration of dataset (years)") +
  geom_hline(aes(yintercept = 0))+
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")



figs3c



ggsave("Checked/Figures/Panels/FigS3c.svg", plot = figs3c, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)




figs3d <- ggplot(baselines.trap, aes(x = duration, y = diff.perc.pa))+
  geom_point(alpha = 0.05)+
  scale_x_continuous(limits = c(0,55), breaks = seq(0,50,10))+
  scale_y_continuous(limits = c(-50,85), breaks = seq(-50,75,25))+
  labs(y = "% change per year in biomass,\ntwo-sample" ,
       x = "Duration of dataset (years)") +
  geom_hline(aes(yintercept = 0))+
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")

figs3d


ggsave("Checked/Figures/Panels/FigS3d.svg", plot = figs3d, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)


# frequency of disagreement in *direction of effect* between linear trend and point-to-point - 
# scatterplot should show this nicely


figs3f <- ggplot(baselines.trap, aes(x = diff.perc.pa, y = perc.pa, colour = agreement))+
  geom_point(alpha = 0.05)+
  scale_colour_manual(values = c("royalblue","goldenrod"))+
  labs(y = "% change per year in biomass,\nlinear model",
       x = "% change per year in biomass,\ntwo-sample")+
  scale_x_continuous(limits = c(-50,85), breaks = seq(-50,75,25))+
  scale_y_continuous(limits = c(-50,85), breaks = seq(-50,75,25))+
  geom_hline(aes(yintercept = 0))+
  geom_vline(aes(xintercept = 0))+
  geom_abline(aes(slope = 1, intercept = 0), linetype = "dotted")+
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")


figs3f



ggsave("Checked/Figures/Panels/FigS3f.svg", plot = figs3f, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)




## construct a multiplot figure


figs3 <- grid.arrange(figs3a,figs3c + ylab(" "),
                      figs3b,figs3d + ylab(" "),
                      figs3e,figs3f + ylab(" "),
                      ncol = 2)
                      
                      
                      
                      
ggsave("Checked/Figures/FigS3.svg", plot = figs3, device = svg,
       width = 350, height = 350, units = "mm", limitsize = T)



## for the subset of these datasets that were of 20 years' duration:

baselines.full.20 <- baselines.full[which(baselines.full$duration == 20), ]
baselines.trap.20 <- baselines.trap[which(baselines.trap$duration == 20), ]

# influence of start year

figs4a <- ggplot(baselines.full.20, aes(x = x, y = perc.pa))+
  geom_line(data = baselines.trap.20, aes(x = x, y = perc.pa, group = trap), colour = "grey80")+
  geom_line(size = 1)+
  scale_x_continuous(limits = c(1965,2000))+
  scale_y_continuous(limits = c(-10,15))+
  labs(y = "% change per year in biomass,\nlinear model" ,
       x = "First year of dataset") +
  geom_hline(aes(yintercept = 0))+
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")



figs4a


ggsave("Checked/Figures/Panels/FigS4a.svg", plot = figs4a, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)



figs4b <- ggplot(baselines.full.20, aes(x = x, y = diff.perc.pa))+
  geom_line(data = baselines.trap.20, aes(x = x, y = diff.perc.pa, group = trap), colour = "grey80")+
  geom_line(size = 1)+
  scale_x_continuous(limits = c(1965,2000))+
  scale_y_continuous(limits = c(-10,15))+
  labs(y = "% change per year in biomass,\ntwo-sample" ,
       x = "First year of dataset") +
  geom_hline(aes(yintercept = 0))+
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")



figs4b



ggsave("Checked/Figures/Panels/FigS4b.svg", plot = figs4b, device = svg,
       width = 240, height = 160, units = "mm", limitsize = T)





gA <- ggplotGrob(figs4a)
gB <- ggplotGrob(figs4b)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
figs4 <- grid.arrange(gA, gB, ncol=1)



ggsave("Checked/Figures/FigS4.svg", plot = figs4, device = svg,
       width = 200, height = 200, units = "mm", limitsize = T)




### how much does raw biomass vary across years?

# full data
min(averageall$inopbiomass)
max(averageall$inopbiomass)

max(averageall$inopbiomass)/min(averageall$inopbiomass)


minmax <- ddply(alltraps.no0, .(trap), summarise,
                min.biomass = min(inopbiomass),
                max.biomass = max(inopbiomass))


minmax$factor <- minmax$max.biomass/minmax$min.biomass


## redo these splitting by decades
averageall$decade <- as.factor(ifelse(averageall$YEAR %in% 1960:1969, "1960s",
                            ifelse(averageall$YEAR %in% 1970:1979, "1970s",
                                   ifelse(averageall$YEAR %in% 1980:1989, "1980s",
                                          ifelse(averageall$YEAR %in% 1990:1999, "1990s",
                                                 ifelse(averageall$YEAR %in% 2000:2009, "2000s",
                                                        ifelse(averageall$YEAR %in% 2010:2019, "2010s","fail")))))))


minmax.decades <- ddply(averageall, .(decade), summarise,
                        min.biomass = min(inopbiomass),
                        max.biomass = max(inopbiomass))

minmax.decades$factor <- minmax.decades$max.biomass/minmax.decades$min.biomass


# and individual traps
alltraps.no0.backup <- alltraps.no0

alltraps.no0.backup$decade <- as.factor(ifelse(alltraps.no0.backup$YEAR %in% 1960:1969, "1960s",
                                      ifelse(alltraps.no0.backup$YEAR %in% 1970:1979, "1970s",
                                             ifelse(alltraps.no0.backup$YEAR %in% 1980:1989, "1980s",
                                                    ifelse(alltraps.no0.backup$YEAR %in% 1990:1999, "1990s",
                                                           ifelse(alltraps.no0.backup$YEAR %in% 2000:2009, "2000s",
                                                                  ifelse(alltraps.no0.backup$YEAR %in% 2010:2019, "2010s","fail")))))))

minmax.decades.traps <- ddply(alltraps.no0.backup, .(decade,trap), summarise,
                        min.biomass = min(inopbiomass),
                        max.biomass = max(inopbiomass))

minmax.decades.traps$factor <- minmax.decades.traps$max.biomass/minmax.decades.traps$min.biomass





#### fourier analysis ####
#### dynamic relationships

# try relationships of biomass to temperature and rainfall as time-series

ts.biomass <- ts(change.average.all$mean.change.inopbiomass, frequency = 1, start = 1967)

plot.ts(ts.biomass)

# we need a package that has been removed from CRAN but is still in the archive

#url <- "https://cran.r-project.org/src/contrib/Archive/BaSAR/BaSAR_1.3.tar.gz"
#pkgFile <- "BaSAR_1.3.tar.gz"
#download.file(url = url, destfile = pkgFile)

# install dependencies

#install.packages("orthopolynom")


# Install package

#install.packages(pkgs = pkgFile, type = "source", repos = NULL)


# delete package tarball
#unlink(pkgFile)

# read in package
library(BaSAR)


# first let's seek the angular frequency of any oscillation

test <- BaSAR.post(change.average.all$mean.change.inopbiomass, start = 2, stop = 20, 10000, 0, change.average.all$year)

plot(test$omega, test$normp, type="h", col="red", ylab = "PDF", xlab = expression(omega))

test$stats

omega <- test$stats[[2]]
omega.sd <- test$stats[[3]]

omega.lci <- omega - (1.96*omega.sd)
omega.uci <- omega + (1.96*omega.sd)

cycles.pa <- omega / (2 * pi)
cycles.pa.lci <- omega.lci / (2 * pi)
cycles.pa.uci <- omega.uci / (2 * pi)


years.per.cycle <- 1/cycles.pa
years.per.cycle.lci <- 1/cycles.pa.lci
years.per.cycle.uci <- 1/cycles.pa.uci



## now put this posterior distribution into a proper ggplot


test$cycles.pa <- test$omega / (2 * pi)
test$years.per.cycle <- 1/test$cycles.pa

plot(test$years.per.cycle, test$normp, type="h", col="red", ylab = "PDF", xlab = "Years per cycle")


fourier <- data.frame(cbind(test$normp, test$years.per.cycle))
colnames(fourier) <- c("normp","years.per.cycle")

figs5 <- ggplot(fourier, aes(x = years.per.cycle, y = normp))+
  geom_area()+
  scale_x_continuous(breaks = seq(2,20,2))+
  labs(x = "Years per cycle",
       y = "Posterior distribution function")+
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = "none")

figs5


ggsave("Checked/Figures/FigS5.svg", plot = figs5, device = svg,
       width = 150, height = 80, units = "mm", limitsize = T)


## and hack out the peaks

fourier$peak[[1]] <- F

for (n in 2:9999){
  fourier$peak[[n]] <- ifelse(fourier$normp[[n]] > fourier$normp[[n-1]] & fourier$normp[[n]] > fourier$normp[[n+1]],
                              T,F)
}


fourier.peaks <- fourier[which(fourier$peak == T), ]


###### development code - unused ####

#### dynamic relationships

# try relationships of biomass to temperature and rainfall as time-series

ts.biomass <- ts(change.average.all$mean.change.inopbiomass, frequency = 1, start = 1967)

plot.ts(ts.biomass)


ts.biomass.raw <- ts(averageall$inopbiomass, frequency = 1, start = 1967)
plot.ts(ts.biomass.raw)

log.ts <- log(ts.biomass.raw)
plot.ts(log.ts)


## autocorrelation
acf.biomass <- acf(ts.biomass.raw)
pacf.biomass <- pacf(ts.biomass.raw)

acf.log.biomass <- acf(log.ts)
pacf.log.biomass <- pacf(log.ts)


# turn temp and rain into time series too

ts.temp <- ts(change.average.all$mean.monthly.temp, frequency = 1, start = 1967)
ts.rain <- ts(change.average.all$mean.annual.rain, frequency = 1, start = 1967)

plot.ts(cbind(ts.biomass.raw,log.ts,ts.temp,ts.rain))


# cross-correlation
ccf.temp <- ccf(ts.biomass.raw,ts.temp)
ccf.rain <- ccf(ts.biomass.raw,ts.rain)

ccf.log.temp <- ccf(log.ts,ts.temp)
ccf.log.rain <- ccf(log.ts,ts.rain)

# test if stationary
adf.test(ts.biomass) # NOT stationary
kpss.test(ts.biomass)

adf.test(log.ts) # stationary
kpss.test(log.ts)





