##############################################################
### WE HAVE A NEW WAY TO ESTIAMTE THE BIOMASS OF THE MOTHS ###
##############################################################

# THIS TAKES THE LOG OF THE DRY MASS AND INCORPORATES ITS STANDARD ERROR
# ALLOWING US TO HAVE SE BARS ON OUR GRAPHS

#AIMS
#1. ADD THE NEW MASS ESTIMATES TO THE MOTH DATA WE HAVE (1980-2016)
#2. NEED TO SEPARATE THE DATA SO EACH INDIVIDUAL MOTH HAS IT'S OWN ROW AND THEN IT CAN BE BOOTSTRAPED

rm(list=ls())

j <- c("rstudioapi","plyr","vegan","lme4","ggplot2","RColorBrewer","car","gridExtra","MuMIn","RVAideMemoire","emmeans","reshape2","moments","dplyr","ggmap","blighty","grid","egg")

new.packages <- j[!(j %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(j, require, character.only = TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

newdat <- read.csv("Checked/TableS3.csv", header = TRUE)

# THE COLUMN 'SPECIES' MATCHES THE COLUMN 'binomial' FROM THE RIS DATA
names(newdat)[5] <- paste("binomial")

# we want to predict the body mass of each moth in the RIS dataset using a normal distribution defined by the estimated dry mass and the standard error of that estimate
# to achieve this we need to melt down the dataset so that there is one row per individual moth
# (rather than one row per species, with a column for abundance)

year.list <- c(1933:1937,1946:1950,1964:2017)
  
year.list

for (year in year.list){
  print(year)

  individuals <- data.frame(INDIVIDUAL = numeric(),
                            BINOMIAL = factor(),
                            TRAPNAME = factor(),
                            YEAR = numeric(),
                            FAMILY = factor(),
                            DRY_MASS = numeric(),
                            log_DRY_MASS = numeric(),
                            SE = numeric(),
                            COUNT = numeric())
  
  data <- read.csv(paste0("data transfer for chris and callum/Biomass calculations/RIS_raw/AllSitesAllMoths ",year,".csv"),header=T)
  data$Year <- year
  
  yearly <- ddply(data, .(Year,TrapName,binomial), summarise,
                  Count = sum(DailyCount))
  newdatcom <- ddply(newdat, .(binomial,FAMILY,DRY_MASS), summarise,
                     log_DRY_MASS = mean(log_DRY_MASS), SE = mean(SE))
  
  yearly.mass <- merge(yearly,newdatcom)
  
for (n in 1:nrow(yearly.mass)){
  row <- yearly.mass[n,]
  no.indivs <- row$Count[[1]]
  indivs <- data.frame(INDIVIDUAL = numeric(no.indivs))
  indivs$INDIVIDUAL <- c(1:no.indivs)
  indivs$BINOMIAL <- row$binomial[[1]]
  indivs$TRAPNAME <- row$TrapName[[1]]
  indivs$YEAR <- row$Year[[1]]
  indivs$RIS_SPECIES <- row$RIS_SPECIES[[1]]
  indivs$FAMILY <- row$FAMILY[[1]]
  indivs$NAME <- row$NAME[[1]]
  indivs$FOREWING_LB <- row$FOREWING_LB[[1]]
  indivs$FOREWING_MED <- row$FOREWING_MED[[1]]
  indivs$FOREWING_UB <- row$FOREWING_UB[[1]]
  indivs$DRY_MASS <- row$DRY_MASS[[1]]
  indivs$log_DRY_MASS <- row$log_DRY_MASS[[1]]
  indivs$SE <- row$SE[[1]]
  indivs$COUNT <- 1
  
  individuals <- rbind(individuals, indivs)
}
  write.csv(individuals,file = paste0("data transfer for chris and callum/Biomass calculations/individuals/individuals",year,".csv"), row.names = F)
}



# to garner an estimate of the error around our estimates of sample-level biomass, we want to resample the *sample-level biomass* of each community multiple times
# that means, for each year's worth of data, repeatedly estimating the mass of each individual moth and summing the annual estimate
# then storing all these annual estimates, and measuring their mean and error



# Lets also add the trap running durations to each file
model2 <- read.csv("Years of operation/Model 2/model2.csv", header=T)
mod2 <- model2[,c(2,23,24)]


#GOING TO RUN THE LOOP FOR 5 ESTIMATE ITERATIONS SO I CAN GET ON WITH SOME ACTUAL ANALYSIS
for (year in year.list){
  print(year)
  
  
  yearly.biomass <- data.frame(YEAR = numeric(),
                               trap = factor(),
                               EST.BIOMASS = numeric(),
                               EST.BIOMASS.SE = numeric())
  trapsyearly.biomass <- data.frame(YEAR = numeric(),
                                    trap = factor(),
                               EST.BIOMASS = numeric(),
                               EST.BIOMASS.SE = numeric())
  
  #Extract each years individuals
  year.indivs <- read.csv(paste0("data transfer for chris and callum/Biomass calculations/individuals/individuals",year,".csv"), header=T)
  
  tryCatch({
  revalue(year.indivs$TRAPNAME, c("Brooms' Barn L" = "Brooms Barn")) -> year.indivs$TRAPNAME
  })
  
  # now generate 1000 independent estimates of the yearly biomass - each based on separate estimates of each moth's body mass
  
  traps.list <- levels(droplevels(year.indivs$TRAPNAME))
  
  for (trap in traps.list){
    trap.indivs <- year.indivs[which(year.indivs$TRAPNAME == trap), ]
    print(trap)
    trap.biomass <- NULL  # seed an output vector
  for (x in 1:1000){
    
    if(round(x, -2) == x){
      print(x)
    }
    
    
    for (n in 1:nrow(trap.indivs)){
      trap.indivs$PRED_DRY_MASS[n] <- exp(rnorm(1, mean = trap.indivs$log_DRY_MASS[n], sd = trap.indivs$SE[n]))
    }
    
   biomass.estimate <- sum(trap.indivs$PRED_DRY_MASS) #(this totals the estimates of all of the moths of the year)
   trap.biomass <- append(trap.biomass, biomass.estimate)
  }
    # now take the mean and standard error of those estimates
    
    EST.BIOMASS <- mean(trap.biomass)
    EST.BIOMASS.SE <- sd(trap.biomass)/sqrt(length(trap.biomass))
    YEAR <- year
    out <- data.frame(YEAR,trap,EST.BIOMASS,EST.BIOMASS.SE)
    trapsyearly.biomass <- rbind(trapsyearly.biomass, out)
    
    
  }  
  yearly.biomass <- rbind(yearly.biomass, trapsyearly.biomass)
  
  merged <- merge(yearly.biomass,mod2, by.x='trap', by.y= 'TrapName')
  
  write.csv(merged, file= paste0("data transfer for chris and callum/Biomass calculations/EstBiomassyearsbootstrapped/EstBiomass",year,".csv"),row.names = F)
  
}



### grouped by species



## WE READ IN THE 'INDIVIDUAL' FILES AND COMPRESS THEM TO HAVE ONE ROW PER YEAR, PER TRAP FOR EACH FAMILY

for (year in year.list){
  print(year)
  
  individuals <- read.csv(file = paste0("data transfer for chris and callum/Biomass calculations/individuals/individuals",year,".csv"), header = T)
  
  trap.list <- levels(droplevels(individuals$TRAPNAME))
  
  yearly <- data.frame(FAMILY = factor(),
                       BINOMIAL = factor(),
                       TRAPNAME = factor(),
                       DRY_MASS = numeric(),
                       countsum = numeric(),
                       YEAR = numeric())
  
  for (trap in trap.list){
    
    print(trap)
    
    familygroup <- data.frame(TRAPNAME = factor(),
                              YEAR = numeric(),
                              FAMILY = factor(),
                              NAME = factor(),
                              DRY_MASS = numeric(),
                              countsum = numeric())
    
    
    inditrap <- individuals[which(individuals$TRAPNAME == trap),]
    
    
    
    
    familygroup <- ddply(inditrap, .(FAMILY,BINOMIAL, TRAPNAME), summarise,
                         DRY_MASS = sum(exp(log_DRY_MASS)), countsum = sum(COUNT))
    
    
    familygroup$YEAR <- year
    
    yearly <- rbind(yearly,familygroup)
  }
  
  write.csv(yearly, file = paste0("data transfer for chris and callumbiomass analysis/Biomass grouped by species/species",year,".csv"), row.names=F)
  
}


