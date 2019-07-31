###############################################################################
### WE WANT TO COMBINE THE YEARS OF OPERATION OF THE TRAPS TO THE DATA FILES###
###############################################################################

#AIMS#
#1 NEED TO ADD THE YEARS OF OPERATION OF EACH TRAP TO THE CSV FILES
#2 WE WANT TO MAKE A FREQUENCY DISTRIBUTION DIAGRAM TO UNDERSTAND WHICH TRAPS ARE USED FOR THE LONGEST TIMES

rm(list=ls())


j <- c("rstudioapi","plyr","vegan","lme4","ggplot2","RColorBrewer","car","gridExtra","MuMIn","RVAideMemoire","emmeans","reshape2","moments","dplyr","ggmap","blighty","grid","egg")

new.packages <- j[!(j %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(j, require, character.only = TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

op <- read.csv(header = TRUE, 'trap_operative_years.csv')
op

#Lets get rid of all the unnecesary columns
#we are left with a file that has just the site name and the years of operation
op_subset = op[,c(1,2,10,11,12,13,14,15,16,17)]
op_subset
write.csv(op_subset, file="traptime.csv", row.names =FALSE)

#LETS REPLACE THE 'NAs' WITH 0s SO WE CAN WORK OUT HOW LONG THE TRAPS HAVE BEEN RUNNING
time <- read.csv(header = TRUE, 'traptime.csv')
time[is.na(time)] <- 0
write.csv(time, file = "traptime0.csv", row.names =FALSE)


# NOW LETS REMOVE emptty spaces and replace them with '0'
time2 <- read.csv(header = TRUE, 'traptime0.csv', stringsAsFactors = FALSE)
time2
time2[time2==""] <- 0
write.csv(time2, file = "traptime1.csv", row.names =FALSE)


###SO NOW WE WANT TO KNOW HOW LONG EACH TRAP HAS BEEN RUNNING###
##CAN DO THIS BY TAKING THE START DATES AWAY FROM THE STOP DATES##

length <- read.csv(header = TRUE, 'traptime1.csv', stringsAsFactors = FALSE)
length

#make new columns for length of time - call it 'Duration'

length$Duration <- (length$Stop1 - length$Start1 + 1)
length$Duration2 <- (length$Stop2 - length$Start2 + 1)
length$Duration3 <- (length$Stop3 - length$Start3 + 1)
length$Duration4 <- (length$Stop4 - length$Start4 + 1)




for (n in 1:nrow(length)){

length[n,'Longest'] <- max((length[n,'Duration']),(length[n,'Duration2']),(length[n,'Duration3']),(length[n,'Duration4']))

}

write.csv(length, file = "duration.csv", row.names = FALSE)



###NOW LETS COMBINE SITE TIMES THAT ARE ONLY A FEW YEARS APART - WE CAN CONSIDER THEM AS THE SAME RUN OF DATA COLLECTION###
#CREATE A NEW COLUMN THAT WILL HOLD THE NEW LONGEST TIMES

length$Interval <- (length$Start2 - length$Stop1 -1)
length$Interval2 <- (length$Start3 - length$Stop2 -1)
length$Interval3 <- (length$Start4 - length$Stop3 -1)

length$Join <- 0
length$Join1 <- 0
length$Join2 <- 0




length$Join <- ifelse(length$Interval>0 & length$Interval <=5, 1,0)
                      
length$Join1 <- ifelse(length$Interval2>0 & length$Interval2 <=5, 2,0)
                             
length$Join2 <- ifelse(length$Interval3>0 & length$Interval3 <=5,3,0)   

#NOW WE HAVE 3 COLUMNS THAT TELL US WHETHER THE INTERVALS BETWEEN TIME FRAMES ARE LESS THAN 3 YEARS
###NOW WE NEED TO MAKE A COLUMN FOR THE REAL LONGEST RUN###

length$Grouped <- ifelse(length$Join == '1' & length$Join1 == '2' & length$Join2 == '3', length$Duration + length$Duration2 + length$Duration3 + length$Duration4, 
                           ifelse(length$Join == '1' & length$Join1 == '2', length$Duration + length$Duration2 + length$Duration3,
                                  ifelse(length$Join1 == '2' & length$Join2 == '3', length$Duration2 + length$Duration3 + length$Duration4,
                                         ifelse(length$Join == '1' , length$Duration + length$Duration2,
                                                ifelse(length$Join1 == '2', length$Duration2 + length$Duration3,
                                                       ifelse(length$Join2 == '3', length$Duration3 + length$Duration4, length$Longest))))))

### SO NOW IN THE COLUMN 'GROUPED' WE HAVE THE LONGEST DURATION OF DATA COLLECTION AT EACH TRAP
# USING DATA FROM TRAPS THAT HAD A GAP IN RECORDING OF 3 OR FEWER YEARS###
colnames(length)[colnames(length)=="grouped"] <- "Grouped"


hist(length$Grouped)




### NOW WE WANT TO  KNOW THE LENGTH OF TIME THE TRAPS WERE RUNNING FOR - CALL THIS 'LONGEST DURATION
# HAVE TO INCORPORATE THE TIME GAPS I.E. THE LONGEST DURATION WILL BE THE DURATIONS + THE INTERVALS

length$Interval[length$Interval<0 ] <- 0
length$Interval2[length$Interval2<0 ] <- 0
length$Interval3[length$Interval3<0 ] <- 0

length$LongestDuration <- ifelse(length$Join == '1' & length$Join1 == '2' & length$Join2 == '3', length$Duration + length$Duration2 + length$Duration3 + length$Duration4 + length$Interval + length$Interval2 + length$Interval3, 
                                ifelse(length$Join == '1' & length$Join1 == '2', length$Duration + length$Duration2 + length$Duration3 +length$Interval + length$Interval2,
                                      ifelse(length$Join1 == '2' & length$Join2 == '3', length$Duration2 + length$Duration3 + length$Duration4 + length$Interval2 + length$Interval3,
                                            ifelse(length$Join == '1' , length$Duration + length$Duration2 + length$Interval,
                                                  ifelse(length$Join1 == '2', length$Duration2 + length$Duration3 + length$Interval2,
                                                         ifelse(length$Join2 == '3', length$Duration3 + length$Duration4 + length$Interval3, length$Longest))))))

hist(length$LongestDuration)
kernel <- density(length$LongestDuration) 
plot(kernel)   

length$durationgroup <- ifelse(length$LongestDuration >= 60, '60+',
                               ifelse(length$LongestDuration >= 50, '50-59',
                               ifelse(length$LongestDuration >= 40, '40-49',
                                      ifelse(length$LongestDuration >= 30, '30-39',
                                             ifelse(length$LongestDuration >= 20, '20-29',
                                                    ifelse(length$LongestDuration >= 10, '10-19',
                                                           ifelse(length$LongestDuration >= 5,'5-9','0-4')))))))


write.csv(length, file = "length_model1.csv", row.names = FALSE,)


#### now try a slightly modified approach ####

# can get rid of the longest durations and the duration groups from the first model


length <- length[c(1:21)]
length


### now we want to join the durations that have an interval lower or equal to them

length$Join <- 0
length$Join1 <- 0
length$Join2 <- 0

length$Join <- ifelse(length$Interval<= length$Duration & length$Interval <=length$Duration2 & length$Start2 !=0, 1,0)
length$Join1 <- ifelse(length$Interval2<= length$Duration2 & length$Interval2 <=length$Duration3 & length$Start3 !=0, 2,0)
length$Join2 <- ifelse(length$Interval3<= length$Duration3 & length$Interval3 <=length$Duration4 & length$Start4 !=0, 3,0)

## SO NOW LETS JOIN THE DURATIONS OF THE TRAPS THAT FIT THIS MODEL

length$Grouped <- ifelse(length$Join == '1' & length$Join1 == '2' & length$Join2 == '3', length$Duration + length$Duration2 + length$Duration3 + length$Duration4, 
                         ifelse(length$Join == '1' & length$Join1 == '2', length$Duration + length$Duration2 + length$Duration3,
                                ifelse(length$Join1 == '2' & length$Join2 == '3', length$Duration2 + length$Duration3 + length$Duration4,
                                       ifelse(length$Join == '1' , length$Duration + length$Duration2,
                                              ifelse(length$Join1 == '2', length$Duration2 + length$Duration3,
                                                     ifelse(length$Join2 == '3', length$Duration3 + length$Duration4, length$Longest))))))

###THE COLUMN 'GROUPED' HAS THE LONGEST TIME OF 'CONTINUOUS' DATA COLLECTION AT EACH TRAP

hist(length$Grouped)


###ABOVE IS THE DURATION THE TRAPS WERE RUNNING FOR, NOT INCORPORATING THE TIME GAPS
##LETS INCLUDE THE TIME GAPS - THIS IS MORE ROBUST AS WE CAN SAY 'THIS TRAP WAS RUNNING FROM 1660-1998' RATHER THAN SAYING '1960-1974-1998'

length$LongestDuration <- ifelse(length$Join == '1' & length$Join1 == '2' & length$Join2 == '3', length$Duration + length$Duration2 + length$Duration3 + length$Duration4 + length$Interval + length$Interval2 + length$Interval3, 
                                 ifelse(length$Join == '1' & length$Join1 == '2', length$Duration + length$Duration2 + length$Duration3 +length$Interval + length$Interval2,
                                        ifelse(length$Join1 == '2' & length$Join2 == '3', length$Duration2 + length$Duration3 + length$Duration4 + length$Interval2 + length$Interval3,
                                               ifelse(length$Join == '1' , length$Duration + length$Duration2 + length$Interval,
                                                      ifelse(length$Join1 == '2', length$Duration2 + length$Duration3 + length$Interval2,
                                                             ifelse(length$Join2 == '3', length$Duration3 + length$Duration4 + length$Interval3, length$Longest))))))

hist(length$LongestDuration)
kernel <- density(length$LongestDuration) 
plot(kernel)

# IT HAS PRETTY MUCH THE SAME DENSITY DISTRIBUTION AS MODEL 1

length$durationgroup <- ifelse(length$LongestDuration >= 60, ' 60+ ',
                               ifelse(length$LongestDuration >= 50, ' 50-59 ',
                                      ifelse(length$LongestDuration >= 40, ' 40-49 ',
                                             ifelse(length$LongestDuration >= 30, ' 30-39 ',
                                                    ifelse(length$LongestDuration >= 20, ' 20-29 ',
                                                           ifelse(length$LongestDuration >= 10, ' 10-19 ',
                                                                  ifelse(length$LongestDuration >= 5,' 5-9 ',' 0-4 ')))))))



a <- table(length$durationgroup)
a   

write.csv(length, file = "model2.csv", row.names = FALSE)

