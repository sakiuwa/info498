library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)

########################################################################################################################################
################## THE CODE HERE WAS USED TO PRIMARILY AGGREGATE THE DATA AND TO EXPORT ################################################
################## THEM AS CSVs SO THAT THEY CAN BE READ IN WHEN CREATING THE PLOTS     ################################################
########################################################################################################################################

############## IRIS' DEMO DATA #################################

# data <- read.csv("../data/tab1-data.csv")
# weights <- read.csv("../data/weights.csv")
# data <- left_join(data, weights, by = "CASEID")
# data$X <- NULL
# 
# data$NEWRACE2[data$NEWRACE2 == 1] <- "White"
# data$NEWRACE2[data$NEWRACE2 == 2] <- "Black/Afr Am"
# data$NEWRACE2[data$NEWRACE2 == 3] <- "Native Am/AK Native "
# data$NEWRACE2[data$NEWRACE2 == 4] <- "Native HI/Other Pac Isl"
# data$NEWRACE2[data$NEWRACE2 == 5] <- "Asian"
# data$NEWRACE2[data$NEWRACE2 == 6] <- "More than one race"
# data$NEWRACE2[data$NEWRACE2 == 7] <- "Hispanic"
# 
# data$IRSEX[data$IRSEX == 1] <- "Male"
# data$IRSEX[data$IRSEX == 2] <- "Female"
# 
# data$CATAG6[data$CATAG6 == 1] <- "12-17 Years Old"
# data$CATAG6[data$CATAG6 == 2] <- "18-25 Years Old"
# data$CATAG6[data$CATAG6 == 3] <- "26-34 Years Old"
# data$CATAG6[data$CATAG6 == 4] <- "35-49 Years Old"
# data$CATAG6[data$CATAG6 == 5] <- "50-64 Years Old"
# data$CATAG6[data$CATAG6 == 6] <- "65 or Older"
# 
# data.user <- data %>% filter(MJEVER == 1)
# data.nonuser <- data %>% filter(MJEVER == 2)
# 
# sd.user <- svydesign(id=~CASEID, strata=~VESTR, weights=~ANALWT_C, data=data.user)
# sd.nonuser <- svydesign(id=~CASEID, strata=~VESTR, weights=~ANALWT_C, data=data.nonuser)
# 
# newrace2.user <- svytable(~NEWRACE2, design=sd.user)
# irsex.user <- svytable(~IRSEX, design=sd.user)
# catag6.user <- svytable(~CATAG6, design=sd.user)
# 
# newrace2.nonuser <- svytable(~NEWRACE2, design=sd.nonuser)
# irsex.nonuser <- svytable(~IRSEX, design=sd.nonuser)
# catag6.nonuser <- svytable(~CATAG6, design=sd.nonuser)
# 
# newrace2.user <- as.data.frame(newrace2.user)
# newrace2.nonuser <- as.data.frame(newrace2.nonuser)
# newrace2.user[, "Usage"] <- 1
# newrace2.nonuser[, "Usage"] <- 2
# newrace2 <- bind_rows(newrace2.user, newrace2.nonuser, id = NULL)
# write.csv(newrace2, file = "newrace2.csv")
# 
# irsex.user <- as.data.frame(irsex.user)
# irsex.nonuser <- as.data.frame(irsex.nonuser)
# irsex.user[, "Usage"] <- 1
# irsex.nonuser[, "Usage"] <- 2
# irsex <- bind_rows(irsex.user, irsex.nonuser, id = NULL)
# write.csv(irsex, file = "irsex.csv")
# 
# catag6.user <- as.data.frame(catag6.user)
# catag6.nonuser <- as.data.frame(catag6.nonuser)
# catag6.user[, "Usage"] <- 1
# catag6.nonuser[, "Usage"] <- 2
# catag6 <- bind_rows(catag6.user, catag6.nonuser, id = NULL)
# write.csv(catag6, file = "catag6.csv")

irsex <- read.csv("data/irsex.csv")
irsex[1] <- NULL
newrace2 <- read.csv("data/newrace2.csv")
newrace2[1] <- NULL
catag6 <- read.csv("data/catag6.csv")
catag6[1] <- NULL

############## LANDON'S DEMO DATA #######################

# Data Prep:
# mjdata <- read.csv("../data/mjdata.csv")
# demographics <- read.csv("../data/demo.csv")
# weights <- read.csv("../data/weights.csv")
# demo.weights <- left_join(demographics, weights, by = "CASEID")
# 
# demo <- select(demo.weights, CASEID, NEWRACE2, IRSEX, CATAG6, ANALWT_C, VESTR)
# demo$NEWRACE2 <- factor(demo$NEWRACE2, 
#                         levels=c(1:7),
#                         labels=c("(1) White", "(2) Black", "(3) Native Am", "(4) Pac. Isl.", "(5) Asian", "(6) NonHisp >1 Race", "(7) Hispanic"))
# demo$CATAG6 <- factor(demo$CATAG6,
#                       levels= c(1:6),
#                       labels=c("(1) 12-17", "(2) 18-25", "(3) 26-34", "(4) 35-49", "(5) 50-64", "(6) 65+"))
# 
# users.only <- filter(mjdata, MJEVER == "(1) Yes") %>% select(CASEID, MJAGE, MJREC, MJDAY30A)
# users.only <- left_join(users.only, demo, by = "CASEID")
# users.only$MJREC <- factor(users.only$MJREC,
#                            levels=c("(01) Within the past 30 days", "(02) More than 30 days ago but within the past 12 mos",
#                                     "(03) More than 12 months ago",                         
#                                     "(08) Used at some point in the past 12 mos LOG ASSN",  
#                                     "(09) Used at some point in the lifetime LOG ASSN",     
#                                     "(11) Used in the past 30 days LOGICALLY ASSIGNED"),
#                            labels=c("(01, 11) Past 30 days", "(02, 08) >30days, but in past 12mos", 
#                                     "(03) > 12 months", "(02, 08) >30days, but in past 12mos", 
#                                     "(09) Some point in lifetime", "(01, 11) Past 30 days"))

# Read in data necessary for this tab
users.only <- read.csv("data/tab2-data.csv")