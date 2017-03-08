library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)


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

# mjdata <- read.csv("data/mjdata.csv")
# demographics <- read.csv("data/demo.csv")
# weights <- read.csv("data/weights.csv")
# demo.weights <- left_join(demographics, weights, by = "CASEID")

# Filter users of marijuana only, and select relevant columns
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

users.only <- read.csv("data/tab2-data.csv")




########### BAO ADDED ############################
# age <-group_by(users.only, MJREC, CATAG6) %>% summarise(count = n()) %>% mutate(perc = round(count/sum(count)*100, 2))  
# a1 <- age %>% filter(MJREC == "(01) Within the past 30 days") %>% select(perc)
# a1 <- c(a1$perc)
# a2 <- age %>% filter(MJREC == "(02) More than 30 days ago but within the past 12 mos")%>% select(perc)
# a2 <- c(a2$perc)
# a3 <- age %>% filter(MJREC == "(03) More than 12 months ago")%>% select(perc)
# a3 <- c(a3$perc)
# a4 <- age %>% filter(MJREC == "(08) Used at some point in the past 12 mos LOG ASSN")%>% select(perc)
# a4 <- c(a4$perc)
# a5 <- age %>% filter(MJREC == "(09) Used at some point in the lifetime LOG ASSN")%>% select(perc)
# a5 <- c(a5$perc)
# a6 <- age %>% filter(MJREC == "(11) Used in the past 30 days LOGICALLY ASSIGNED")%>% select(perc)
# a6 <- c(a6$perc)

# a.all <- c("Within past 30 days", "More than 30 days but within past 12 months", "More than 12 months", "Past 12 months (LOG)", "In lifetime (LOG)", "Past 30 days (LOG)")
# a.age <- c("12-17", "18-25", "26-34", "35-49", "50-64", "65+")
# 
# age.plot <- data.frame(t(data.frame(a1,a2,a3,a4,a5,a6)))
# age.plot$age <- a.age
# age.plot$answer <- a.all

# data <- age.plot
# plot <- plot_ly(data, x=~X1, y=~answer, name= "12-17", type="bar") %>% 
#   add_trace(x =~X2, name = "18-25", marker = list(color = '7AA27B')) %>%
#   add_trace(x =~X3, name = "26-34", marker = list(color = '9FBEA0')) %>%
#   add_trace(x =~X4, name = "35-49", marker = list(color = 'CDE0CE')) %>%
#   add_trace(x =~X5, name = "50-64", marker = list(color = 'E5F1E5')) %>% 
#   add_trace(x =~X6, name = "65+", marker = list(color = 'E5F1E5')) %>%
#   layout(title ="Breakdown by Age Group", xaxis = list(title = ""),yaxis = list(title = "Percent"),barmode="stack")
# return(plot)
