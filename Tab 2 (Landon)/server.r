### Server of second tab
### by Landon Young
### 
### This tab of the shiny app will be looking at correlations between 
### marijuana users' self-reported mental health and general health 

# Load in libraries
library(plotly)
library(ggplot2)
library(dplyr)
library(shiny)
library(survey)

# Read in marijuana and demographics data from survey

mjdata <- read.csv("../data/mjdata.csv")
demographics <- read.csv("../data/demo.csv")
weights <- read.csv("../data/weights.csv")
demo.weights <- left_join(demographics, weights, by = "CASEID")

# Filter users of marijuana only, and select relevant columns
demo <- select(demo.weights, CASEID, NEWRACE2, IRSEX, CATAG6, ANALWT_C, VESTR)
demo$NEWRACE2 <- factor(demo$NEWRACE2, 
                        levels=c(1:7),
                        labels=c("(1) White", "(2) Black", "(3) Native Am", "(4) Pac. Isl.", "(5) Asian", "(6) NonHisp >1 Race", "(7) Hispanic"))
demo$CATAG6 <- factor(demo$CATAG6,
                      levels= c(1:6),
                      labels=c("(1) 12-17", "(2) 18-25", "(3) 26-34", "(4) 35-49", "(5) 50-64", "(6) 65+"))

users.only <- filter(mjdata, MJEVER == "(1) Yes") %>% select(CASEID, MJAGE, MJREC, MJDAY30A)
users.only <- left_join(users.only, demo, by = "CASEID")

# Make groups and percentage columns
grouping <- group_by(users.only, NEWRACE2) %>% summarise(count = n(), nationalcount = sum(ANALWT_C)) %>% mutate(perc = count/sum(count), natperc = nationalcount/sum(nationalcount))





# # Make a survey design
# mjdesign <- svydesign(id=~1, strata=~VESTR, weights=~ANALWT_C , data=users.only)
# 
# # Make proportions columns
# mjage2 <- data.frame(prop.table(svytable(~MJAGE, design=mjdesign)))
# mjrec2 <- data.frame(prop.table(svytable(~MJREC, design=mjdesign)))
# mjday30a2 <- data.frame(prop.table(svytable(~MJDAY30A, design=mjdesign)))
# 
# users.only$MJAGE2 <- factor(users.only$MJAGE,
#                              levels = c(1:50, 52:56, 59:62, 64:65, 68:70, 72, 77),
#                              labels = mjage2$Freq)
# 
# users.only$MJREC2 <- factor(users.only$MJREC,
#                             levels = c("(01) Within the past 30 days", 
#                                        "(02) More than 30 days ago but within the past 12 mos",
#                                        "(03) More than 12 months ago",                      
#                                        "(08) Used at some point in the past 12 mos LOG ASSN",  
#                                        "(09) Used at some point in the lifetime LOG ASSN",     
#                                        "(11) Used in the past 30 days LOGICALLY ASSIGNED"),
#                             labels = mjrec2$Freq)
# 
# users.only$MJDAY30A2 <- factor(users.only$MJDAY30A,
#                             levels = c(1:30),
#                             labels = mjday30a2$Freq)


shinyServer(function(input, output) {
  
  # # Reactive function for demographics within MJAGE question
  # mjagedems <- reactive({
  #   if(input$demo == 'factor(CATAG6)') {
  #     temp <- group_by(users.only, MJAGE, CATAG6) %>%  summarise(count = n())
  #     return()
  #   } else if (input$demo == 'IRSEX') {
  #     temp <- group_by(users.only, MJAGE, IRSEX) %>%  summarise(count = n())
  #     return()
  #   } else {
  #     temp <- group_by(users.only, MJAGE, NEWRACE2) %>%  summarise(count = n())
  #     return(temp)
  #   }
  # }) 
  # 
  # # Reactive function for demographics within MJREC question
  # mjrecdems <- reactive({
  #   if(input$demo == 'factor(CATAG6)') {
  #     temp <- group_by(users.only, MJREC, CATAG6) %>%  summarise(count = n())
  #     return()
  #   } else if (input$demo == 'IRSEX') {
  #     temp <- group_by(users.only, MJREC, IRSEX) %>%  summarise(count = n())
  #     return()
  #   } else {
  #     temp <- group_by(users.only, MJREC, NEWRACE2) %>%  summarise(count = n())
  #     return(temp)
  #   }
  # }) 
  # 
  # # Reactive function for demographics withing MJDAY30A question
  # mjday30adems <- reactive({
  #   if(input$demo == 'factor(CATAG6)') {
  #     temp <- group_by(users.only, MJDAY30A, CATAG6) %>%  summarise(count = n())
  #     return()
  #   } else if (input$demo == 'IRSEX') {
  #     temp <- group_by(users.only, MJDAY30A, IRSEX) %>%  summarise(count = n())
  #     return()
  #   } else {
  #     temp <- group_by(users.only, MJDAY30A, NEWRACE2) %>%  summarise(count = n())
  #     return(temp)
  #   }
  # }) 
  
  # Reactive function to choose particular graph depending on the selected question.
  plot <- reactive({
    mjdemo <- input$demo
    
    if(input$question == 'MJAGE') {  
    #   slidevec <- input$agerange
    #   slidemin <- slidevec[1]
    #   slidemax <- slidevec[2]
    #   if(input$demo == 'factor(CATAG6)') {
    #     mjdemo <- 'factor(CATAG6)'
    #   } else if (input$demo == 'IRSEX') {
    #     mjdemo <- 'IRSEX'
    #   } else {
    #     mjdemo <- 'factor(NEWRACE2)'
    #   }
      ranged.users.only <- filter(users.only, MJAGE >= slidemin, MJAGE <= slidemax) %>% group_by_('MJAGE', mjdemo) %>% summarise(count = n(), nationalcount = sum(ANALWT_C)) %>% mutate(perc = count/sum(count), natperc = nationalcount/sum(nationalcount))
      return(plot_ly(ranged.users.only, x=~MJAGE, y=~count, color=~factor(CATAG6), type="bar") %>% layout(barmode="stack"))
    } else if (input$question == 'MJREC') {
      return(ggplot(users.only, aes_string(x='MJREC', fill=input$demo)) + geom_bar(width = 0.4, colour = "black") + theme(axis.text.y = element_text(size = 8)) + coord_flip())
    } else {  
      return(ggplot(users.only, aes_string(x='MJDAY30A', fill=input$demo)) + geom_bar(width=0.4, colour="black") + scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99", "#339900", "#66CC00", "#33CC33", "#99CC00"))) # + facet_wrap(~ input$demo))??
    }
  })

 output$demo_plot <- renderPlotly({
  
   ggplotly(plot(), originalData = FALSE) %>% mutate(ydiff = ymax - ymin) %>% 
     add_text(
       x = ~x, y = ~1 - (ymin + ymax) / 2,
       text = ~ifelse(ydiff > 0.02, round(ydiff, 2), ""),
       showlegend = FALSE, hoverinfo = "none",
       color = I("black"), size = I(9)
     )

  })
})