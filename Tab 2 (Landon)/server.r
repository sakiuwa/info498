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
users.only <- read.csv("../data/tab2-data.csv")

shinyServer(function(input, output) {
  # Reactive function to choose particular graph depending on the selected question and demographic.
  plot <- reactive({
    if(input$question == "MJAGE") { # If "Age first used?" selected
      
      # Deals with the slider input and adjusts data accordingly
      slidevec <- input$agerange
      slidemin <- slidevec[1]
      slidemax <- slidevec[2]
      slider.data <- filter(users.only, MJAGE >= slidemin, MJAGE <= slidemax)
      
      if(input$demo == "CATAG6") { # If Age demographic is selected
        data <- group_by(slider.data, MJAGE, CATAG6) %>% summarise(count = n()) %>% mutate(perc = round(count/sum(count)*100, 2))
        return(plot_ly(data, x=~MJAGE, y=~count, color=~CATAG6, type="bar") %>% layout(barmode='stack', xaxis=list(title="Age first used marijuana"), yaxis= list(title="# of respondents")))
        
      } else if(input$demo == "IRSEX") { # If Sex demographic is selected
        data <- group_by(slider.data, MJAGE, IRSEX) %>% summarise(count = n()) %>% mutate(perc = round(count/sum(count)*100, 2))
        return(plot_ly(data, x=~MJAGE, y=~count, color=~IRSEX, type="bar") %>% layout(barmode="stack", xaxis=list(title="Age first used marijuana"), yaxis= list(title="# of respondents")))
        
      } else { # If Race demographic is selected
        data <- group_by(slider.data, MJAGE, NEWRACE2) %>% summarise(count = n()) %>% mutate(perc = round(count/sum(count)*100, 2))
        return(plot_ly(data, x=~MJAGE, y=~count, color=~NEWRACE2, type="bar") %>% layout(barmode="stack", xaxis=list(title="Age first used marijuana"), yaxis= list(title="# of respondents")))
      }
    } else if(input$question == "MJREC") { # If "How long since used?" selected
      if(input$demo == "CATAG6") {
        data <- group_by(users.only, MJREC, CATAG6) %>% summarise(natcount = sum(ANALWT_C)) %>% mutate(natperc = round(natcount/sum(natcount)*100, 2))
        return(plot_ly(data, x=~MJREC, y=~natperc, color=~CATAG6, type="bar") %>% layout(barmode="stack", xaxis=list(title="Time since last used marijuana"), yaxis=list(title="National %")))
      } else if(input$demo == "IRSEX") {
        data <- group_by(users.only, MJREC, IRSEX) %>% summarise(natcount = sum(ANALWT_C)) %>% mutate(natperc = round(natcount/sum(natcount)*100, 2))
        return(plot_ly(data, x=~MJREC, y=~natperc, color=~IRSEX, type="bar") %>% layout(barmode="stack", xaxis=list(title="Time since last used marijuana"), yaxis=list(title="National %")))
      } else {
        data <- group_by(users.only, MJREC, NEWRACE2) %>% summarise(natcount = sum(ANALWT_C)) %>% mutate(natperc = round(natcount/sum(natcount)*100, 2))
        return(plot_ly(data, x=~MJREC, y=~natperc, color=~NEWRACE2, type="bar") %>% layout(barmode="stack", xaxis=list(title="Time since last used marijuana"), yaxis=list(title="National %")))
      }
    } else { # If "Last 30 days?" selected
      if(input$demo == "CATAG6") {
        data <- group_by(users.only, MJDAY30A, CATAG6) %>% summarise(natcount = round(sum(ANALWT_C), 0))
        return(plot_ly(data, x=~MJDAY30A, y=~natcount, color=~CATAG6, type="bar", hoverinfo='text', text= ~paste('Count: ', format(natcount, big.mark=",", scientific=FALSE))) %>% layout(barmode="stack", xaxis=list(title="Days used in last 30 days"), yaxis=list(title="National count, based on weights")))
      } else if(input$demo == "IRSEX") {
        data <- group_by(users.only, MJDAY30A, IRSEX) %>% summarise(natcount = round(sum(ANALWT_C), 0)) 
        return(plot_ly(data, x=~MJDAY30A, y=~natcount, color=~IRSEX, type="bar", hoverinfo='text', text= ~paste('Count: ', format(natcount, big.mark=",", scientific=FALSE))) %>% layout(barmode="stack", xaxis=list(title="Days used in last 30 days"), yaxis=list(title="National count, based on weights")))
      } else {
        data <- group_by(users.only, MJDAY30A, NEWRACE2) %>% summarise(natcount = round(sum(ANALWT_C), 0)) 
        return(plot_ly(data, x=~MJDAY30A, y=~natcount, color=~NEWRACE2, type="bar", hoverinfo='text', text= ~paste('Count: ', format(natcount, big.mark=",", scientific=FALSE))) %>% layout(barmode="stack", xaxis=list(title="Days used in last 30 days"), yaxis=list(title="National count, based on weights")))
      }
    }
  })

  output$demo_plot <- renderPlotly({
    # Calls reactive function above to output the correct plot
    plot()
   
  })
})