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

shinyServer(function(input, output) {
  # Reactive function to choose particular graph depending on the selected question.
  plot <- reactive({
    if(input$question == "MJAGE") {
      slidevec <- input$agerange
      slidemin <- slidevec[1]
      slidemax <- slidevec[2]
      slider.data <- filter(users.only, MJAGE >= slidemin, MJAGE <= slidemax)
      if(input$demo == "CATAG6") {
        data <- group_by(slider.data, MJAGE, CATAG6) %>% summarise(count = n()) %>% mutate(perc = round(count/sum(count)*100, 2))
        return(plot_ly(data, x=~MJAGE, y=~perc, color=~CATAG6, type="bar") %>% layout(barmode="stack"))
      } else if(input$demo == "IRSEX") {
        data <- group_by(slider.data, MJAGE, IRSEX) %>% summarise(count = n()) %>% mutate(perc = round(count/sum(count)*100, 2))
        return(plot_ly(data, x=~MJAGE, y=~perc, color=~IRSEX, type="bar") %>% layout(barmode="stack"))
      } else {
        data <- group_by(slider.data, MJAGE, NEWRACE2) %>% summarise(count = n()) %>% mutate(perc = round(count/sum(count)*100, 2))
        return(plot_ly(data, x=~MJAGE, y=~perc, color=~NEWRACE2, type="bar") %>% layout(barmode="stack"))
      }
    } else if(input$question == "MJREC") {
      if(input$demo == "CATAG6") {
        data <- group_by(users.only, MJREC, CATAG6) %>% summarise(count = n()) %>% mutate(perc = round(count/sum(count)*100, 2))
        return(plot_ly(data, x=~perc, y=~MJREC, color=~CATAG6, type="bar", orientation = 'h') %>% layout(barmode="stack"))
      } else if(input$demo == "IRSEX") {
        data <- group_by(users.only, MJREC, IRSEX) %>% summarise(count = n()) %>% mutate(perc = round(count/sum(count)*100, 2))
        return(plot_ly(data, x=~perc, y=~MJREC, color=~IRSEX, type="bar", orientation = 'h') %>% layout(barmode="stack"))
      } else {
        data <- group_by(users.only, MJREC, NEWRACE2) %>% summarise(count = n()) %>% mutate(perc = round(count/sum(count)*100, 2))
        return(plot_ly(data, x=~perc, y=~MJREC, color=~NEWRACE2, type="bar", orientation = 'h') %>% layout(barmode="stack"))
      }
    } else {
      if(input$demo == "CATAG6") {
        data <- group_by(users.only, MJDAY30A, CATAG6) %>% summarise(count = n()) %>% mutate(perc = round(count/sum(count)*100, 2))
        return(plot_ly(data, x=~MJDAY30A, y=~perc, color=~CATAG6, type="bar") %>% layout(barmode="stack"))
      } else if(input$demo == "IRSEX") {
        data <- group_by(users.only, MJDAY30A, IRSEX) %>% summarise(count = n()) %>% mutate(perc = round(count/sum(count)*100, 2))
        return(plot_ly(data, x=~MJDAY30A, y=~perc, color=~IRSEX, type="bar") %>% layout(barmode="stack"))
      } else {
        data <- group_by(users.only, MJDAY30A, NEWRACE2) %>% summarise(count = n()) %>% mutate(perc = round(count/sum(count)*100, 2))
        return(plot_ly(data, x=~MJDAY30A, y=~perc, color=~NEWRACE2, type="bar") %>% layout(barmode="stack"))
      }
    }
  })

 output$demo_plot <- renderPlotly({
  
   plot()
   
  })
})