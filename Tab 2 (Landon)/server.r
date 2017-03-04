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

# Read in marijuana and demographics data from survey

mjdata <- read.csv("../data/mjdata.csv")
demographics <- read.csv("../data/demo.csv")

# Filter users of marijuana only, and select relevant columns
demo <- select(demographics, CASEID, NEWRACE2, IRSEX, CATAG6)
users.only <- select(mjdata, CASEID, MJEVER, MJAGE, MJREC, MJDAY30A) %>% filter(MJEVER == "(1) Yes")
users.only <- left_join(users.only, demo, by = "CASEID")

shinyServer(function(input, output) {
  
 output$demo_plot <- renderPlotly({
   print(input$question)
  x <- input$question
  print(x)
  color <- input$demo
   
   g <- ggplot(users.only, aes(x)) + geom_bar(aes(colour = color))
   ggplotly(g)

  
  
  #output$demo_plot <- renderPlotly ({
   # return(plot_ly(users.only, x = ~MJAGE, color = ~IRSEX, barmode='stack'))
  })
})