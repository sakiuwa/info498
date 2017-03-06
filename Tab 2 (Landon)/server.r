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
  database <- reactive({
    if(input$question == 'MJAGE') {  
      slidevec <- input$agerange
      slidemin <- slidevec[1]
      slidemax <- slidevec[2]
      ranged.users.only <- filter(users.only, MJAGE >= slidemin, MJAGE <= slidemax)
      return(ggplot(ranged.users.only, aes_string(x=input$question, fill=input$demo)) + geom_bar(width = 0.4, colour = "black"))
    } else if (input$question == 'MJREC') {
      return(ggplot(users.only, aes_string(x=input$question, fill=input$demo)) + geom_bar(width = 0.4, colour = "black") + theme(axis.text.y = element_text(size = 8)) + coord_flip())
    } else {  
      return(ggplot(users.only, aes_string(x=input$question)) + geom_bar(width=0.4, colour="black") # + facet_wrap(~ input$demo))??
    }
  })

 output$demo_plot <- renderPlotly({
   
   ggplotly(database())

  })
})