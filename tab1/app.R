#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)

data <- read.csv("../data/tab1-data.csv")
data$X <- NULL

data$NEWRACE2[data$NEWRACE2 == 1] <- "White"
data$NEWRACE2[data$NEWRACE2 == 2] <- "Black/Afr Am"
data$NEWRACE2[data$NEWRACE2 == 3] <- "Native Am/AK Native "
data$NEWRACE2[data$NEWRACE2 == 4] <- "Native HI/Other Pac Isl"
data$NEWRACE2[data$NEWRACE2 == 5] <- "Asian"
data$NEWRACE2[data$NEWRACE2 == 6] <- "More than one race"
data$NEWRACE2[data$NEWRACE2 == 7] <- "Hispanic"

data$IRSEX[data$IRSEX == 1] <- "Male"
data$IRSEX[data$IRSEX == 2] <- "Female"

data$CATAG6[data$CATAG6 == 1] <- "12-17 Years Old"
data$CATAG6[data$CATAG6 == 2] <- "18-25 Years Old"
data$CATAG6[data$CATAG6 == 3] <- "26-34 Years Old"
data$CATAG6[data$CATAG6 == 4] <- "35-49 Years Old"
data$CATAG6[data$CATAG6 == 5] <- "50-64 Years Old"
data$CATAG6[data$CATAG6 == 6] <- "65 or Older"

data.user <- data %>% filter(MJEVER == 1)
data.nonuser <- data %>% filter(MJEVER == 2)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Marijuana User and Non-user Demographic Information"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "demographics",
                     label = "Demographics",
                     choices = c("Age", "Sex", "Race"),
                     selected = "Age"),
         
         selectInput(inputId = "use",
                     label = "Marijuana Usage",
                     choices = list("User", "Non-user", "Both"),
                     selected = "User")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("usePlot")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
   output$usePlot <- renderPlot({

      if (input$use == "User") {
        plot.data <- data.user
      } else if (input$use == "Non-user") {
        plot.data <- data.nonuser
      } else {
        plot.data <- data
      }
      
     if (input$demographics == "Age") {
       x = plot.data$CATAG6
     } else if (input$demographics == "Sex") {
       x = plot.data$IRSEX
     } else {
       x = plot.data$NEWRACE2
     }
      
     # draw the plot
     ggplot(plot.data, aes(x=x)) + geom_bar()
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

